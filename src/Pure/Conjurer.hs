{-# language DerivingStrategies, TypeFamilies, FlexibleContexts, UndecidableInstances, DeriveGeneric, DeriveAnyClass, FlexibleInstances,
      TemplateHaskell, AllowAmbiguousTypes, RankNTypes, DataKinds, PartialTypeSignatures, TypeApplications, ScopedTypeVariables,
      DuplicateRecordFields, StandaloneDeriving, MultiParamTypeClasses, NamedFieldPuns, RecordWildCards, PatternSynonyms, 
      BlockArguments, LambdaCase, CPP, DerivingVia, OverloadedStrings, DefaultSignatures, TypeOperators, InstanceSigs, RoleAnnotations
  #-}
module Pure.Conjurer where

{-
TODO: interface hiding via export control
-}

import Pure.Conjurer.Form
import Pure.Conjurer.Form.Field

import Pure.Auth (Username,Token(..),withToken)
import Pure.Sync
import Pure.Data.JSON hiding (Index,Key)
import Pure.Data.Marker
import qualified Pure.Data.Txt as Txt
import Pure.Elm.Component hiding (key,pattern Delete,Listener,root,pattern Form)
import qualified Pure.Elm.Component as Pure
import Pure.Hooks hiding (dispatch)
import Pure.Maybe
import Pure.WebSocket as WS hiding (Index,identify)
import Pure.Router hiding (goto)
import qualified Pure.Router as Router
import Pure.Sorcerer as Sorcerer

import Data.Char
import Data.Typeable
import Data.Hashable
import Data.List as List
import Data.Void
import GHC.Generics as G
import GHC.TypeLits
import Data.Text (Text(..))
import System.IO.Unsafe

import Prelude

import Unsafe.Coerce

type Self  = Username
type Owner = Username

newtype Slug a = Slug Txt
  deriving (Eq,Ord,ToJSON,FromJSON,Hashable) via Txt
type role Slug nominal

instance FromTxt (Slug a) where
  fromTxt = unsafeCoerce . toSlug

instance ToTxt (Slug a) where
  toTxt (Slug t) = t

instance Field (Slug a) where
  field onchange current =
    Input <| OnInput (withInput onchange) . Value (toTxt current)

-- Idempotent.
--
-- prop> \(x :: String) -> toSlug (toTxt (toSlug (toTxt x))) == toSlug (toTxt x)
-- 
toSlug :: ToTxt a => a -> Slug a
toSlug = Slug . Txt.intercalate "-" . Txt.words . Txt.toLower . Txt.map f . Txt.replace "'" "" . toTxt
    where f c | isAlphaNum c = c | otherwise = ' '

newtype Implicit a = Implicit { implicit :: a }
  deriving stock (Generic,Eq,Ord)
  deriving anyclass Default
instance Field (Implicit a) where
  field _ _ = Pure.Null

newtype Key a = Key Marker
  deriving stock Generic
  deriving (ToJSON,FromJSON,Eq,Ord,Hashable) via Marker
type role Key nominal

data family Identifier resource :: *
data family Resource resource :: *
data family Product resource :: *
data family Preview resource :: *
data family Builder resource :: *

class Identifiable f resource where
  identify :: f resource -> Identifier resource

class Typeable resource => Routable resource where
  root :: Txt
  root = "/" <> Txt.toLower (toTxt (show (typeRepTyCon (typeOf (undefined :: resource)))))

  route :: (Identifier resource -> rt) -> Routing rt (Maybe x)

  locate :: Identifier resource -> Txt

class Typeable resource => Producible resource where
  -- It's possible that product is not needed for some particular use-case
  -- where the productmsg/product listener isn't initialized. For that
  -- case, there is a default instance that returns an error - attempting
  -- to force the produced `Product resource` from this default instance
  -- will throw a helpful error.
  produce :: Resource resource -> IO (Product resource)
  produce _ = 
    let tc = show (typeRepTyCon (typeOf (undefined :: resource)))
    in pure (error $ "Producible " <> tc <> " => produce :: Resource " <> tc <> " -> IO (Product " <> tc <> "): Not implemented.")

class Typeable resource => Previewable resource where
  -- It's possible that preview is not needed for some particular use-case
  -- where the previewmsg/preview listener isn't initialized. For that
  -- case, there is a default instance that returns an error - attempting
  -- to force the produced `Preview resource` from this default instance
  -- will throw a helpful error.
  preview :: Resource resource -> Product resource -> IO (Preview resource)
  preview _ _ =
    let tc = show (typeRepTyCon (typeOf (undefined :: resource)))
    in pure (error $ "Previewable " <> tc <> " => preview :: Resource " <> tc <> " -> Product " <> tc <> " -> IO (Preview " <> tc <> "): Not implemented.")

class Typeable resource => Buildable resource where
  builder :: (Builder resource -> IO ()) -> View
  default builder :: Form (Builder resource) => (Builder resource -> IO ()) -> View
  builder = form

  build :: Builder resource -> IO (Resource resource)

--------------------------------------------------------------------------------
-- Raw resource - typed storage by UUID (Key). Think S3-like where each bucket
-- is a resource type and objects in that bucket correspond to that associated 
-- resource type. Controlling access to a resource is an important part of
-- implementing a safe backend API - as an aid toward that end, a resource is 
-- stored indexed by both a Username and a Key. This keeps most APIs simple
-- while allowing more complex access patterns be controlled with the 
-- `Permissions` structure.

data ResourceMsg resource
  = ResourceCreated (Resource resource)
  | ResourceUpdated (Resource resource)
  | ResourceDeleted
  deriving stock Generic
deriving instance ToJSON (Resource resource) => ToJSON (ResourceMsg resource)
deriving instance FromJSON (Resource resource) => FromJSON (ResourceMsg resource)

instance (Typeable resource, ToJSON (ResourceMsg resource), FromJSON (ResourceMsg resource)) => Source (ResourceMsg resource) where
  data Stream (ResourceMsg resource) = ResourceStream Username (Key resource)
    deriving stock Generic
    deriving anyclass Hashable

instance (Typeable resource, FromJSON (Resource resource), ToJSON (Resource resource)) => Aggregable (ResourceMsg resource) (Resource resource) where
  update (ResourceCreated r) Nothing = Update r
  update (ResourceUpdated r) (Just _) = Update r
  update ResourceDeleted (Just _) = Delete
  update _ _ = Ignore

--------------------------------------------------------------------------------
-- A log of items added to a bucket. Since objects associated with added keys 
-- can be removed, the log does not represent all active/live objects - to
-- determine if an item is alive, you must try to read it, by key.

data Index resource = Index
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data IndexMsg resource
  = ResourceAdded Owner (Key resource)
  deriving stock Generic
deriving instance ToJSON (Key resource) => ToJSON (IndexMsg resource)
deriving instance FromJSON (Key resource) => FromJSON (IndexMsg resource)

instance (Typeable resource, ToJSON (IndexMsg resource), FromJSON (IndexMsg resource)) => Source (IndexMsg resource) where
  data Stream (IndexMsg resource) = IndexStream
    deriving stock Generic
    deriving anyclass (Hashable,ToJSON,FromJSON)

-- The index is simply the event stream - this is a proxy to materialize that
-- event stream because we need a listener associating a message stream with an
-- aggregate, otherwise the messages will be dropped. I guess I could switch to
-- Void? 
--
-- If I got things right in pure-sorcerer, this shouldn't materialize an 
-- aggregate, but it's possible I still haven't gotten that quite right and 
-- this will open and write a null JSON value to the aggregate.
--
-- See `withResource` for accessing all non-deleted resources of a given type.
instance Typeable resource => Aggregable (IndexMsg resource) (Index resource) where
  update _ _ = Ignore

-- Iterate over all live objects of a given type. This is a method by which to 
-- migrate from one format to another, if done before a server initialization 
-- (when the index is not being modified). This should be lazy, and thus 
-- streaming, but I still need to check that to be sure `Sorcerer.events` 
-- is working as expected.
withResources :: forall resource. (Typeable resource, FromJSON (Resource resource), ToJSON (Resource resource)) => (Resource resource -> IO ()) -> IO ()
withResources f = do
  rs <- Sorcerer.events (IndexStream :: Stream (IndexMsg resource))
  for_ rs $ \(ResourceAdded un k) -> do
    mr <- Sorcerer.read (ResourceStream un k)
    for_ mr f

--------------------------------------------------------------------------------
-- Product

data ProductMsg resource
  = ProductCreated (Product resource)
  | ProductUpdated (Product resource)
  | ProductDeleted
  deriving stock Generic
deriving instance ToJSON (Product resource) => ToJSON (ProductMsg resource)
deriving instance FromJSON (Product resource) => FromJSON (ProductMsg resource)

instance (Typeable resource, ToJSON (ProductMsg resource), FromJSON (ProductMsg resource), Hashable (Identifier resource)) => Source (ProductMsg resource) where
  data Stream (ProductMsg resource) = ProductStream Username (Identifier resource)
    deriving stock Generic
deriving instance Hashable (Identifier resource) => Hashable (Stream (ProductMsg resource))
deriving instance ToJSON   (Identifier resource) => ToJSON   (Stream (ProductMsg resource))
deriving instance FromJSON (Identifier resource) => FromJSON (Stream (ProductMsg resource))

instance (Typeable resource, FromJSON (Product resource), ToJSON (Product resource), Hashable (Identifier resource)) => Aggregable (ProductMsg resource) (Product resource) where
  update (ProductCreated p) Nothing = Update p
  update (ProductUpdated p) (Just _) = Update p
  update ProductDeleted (Just _) = Delete
  update _ _ = Ignore

--------------------------------------------------------------------------------
-- Preview

data PreviewMsg resource
  = PreviewCreated (Preview resource)
  | PreviewUpdated (Preview resource)
  | PreviewDeleted
  deriving stock Generic
deriving instance ToJSON (Preview resource) => ToJSON (PreviewMsg resource)
deriving instance FromJSON (Preview resource) => FromJSON (PreviewMsg resource)

instance (Typeable resource, ToJSON (PreviewMsg resource), FromJSON (PreviewMsg resource), Hashable (Identifier resource)) => Source (PreviewMsg resource) where
  data Stream (PreviewMsg resource) = PreviewStream Username (Identifier resource)
    deriving stock Generic
deriving instance Hashable (Identifier resource) => Hashable (Stream (PreviewMsg resource))
deriving instance ToJSON   (Identifier resource) => ToJSON   (Stream (PreviewMsg resource))
deriving instance FromJSON (Identifier resource) => FromJSON (Stream (PreviewMsg resource))

instance (Typeable resource, FromJSON (Preview resource), ToJSON (Preview resource), Hashable (Identifier resource)) => Aggregable (PreviewMsg resource) (Preview resource) where
  update (PreviewCreated p) Nothing = Update p
  update (PreviewUpdated p) (Just _) = Update p
  update PreviewDeleted (Just _) = Delete
  update _ _ = Ignore

--------------------------------------------------------------------------------
-- A list of previewed resources. In theory, it should be possible to create a
-- resource that doesn't have a preview and simply omit the listener for 
-- @(ListingMsg resource) @(Listing resource) from the set of active listeners.

data Listing a = Listing
  { listing :: [Preview a] 
  } deriving stock Generic
deriving instance ToJSON (Preview a) => ToJSON (Listing a)
deriving instance FromJSON (Preview a) => FromJSON (Listing a)

data ListingMsg a
  = PreviewItemAdded   (Preview a)
  | PreviewItemUpdated (Preview a)
  | PreviewItemRemoved (Identifier a)
  deriving stock Generic
deriving instance (ToJSON (Identifier a), ToJSON (Preview a)) => ToJSON (ListingMsg a)
deriving instance (FromJSON (Identifier a), FromJSON (Preview a)) => FromJSON (ListingMsg a)

instance (Typeable a, ToJSON (Identifier a), FromJSON (Identifier a), ToJSON (Preview a), FromJSON (Preview a)) => Source (ListingMsg a) where
  data Stream (ListingMsg a) = GlobalListingStream | UserListingStream Username
    deriving stock Generic
    deriving anyclass Hashable

instance (Typeable a, Identifiable Preview a, Eq (Identifier a), ToJSON (Identifier a), ToJSON (Preview a), FromJSON (Identifier a), FromJSON (Preview a)) => Aggregable (ListingMsg a) (Listing a) where
  update (PreviewItemAdded p)   Nothing  = Update Listing { listing = [p] }
  update (PreviewItemAdded p)   (Just l) = Update Listing { listing = p : List.filter (((/=) (identify p)) . identify) (listing l) }
  update (PreviewItemUpdated p) (Just l) = Update l { listing = fmap (\old -> if identify old == identify p then p else old) (listing l) }
  update (PreviewItemRemoved i) (Just l) = Update l { listing = List.filter ((/= i) . identify) (listing l) }
  update _ _                             = Ignore

--------------------------------------------------------------------------------  

resourceDB 
  :: forall resource. 
    ( Typeable resource
    , Identifiable Preview resource
    , ToJSON (Resource resource), FromJSON (Resource resource)
    , ToJSON (Identifier resource), FromJSON (Identifier resource), Eq (Identifier resource)
    , ToJSON (Preview resource), FromJSON (Preview resource)
    , ToJSON (Product resource), FromJSON (Product resource)
    , Hashable (Identifier resource)
    ) => [Listener]
resourceDB = 
  [ listener @(ResourceMsg resource) @(Resource resource)
  , listener @(IndexMsg resource) @(Index resource)
  , listener @(ProductMsg resource) @(Product resource)
  , listener @(PreviewMsg resource) @(Preview resource)
  , listener @(ListingMsg resource) @(Listing resource)
  ]

newKey :: IO (Key resource)
newKey = Key <$> markIO

tryCreateResource 
  :: forall resource. 
    ( Typeable resource
    , Identifiable Resource resource
    , Identifiable Preview resource
    , Eq (Identifier resource), Hashable (Identifier resource)
    , ToJSON (Identifier resource) , ToJSON (Product resource), ToJSON (Preview resource), ToJSON (Resource resource)
    , FromJSON (Identifier resource), FromJSON (Product resource), FromJSON (Preview resource), FromJSON (Resource resource)
    , Previewable resource, Producible resource
    ) => Callbacks resource -> Owner -> Key resource -> Resource resource -> IO Bool
tryCreateResource Callbacks {..} owner key resource =
  -- In the rare (nearly-impossible?) case that this key is already used, trying to re-create a resource
  -- will fail with `Ignored` and this method will return False.
  Sorcerer.observe (ResourceStream owner key :: Stream (ResourceMsg resource)) (ResourceCreated resource) >>= \case
    Added (_ :: Resource resource) -> do
      let i = identify resource
      pro <- produce resource
      pre <- preview resource pro
      Sorcerer.write (IndexStream @resource) (ResourceAdded owner key)
      Sorcerer.write (ProductStream owner i) (ProductCreated pro)
      Sorcerer.write (PreviewStream owner i) (PreviewCreated pre)
      ~(Update (Listing globalListing)) <- Sorcerer.transact (GlobalListingStream @resource) (PreviewItemAdded pre)
      ~(Update (Listing userListing)) <- Sorcerer.transact (UserListingStream @resource owner) (PreviewItemAdded pre)
      onCreateResource owner i key resource
      onCreateProduct owner i pro
      onCreatePreview owner i pre
      onUpdateListing Nothing globalListing
      onUpdateListing (Just owner) userListing
      pure True
    _ ->
      pure False

tryUpdateResource 
  :: forall resource. 
    ( Typeable resource
    , Identifiable Resource resource
    , Identifiable Preview resource
    , Eq (Identifier resource), Hashable (Identifier resource)
    , ToJSON (Identifier resource), ToJSON (Product resource), ToJSON (Preview resource), ToJSON (Resource resource)
    , FromJSON (Identifier resource), FromJSON (Product resource), FromJSON (Preview resource), FromJSON (Resource resource)
    , Previewable resource, Producible resource
    ) => Callbacks resource -> Owner -> Key resource -> Resource resource -> IO Bool
tryUpdateResource Callbacks {..} owner key resource =
  Sorcerer.observe (ResourceStream owner key :: Stream (ResourceMsg resource)) (ResourceCreated resource) >>= \case
    Updated _ (_ :: Resource resource) -> do
      let i = identify resource
      pro <- produce resource
      pre <- preview resource pro
      Sorcerer.write (ProductStream owner i) (ProductUpdated pro)
      Sorcerer.write (PreviewStream owner i) (PreviewUpdated pre)
      ~(Update (Listing globalListing)) <- Sorcerer.transact (GlobalListingStream @resource) (PreviewItemAdded pre)
      ~(Update (Listing userListing)) <- Sorcerer.transact (UserListingStream @resource owner) (PreviewItemAdded pre)
      onUpdateResource owner i key resource
      onUpdateProduct owner i pro
      onUpdatePreview owner i pre
      onUpdateListing Nothing globalListing
      onUpdateListing (Just owner) userListing
      pure True
    _ ->
      pure False

tryDeleteResource 
  :: forall resource. 
    ( Typeable resource
    , Identifiable Resource resource
    , Identifiable Preview resource
    , Eq (Identifier resource), Hashable (Identifier resource)
    , ToJSON (Identifier resource) , ToJSON (Product resource), ToJSON (Preview resource), ToJSON (Resource resource)
    , FromJSON (Identifier resource), FromJSON (Product resource), FromJSON (Preview resource), FromJSON (Resource resource)
    ) => Callbacks resource -> Owner -> Key resource -> IO Bool
tryDeleteResource Callbacks {..} owner key =
  Sorcerer.observe (ResourceStream owner key :: Stream (ResourceMsg resource)) ResourceDeleted >>= \case
    Deleted r -> do
      let i = identify r
      ~(Deleted pre) <- Sorcerer.observe (PreviewStream owner i) PreviewDeleted
      ~(Deleted pro) <- Sorcerer.observe (ProductStream owner i) ProductDeleted
      ~(Update (Listing globalListing)) <- Sorcerer.transact (GlobalListingStream @resource) (PreviewItemRemoved i)
      ~(Update (Listing userListing)) <- Sorcerer.transact (UserListingStream @resource owner) (PreviewItemRemoved i)
      onDeleteResource owner i key r
      onDeletePreview owner i pre
      onDeleteProduct owner i pro
      onUpdateListing Nothing globalListing
      onUpdateListing (Just owner) userListing
      pure True
    _ -> do
      pure False

--------------------------------------------------------------------------------
-- Sadly, polymorphic API endpoints can't currently be derived with 
-- mkRequest/mkMessage

data CreateResource resource
instance Identify (CreateResource resource)
instance (Typeable resource) => Request (CreateResource resource) where
  type Req (CreateResource resource) = (Int,(Username,Resource resource))
  type Rsp (CreateResource resource) = Maybe (Identifier resource)

createResource :: Proxy (CreateResource resource)
createResource = Proxy

data ReadResource resource
instance Identify (ReadResource resource)
instance (Typeable resource) => Request (ReadResource resource) where
  type Req (ReadResource resource) = (Int,(Username,Key resource))
  type Rsp (ReadResource resource) = Maybe (Resource resource)

readResource :: Proxy (ReadResource resource)
readResource = Proxy

data UpdateResource resource
instance Identify (UpdateResource resource)
instance (Typeable resource) => Request (UpdateResource resource) where
  type Req (UpdateResource resource) = (Int,(Username,Key resource,Resource resource))
  type Rsp (UpdateResource resource) = Maybe Bool

updateResource :: Proxy (UpdateResource resource)
updateResource = Proxy

data DeleteResource resource
instance Identify (DeleteResource resource)
instance (Typeable resource) => Request (DeleteResource resource) where
  type Req (DeleteResource resource) = (Int,(Username,Key resource))
  type Rsp (DeleteResource resource) = Maybe Bool

deleteResource :: Proxy (DeleteResource resource)
deleteResource = Proxy

data ReadProduct resource
instance Identify (ReadProduct resource)
instance (Typeable resource) => Request (ReadProduct resource) where
  type Req (ReadProduct resource) = (Int,(Username,Identifier resource))
  type Rsp (ReadProduct resource) = Maybe (Product resource)

readProduct :: Proxy (ReadProduct resource)
readProduct = Proxy

data ReadPreview resource
instance Identify (ReadPreview resource)
instance (Typeable resource) => Request (ReadPreview resource) where
  type Req (ReadPreview resource) = (Int,(Username,Identifier resource))
  type Rsp (ReadPreview resource) = Maybe (Preview resource)

readPreview :: Proxy (ReadPreview resource)
readPreview = Proxy

data ReadListing resource
instance Identify (ReadListing resource)
instance (Typeable resource) => Request (ReadListing resource) where
  type Req (ReadListing resource) = (Int,Maybe Username)
  type Rsp (ReadListing resource) = Maybe [Preview resource]

readListing :: Proxy (ReadListing resource)
readListing = Proxy

type ResourcePublishingAPI resource = 
  '[ CreateResource resource
   , ReadResource resource
   , UpdateResource resource
   , DeleteResource resource
   ]

type ResourceReadingAPI resource =
  '[ ReadProduct resource
   , ReadPreview resource
   , ReadListing resource
   ]

resourcePublishingAPI :: forall resource. Typeable resource => API _ (ResourcePublishingAPI resource)
resourcePublishingAPI = api msgs reqs
  where
    msgs = WS.none
    reqs = createResource @resource
       <:> readResource @resource
       <:> updateResource @resource
       <:> deleteResource @resource
       <:> WS.none

resourceReadingAPI :: forall resource. Typeable resource => API _ (ResourceReadingAPI resource)
resourceReadingAPI = api msgs reqs
  where
    msgs = WS.none
    reqs = readProduct @resource
       <:> readPreview @resource
       <:> readListing @resource
       <:> WS.none

data Permissions resource = Permissions
  { canCreateResource :: Self -> Owner -> Key resource -> IO Bool
  , canReadResource   :: Self -> Owner -> Key resource -> IO Bool 
  , canUpdateResource :: Self -> Owner -> Key resource -> IO Bool
  , canDeleteResource :: Self -> Owner -> Key resource -> IO Bool
  , canReadProduct    :: Identifier resource -> IO Bool
  , canReadPreview    :: Identifier resource -> IO Bool
  , canReadListing    :: Maybe Owner -> IO Bool
  }

instance Default (Permissions resource) where
  def = 
    Permissions 
      { canCreateResource = \s o _ -> pure (s == o) 
      , canReadResource   = \s o _ -> pure (s == o) 
      , canUpdateResource = \s o _ -> pure (s == o) 
      , canDeleteResource = \s o _ -> pure (s == o) 
      , canReadProduct    = \_     -> pure True 
      , canReadPreview    = \_     -> pure True 
      , canReadListing    = \_     -> pure True
      } 

unsafeLoggingPermissions :: forall resource. (Typeable resource, ToJSON (Identifier resource)) => Permissions resource
unsafeLoggingPermissions =
  let 
    tc = toTxt (show (typeRepTyCon (typeOf (undefined :: resource)))) 

    yes :: ToJSON a => Txt -> a -> IO Bool
    yes tag x = logJSON (tag <> " " <> tc,x) >> pure True
  in 
    Permissions 
      { canCreateResource = \_ _ -> yes "Allowing creation of"
      , canReadResource   = \_ _ -> yes "Allowing read of" 
      , canUpdateResource = \_ _ -> yes "Allowing update of"
      , canDeleteResource = \_ _ -> yes "Allowing deletion of"
      , canReadProduct    = yes "Allowing read of Product of"
      , canReadPreview    = yes "Allowing read of Preview of"
      , canReadListing    = \_   -> yes "Allowing listing of Previews of" ()
      }

data Callbacks resource = Callbacks
  { onCreateResource  :: Owner -> Identifier resource -> Key resource -> Resource resource -> IO ()
  , onReadResource    :: Owner -> Identifier resource -> Key resource -> Resource resource -> IO ()
  , onUpdateResource  :: Owner -> Identifier resource -> Key resource -> Resource resource -> IO ()
  , onDeleteResource  :: Owner -> Identifier resource -> Key resource -> Resource resource -> IO ()
  , onCreateProduct   :: Owner -> Identifier resource -> Product resource -> IO ()
  , onReadProduct     :: Owner -> Identifier resource -> Product resource -> IO ()
  , onUpdateProduct   :: Owner -> Identifier resource -> Product resource -> IO ()
  , onDeleteProduct   :: Owner -> Identifier resource -> Product resource -> IO ()
  , onCreatePreview   :: Owner -> Identifier resource -> Preview resource -> IO ()
  , onReadPreview     :: Owner -> Identifier resource -> Preview resource -> IO ()
  , onUpdatePreview   :: Owner -> Identifier resource -> Preview resource -> IO ()
  , onDeletePreview   :: Owner -> Identifier resource -> Preview resource -> IO ()
  , onReadListing     :: Maybe Owner -> [Preview resource] -> IO ()
  , onUpdateListing   :: Maybe Owner -> [Preview resource] -> IO ()
  }

instance Default (Callbacks resource) where
  def = Callbacks 
    def def def def
    def def def def  
    def def def def  
    def def

resourcePublishingBackend :: 
  ( Typeable resource
  , ToJSON (Resource resource), FromJSON (Resource resource)
  , Identifiable Resource resource
  , Identifiable Preview resource
  , Previewable resource, Producible resource
  , ToJSON (Identifier resource)
  , ToJSON (Product resource), FromJSON (Product resource)
  , ToJSON (Preview resource), FromJSON (Preview resource)
  , FromJSON (Identifier resource), ToJSON (Identifier resource), Hashable (Identifier resource), Eq (Identifier resource)
  ) => Username -> Permissions resource -> Callbacks resource -> Endpoints '[] (ResourcePublishingAPI resource) '[] (ResourcePublishingAPI resource)
resourcePublishingBackend user permissions callbacks = Endpoints resourcePublishingAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleCreateResource user permissions callbacks
       <:> handleReadResource user permissions callbacks
       <:> handleUpdateResource user permissions callbacks
       <:> handleDeleteResource user permissions callbacks
       <:> WS.none

resourceReadingBackend :: 
  ( Typeable resource
  , ToJSON (Resource resource), FromJSON (Resource resource)
  , Identifiable Resource resource
  , Identifiable Preview resource
  , Previewable resource, Producible resource
  , ToJSON (Identifier resource)
  , ToJSON (Product resource), FromJSON (Product resource)
  , ToJSON (Preview resource), FromJSON (Preview resource)
  , FromJSON (Identifier resource), ToJSON (Identifier resource), Hashable (Identifier resource), Eq (Identifier resource)
  ) => Permissions resource -> Callbacks resource -> Endpoints '[] (ResourceReadingAPI resource) '[] (ResourceReadingAPI resource)
resourceReadingBackend permissions callbacks = Endpoints resourceReadingAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleReadProduct permissions callbacks
       <:> handleReadPreview permissions callbacks
       <:> handleReadListing permissions callbacks
       <:> WS.none

handleCreateResource 
  :: forall resource. 
     ( Typeable resource
     , Identifiable Resource resource
     , Identifiable Preview resource
     , ToJSON (Resource resource), FromJSON (Resource resource)
     , Producible resource, Previewable resource
     , FromJSON (Product resource), ToJSON (Product resource)
     , FromJSON (Preview resource), ToJSON (Preview resource)
     , FromJSON (Identifier resource), ToJSON (Identifier resource), Hashable (Identifier resource), Eq (Identifier resource)
     ) => Self -> Permissions resource -> Callbacks resource -> RequestHandler (CreateResource resource)
handleCreateResource self Permissions { canCreateResource } callbacks = responding do
  (owner :: Owner,resource :: Resource resource) <- acquire
  response <- liftIO do
    key <- newKey
    can <- canCreateResource self owner key
    if can then
      tryCreateResource callbacks owner key resource >>= \case
        True -> pure (Just (identify resource))
        _    -> pure Nothing
    else
      pure Nothing
  reply response

handleReadResource 
  :: forall resource. 
    ( Typeable resource
    , ToJSON (Resource resource), FromJSON (Resource resource) 
    , Identifiable Resource resource
    ) => Self -> Permissions resource -> Callbacks resource -> RequestHandler (ReadResource resource)
handleReadResource self Permissions { canReadResource } Callbacks {..} = responding do
  (owner :: Owner,k :: Key resource) <- acquire
  can <- liftIO (canReadResource self owner k)
  if can then do
    Sorcerer.read (ResourceStream owner k :: Stream (ResourceMsg resource)) >>= \case
      Just r -> do
        liftIO (onReadResource owner (identify r) k r)
        reply (Just r)
      _ -> do
        reply Nothing
  else
    reply Nothing

handleUpdateResource
  :: forall resource. 
    ( Typeable resource
    , Identifiable Resource resource
    , Identifiable Preview resource
    , ToJSON (Resource resource), FromJSON (Resource resource) 
    , Producible resource, Previewable resource
    , FromJSON (Product resource), ToJSON (Product resource)
    , FromJSON (Preview resource), ToJSON (Preview resource)
    , Eq (Identifier resource), FromJSON (Identifier resource), Hashable (Identifier resource), ToJSON (Identifier resource)
    ) => Self -> Permissions resource -> Callbacks resource -> RequestHandler (UpdateResource resource)
handleUpdateResource self Permissions { canUpdateResource } callbacks = responding do
  (owner :: Owner,key :: Key resource,resource :: Resource resource) <- acquire
  response <- liftIO do
    can <- canUpdateResource self owner key
    if can then
      Just <$> tryUpdateResource callbacks owner key resource
    else
      pure Nothing
  reply response

handleDeleteResource
  :: forall resource. 
     ( Typeable resource
     , Identifiable Resource resource
     , Identifiable Preview resource
     , ToJSON (Resource resource), FromJSON (Resource resource) 
     , ToJSON (Product resource), FromJSON (Product resource)
     , ToJSON (Preview resource), FromJSON (Preview resource)
     , Eq (Identifier resource), Hashable (Identifier resource), ToJSON (Identifier resource), FromJSON (Identifier resource)
     ) => Self -> Permissions resource -> Callbacks resource -> RequestHandler (DeleteResource resource)
handleDeleteResource self Permissions { canDeleteResource } callbacks = responding do
  (owner :: Owner,key :: Key resource) <- acquire
  response <- liftIO do
    can <- canDeleteResource self owner key
    if can then
      Just <$> tryDeleteResource callbacks owner key    
    else do
      pure Nothing
  reply response

handleReadProduct
  :: forall resource. 
     ( Typeable resource
     , ToJSON (Product resource), FromJSON (Product resource)
     , Hashable (Identifier resource), FromJSON (Identifier resource)
     ) => Permissions resource -> Callbacks resource -> RequestHandler (ReadProduct resource)
handleReadProduct Permissions {..} Callbacks {..} = responding do
  (owner :: Owner,i) <- acquire
  can <- liftIO (canReadProduct i)
  if can then do
    Sorcerer.read (ProductStream owner i) >>= \case
      Just p -> do
        liftIO (onReadProduct owner i p)
        reply (Just p)
      _ ->
        reply Nothing
  else
    reply Nothing

handleReadPreview
  :: forall resource. 
     ( Typeable resource
     , Identifiable Resource resource
     , ToJSON (Preview resource), FromJSON (Preview resource)
     , Hashable (Identifier resource), FromJSON (Identifier resource)
     ) => Permissions resource -> Callbacks resource -> RequestHandler (ReadPreview resource)
handleReadPreview Permissions {..} Callbacks {..} = responding do
  (owner :: Owner,i) <- acquire
  can <- liftIO (canReadPreview i)
  if can then do
    Sorcerer.read (PreviewStream owner i) >>= \case
      Just p -> do
        liftIO (onReadPreview owner i p)
        reply (Just p)
      _ ->
        reply Nothing
  else
    reply Nothing

handleReadListing
  :: forall resource. 
     ( Typeable resource
     , Identifiable Preview resource
     , ToJSON (Preview resource), FromJSON (Preview resource)
     , Eq (Identifier resource), ToJSON (Identifier resource), FromJSON (Identifier resource)
     ) => Permissions resource -> Callbacks resource -> RequestHandler (ReadListing resource)
handleReadListing Permissions {..} Callbacks {..} = responding do
  (owner :: Maybe Owner) <- acquire
  can <- liftIO (canReadListing owner)
  if can then do
    Sorcerer.read (maybe (GlobalListingStream @resource) (\o -> UserListingStream @resource o) owner) >>= \case
      Just l@(Listing ps) -> do
        liftIO (onReadListing owner ps)
        reply (Just ps)
      _ ->
        reply Nothing
  else
    reply Nothing

data ResourceRoute resource
  = CreateResource Owner
  | UpdateResource Owner (Key resource)
  | ReadProduct Owner (Identifier resource)
  | ReadPreview Owner (Identifier resource)
  | ListPreviews (Maybe Owner)

deriving instance Eq (Identifier resource) => Eq (ResourceRoute resource)

resourceRoutes :: forall resource route. (Typeable resource, Routable resource) => (ResourceRoute resource -> route) -> Routing route _
resourceRoutes liftResource =
  path (root @resource) do
    let disp = dispatch . liftResource

    path "/new/:username" do
      user <- "username"
      disp (CreateResource user)

    path "/update/:username/:marker" do
      user <- "username"
      marker <- "marker"
      disp (UpdateResource user (Key (decodeBase16 marker)))

    path "/list" do
      path "/:username" do
        user <- "username"
        disp (ListPreviews (Just user))
      disp (ListPreviews Nothing)
      
    path "/preview" do
      path "/:username" do
        user <- "username"
        Pure.Conjurer.route (liftResource . ReadPreview user) 

    path "/:username" do
      user <- "username"
      Pure.Conjurer.route (liftResource . ReadProduct user)

resourceLocation :: forall resource. (Typeable resource, Routable resource) => ResourceRoute resource -> Txt
resourceLocation route = 
  root @resource <> 
    case route of
      CreateResource u         -> "/new/" <> toTxt u
      UpdateResource u (Key m) -> "/update/" <> toTxt u <> "/" <> encodeBase16 m
      ReadProduct u i          -> "/" <> toTxt u <> "/" <> locate i
      ReadPreview u i          -> "/preview/" <> toTxt u <> "/" <> locate i
      ListPreviews mu          -> "/list" <> maybe "" (\u -> "/" <> toTxt u) mu

ref :: (Typeable resource, Routable resource, HasFeatures a) => ResourceRoute resource -> a -> a
ref = lref . resourceLocation

goto :: (Typeable resource, Routable resource) => ResourceRoute resource -> IO ()
goto = Router.goto . resourceLocation

{-
TODO: refine and improve resourcePage

* Add delete buttons to ListPreviews if the user is logged in and owns the listing.
* Add controls to product/preview if the user is logged in and owns the resource.

-}

resourcePage 
  :: forall _role resource. 
    ( Typeable _role
    , Typeable resource
    , Routable resource
    , Buildable resource
    , ToJSON (Resource resource), FromJSON (Resource resource)
    , Form (Resource resource)
    , Component (Product resource), FromJSON (Product resource)
    , Component (Preview resource), FromJSON (Preview resource)
    , ToJSON (Identifier resource), FromJSON (Identifier resource)
    ) => WebSocket -> ResourceRoute resource -> View
resourcePage ws (CreateResource un) =
  withToken @_role $ maybe "Not Authorized" $ \(Token (_,_)) -> 
    let onSubmit builder = do
          resource <- build @resource builder
          mi <- sync (request (resourcePublishingAPI @resource) ws (createResource @resource) (un,resource))
          for_ mi (goto . ReadProduct un)
    in builder onSubmit

resourcePage ws (ReadProduct un i) = 
  producing producer (consuming consumer)
  where
    producer = sync (request (resourceReadingAPI @resource) ws (readProduct @resource) (un,i))
    consumer = maybe "Not Found" run

resourcePage ws (ReadPreview un i) = 
  producing producer (consuming consumer)
  where
    producer = sync (request (resourceReadingAPI @resource) ws (readPreview @resource) (un,i))
    consumer = maybe "Not Found" run

resourcePage ws (ListPreviews mu) = 
  producing producer (consuming (maybe "Not Found" consumer))
  where
    producer = sync (request (resourceReadingAPI @resource) ws (readListing @resource) mu)
    consumer ps = Ul <||> [ Li <||> [ run p ] | p <- ps ]