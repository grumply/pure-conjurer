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
import Pure.Elm.Component hiding (key,pattern Delete,Listener,root,pattern Form,List)
import qualified Pure.Elm.Component as Pure
import Pure.Hooks hiding (dispatch)
import Pure.Maybe
import Pure.WebSocket as WS hiding (Index,identify)
import Pure.Router hiding (goto)
import qualified Pure.Router as Router
import Pure.Sorcerer as Sorcerer hiding (Read)

import Data.Char
import Data.Typeable
import Data.Hashable
import Data.List as List
import Data.Void
import GHC.Generics as G
import GHC.TypeLits
import Data.Text (Text(..))
import System.IO.Unsafe

import Prelude hiding (Read)

import Unsafe.Coerce

{-
1000 lines of abstract nonsense.

This sort of abstract nonsense is demonstrable of the failings of the existing 
modeling.  But, it currently remove a lot of boilerplate from some very common 
cases - specifically CRU/List with advanced permissions controls and callbacks
and sub-resource assocations. 

All magic starts with cheap tricks; figure out what can generalize all of this 
nonsense.

-}

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

data family Resource resource :: *
data family Product resource :: *
data family Preview resource :: *

class Typeable resource => Preprocessable resource where
  preprocess :: Resource resource -> IO (Maybe (Resource resource))
  preprocess = pure . Just

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
    
  stream (ResourceStream un (Key m)) = 
    let root = "conjuredb/" ++ show (typeRepTyCon (typeOf (undefined :: resource)))
    in root ++ "/" ++ fromTxt (toTxt un) ++ "/" ++ fromTxt (encodeBase62 m) ++ ".stream"

instance (Typeable resource, FromJSON (Resource resource), ToJSON (Resource resource)) => Aggregable (ResourceMsg resource) (Resource resource) where
  update (ResourceCreated r) Nothing = Update r
  update (ResourceUpdated r) (Just _) = Update r
  update ResourceDeleted (Just _) = Delete
  update _ _ = Ignore

  aggregate = "resource.aggregate"

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
    
  stream IndexStream = 
    let root = "conjuredb/" ++ show (typeRepTyCon (typeOf (undefined :: resource)))
    in root ++ "/index.stream"

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

instance (Typeable resource, ToJSON (ProductMsg resource), FromJSON (ProductMsg resource)) => Source (ProductMsg resource) where
  data Stream (ProductMsg resource) = ProductStream Username (Key resource)
    deriving stock Generic
    deriving anyclass (Hashable,ToJSON,FromJSON)

  stream (ProductStream un (Key m)) = 
    let root = "conjuredb/" ++ show (typeRepTyCon (typeOf (undefined :: resource)))
    in root ++ "/" ++ fromTxt (toTxt un) ++ "/" ++ fromTxt (encodeBase62 m) ++ ".stream"

instance (Typeable resource, FromJSON (Product resource), ToJSON (Product resource)) => Aggregable (ProductMsg resource) (Product resource) where
  update (ProductCreated p) Nothing = Update p
  update (ProductUpdated p) (Just _) = Update p
  update ProductDeleted (Just _) = Delete
  update _ _ = Ignore

  aggregate = "product.aggregate"

--------------------------------------------------------------------------------
-- Preview

data PreviewMsg resource
  = PreviewCreated (Preview resource)
  | PreviewUpdated (Preview resource)
  | PreviewDeleted
  deriving stock Generic
deriving instance ToJSON (Preview resource) => ToJSON (PreviewMsg resource)
deriving instance FromJSON (Preview resource) => FromJSON (PreviewMsg resource)

instance (Typeable resource, ToJSON (PreviewMsg resource), FromJSON (PreviewMsg resource)) => Source (PreviewMsg resource) where
  data Stream (PreviewMsg resource) = PreviewStream Username (Key resource)
    deriving stock Generic
    deriving anyclass (Hashable,ToJSON,FromJSON)

  stream (PreviewStream un (Key m)) = 
    let root = "conjuredb/" ++ show (typeRepTyCon (typeOf (undefined :: resource)))
    in root ++ "/" ++ fromTxt (toTxt un) ++ "/" ++ fromTxt (encodeBase62 m) ++ ".stream"

instance (Typeable resource, FromJSON (Preview resource), ToJSON (Preview resource)) => Aggregable (PreviewMsg resource) (Preview resource) where
  update (PreviewCreated p) Nothing = Update p
  update (PreviewUpdated p) (Just _) = Update p
  update PreviewDeleted (Just _) = Delete
  update _ _ = Ignore

  aggregate = "preview.aggregate"

--------------------------------------------------------------------------------
-- A list of previewed resources. In theory, it should be possible to create a
-- resource that doesn't have a preview and simply omit the listener for 
-- @(ListingMsg resource) @(Listing resource) from the set of active listeners.

data Listing a = Listing
  { listing :: [(Key a,Preview a)] 
  } deriving stock Generic
deriving instance ToJSON (Preview a) => ToJSON (Listing a)
deriving instance FromJSON (Preview a) => FromJSON (Listing a)

data ListingMsg a
  = PreviewItemAdded   (Key a) (Preview a)
  | PreviewItemUpdated (Key a) (Preview a)
  | PreviewItemRemoved (Key a)
  deriving stock Generic
deriving instance (ToJSON (Preview a)) => ToJSON (ListingMsg a)
deriving instance (FromJSON (Preview a)) => FromJSON (ListingMsg a)

instance (Typeable a, ToJSON (Preview a), FromJSON (Preview a)) => Source (ListingMsg a) where
  data Stream (ListingMsg a) = GlobalListingStream | UserListingStream Username
    deriving stock Generic
    deriving anyclass Hashable

  stream (UserListingStream un) = 
    let root = "conjuredb/" ++ show (typeRepTyCon (typeOf (undefined :: a)))
    in root ++ "/" ++ fromTxt (toTxt un) ++ "/listing.stream"
  
  stream GlobalListingStream =
    let root = "conjuredb/" ++ show (typeRepTyCon (typeOf (undefined :: a)))
    in root ++ "/listing.stream"

instance ( Typeable a , ToJSON (Preview a), FromJSON (Preview a)) => Aggregable (ListingMsg a) (Listing a) where
  update (PreviewItemAdded k p)   Nothing  = Update Listing { listing = [(k,p)] }
  update (PreviewItemAdded k p)   (Just l) = Update Listing { listing = (k,p) : List.filter (((/=) k) . fst) (listing l) }
  update (PreviewItemUpdated k p) (Just l) = Update l { listing = fmap (\old -> if fst old == k then (k,p) else old) (listing l) }
  update (PreviewItemRemoved k)   (Just l) = Update l { listing = List.filter ((/= k) . fst) (listing l) }
  update _ _                               = Ignore
  
  aggregate = "listing.aggregate"

--------------------------------------------------------------------------------  

resourceDB 
  :: forall resource. 
    ( Typeable resource
    , ToJSON (Resource resource), FromJSON (Resource resource)
    , ToJSON (Preview resource), FromJSON (Preview resource)
    , ToJSON (Product resource), FromJSON (Product resource)
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
    , Preprocessable resource, Previewable resource, Producible resource
    , ToJSON (Resource resource), FromJSON (Resource resource)
    , ToJSON (Product resource), FromJSON (Product resource)
    , ToJSON (Preview resource), FromJSON (Preview resource)
    ) => Callbacks resource -> Owner -> Key resource -> Resource resource -> IO Bool
tryCreateResource Callbacks {..} owner key resource0 = do
  mresource <- preprocess resource0
  case mresource of
    Nothing -> pure False
    Just resource -> do
      -- In the rare (nearly-impossible?) case that this key is already used, trying to re-create a resource
      -- will fail with `Ignored` and this method will return False.
      Sorcerer.observe (ResourceStream owner key :: Stream (ResourceMsg resource)) (ResourceCreated resource) >>= \case
        Added (new :: Resource resource) -> do
          pro <- produce new
          pre <- preview new pro
          Sorcerer.write (ProductStream owner key) (ProductCreated pro)
          Sorcerer.write (PreviewStream owner key) (PreviewCreated pre)
          Sorcerer.write (IndexStream @resource) (ResourceAdded owner key)
          ~(Update (Listing (globalListing :: [(Key resource,Preview resource)]))) <- Sorcerer.transact (GlobalListingStream @resource) (PreviewItemAdded key pre)
          ~(Update (Listing (userListing :: [(Key resource,Preview resource)]))) <- Sorcerer.transact (UserListingStream @resource owner) (PreviewItemAdded key pre)
          onCreateResource owner key new
          onCreateProduct owner key pro
          onCreatePreview owner key pre
          onUpdateListing Nothing globalListing
          onUpdateListing (Just owner) userListing
          pure True
        _ ->
          pure False

tryUpdateResource 
  :: forall resource. 
    ( Typeable resource
    , Preprocessable resource, Previewable resource, Producible resource
    , ToJSON (Resource resource), FromJSON (Resource resource)
    , ToJSON (Product resource), FromJSON (Product resource)
    , ToJSON (Preview resource), FromJSON (Preview resource)
    ) => Callbacks resource -> Owner -> Key resource -> Resource resource -> IO Bool
tryUpdateResource Callbacks {..} owner key resource0 = do
  mresource <- preprocess resource0
  case mresource of
    Nothing -> pure False
    Just resource -> do
      Sorcerer.observe (ResourceStream owner key :: Stream (ResourceMsg resource)) (ResourceCreated resource) >>= \case
        Updated _ (new :: Resource resource) -> do
          pro <- produce new
          pre <- preview new pro
          Sorcerer.write (ProductStream owner key) (ProductUpdated pro)
          Sorcerer.write (PreviewStream owner key) (PreviewUpdated pre)
          ~(Update (Listing (globalListing :: [(Key resource,Preview resource)]))) <- Sorcerer.transact (GlobalListingStream @resource) (PreviewItemAdded key pre)
          ~(Update (Listing (userListing :: [(Key resource,Preview resource)]))) <- Sorcerer.transact (UserListingStream @resource owner) (PreviewItemAdded key pre)
          onUpdateResource owner key new
          onUpdateProduct owner key pro
          onUpdatePreview owner key pre
          onUpdateListing Nothing globalListing
          onUpdateListing (Just owner) userListing
          pure True
        _ ->
          pure False

tryDeleteResource 
  :: forall resource. 
    ( Typeable resource
    , ToJSON (Resource resource), FromJSON (Resource resource)
    , ToJSON (Product resource), FromJSON (Product resource)
    , ToJSON (Preview resource), FromJSON (Preview resource)
    ) => Callbacks resource -> Owner -> Key resource -> IO Bool
tryDeleteResource Callbacks {..} owner key =
  Sorcerer.observe (ResourceStream owner key :: Stream (ResourceMsg resource)) ResourceDeleted >>= \case
    Deleted r -> do
      ~(Deleted pre) <- Sorcerer.observe (PreviewStream owner key) PreviewDeleted
      ~(Deleted pro) <- Sorcerer.observe (ProductStream owner key) ProductDeleted
      ~(Update (Listing (globalListing :: [(Key resource,Preview resource)]))) <- Sorcerer.transact (GlobalListingStream @resource) (PreviewItemRemoved key)
      ~(Update (Listing (userListing :: [(Key resource,Preview resource)]))) <- Sorcerer.transact (UserListingStream @resource owner) (PreviewItemRemoved key)
      onDeleteResource owner key r
      onDeletePreview owner key pre
      onDeleteProduct owner key pro
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
  type Rsp (CreateResource resource) = Maybe (Key resource)

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
  type Req (ReadProduct resource) = (Int,(Username,Key resource))
  type Rsp (ReadProduct resource) = Maybe (Product resource)

readProduct :: Proxy (ReadProduct resource)
readProduct = Proxy

data ReadPreview resource
instance Identify (ReadPreview resource)
instance (Typeable resource) => Request (ReadPreview resource) where
  type Req (ReadPreview resource) = (Int,(Username,Key resource))
  type Rsp (ReadPreview resource) = Maybe (Preview resource)

readPreview :: Proxy (ReadPreview resource)
readPreview = Proxy

data ReadListing resource
instance Identify (ReadListing resource)
instance (Typeable resource) => Request (ReadListing resource) where
  type Req (ReadListing resource) = (Int,Maybe Username)
  type Rsp (ReadListing resource) = Maybe [(Key resource,Preview resource)]

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

resourcePublishingAPI :: forall resource. Typeable resource => API '[] (ResourcePublishingAPI resource)
resourcePublishingAPI = api msgs reqs
  where
    msgs = WS.none
    reqs = createResource @resource
       <:> readResource @resource
       <:> updateResource @resource
       <:> deleteResource @resource
       <:> WS.none

resourceReadingAPI :: forall resource. Typeable resource => API '[] (ResourceReadingAPI resource)
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
  , canUpdateResource :: Self -> Owner -> Key resource -> IO Bool -- Note that this subsumes `canDeleteResource` in cases when an identifier changes on update
  , canDeleteResource :: Self -> Owner -> Key resource -> IO Bool
  , canReadProduct    :: Key resource -> IO Bool
  , canReadPreview    :: Key resource -> IO Bool
  , canReadListing    :: Maybe Owner -> IO Bool
  }

instance Default (Permissions resource) where
  -- default permissions allows the resource creator to create/read/update/delete
  -- and allows anyone else to read products, previews and listings of previews.
  def = Permissions
    { canCreateResource = \s o _ -> pure (s == o)
    , canReadResource   = \s o _ -> pure (s == o)
    , canUpdateResource = \s o _ -> pure (s == o)
    , canDeleteResource = \s o _ -> pure (s == o)
    , canReadProduct    = \_ -> pure True
    , canReadPreview    = \_ -> pure True
    , canReadListing    = \_ -> pure True
    }

data Callbacks resource = Callbacks
  { onCreateResource :: Owner -> Key resource -> Resource resource -> IO ()
  , onReadResource   :: Owner -> Key resource -> Resource resource -> IO ()
  , onUpdateResource :: Owner -> Key resource -> Resource resource -> IO ()
  , onDeleteResource :: Owner -> Key resource -> Resource resource -> IO ()
  , onCreateProduct  :: Owner -> Key resource -> Product resource -> IO ()
  , onReadProduct    :: Owner -> Key resource -> Product resource -> IO ()
  , onUpdateProduct  :: Owner -> Key resource -> Product resource -> IO ()
  , onDeleteProduct  :: Owner -> Key resource -> Product resource -> IO ()
  , onCreatePreview  :: Owner -> Key resource -> Preview resource -> IO ()
  , onReadPreview    :: Owner -> Key resource -> Preview resource -> IO ()
  , onUpdatePreview  :: Owner -> Key resource -> Preview resource -> IO ()
  , onDeletePreview  :: Owner -> Key resource -> Preview resource -> IO ()
  , onReadListing    :: Maybe Owner -> [(Key resource,Preview resource)] -> IO ()
  , onUpdateListing  :: Maybe Owner -> [(Key resource,Preview resource)] -> IO ()
  }

instance Default (Callbacks resource) where
  -- default callbacks simply ignore arguments and return ()
  def = Callbacks
    { onCreateResource = def
    , onReadResource   = def
    , onUpdateResource = def
    , onDeleteResource = def
    , onCreateProduct  = def
    , onReadProduct    = def
    , onUpdateProduct  = def
    , onDeleteProduct  = def
    , onCreatePreview  = def
    , onReadPreview    = def
    , onUpdatePreview  = def
    , onDeletePreview  = def
    , onReadListing    = def
    , onUpdateListing  = def
    }

resourcePublishingBackend :: 
  ( Typeable resource
  , Preprocessable resource, Previewable resource, Producible resource
  , ToJSON (Resource resource), FromJSON (Resource resource)
  , ToJSON (Product resource), FromJSON (Product resource)
  , ToJSON (Preview resource), FromJSON (Preview resource)
  ) => Username -> Permissions resource -> Callbacks resource -> Endpoints '[] (ResourcePublishingAPI resource) '[] (ResourcePublishingAPI resource)
resourcePublishingBackend user ps cs = Endpoints resourcePublishingAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleCreateResource user ps cs 
       <:> handleReadResource user ps cs
       <:> handleUpdateResource user ps cs
       <:> handleDeleteResource user ps cs
       <:> WS.none

resourceReadingBackend :: 
  ( Typeable resource
  , Previewable resource, Producible resource
  , ToJSON (Resource resource), FromJSON (Resource resource)
  , ToJSON (Product resource), FromJSON (Product resource)
  , ToJSON (Preview resource), FromJSON (Preview resource)
  ) => Permissions resource -> Callbacks resource -> Endpoints '[] (ResourceReadingAPI resource) '[] (ResourceReadingAPI resource)
resourceReadingBackend ps cs = Endpoints resourceReadingAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleReadProduct ps cs 
       <:> handleReadPreview ps cs
       <:> handleReadListing ps cs
       <:> WS.none

handleCreateResource 
  :: forall resource. 
    ( Typeable resource
    , Preprocessable resource, Producible resource, Previewable resource
    , ToJSON (Resource resource), FromJSON (Resource resource)
    , ToJSON (Product resource), FromJSON (Product resource)
    , ToJSON (Preview resource), FromJSON (Preview resource)
    ) => Self -> Permissions resource -> Callbacks resource -> RequestHandler (CreateResource resource)
handleCreateResource self Permissions {..} callbacks = responding do
  (owner :: Owner,resource :: Resource resource) <- acquire
  response <- liftIO do
    key <- newKey
    can <- canCreateResource self owner key
    if can then
      tryCreateResource callbacks owner key resource >>= \case
        True -> pure (Just key)
        _    -> pure Nothing
    else
      pure Nothing
  reply response

handleReadResource 
  :: forall resource. 
    ( Typeable resource
    , ToJSON (Resource resource), FromJSON (Resource resource) 
    ) => Self -> Permissions resource -> Callbacks resource -> RequestHandler (ReadResource resource)
handleReadResource self Permissions {..} Callbacks {..} = responding do
  (owner :: Owner,k :: Key resource) <- acquire
  can <- liftIO (canReadResource self owner k)
  if can then do
    Sorcerer.read (ResourceStream owner k :: Stream (ResourceMsg resource)) >>= \case
      Just r -> do
        liftIO (onReadResource owner k r)
        reply (Just r)
      _ -> do
        reply Nothing
  else
    reply Nothing

handleUpdateResource
  :: forall resource. 
    ( Typeable resource
    , Preprocessable resource, Producible resource, Previewable resource
    , ToJSON (Resource resource), FromJSON (Resource resource) 
    , ToJSON (Product resource), FromJSON (Product resource)
    , ToJSON (Preview resource), FromJSON (Preview resource)
    ) => Self -> Permissions resource -> Callbacks resource -> RequestHandler (UpdateResource resource)
handleUpdateResource self Permissions {..} callbacks = responding do
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
    , ToJSON (Resource resource), FromJSON (Resource resource) 
    , ToJSON (Product resource), FromJSON (Product resource)
    , ToJSON (Preview resource), FromJSON (Preview resource)
    ) => Self -> Permissions resource -> Callbacks resource -> RequestHandler (DeleteResource resource)
handleDeleteResource self Permissions {..} callbacks = responding do
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
    , ToJSON (Preview resource), FromJSON (Preview resource)
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
    , ToJSON (Preview resource), FromJSON (Preview resource)
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

root :: forall resource. Typeable resource => Txt
root = "/" <> Txt.toLower (toTxt (show (typeRepTyCon (typeOf (undefined :: resource)))))

class Pathable a where
  toPath :: a -> Txt
  default toPath :: (Generic a, GPathable (Rep a)) => a -> Txt
  toPath = gtoPath . G.from

  fromPath :: Routing x (Maybe a)
  default fromPath :: (Generic a, GPathable (Rep a)) => Routing x (Maybe a)
  fromPath = fmap (fmap (G.to :: Rep a x -> a)) gfromPath

instance Pathable (Key a) where
  toPath (Key k) = "/" <> encodeBase62 k
  fromPath = path' "/:key" (fmap (Key . decodeBase62) "key")

instance Pathable Txt where
  toPath = ("/" <>)
  fromPath = path' "/:txt" "txt"

instance Pathable (Slug a) where
  toPath = toPath . toTxt
  fromPath = fmap (fmap fromTxt) fromPath

instance Pathable () where
  toPath _ = ""
  fromPath = pure (Just ())

class GPathable f where
  gtoPath :: f a -> Txt
  gfromPath :: Routing x (Maybe (f a))

instance GPathable V1 where
  gtoPath _ = ""
  gfromPath = pure (Just (error "GPathable V1 => gfromPath: tried to materialize a void type."))

instance GPathable U1 where
  gtoPath _ = ""
  gfromPath = pure (Just U1)

instance GPathable x => GPathable (M1 r m x) where
  gtoPath (M1 x) = gtoPath x
  gfromPath = fmap (fmap M1) gfromPath

instance Pathable x => GPathable (K1 r x) where
  gtoPath (K1 x) = toPath x
  gfromPath = fmap (fmap K1) fromPath

instance (Typeable a, Typeable b, GPathable a, GPathable b) => GPathable ((:*:) a b) where
  gtoPath (a :*: b) = gtoPath a <> gtoPath b
  gfromPath = do
    ma <- gfromPath 
    mb <- gfromPath
    pure ((:*:) <$> ma <*> mb)

data Read resource = Read Username (Key resource)
class Typeable resource => Readable resource where
  readRoute :: (Read resource -> rt) -> Routing rt ()
  default readRoute :: (Read resource -> rt) -> Routing rt ()
  readRoute f =
    void do
      path (root @resource) do
        path "/:username" do
          un <- "username"
          mi <- fromPath @(Key resource)
          case mi of
            Nothing -> continue
            Just i -> dispatch (f (Read un i))

  toReadRoute :: Read resource -> Txt
  default toReadRoute :: Read resource -> Txt
  toReadRoute (Read un i) = root @resource <> "/" <> toTxt un <> toPath i

  toRead :: WebSocket -> Read resource -> View
  default toRead :: (Component (Product resource), FromJSON (Product resource)) => WebSocket -> Read resource -> View
  toRead ws (Read un i) = producing producer (consuming consumer)
    where
      producer = sync (request (resourceReadingAPI @resource) ws (readProduct @resource) (un,i))
      consumer = maybe "Not Found" run

data Create _role resource = Create Username
class (Typeable resource, Typeable _role, ToJSON (Resource resource)) => Creatable _role resource where
  createRoute :: (Create _role resource -> rt) -> Routing rt ()
  default createRoute :: (Create _role resource -> rt) -> Routing rt ()
  createRoute f =
    void do
      path (root @resource) do
        path "/new/:username" do
          un <- "username"
          dispatch (f (Create un))

  toCreateRoute :: Create _role resource -> Txt
  default toCreateRoute :: Create _role resource -> Txt
  toCreateRoute (Create un) = root @resource <> "/new/" <> toTxt un

  toCreate :: WebSocket -> Create _role resource -> View
  default toCreate :: (Readable resource, Form (Resource resource)) => WebSocket -> Create _role resource -> View
  toCreate ws c@(Create un) =
    withToken @_role $ maybe "Not Authorized" $ \(Token (_,_)) -> 
      let 
        onSubmit resource = do
          mi <- sync (request (resourcePublishingAPI @resource) ws (createResource @resource) (un,resource))
          for_ mi (Router.goto . toReadRoute . Read un)
      in 
        form onSubmit

data List resource = List (Maybe Username)
class (Typeable resource) => Listable resource where
  listRoute :: (List resource -> rt) -> Routing rt ()
  default listRoute :: (List resource -> rt) -> Routing rt ()
  listRoute f =
    void do
      path (root @resource) do
        path "/list" do
          path "/:username" do
            un <- "username"
            dispatch (f (List (Just un)))
          dispatch (f (List Nothing))

  toListRoute :: List resource -> Txt
  toListRoute (List mun) = root @resource <> "/list" <> maybe "" (\un -> "/" <> toTxt un) mun

  toList :: WebSocket -> List resource -> View
  default toList :: (Component (Key resource,Preview resource), FromJSON (Preview resource)) => WebSocket -> List resource -> View
  toList ws (List mun) =
     producing producer (consuming (maybe "Not Found" consumer))
    where
      producer = sync (request (resourceReadingAPI @resource) ws (readListing @resource) mun)
      consumer ps = Ul <||> [ Li <||> [ run p ] | p <- ps ]

--------------------------------------------------------------------------------
-- Subresources

data Sublisting parent a = Sublisting
  { sublisting :: [(Key a,Preview a)]
  } deriving stock Generic
deriving instance ToJSON (Preview a) => ToJSON (Sublisting parent a)
deriving instance FromJSON (Preview a) => FromJSON (Sublisting parent a)

newtype SublistingMsg parent a = SublistingMsg (ListingMsg a)
  deriving stock Generic
deriving instance (ToJSON (ListingMsg a)) => ToJSON (SublistingMsg parent a)
deriving instance (FromJSON (ListingMsg a)) => FromJSON (SublistingMsg parent a)

instance ( Typeable parent, Typeable a , ToJSON (Preview a), FromJSON (Preview a)) => Source (SublistingMsg parent a) where
  data Stream (SublistingMsg parent a) = AssociatedListingStream (Key parent)
    deriving stock Generic
    deriving anyclass Hashable

  stream (AssociatedListingStream (Key m)) =
    let root = "conjuredb/" ++ show (typeRepTyCon (typeOf (undefined :: a)))
    in root ++ "/subresources/" ++ fromTxt (encodeBase62 m) ++ ".stream"

instance ( Typeable parent, Typeable a , ToJSON (Preview a), FromJSON (Preview a)) => Aggregable (SublistingMsg parent a) (Sublisting parent a) where
  update (SublistingMsg (PreviewItemAdded k p))   Nothing  = Update Sublisting { sublisting = [(k,p)] }
  update (SublistingMsg (PreviewItemAdded k p))   (Just l) = Update Sublisting { sublisting = (k,p) : List.filter ((/= k) . fst) (sublisting l) }
  update (SublistingMsg (PreviewItemUpdated k p)) (Just l) = Update l { sublisting = fmap (\old -> if fst old == k then (k,p) else old) (sublisting l) }
  update (SublistingMsg (PreviewItemRemoved k))   (Just l) = Update l { sublisting = List.filter ((/= k) . fst) (sublisting l) }
  update _ _                                             = Ignore
  
  aggregate = "sublisting.aggregate"

data ReadSublisting parent resource
instance Identify (ReadSublisting parent resource)
instance (Typeable parent, Typeable resource) => Request (ReadSublisting parent resource) where
  type Req (ReadSublisting parent resource) = (Int,Key parent)
  type Rsp (ReadSublisting parent resource) = Maybe [(Key resource,Preview resource)]

readSublisting :: Proxy (ReadSublisting parent resource)
readSublisting = Proxy

type SubresourceReadingAPI parent resource =
  '[ ReadSublisting parent resource
   ]

subresourceReadingAPI :: forall parent resource. (Typeable parent, Typeable resource) => API '[] (SubresourceReadingAPI parent resource)
subresourceReadingAPI = api msgs reqs
  where
    msgs = WS.none
    reqs = readSublisting @parent @resource
       <:> WS.none

data SubresourcePermissions parent resource = SubresourcePermissions
  { canReadSublisting :: Key parent -> IO Bool
  }

instance Default (SubresourcePermissions parent resource) where
  def = SubresourcePermissions
    { canReadSublisting = \_ -> pure True
    }

data SubresourceCallbacks parent resource = SubresourceCallbacks
  { onReadSublisting :: Key parent -> [(Key resource,Preview resource)] -> IO ()
  }

instance Default (SubresourceCallbacks parent resource) where
  def = SubresourceCallbacks
    { onReadSublisting = def
    }

addSubresource
  :: forall parent resource.
    ( Typeable parent, Typeable resource
    , ToJSON (SublistingMsg parent resource)
    , ToJSON (Preview resource), FromJSON (Preview resource)
    ) => Key parent -> Key resource -> Preview resource -> IO ()
addSubresource parent resource preview = void do
  Sorcerer.write (AssociatedListingStream parent :: Stream (SublistingMsg parent resource)) 
    (SublistingMsg (PreviewItemAdded resource preview) :: SublistingMsg parent resource)
 
updateSubresource
  :: forall parent resource.
    ( Typeable parent, Typeable resource
    , ToJSON (SublistingMsg parent resource)
    , ToJSON (Preview resource), FromJSON (Preview resource)
    ) => Key parent -> Key resource -> Preview resource -> IO ()
updateSubresource parent resource preview = void do
  Sorcerer.write (AssociatedListingStream parent :: Stream (SublistingMsg parent resource)) 
    (SublistingMsg (PreviewItemUpdated resource preview) :: SublistingMsg parent resource)

removeSubresource
  :: forall parent resource.
    ( Typeable parent, Typeable resource
    , ToJSON (SublistingMsg parent resource)
    , ToJSON (Preview resource), FromJSON (Preview resource)
    ) => Key parent -> Key resource -> IO ()
removeSubresource parent resource = void do
  Sorcerer.write (AssociatedListingStream parent :: Stream (SublistingMsg parent resource)) 
    (SublistingMsg (PreviewItemRemoved resource) :: SublistingMsg parent resource)

subresourceReadingBackend 
  :: forall parent resource.
    ( Typeable parent, Typeable resource
    , ToJSON (Preview resource), FromJSON (Preview resource)
    ) => SubresourcePermissions parent resource -> SubresourceCallbacks parent resource -> Endpoints '[] (SubresourceReadingAPI parent resource) '[] (SubresourceReadingAPI parent resource)
subresourceReadingBackend ps cs = Endpoints subresourceReadingAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleReadSublisting ps cs
       <:> WS.none

handleReadSublisting
  :: forall parent resource.
    ( Typeable parent, Typeable resource
    , ToJSON (Preview resource), FromJSON (Preview resource)
    ) => SubresourcePermissions parent resource -> SubresourceCallbacks parent resource -> RequestHandler (ReadSublisting parent resource)
handleReadSublisting SubresourcePermissions {..} SubresourceCallbacks {..} = responding do
  (i :: Key parent) <- acquire
  can <- liftIO (canReadSublisting i)
  if can then do
    Sorcerer.read ((AssociatedListingStream i) :: Stream (SublistingMsg parent resource)) >>= \case
      Just (l@(Sublisting ps) :: Sublisting parent resource) -> do
        liftIO (onReadSublisting i ps)
        reply (Just ps)
      _ ->
        reply Nothing
  else
    reply Nothing

data Sublist parent resource = Sublist (Key parent)
class (Typeable parent, Typeable resource) => Sublistable parent resource where
  sublistRoute :: (Sublist parent resource -> rt) -> Routing rt ()
  default sublistRoute :: (Sublist parent resource -> rt) -> Routing rt ()
  sublistRoute f =
    void do
      path (root @resource) do
        path "/list" do
          mc <- fromPath @(Key parent)
          case mc of
            Nothing -> continue
            Just i  -> dispatch (f (Sublist i))
            
  toSublistRoute :: Sublist parent resource -> Txt
  default toSublistRoute :: Sublist parent resource -> Txt
  toSublistRoute (Sublist i) = root @resource <> "/list" <> toPath i

  toSublist :: WebSocket -> Sublist parent resource -> View
  default toSublist :: (FromJSON (Preview resource), Component (Key resource,Preview resource)) => WebSocket -> Sublist parent resource -> View
  toSublist ws (Sublist i) =
     producing producer (consuming (maybe "Not Found" consumer))
    where
      producer = sync (request (subresourceReadingAPI @parent @resource) ws (readSublisting :: Proxy (ReadSublisting parent resource)) i)
      consumer ps = Ul <||> [ Li <||> [ run p ] | p <- ps ]

--------------------------------------------------------------------------------

data ResourceRoute _role resource
  = CreateR (Create _role resource)
  | ReadR (Read resource)
  | ListR (List resource)

resourcePage :: forall _role resource.  ( Readable resource, Creatable _role resource, Listable resource) 
             => WebSocket -> ResourceRoute _role resource -> View
resourcePage ws = \case
  CreateR c -> toCreate ws c
  ReadR r   -> toRead ws r
  ListR r   -> toList ws r

resourceRoutes 
  :: forall _role resource route. 
    ( Typeable resource
    , Readable resource, Creatable _role resource, Listable resource 
    ) => (ResourceRoute _role resource -> route) -> Routing route ()
resourceRoutes lift = do
  createRoute (lift . CreateR)
  readRoute (lift . ReadR)
  listRoute (lift . ListR)

resourceLocation 
  :: forall _role resource. 
    ( Typeable resource
    , Readable resource, Creatable _role resource, Listable resource
    ) => ResourceRoute _role resource -> Txt
resourceLocation = \case
  CreateR r -> toCreateRoute r
  ReadR r   -> toReadRoute r
  ListR r   -> toListRoute r

ref :: forall _role resource a. (Typeable resource, Readable resource, Creatable _role resource, Listable resource, HasFeatures a) => ResourceRoute _role resource -> a -> a
ref = lref . resourceLocation

goto :: forall _role resource. (Typeable resource, Readable resource, Creatable _role resource, Listable resource) => ResourceRoute _role resource -> IO ()
goto = Router.goto . resourceLocation

--------------------------------------------------------------------------------

data SubresourceRoute parent resource
  = SublistR (Sublist parent resource)

subresourcePage :: forall parent resource. ( Sublistable parent resource )
                 => WebSocket -> SubresourceRoute parent resource -> View
subresourcePage ws = \case
  SublistR r -> toSublist ws r

subresourceRoutes
  :: forall parent resource route.
    ( Typeable parent, Typeable resource 
    , Sublistable parent resource
    ) => (SubresourceRoute parent resource -> route) -> Routing route ()
subresourceRoutes lift = do
  sublistRoute (lift . SublistR)

subresourceLocation
  :: forall parent resource.
    ( Typeable parent, Typeable resource
    , Sublistable parent resource
    ) => SubresourceRoute parent resource -> Txt
subresourceLocation = \case
  SublistR r -> toSublistRoute r

subref :: forall parent resource a. (Typeable parent, Typeable resource, Sublistable parent resource, HasFeatures a) => SubresourceRoute parent resource -> a -> a
subref = lref . subresourceLocation

subgoto :: forall parent resource. (Typeable parent, Typeable resource, Sublistable parent resource) => SubresourceRoute parent resource -> IO ()
subgoto = Router.goto . subresourceLocation 
