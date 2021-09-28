{-# language DerivingStrategies, TypeFamilies, FlexibleContexts, UndecidableInstances, DeriveGeneric, DeriveAnyClass, FlexibleInstances,
      TemplateHaskell, AllowAmbiguousTypes, RankNTypes, DataKinds, PartialTypeSignatures, TypeApplications, ScopedTypeVariables,
      DuplicateRecordFields, StandaloneDeriving, MultiParamTypeClasses, NamedFieldPuns, RecordWildCards, PatternSynonyms, 
      BlockArguments, LambdaCase, CPP, DerivingVia, OverloadedStrings, DefaultSignatures, TypeOperators, InstanceSigs
  #-}
module Pure.Conjurer where

import Pure.Conjurer.Form

import Pure.Auth 
import Pure.Sync
import Pure.Data.JSON hiding (Index)
import qualified Pure.Data.Txt as Txt
import Pure.Elm.Component hiding (key,pattern Delete,Listener,root,pattern Form)
import qualified Pure.Elm.Component as Pure
import Pure.Hooks hiding (dispatch)
import Pure.Maybe
import Pure.WebSocket as WS hiding (Index)
import Pure.Router hiding (goto)
import qualified Pure.Router as Router
import Pure.Sorcerer as Sorcerer

import Data.Char
import Data.Typeable
import Data.Hashable
import Data.List as List
import GHC.Generics as G
import GHC.TypeLits
import Data.Text (Text(..))
import System.IO.Unsafe

import Prelude

import Unsafe.Coerce

newtype Slug a = Slug Txt
  deriving (Eq,Ord,ToTxt,FromTxt,ToJSON,FromJSON,Hashable) via Txt

-- Idempotent.
--
-- prop> \(x :: String) -> toSlug (toTxt (toSlug (toTxt x))) == toSlug (toTxt x)
-- 
toSlug :: ToTxt a => a -> Slug resource
toSlug = Slug . Txt.intercalate "-" . Txt.words . Txt.toLower . Txt.map f . Txt.replace "'" "" . toTxt
    where f c | isAlphaNum c = c | otherwise = ' '

class Typeable resource => IsResource resource where
  data Resource resource :: *

  root :: Txt
  root = "/" <> Txt.toLower (toTxt (show (typeRepTyCon (typeOf (undefined :: resource)))))

  slug :: Resource resource -> Slug resource

data ResourceMsg resource
  = ResourceCreated (Resource resource)
  | ResourceUpdated (Resource resource)
  | ResourceDeleted
  deriving stock Generic
deriving instance ToJSON (Resource resource) => ToJSON (ResourceMsg resource)
deriving instance FromJSON (Resource resource) => FromJSON (ResourceMsg resource)

instance (Typeable resource, ToJSON (ResourceMsg resource), FromJSON (ResourceMsg resource)) => Source (ResourceMsg resource) where
  data Stream (ResourceMsg resource) = ResourceStream (Slug resource)
    deriving stock Generic
    deriving anyclass Hashable

instance (Typeable resource, FromJSON (Resource resource), ToJSON (Resource resource)) => Aggregable (ResourceMsg resource) (Resource resource) where
  update (ResourceCreated r) _ = Update r
  update (ResourceUpdated r) (Just _) = Update r
  update ResourceDeleted (Just _) = Delete
  update _ _ = Ignore

data Index resource = Index
  { resources :: [Slug resource]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data IndexMsg resource
  = ResourceAdded (Slug resource)
  | ResourceRemoved (Slug resource)
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance (Typeable resource, ToJSON (IndexMsg resource), FromJSON (IndexMsg resource)) => Source (IndexMsg resource) where
  data Stream (IndexMsg resource) = IndexStream
    deriving stock Generic
    deriving anyclass Hashable

instance (Typeable resource, ToJSON (IndexMsg resource), FromJSON (IndexMsg resource)) => Aggregable (IndexMsg resource) (Index resource) where
  update (ResourceAdded k) Nothing = Update (Index [k])
  update (ResourceAdded k) (Just i) = Update i { resources = k : resources i }
  update (ResourceRemoved k) (Just i) = Update i { resources = List.filter (/= k) (resources i) }
  update _ _ = Ignore

resourceDB :: forall resource. 
              ( Typeable resource
              , IsResource resource 
              , ToJSON (Resource resource), FromJSON (Resource resource)
              ) => [Listener]
resourceDB = 
  [ listener @(ResourceMsg resource) @(Resource resource)
  , listener @(IndexMsg resource) @(Index resource)
  ]

--------------------------------------------------------------------------------
-- Sadly, polymorphic API endpoints can't currently be derived with 
-- mkRequest/mkMessage

data CreateResource resource
instance Identify (CreateResource resource)
instance (Typeable resource, IsResource resource) => Request (CreateResource resource) where
  type Req (CreateResource resource) = (Int,Resource resource)
  type Rsp (CreateResource resource) = Maybe Bool

createResource :: Proxy (CreateResource resource)
createResource = Proxy

data ReadResource resource
instance Identify (ReadResource resource)
instance (Typeable resource, IsResource resource) => Request (ReadResource resource) where
  type Req (ReadResource resource) = (Int,Slug resource)
  type Rsp (ReadResource resource) = Maybe (Resource resource)

readResource :: Proxy (ReadResource resource)
readResource = Proxy

data UpdateResource resource
instance Identify (UpdateResource resource)
instance (Typeable resource, IsResource resource) => Request (UpdateResource resource) where
  type Req (UpdateResource resource) = (Int,Resource resource)
  type Rsp (UpdateResource resource) = Maybe Bool

updateResource :: Proxy (UpdateResource resource)
updateResource = Proxy

data DeleteResource resource
instance Identify (DeleteResource resource)
instance (Typeable resource, IsResource resource) => Request (DeleteResource resource) where
  type Req (DeleteResource resource) = (Int,Slug resource)
  type Rsp (DeleteResource resource) = Maybe Bool

deleteResource :: Proxy (DeleteResource resource)
deleteResource = Proxy

data ListResources resource
instance Identify (ListResources resource)
instance (Typeable resource, IsResource resource) => Request (ListResources resource) where
  type Req (ListResources resource) = (Int,())
  type Rsp (ListResources resource) = Maybe [Slug resource]

listResources :: Proxy (ListResources resource)
listResources = Proxy

type ResourceCRUDL resource = 
  '[ CreateResource resource
   , ReadResource resource
   , UpdateResource resource
   , DeleteResource resource
   , ListResources resource
   ]

resourceAPI :: forall resource.  ( Typeable resource, IsResource resource ) => API _ (ResourceCRUDL resource)
resourceAPI = api msgs reqs
  where
    msgs = WS.none
    reqs = createResource @resource
       <:> readResource @resource
       <:> updateResource @resource
       <:> deleteResource @resource
       <:> listResources @resource
       <:> WS.none

data Permissions resource = Permissions
  { canCreate :: Slug resource -> IO Bool
  , canRead   :: Slug resource -> IO Bool 
  , canUpdate :: Slug resource -> IO Bool
  , canDelete :: Slug resource -> IO Bool
  , canList   :: IO Bool
  }

defaultPermissions :: forall resource. Typeable resource => Permissions resource
defaultPermissions =
  let tc = toTxt (show (typeRepTyCon (typeOf (undefined :: resource)))) 
  in Permissions 
      { canCreate = \slug -> logJSON ("Allowing creation of " <> tc,slug) >> pure True 
      , canRead   = \slug -> logJSON ("Allowing read of " <> tc,slug) >> pure True
      , canUpdate = \slug -> logJSON ("Allowing update of " <> tc,slug) >> pure True 
      , canDelete = \slug -> logJSON ("Allowing deletion of " <> tc,slug) >> pure True
      , canList   = logJSON ("Allowing listing of " <> tc) >> pure True
      }

data Callbacks resource = Callbacks
  { onCreate  :: Slug resource -> Maybe (Resource resource) -> IO ()
  , onRead    :: Slug resource -> Maybe (Resource resource) -> IO ()
  , onUpdate  :: Slug resource -> Maybe (Resource resource) -> IO ()
  , onDelete  :: Slug resource -> Bool -> IO ()
  , onList    :: IO ()
  }

defaultCallbacks :: forall resource. Typeable resource => Callbacks resource
defaultCallbacks =
  let tc = toTxt (show (typeRepTyCon (typeOf (undefined :: resource)))) 
  in Callbacks
      { onCreate = \slug _ -> logJSON ("Created " <> tc,slug)
      , onRead   = \slug _ -> logJSON ("Read " <> tc,slug)
      , onUpdate = \slug _ -> logJSON ("Updated " <> tc,slug)
      , onDelete = \slug success -> if success then logJSON ("Deleted " <> tc,slug) else pure ()
      , onList   = logJSON ("Listed " <> tc)
      }

resourceBackend :: ( Typeable resource, IsResource resource, ToJSON (Resource resource), FromJSON (Resource resource) )
                => Permissions resource -> Callbacks resource -> Endpoints '[] (ResourceCRUDL resource) '[] (ResourceCRUDL resource)
resourceBackend permissions callbacks = Endpoints resourceAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleCreateResource permissions callbacks
       <:> handleReadResource permissions callbacks
       <:> handleUpdateResource permissions callbacks
       <:> handleDeleteResource permissions callbacks
       <:> handleListResources permissions callbacks
       <:> WS.none

handleCreateResource 
  :: forall resource. (Typeable resource, IsResource resource, ToJSON (Resource resource), FromJSON (Resource resource) )
  => Permissions resource -> Callbacks resource -> RequestHandler (CreateResource resource)
handleCreateResource Permissions { canCreate } Callbacks { onCreate } = responding do
  resource :: Resource resource <- acquire
  let k = slug resource
  liftIO (print "Here")
  can <- liftIO (canCreate k)
  if can then do
    result <- Sorcerer.transact (ResourceStream k :: Stream (ResourceMsg resource)) (ResourceCreated resource)
    case result of
      Update (_ :: Resource resource) -> do
        Sorcerer.write (IndexStream @resource) (ResourceAdded k)
        reply (Just True)
        liftIO (onCreate k (Just resource))
      _ -> do
        liftIO (onCreate k Nothing)
        reply (Just False)
  else
    reply Nothing

handleReadResource 
  :: forall resource. (Typeable resource, IsResource resource, ToJSON (Resource resource), FromJSON (Resource resource) ) 
  => Permissions resource -> Callbacks resource -> RequestHandler (ReadResource resource)
handleReadResource Permissions { canRead } Callbacks { onRead } = responding do
  k :: Slug resource <- acquire
  can <- liftIO (canRead k)
  if can then do
    Sorcerer.read (ResourceStream k :: Stream (ResourceMsg resource)) >>= \case
      Just r -> do
        reply (Just r)
        liftIO (onRead k (Just r))
      _ -> do
        liftIO (onRead k Nothing)
        reply Nothing
  else
    reply Nothing

handleUpdateResource
  :: forall resource. (Typeable resource, IsResource resource, ToJSON (Resource resource), FromJSON (Resource resource) )
  => Permissions resource -> Callbacks resource -> RequestHandler (UpdateResource resource)
handleUpdateResource Permissions { canUpdate } Callbacks { onUpdate } = responding do
  resource :: Resource resource <- acquire
  let k = slug resource
  can <- liftIO (canUpdate k)
  if can then do
    result <- Sorcerer.transact (ResourceStream k :: Stream (ResourceMsg resource)) (ResourceUpdated resource)
    case result of
      Update (r :: Resource resource) -> do
        reply (Just True)
        liftIO (onUpdate k (Just r))
      _ -> do
        liftIO (onUpdate k Nothing)
        reply (Just False)
  else
    reply Nothing

handleDeleteResource
  :: forall resource. (Typeable resource, IsResource resource, ToJSON (Resource resource), FromJSON (Resource resource) )
  => Permissions resource -> Callbacks resource -> RequestHandler (DeleteResource resource)
handleDeleteResource Permissions { canDelete } Callbacks { onDelete } = responding do
  k :: Slug resource <- acquire
  can <- liftIO (canDelete k) 
  if can then do
    result <- Sorcerer.transact (ResourceStream k :: Stream (ResourceMsg resource)) ResourceDeleted
    case result of
      (Delete :: Maybe (Maybe (Resource resource))) -> do
        Sorcerer.write (IndexStream @resource) (ResourceRemoved k)
        liftIO (onDelete k True)
        reply (Just True)
      _ -> do
        liftIO (onDelete k False)
        reply (Just False)
  else do
    reply Nothing

handleListResources
  :: forall resource. (Typeable resource, IsResource resource )
  => Permissions resource -> Callbacks resource -> RequestHandler (ListResources resource)
handleListResources Permissions { canList } Callbacks { onList } = responding do
  can <- liftIO canList
  if can then do
    mr <- Sorcerer.read (IndexStream @resource) 
    reply (Just (maybe [] resources mr))
    liftIO onList
  else do
    reply Nothing

data ResourceRoute resource
  = CreateResource
  | ReadResource (Slug resource)
  | UpdateResource (Slug resource)
  | ListResources
  deriving stock (Eq)

instance Theme CreateResource where
  theme c =
    is c do
      has (tag Label) do
        text-transform =: capitalize
        
instance Theme UpdateResource where
  theme c =
    is c do
      has (tag Label) do
        text-transform =: capitalize

instance Theme ReadResource

instance Theme ListResources

resourceRoutes :: forall resource route. IsResource resource => (ResourceRoute resource -> route) -> Routing route _
resourceRoutes liftResource =
  path (root @resource) do
    let disp = dispatch . liftResource

    path "/new" do
      disp CreateResource

    path "/list" do
      disp ListResources

    path "/update/:slug" do
      slug <- "slug"
      disp (UpdateResource slug)

    path "/:slug" do
      slug <- "slug"
      disp (ReadResource slug)

resourceLocation :: forall resource. IsResource resource => ResourceRoute resource -> Txt
resourceLocation route = 
  root @resource <> 
    case route of
      CreateResource      -> "/new"
      ListResources       -> "/list"
      UpdateResource slug -> "/update/" <> toTxt slug
      ReadResource slug   -> "/" <> toTxt slug

ref :: (IsResource resource, HasFeatures a) => ResourceRoute resource -> a -> a
ref = lref . resourceLocation

goto :: IsResource resource => ResourceRoute resource -> IO ()
goto = Router.goto . resourceLocation

resourcePage :: forall _role resource. 
                ( Typeable _role
                , Typeable resource
                , IsResource resource, ToJSON (Resource resource), FromJSON (Resource resource)
                , Form (Resource resource)
                , Theme resource
                , Component (Resource resource)
                ) => WebSocket -> ResourceRoute resource -> View
resourcePage ws CreateResource =
  let onSubmit resource = void (sync (request (resourceAPI @resource) ws (createResource @resource) resource))
  in withToken @_role $ maybe "Not Authorized" (\_ -> Div <| Themed @CreateResource . Themed @resource |> [ form onSubmit ])
resourcePage ws ListResources = withToken @_role $ maybe "Not Authorized" (\_ -> producing producer (consuming (maybe Pure.Null consumer)))
  where
    producer = sync (request (resourceAPI @resource) ws (listResources @resource) ())
    consumer ss = 
      Table <| Themed @ListResources . Themed @resource |>
        ( Tr <||>
          [ Th <||> [ "Slug" ] 
          , Th <| ColSpan 2 |> [ "Actions" ]
          ] 
        : [ Tr <||>
            [ Td <||> [ txt s ]
            , Td <||> [ Button <| OnClick (\_ -> delete) |> [ "Delete" ] ]
            , Td <||> [ Button <| OnClick (\_ -> goto (UpdateResource s)) |> [ "Edit" ] ]
            ]
          | s <- ss 
          , let 
              delete = do
                sync (request (resourceAPI @resource) ws (deleteResource @resource) s) >>= \case
                  Just True -> goto (ListResources @resource)
                  _ -> pure ()
          ]
        )
resourcePage ws (UpdateResource s) = withToken @_role $ maybe "Not Authorized" (\_ -> producing producer (consuming consumer))
  where
    producer = sync (request (resourceAPI @resource) ws (readResource @resource) s)
    consumer = maybe "Not Found" (const "TODO: Update Resource")
resourcePage ws (ReadResource s) = withToken @_role $ maybe "Not Authorized" (\_ -> producing producer (consuming consumer))
  where
    producer = sync (request (resourceAPI @resource) ws (readResource @resource) s)
    consumer = maybe "Not Found" run
