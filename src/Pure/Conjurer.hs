module Pure.Conjurer (module Pure.Conjurer, module Export) where

import Pure.Conjurer.API as Export
import Pure.Conjurer.Callbacks as Export
import Pure.Conjurer.Context as Export
import Pure.Conjurer.Creatable as Export
import Pure.Conjurer.Fieldable as Export
import Pure.Conjurer.Formable as Export
import Pure.Conjurer.Index as Export
import Pure.Conjurer.Key as Export
import Pure.Conjurer.Listable as Export
import Pure.Conjurer.Listing as Export
import Pure.Conjurer.Pathable as Export
import Pure.Conjurer.Permissions as Export
import Pure.Conjurer.Previewable as Export
import Pure.Conjurer.Producible as Export
import Pure.Conjurer.Readable as Export
import Pure.Conjurer.Resource as Export
import Pure.Conjurer.Rootable as Export
import Pure.Conjurer.Slug as Export
import Pure.Conjurer.Updatable as Export

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Txt as Txt
import Pure.Elm.Component (View,HasFeatures,pattern OnTouchStart,pattern OnMouseDown)
import Pure.Router as Router (Routing,goto,lref)
import Pure.Sorcerer as Sorcerer hiding (Read,pattern Update)
import qualified Pure.Sorcerer as Sorcerer
import Pure.WebSocket as WS hiding (Index,identify)
import Pure.WebSocket.Cache

import Control.Concurrent
import Control.Monad
import Data.Typeable
import Data.Hashable
import GHC.Generics

import Prelude hiding (Read)

--------------------------------------------------------------------------------  

db :: forall a. 
    ( Typeable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a)
    ) => [Listener]
db = 
  [ listener @(ResourceMsg a) @(Resource a)
  , listener @(IndexMsg a) @(Index a)
  , listener @(ProductMsg a) @(Product a)
  , listener @(PreviewMsg a) @(Preview a)
  , listener @(ListingMsg a) @(Listing a)
  ]

tryCreate
  :: forall a. 
    ( Typeable a
    , Processable a, Previewable a, Producible a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a)
    ) => Callbacks a -> Context a -> Name a -> Resource a -> IO Bool
tryCreate Callbacks {..} ctx name a0 = do
  ma <- process a0
  case ma of
    Nothing -> pure False
    Just a -> do
      Sorcerer.observe (ResourceStream ctx name) (SetResource a) >>= \case
        Added (new :: Resource a) -> do
          pro <- produce new
          pre <- preview new pro
          (Sorcerer.Update (_ :: Product a)) <- Sorcerer.transact (ProductStream ctx name) (SetProduct pro)
          (Sorcerer.Update (_ :: Preview a)) <- Sorcerer.transact (PreviewStream ctx name) (SetPreview pre)
          Sorcerer.write (IndexStream @a) (ResourceAdded ctx name)
          (Sorcerer.Update (Listing (userListing :: [(Name a,Preview a)]))) <- 
            Sorcerer.transact (UserListingStream ctx) (SetPreviewItem name pre)
          onCreate ctx name new pro pre
          pure True
        _ ->
          pure False

tryUpdate 
  :: forall a. 
    ( Typeable a
    , Processable a, Previewable a, Producible a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a)
    ) => Callbacks a -> Context a -> Name a -> Resource a -> IO Bool
tryUpdate Callbacks {..} ctx name a0 = do
  ma <- process a0
  case ma of
    Nothing -> pure False
    Just a -> do
      Sorcerer.observe (ResourceStream ctx name) (SetResource a) >>= \case
        Updated _ (new :: Resource a) -> do
          pro <- produce new
          pre <- preview new pro
          (Sorcerer.Update (_ :: Product a)) <- Sorcerer.transact (ProductStream ctx name) (SetProduct pro)
          (Sorcerer.Update (_ :: Preview a)) <- Sorcerer.transact (PreviewStream ctx name) (SetPreview pre)
          (Sorcerer.Update (Listing (userListing :: [(Name a,Preview a)]))) <- 
            Sorcerer.transact (UserListingStream ctx) (SetPreviewItem name pre)
          onUpdate ctx name new pro pre
          pure True
        _ ->
          pure False

tryDelete 
  :: forall a. 
    ( Typeable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a)
    ) => Callbacks a -> Context a -> Name a -> IO Bool
tryDelete Callbacks {..} ctx name =
  Sorcerer.observe (ResourceStream ctx name) DeleteResource >>= \case
    Deleted r -> do
      Deleted pre <- Sorcerer.observe (PreviewStream ctx name) DeletePreview
      Deleted pro <- Sorcerer.observe (ProductStream ctx name) DeleteProduct
      (Sorcerer.Update (Listing (userListing :: [(Name a,Preview a)]))) <- 
        Sorcerer.transact (UserListingStream ctx) (DeletePreviewItem name)
      onDelete ctx name r pro pre
      pure True
    _ -> do
      pure False

tryReadResource
  :: forall a.
    ( Typeable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , Hashable (Context a), Pathable (Context a)
    , Hashable (Name a), Pathable (Name a)
    ) => Context a -> Name a -> IO (Maybe (Resource a))
tryReadResource ctx name = Sorcerer.read (ResourceStream ctx name)

tryReadPreview
  :: forall a.
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a)
    ) => Context a -> Name a -> IO (Maybe (Preview a))
tryReadPreview ctx name = Sorcerer.read (PreviewStream ctx name)

tryReadProduct
  :: forall a.
    ( Typeable a
    , ToJSON (Product a), FromJSON (Product a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a)
    ) => Context a -> Name a -> IO (Maybe (Product a))
tryReadProduct ctx name = Sorcerer.read (ProductStream ctx name)

tryReadListing
  :: forall a.
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , Pathable (Context a), Hashable (Context a)
    , ToJSON (Name a), FromJSON (Name a), Eq (Name a)
    ) => Context a -> IO (Maybe [(Name a,Preview a)])
tryReadListing ctx =
  Sorcerer.read (UserListingStream ctx) >>= \case
    Just (Listing ps) -> pure (Just ps)
    Nothing -> pure Nothing

--------------------------------------------------------------------------------

publishing :: 
  ( Typeable a
  , Processable a, Nameable a, Previewable a, Producible a 
  , ToJSON (Resource a), FromJSON (Resource a)
  , ToJSON (Product a), FromJSON (Product a)
  , ToJSON (Preview a), FromJSON (Preview a)
  , ToJSON (Context a), FromJSON (Context a)
  , ToJSON (Name a), FromJSON (Name a)
  , Pathable (Context a), Hashable (Context a)
  , Pathable (Name a), Hashable (Name a), Eq (Name a)
  ) => Permissions a -> Callbacks a 
    -> Endpoints '[] (PublishingAPI a) '[] (PublishingAPI a)
publishing ps cs = Endpoints publishingAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleCreateResource ps cs 
       <:> handleReadResource ps cs
       <:> handleUpdateResource ps cs
       <:> handleDeleteResource ps cs
       <:> WS.none

reading :: 
  ( Typeable a
  , ToJSON (Product a), FromJSON (Product a)
  , ToJSON (Preview a), FromJSON (Preview a)
  , ToJSON (Context a), FromJSON (Context a)
  , ToJSON (Name a), FromJSON (Name a)
  , Pathable (Context a), Hashable (Context a)
  , Pathable (Name a), Hashable (Name a), Eq (Name a)
  ) => Permissions a -> Callbacks a 
    -> Endpoints '[] (ReadingAPI a) '[] (ReadingAPI a)
reading ps cs = Endpoints readingAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleReadProduct ps cs 
       <:> handleReadPreview ps cs
       <:> handleReadListing ps cs
       <:> WS.none

handleCreateResource 
  :: forall a. 
    ( Typeable a
    , Processable a, Nameable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a)
    ) => Permissions a -> Callbacks a -> RequestHandler (CreateResource a)
handleCreateResource Permissions {..} callbacks = responding do
  (ctx,resource) <- acquire
  response <- liftIO do
    can <- canCreate ctx
    if can then do
      let name = toName resource
      tryCreate callbacks ctx name resource >>= \case
        True -> pure (Just name)
        _    -> pure Nothing
    else
      pure Nothing
  reply response

handleReadResource 
  :: forall a. 
    ( Typeable a
    , ToJSON (Resource a), FromJSON (Resource a) 
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Hashable (Context a), Pathable (Context a)
    , Hashable (Name a), Pathable (Name a)
    ) => Permissions a -> Callbacks a -> RequestHandler (ReadResource a)
handleReadResource Permissions {..} Callbacks {..} = responding do
  (ctx,name) <- acquire
  can <- liftIO (canRead ctx name)
  response <- 
    if can then do
      Sorcerer.read (ResourceStream ctx name) >>= \case
        Just r -> pure (Just r)
        _      -> pure Nothing
    else
      pure Nothing
  reply response

handleUpdateResource
  :: forall a. 
    ( Typeable a
    , Processable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a) 
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a)
    ) => Permissions a -> Callbacks a -> RequestHandler (UpdateResource a)
handleUpdateResource Permissions {..} callbacks = responding do
  (ctx,name,resource) <- acquire
  response <- liftIO do
    can <- canUpdate ctx name
    if can then
      Just <$> tryUpdate callbacks ctx name resource
    else
      pure Nothing
  reply response

handleDeleteResource
  :: forall a. 
    ( Typeable a
    , ToJSON (Resource a), FromJSON (Resource a) 
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a)
    ) => Permissions a -> Callbacks a -> RequestHandler (DeleteResource a)
handleDeleteResource Permissions {..} callbacks = responding do
  (ctx,name) <- acquire
  response <- liftIO do
    can <- canDelete ctx name
    if can then
      Just <$> tryDelete callbacks ctx name
    else do
      pure Nothing
  reply response

handleReadProduct
  :: forall a. 
    ( Typeable a
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a)
    ) => Permissions a -> Callbacks a -> RequestHandler (ReadProduct a)
handleReadProduct Permissions {..} Callbacks { onRead } = responding do
  (ctx,name) <- acquire
  response <- liftIO do
    can <- canRead ctx name
    if can then 
      tryReadProduct ctx name >>= \case
        Just p -> do
          onRead ctx name p
          pure (Just p)
        _ ->
          pure Nothing
    else 
      pure Nothing
  reply response

handleReadPreview
  :: forall a. 
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a)
    ) => Permissions a -> Callbacks a -> RequestHandler (ReadPreview a)
handleReadPreview Permissions {..} Callbacks {..} = responding do
  (ctx,name) <- acquire
  response <- liftIO do
    can <- canRead ctx name
    if can then 
      tryReadPreview ctx name 
    else 
      pure Nothing
  reply response

handleReadListing
  :: forall a. 
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Eq (Name a)
    ) => Permissions a -> Callbacks a -> RequestHandler (ReadListing a)
handleReadListing Permissions {..} Callbacks {..} = responding do
  ctx <- acquire
  response <- liftIO do
    can <- canList ctx
    if can then 
      tryReadListing ctx >>= \case
        Just ps -> do
          onList ctx ps
          pure (Just ps)
        _ ->
          pure Nothing
    else 
      pure Nothing
  reply response

--------------------------------------------------------------------------------

data Route _role a
  = CreateR (Context a)
  | UpdateR (Context a) (Name a)
  | ReadR (Context a) (Name a)
  | ListR (Context a)
  deriving stock Generic
  
deriving instance (Ord (Context a), Ord (Name a)) => Ord (Route _role a)
deriving instance (Eq (Context a), Eq (Name a)) => Eq (Route _role a)
deriving instance (Show (Context a), Show (Name a)) => Show (Route _role a)
deriving instance (ToJSON (Context a), ToJSON (Name a)) => ToJSON (Route _role a)
deriving instance (FromJSON (Context a), FromJSON (Name a)) => FromJSON (Route _role a)

pages 
  :: forall _role a.  
    ( Readable a, Creatable _role a, Updatable _role a, Listable a
    ) => WebSocket -> Route _role a -> View
pages ws = \case
  CreateR ctx    -> toCreate ws ctx
  UpdateR ctx nm -> toUpdate ws ctx nm
  ReadR   ctx nm -> toRead ws ctx nm
  ListR   ctx    -> toList ws ctx

routes 
  :: forall _role a route. 
    ( Typeable a
    , Readable a, Creatable _role a, Updatable _role a, Listable a 
    ) => (Route _role a -> route) -> Routing route ()
routes lift = do
  listRoute (\ctx -> lift (ListR ctx))
  updateRoute (\ctx nm -> lift (UpdateR ctx nm))
  createRoute (\ctx -> lift (CreateR ctx))
  readRoute (\ctx nm -> lift (ReadR ctx nm))

location 
  :: forall _role a. 
    ( Typeable a
    , Readable a, Creatable _role a, Updatable _role a, Listable a
    ) => Route _role a -> Txt
location = \case
  CreateR ctx    -> toCreateRoute ctx
  UpdateR ctx nm -> toUpdateRoute ctx nm
  ReadR ctx nm   -> toReadRoute ctx nm
  ListR ctx      -> toListRoute ctx

ref 
  :: forall _role a v. 
    ( Typeable a
    , Readable a, Creatable _role a, Updatable _role a, Listable a
    , HasFeatures v
    ) => Route _role a -> v -> v
ref = lref . location

goto 
  :: forall _role a. 
    ( Typeable a
    , Readable a, Creatable _role a, Updatable _role a, Listable a
    ) => Route _role a -> IO ()
goto = Router.goto . location

preload
  :: forall _role a v.
    ( Typeable a
    , Readable a, Creatable _role a, Updatable _role a, Listable a
    , ToJSON (Name a), FromJSON (Name a), Ord (Name a)
    , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
    , FromJSON (Product a)
    , FromJSON (Preview a)
    , HasFeatures v
    ) => Route _role a -> v -> v
preload rt = OnMouseDown load . OnTouchStart load
  where
    load _ = case rt of
      ReadR ctx nm -> 
        void $ forkIO $ void $ do
          req Cached (readingAPI @a)
            (readProduct @a) 
            (ctx,nm)

      ListR ctx ->
        void $ forkIO $ void $ do
          req Cached (readingAPI @a)
            (readListing @a) 
            ctx
            
      _ ->
        pure ()
