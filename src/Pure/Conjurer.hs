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
import Pure.Conjurer.Name as Export 
import Pure.Conjurer.Pathable as Export
import Pure.Conjurer.Permissions as Export
import Pure.Conjurer.Previewable as Export
import Pure.Conjurer.Previews as Export
import Pure.Conjurer.Producible as Export
import Pure.Conjurer.Readable as Export
import Pure.Conjurer.Resource as Export
import Pure.Conjurer.Rootable as Export
import Pure.Conjurer.Routable as Export 
import Pure.Conjurer.Slug as Export
import Pure.Conjurer.Updatable as Export

import Pure.Data.JSON (ToJSON(..),FromJSON(..),encodeBS,decodeBS)
import Pure.Data.Render ()
import Pure.Data.Txt as Txt
import Pure.Elm.Application (storeScrollPosition)
import Pure.Elm.Component (View,HasFeatures,pattern OnClickWith,intercept,pattern Href,pattern OnTouchStart,pattern OnMouseDown,Theme(..),pattern Themed,(|>),(<|),pattern Div,Component(run))
import Pure.Router as Router (Routing,goto)
import Pure.Sorcerer as Sorcerer hiding (Read,pattern Update)
import qualified Pure.Sorcerer as Sorcerer
import Pure.WebSocket as WS hiding (Index,identify)
import Pure.WebSocket.Cache

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict as Map
import Data.Set as Set
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Hashable
import Data.IORef
import Data.Maybe
import Data.Typeable
import GHC.Exts
import GHC.Generics
import System.IO.Unsafe
import Unsafe.Coerce

import Prelude hiding (Read)

--------------------------------------------------------------------------------  

db :: forall a. 
    ( Typeable a
    , Amendable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => [Listener]
db = 
  [ listener @(ResourceMsg a) @(Resource a)
  , listener @(IndexMsg a) @(Index a)
  , listener @(ProductMsg a) @(Product a)
  , listener @(PreviewMsg a) @(Preview a)
  , listener @(PreviewsMsg a) @(Previews a)
  ]

type Queue = TMVar (TQueue (IO ()))

{-# NOINLINE lockTable #-}
lockTable :: IORef (Map TypeRep (TMVar (Map Any Queue)))
lockTable = unsafePerformIO (newIORef mempty)

getResourceLocks :: forall a. (Ord (Context a), Ord (Name a), Typeable a) => IO (TMVar (Map Any Queue))
getResourceLocks =
  atomicModifyIORef' lockTable $ \lt ->
    let tr = typeOf (undefined :: a) in
    case Map.lookup tr lt of
      Just rqs -> (lt,rqs)
      Nothing -> unsafePerformIO do
        rqs <- newTMVarIO (unsafeCoerce (mempty :: Map (Context a,Name a) Queue))
        pure (Map.insert tr rqs lt,rqs)

withLock :: forall a b. (Ord (Context a), Ord (Name a), Typeable a) => Context a -> Name a -> IO b -> IO b
withLock ctx nm f = getResourceLocks @a >>= start
  where 
    cleanup rqs_ = do
      join $ atomically do
        rqs <- takeTMVar rqs_
        case Map.lookup (ctx,nm) (unsafeCoerce rqs) of
          Just q -> do
            tq <- takeTMVar q 
            x <- tryReadTQueue tq
            case x of
              Nothing -> do
                putTMVar rqs_ (unsafeCoerce (Map.delete (ctx,nm) (unsafeCoerce rqs)))
                pure (pure ())
              Just next -> do
                putTMVar q tq
                putTMVar rqs_ rqs
                pure next

    start rqs_ = do
      join $ atomically do
        rqs <- takeTMVar rqs_
        case Map.lookup (ctx,nm) (unsafeCoerce rqs) of
          Just q -> do
            tq <- takeTMVar q
            mv <- newEmptyTMVar
            writeTQueue tq do
              atomically do 
                putTMVar mv ()
            putTMVar q tq
            putTMVar rqs_ rqs
            pure do
              atomically (takeTMVar mv)
              f `finally` cleanup rqs_

          Nothing -> do
            q <- newTMVar =<< newTQueue
            let rqs' = unsafeCoerce (Map.insert (ctx,nm) (unsafeCoerce rqs))
            putTMVar rqs_ rqs'
            pure do
              f `finally` cleanup rqs_

tryCreate
  :: forall a. 
    ( Typeable a
    , Processable a, Amendable a, Nameable a, Previewable a, Producible a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> Context a -> Resource a -> IO (Maybe (Name a,Product a,Preview a,[(Name a,Preview a)]))
tryCreate Permissions {..} Callbacks {..} ctx a0 = do
  ma <- process False a0
  case ma of
    Nothing -> pure Nothing
    Just a -> do
      let name = toName a
      can <- canCreate ctx name
      if can then do
        withLock ctx name do
          Sorcerer.observe (ResourceStream ctx name) (SetResource a) >>= \case
            Added (new :: Resource a) -> do
              pro <- produce False new
              pre <- preview False new pro
              (Sorcerer.Update (_ :: Product a)) <- Sorcerer.transact (ProductStream ctx name) (SetProduct pro)
              (Sorcerer.Update (_ :: Preview a)) <- Sorcerer.transact (PreviewStream ctx name) (SetPreview pre)
              Sorcerer.write (IndexStream @a) (ResourceAdded ctx name)
              (Sorcerer.Update (Previews (previews :: [(Name a,Preview a)]))) <- 
                Sorcerer.transact (PreviewsStream ctx) (SetPreviewItem name pre)
              onCreate ctx name new pro pre
              pure (Just (name,pro,pre,previews))
            _ ->
              pure Nothing
      else
        pure Nothing

tryUpdate 
  :: forall a. 
    ( Typeable a
    , Processable a, Amendable a, Nameable a, Previewable a, Producible a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> Resource a -> IO (Maybe (Product a,Preview a,[(Name a,Preview a)]))
tryUpdate Permissions {..} Callbacks {..} ctx name a0 = do
  ma <- process False a0
  case ma of
    Nothing -> pure Nothing
    Just a -> do
      can <- canUpdate ctx name
      if can then do
        withLock ctx name do
          Sorcerer.transact (ResourceStream ctx name) (SetResource a) >>= \case
            Sorcerer.Update (new :: Resource a) -> do
              pro <- produce False new 
              pre <- preview False new pro
              (Sorcerer.Update (_ :: Product a)) <- Sorcerer.transact (ProductStream ctx name) (SetProduct pro)
              (Sorcerer.Update (_ :: Preview a)) <- Sorcerer.transact (PreviewStream ctx name) (SetPreview pre)
              (Sorcerer.Update (Previews (previews :: [(Name a,Preview a)]))) <- 
                Sorcerer.transact (PreviewsStream ctx) (SetPreviewItem name pre)
              onUpdate ctx name new pro pre
              pure (Just (pro,pre,previews))
            _ ->
              pure Nothing
      else
        pure Nothing

tryAmend 
  :: forall a. 
    ( Typeable a
    , Amendable a, Previewable a, Producible a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> Amend a -> IO (Maybe (Product a,Preview a,[(Name a,Preview a)]))
tryAmend Permissions {..} Callbacks {..} ctx name a = do
  can <- canUpdate ctx name
  if can then
    withLock ctx name do
      Sorcerer.transact (ResourceStream ctx name) (AmendResource a) >>= \case
        Sorcerer.Update (new :: Resource a) -> do
          pro <- produce False new
          pre <- preview False new pro
          (Sorcerer.Update (_ :: Product a)) <- Sorcerer.transact (ProductStream ctx name) (SetProduct pro)
          (Sorcerer.Update (_ :: Preview a)) <- Sorcerer.transact (PreviewStream ctx name) (SetPreview pre)
          (Sorcerer.Update (Previews (previews :: [(Name a,Preview a)]))) <- 
            Sorcerer.transact (PreviewsStream ctx) (SetPreviewItem name pre)
          onUpdate ctx name new pro pre
          pure (Just (pro,pre,previews))
        _ ->
          pure Nothing
  else
    pure Nothing

tryDelete 
  :: forall a. 
    ( Typeable a
    , Amendable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> IO (Maybe (Product a,Preview a,[(Name a,Preview a)]))
tryDelete Permissions {..} Callbacks {..} ctx name = do
  can <- canDelete ctx name
  if can then do
    withLock ctx name do
      Sorcerer.observe (ResourceStream ctx name) DeleteResource >>= \case
        Deleted r -> do
          Deleted pre <- Sorcerer.observe (PreviewStream ctx name) DeletePreview
          Deleted pro <- Sorcerer.observe (ProductStream ctx name) DeleteProduct
          (Sorcerer.Update (Previews (previews :: [(Name a,Preview a)]))) <- 
            Sorcerer.transact (PreviewsStream ctx) (DeletePreviewItem name)
          onDelete ctx name r pro pre
          pure (Just (pro,pre,previews))
        _ -> do
          pure Nothing
  else
    pure Nothing

tryReadResource
  :: forall a.
    ( Typeable a
    , Amendable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , Hashable (Context a), Pathable (Context a)
    , Hashable (Name a), Pathable (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> IO (Maybe (Resource a))
tryReadResource Permissions {..} Callbacks {..} ctx name = do
  can <- canRead ctx name
  if can then do
    mres <- Sorcerer.read (ResourceStream ctx name)
    case mres of
      Nothing -> pure Nothing
      Just res -> do
        onResource ctx name res
        pure (Just res)
  else
    pure Nothing

tryReadPreview
  :: forall a.
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> IO (Maybe (Preview a))
tryReadPreview Permissions {..} Callbacks {..} ctx name = do
  can <- canRead ctx name
  if can then do
    mpre <- Sorcerer.read (PreviewStream ctx name)
    case mpre of
      Nothing -> pure Nothing
      Just pre -> do
        onPreview ctx name pre
        pure (Just pre)
  else
    pure Nothing

tryReadProduct
  :: forall a.
    ( Typeable a
    , ToJSON (Product a), FromJSON (Product a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> IO (Maybe (Product a))
tryReadProduct Permissions {..} Callbacks {..} ctx name = do
  can <- canRead ctx name
  if can then do
    mpro <- Sorcerer.read (ProductStream ctx name)
    case mpro of
      Nothing -> pure Nothing
      Just pro -> do
        onRead ctx name pro
        pure (Just pro)
  else
    pure Nothing

tryReadListing
  :: forall a.
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , Pathable (Context a), Hashable (Context a)
    , ToJSON (Name a), FromJSON (Name a), Eq (Name a)
    ) => Permissions a -> Callbacks a -> Context a -> IO (Maybe [(Name a,Preview a)])
tryReadListing Permissions {..} Callbacks {..} ctx = do
  can <- canList ctx
  if can then do
    mps <- Sorcerer.read (PreviewsStream ctx) >>= \case
      Just (Previews ps) -> pure (Just ps)
      Nothing -> pure Nothing
    case mps of
      Nothing -> pure Nothing
      Just ps -> do
        onList ctx ps
        pure (Just ps)
  else
    pure Nothing

--------------------------------------------------------------------------------

publishing :: 
  ( Typeable a
  , Processable a, Amendable a, Nameable a, Previewable a, Producible a 
  , ToJSON (Resource a), FromJSON (Resource a)
  , ToJSON (Product a), FromJSON (Product a)
  , ToJSON (Preview a), FromJSON (Preview a)
  , ToJSON (Context a), FromJSON (Context a)
  , ToJSON (Name a), FromJSON (Name a)
  , Pathable (Context a), Hashable (Context a), Ord (Context a)
  , Pathable (Name a), Hashable (Name a), Eq (Name a), Ord (Name a)
  , FromJSON (Amend a), ToJSON (Amend a)
  ) => Permissions a -> Callbacks a 
    -> Endpoints '[] (PublishingAPI a) '[] (PublishingAPI a)
publishing ps cs = Endpoints publishingAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleCreateResource ps cs 
       <:> handleReadResource ps cs
       <:> handleUpdateResource ps cs
       <:> handleDeleteResource ps cs
       <:> handlePreviewResource ps cs
       <:> handleAmendResource ps cs
       <:> handlePreviewAmendResource ps cs
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
    , Processable a, Amendable a, Nameable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> RequestHandler (CreateResource a)
handleCreateResource permissions callbacks = responding do
  (ctx,resource) <- acquire
  response <- liftIO do
    tryCreate permissions callbacks ctx resource >>= \case
      Just (name,_,_,_) -> pure (Just name)
      _ -> pure Nothing
  reply response

handleReadResource 
  :: forall a. 
    ( Typeable a
    , Amendable a
    , ToJSON (Resource a), FromJSON (Resource a) 
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Hashable (Context a), Pathable (Context a)
    , Hashable (Name a), Pathable (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> RequestHandler (ReadResource a)
handleReadResource permissions callbacks = responding do
  (ctx,name) <- acquire
  response <- liftIO (tryReadResource permissions callbacks ctx name)
  reply response

handleUpdateResource
  :: forall a. 
    ( Typeable a
    , Processable a, Amendable a, Nameable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a) 
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> RequestHandler (UpdateResource a)
handleUpdateResource permissions callbacks = responding do
  (ctx,name,resource) <- acquire
  response <- liftIO do
    result <- tryUpdate permissions callbacks ctx name resource
    pure (isJust result)
  reply response

handleDeleteResource
  :: forall a. 
    ( Typeable a
    , Amendable a
    , ToJSON (Resource a), FromJSON (Resource a) 
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> RequestHandler (DeleteResource a)
handleDeleteResource permissions callbacks = responding do
  (ctx,name) <- acquire
  response <- liftIO do
     result <- tryDelete permissions callbacks ctx name
     pure (isJust result)
  reply response

handlePreviewResource
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
    ) => Permissions a -> Callbacks a -> RequestHandler (PreviewResource a)
handlePreviewResource permissions callbacks = responding do
  (ctx,res0) <- acquire
  response <- liftIO do
    -- I don't like the ordering here, but process 
    -- needs to run before canCreate because it 
    -- may seed/alter the Context and/or Name.
    mres <- process True res0
    case mres of
      Nothing -> pure Nothing
      Just res -> do
        let name = toName res
        can <- canCreate permissions ctx name
        if can then do
          pro <- produce True res
          pre <- preview True res pro
          pure (Just (ctx,name,pre,pro,res))
        else
          pure Nothing
  reply response

handlePreviewAmendResource
  :: forall a. 
    ( Typeable a
    , Amendable a, Nameable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a)
    , ToJSON (Amend a), FromJSON (Amend a)
    ) => Permissions a -> Callbacks a -> RequestHandler (PreviewAmendResource a)
handlePreviewAmendResource permissions callbacks = responding do
  (ctx,name,a) <- acquire
  response <- liftIO do 
    tryReadResource permissions callbacks ctx name >>= \case
      Nothing -> pure Nothing
      Just resource -> do
        can <- canUpdate permissions ctx name
        if can then do
          let res = amend a resource
          pro <- produce True res
          pre <- preview True res pro
          pure (Just (ctx,name,pre,pro,res))
        else
          pure Nothing
  reply response

handleAmendResource
  :: forall a. 
    ( Typeable a
    , Amendable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a) 
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> RequestHandler (AmendResource a)
handleAmendResource permissions callbacks = responding do
  (ctx,name,amend) <- acquire
  response <- liftIO do
    result <- tryAmend permissions callbacks ctx name amend
    pure (isJust result)
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
handleReadProduct permissions callbacks = responding do
  (ctx,name) <- acquire
  response <- liftIO (tryReadProduct permissions callbacks ctx name)
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
handleReadPreview permissions callbacks = responding do
  (ctx,name) <- acquire
  response <- liftIO (tryReadPreview permissions callbacks ctx name)
  reply response

handleReadListing
  :: forall a. 
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Eq (Name a)
    ) => Permissions a -> Callbacks a -> RequestHandler (ReadListing a)
handleReadListing permissions callbacks = responding do
  ctx <- acquire
  response <- liftIO (tryReadListing permissions callbacks ctx)
  reply response

--------------------------------------------------------------------------------

data ResponseMap = ResponseMap (Map Any (IORef ByteString))

data Cache = Cache 
  { previews :: IORef (Map TypeRep (IORef ResponseMap))
  , products :: IORef (Map TypeRep (IORef ResponseMap))
  , listings :: IORef (Map TypeRep (IORef ResponseMap))
  }

{-# NOINLINE conjurerCache #-}
conjurerCache :: Cache 
conjurerCache = unsafePerformIO do
  Cache 
    <$> newIORef Map.empty 
    <*> newIORef Map.empty
    <*> newIORef Map.empty

cache 
  :: forall resource. 
    ( Typeable resource
    , ToJSON (Product resource), FromJSON (Product resource)
    , ToJSON (Preview resource), FromJSON (Preview resource)
    , ToJSON (Context resource), FromJSON (Context resource)
    , ToJSON (Name resource), FromJSON (Name resource)
    , Ord (Name resource), Hashable (Name resource), Pathable (Name resource)
    , Ord (Context resource), Hashable (Context resource), Pathable (Context resource)
    ) => IO ()
cache = do
  contexts <- newIORef Set.empty

  Export.iterate @resource $ \ctx nm -> do

    Sorcerer.read (PreviewStream ctx nm) >>= 
      traverse_ (cachePreview ctx nm :: Preview resource -> IO ())

    Sorcerer.read (ProductStream ctx nm) >>= 
      traverse_ (cacheProduct ctx nm :: Product resource -> IO ())

    modifyIORef contexts (Set.insert ctx)

  cs <- readIORef contexts

  for_ cs $ \ctx -> do

    Sorcerer.read (PreviewsStream ctx) >>= \case
      Just (Previews nps :: Previews resource) -> cacheListing ctx nps
      _ -> pure ()

cacheProduct 
  :: forall resource.
    ( Typeable resource
    , ToJSON (Product resource)
    , Ord (Context resource), Ord (Name resource)
    ) => Context resource -> Name resource -> Product resource -> IO ()
cacheProduct ctx nm (encodeBS . Just -> !pro) = do
  -- Don't look; I'm hideous!
  let ty = typeOf (undefined :: ReadProduct resource)
  atomicModifyIORef' (products conjurerCache) $ \m ->
    case Map.lookup ty m of
      Nothing -> unsafePerformIO do
        pro_ <- newIORef pro
        rm_ <- newIORef (ResponseMap $ unsafeCoerce (Map.singleton (ctx,nm) pro_))
        pure (Map.insert ty rm_ m,())
      Just rm_ -> unsafePerformIO do
        modifyIORef rm_ $ \x@(ResponseMap rm) -> unsafePerformIO do
          case Map.lookup (ctx,nm) (unsafeCoerce rm) of
            Nothing -> do
              pro_ <- newIORef pro
              pure $ ResponseMap $ unsafeCoerce (Map.insert (ctx,nm) pro_ (unsafeCoerce rm))
            Just pro_ -> do
              writeIORef pro_ pro
              pure x         
        pure (m,())

deleteProduct
  :: forall resource.
    ( Typeable resource
    , Ord (Context resource), Ord (Name resource)
    ) => Context resource -> Name resource -> IO ()
deleteProduct ctx nm = do
   -- Don't look; I'm hideous!
  let ty = typeOf (undefined :: ReadProduct resource)
  atomicModifyIORef' (products conjurerCache) $ \m -> 
    case Map.lookup ty m of
      Nothing -> (m,())
      Just rm_ -> unsafePerformIO do
        modifyIORef rm_ $ \(ResponseMap rm) -> 
          ResponseMap $ unsafeCoerce (Map.delete (ctx,nm) (unsafeCoerce rm))
        pure (m,())

tryReadProductFromCache
  :: forall a.
    ( Typeable a
    , Ord (Context a), Ord (Name a)
    , FromJSON (Product a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> IO (Maybe ByteString)
tryReadProductFromCache Permissions {..} Callbacks {..} ctx nm = do
  can <- canRead ctx nm
  if can then do
    let ty = typeOf (undefined :: ReadProduct a)
    ps <- readIORef (products conjurerCache) 
    case Map.lookup ty ps of
      Nothing -> pure Nothing
      Just pros_ -> do
        ResponseMap pros <- readIORef pros_
        case Map.lookup (ctx,nm) (unsafeCoerce pros) of
          Just bs_ -> do
            bs <- readIORef bs_
            onRead ctx nm (fromJust (decodeBS bs)) -- this will error if `decode . encode /= Just`
            pure (Just bs)
          _ -> 
            pure Nothing
  else
    pure Nothing

cachePreview
  :: forall resource.
    ( Typeable resource
    , ToJSON (Preview resource)
    , Ord (Context resource), Ord (Name resource)
    ) => Context resource -> Name resource -> Preview resource -> IO ()
cachePreview ctx nm (encodeBS . Just -> !pre) = do
  -- Don't look; I'm hideous!
  let ty = typeOf (undefined :: ReadPreview resource)
  atomicModifyIORef' (Pure.Conjurer.previews conjurerCache) $ \m -> 
    case Map.lookup ty m of
      Nothing -> unsafePerformIO do
        pre_ <- newIORef pre
        rm_ <- newIORef (ResponseMap $ unsafeCoerce (Map.singleton (ctx,nm) pre_))
        pure (Map.insert ty rm_ m,())
      Just rm_ -> unsafePerformIO do
        modifyIORef rm_ $ \x@(ResponseMap rm) -> unsafePerformIO do
          case Map.lookup (ctx,nm) (unsafeCoerce rm) of
            Nothing -> do
              pre_ <- newIORef pre
              pure $ ResponseMap $ unsafeCoerce (Map.insert (ctx,nm) pre_ (unsafeCoerce rm))
            Just pre_ -> do
              writeIORef pre_ pre
              pure x
        pure (m,())

deletePreview
  :: forall resource.
    ( Typeable resource
    , Ord (Context resource), Ord (Name resource)
    ) => Context resource -> Name resource -> IO ()
deletePreview ctx nm = do
   -- Don't look; I'm hideous!
  let ty = typeOf (undefined :: ReadPreview resource)
  atomicModifyIORef' (Pure.Conjurer.previews conjurerCache) $ \m -> 
    case Map.lookup ty m of
      Nothing -> (m,())
      Just rm_ -> unsafePerformIO do
        modifyIORef rm_ $ \(ResponseMap rm) -> 
          ResponseMap $ unsafeCoerce (Map.delete (ctx,nm) (unsafeCoerce rm))
        pure (m,())

tryReadPreviewFromCache
  :: forall a.
    ( Typeable a
    , Ord (Context a), Ord (Name a)
    , FromJSON (Preview a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> IO (Maybe ByteString)
tryReadPreviewFromCache Permissions {..} Callbacks {..} ctx nm = do
  can <- canRead ctx nm
  if can then do
    let ty = typeOf (undefined :: ReadPreview a)
    ps <- readIORef (Pure.Conjurer.previews conjurerCache) 
    case Map.lookup ty ps of
      Nothing -> pure Nothing
      Just pros_ -> do
        ResponseMap pres <- readIORef pros_
        case Map.lookup (ctx,nm) (unsafeCoerce pres) of
          Just bs_ -> do
            bs <- readIORef bs_
            onPreview ctx nm (fromJust (decodeBS bs)) -- this will error if `decode . encode /= Just`
            pure (Just bs)
          _ -> 
            pure Nothing
  else
    pure Nothing

cacheListing
  :: forall resource.
    ( Typeable resource
    , ToJSON (Preview resource)
    , ToJSON (Name resource)
    , Ord (Context resource)
    ) => Context resource -> [(Name resource,Preview resource)] -> IO ()
cacheListing ctx (encodeBS . Just -> !nps) = do
  -- Don't look; I'm hideous!
  let ty = typeOf (undefined :: ReadListing resource)
  atomicModifyIORef' (listings conjurerCache) $ \m -> 
    case Map.lookup ty m of
      Nothing -> unsafePerformIO do
        nps_ <- newIORef nps
        rm_ <- newIORef (ResponseMap $ unsafeCoerce (Map.singleton ctx nps_))
        pure (Map.insert ty rm_ m,())
      Just rm_ -> unsafePerformIO do
        modifyIORef rm_ $ \x@(ResponseMap rm) -> unsafePerformIO do
          case Map.lookup ctx (unsafeCoerce rm) of
            Nothing -> do
              nps_ <- newIORef nps
              pure $ ResponseMap $ unsafeCoerce (Map.insert ctx nps_ (unsafeCoerce rm))
            Just nps_ -> do
              writeIORef nps_ nps
              pure x
        pure (m,())

tryReadListingFromCache
  :: forall a.
    ( Typeable a
    , Ord (Context a)
    , FromJSON (Preview a)
    , FromJSON (Name a)
    ) => Permissions a -> Callbacks a -> Context a -> IO (Maybe ByteString)
tryReadListingFromCache Permissions {..} Callbacks {..} ctx = do
  can <- canList ctx
  if can then do
    let ty = typeOf (undefined :: ReadListing a)
    ls <- readIORef (listings conjurerCache) 
    case Map.lookup ty ls of
      Nothing -> pure Nothing
      Just lsts_ -> do
        ResponseMap lsts <- readIORef lsts_
        case Map.lookup ctx (unsafeCoerce lsts) of
          Just bs_ -> do
            bs <- readIORef bs_
            onList ctx (fromJust (decodeBS bs)) -- this will error if `decode . encode /= Just`
            pure (Just bs)
          _ -> 
            pure Nothing
  else
    pure Nothing

cachingPublishing :: 
  ( Typeable a
  , Processable a, Amendable a, Nameable a, Previewable a, Producible a 
  , ToJSON (Resource a), FromJSON (Resource a)
  , ToJSON (Product a), FromJSON (Product a)
  , ToJSON (Preview a), FromJSON (Preview a)
  , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
  , ToJSON (Name a), FromJSON (Name a), Ord (Name a)
  , Pathable (Context a), Hashable (Context a)
  , Pathable (Name a), Hashable (Name a), Eq (Name a)
  , FromJSON (Amend a), ToJSON (Amend a)
  ) => Permissions a -> Callbacks a 
    -> Endpoints '[] (PublishingAPI a) '[] (PublishingAPI a)
cachingPublishing ps cs = Endpoints publishingAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleCachingCreateResource ps cs 
       <:> handleReadResource ps cs
       <:> handleCachingUpdateResource ps cs
       <:> handleCachingDeleteResource ps cs
       <:> handlePreviewResource ps cs
       <:> handleCachingAmendResource ps cs
       <:> handlePreviewAmendResource ps cs
       <:> WS.none

cachingReading :: 
  ( Typeable a
  , ToJSON (Product a), FromJSON (Product a)
  , ToJSON (Preview a), FromJSON (Preview a)
  , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
  , ToJSON (Name a), FromJSON (Name a), Ord (Name a)
  , Pathable (Context a), Hashable (Context a)
  , Pathable (Name a), Hashable (Name a), Eq (Name a)
  ) => Permissions a -> Callbacks a 
    -> Endpoints '[] (ReadingAPI a) '[] (ReadingAPI a)
cachingReading ps cs = Endpoints readingAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleCachingReadProduct ps cs 
       <:> handleCachingReadPreview ps cs
       <:> handleCachingReadListing ps cs
       <:> WS.none

handleCachingCreateResource 
  :: forall a. 
    ( Typeable a
    , Processable a, Amendable a, Nameable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Ord (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> RequestHandler (CreateResource a)
handleCachingCreateResource permissions callbacks = responding do
  (ctx,resource) <- acquire
  response <- liftIO do
    tryCreate permissions callbacks ctx resource >>= \case
      Just (name,pro,pre,lst) -> do
        cacheProduct ctx name pro
        cachePreview ctx name pre
        cacheListing ctx lst
        pure (Just name)
      _ -> 
        pure Nothing
  reply response

handleCachingUpdateResource
  :: forall a. 
    ( Typeable a
    , Processable a, Amendable a, Nameable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a) 
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> RequestHandler (UpdateResource a)
handleCachingUpdateResource permissions callbacks = responding do
  (ctx,name,resource) <- acquire
  response <- liftIO do
    result <- tryUpdate permissions callbacks ctx name resource
    case result of
      Nothing -> pure False
      Just (pro,pre,lst) -> do
        cacheProduct ctx name pro
        cachePreview ctx name pre
        cacheListing ctx lst
        pure True
  reply response

handleCachingDeleteResource
  :: forall a. 
    ( Typeable a
    , Amendable a
    , ToJSON (Resource a), FromJSON (Resource a) 
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Ord (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> RequestHandler (DeleteResource a)
handleCachingDeleteResource permissions callbacks = responding do
  (ctx,name) <- acquire
  response <- liftIO do
     result <- tryDelete permissions callbacks ctx name
     case result of
       Nothing -> pure False
       Just (pro,pre,lst) -> do
        deleteProduct ctx name
        deletePreview ctx name
        cacheListing ctx lst
        pure True
  reply response

handleCachingAmendResource
  :: forall a. 
    ( Typeable a
    , Amendable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a) 
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> RequestHandler (AmendResource a)
handleCachingAmendResource permissions callbacks = responding do
  (ctx,name,amend) <- acquire
  response <- liftIO do
    result <- tryAmend permissions callbacks ctx name amend
    case result of
      Nothing -> pure False
      Just (pro,pre,lst) -> do
        cacheProduct ctx name pro
        cachePreview ctx name pre
        cacheListing ctx lst
        pure True
  reply response

handleCachingReadProduct
  :: forall a. 
    ( Typeable a
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Ord (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a)
    ) => Permissions a -> Callbacks a -> RequestHandler (ReadProduct a)
handleCachingReadProduct permissions callbacks = responding do
  (ctx,name) <- acquire
  response <- liftIO (tryReadProductFromCache permissions callbacks ctx name)
  case response of
    Just rsp -> customReplyRaw rsp
    Nothing -> reply Nothing

handleCachingReadPreview
  :: forall a. 
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Ord (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a)
    ) => Permissions a -> Callbacks a -> RequestHandler (ReadPreview a)
handleCachingReadPreview permissions callbacks = responding do
  (ctx,name) <- acquire
  response <- liftIO (tryReadPreviewFromCache permissions callbacks ctx name)
  case response of
    Just rsp -> customReplyRaw rsp
    Nothing  -> reply Nothing

handleCachingReadListing
  :: forall a. 
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Eq (Name a)
    , Ord (Context a)
    ) => Permissions a -> Callbacks a -> RequestHandler (ReadListing a)
handleCachingReadListing permissions callbacks = responding do
  ctx <- acquire
  response <- liftIO (tryReadListingFromCache permissions callbacks ctx)
  case response of
    Just rsp -> customReplyRaw rsp
    Nothing  -> reply Nothing

customReplyRaw :: ByteString -> Responding rq rsp ()
#ifdef __GHCJS__
customReplyRaw = replyRaw . toTxt
#else
customReplyRaw = replyRaw
#endif

--------------------------------------------------------------------------------

data Route a 
  = ReadR (Context a) (Name a)
  | ListR (Context a)
  | CreateR (Context a)
  | UpdateR (Context a) (Name a)

{- 
-- I have a feeling these are expensive.
deriving instance (Generic (Context a), Generic (Name a)) => Generic (Route a)
deriving instance (Ord (Context a), Ord (Name a)) => Ord (Route a)
deriving instance (Eq (Context a), Eq (Name a)) => Eq (Route a)
deriving instance (Show (Context a), Show (Name a)) => Show (Route a)
deriving instance (ToJSON (Context a),ToJSON (Name a)) => ToJSON (Route a)
deriving instance (FromJSON (Context a),FromJSON (Name a)) => FromJSON (Route a)
-}

data Conjured = Create | Read | Update | List
instance Theme Conjured
instance Theme Create
instance Theme Update
instance Theme Read
instance Theme List

pages :: forall _role a. (Creatable _role a, Listable a, Readable a, Updatable _role a) => WebSocket -> Route a -> View
pages ws = \case
  ReadR ctx nm   -> Div <| Themed @Conjured . Themed @Read |> [ toRead ws ctx nm ]
  ListR ctx      -> Div <| Themed @Conjured . Themed @List |> [ Export.toList ws ctx ]
  CreateR ctx    -> Div <| Themed @Conjured . Themed @Create |> [ toCreate @_role ws ctx ]
  UpdateR ctx nm -> Div <| Themed @Conjured . Themed @Update |> [ toUpdate @_role ws ctx nm ]

readPages :: (Readable a, Listable a) => WebSocket -> Route a -> View
readPages ws = \case
  ReadR ctx nm   -> Div <| Themed @Conjured . Themed @Read |> [ toRead ws ctx nm ]
  UpdateR ctx nm -> Div <| Themed @Conjured . Themed @Read |> [ toRead ws ctx nm ]
  ListR ctx      -> Div <| Themed @Conjured . Themed @List |> [ Export.toList ws ctx ]
  CreateR ctx    -> Div <| Themed @Conjured . Themed @List |> [ Export.toList ws ctx ]

routes :: forall a route. (Typeable a, Routable a) 
       => (Route a -> route) -> Routing route ()
routes lift = do
  listRoute (\ctx -> lift (ListR ctx))
  updateRoute (\ctx nm -> lift (UpdateR ctx nm))
  createRoute (\ctx -> lift (CreateR ctx))
  readRoute (\ctx nm -> lift (ReadR ctx nm))

location :: Routable a => Route a -> Txt
location = \case
  ReadR ctx nm   -> toReadRoute ctx nm
  ListR ctx      -> toListRoute ctx
  CreateR ctx    -> toCreateRoute ctx
  UpdateR ctx nm -> toUpdateRoute ctx nm

ref :: (Routable a, HasFeatures v) => Route a -> v -> v
ref = go . location
  where
    go t a = OnClickWith intercept (\_ -> storeScrollPosition >> Router.goto t) (Href t a) 

goto :: Routable a => Route a -> IO ()
goto = go . location
  where
    go r = do
      storeScrollPosition
      Router.goto r

preload
  :: forall a v.
    ( Typeable a
    , ToJSON (Name a), FromJSON (Name a), Ord (Name a)
    , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
    , FromJSON (Product a)
    , FromJSON (Preview a)
    , HasFeatures v
    ) => Route a -> v -> v
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

--------------------------------------------------------------------------------
-- A very simple static renderer. The goal is to generate content for indexing
-- by crawlers, not to generate a static version of the site viewable by the
-- public.

generateStatic 
  :: forall a. 
    ( Typeable a
    , Routable a
    , ToJSON (Name a), FromJSON (Name a), Hashable (Name a), Pathable (Name a)
    , ToJSON (Context a), FromJSON (Context a), Hashable (Context a), Pathable (Context a)
    , ToJSON (Product a), FromJSON (Product a)
    , Component (Product a)
    ) => WebSocket -> IO ()
generateStatic = generateStaticWith @a "dist/static/" defaultTemplate

defaultTemplate :: (Component a) => a -> IO Txt
defaultTemplate a = pure $
  "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"></head><body>" <> toTxt (run a) <> "</body></html>"

generateStaticWith 
  :: forall a. 
    ( Typeable a
    , Routable a
    , ToJSON (Name a), FromJSON (Name a), Hashable (Name a), Pathable (Name a)
    , ToJSON (Context a), FromJSON (Context a), Hashable (Context a), Pathable (Context a)
    , ToJSON (Product a), FromJSON (Product a)
    ) => FilePath -> (Product a -> IO Txt) -> WebSocket -> IO ()
generateStaticWith path template ws = do
  Export.iterate @a $ \ctx nm -> do
    Sorcerer.read (ProductStream ctx nm) >>= \case
      Just pro -> do
        page <- template pro 
        let p = path <> fromTxt (toReadRoute ctx nm) <> ".html"
        createDirectoryIfMissing True (takeDirectory p)
        Prelude.writeFile p (fromTxt page)
      Nothing ->
        pure ()

      
