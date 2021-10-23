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
    , Amendable a, Previewable a, Producible a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Callbacks a -> Context a -> Name a -> Resource a -> IO (Maybe (Product a,Preview a,[(Name a,Preview a)]))
tryCreate Callbacks {..} ctx name a = do
  withLock ctx name $ do
    Sorcerer.observe (ResourceStream ctx name) (SetResource a) >>= \case
      Added (new :: Resource a) -> do
        pro <- produce True new
        pre <- preview True new pro
        (Sorcerer.Update (_ :: Product a)) <- Sorcerer.transact (ProductStream ctx name) (SetProduct pro)
        (Sorcerer.Update (_ :: Preview a)) <- Sorcerer.transact (PreviewStream ctx name) (SetPreview pre)
        Sorcerer.write (IndexStream @a) (ResourceAdded ctx name)
        (Sorcerer.Update (Previews (previews :: [(Name a,Preview a)]))) <- 
          Sorcerer.transact (PreviewsStream ctx) (SetPreviewItem name pre)
        onCreate ctx name new pro pre
        pure (Just (pro,pre,previews))
      _ ->
        pure Nothing

tryUpdate 
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
    ) => Callbacks a -> Context a -> Name a -> Resource a -> IO (Maybe (Product a,Preview a,[(Name a,Preview a)]))
tryUpdate Callbacks {..} ctx name a = do
  withLock ctx name $ do
    Sorcerer.transact (ResourceStream ctx name) (SetResource a) >>= \case
      Sorcerer.Update (new :: Resource a) -> do
        pro <- produce True new 
        pre <- preview True new pro
        (Sorcerer.Update (_ :: Product a)) <- Sorcerer.transact (ProductStream ctx name) (SetProduct pro)
        (Sorcerer.Update (_ :: Preview a)) <- Sorcerer.transact (PreviewStream ctx name) (SetPreview pre)
        (Sorcerer.Update (Previews (previews :: [(Name a,Preview a)]))) <- 
          Sorcerer.transact (PreviewsStream ctx) (SetPreviewItem name pre)
        onUpdate ctx name new pro pre
        pure (Just (pro,pre,previews))
      _ ->
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
    ) => Callbacks a -> Context a -> Name a -> Amend a -> IO (Maybe (Product a,Preview a,[(Name a,Preview a)]))
tryAmend Callbacks {..} ctx name a = do
  withLock ctx name $ do
    Sorcerer.transact (ResourceStream ctx name) (AmendResource a) >>= \case
      Sorcerer.Update (new :: Resource a) -> do
        pro <- produce True new
        pre <- preview True new pro
        (Sorcerer.Update (_ :: Product a)) <- Sorcerer.transact (ProductStream ctx name) (SetProduct pro)
        (Sorcerer.Update (_ :: Preview a)) <- Sorcerer.transact (PreviewStream ctx name) (SetPreview pre)
        (Sorcerer.Update (Previews (previews :: [(Name a,Preview a)]))) <- 
          Sorcerer.transact (PreviewsStream ctx) (SetPreviewItem name pre)
        onUpdate ctx name new pro pre
        pure (Just (pro,pre,previews))
      _ ->
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
    ) => Callbacks a -> Context a -> Name a -> IO (Maybe (Product a,Preview a,[(Name a,Preview a)]))
tryDelete Callbacks {..} ctx name =
  withLock ctx name $ do
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

tryReadResource
  :: forall a.
    ( Typeable a
    , Amendable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , Hashable (Context a), Pathable (Context a)
    , Hashable (Name a), Pathable (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
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
  Sorcerer.read (PreviewsStream ctx) >>= \case
    Just (Previews ps) -> pure (Just ps)
    Nothing -> pure Nothing

--------------------------------------------------------------------------------

publishing :: 
  ( Typeable a
  , Amendable a, Nameable a, Previewable a, Producible a 
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
    , Amendable a, Nameable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> RequestHandler (CreateResource a)
handleCreateResource Permissions {..} callbacks = responding do
  (ctx,resource) <- acquire
  response <- liftIO do
    let name = toName resource
    can <- canCreate ctx name
    if can then do
      tryCreate callbacks ctx name resource >>= \case
        Just _ -> pure (Just name)
        _      -> pure Nothing
    else
      pure Nothing
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
    , Amendable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a) 
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> RequestHandler (UpdateResource a)
handleUpdateResource Permissions {..} callbacks = responding do
  (ctx,name,resource) <- acquire
  response <- liftIO do
    can <- canUpdate ctx name
    if can then
      Just . isJust <$> tryUpdate callbacks ctx name resource
    else
      pure Nothing
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
handleDeleteResource Permissions {..} callbacks = responding do
  (ctx,name) <- acquire
  response <- liftIO do
    can <- canDelete ctx name
    if can then
      Just . isJust <$> tryDelete callbacks ctx name
    else do
      pure Nothing
  reply response

handlePreviewResource
  :: forall a. 
    ( Typeable a
    , Nameable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a)
    ) => Permissions a -> Callbacks a -> RequestHandler (PreviewResource a)
handlePreviewResource Permissions {..} callbacks = responding do
  (ctx,resource) <- acquire
  response <- liftIO do
    let name = toName resource
    can <- canCreate ctx name
    if can then do
      pro <- produce False resource
      pre <- preview False resource pro
      pure (Just (ctx,name,pre,pro,resource))
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
handlePreviewAmendResource Permissions {..} callbacks = responding do
  (ctx,name,a) <- acquire
  response <- liftIO do
    can <- canCreate ctx name
    if can then do
      tryReadResource ctx name >>= \case
        Nothing -> pure Nothing
        Just resource -> do
          let res = amend a resource
          pro <- produce False res
          pre <- preview False res pro
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
handleAmendResource Permissions {..} callbacks = responding do
  (ctx,name,amend) <- acquire
  response <- liftIO do
    can <- canUpdate ctx name
    if can then
      Just . isJust <$> tryAmend callbacks ctx name amend
    else
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
  :: forall resource.
    ( Typeable resource
    , Ord (Context resource), Ord (Name resource)
    ) => Context resource -> Name resource -> IO (Maybe ByteString)
tryReadProductFromCache ctx nm = do
  let ty = typeOf (undefined :: ReadProduct resource)
  ps <- readIORef (products conjurerCache) 
  case Map.lookup ty ps of
    Nothing -> pure Nothing
    Just pros_ -> do
      ResponseMap pros <- readIORef pros_
      case Map.lookup (ctx,nm) (unsafeCoerce pros) of
        Just bs_ -> Just <$> readIORef bs_
        _ -> pure Nothing

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
  :: forall resource.
    ( Typeable resource
    , Ord (Context resource), Ord (Name resource)
    ) => Context resource -> Name resource -> IO (Maybe ByteString)
tryReadPreviewFromCache ctx nm = do
  let ty = typeOf (undefined :: ReadPreview resource)
  ps <- readIORef (Pure.Conjurer.previews conjurerCache) 
  case Map.lookup ty ps of
    Nothing -> pure Nothing
    Just pros_ -> do
      ResponseMap pres <- readIORef pros_
      case Map.lookup (ctx,nm) (unsafeCoerce pres) of
        Just bs_ -> Just <$> readIORef bs_
        _ -> pure Nothing

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
  :: forall resource.
    ( Typeable resource
    , Ord (Context resource)
    ) => Context resource -> IO (Maybe ByteString)
tryReadListingFromCache ctx = do
  let ty = typeOf (undefined :: ReadListing resource)
  ls <- readIORef (listings conjurerCache) 
  case Map.lookup ty ls of
    Nothing -> pure Nothing
    Just lsts_ -> do
      ResponseMap lsts <- readIORef lsts_
      case Map.lookup ctx (unsafeCoerce lsts) of
        Just bs_ -> Just <$> readIORef bs_
        _ -> pure Nothing

cachingPublishing :: 
  ( Typeable a
  , Amendable a, Nameable a, Previewable a, Producible a 
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
    , Amendable a, Nameable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Ord (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a), Eq (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> RequestHandler (CreateResource a)
handleCachingCreateResource Permissions {..} callbacks = responding do
  (ctx,resource) <- acquire
  response <- liftIO do
    let name = toName resource
    can <- canCreate ctx name
    if can then do
      tryCreate callbacks ctx name resource >>= \case
        Just (pro,pre,lst) -> do
          cacheProduct ctx name pro
          cachePreview ctx name pre
          cacheListing ctx lst
          pure (Just name)
        _ -> 
          pure Nothing
    else
      pure Nothing
  reply response

handleCachingUpdateResource
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
    ) => Permissions a -> Callbacks a -> RequestHandler (UpdateResource a)
handleCachingUpdateResource Permissions {..} callbacks = responding do
  (ctx,name,resource) <- acquire
  response <- liftIO do
    can <- canUpdate ctx name
    if can then
      tryUpdate callbacks ctx name resource >>= \case
        Nothing -> pure (Just False)
        Just (pro,pre,lst) -> do
          cacheProduct ctx name pro
          cachePreview ctx name pre
          cacheListing ctx lst
          pure (Just True)
    else
      pure Nothing
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
handleCachingDeleteResource Permissions {..} callbacks = responding do
  (ctx,name) <- acquire
  response <- liftIO do
    can <- canDelete ctx name
    if can then
      tryDelete callbacks ctx name >>= \case
        Nothing -> pure (Just False)
        Just (pro,pre,lst) -> do
          deleteProduct ctx name
          deletePreview ctx name
          cacheListing ctx lst
          pure (Just True)
    else do
      pure Nothing
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
handleCachingAmendResource Permissions {..} callbacks = responding do
  (ctx,name,amend_) <- acquire
  response <- liftIO do
    can <- canUpdate ctx name
    if can then
      tryAmend callbacks ctx name amend_ >>= \case
        Nothing -> pure (Just False)
        Just (pro,pre,lst) -> do
          cacheProduct ctx name pro
          cachePreview ctx name pre
          cacheListing ctx lst
          pure (Just True)
    else
      pure Nothing
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
handleCachingReadProduct Permissions {..} Callbacks { onRead } = responding do
  (ctx,name) <- acquire
  can <- liftIO (canRead ctx name)
  if can then 
    liftIO (tryReadProductFromCache ctx name) >>= \case
      Just p -> do
        liftIO (onRead ctx name (fromJust (decodeBS p)))
        customReplyRaw p
      _ ->
        reply Nothing
  else 
    reply Nothing

handleCachingReadPreview
  :: forall a. 
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Ord (Name a)
    , Pathable (Context a), Hashable (Context a)
    , Pathable (Name a), Hashable (Name a)
    ) => Permissions a -> Callbacks a -> RequestHandler (ReadPreview a)
handleCachingReadPreview Permissions {..} Callbacks {..} = responding do
  (ctx,name) <- acquire
  can <- liftIO (canRead ctx name)
  if can then do
    liftIO (tryReadPreviewFromCache ctx name) >>= \case
      Just p  -> customReplyRaw p
      Nothing -> reply Nothing
  else 
    reply Nothing

handleCachingReadListing
  :: forall a. 
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Eq (Name a)
    , Ord (Context a)
    ) => Permissions a -> Callbacks a -> RequestHandler (ReadListing a)
handleCachingReadListing Permissions {..} Callbacks {..} = responding do
  ctx <- acquire
  can <- liftIO (canList ctx)
  if can then 
    liftIO (tryReadListingFromCache ctx) >>= \case
      Just ps -> do
        liftIO (onList ctx (fromJust (decodeBS ps)))
        customReplyRaw ps
      _ ->
        reply Nothing
  else 
    reply Nothing

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

pages :: (Creatable _role a, Listable a, Readable a, Updatable _role a) => WebSocket -> Route a -> View
pages ws = \case
  ReadR ctx nm   -> Div <| Themed @Conjured . Themed @Read |> [ toRead ws ctx nm ]
  ListR ctx      -> Div <| Themed @Conjured . Themed @List |> [ Export.toList ws ctx ]
  CreateR ctx    -> Div <| Themed @Conjured . Themed @Create |> [ toCreate ws ctx ]
  UpdateR ctx nm -> Div <| Themed @Conjured . Themed @Update |> [ toUpdate ws ctx nm ]

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

      
