{-# language GADTs, ScopedTypeVariables, DuplicateRecordFields #-}
module Pure.Conjurer.Analytics where

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Bloom.Scalable as Bloom
import Pure.Data.JSON hiding (encode,decode)
import Pure.Data.Txt
import Pure.Data.Time
import Pure.Data.Marker
import Pure.Router (route)
import Pure.Elm.Component (HasCallStack,timed,Default(..))
import Pure.Sorcerer hiding (events)
import Pure.Sorcerer.Manager (Manageable(..))
import qualified Pure.Sorcerer as Sorcerer
import Pure.WebSocket as WS hiding (Nat,rep)

import Data.Hashable

import Control.Monad
import Data.Foldable 
import Data.Function
import Data.Typeable
import Data.IORef
import Data.List as List
import Data.Maybe
import GHC.Exts (Any)
import GHC.Generics
import GHC.TypeLits
import Prelude
import Unsafe.Coerce
import System.IO.Unsafe

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict as Map
import Data.Set as Set

#ifndef __GHCJS__
import qualified Data.IP as IPR (IP(..),fromSockAddr)
#endif

-- NOTES:
-- 
-- When building related top and related popular, the related events must be 
-- static! That is, if your event has dynamic information, it will not analyze
-- as you might hope. This works well for events that are static routes, like 
-- those produced with `recordRead`.
--
-- Events are largely deduplicated by IP, but this is not sufficient to prevent 
-- manipulation. Voting ring detection to come.

data IP = IP {-# UNPACK #-}!Txt
  deriving stock (Generic,Eq,Ord,Show)
  deriving anyclass (ToJSON,FromJSON,Hashable)

instance ToTxt IP where toTxt (IP ip) = ip

fromWebSocket :: WebSocket -> IO IP
fromWebSocket ws_ = do
#ifndef __GHCJS__
  ws <- liftIO (readIORef ws_)

  let
    -- 192.0.2.0 is reserved for documentation and examples, 
    -- so should be safe as a default.
    ip = fromMaybe (Prelude.read "192.0.2.0") do 
      (sa,_,_,_) <- wsSocket ws
      (ip,_) <- IPR.fromSockAddr sa
      pure ip

  pure $!
    case ip of
      IPR.IPv4 ipv4 -> IP (toTxt (show ipv4))
      IPR.IPv6 ipv6 -> IP (toTxt (show ipv6))
#else
  pure (IP def)
#endif

newtype SessionId = SessionId Marker
  deriving stock Generic
  deriving (ToJSON,FromJSON,Eq,Ord,Hashable,ToTxt) via Marker
  deriving anyclass Pathable

newSessionId :: IO SessionId
newSessionId = SessionId <$> markIO

--------------------------------------------------------------------------------

data Session = Session 
  { sessionid :: SessionId
  , start     :: Time
  , end       :: Time
  , ip        :: IP
  , user      :: Maybe Username
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data SessionMsg
  = SessionStart SessionId Time IP
  | SessionUser Time Username 
  | SessionEvent Time Txt
  | SessionEnd Time
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Streamable SessionMsg where
  data Stream SessionMsg = SessionStream SessionId
    deriving stock (Generic,Eq,Ord)
    deriving anyclass Hashable

  stream (SessionStream sid) =
    "conjurer/analytics/session" 
      ++ fromTxt (toPath sid)
      ++ ".stream"

instance Aggregable SessionMsg Session where
  update (SessionStart sessionid t ip) Nothing = 
    Sorcerer.Update Session 
      { start = t 
      , end = t 
      , user = Nothing 
      , ..
      }

  update (SessionUser t un) (Just ses) = 
    Sorcerer.Update ses 
      { end = t
      , user = Just un 
      }

  update (SessionEnd t) (Just ses) = 
    Sorcerer.Update ses 
      { end = t }

  update _ _ = Ignore
  
  aggregate = "session.aggregate"

--------------------------------------------------------------------------------

data Sessions = Sessions {-# UNPACK #-}!Time {-# UNPACK #-}!Int
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data SessionsMsg = SessionCreated Time SessionId
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Streamable SessionsMsg where
  data Stream SessionsMsg = SessionsStream
    deriving stock (Generic,Eq,Ord)
    deriving anyclass Hashable

  stream SessionsStream =
    "conjurer/analytics/sessions.stream"

instance Aggregable SessionsMsg Sessions where
  update (SessionCreated t _) Nothing = Sorcerer.Update (Sessions t 1)
  update _ (Just (Sessions t n)) = Sorcerer.Update (Sessions t (n + 1))
  
  aggregate = "sessions.aggregate"

sessionsCount :: IO Int
sessionsCount = 
  Sorcerer.read SessionsStream >>= \case
    Just (Sessions _ n) -> pure n
    _ -> pure 0

oldestSession :: IO Time
oldestSession =
  Sorcerer.read SessionsStream >>= \case
    Just (Sessions t _) -> pure t
    _ -> time

listSessions :: IO [SessionId]
listSessions = fmap getSessionId <$> Sorcerer.events SessionsStream
  where getSessionId (SessionCreated _ sid) = sid

--------------------------------------------------------------------------------

data GlobalAnalytics = GlobalAnalytics {-# UNPACK #-}!Int {-# UNPACK #-}!Int
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data GlobalAnalyticsMsg
  = GlobalAnalyticsEvent Time SessionId IP Txt
  | GlobalResourceCreated Time SessionId Txt
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Streamable GlobalAnalyticsMsg where
  data Stream GlobalAnalyticsMsg = GlobalAnalyticsStream
    deriving stock (Generic,Eq,Ord)
    deriving anyclass Hashable

  stream GlobalAnalyticsStream = 
    "conjurer/analytics/analytics.stream"

instance Manageable GlobalAnalyticsMsg where
  -- Don't burden the IO subsystem with unimportant writes
  threshold = Seconds 1 0
  batch = 100

instance Aggregable GlobalAnalyticsMsg GlobalAnalytics where
  update (GlobalAnalyticsEvent _ _ _ _) = Sorcerer.Update . \case
    Just (GlobalAnalytics n m) -> GlobalAnalytics (n + 1) m
    Nothing                    -> GlobalAnalytics 1 0
  update (GlobalResourceCreated _ _ _) = Sorcerer.Update . \case
    Just (GlobalAnalytics n m) -> GlobalAnalytics n (m + 1)
    Nothing                    -> GlobalAnalytics 0 1

  aggregate = "analytics.aggregate"

-- Used for seeding bloom filters during event analysis.
eventsCount :: IO Int
eventsCount =
  Sorcerer.read GlobalAnalyticsStream >>= \case
    Just (GlobalAnalytics n _) -> pure n
    _ -> pure 0

-- Used for seeding bloom filters during event analysis.
resourceCount :: IO Int
resourceCount =
  Sorcerer.read GlobalAnalyticsStream >>= \case
    Just (GlobalAnalytics _ n) -> pure n
    _ -> pure 0

--------------------------------------------------------------------------------

type Association = (Time,Double,Int)
type Associations = Map Txt Association

type Analysis = (Time,Time,Double,Int,Associations)
type Analyses = Map Txt Analysis
pattern Analysis 
  :: Time -> Time -> Double -> Int -> Map Txt (Time,Double,Int) 
  -> Analysis
pattern Analysis { created, latest, decaying, count, associations } = 
  (created,latest,decaying,count,associations)


data Analyzer = Analyzer
  { analyses :: Analyses
  , hits     :: Bloom -- IP/resource pairs
  , sessions :: Bloom
  }

-- Finalize corrects for staleness. This must be run for accurate results!
finalize :: Time -> Time -> Analyzer -> Analyzer
finalize decay@(Milliseconds d _) now@(Milliseconds n _) Analyzer { analyses, .. } =
  Analyzer { analyses = fmap updateAnalysis analyses, ..  }
  where
    updateAnalysis Analysis {..} = 
      let 
        Milliseconds l _ = latest
      in 
        Analysis
          { latest   = now 
          , decaying = decaying * 2 ** (fromIntegral (l - n) / fromIntegral d) + 1
          , count    = count + 1
          , associations = fmap updateAssociation associations
          , ..
          }
        
    updateAssociation (latest,decaying,count) =
      let 
        Milliseconds l _ = latest
        decaying' = decaying * 2 ** (fromIntegral (l - n) / fromIntegral d) + 1
      in 
        (latest,decaying',count + 1)

analyzeAll :: HasCallStack => Time -> IO Analyses
analyzeAll _decay@(Milliseconds (fromIntegral -> d) _) = timed do 

  -- c is a critical value. Ranking is performed by
  -- taking the lower bound of a beta distribution 
  -- constructed from the count of hits or the decayed
  -- count of hits and the total count of events minus
  -- the count of hits.
  c <- eventsCount

  start <- 
    Analyzer
      <$> pure Map.empty
      <*> Bloom.new 0.001 c
      <*> (Bloom.new 0.001 =<< sessionsCount)

  evs <- Sorcerer.events GlobalAnalyticsStream
  
  analyzer <- foldM analyzeGlobalStream start evs
  now <- time
  pure (analyses (finalize _decay now analyzer))

  where
    analyzeGlobalStream Analyzer {..} = \case

      GlobalResourceCreated t _ r -> do
        let
          add = \case
            Nothing -> 
              Analysis
                { created      = t
                , latest       = t
                , decaying     = 1
                , count        = 1
                , associations = Map.empty
                }

            -- Confusingly, analyzeSessionStream may add a 
            -- resource before analyzeGlobalStream does.
            -- So, we need to correct the time; this could 
            -- make the decaying value slightly incorrect
            -- but it shouldn't be too far off.
            Just Analysis {..} -> 
              Analysis 
                { created = t
                , .. 
                }

        pure Analyzer
          { analyses = Map.alter (Just . add) r analyses 
          , ..
          }

      GlobalAnalyticsEvent t sid ip hit -> do

        let
          create Nothing = 
            Analysis
              { created      = t
              , latest       = t
              , decaying     = 1
              , count        = 1
              , associations = Map.empty
              }

          create (Just Analysis {..}) = 
            let 
              Milliseconds c _ = created
              Milliseconds n _ = t
            in 
              Analysis 
                { decaying = decaying * 2 ** (fromIntegral (c - n) / fromIntegral d) + 1
                , count    = count + 1
                , .. 
                }

          new = 
            Analyzer 
              { analyses = Map.alter (Just . create) hit analyses 
              , ..
              }
              
        newVisit <- Bloom.update hits (toTxt ip <> hit) 
        if newVisit then do

          newSession <- Bloom.update sessions sid
          if newSession then do

            evs <- Sorcerer.events (SessionStream sid)
            snd <$> foldM analyzeSessionStream (Nothing,new) evs

          else

            pure new

        else

          pure Analyzer {..}

    analyzeSessionStream (source,Analyzer {..}) = \case

      SessionEvent t destination ->
        
        let 
          analyses' = maybe id (Map.alter (Just . add)) source analyses 
            where
              add = \case
                Nothing -> 
                  Analysis
                    { created      = t
                    , latest       = t
                    , decaying     = 1
                    , count        = 1
                    , associations = Map.singleton destination (t,1,1)
                    }

                Just Analysis {..} ->
                  Analysis 
                    { associations = Map.alter (Just . upd) destination associations
                    , ..
                    }

              upd = \case
                Nothing -> (t,1,1)

                -- the `+ 1` is a good extension point for enrichment
                Just (Milliseconds l _,decaying,count) -> 
                  let 
                    Milliseconds n _ = t
                    decaying' = decaying * 2 ** (fromIntegral (l - n) / fromIntegral d) + 1 
                  in 
                    (t,decaying',count + 1)

        in
          pure (Just destination,Analyzer { analyses = analyses', .. })

      _ -> 
        pure (source,Analyzer {..})

--------------------------------------------------------------------------------
-- TODO: switch to a true graph-based approach, since there are some nice
-- algorithms for these sorts of analyses.

data Analyzed a = Analyzed
  { popular                  :: [(Context a,Name a)]
  , top                      :: [(Context a,Name a)]
  , recent                   :: [(Context a,Name a)]
  , popularByContext         :: Map (Context a) [(Context a,Name a)]
  , topByContext             :: Map (Context a) [(Context a,Name a)]
  , recentByContext          :: Map (Context a) [(Context a,Name a)]
  , relatedPopularByResource :: Map (Context a,Name a) [Txt]
  , relatedTopByResource     :: Map (Context a,Name a) [Txt]
  }

toAnalyzed :: forall a. (Rootable a, Routable a, Ord (Context a), Ord (Name a)) => Analyses -> Analyzed a
toAnalyzed analyses = Analyzed {..}
  where

    matches :: [((Context a,Name a),Analysis)]
    matches = catMaybes . fmap match . Map.toList $ analyses
      where
        parse :: Txt -> Maybe (Context a,Name a)
        -- Not sure what approach I should take to make this safe.
        parse evt = unsafePerformIO (route (readRoute (,)) evt)

        match (evt,analysis) 
          | Just ctxnm <- parse evt = Just (ctxnm,analysis)
          | otherwise = Nothing

    contexts :: [Context a]
    contexts = streamNub . fmap (fst . fst) $ matches


    popular, top, recent :: [(Context a,Name a)]
    popular = fmap fst (sortBy (flip compare `on` fmap target) matches)
      where
        target Analysis { decaying } = decaying

    top = fmap fst (sortBy (flip compare `on` fmap target) matches)
      where
        target Analysis { count } = count
        
    recent = fmap fst (sortBy (flip compare `on` fmap target) matches)
      where
        target Analysis { created } = created


    popularByContext, topByContext, recentByContext :: Map (Context a) [(Context a,Name a)]
    popularByContext = Map.fromList (fmap go contexts)
      where
        go context = (context,List.filter ((== context) . fst) popular)

    topByContext = Map.fromList (fmap go contexts)
      where
        go context = (context,List.filter ((== context) . fst) top)
 
    recentByContext = Map.fromList (fmap go contexts)
      where
        go context = (context,List.filter ((== context) . fst) recent)


    relatedPopularByResource, relatedTopByResource :: Map (Context a,Name a) [Txt]
    relatedPopularByResource = Map.fromList . fmap (fmap extract) $ matches
      where
        extract Analysis { associations } = fmap fst . sortBy (flip compare `on` fmap target) . Map.toList $ associations
          where
            target (_,decaying,_) = decaying

    relatedTopByResource = Map.fromList . fmap (fmap extract) $ matches
      where
        extract Analysis { associations } = fmap fst . sortBy (flip compare `on` fmap target) . Map.toList $ associations
          where
            target (_,_,count) = count

class Analyzeable (as :: [*]) where
  analyzeEach :: HasCallStack => Analyses -> IO ()

instance Analyzeable '[] where
  analyzeEach _ = pure ()

instance 
  ( Typeable a
  , Rootable a, Routable a
  , Ord (Context a), Hashable (Context a), Pathable (Context a), ToJSON (Context a)
  , Ord (Name a), Hashable (Name a), Pathable (Name a), ToJSON (Name a)
  , Analyzeable as
  ) => Analyzeable (a : as) 
  where
    analyzeEach analyses = timed do
      let Analyzed {..} = toAnalyzed @a analyses

      addPopularForNamespaceToCache popular
      addTopForNamespaceToCache top
      addRecentForNamespaceToCache recent

      for_ (Map.toList popularByContext) (uncurry addPopularForContextToCache)
      for_ (Map.toList topByContext) (uncurry addTopForContextToCache)
      for_ (Map.toList recentByContext) (uncurry addRecentForContextToCache) 

      for_ (Map.toList relatedPopularByResource) $ \((ctx,nm),pop) ->
        addRelatedPopularForResourceToCache ctx nm pop
      for_ (Map.toList relatedTopByResource) $ \((ctx,nm),top) ->
        addRelatedTopForResourceToCache ctx nm top

      analyzeEach @as analyses
      
--------------------------------------------------------------------------------

recordStart :: WebSocket -> IO SessionId
recordStart ws = do
  ip  <- fromWebSocket ws
  sid <- newSessionId
  now <- time
  Sorcerer.write SessionsStream do
    SessionCreated now sid
  Sorcerer.write (SessionStream sid) do
    SessionStart sid now ip
  pure sid

recordUser :: SessionId -> Username -> IO ()
recordUser sid un = do
  now <- time 
  Sorcerer.write (SessionStream sid) do
    SessionUser now un

recordRead 
  :: forall a. 
    ( Typeable a
    , Routable a
    , ToJSON (Context a), FromJSON (Context a)
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => SessionId -> IP -> Context a -> Name a -> IO ()
recordRead sid ip ctx nm = do
  now <- time
  let r = toReadRoute ctx nm
  Sorcerer.write (SessionStream sid) do
    SessionEvent now r
  Sorcerer.write GlobalAnalyticsStream do
    GlobalAnalyticsEvent now sid ip r

recordCreate
  :: forall a.
    ( Typeable a
    , Routable a
    , ToJSON (Context a), FromJSON (Context a)
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => SessionId -> Context a -> Name a -> IO ()
recordCreate sid ctx nm = do
  now <- time
  Sorcerer.write GlobalAnalyticsStream do
    GlobalResourceCreated now sid (toReadRoute ctx nm)

recordEvent :: SessionId -> Txt -> IO ()
recordEvent sid evt = do
  now <- time
  Sorcerer.write (SessionStream sid) do
    SessionEvent now evt

recordEnd :: SessionId -> IO ()
recordEnd sid = do
  now <- time
  Sorcerer.write (SessionStream sid) do
    SessionEnd now

--------------------------------------------------------------------------------  

addAnalytics 
  :: forall a.
    ( Typeable a
    , Routable a
    , ToJSON (Context a), FromJSON (Context a)
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    )  => SessionId -> IP -> Callbacks a -> Callbacks a
addAnalytics sid ip cbs = cbs { onRead = analyzeRead }
  where
    analyzeRead ctx name product = do
      recordRead sid ip ctx name
      onRead cbs ctx name product
      
    analyzeCreate ctx name res pro pre lst = do
      recordCreate sid ctx name
      onCreate cbs ctx name res pro pre lst

--------------------------------------------------------------------------------

{-# INLINE streamNubOn #-}
streamNubOn :: Ord x => (a -> x) -> [a] -> [a]
streamNubOn f = go Set.empty
  where
    go acc [] = []
    go acc (a : as) = let x = f a in
      if Set.member x acc then
        go acc as
      else
        a : go (Set.insert x acc) as

{-# INLINE streamNub #-}
streamNub :: Ord a => [a] -> [a]
streamNub = streamNubOn id

bloomNubOn :: ToTxt x => (a -> x) -> [a] -> IO [a]
bloomNubOn f xs = unsafeInterleaveIO do
  b <- bloom 0.001
  go b xs
  where
    go _ [] = pure []
    go b (a : as) =
      Bloom.update b (f a) >>= \case
        True -> (a:) <$> go b as
        _ -> go b as

bloomNub :: ToTxt a => [a] -> IO [a]
bloomNub = bloomNubOn id 

--------------------------------------------------------------------------------
-- Naming:
--
--  > Popular is time-relative. Something that is popular now may not be popular
--    in the future.
--
--  > Top is time-irrelative, absolute.
--
--  > Related is derived from events that appear together, in the same session. 
--

data ListPopularForNamespace a
instance Identify (ListPopularForNamespace a)
instance Typeable a => Request (ListPopularForNamespace a) where
  type Req (ListPopularForNamespace a) = (Int,())
  type Rsp (ListPopularForNamespace a) = [(Context a,Name a)]

listPopularForNamespace :: Proxy (ListPopularForNamespace a)
listPopularForNamespace = Proxy

data ListTopForNamespace a
instance Identify (ListTopForNamespace a)
instance Typeable a => Request (ListTopForNamespace a) where
  type Req (ListTopForNamespace a) = (Int,())
  type Rsp (ListTopForNamespace a) = [(Context a,Name a)]

listTopForNamespace :: Proxy (ListTopForNamespace a)
listTopForNamespace = Proxy

data ListRecentForNamespace a
instance Identify (ListRecentForNamespace a)
instance Typeable a => Request (ListRecentForNamespace a) where
  type Req (ListRecentForNamespace a) = (Int,())
  type Rsp (ListRecentForNamespace a) = [(Context a,Name a)]

listRecentForNamespace :: Proxy (ListRecentForNamespace a)
listRecentForNamespace = Proxy

data ListPopularForContext a
instance Identify (ListPopularForContext a)
instance Typeable a => Request (ListPopularForContext a) where
  type Req (ListPopularForContext a) = (Int,Context a)
  type Rsp (ListPopularForContext a) = [(Context a,Name a)]

listPopularForContext :: Proxy (ListPopularForContext a)
listPopularForContext = Proxy

data ListTopForContext a
instance Identify (ListTopForContext a)
instance Typeable a => Request (ListTopForContext a) where
  type Req (ListTopForContext a) = (Int,Context a)
  type Rsp (ListTopForContext a) = [(Context a,Name a)]

listTopForContext :: Proxy (ListTopForContext a)
listTopForContext = Proxy

data ListRecentForContext a
instance Identify (ListRecentForContext a)
instance Typeable a => Request (ListRecentForContext a) where
  type Req (ListRecentForContext a) = (Int,Context a)
  type Rsp (ListRecentForContext a) = [(Context a,Name a)]

listRecentForContext :: Proxy (ListRecentForContext a)
listRecentForContext = Proxy

data ListRelatedTopForResource a
instance Identify (ListRelatedTopForResource a)
instance Typeable a => Request (ListRelatedTopForResource a) where
  type Req (ListRelatedTopForResource a) = (Int,(Context a,Name a))
  type Rsp (ListRelatedTopForResource a) = [Txt]

listRelatedTopForResource :: Proxy (ListRelatedTopForResource a)
listRelatedTopForResource = Proxy

data ListRelatedPopularForResource a
instance Identify (ListRelatedPopularForResource a)
instance Typeable a => Request (ListRelatedPopularForResource a) where
  type Req (ListRelatedPopularForResource a) = (Int,(Context a,Name a))
  type Rsp (ListRelatedPopularForResource a) = [Txt]

listRelatedPopularForResource :: Proxy (ListRelatedPopularForResource a)
listRelatedPopularForResource = Proxy

type AnalyticsAPI a =
  '[ ListPopularForNamespace a
   , ListTopForNamespace a
   , ListRecentForNamespace a
   , ListPopularForContext a
   , ListTopForContext a
   , ListRecentForContext a
   , ListRelatedPopularForResource a
   , ListRelatedTopForResource a
   ]

analyticsAPI
  :: forall a.
    ( Typeable a 
    ) => API '[] (AnalyticsAPI a)
analyticsAPI = api msgs reqs
  where
    msgs = WS.none
    reqs = listPopularForNamespace @a
       <:> listTopForNamespace @a
       <:> listRecentForNamespace @a
       <:> listPopularForContext @a
       <:> listTopForContext @a
       <:> listRecentForContext @a
       <:> listRelatedPopularForResource @a
       <:> listRelatedTopForResource @a
       <:> WS.none

--------------------------------------------------------------------------------

-- lower bound of Wilson score
rank :: Double -> Double -> Double
rank positive total 
  | total == 0 = 0
  | total < positive = 0
  | otherwise =
    let 
      z = 0.96
      z2 = z ^ 2
      r = positive / total
      x = r + z2 / (2 * total)
      y = z * sqrt((r * (1 - r) + z2 / (4 * total) ) / total)
    in 
      (x - y) / (1 + z2 / total)

--------------------------------------------------------------------------------
--
data AnalyticsCache = AnalyticsCache
  { popularForNamespace        :: IORef (Map TypeRep ByteString)
  , topForNamespace            :: IORef (Map TypeRep ByteString)
  , recentForNamespace         :: IORef (Map TypeRep ByteString)
  , popularForContext          :: IORef (Map TypeRep (Map Any ByteString))
  , topForContext              :: IORef (Map TypeRep (Map Any ByteString))
  , recentForContext           :: IORef (Map TypeRep (Map Any ByteString))
  , relatedPopularForResource  :: IORef (Map TypeRep (Map Any ByteString))
  , relatedTopForResource      :: IORef (Map TypeRep (Map Any ByteString))
  }

{-# NOINLINE analyticsCache #-}
analyticsCache :: AnalyticsCache
analyticsCache = unsafePerformIO do
  AnalyticsCache
    <$> newIORef Map.empty 
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty

addPopularForNamespaceToCache
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a), ToJSON (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a), ToJSON (Name a)
    ) => [(Context a,Name a)] -> IO ()
addPopularForNamespaceToCache pfn = do
  let ty = typeOf (undefined :: a)
  atomicModifyIORef' (popularForNamespace analyticsCache) $ \map ->
    (Map.insert ty (encodeBS pfn) map,())

addTopForNamespaceToCache
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a), ToJSON (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a), ToJSON (Name a)
    ) => [(Context a,Name a)] -> IO ()
addTopForNamespaceToCache tfn = do
  let ty = typeOf (undefined :: a)
  atomicModifyIORef' (topForNamespace analyticsCache) $ \map ->
    (Map.insert ty (encodeBS tfn) map,())

addRecentForNamespaceToCache
   :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a), ToJSON (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a), ToJSON (Name a)
    ) => [(Context a,Name a)] -> IO ()
addRecentForNamespaceToCache rfn = do
  let ty = typeOf (undefined :: a)
  atomicModifyIORef' (recentForNamespace analyticsCache) $ \map ->
    (Map.insert ty (encodeBS rfn) map,())

addPopularForContextToCache
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a), ToJSON (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a), ToJSON (Name a)
    ) => Context a -> [(Context a,Name a)] -> IO ()
addPopularForContextToCache ctx pfc = do
  let ty = typeOf (undefined :: a)
  atomicModifyIORef' (popularForContext analyticsCache) $ \old ->
    case Map.lookup ty old of
      Nothing  -> (Map.insert ty (unsafeCoerce $ Map.singleton ctx (encodeBS pfc)) old,())
      Just tym -> (Map.insert ty (unsafeCoerce $ Map.insert ctx (encodeBS pfc) (unsafeCoerce tym)) old,())

addTopForContextToCache
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a), ToJSON (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a), ToJSON (Name a)
    ) => Context a -> [(Context a,Name a)] -> IO ()
addTopForContextToCache ctx tfc = do
  let ty = typeOf (undefined :: a)
  atomicModifyIORef' (topForContext analyticsCache) $ \old ->
    case Map.lookup ty old of
      Nothing  -> (Map.insert ty (unsafeCoerce $ Map.singleton ctx (encodeBS tfc)) old,())
      Just tym -> (Map.insert ty (unsafeCoerce $ Map.insert ctx (encodeBS tfc) (unsafeCoerce tym)) old,())

addRecentForContextToCache
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a), ToJSON (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a), ToJSON (Name a)
    ) => Context a -> [(Context a,Name a)] -> IO ()
addRecentForContextToCache ctx rfc = do
  let ty = typeOf (undefined :: a)
  atomicModifyIORef' (recentForContext analyticsCache) $ \old ->
    case Map.lookup ty old of
      Nothing  -> (Map.insert ty (unsafeCoerce $ Map.singleton ctx (encodeBS rfc)) old,())
      Just tym -> (Map.insert ty (unsafeCoerce $ Map.insert ctx (encodeBS rfc) (unsafeCoerce tym)) old,())

addRelatedPopularForResourceToCache
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> Name a -> [Txt] -> IO ()
addRelatedPopularForResourceToCache ctx nm rpfr = do
  let ty = typeOf (undefined :: a)
  atomicModifyIORef' (relatedPopularForResource analyticsCache) $ \old ->
    case Map.lookup ty old of
      Nothing  -> (Map.insert ty (unsafeCoerce $ Map.singleton (ctx,nm) (encodeBS rpfr)) old,())
      Just tym -> (Map.insert ty (unsafeCoerce $ Map.insert (ctx,nm) (encodeBS rpfr) (unsafeCoerce tym)) old,())

addRelatedTopForResourceToCache
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> Name a -> [Txt] -> IO ()
addRelatedTopForResourceToCache ctx nm rtfr = do
  let ty = typeOf (undefined :: a)
  atomicModifyIORef' (relatedTopForResource analyticsCache) $ \old ->
    case Map.lookup ty old of
      Nothing  -> (Map.insert ty (unsafeCoerce $ Map.singleton (ctx,nm) (encodeBS rtfr)) old,())
      Just tym -> (Map.insert ty (unsafeCoerce $ Map.insert (ctx,nm) (encodeBS rtfr) (unsafeCoerce tym)) old,())

readPopularForNamespaceFromCache :: forall a. Typeable a => IO (Maybe ByteString)
readPopularForNamespaceFromCache = do
  let ty = typeOf (undefined :: a)
  pfn <- readIORef (popularForNamespace analyticsCache)
  pure (Map.lookup ty pfn)

readTopForNamespaceFromCache :: forall a. Typeable a => IO (Maybe ByteString)
readTopForNamespaceFromCache = do
  let ty = typeOf (undefined :: a)
  tfn <- readIORef (topForNamespace analyticsCache)
  pure (Map.lookup ty tfn)

readRecentForNamespaceFromCache :: forall a. Typeable a => IO (Maybe ByteString)
readRecentForNamespaceFromCache = do
  let ty = typeOf (undefined :: a)
  rfn <- readIORef (recentForNamespace analyticsCache)
  pure (Map.lookup ty rfn)

readPopularForContextFromCache 
  :: forall a. 
    ( Typeable a 
    , Ord (Context a)
    ) => Context a -> IO (Maybe ByteString)
readPopularForContextFromCache ctx = do
  let ty = typeOf (undefined :: a)
  tfc <- readIORef (topForContext analyticsCache)
  pure do
    map <- Map.lookup ty tfc
    Map.lookup ctx (unsafeCoerce map)

readTopForContextFromCache 
  :: forall a. 
    ( Typeable a 
    , Ord (Context a)
    ) => Context a -> IO (Maybe ByteString)
readTopForContextFromCache ctx = do
  let ty = typeOf (undefined :: a)
  tfc <- readIORef (topForContext analyticsCache)
  pure do
    map <- Map.lookup ty tfc
    Map.lookup ctx (unsafeCoerce map)

readRecentForContextFromCache
  :: forall a.
    ( Typeable a
    , Ord (Context a)
    ) => Context a -> IO (Maybe ByteString)
readRecentForContextFromCache ctx = do
  let ty = typeOf (undefined :: a)
  rfc <- readIORef (recentForContext analyticsCache)
  pure do
    map <- Map.lookup ty rfc
    Map.lookup ctx (unsafeCoerce map)

readRelatedPopularForResourceFromCache 
  :: forall a. 
    ( Typeable a 
    , Ord (Context a)
    , Ord (Name a)
    ) => Context a -> Name a -> IO (Maybe ByteString)
readRelatedPopularForResourceFromCache ctx nm = do
  let ty = typeOf (undefined :: a)
  rpfr <- readIORef (relatedPopularForResource analyticsCache)
  pure do
    map <- Map.lookup ty rpfr
    Map.lookup (ctx,nm) (unsafeCoerce map)

readRelatedTopForResourceFromCache 
  :: forall a. 
    ( Typeable a 
    , Ord (Context a)
    , Ord (Name a)
    ) => Context a -> Name a -> IO (Maybe ByteString)
readRelatedTopForResourceFromCache ctx nm = do
  let ty = typeOf (undefined :: a)
  rtfr <- readIORef (relatedTopForResource analyticsCache)
  pure do
    map <- Map.lookup ty rtfr
    Map.lookup (ctx,nm) (unsafeCoerce map)
 
--------------------------------------------------------------------------------

analytics 
  :: ( Typeable a
     , FromJSON (Context a), ToJSON (Context a), Ord (Context a)
     , FromJSON (Name a), ToJSON (Name a), Ord (Name a)
     ) => Permissions a -> Endpoints '[] (AnalyticsAPI a) '[] (AnalyticsAPI a)
analytics ps = Endpoints analyticsAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPopularForNamespace ps 
       <:> handleListTopForNamespace ps
       <:> handleListRecentForNamespace ps
       <:> handleListPopularForContext ps
       <:> handleListTopForContext ps
       <:> handleListRecentForContext ps
       <:> handleListRelatedPopularForResource ps
       <:> handleListRelatedTopForResource ps
       <:> WS.none

handleListPopularForNamespace 
  :: forall a. 
    ( Typeable a
    , ToJSON (Context a), ToJSON (Name a)
    ) => Permissions a -> RequestHandler (ListPopularForNamespace a)
handleListPopularForNamespace Permissions {..} = responding do
  can <- liftIO canEnum 
  if can then do
    response <- liftIO (readPopularForNamespaceFromCache @a)
    case response of
      Just rsp -> customReplyRaw rsp
      Nothing  -> reply []
  else
    reply []

handleListTopForNamespace 
  :: forall a. 
    ( Typeable a
    , ToJSON (Context a), ToJSON (Name a)
    ) => Permissions a -> RequestHandler (ListTopForNamespace a)
handleListTopForNamespace Permissions {..} = responding do
  can <- liftIO canEnum 
  if can then do
    response <- liftIO (readTopForNamespaceFromCache @a)
    case response of
      Just rsp -> customReplyRaw rsp
      Nothing  -> reply []
  else
    reply []

handleListRecentForNamespace 
  :: forall a. 
    ( Typeable a
    , ToJSON (Context a), ToJSON (Name a) 
    ) => Permissions a -> RequestHandler (ListRecentForNamespace a)
handleListRecentForNamespace Permissions {..} = responding do
  can <- liftIO canEnum
  if can then do
    response <- liftIO (readRecentForNamespaceFromCache @a)
    case response of
      Just rsp -> customReplyRaw rsp
      Nothing  -> reply []
  else
    reply []

handleListPopularForContext 
  :: forall a. 
    ( Typeable a
    , FromJSON (Context a), ToJSON (Context a), Ord (Context a)
    , ToJSON (Name a)
    ) => Permissions a -> RequestHandler (ListPopularForContext a)
handleListPopularForContext Permissions {..} = responding do
  ctx <- acquire
  can <- liftIO (canList ctx) 
  if can then do
    response <- liftIO (readPopularForContextFromCache ctx)
    case response of
      Just rsp -> customReplyRaw rsp
      Nothing  -> reply []
  else
    reply []

handleListTopForContext 
  :: forall a. 
    ( Typeable a
    , FromJSON (Context a), ToJSON (Context a), Ord (Context a)
    , ToJSON (Name a)
    ) => Permissions a -> RequestHandler (ListTopForContext a)
handleListTopForContext Permissions {..} = responding do
  ctx <- acquire
  can <- liftIO (canList ctx) 
  if can then do
    response <- liftIO (readTopForContextFromCache ctx)
    case response of
      Just rsp -> customReplyRaw rsp
      Nothing  -> reply []
  else
    reply []

handleListRecentForContext 
  :: forall a. 
    ( Typeable a
    , FromJSON (Context a), ToJSON (Context a), Ord (Context a)
    , ToJSON (Name a)
    ) => Permissions a -> RequestHandler (ListRecentForContext a)
handleListRecentForContext Permissions {..} = responding do
  ctx <- acquire
  can <- liftIO (canList ctx)
  if can then do
    response <- liftIO (readRecentForContextFromCache ctx)
    case response of
      Just rsp -> customReplyRaw rsp
      Nothing  -> reply []
  else
    reply []

handleListRelatedPopularForResource 
  :: forall a. 
    ( Typeable a
    , FromJSON (Context a), Ord (Context a)
    , FromJSON (Name a), Ord (Name a)
    ) => Permissions a -> RequestHandler (ListRelatedPopularForResource a)
handleListRelatedPopularForResource Permissions {..} = responding do
  (ctx,nm) <- acquire
  can <- liftIO (canList ctx) 
  if can then do
    response <- liftIO (readRelatedPopularForResourceFromCache ctx nm)
    case response of
      Just rsp -> customReplyRaw rsp
      Nothing  -> reply []
  else
    reply []

handleListRelatedTopForResource 
  :: forall a. 
    ( Typeable a
    , FromJSON (Context a), Ord (Context a)
    , FromJSON (Name a), Ord (Name a)
    ) => Permissions a -> RequestHandler (ListRelatedTopForResource a)
handleListRelatedTopForResource Permissions {..} = responding do
  (ctx,nm) <- acquire
  can <- liftIO (canList ctx) 
  if can then do
    response <- liftIO (readRelatedTopForResourceFromCache ctx nm)
    case response of
      Just rsp -> customReplyRaw rsp
      Nothing  -> reply []
  else
    reply []
--
