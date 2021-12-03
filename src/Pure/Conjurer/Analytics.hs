{-# language GADTs, ScopedTypeVariables #-}
module Pure.Conjurer.Analytics where

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Data.Bloom as Bloom
import Pure.Data.JSON hiding (encode,decode)
import Pure.Data.Txt
import Pure.Data.Time
import Pure.Data.Marker
import Pure.Router (route)
import Pure.Elm.Component (Default(..))
import Pure.Sorcerer hiding (events)
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
  deriving (ToJSON,FromJSON,Eq,Ord,Hashable) via Marker
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
  , events    :: [(Time,Txt)]
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
      , events = []
      , ..
      }

  update (SessionUser t un) (Just ses) = 
    Sorcerer.Update ses 
      { end = t
      , user = Just un 
      }

  update (SessionEvent t ev) (Just ses) = 
    Sorcerer.Update ses 
      { end = t
      , events = (t,ev) : events ses 
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

--------------------------------------------------------------------------------

data GlobalAnalytics = GlobalAnalytics {-# UNPACK #-}!Time {-# UNPACK #-}!Int
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data GlobalAnalyticsMsg a
  = GlobalAnalyticsEvent Time SessionId
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance ( Typeable a) => Streamable (GlobalAnalyticsMsg a) where
  data Stream (GlobalAnalyticsMsg a) = GlobalAnalyticsStream
    deriving stock (Generic,Eq,Ord)
    deriving anyclass Hashable

  stream GlobalAnalyticsStream = 
    "conjurer/analytics/global/" 
      ++ fromTxt (rep @a)
      ++ ".stream"

instance ( Typeable a) => Aggregable (GlobalAnalyticsMsg a) GlobalAnalytics where
  update (GlobalAnalyticsEvent t _) Nothing = Sorcerer.Update (GlobalAnalytics t 1)
  update _ (Just (GlobalAnalytics t n)) = Sorcerer.Update (GlobalAnalytics t (n + 1))

  aggregate = "analytics.aggregate"

--------------------------------------------------------------------------------

data ContextAnalytics = ContextAnalytics {-# UNPACK #-}!Time {-# UNPACK #-}!Int
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data ContextAnalyticsMsg a
  = ContextAnalyticsEvent Time SessionId
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance 
  ( Typeable a
  , Hashable (Context a), Pathable (Context a)
  ) => Streamable (ContextAnalyticsMsg a) 
  where
    data Stream (ContextAnalyticsMsg a) = ContextAnalyticsStream (Context a)
      deriving stock Generic

    stream (ContextAnalyticsStream ctx) = 
      "conjurer/analytics/contexts/" 
        ++ fromTxt (rep @a)
        ++ fromTxt (toPath ctx)
        ++ ".stream"

deriving instance (Eq (Context a)) => Eq (Stream (ContextAnalyticsMsg a))
deriving instance (Ord (Context a)) => Ord (Stream (ContextAnalyticsMsg a))
deriving instance (Hashable (Context a)) => Hashable (Stream (ContextAnalyticsMsg a))

instance 
  ( Typeable a
  , Hashable (Context a), Pathable (Context a)
  ) => Aggregable (ContextAnalyticsMsg a) ContextAnalytics 
  where
    update (ContextAnalyticsEvent t _) Nothing = Sorcerer.Update (ContextAnalytics t 1)
    update _ (Just (ContextAnalytics t n)) = Sorcerer.Update (ContextAnalytics t (n + 1))

    aggregate = "analytics.aggregate"

--------------------------------------------------------------------------------

data ResourceAnalytics = ResourceAnalytics {-# UNPACK #-}!Time {-# UNPACK #-}!Int
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data ResourceAnalyticsMsg a
  = ResourceAnalyticsEvent Time SessionId
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance 
  ( Typeable a
  , Hashable (Context a), Pathable (Context a)
  , Hashable (Name a), Pathable (Name a)
  ) => Streamable (ResourceAnalyticsMsg a) 
  where
    data Stream (ResourceAnalyticsMsg a) = ResourceAnalyticsStream (Context a) (Name a)
      deriving stock Generic

    stream (ResourceAnalyticsStream ctx nm) = 
      "conjurer/analytics/resources/" 
        ++ fromTxt (rep @a)
        ++ fromTxt (toPath ctx)
        ++ fromTxt (toPath nm)
        ++ ".stream"

deriving instance (Eq (Context a) , Eq (Name a)) => Eq (Stream (ResourceAnalyticsMsg a))
deriving instance (Ord (Context a), Ord (Name a)) => Ord (Stream (ResourceAnalyticsMsg a))
deriving instance (Hashable (Context a), Hashable (Name a)) => Hashable (Stream (ResourceAnalyticsMsg a))

instance 
  ( Typeable a
  , Hashable (Context a), Pathable (Context a)
  , Hashable (Name a), Pathable (Name a)
  ) => Aggregable (ResourceAnalyticsMsg a) ResourceAnalytics 
  where
    update (ResourceAnalyticsEvent t _) Nothing = Sorcerer.Update (ResourceAnalytics t 1)
    update _ (Just (ResourceAnalytics t n)) = Sorcerer.Update (ResourceAnalytics t (n + 1))

    aggregate = "analytics.aggregate"

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
    ) => SessionId -> Context a -> Name a -> IO ()
recordRead sid ctx nm = do
  now <- time

  Sorcerer.write (SessionStream sid) do
    SessionEvent now (toReadRoute ctx nm)

  Sorcerer.write (GlobalAnalyticsStream @a) do
    GlobalAnalyticsEvent now sid

  Sorcerer.write (ContextAnalyticsStream ctx) do
    ContextAnalyticsEvent now sid

  Sorcerer.write (ResourceAnalyticsStream ctx nm) do
    ResourceAnalyticsEvent now sid

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
    )  => SessionId -> Callbacks a -> Callbacks a
addAnalytics sid cbs = cbs { onRead = analyzeRead }
  where
    analyzeRead ctx name product = do
      recordRead sid ctx name
      onRead cbs ctx name product

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

--------------------------------------------------------------------------------

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
listSessions = streamNub . fmap getSessionId <$> Sorcerer.events SessionsStream
  where getSessionId (SessionCreated _ sid) = sid

analyticsCountForNamespace 
  :: forall a. 
    ( Typeable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => IO Int
analyticsCountForNamespace =
  Sorcerer.read (GlobalAnalyticsStream @a) >>= \case
    Just (GlobalAnalytics _ n) -> pure n
    _ -> pure 0

oldestSessionForNamespace 
  :: forall a. 
    ( Typeable a 
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => IO Time
oldestSessionForNamespace =
  Sorcerer.read (GlobalAnalyticsStream @a) >>= \case
    Just (GlobalAnalytics t _) -> pure t
    _ -> time

listSessionsForNamespace 
  :: forall a. 
    ( Typeable a 
    , Hashable (Context a), Pathable (Context a)
    , Hashable (Name a), Pathable (Name a)
    ) => IO [SessionId]
listSessionsForNamespace = 
  streamNub . fmap getSessionId <$> Sorcerer.events (GlobalAnalyticsStream @a)
  where 
    getSessionId (GlobalAnalyticsEvent _ sid) = sid

listUnique
  :: forall a.
     ( Typeable a
     , Hashable (Context a), Pathable (Context a), Ord (Context a)
     , Hashable (Name a), Pathable (Name a), Ord (Name a)
     ) => IO [(Context a,Name a)]
listUnique = do
  uniqueSessionIds <- listSessionsForNamespace @a
  streamNub . catMaybes . Prelude.concatMap resources <$> sessions uniqueSessionIds
  where
    resources Session {..} = fmap parse events

    parse (_,evt) = unsafePerformIO (route (readRoute (,)) evt)

listUniqueContexts
  :: forall a.
    ( Typeable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a)
    ) => IO [Context a]
listUniqueContexts = do
  uniqueSessionIds <- listSessionsForNamespace @a
  streamNub . catMaybes . Prelude.concatMap contexts <$> sessions uniqueSessionIds
  where
    contexts Session {..} = fmap parse events

    parse (_,evt) =
      case unsafePerformIO (route (readRoute (,)) evt) of
        Just (ctx,_) -> Just ctx
        _ -> Nothing

analyticsCountForContext 
  :: forall a. 
    ( Typeable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> IO Int
analyticsCountForContext ctx =
  Sorcerer.read (ContextAnalyticsStream ctx) >>= \case
    Just (ContextAnalytics _ n) -> pure n
    _ -> pure 0

oldestSessionForContext
  :: forall a. 
    ( Typeable a 
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> IO Time
oldestSessionForContext ctx =
  Sorcerer.read (ContextAnalyticsStream ctx) >>= \case
    Just (ContextAnalytics t _) -> pure t
    _ -> time

listSessionsForContext 
  :: forall a. 
    ( Typeable a
    , Hashable (Context a), Pathable (Context a)
    , Hashable (Name a), Pathable (Name a)
    ) => Context a -> IO [SessionId]
listSessionsForContext ctx = 
  streamNub . fmap getSessionId <$> Sorcerer.events (ContextAnalyticsStream ctx)
  where 
    getSessionId (ContextAnalyticsEvent _ sid) = sid

listUniqueResources
  :: forall a.
    ( Typeable a
    , Hashable (Context a), Pathable (Context a), Eq (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> IO [Name a]
listUniqueResources ctx = do
  uniqueSessionIds <- listSessionsForNamespace @a
  streamNub . catMaybes . Prelude.concatMap resources <$> sessions uniqueSessionIds
  where
    resources Session {..} = fmap parse events

    parse (_,evt) =
      case unsafePerformIO (route (readRoute (,)) evt) of
        Just (ctx',nm) | ctx == ctx' -> Just nm
        _ -> Nothing

analyticsCountForResource
  :: forall a. 
    ( Typeable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> Name a -> IO Int
analyticsCountForResource ctx nm =
  Sorcerer.read (ResourceAnalyticsStream ctx nm) >>= \case
    Just (ResourceAnalytics _ n) -> pure n
    _ -> pure 0

oldestSessionForResource
  :: forall a. 
    ( Typeable a 
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> Name a -> IO Time
oldestSessionForResource ctx nm =
  Sorcerer.read (ResourceAnalyticsStream ctx nm) >>= \case
    Just (ResourceAnalytics t _) -> pure t
    _ -> time

listSessionsForResource 
  :: forall a. 
    ( Typeable a
    , Hashable (Context a), Pathable (Context a)
    , Hashable (Name a), Pathable (Name a)
    ) => Context a -> Name a -> IO [SessionId]
listSessionsForResource ctx nm = 
  streamNub . fmap getSessionId <$> Sorcerer.events (ResourceAnalyticsStream ctx nm)
  where 
    getSessionId (ResourceAnalyticsEvent _ sid) = sid

session :: SessionId -> IO (Maybe Session)
session = Sorcerer.read . SessionStream

sessions :: [SessionId] -> IO [Session]
sessions = fmap catMaybes . traverse session

namespaceSessions
  :: forall a. 
    ( Typeable a
    , Hashable (Context a), Pathable (Context a)
    , Hashable (Name a), Pathable (Name a)
    ) => IO [Session]
namespaceSessions = 
  listSessionsForNamespace @a >>= sessions

contextSessions
  :: forall a. 
    ( Typeable a
    , Hashable (Context a), Pathable (Context a)
    , Hashable (Name a), Pathable (Name a)
    ) => Context a -> IO [Session]
contextSessions ctx = 
  listSessionsForContext ctx >>= sessions

resourceSessions 
  :: forall a. 
    ( Typeable a
    , Hashable (Context a), Pathable (Context a)
    , Hashable (Name a), Pathable (Name a)
    ) => Context a -> Name a -> IO [Session]
resourceSessions ctx nm = 
  listSessionsForResource ctx nm >>= sessions

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

data ListRelatedTopForNamespace a
instance Identify (ListRelatedTopForNamespace a)
instance Typeable a => Request (ListRelatedTopForNamespace a) where
  type Req (ListRelatedTopForNamespace a) = (Int,())
  type Rsp (ListRelatedTopForNamespace a) = [Txt]

listRelatedTopForNamespace :: Proxy (ListRelatedTopForNamespace a)
listRelatedTopForNamespace = Proxy

data ListRelatedPopularForNamespace a
instance Identify (ListRelatedPopularForNamespace a)
instance Typeable a => Request (ListRelatedPopularForNamespace a) where
  type Req (ListRelatedPopularForNamespace a) = (Int,())
  type Rsp (ListRelatedPopularForNamespace a) = [Txt]

listRelatedPopularForNamespace :: Proxy (ListRelatedPopularForNamespace a)
listRelatedPopularForNamespace = Proxy

data ListRelatedTopForContext a
instance Identify (ListRelatedTopForContext a) where
instance Typeable a => Request (ListRelatedTopForContext a) where
  type Req (ListRelatedTopForContext a) = (Int,Context a)
  type Rsp (ListRelatedTopForContext a) = [Txt]

listRelatedTopForContext :: Proxy (ListRelatedTopForContext a)
listRelatedTopForContext = Proxy

data ListRelatedPopularForContext a
instance Identify (ListRelatedPopularForContext a) where
instance Typeable a => Request (ListRelatedPopularForContext a) where
  type Req (ListRelatedPopularForContext a) = (Int,Context a)
  type Rsp (ListRelatedPopularForContext a) = [Txt]

listRelatedPopularForContext :: Proxy (ListRelatedPopularForContext a)
listRelatedPopularForContext = Proxy

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
   , ListRelatedPopularForNamespace a
   , ListRelatedTopForNamespace a
   , ListRelatedPopularForContext a
   , ListRelatedTopForContext a
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
       <:> listRelatedPopularForNamespace @a
       <:> listRelatedTopForNamespace @a
       <:> listRelatedPopularForContext @a
       <:> listRelatedTopForContext @a
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
-- TODO: Rewrite. This naive approach will not scale well.
-- An online algorithm would be much better, even if it is persisted.

buildPopularForNamespace 
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a), ToJSON (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a), ToJSON (Name a)
    ) => IO [(Context a,Name a)]
buildPopularForNamespace = do
  Milliseconds (fromIntegral -> now) _ <- time
  c <- fromIntegral <$> analyticsCountForNamespace @a
  ctxnms <- listUnique @a
  counts <- Map.toList <$> foldM withContextAndName Map.empty ctxnms
  let 
    ranked = fmap (fmap (\(b,t,v) -> let (!newt,!newv) = upd (t,v) (now,1) in rank newv c)) counts
    sorted = List.sortBy (flip compare `on` snd) ranked
    result = fmap fst sorted
  pure result
  where
    Milliseconds (fromIntegral -> d) _ = Day 

    upd (oldt,oldv) (newt,newv) = 
      let v = oldv * 2 ** ((oldt - newt) / d) + newv
      in (newt,v)

    withContextAndName acc (ctx,nm) = do
      n <- analyticsCountForResource ctx nm
      ss <- listSessionsForResource ctx nm >>= sessions
      foldM (withSession n) acc ss
      where
        withSession n acc Session {..} = 
          foldM withEvent acc events
          where
            withEvent acc (Milliseconds (fromIntegral -> t) _,evt) = do
              mctxnm <- route (readRoute (,)) evt
              case mctxnm of
                Just (ctx',nm') | ctx == ctx', nm == nm' ->
                  case Map.lookup (ctx,nm) acc of
                    Nothing -> do
                      b <- new 0.01 n
                      Bloom.add b ip
                      pure $ Map.insert (ctx,nm) (b,t,1) acc
                    Just (b,oldt,oldv) -> do
                      updated <- Bloom.update b ip
                      if updated then 
                        let (!newt,!newv) = upd (oldt,oldv) (t,1)
                        in pure $ Map.insert (ctx,nm) (b,newt,newv) acc
                      else
                        pure acc
                _ -> 
                  pure acc

buildTopForNamespace 
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => IO [(Context a,Name a)]
buildTopForNamespace = do
  c <- fromIntegral <$> analyticsCountForNamespace @a
  ctxnms <- listUnique @a
  counts <- Map.toList <$> foldM withContextAndName Map.empty ctxnms
  let 
    ranked = fmap (fmap (\(b,v) -> rank v c)) counts
    sorted = List.sortBy (flip compare `on` snd) ranked
    result = fmap fst sorted
  pure result
  where
    Milliseconds (fromIntegral -> d) _ = Day 

    withContextAndName acc (ctx,nm) = do
      n <- analyticsCountForResource ctx nm
      ss <- listSessionsForResource ctx nm >>= sessions
      foldM (withSession n) acc ss
      where
        withSession n acc Session {..} =
          foldM withEvent acc events
          where
            withEvent acc (_,evt) = do
              mctxnm <- route (readRoute (,)) evt
              case mctxnm of
                Just (ctx',nm') | ctx == ctx', nm == nm' ->
                  case Map.lookup (ctx,nm) acc of
                    Nothing -> do
                      b <- new 0.01 n
                      Bloom.add b ip
                      pure $! Map.insert (ctx,nm) (b,1) acc
                    Just (b,v) -> do
                      updated <- Bloom.update b ip
                      if updated then 
                        pure $! Map.insert (ctx,nm) (b,v + 1) acc
                      else
                        pure acc
                _ ->
                  pure acc

buildRecentForNamespace 
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => IO [(Context a,Name a)]
buildRecentForNamespace = do
  ctxs <- listUniqueContexts @a
  ages <- Map.toList <$> foldM withContext Map.empty ctxs
  let
    sorted = List.sortBy (flip compare `on` snd) ages
    result = fmap fst sorted
  pure result
  where
    withContext acc ctx = do
      nms <- listUniqueResources ctx
      foldM withResource acc nms
      where
        withResource acc nm = do
          t <- oldestSessionForResource ctx nm
          pure (Map.insert (ctx,nm) t acc)

buildPopularForContext 
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> IO [(Context a,Name a)]
buildPopularForContext ctx = do
  Milliseconds (fromIntegral -> now) _ <- time
  c <- fromIntegral <$> analyticsCountForNamespace @a
  nms <- listUniqueResources ctx
  counts <- Map.toList <$> foldM withResource Map.empty nms
  let 
    ranked = fmap (fmap (\(b,t,v) -> let (newt,newv) = upd (t,v) (now,1) in rank newv c)) counts
    sorted = List.sortBy (flip compare `on` snd) ranked
    result = fmap fst sorted
  pure result
  where
    Milliseconds (fromIntegral -> d) _ = Day 

    upd (oldt,oldv) (newt,newv) = 
      let !v = oldv * 2 ** ((oldt - newt) / d) + newv
      in (newt,v)

    withResource acc nm = do
      n <- analyticsCountForResource ctx nm
      ss <- listSessionsForResource ctx nm >>= sessions
      foldM (withSession n) acc ss
      where
        withSession n acc Session {..} =
          foldM withEvent acc events
          where
            withEvent acc (Milliseconds (fromIntegral -> t) _,evt) = do
              mctxnm <- route (readRoute (,)) evt
              case mctxnm of
                Just (ctx',nm') | ctx == ctx', nm == nm' ->
                  case Map.lookup (ctx,nm) acc of
                    Nothing -> do
                      b <- new 0.01 n
                      Bloom.add b ip
                      pure $! Map.insert (ctx,nm) (b,t,1) acc
                    Just (b,oldt,oldv) -> do
                      updated <- Bloom.update b ip
                      if updated then 
                        let (newt,newv) = upd (oldt,oldv) (t,1)
                        in pure $! Map.insert (ctx,nm) (b,newt,newv) acc
                      else
                        pure acc
                _ ->
                  pure acc

buildTopForContext 
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> IO [(Context a,Name a)]
buildTopForContext ctx = do
  c <- fromIntegral <$> analyticsCountForNamespace @a
  nms <- listUniqueResources ctx
  counts <- Map.toList <$> foldM withResource Map.empty nms
  let 
    ranked = fmap (fmap (\(b,v) -> rank v c)) counts
    sorted = List.sortBy (flip compare `on` snd) ranked
    result = fmap fst sorted
  pure result
  where
    withResource acc nm = do
      n <- analyticsCountForResource ctx nm
      ss <- listSessionsForResource ctx nm >>= sessions
      foldM (withSession n) acc ss
      where
        withSession n acc Session {..} = 
          foldM withEvent acc events
          where
            withEvent acc (_,evt) = do
              mctxnm <- route (readRoute (,)) evt
              case mctxnm of
                Just (ctx',nm') | ctx == ctx', nm == nm' ->
                  case Map.lookup (ctx,nm) acc of
                    Nothing -> do
                      b <- new 0.01 n
                      Bloom.add b ip
                      pure $! Map.insert (ctx,nm) (b,1) acc
                    Just (b,oldv) -> do
                      updated <- Bloom.update b ip
                      if updated then 
                        pure $! Map.insert (ctx,nm) (b,oldv + 1) acc
                      else
                        pure acc
                _ ->
                  pure acc

buildRecentForContext
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> IO [(Context a,Name a)]
buildRecentForContext ctx = do
  nms <- listUniqueResources ctx
  ages <- Map.toList <$> foldM withResource Map.empty nms
  let
    sorted = List.sortBy (flip compare `on` snd) ages
    result = fmap fst sorted
  pure result
  where
    withResource acc nm = do
      t <- oldestSessionForResource ctx nm
      pure (Map.insert (ctx,nm) t acc)

-- Consider weighting distance from target in session.
buildRelatedPopularForNamespace
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => IO [Txt]
buildRelatedPopularForNamespace = do
  Milliseconds (fromIntegral -> now) _ <- time
  c <- analyticsCountForNamespace @a
  ss <- listSessionsForNamespace @a >>= sessions
  counts <- Map.toList <$> foldM (withSession c) Map.empty ss
  let 
    ranked = fmap (fmap (\(b,t,v) -> let (newt,newv) = upd (t,v) (now,1) in rank newv (fromIntegral c))) counts
    sorted = List.sortBy (flip compare `on` snd) ranked
    result = fmap fst sorted
  pure result
  where
    Milliseconds (fromIntegral -> d) _ = Day 

    upd (oldt,oldv) (newt,newv) = 
      let !v = oldv * 2 ** ((oldt - newt) / d) + newv
      in (newt,v)

    withSession c acc Session {..} =
      foldM withEvent acc events
      where
        withEvent acc (Milliseconds (fromIntegral -> t) _,evt) = 
          case Map.lookup evt acc of
            Nothing -> do
              b <- new 0.01 c
              Bloom.add b ip
              pure $! Map.insert evt (b,t,1) acc
            Just (b,oldt,oldv) -> do
              updated <- Bloom.update b ip
              if updated then
                let (newt,newv) = upd (oldt,oldv) (t,1)
                in pure $! Map.insert evt (b,newt,newv) acc
              else
                pure acc

-- Consider weighting distance from target in session.
buildRelatedTopForNamespace 
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => IO [Txt]
buildRelatedTopForNamespace = do
  c <- analyticsCountForNamespace @a
  ss <- listSessionsForNamespace @a >>= sessions
  counts <- Map.toList <$> foldM (withSession c) Map.empty ss
  let 
    ranked = fmap (fmap (\(b,v) -> rank (fromIntegral v) (fromIntegral c))) counts
    sorted = List.sortBy (flip compare `on` snd) ranked
    result = fmap fst sorted
  pure result
  where
    withSession c acc Session {..} =
      foldM withEvent acc events
      where
        withEvent acc (_,evt) = 
          case Map.lookup evt acc of
            Nothing -> do
              b <- new 0.01 c
              Bloom.add b ip
              pure $! Map.insert evt (b,1) acc
            Just (b,oldv) -> do
              updated <- Bloom.update b ip
              if updated then
                pure $! Map.insert evt (b,oldv + 1) acc
              else
                pure acc

-- Consider weighting distance from target in session.
buildRelatedPopularForContext 
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> IO [Txt]
buildRelatedPopularForContext ctx = do
  Milliseconds (fromIntegral -> now) _ <- time
  c <- analyticsCountForNamespace @a
  ss <- listSessionsForContext ctx >>= sessions
  counts <- Map.toList <$> foldM (withSession c) Map.empty ss
  let 
    ranked = fmap (fmap (\(b,t,v) -> let (newt,newv) = upd (t,v) (now,1) in rank newv (fromIntegral c))) counts
    sorted = List.sortBy (flip compare `on` snd) ranked
    result = fmap fst sorted
  pure result
  where
    Milliseconds (fromIntegral -> d) _ = Day 

    upd (oldt,oldv) (newt,newv) = 
      let !v = oldv * 2 ** ((oldt - newt) / d) + newv
      in (newt,v)

    withSession c acc Session {..} =
      foldM withEvent acc events
      where
        withEvent acc (Milliseconds (fromIntegral -> t) _,evt) = 
          case Map.lookup evt acc of
            Nothing -> do
              b <- new 0.01 c
              Bloom.add b ip
              pure $! Map.insert evt (b,t,1) acc
            Just (b,oldt,oldv) -> do
              updated <- Bloom.update b ip
              if updated then
                let (newt,newv) = upd (oldt,oldv) (t,1)
                in pure $! Map.insert evt (b,newt,newv) acc
              else
                pure acc

-- Consider weighting distance from target in session.
buildRelatedTopForContext 
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> IO [Txt]
buildRelatedTopForContext ctx = do
  c <- analyticsCountForNamespace @a
  ss <- listSessionsForContext ctx >>= sessions
  counts <- Map.toList <$> foldM (withSession c) Map.empty ss
  let 
    ranked = fmap (fmap (\(b,v) -> rank (fromIntegral v) (fromIntegral c))) counts
    sorted = List.sortBy (flip compare `on` snd) ranked
    result = fmap fst sorted
  pure result
  where
    withSession c acc Session {..} =
      foldM withEvent acc events
      where
        withEvent acc (_,evt) = 
          case Map.lookup evt acc of
            Nothing -> do
              b <- new 0.01 c
              Bloom.add b ip
              pure $! Map.insert evt (b,1) acc
            Just (b,oldv) -> do
              updated <- Bloom.update b ip
              if updated then
                pure $! Map.insert evt (b,oldv + 1) acc
              else
                pure acc

-- Consider weighting distance from target in session.
buildRelatedPopularForResource 
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> Name a -> IO [Txt]
buildRelatedPopularForResource ctx nm = do
  Milliseconds (fromIntegral -> now) _ <- time
  c <- analyticsCountForNamespace @a
  ss <- listSessionsForResource ctx nm >>= sessions
  counts <- Map.toList <$> foldM (withSession c) Map.empty ss
  let 
    ranked = fmap (fmap (\(b,t,v) -> let (newt,newv) = upd (t,v) (now,1) in rank newv (fromIntegral c))) counts
    sorted = List.sortBy (flip compare `on` snd) ranked
    result = fmap fst sorted
  pure result
  where
    Milliseconds (fromIntegral -> d) _ = Day 

    upd (oldt,oldv) (newt,newv) = 
      let !v = oldv * 2 ** ((oldt - newt) / d) + newv
      in (newt,v)

    withSession c acc Session {..} =
      foldM withEvent acc events
      where
        withEvent acc (Milliseconds (fromIntegral -> t) _,evt) = 
          case Map.lookup evt acc of
            Nothing -> do
              b <- new 0.01 c
              Bloom.add b ip
              pure $! Map.insert evt (b,t,1) acc
            Just (b,oldt,oldv) -> do
              updated <- Bloom.update b ip
              if updated then
                let (newt,newv) = upd (oldt,oldv) (t,1)
                in pure $! Map.insert evt (b,newt,newv) acc
              else
                pure acc

-- Consider weighting distance from target in session.
buildRelatedTopForResource 
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> Name a -> IO [Txt]
buildRelatedTopForResource ctx nm = do
  c <- analyticsCountForNamespace @a
  ss <- listSessionsForResource ctx nm >>= sessions
  counts <- Map.toList <$> foldM (withSession c) Map.empty ss
  let 
    ranked = fmap (fmap (\(b,v) -> rank (fromIntegral v) (fromIntegral c))) counts
    sorted = List.sortBy (flip compare `on` snd) ranked
    result = fmap fst sorted
  pure result
  where
    withSession c acc Session {..} =
      foldM withEvent acc events
      where
        withEvent acc (_,evt) = 
          case Map.lookup evt acc of
            Nothing -> do
              b <- new 0.01 c
              Bloom.add b ip
              pure $! Map.insert evt (b,1) acc
            Just (b,oldv) -> do
              updated <- Bloom.update b ip
              if updated then
                pure $! Map.insert evt (b,oldv + 1) acc
              else
                pure acc

--------------------------------------------------------------------------------

data AnalyticsCache = AnalyticsCache
  { popularForNamespace        :: IORef (Map TypeRep ByteString)
  , topForNamespace            :: IORef (Map TypeRep ByteString)
  , recentForNamespace         :: IORef (Map TypeRep ByteString)
  , popularForContext          :: IORef (Map TypeRep (Map Any ByteString))
  , topForContext              :: IORef (Map TypeRep (Map Any ByteString))
  , recentForContext           :: IORef (Map TypeRep (Map Any ByteString))
  , relatedPopularForNamespace :: IORef (Map TypeRep ByteString)
  , relatedTopForNamespace     :: IORef (Map TypeRep ByteString)
  , relatedPopularForContext   :: IORef (Map TypeRep (Map Any ByteString))
  , relatedTopForContext       :: IORef (Map TypeRep (Map Any ByteString))
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

addRelatedPopularForNamespaceToCache
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => [Txt] -> IO ()
addRelatedPopularForNamespaceToCache rpfn = do
  let ty = typeOf (undefined :: a)
  atomicModifyIORef' (relatedPopularForNamespace analyticsCache) $ \map ->
    (Map.insert ty (encodeBS rpfn) map,())
    
addRelatedTopForNamespaceToCache
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => [Txt] -> IO ()
addRelatedTopForNamespaceToCache rtfn = do
  let ty = typeOf (undefined :: a)
  atomicModifyIORef' (relatedTopForNamespace analyticsCache) $ \map ->
    (Map.insert ty (encodeBS rtfn) map,())
     
addRelatedPopularForContextToCache
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> [Txt] -> IO ()
addRelatedPopularForContextToCache ctx rpfc = do
  let ty = typeOf (undefined :: a)
  atomicModifyIORef' (relatedPopularForContext analyticsCache) $ \old ->
    case Map.lookup ty old of
      Nothing  -> (Map.insert ty (unsafeCoerce $ Map.singleton ctx (encodeBS rpfc)) old,())
      Just tym -> (Map.insert ty (unsafeCoerce $ Map.insert ctx (encodeBS rpfc) (unsafeCoerce tym)) old,())

addRelatedTopForContextToCache
  :: forall a. 
    ( Typeable a
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Context a -> [Txt] -> IO ()
addRelatedTopForContextToCache ctx rtfc = do
  let ty = typeOf (undefined :: a)
  atomicModifyIORef' (relatedTopForContext analyticsCache) $ \old ->
    case Map.lookup ty old of
      Nothing  -> (Map.insert ty (unsafeCoerce $ Map.singleton ctx (encodeBS rtfc)) old,())
      Just tym -> (Map.insert ty (unsafeCoerce $ Map.insert ctx (encodeBS rtfc) (unsafeCoerce tym)) old,())
   
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

readRelatedPopularForNamespaceFromCache
  :: forall a.
    ( Typeable a
    ) => IO (Maybe ByteString)
readRelatedPopularForNamespaceFromCache = do
  let ty = typeOf (undefined :: a)
  rpfn <- readIORef (relatedPopularForNamespace analyticsCache)
  pure (Map.lookup ty rpfn)
  
readRelatedTopForNamespaceFromCache
  :: forall a.
    ( Typeable a
    ) => IO (Maybe ByteString)
readRelatedTopForNamespaceFromCache = do
  let ty = typeOf (undefined :: a)
  rtfn <- readIORef (relatedPopularForNamespace analyticsCache)
  pure (Map.lookup ty rtfn)

readRelatedPopularForContextFromCache 
  :: forall a. 
    ( Typeable a 
    , Ord (Context a)
    ) => Context a -> IO (Maybe ByteString)
readRelatedPopularForContextFromCache ctx = do
  let ty = typeOf (undefined :: a)
  rpfc <- readIORef (relatedPopularForContext analyticsCache)
  pure do
    map <- Map.lookup ty rpfc
    Map.lookup ctx (unsafeCoerce map)

readRelatedTopForContextFromCache 
  :: forall a. 
    ( Typeable a 
    , Ord (Context a)
    ) => Context a -> IO (Maybe ByteString)
readRelatedTopForContextFromCache ctx = do
  let ty = typeOf (undefined :: a)
  rtfc <- readIORef (relatedTopForContext analyticsCache)
  pure do
    map <- Map.lookup ty rtfc
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
       <:> handleListRelatedPopularForNamespace ps
       <:> handleListRelatedTopForNamespace ps
       <:> handleListRelatedPopularForContext ps
       <:> handleListRelatedTopForContext ps
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

handleListRelatedPopularForNamespace 
  :: forall a. 
    ( Typeable a 
    ) => Permissions a -> RequestHandler (ListRelatedPopularForNamespace a)
handleListRelatedPopularForNamespace Permissions {..} = responding do
  can <- liftIO canEnum 
  if can then do
    response <- liftIO (readRelatedPopularForNamespaceFromCache @a)
    case response of
      Just rsp -> customReplyRaw rsp
      Nothing  -> reply []
  else
    reply []

handleListRelatedTopForNamespace 
  :: forall a. 
    ( Typeable a 
    ) => Permissions a -> RequestHandler (ListRelatedTopForNamespace a)
handleListRelatedTopForNamespace Permissions {..} = responding do
  can <- liftIO canEnum 
  if can then do
    response <- liftIO (readRelatedTopForNamespaceFromCache @a)
    case response of
      Just rsp -> customReplyRaw rsp
      Nothing  -> reply []
  else
    reply []

handleListRelatedPopularForContext 
  :: forall a. 
    ( Typeable a
    , FromJSON (Context a), Ord (Context a)
    ) => Permissions a -> RequestHandler (ListRelatedPopularForContext a)
handleListRelatedPopularForContext Permissions {..} = responding do
  ctx <- acquire
  can <- liftIO (canList ctx) 
  if can then do
    response <- liftIO (readRelatedPopularForContextFromCache ctx)
    case response of
      Just rsp -> customReplyRaw rsp
      Nothing  -> reply []
  else
    reply []

handleListRelatedTopForContext 
  :: forall a. 
    ( Typeable a
    , FromJSON (Context a), Ord (Context a)
    ) => Permissions a -> RequestHandler (ListRelatedTopForContext a)
handleListRelatedTopForContext Permissions {..} = responding do
  ctx <- acquire
  can <- liftIO (canList ctx) 
  if can then do
    response <- liftIO (readRelatedTopForContextFromCache ctx)
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

--------------------------------------------------------------------------------

analyze 
  :: forall a.
    ( Typeable a 
    , Routable a
    , Hashable (Context a), Pathable (Context a), Ord (Context a), ToJSON (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a), ToJSON (Name a)
    ) => IO ()
analyze = do
  buildPopularForNamespace @a >>= addPopularForNamespaceToCache
  buildTopForNamespace @a >>= addTopForNamespaceToCache
  buildRecentForNamespace @a >>= addRecentForNamespaceToCache
  buildRelatedPopularForNamespace @a >>= addRelatedPopularForNamespaceToCache @a
  buildRelatedTopForNamespace @a >>= addRelatedTopForNamespaceToCache @a

  ctxs <- listUniqueContexts @a
  for_ ctxs $ \ctx -> do 
    buildPopularForContext ctx >>= addPopularForContextToCache ctx
    buildTopForContext ctx >>= addTopForContextToCache ctx
    buildRecentForContext ctx >>= addRecentForContextToCache ctx
    buildRelatedPopularForContext ctx >>= addRelatedPopularForContextToCache ctx
    buildRelatedTopForContext ctx >>= addRelatedTopForContextToCache ctx

    nms <- listUniqueResources ctx
    for_ nms $ \nm -> do
      buildRelatedPopularForResource ctx nm >>= addRelatedPopularForResourceToCache ctx nm
      buildRelatedTopForResource ctx nm >>= addRelatedTopForResourceToCache ctx nm
 