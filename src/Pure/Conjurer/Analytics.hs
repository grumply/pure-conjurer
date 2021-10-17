{-# language GADTs #-}
module Pure.Conjurer.Analytics where

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Data.JSON hiding (encode,decode)
import Pure.Data.Txt
import Pure.Data.Time
import Pure.Data.Marker
import Pure.Elm.Component (Default(..))
import Pure.Sorcerer as Sorcerer
import Pure.WebSocket hiding (Nat)

import Data.Hashable

import Data.Typeable
import Data.IORef
import Data.Maybe
import GHC.Exts (Any)
import GHC.Generics
import GHC.TypeLits
import Prelude
import Unsafe.Coerce

#ifndef __GHCJS__
import qualified Data.IP as IPR (IP(..),fromSockAddr)
#endif

data IP = IP {-# UNPACK #-}!Txt
  deriving stock (Generic,Eq,Ord,Show)
  deriving anyclass (ToJSON,FromJSON,Hashable)

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

newSessionId :: IO SessionId
newSessionId = SessionId <$> markIO

--------------------------------------------------------------------------------

data SessionMsg
  = SessionStart Time IP
  | SessionUser Time Username 
  | SessionRead Time Txt
  | SessionEnd Time
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Source SessionMsg where
  data Stream SessionMsg = SessionStream SessionId
    deriving stock Generic
    deriving anyclass Hashable

  stream (SessionStream (SessionId m)) =
    "conjuredb/sessions/" ++ fromTxt (encodeBase62 m) ++ ".stream"

data Session = Session
instance Aggregable SessionMsg Session where
  update _ _ = Ignore
  
  aggregate = "session.aggregate"

--------------------------------------------------------------------------------

data AnalyticsMsg a
  = ResourceRead Time SessionId (Context a) (Name a)
  deriving stock Generic
  
deriving instance (ToJSON (Context a),ToJSON (Name a))
  => ToJSON (AnalyticsMsg a)
  
deriving instance (FromJSON (Context a),FromJSON (Name a))
  => FromJSON (AnalyticsMsg a)

instance 
  ( Typeable a
  , ToJSON (Context a), FromJSON (Context a)
  , ToJSON (Name a), FromJSON (Name a)
  ) => Source (AnalyticsMsg a) 
  where
    data Stream (AnalyticsMsg a) = AnalyticsStream 
      deriving stock Generic
      deriving anyclass Hashable

    stream AnalyticsStream =
      "conjuredb/analytics/" 
        ++ show (typeRepTyCon (typeOf (undefined :: a))) 
        ++ "/analytics.stream" 

data Analytics = Analytics 
instance 
  ( Typeable a 
  , ToJSON (Context a), FromJSON (Context a)
  , ToJSON (Name a), FromJSON (Name a)
  ) => Aggregable (AnalyticsMsg a) Analytics 
  where
    update _ _ = Ignore
    aggregate = "analytics.aggregate"

--------------------------------------------------------------------------------

recordSessionStart :: WebSocket -> IO SessionId
recordSessionStart ws = do
  ip  <- fromWebSocket ws
  sid <- newSessionId
  now <- time
  Sorcerer.write (SessionStream sid) (SessionStart now ip)
  pure sid

recordSessionUser :: SessionId -> Username -> IO ()
recordSessionUser sid un = do
  now <- time 
  Sorcerer.write (SessionStream sid) (SessionUser now un)

recordRead 
  :: forall a. 
    ( Typeable a
    , Routable a
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    ) => SessionId -> Context a -> Name a -> IO ()
recordRead sid ctx name = do
  now <- time
  Sorcerer.write (SessionStream sid) (SessionRead now (toReadRoute ctx name))
  Sorcerer.write (AnalyticsStream @a) (ResourceRead now sid ctx name)

recordSessionEnd :: SessionId -> IO ()
recordSessionEnd sid = do
  now <- time
  Sorcerer.write (SessionStream sid) (SessionEnd now)

--------------------------------------------------------------------------------  

addAnalytics 
  :: forall a.
    ( Typeable a
    , Routable a
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    )  => SessionId -> Callbacks a -> Callbacks a
addAnalytics sid cbs = cbs { onRead = analyzeRead }
  where
    analyzeRead ctx name product = do
      recordRead sid ctx name
      onRead cbs ctx name product

--------------------------------------------------------------------------------