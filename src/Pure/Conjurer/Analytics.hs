{-# language GADTs #-}
module Pure.Conjurer.Analytics where

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Data.JSON hiding (encode,decode)
import Pure.Data.Txt
import Pure.Data.Time
import Pure.Data.Marker
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
  | SessionRead Time (Key Any)
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

data AnalyticsMsg resource
  = ResourceRead Time SessionId (Key resource)
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Typeable resource => Source (AnalyticsMsg resource) where
  data Stream (AnalyticsMsg resource) = AnalyticsStream 
    deriving stock Generic
    deriving anyclass Hashable

  stream AnalyticsStream =
    let root = "conjuredb/" ++ show (typeRepTyCon (typeOf (undefined :: resource)))
    in root ++ "/analytics.stream" 

data Analytics = Analytics 
instance Typeable resource => Aggregable (AnalyticsMsg resource) Analytics where
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

recordRead :: forall resource. Typeable resource => SessionId -> Key resource -> IO ()
recordRead sid key = do
  now <- time
  Sorcerer.write (SessionStream sid) (SessionRead now (unsafeCoerce key))
  Sorcerer.write (AnalyticsStream :: Stream (AnalyticsMsg resource)) (ResourceRead now sid key)

recordSessionEnd :: SessionId -> IO ()
recordSessionEnd sid = do
  now <- time
  Sorcerer.write (SessionStream sid) (SessionEnd now)

--------------------------------------------------------------------------------  

addAnalytics :: Typeable resource => SessionId -> Callbacks resource -> Callbacks resource
addAnalytics sid cbs = cbs { onReadProduct = analyzeRead }
  where
    analyzeRead owner key product = do
      recordRead sid key
      onReadProduct cbs owner key product

--------------------------------------------------------------------------------