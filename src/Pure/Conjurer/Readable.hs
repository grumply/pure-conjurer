module Pure.Conjurer.Readable (Readable(..)) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Pathable
import Pure.Conjurer.Producible
import Pure.Conjurer.Resource
import Pure.Conjurer.Rootable

import Pure.Data.JSON
import Pure.Elm.Component hiding (root)
import Pure.Maybe
import Pure.Router as Router
import Pure.Sync
import Pure.WebSocket

import Data.Typeable

class Readable resource where
  readRoute :: (Context resource -> Name resource -> rt) -> Routing rt ()
  default readRoute :: (Rootable resource, Pathable (Context resource), Pathable (Name resource)) => (Context resource -> Name resource -> rt) -> Routing rt ()
  readRoute f =
    void do
      path (root @resource) do
        mctx <- fromPath
        mnm  <- fromPath
        case (,) <$> mctx <*> mnm of
          Just (ctx,nm) -> dispatch (f ctx nm)
          Nothing       -> continue

  toReadRoute :: Context resource -> Name resource -> Txt
  default toReadRoute :: (Rootable resource, Pathable (Context resource), Pathable (Name resource)) => Context resource -> Name resource -> Txt
  toReadRoute ctx nm = root @resource <> toPath ctx <> toPath nm

  toRead :: WebSocket -> Context resource -> Name resource -> View
  default toRead :: (Typeable resource, Component (Product resource), FromJSON (Context resource), ToJSON (Context resource), FromJSON (Name resource), ToJSON (Name resource), FromJSON (Product resource)) => WebSocket -> Context resource -> Name resource -> View
  toRead ws ctx nm = producing producer (consuming consumer)
    where
      producer = sync (request (readingAPI @resource) ws (readProduct @resource) (ctx,nm))
      consumer = maybe "Not Found" run