module Pure.Conjurer.Listable (Listable(..),KeyedPreview(..)) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Pathable
import Pure.Conjurer.Previewable
import Pure.Conjurer.Readable
import Pure.Conjurer.Resource
import Pure.Conjurer.Rootable

import Pure.Data.JSON
import Pure.Elm.Component hiding (root)
import Pure.Router as Router
import Pure.Maybe
import Pure.Sync
import Pure.WebSocket

import Data.Typeable

class Listable resource where
  listRoute :: (Context resource -> rt) -> Routing rt ()
  default listRoute :: (Rootable resource, Pathable (Context resource)) => (Context resource -> rt) -> Routing rt ()
  listRoute f =
    void do
      path (root @resource) do
        path "/list" do
          mctx <- fromPath 
          case mctx of
            Just ctx -> dispatch (f ctx)
            Nothing  -> continue

  toListRoute :: Context resource -> Txt
  default toListRoute :: (Rootable resource, Pathable (Context resource)) => Context resource -> Txt
  toListRoute ctx = root @resource <> "/list" <> toPath ctx

  toList :: WebSocket -> Context resource -> View
  default toList 
    :: ( Typeable resource
       , Component (KeyedPreview resource)
       , Readable resource
       , FromJSON (Preview resource)
       , ToJSON (Context resource), FromJSON (Context resource)
       , ToJSON (Name resource), FromJSON (Name resource)
       ) => WebSocket -> Context resource -> View
  toList ws ctx =
     producing producer (consuming (maybe "Not Found" consumer))
    where
      producer = sync (request (readingAPI @resource) ws (readListing @resource) ctx)
      consumer ps = 
        Ul <||> 
          [ Li <| OnClick (\_ -> Router.goto (toReadRoute ctx nm)) |> 
            [ run (KeyedPreview ctx nm p) ] 
          | (nm,p) <- ps 
          ]

data KeyedPreview resource = KeyedPreview (Context resource) (Name resource) (Preview resource)
