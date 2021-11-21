module Pure.Conjurer.Listable (Listable(..),ShouldPreloadPreviews,Listing,cachingToList) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Pathable
import Pure.Conjurer.Previewable
import Pure.Conjurer.Producible
import Pure.Conjurer.Resource
import Pure.Conjurer.Rootable
import Pure.Conjurer.Routable

import Pure.Data.JSON
import Pure.Elm.Application (storeScrollPosition)
import Pure.Elm.Component hiding (root)
import Pure.Router as Router
import Pure.Maybe
import Pure.Sync
import Pure.WebSocket
import Pure.WebSocket.Cache

import Control.Concurrent
import Data.Typeable

data Listing
instance Theme Listing

class Listable resource where
  toList :: WebSocket -> Context resource -> View
  default toList 
    :: ( Typeable resource
       , Routable resource
       , Theme resource
       , Component (Preview resource)
       , FromJSON (Preview resource)
       , ToJSON (Context resource), FromJSON (Context resource)
       , ToJSON (Name resource), FromJSON (Name resource)
       , Eq (Context resource)
       ) => WebSocket -> Context resource -> View
  toList ws ctx =
    producingKeyed ctx producer (\ctx -> consuming (maybe "Not Found" (consumer ctx)))
    where
      producer = sync .
        request (readingAPI @resource) ws 
          (readListing @resource) 

      consumer ctx ps = 
        Ul <| Themed @resource . Themed @Listing |> 
          [ Li <| go (toReadRoute ctx nm) |> 
            [ run p ] 
          | (nm,p) <- ps 
          ]
        where
          go t a = OnClickWith intercept (\_ -> storeScrollPosition >> goto t) (Href t a) 

type ShouldPreloadPreviews = Bool

cachingToList 
  :: forall resource.
    ( Typeable resource
    , Routable resource
    , Theme resource
    , Component (Preview resource)
    , FromJSON (Preview resource)
    , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
    , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
    , FromJSON (Product resource)
    , Eq (Context resource)
    ) 
  => ShouldPreloadPreviews -> WebSocket -> Context resource -> View
cachingToList shouldPreloadPreviews _ ctx =
  producingKeyed ctx producer (\ctx -> consuming (maybe "Not Found" (consumer ctx)))
  where
    producer ctx = do
      rsp <- req Cached (readingAPI @resource)
        (readListing @resource) 
        ctx
      pure rsp

    consumer ctx ps = 
      Ul <| Themed @resource . Themed @Listing |> 
        [ Li <| go (toReadRoute ctx nm) . preload ctx nm |> 
          [ run p ] 
        | (nm,p) <- ps 
        ]
      where
        go t a = OnClickWith intercept (\_ -> storeScrollPosition >> goto t) (Href t a) 
        preload ctx nm 
          | shouldPreloadPreviews = OnMouseDown load . OnTouchStart load
          | otherwise             = id
          where
            load _ = void $ forkIO $ void $
              req Cached (readingAPI @resource) 
                (readProduct @resource) 
                (ctx,nm)
                
instance {-# INCOHERENT #-}
  ( ToJSON (Context resource), FromJSON (Context resource), Pathable (Context resource), Eq (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource), Pathable (Name resource), Eq (Name resource)
  , Theme resource
  , FromJSON (Preview resource)
  , Component (Preview resource)
  ) => Listable resource