module Pure.Conjurer.Listable (Listable(..),ShouldPreloadPreviews,Listing,cachingToList) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Pathable
import Pure.Conjurer.Previewable
import Pure.Conjurer.Producible
import Pure.Conjurer.Resource
import Pure.Conjurer.Rootable
import Pure.Conjurer.Routable

import qualified Pure.Data.View
import Pure.Data.JSON
import Pure.Elm.Application (storeScrollPosition)
import Pure.Elm.Component hiding (root)
import Pure.Router as Router
import Pure.Maybe
import Pure.Sync
import Pure.WebSocket
import Pure.WebSocket.Cache hiding (sync)

import Control.Concurrent
import Data.Typeable

data Listing a
instance Theme Listing
instance {-# OVERLAPPABLE #-} Typeable a => Theme (Listing a)

class Listable resource where
  toList :: WebSocket -> Context resource -> View
  default toList 
    :: ( Typeable resource
       , Routable resource
       , Pure (Preview resource)
       , FromJSON (Preview resource)
       , ToJSON (Context resource), FromJSON (Context resource)
       , ToJSON (Name resource), FromJSON (Name resource)
       , Eq (Context resource)
       , Pathable (Context resource), Pathable (Name resource)
       , Theme (Listing resource)
       ) => WebSocket -> Context resource -> View
  toList ws ctx =
    producingKeyed ctx producer (\ctx -> consuming (maybe "Not Found" (consumer ctx)))
    where
      producer = sync .
        request (readingAPI @resource) ws 
          (readListing @resource) 

      consumer :: Context resource -> [(Name resource,Preview resource)] -> View
      consumer ctx ps = 
        Ul <| Themed @Listing . Themed @(Listing resource) |> 
          [ Li <| go (toReadRoute ctx nm) |> 
            [ View p ] 
          | (nm,p) <- ps 
          ]
        where
          go t a = OnClickWith intercept (\_ -> storeScrollPosition >> goto t) (Href t a) 

type ShouldPreloadPreviews = Bool

cachingToList 
  :: forall _role resource.
    ( Typeable _role
    , Typeable resource
    , Routable resource
    , Pure (Preview resource)
    , FromJSON (Preview resource)
    , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
    , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
    , FromJSON (Product resource)
    , Eq (Context resource)
    , Theme (Listing resource)
    ) 
  => ShouldPreloadPreviews -> WebSocket -> Context resource -> View
cachingToList shouldPreloadPreviews _ ctx =
  producingKeyed ctx producer (\ctx -> consuming (maybe "Not Found" (consumer ctx)))
  where
    producer ctx = do
      rsp <- req @_role Cached (readingAPI @resource)
        (readListing @resource) 
        ctx
      pure rsp

    consumer ctx ps = 
      Ul <| Themed @Listing . Themed @(Listing resource) |> 
        [ Li <| go (toReadRoute ctx nm) . preload ctx nm |> 
          [ View p ] 
        | (nm,p) <- ps 
        ]
      where
        go t a = OnClickWith intercept (\_ -> storeScrollPosition >> goto t) (Href t a) 
        preload ctx nm 
          | shouldPreloadPreviews = OnMouseDown load . OnTouchStart load
          | otherwise             = id
          where
            load _ = void $ forkIO $ void $
              req @_role Cached (readingAPI @resource) 
                (readProduct @resource) 
                (ctx,nm)
                