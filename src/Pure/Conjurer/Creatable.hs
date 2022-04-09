module Pure.Conjurer.Creatable (Creatable(..),Creating) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Formable
import Pure.Conjurer.Name
import Pure.Conjurer.Pathable
import Pure.Conjurer.Producible
import Pure.Conjurer.Previewable
import Pure.Conjurer.Resource
import Pure.Conjurer.Rootable
import Pure.Conjurer.Routable
import Pure.Conjurer.Updatable (Previewing)

import qualified Pure.Data.View
import Pure.Auth (Access(..),authorize,defaultOnRegistered)
import Pure.Data.Default
import Pure.Data.JSON
import Pure.Elm.Component hiding (root)
import Pure.Maybe
import Pure.Router as Router
import Pure.Sync
import Pure.WebSocket
import Pure.WebSocket.Cache (flush)

import Data.Typeable

data Creating a
instance Theme Creating
instance {-# OVERLAPPABLE #-} Typeable a => Theme (Creating a)

class Creatable _role resource where
  toCreate :: WebSocket -> Context resource -> View
  default toCreate 
    :: ( Typeable resource, Typeable _role
       , Routable resource
       , ToJSON (Resource resource), FromJSON (Resource resource), Default (Resource resource)
       , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
       , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
       , FromJSON (Preview resource)
       , FromJSON (Product resource)
       , Formable (Resource resource)
       , Pure (Preview resource)
       , Pure (Product resource)
       , Theme (Creating resource)
       , Theme (Previewing resource)
       ) => WebSocket -> Context resource -> View
  toCreate ws ctx =
    authorize @_role (Access ws id defaultOnRegistered) $ \_ -> 
      let 
        onPreview resource = do
          r <- sync do
            request (publishingAPI @resource) ws
              (previewResource @resource)
              (ctx,resource)
          case r of
            Nothing -> pure "Failed to preview."
            Just (ctx,nm,pre,pro,res) -> pure do
              Div <| Themed @Previewing . Themed @(Previewing resource) |>
                [ View pre
                , View pro
                ]

        onSubmit resource = do
          mi <- sync do
            request (publishingAPI @resource) ws 
              (createResource @resource) 
              (ctx,resource)

          for_ mi $ \nm -> do
            flush @_role (readingAPI @resource) (readPreview @resource) (ctx,nm)
            flush @_role (readingAPI @resource) (readProduct @resource) (ctx,nm)
            Router.goto (toReadRoute ctx nm)

      in 
        Div <| Themed @Creating . Themed @(Creating resource) |>
          [ form onSubmit onPreview def
          ]

instance {-# OVERLAPPABLE #-}
  ( Typeable resource, Typeable _role
  , Routable resource
  , ToJSON (Resource resource), FromJSON (Resource resource), Default (Resource resource)
  , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
  , FromJSON (Preview resource)
  , FromJSON (Product resource)
  , Formable (Resource resource)
  , Pure (Preview resource)
  , Pure (Product resource)
  , Theme (Creating resource)
  , Theme (Previewing resource)
  ) => Creatable _role resource