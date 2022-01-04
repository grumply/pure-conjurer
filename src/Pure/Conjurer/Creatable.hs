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

import Data.Typeable

data Creating
instance Theme Creating

class Creatable _role resource where
  toCreate :: WebSocket -> Context resource -> View
  default toCreate 
    :: ( Typeable resource, Typeable _role
       , Routable resource
       , ToJSON (Resource resource), FromJSON (Resource resource), Default (Resource resource)
       , ToJSON (Context resource), FromJSON (Context resource)
       , FromJSON (Name resource)
       , FromJSON (Preview resource)
       , FromJSON (Product resource)
       , Formable (Resource resource)
       , Pure (Preview resource)
       , Pure (Product resource)
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
              Div <| Themed @Previewing |>
                [ View pre
                , View pro
                ]

        onSubmit resource = do
          mi <- sync do
            request (publishingAPI @resource) ws 
              (createResource @resource) 
              (ctx,resource)
          for_ mi (Router.goto . toReadRoute ctx)
      in 
        Div <| Themed @Creating |>
          [ form onSubmit onPreview def
          ]

instance {-# INCOHERENT #-}
  ( Typeable resource, Typeable _role
  , Routable resource
  , ToJSON (Resource resource), FromJSON (Resource resource), Default (Resource resource)
  , ToJSON (Context resource), FromJSON (Context resource)
  , FromJSON (Name resource)
  , FromJSON (Preview resource)
  , FromJSON (Product resource)
  , Formable (Resource resource)
  , Pure (Preview resource)
  , Pure (Product resource)
  ) => Creatable _role resource