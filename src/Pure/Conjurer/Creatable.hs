module Pure.Conjurer.Creatable (Creatable(..)) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Formable
import Pure.Conjurer.Pathable
import Pure.Conjurer.Readable
import Pure.Conjurer.Resource
import Pure.Conjurer.Rootable

import Pure.Auth (authorize)
import Pure.Data.Default
import Pure.Data.JSON
import Pure.Elm.Component hiding (root)
import Pure.Maybe
import Pure.Router as Router
import Pure.Sync
import Pure.WebSocket

import Data.Typeable

class Creatable _role resource | resource -> _role where
  createRoute :: (Context resource -> rt) -> Routing rt ()
  default createRoute :: (Rootable resource, Pathable (Context resource)) => (Context resource -> rt) -> Routing rt ()
  createRoute f =
    void do
      path (root @resource) do
        path "/new" do
          mctx <- fromPath
          case mctx of
            Just ctx -> dispatch (f ctx)
            Nothing  -> continue

  toCreateRoute :: Context resource -> Txt
  default toCreateRoute :: (Rootable resource, Pathable (Context resource)) => Context resource -> Txt
  toCreateRoute ctx = root @resource <> "/new" <> toPath ctx

  toCreate :: WebSocket -> Context resource -> View
  default toCreate 
    :: ( Typeable resource, Typeable _role
       , Readable resource
       , ToJSON (Resource resource), Default (Resource resource)
       , ToJSON (Context resource), FromJSON (Context resource)
       , FromJSON (Name resource)
       , Formable (Resource resource)
       ) => WebSocket -> Context resource -> View
  toCreate ws ctx =
    authorize @_role $ maybe "Not Authorized" $ \_ -> 
      let 
        onSubmit resource = do
          mi <- sync (request (publishingAPI @resource) ws (createResource @resource) (ctx,resource))
          for_ mi (Router.goto . toReadRoute ctx)
      in 
        form onSubmit def

