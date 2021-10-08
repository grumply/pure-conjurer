module Pure.Conjurer.Updatable (Updatable(..)) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Formable
import Pure.Conjurer.Pathable
import Pure.Conjurer.Readable
import Pure.Conjurer.Resource
import Pure.Conjurer.Rootable

import Pure.Auth (authorize)
import Pure.Data.JSON
import Pure.Elm.Component hiding (root)
import Pure.Maybe
import Pure.Router as Router
import Pure.Sync
import Pure.WebSocket

import Data.Typeable

class Updatable _role resource | resource -> _role where
  updateRoute :: (Context resource -> Name resource -> rt) -> Routing rt ()
  default updateRoute :: (Rootable resource, Pathable (Context resource), Pathable (Name resource)) => (Context resource -> Name resource -> rt) -> Routing rt ()
  updateRoute f =
    void do
      path (root @resource) do
        path "/update" do
          mctx <- fromPath
          mn   <- fromPath
          case (,) <$> mctx <*> mn of
            Just (ctx,nm) -> dispatch (f ctx nm)
            Nothing       -> continue

  toUpdateRoute :: Context resource -> Name resource -> Txt
  default toUpdateRoute :: (Rootable resource, Pathable (Context resource), Pathable (Name resource)) => Context resource -> Name resource -> Txt
  toUpdateRoute ctx nm = root @resource <> "/update" <> toPath ctx <> toPath nm 

  toUpdate :: WebSocket -> Context resource -> Name resource -> View
  default toUpdate 
    :: ( Typeable resource, Typeable _role
       , ToJSON (Context resource), FromJSON (Context resource)
       , ToJSON (Name resource)
       , ToJSON (Resource resource), FromJSON (Resource resource)
       , Readable resource
       , Formable (Resource resource)
       ) => WebSocket -> Context resource -> Name resource -> View
  toUpdate ws ctx nm =
    authorize @_role $ maybe "Not Authorized" $ \_ ->
      producing producer (consuming consumer)
    where
      producer = sync (request (publishingAPI @resource) ws (readResource @resource) (ctx,nm))
      consumer = maybe "Not Found" (form onSubmit) 

      onSubmit resource = do
        did <- sync (request (publishingAPI @resource) ws (updateResource @resource) (ctx,nm,resource))
        case did of
          Just True -> Router.goto (toReadRoute ctx nm)
          _         -> pure ()

