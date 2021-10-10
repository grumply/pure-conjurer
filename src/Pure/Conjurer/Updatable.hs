module Pure.Conjurer.Updatable (Updatable(..),cachingToUpdate) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Formable
import Pure.Conjurer.Pathable
import Pure.Conjurer.Previewable
import Pure.Conjurer.Producible
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
import Pure.WebSocket.Cache

import Data.Typeable

class Updatable _role resource | resource -> _role where
  updateRoute :: (Context resource -> Name resource -> rt) -> Routing rt ()
  default updateRoute 
    :: ( Rootable resource, Pathable (Context resource), Pathable (Name resource)
       ) => (Context resource -> Name resource -> rt) -> Routing rt ()
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
  default toUpdateRoute 
    :: ( Rootable resource, Pathable (Context resource), Pathable (Name resource)
       ) => Context resource -> Name resource -> Txt
  toUpdateRoute ctx nm = root @resource <> "/update" <> toPath ctx <> toPath nm 

  toUpdate :: WebSocket -> Context resource -> Name resource -> View
  default toUpdate 
    :: ( Typeable resource, Typeable _role
       , ToJSON (Context resource), FromJSON (Context resource)
       , ToJSON (Name resource), FromJSON (Name resource)
       , ToJSON (Resource resource), FromJSON (Resource resource)
       , FromJSON (Preview resource)
       , FromJSON (Product resource)
       , Readable resource
       , Formable (Resource resource)
       , Component (Preview resource)
       , Component (Product resource)
       ) => WebSocket -> Context resource -> Name resource -> View
  toUpdate ws ctx nm =
    authorize @_role $ maybe "Not Authorized" $ \_ ->
      producing producer (consuming consumer)
    where
      producer = sync do 
        request (publishingAPI @resource) ws 
          (readResource @resource) 
          (ctx,nm)

      consumer = maybe "Not Found" (form onSubmit onPreview) 
      
      onPreview resource = do
        r <- sync do
          request (publishingAPI @resource) ws
            (previewResource @resource)
            (ctx,resource)
        case r of
          Nothing -> pure "Failed to preview."
          Just (ctx,nm,pre,pro,res) -> pure do
            Div <||>
              [ run pre
              , run pro
              ]

      onSubmit resource = do
        did <- sync do
          request (publishingAPI @resource) ws 
            (updateResource @resource) 
            (ctx,nm,resource)
            
        case did of
          Just True -> Router.goto (toReadRoute ctx nm)
          _         -> pure ()

cachingToUpdate 
  :: forall _role resource.
    ( Typeable resource, Typeable _role
    , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
    , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
    , ToJSON (Resource resource), FromJSON (Resource resource)
    , FromJSON (Preview resource)
    , FromJSON (Product resource)
    , Readable resource
    , Formable (Resource resource)
    , Component (Preview resource)
    , Component (Product resource)
    ) => WebSocket -> Context resource -> Name resource -> View
cachingToUpdate ws ctx nm =
  authorize @_role $ maybe "Not Authorized" $ \_ ->
    producing producer (consuming consumer)
  where
    producer = sync do 
      request (publishingAPI @resource) ws 
        (readResource @resource) 
        (ctx,nm)

    consumer = maybe "Not Found" (form onSubmit onPreview) 
    
    onPreview resource = do
      r <- sync do
        request (publishingAPI @resource) ws
          (previewResource @resource)
          (ctx,resource)
      case r of
        Nothing -> pure "Failed to preview."
        Just (ctx,nm,pre,pro,res) -> pure do
          Div <||>
            [ run pre
            , run pro
            ]

    onSubmit resource = do
      did <- sync do
        request (publishingAPI @resource) ws 
          (updateResource @resource) 
          (ctx,nm,resource) 
          
      case did of
        Just True -> do
          req Fresh (readingAPI @resource)
            (readProduct @resource)
            (ctx,nm)
          Router.goto (toReadRoute ctx nm)

        _ -> 
          pure ()

