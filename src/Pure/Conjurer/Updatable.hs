module Pure.Conjurer.Updatable (Updatable(..),Previewing,Updating,cachingToUpdate) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Formable
import Pure.Conjurer.Pathable
import Pure.Conjurer.Previewable
import Pure.Conjurer.Producible
import Pure.Conjurer.Resource
import Pure.Conjurer.Rootable
import Pure.Conjurer.Routable

import qualified Pure.Data.View
import Pure.Auth (Access(..),authorize,defaultOnRegistered)
import Pure.Data.JSON
import Pure.Elm.Component hiding (root,Update)
import Pure.Maybe
import Pure.Router as Router
import Pure.Sync
import Pure.WebSocket
import Pure.WebSocket.Cache

import Data.Typeable

data Updating
data Previewing
instance Theme Updating
instance Theme Previewing

class Updatable _role resource where
  toUpdate :: WebSocket -> Context resource -> Name resource -> View
  default toUpdate 
    :: ( Typeable resource, Typeable _role
       , Routable resource
       , Theme resource
       , ToJSON (Context resource), FromJSON (Context resource)
       , ToJSON (Name resource), FromJSON (Name resource)
       , ToJSON (Resource resource), FromJSON (Resource resource)
       , FromJSON (Preview resource)
       , FromJSON (Product resource)
       , Formable (Resource resource)
       , Pure (Preview resource)
       , Pure (Product resource)
       , Eq (Context resource)
       , Eq (Name resource)
       ) => WebSocket -> Context resource -> Name resource -> View
  toUpdate ws ctx nm =
    authorize @_role (Access ws id defaultOnRegistered) $ \_ ->
      producingKeyed (ctx,nm) producer (\_ -> consuming consumer)
    where
      producer = sync .
        request (publishingAPI @resource) ws 
          (readResource @resource) 

      consumer = maybe "Not Found" (\x -> Div <| Themed @resource . Themed @Updating |> [ form onSubmit onPreview x ]) 
      
      onPreview resource = do
        r <- sync do
          request (publishingAPI @resource) ws
            (previewResource @resource)
            (ctx,resource)
        case r of
          Nothing -> pure "Failed to preview."
          Just (ctx,nm,pre,pro,res) -> pure do
            Div <| Themed @resource . Themed @Previewing |>
              [ View pre
              , View pro
              ]

      onSubmit resource = do
        did <- sync do
          request (publishingAPI @resource) ws 
            (updateResource @resource) 
            (ctx,nm,resource)
            
        if did then
          Router.goto (toReadRoute ctx nm)
        else
          pure ()

cachingToUpdate 
  :: forall _role resource.
    ( Typeable resource, Typeable _role
    , Theme resource
    , Routable resource
    , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
    , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
    , ToJSON (Resource resource), FromJSON (Resource resource)
    , FromJSON (Preview resource)
    , FromJSON (Product resource)
    , Formable (Resource resource)
    , Pure (Preview resource)
    , Pure (Product resource)
    , Eq (Context resource)
    , Eq (Name resource)
    ) => WebSocket -> Context resource -> Name resource -> View
cachingToUpdate ws ctx nm =
  authorize @_role (Access ws id defaultOnRegistered) $ \_ ->
    producingKeyed (ctx,nm) producer (\_ -> consuming consumer)
  where
    producer = sync .
      request (publishingAPI @resource) ws 
        (readResource @resource) 

    consumer = maybe "Not Found" (\x -> Div <| Themed @resource . Themed @Updating |> [ form onSubmit onPreview x ]) 
    
    onPreview resource = do
      r <- sync do
        request (publishingAPI @resource) ws
          (previewResource @resource)
          (ctx,resource)
      case r of
        Nothing -> pure "Failed to preview."
        Just (ctx,nm,pre,pro,res) -> pure do
          Div <| Themed @resource . Themed @Previewing |>
            [ View pre
            , View pro
            ]

    onSubmit resource = do
      did <- sync do
        request (publishingAPI @resource) ws 
          (updateResource @resource) 
          (ctx,nm,resource) 
          
      if did then do
        req Fresh (readingAPI @resource)
          (readProduct @resource)
          (ctx,nm)
        Router.goto (toReadRoute ctx nm)
      else 
        pure ()

instance {-# INCOHERENT #-}
  ( Typeable _role
  , ToJSON (Context resource), FromJSON (Context resource), Pathable (Context resource), Eq (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource), Pathable (Name resource), Eq (Name resource)
  , ToJSON (Resource resource), FromJSON (Resource resource)
  , FromJSON (Preview resource)
  , FromJSON (Product resource)
  , Formable (Resource resource)
  , Component (Preview resource)
  , Component (Product resource)
  , Theme resource
  ) => Updatable _role resource