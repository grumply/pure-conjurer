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

data Updating a
data Previewing a
instance Theme Updating
instance {-# OVERLAPPABLE #-} Typeable a => Theme (Updating a)
instance Theme Previewing
instance {-# OVERLAPPABLE #-} Typeable a => Theme (Previewing a)

class Updatable _role resource where
  toUpdate :: WebSocket -> Context resource -> Name resource -> View
  default toUpdate 
    :: ( Typeable resource, Typeable _role
       , Routable resource
       , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
       , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
       , ToJSON (Resource resource), FromJSON (Resource resource)
       , FromJSON (Preview resource)
       , FromJSON (Product resource)
       , Formable (Resource resource)
       , Pure (Preview resource)
       , Pure (Product resource)
       , Theme (Updating resource)
       , Theme (Previewing resource)
       ) => WebSocket -> Context resource -> Name resource -> View
  toUpdate ws ctx nm =
    authorize @_role (Access ws id defaultOnRegistered) $ \_ ->
      producingKeyed (ctx,nm) producer (\_ -> consuming consumer)
    where
      producer = sync .
        request (publishingAPI @resource) ws 
          (readResource @resource) 

      consumer = maybe "Not Found" (\x -> Div <| Themed @Updating . Themed @(Updating resource) |> [ form onSubmit onPreview x ]) 
      
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
        did <- sync do
          request (publishingAPI @resource) ws 
            (updateResource @resource) 
            (ctx,nm,resource)
        flush @_role (readingAPI @resource) (readPreview @resource) (ctx,nm)
        flush @_role (readingAPI @resource) (readProduct @resource) (ctx,nm)
        if did then
          Router.goto (toReadRoute ctx nm)
        else
          pure ()

cachingToUpdate 
  :: forall _role resource.
    ( Typeable resource, Typeable _role
    , Routable resource
    , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
    , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
    , ToJSON (Resource resource), FromJSON (Resource resource)
    , FromJSON (Preview resource)
    , FromJSON (Product resource)
    , Formable (Resource resource)
    , Pure (Preview resource)
    , Pure (Product resource)
    , Theme (Updating resource)
    , Theme (Previewing resource)
    ) => WebSocket -> Context resource -> Name resource -> View
cachingToUpdate ws ctx nm =
  authorize @_role (Access ws id defaultOnRegistered) $ \_ ->
    producingKeyed (ctx,nm) producer (\_ -> consuming consumer)
  where
    producer = sync .
      request (publishingAPI @resource) ws 
        (readResource @resource) 

    consumer = maybe "Not Found" (\x -> Div <| Themed @Updating . Themed @(Updating resource) |> [ form onSubmit onPreview x ]) 
    
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
      did <- sync do
        request (publishingAPI @resource) ws 
          (updateResource @resource) 
          (ctx,nm,resource) 
          
      if did then do
        flush @_role (readingAPI @resource) (readPreview @resource) (ctx,nm)
        flush @_role (readingAPI @resource) (readProduct @resource) (ctx,nm)
        Router.goto (toReadRoute ctx nm)
      else 
        pure ()

instance {-# OVERLAPPABLE #-}
  ( Typeable _role
  , Typeable resource
  , ToJSON (Context resource), FromJSON (Context resource), Pathable (Context resource), Ord (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource), Pathable (Name resource), Ord (Name resource)
  , ToJSON (Resource resource), FromJSON (Resource resource)
  , FromJSON (Preview resource)
  , FromJSON (Product resource)
  , Formable (Resource resource)
  , Pure (Preview resource)
  , Pure (Product resource)
  , Theme (Updating resource)
  , Theme (Previewing resource)
  ) => Updatable _role resource