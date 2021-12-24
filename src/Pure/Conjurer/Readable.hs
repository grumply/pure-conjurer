module Pure.Conjurer.Readable (Readable(..),Reading,toReadWith,cachingToReadWith,cachingToRead) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Pathable
import Pure.Conjurer.Producible
import Pure.Conjurer.Resource
import Pure.Conjurer.Rootable

import qualified Pure.Data.View
import Pure.Data.JSON
import Pure.Elm.Component hiding (root)
import Pure.Maybe
import Pure.Router as Router
import Pure.Sync
import Pure.WebSocket
import Pure.WebSocket.Cache

import Control.Concurrent
import Data.Typeable

import Prelude hiding (Read)

data Reading
instance Theme Reading

class Readable resource where
  toRead :: WebSocket -> Context resource -> Name resource -> View
  default toRead 
    :: ( Typeable resource
       , Theme resource
       , Pure (Product resource)
       , FromJSON (Context resource), ToJSON (Context resource)
       , FromJSON (Name resource), ToJSON (Name resource)
       , FromJSON (Product resource)
       , Eq (Context resource)
       , Eq (Name resource)
       ) => WebSocket -> Context resource -> Name resource -> View
  toRead = 
    toReadWith $ \_ _ ->
      maybe "Not Found" (\x -> Div <| Themed @resource . Themed @Reading |> [ View x ])

toReadWith
  :: forall resource.
    ( Typeable resource
     , FromJSON (Context resource), ToJSON (Context resource)
     , FromJSON (Name resource), ToJSON (Name resource)
     , FromJSON (Product resource)
     , Eq (Context resource)
     , Eq (Name resource)
     ) => (Context resource -> Name resource -> Maybe (Product resource) -> View) -> WebSocket -> Context resource -> Name resource -> View
toReadWith f ws ctx nm = producingKeyed (ctx,nm) producer (consuming . consumer)
  where
    producer = sync .
      request (readingAPI @resource) ws 
        (readProduct @resource) 

    consumer (ctx,nm) = f ctx nm 

cachingToReadWith
  :: forall _role resource.
    ( Typeable _role
    , Typeable resource
    , FromJSON (Context resource), ToJSON (Context resource), Ord (Context resource)
    , FromJSON (Name resource), ToJSON (Name resource), Ord (Name resource)
    , FromJSON (Product resource)
    , Eq (Context resource)
    , Eq (Name resource)
    ) 
  => (Context resource -> Name resource -> Maybe (Product resource) -> View) -> WebSocket -> Context resource -> Name resource -> View
cachingToReadWith f _ ctx nm = producingKeyed (ctx,nm) producer (consuming . consumer)
  where
    producer =
      req @_role Cached (readingAPI @resource)
        (readProduct @resource) 

    consumer (ctx,nm) = f ctx nm

cachingToRead 
  :: forall _role resource.
    ( Typeable _role
    , Typeable resource
    , Theme resource
    , Pure (Product resource)
    , FromJSON (Context resource), ToJSON (Context resource), Ord (Context resource)
    , FromJSON (Name resource), ToJSON (Name resource), Ord (Name resource)
    , FromJSON (Product resource)
    , Eq (Context resource)
    , Eq (Name resource)
    ) 
  => WebSocket -> Context resource -> Name resource -> View
cachingToRead = 
  cachingToReadWith @_role $ \_ _ ->
    maybe "Not Found" (\x -> Div <| Themed @resource . Themed @Reading |> [ View x ])

instance {-# INCOHERENT #-}
  ( Theme resource
  , ToJSON (Context resource), FromJSON (Context resource), Eq (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource), Eq (Name resource)
  , FromJSON (Product resource)
  , Pure (Product resource)
  ) => Readable resource