module Pure.Conjurer.Readable (Readable(..),Reading,cachingToRead) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Pathable
import Pure.Conjurer.Producible
import Pure.Conjurer.Resource
import Pure.Conjurer.Rootable

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
       , Component (Product resource)
       , FromJSON (Context resource), ToJSON (Context resource)
       , FromJSON (Name resource), ToJSON (Name resource)
       , FromJSON (Product resource)
       , Eq (Context resource)
       , Eq (Name resource)
       ) => WebSocket -> Context resource -> Name resource -> View
  toRead ws ctx nm = producingKeyed (ctx,nm) producer (\_ -> consuming consumer)
    where
      producer = sync .
        request (readingAPI @resource) ws 
          (readProduct @resource) 

      consumer = maybe "Not Found" (\x -> Div <| Themed @resource . Themed @Reading |> [ run x ])

cachingToRead 
  :: forall resource.
    ( Typeable resource
    , Theme resource
    , Component (Product resource)
    , FromJSON (Context resource), ToJSON (Context resource), Ord (Context resource)
    , FromJSON (Name resource), ToJSON (Name resource), Ord (Name resource)
    , FromJSON (Product resource)
    , Eq (Context resource)
    , Eq (Name resource)
    ) 
  => WebSocket -> Context resource -> Name resource -> View
cachingToRead _ ctx nm = producingKeyed (ctx,nm) producer (\_ -> consuming consumer)
  where
    producer =
      req Cached (readingAPI @resource)
        (readProduct @resource) 

    consumer = 
      maybe "Not Found" (\x -> Div <| Themed @resource . Themed @Reading |> [ run x ])

instance {-# INCOHERENT #-}
  ( Theme resource
  , ToJSON (Context resource), FromJSON (Context resource), Eq (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource), Eq (Name resource)
  , FromJSON (Product resource)
  , Component (Product resource)
  ) => Readable resource