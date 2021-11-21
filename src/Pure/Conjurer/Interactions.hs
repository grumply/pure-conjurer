module Pure.Conjurer.Interactions where

import Pure.Conjurer.Context
import Pure.Conjurer.Name
import Pure.Conjurer.Rep
import Pure.Conjurer.Resource

import Pure.Auth (Username)
import Pure.Data.Txt (FromTxt(..))
import Pure.Elm.Component (Default(..))

import Data.Typeable

import Prelude

data family Action a
data family Reaction a

data Interactions a = Interactions
  { interact :: Context a -> Name a -> Resource a -> Action a -> IO (Reaction a)
  }

class Typeable a => DefaultInteractions a where
  interactions :: Maybe Username -> Interactions a
  interactions _ = 
    let 
      tc = fromTxt (rep @a)

      err = "Interactions from DefaultInteractions " <> tc 
         <> " => interact :: Context " <> tc
         <> " -> Name " <> tc
         <> " -> Resource " <> tc 
         <> " -> Action " <> tc 
         <> " -> IO (Reaction " <> tc <> "): Not implemented."
      
    in 
      Interactions (\_ _ _ _ -> error err)

instance {-# INCOHERENT #-} Typeable x => DefaultInteractions x