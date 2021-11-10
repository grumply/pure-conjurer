module Pure.Conjurer.Interactions where

import Pure.Conjurer.Context
import Pure.Conjurer.Name
import Pure.Conjurer.Resource

import Pure.Elm.Component (Default(..))

import Data.Typeable

data family Action a
data family Reaction a

data Interactions a = Interactions
  { interact :: Context a -> Name a -> Resource a -> Action a -> IO (Reaction a)
  }

instance Typeable a => Default (Interactions a) where
  def = 
    let tc = show (typeRepTyCon (typeOf (undefined :: a)))
    in Interactions (\_ _ _ _ -> error $ "Interactions (" <> tc <> "): Not implemented.")