module Pure.Conjurer.Interaction where

import Pure.Conjurer.Context
import Pure.Conjurer.Name
import Pure.Conjurer.Resource

import Pure.Elm.Component (Default(..))

import Data.Typeable

data family Action a
data family Reaction a

data Interaction a = Interaction
  { interact :: Context a -> Name a -> Resource a -> Action a -> IO (Reaction a)
  }

instance Typeable a => Default (Interaction a) where
  def = 
    let tc = show (typeRepTyCon (typeOf (undefined :: a)))
    in Interaction (\_ _ _ _ -> error $ "Interaction (" <> tc <> "): Not implemented.")