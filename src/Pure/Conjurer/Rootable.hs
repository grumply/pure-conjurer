module Pure.Conjurer.Rootable where

import Pure.Data.Txt as Txt

import Data.Typeable

class Rootable a where
  root :: Txt
  default root :: Typeable a => Txt
  root = "/" <> Txt.toLower (toTxt (show (typeRepTyCon (typeOf (undefined :: a)))))

instance {-# OVERLAPPABLE #-} Typeable a => Rootable a