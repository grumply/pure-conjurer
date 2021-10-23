module Pure.Conjurer.Producible (Stream(..),Product(..),ProductMsg(..),Producible(..)) where

import Pure.Conjurer.Context
import Pure.Conjurer.Pathable
import Pure.Conjurer.Resource

import Pure.Data.JSON
import Pure.Data.Txt
import Pure.Sorcerer as Sorcerer

import Data.Hashable

import Data.Typeable
import GHC.Generics

data family Product a :: *

class Producible a where
  -- | WARNING: `produce` is run during preview!
  produce :: Resource a -> IO (Product a)
  default produce :: Typeable a => Resource a -> IO (Product a)
  produce _ = 
    let tc = show (typeRepTyCon (typeOf (undefined :: a)))
    in pure (error $ "Producible " <> tc <> " => produce :: Resource " <> tc <> " -> IO (Product " <> tc <> "): Not implemented.")

data ProductMsg a
  = SetProduct (Product a)
  | DeleteProduct
  deriving stock Generic

deriving instance ToJSON (Product a) 
  => ToJSON (ProductMsg a)

deriving instance FromJSON (Product a) 
  => FromJSON (ProductMsg a)

instance 
  ( Typeable a
  , ToJSON (ProductMsg a), FromJSON (ProductMsg a)
  , Hashable (Context a), Pathable (Context a)
  , Hashable (Name a), Pathable (Name a)
  ) => Source (ProductMsg a) 
  where
    data Stream (ProductMsg a) = ProductStream (Context a) (Name a)
      deriving stock Generic

    stream (ProductStream ctx nm) = 
      "conjurer/products/" 
        ++ show (typeRepTyCon (typeOf (undefined :: a))) 
        ++ fromTxt (toPath ctx)
        ++ fromTxt (toPath nm)
        ++ ".stream"

deriving instance (Hashable (Context a), Hashable (Name a)) 
  => Hashable (Stream (ProductMsg a))
  
instance 
  ( Typeable a
  , Hashable (Context a), Pathable (Context a)
  , Hashable (Name a), Pathable (Name a)
  , FromJSON (Product a), ToJSON (Product a)
  ) => Aggregable (ProductMsg a) (Product a) 
  where
    update (SetProduct p) _ = Sorcerer.Update p
    update DeleteProduct (Just _) = Delete
    update _ _ = Ignore

    aggregate = "product.aggregate"