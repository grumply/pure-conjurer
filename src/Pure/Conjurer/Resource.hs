module Pure.Conjurer.Resource (Stream(..),Resource(..),ResourceMsg(..),Name(..),Nameable(..),Amendable(..)) where

import Pure.Conjurer.Context
import Pure.Conjurer.Name
import Pure.Conjurer.Pathable

import Pure.Data.JSON
import Pure.Data.Txt
import Pure.Sorcerer as Sorcerer

import Data.Hashable

import Data.Typeable
import GHC.Generics

data family Resource a :: *

class Amendable a where
  data Amend a :: *
  amend :: Amend a -> Resource a -> Resource a
  amend _ = id

class Nameable a where
  toName :: Resource a -> Name a

data ResourceMsg a
  = SetResource (Resource a)
  | AmendResource (Amend a)
  | DeleteResource
  deriving stock Generic

deriving instance (ToJSON (Amend a), ToJSON (Resource a))
  => ToJSON (ResourceMsg a)

deriving instance (FromJSON (Amend a), FromJSON (Resource a))
  => FromJSON (ResourceMsg a)

instance 
  ( Typeable a
  , ToJSON (ResourceMsg a), FromJSON (ResourceMsg a)
  , Hashable (Context a), Pathable (Context a)
  , Hashable (Name a), Pathable (Name a)
  ) => Source (ResourceMsg a) 
  where
    data Stream (ResourceMsg a) = ResourceStream (Context a) (Name a)
      deriving stock Generic
      
    stream (ResourceStream ctx nm) = 
      "conjurer/resources/" 
        ++ show (typeRepTyCon (typeOf (undefined :: a))) 
        ++ fromTxt (toPath ctx)
        ++ fromTxt (toPath nm)
        ++ ".stream"

deriving instance (Hashable (Context a), Hashable (Name a)) 
  => Hashable (Stream (ResourceMsg a))
  
instance 
  ( Typeable a
  , Amendable a
  , FromJSON (Amend a), ToJSON (Amend a)
  , Hashable (Context a), Pathable (Context a)
  , Hashable (Name a), Pathable (Name a)
  , FromJSON (Resource a), ToJSON (Resource a)
  ) => Aggregable (ResourceMsg a) (Resource a)
  where
    update (SetResource r) _ = Sorcerer.Update r
    update (AmendResource c) (Just r) = Sorcerer.Update (amend c r)
    update DeleteResource (Just _) = Delete
    update _ _ = Ignore

    aggregate = "resource.aggregate"