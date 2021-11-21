module Pure.Conjurer.Index where

import Pure.Conjurer.Context
import Pure.Conjurer.Resource
import Pure.Conjurer.Rep

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Txt (FromTxt(..))
import Pure.Sorcerer as Sorcerer

import Data.Hashable

import Data.Foldable
import Data.Typeable
import GHC.Generics

data Index a = Index
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data IndexMsg a
  = ResourceAdded (Context a) (Name a)
  deriving stock Generic

deriving instance (ToJSON (Context a), ToJSON (Name a))
  => ToJSON (IndexMsg a)

deriving instance (FromJSON (Context a), FromJSON (Name a))
  => FromJSON (IndexMsg a)

instance 
  ( Typeable a
  , ToJSON (IndexMsg a), FromJSON (IndexMsg a)
  ) => Source (IndexMsg a) 
  where
    data Stream (IndexMsg a) = IndexStream 
      deriving stock Generic
      deriving anyclass (Hashable,ToJSON,FromJSON)
      
    stream IndexStream = 
      "conjurer/indexes/" 
        ++ fromTxt (rep @a)
        ++ "/index.stream"

instance 
  ( Typeable a
  , ToJSON (Name a), FromJSON (Name a)
  , ToJSON (Context a), FromJSON (Context a)
  ) => Aggregable (IndexMsg a) (Index a) 
  where
    update _ _ = Ignore
    aggregate = "index.aggregate"

iterate 
  :: forall a. 
    ( Typeable a
    , ToJSON (Name a), FromJSON (Name a)
    , ToJSON (Context a), FromJSON (Context a)
    ) => (Context a -> Name a -> IO ()) -> IO ()
iterate f = do
  rs <- Sorcerer.events (IndexStream :: Stream (IndexMsg a))
  for_ rs $ \(ResourceAdded ctx nm) -> do
    f ctx nm