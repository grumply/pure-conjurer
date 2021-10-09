module Pure.Conjurer.Previewable (Stream(..),Preview(..),PreviewMsg(..),Previewable(..)) where

import Pure.Conjurer.Context
import Pure.Conjurer.Pathable
import Pure.Conjurer.Producible
import Pure.Conjurer.Resource

import Pure.Data.JSON
import Pure.Data.Txt
import Pure.Sorcerer as Sorcerer

import Data.Hashable

import Data.Typeable
import GHC.Generics

data family Preview a :: *

class Previewable a where
  preview :: Resource a -> Product a -> IO (Preview a)
  default preview :: Typeable a => Resource a -> Product a -> IO (Preview a)
  preview _ _ =
    let 
      tc = show (typeRepTyCon (typeOf (undefined :: a)))
      err = "Previewable " <> tc 
         <> " => preview :: Resource " <> tc 
         <> " -> Product " <> tc 
         <> " -> IO (Preview " <> tc <> "): Not implemented."
    in 
      pure (error err)

data PreviewMsg a
  = SetPreview (Preview a)
  | DeletePreview
  deriving stock Generic
deriving instance ToJSON (Preview a) => ToJSON (PreviewMsg a)
deriving instance FromJSON (Preview a) => FromJSON (PreviewMsg a)

instance 
  ( Typeable a
  , ToJSON (PreviewMsg a), FromJSON (PreviewMsg a)
  , Hashable (Context a), Pathable (Context a)
  , Hashable (Name a), Pathable (Name a)
  ) => Source (PreviewMsg a) 
  where
    data Stream (PreviewMsg a) = PreviewStream (Context a) (Name a)
      deriving stock Generic

    stream (PreviewStream ctx nm) = 
      "conjurer/previews/" 
        ++ show (typeRepTyCon (typeOf (undefined :: a))) 
        ++ fromTxt (toPath ctx)
        ++ fromTxt (toPath nm)
        ++ ".stream"

deriving instance (Hashable (Context a), Hashable (Name a)) 
  => Hashable (Stream (PreviewMsg a))

instance 
  ( Typeable a
  , Hashable (Context a), Pathable (Context a)
  , Hashable (Name a), Pathable (Name a)
  , FromJSON (Preview a), ToJSON (Preview a)
  ) => Aggregable (PreviewMsg a) (Preview a) 
  where
    update (SetPreview p) _ = Sorcerer.Update p
    update DeletePreview (Just _) = Delete
    update _ _ = Ignore

    aggregate = "preview.aggregate"