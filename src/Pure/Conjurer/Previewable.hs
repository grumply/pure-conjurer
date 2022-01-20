module Pure.Conjurer.Previewable (Streamable(..),Stream(..),Preview(..),PreviewMsg(..),Previewable(..)) where

import Pure.Conjurer.Context
import Pure.Conjurer.Name
import Pure.Conjurer.Pathable
import Pure.Conjurer.Producible
import Pure.Conjurer.Rep
import Pure.Conjurer.Resource

import Pure.Data.JSON
import Pure.Data.Txt (FromTxt(..))
import Pure.Sorcerer as Sorcerer
import Pure.Theme

import Data.Hashable

import Data.Typeable
import GHC.Generics

import Prelude

data family Preview a :: *

instance Theme Preview

type Previewing = Bool
class Previewable a where
  preview :: Previewing -> Context a -> Name a -> Resource a -> Product a -> IO (Preview a)
  default preview :: Typeable a => Previewing -> Context a -> Name a -> Resource a -> Product a -> IO (Preview a)
  preview _ _ _ _ _ =
    let 
      tc = fromTxt (rep @a)
      err = "Previewable " <> tc 
         <> " => preview :: Previewing -> Context " <> tc
         <> " -> Name " <> tc
         <> " -> Resource " <> tc 
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
  ) => Streamable (PreviewMsg a) 
  where
    data Stream (PreviewMsg a) = PreviewStream (Context a) (Name a)
      deriving stock Generic

    stream (PreviewStream ctx nm) = 
      "conjurer/preview/" 
        ++ fromTxt (rep @a)
        ++ fromTxt (toPath ctx)
        ++ fromTxt (toPath nm)
        ++ ".stream"

deriving instance (Eq (Context a), Eq (Name a)) 
  => Eq (Stream (PreviewMsg a))
deriving instance (Ord (Context a), Ord (Name a)) 
  => Ord (Stream (PreviewMsg a))
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