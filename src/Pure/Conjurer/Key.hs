module Pure.Conjurer.Key (Key(),newKey) where

import Pure.Conjurer.Fieldable
import Pure.Conjurer.Pathable

import Pure.Data.Default
import Pure.Data.JSON hiding (Key)
import Pure.Data.Marker
import Pure.Data.Txt
import Pure.Elm.Component as Pure (pattern Null)

import Data.Hashable

import System.IO.Unsafe
import GHC.Generics

newtype Key a = Key Marker
  deriving stock Generic
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Eq,Ord,Hashable) via Marker
type role Key nominal

instance Fieldable (Key a) where
  field _ _ = Pure.Null

instance Default (Key a) where
  {-# NOINLINE def #-}
  def = Key (unsafePerformIO markIO)

instance Pathable (Key a) where
  toPath (Key m) = toPath m
  fromPath = fmap (fmap Key) fromPath

newKey :: IO (Key b)
newKey = Key <$> markIO