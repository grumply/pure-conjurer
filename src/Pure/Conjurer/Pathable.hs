module Pure.Conjurer.Pathable where

import Pure.Data.Txt
import Pure.Data.Marker
import Pure.Router

import Data.Typeable
import GHC.Generics as G
import GHC.TypeLits

class Pathable a where
  toPath :: a -> Txt
  default toPath :: (Generic a, GPathable (Rep a)) => a -> Txt
  toPath = gtoPath . G.from

  fromPath :: Routing x (Maybe a)
  default fromPath :: (Generic a, GPathable (Rep a)) => Routing x (Maybe a)
  fromPath = fmap (fmap (G.to :: Rep a x -> a)) gfromPath

instance Pathable Marker where
  toPath m = "/" <> toTxt m
  fromPath = path' "/:marker" "marker"

instance (TypeError (Text "Txt is not safely Pathable; use Slug.")) => Pathable Txt where
  toPath = error "unreachable"
  fromPath = error "unreachable"

instance (TypeError (Text "String is not safely Pathable; use Slug.")) => Pathable String where
  toPath = error "unreachable"
  fromPath = error "unreachable"

instance Pathable () where
  toPath _ = ""
  fromPath = pure (Just ())

class GPathable f where
  gtoPath :: f a -> Txt
  gfromPath :: Routing x (Maybe (f a))

instance GPathable V1 where
  gtoPath _ = ""
  gfromPath = pure (Just (error "GPathable V1 => gfromPath: tried to materialize a void type."))

instance GPathable U1 where
  gtoPath _ = ""
  gfromPath = pure (Just U1)

instance GPathable x => GPathable (M1 r m x) where
  gtoPath (M1 x) = gtoPath x
  gfromPath = fmap (fmap M1) gfromPath

instance Pathable x => GPathable (K1 r x) where
  gtoPath (K1 x) = toPath x
  gfromPath = fmap (fmap K1) fromPath

instance (Typeable a, Typeable b, GPathable a, GPathable b) => GPathable ((:*:) a b) where
  gtoPath (a :*: b) = gtoPath a <> gtoPath b
  gfromPath = do
    ma <- gfromPath 
    mb <- gfromPath
    pure ((:*:) <$> ma <*> mb)

