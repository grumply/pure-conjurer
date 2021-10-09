module Pure.Conjurer.Listing where

import Pure.Conjurer.Context
import Pure.Conjurer.Pathable
import Pure.Conjurer.Previewable
import Pure.Conjurer.Resource

import Pure.Data.JSON
import Pure.Data.Txt
import Pure.Sorcerer as Sorcerer

import Data.Hashable

import Data.List as List
import Data.Typeable
import GHC.Generics

-- TODO: pull this out of the default pages and make it optional.
--       This could be expensive when there are many resources
--       for a given context.

data Listing a = Listing
  { listing :: [(Name a,Preview a)] 
  } deriving stock Generic

deriving instance (ToJSON (Name a), ToJSON (Preview a))
  => ToJSON (Listing a)

deriving instance (FromJSON (Name a), FromJSON (Preview a))
  => FromJSON (Listing a)

data ListingMsg a
  = SetPreviewItem (Name a) (Preview a)
  | DeletePreviewItem (Name a)
  deriving stock Generic

deriving instance (ToJSON (Name a), ToJSON (Preview a)) 
  => ToJSON (ListingMsg a)

deriving instance (FromJSON (Name a), FromJSON (Preview a)) 
  => FromJSON (ListingMsg a)

instance 
  ( Typeable a
  , Pathable (Context a), Hashable (Context a)
  , ToJSON (Name a), FromJSON (Name a)
  , ToJSON (Preview a), FromJSON (Preview a)
  ) => Source (ListingMsg a) 
  where
    data Stream (ListingMsg a) = UserListingStream (Context a)
      deriving stock Generic

    stream (UserListingStream ctx) = 
      "conjurer/listings/" 
        ++ show (typeRepTyCon (typeOf (undefined :: a))) 
        ++ fromTxt (toPath ctx)
        ++ "/listing.stream"

deriving instance (Hashable (Context a))
  => Hashable (Stream (ListingMsg a))

-- Overlappable in case the need arises to, for instance, limit the length of a listing.
instance {-# OVERLAPPABLE #-} 
  ( Typeable a 
  , Eq (Name a), ToJSON (Name a), FromJSON (Name a)
  , Pathable (Context a), Hashable (Context a)
  , ToJSON (Preview a), FromJSON (Preview a)
  ) => Aggregable (ListingMsg a) (Listing a) 
  where
    update = defaultListingUpdate id 
    aggregate = "listing.aggregate"

defaultListingUpdate 
  :: Eq (Name x) 
  => (forall a. [a] -> [a]) 
  -> ListingMsg x -> Maybe (Listing x) -> Maybe (Maybe (Listing x))
defaultListingUpdate f = update
  where
    update (SetPreviewItem nm p) Nothing = 
      Sorcerer.Update Listing 
        { listing = f [(nm,p)] }

    update (SetPreviewItem nm p) (Just l) = 
      Sorcerer.Update Listing 
        { listing = f $ (nm,p) : List.filter (((/=) nm) . fst) (listing l) }

    update (DeletePreviewItem nm) (Just l) = 
      Sorcerer.Update l 
        { listing = List.filter ((/= nm) . fst) (listing l) }

    update _ _ = Ignore