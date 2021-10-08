module Pure.Conjurer.Slug (Slug()) where

import Pure.Conjurer.Pathable

import Pure.Data.JSON
import Pure.Data.Txt as Txt
import Pure.Router

import Data.Hashable

import Data.Char

newtype Slug x = Slug Txt
  deriving (Eq,Ord,ToJSON,FromJSON,Hashable) via Txt
type role Slug nominal

instance ToTxt (Slug x) where
  toTxt (Slug x) = x

instance FromTxt (Slug x) where
  fromTxt = toSlug

instance Pathable (Slug a) where
  toPath (Slug s) = "/" <> s
  fromPath = path' "/:slug" "/slug"

-- Idempotent.
--
-- prop> \(x :: String) -> toSlug (toTxt (toSlug (toTxt x))) == toSlug (toTxt x)
-- 
toSlug :: ToTxt a => a -> Slug b
toSlug = Slug . Txt.intercalate "-" . Txt.words . Txt.toLower . Txt.map f . Txt.replace "'" "" . toTxt
    where f c | isAlphaNum c = c | otherwise = ' '

 