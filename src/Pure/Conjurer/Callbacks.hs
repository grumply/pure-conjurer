module Pure.Conjurer.Callbacks where

import Pure.Conjurer.Context
import Pure.Conjurer.Interaction
import Pure.Conjurer.Previewable
import Pure.Conjurer.Producible
import Pure.Conjurer.Resource

import Pure.Data.Default

data Callbacks resource = Callbacks
  { onCreate   :: Context resource -> Name resource -> Resource resource -> Product resource -> Preview resource -> IO ()
  , onUpdate   :: Context resource -> Name resource -> Resource resource -> Product resource -> Preview resource -> IO ()
  , onDelete   :: Context resource -> Name resource -> Resource resource -> Product resource -> Preview resource -> IO ()
  , onAmend    :: Context resource -> Name resource -> Resource resource -> Product resource -> Preview resource -> Amend resource -> IO ()
  , onInteract :: Context resource -> Name resource -> Resource resource -> Action resource -> Reaction resource -> IO ()
  , onResource :: Context resource -> Name resource -> Resource resource -> IO ()
  , onRead     :: Context resource -> Name resource -> Product resource  -> IO ()
  , onPreview  :: Context resource -> Name resource -> Preview resource -> IO ()
  , onList     :: Context resource -> [(Name resource,Preview resource)] -> IO ()
  }

instance Default (Callbacks resource) where
  def = Callbacks
    { onCreate   = def
    , onUpdate   = def
    , onDelete   = def
    , onAmend    = def
    , onInteract = def
    , onResource = def
    , onRead     = def
    , onPreview  = def
    , onList     = def
    }