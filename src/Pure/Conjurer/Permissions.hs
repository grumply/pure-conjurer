module Pure.Conjurer.Permissions where

import Pure.Auth (Username)
import Pure.Conjurer.Context
import Pure.Conjurer.Resource
import Pure.Conjurer.Interactions

class Ownable resource where
  isOwner :: Username -> Context resource -> Name resource -> IO Bool

data Permissions resource = Permissions
  { canCreate   :: Context resource -> Name resource -> Resource resource -> IO Bool
  , canUpdate   :: Context resource -> Name resource -> IO Bool
  , canAmend    :: Context resource -> Name resource -> Amend resource -> IO Bool
  , canInteract :: Context resource -> Name resource -> Action resource -> IO Bool
  , canDelete   :: Context resource -> Name resource -> IO Bool
  , canRead     :: Context resource -> Name resource -> IO Bool
  , canList     :: Context resource -> IO Bool
  }

fullPermissions :: Permissions resource
fullPermissions = Permissions
  { canCreate   = \_ _ _ -> pure True
  , canUpdate   = \_ _ -> pure True
  , canAmend    = \_ _ _ -> pure True
  , canInteract = \_ _ _ -> pure True
  , canDelete   = \_ _ -> pure True
  , canRead     = \_ _ -> pure True
  , canList     = \_   -> pure True
  }

readPermissions :: Permissions resource
readPermissions = Permissions
  { canCreate   = \_ _ _ -> pure False
  , canUpdate   = \_ _ -> pure False
  , canAmend    = \_ _ _ -> pure False
  , canInteract = \_ _ _ -> pure False
  , canDelete   = \_ _ -> pure False
  , canRead     = \_ _ -> pure True
  , canList     = \_   -> pure True
  }

defaultPermissions :: Ownable resource => Maybe Username -> Permissions resource
defaultPermissions = \case
  Nothing -> readPermissions
  Just un -> readPermissions
    { canCreate   = \ctx nm _ -> isOwner un ctx nm
    , canUpdate   = isOwner un
    , canAmend    = \ctx nm _ -> isOwner un ctx nm
    , canInteract = \ctx nm _ -> isOwner un ctx nm
    , canDelete   = isOwner un
    }