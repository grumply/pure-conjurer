module Pure.Conjurer.Permissions where

import Pure.Auth (Username)
import Pure.Conjurer.Context
import Pure.Conjurer.Resource

class Ownable resource where
  isOwner :: Username -> Context resource -> Name resource -> IO Bool

data Permissions resource = Permissions
  { canCreate :: Context resource -> Name resource -> IO Bool
  , canUpdate :: Context resource -> Name resource -> IO Bool
  , canDelete :: Context resource -> Name resource -> IO Bool
  , canRead   :: Context resource -> Name resource -> IO Bool
  , canList   :: Context resource -> IO Bool
  }

fullPermissions :: Permissions resource
fullPermissions = Permissions
  { canCreate = \_ _ -> pure True
  , canUpdate = \_ _ -> pure True
  , canDelete = \_ _ -> pure True
  , canRead   = \_ _ -> pure True
  , canList   = \_   -> pure True
  }

readPermissions :: Permissions resource
readPermissions = Permissions
  { canCreate = \_ _ -> pure False
  , canUpdate = \_ _ -> pure False
  , canDelete = \_ _ -> pure False
  , canRead   = \_ _ -> pure True
  , canList   = \_   -> pure True
  }

defaultPermissions :: Ownable resource => Maybe Username -> Permissions resource
defaultPermissions = \case
  Nothing -> readPermissions
  Just un -> readPermissions
    { canCreate = isOwner un
    , canUpdate = isOwner un
    , canDelete = isOwner un
    }