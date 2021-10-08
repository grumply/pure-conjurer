module Pure.Conjurer.Permissions where

import Pure.Conjurer.Context
import Pure.Conjurer.Resource

data Permissions resource = Permissions
  { canCreate :: Context resource -> IO Bool
  , canUpdate :: Context resource -> Name resource -> IO Bool
  , canDelete :: Context resource -> Name resource -> IO Bool
  , canRead   :: Context resource -> Name resource -> IO Bool
  , canList   :: Context resource -> IO Bool
  }

unsafeDefaultPermissions :: Permissions resource
unsafeDefaultPermissions = Permissions
  { canCreate = \_   -> pure True
  , canUpdate = \_ _ -> pure True
  , canDelete = \_ _ -> pure True
  , canRead   = \_ _ -> pure True
  , canList   = \_   -> pure True
  }

