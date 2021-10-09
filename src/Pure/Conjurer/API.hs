module Pure.Conjurer.API where

import Pure.Conjurer.Context
import Pure.Conjurer.Previewable
import Pure.Conjurer.Producible
import Pure.Conjurer.Resource

import Pure.Data.JSON
import Pure.WebSocket as WS

import Data.Proxy
import Data.Typeable

data CreateResource resource
instance Identify (CreateResource resource)
instance 
  ( Typeable resource, ToJSON (Context resource), FromJSON (Context resource)
  ) => Request (CreateResource resource) 
  where
    type Req (CreateResource resource) = (Int,(Context resource,Resource resource))
    type Rsp (CreateResource resource) = Maybe (Name resource)

createResource :: Proxy (CreateResource resource)
createResource = Proxy

data ReadResource resource
instance Identify (ReadResource resource)
instance 
  ( Typeable resource, ToJSON (Context resource), FromJSON (Context resource)
  ) => Request (ReadResource resource) 
  where
    type Req (ReadResource resource) = (Int,(Context resource,Name resource))
    type Rsp (ReadResource resource) = Maybe (Resource resource)

readResource :: Proxy (ReadResource resource)
readResource = Proxy

data UpdateResource resource
instance Identify (UpdateResource resource)
instance 
  ( Typeable resource, ToJSON (Context resource), FromJSON (Context resource)
  ) => Request (UpdateResource resource) 
  where
    type Req (UpdateResource resource) = (Int,(Context resource,Name resource,Resource resource))
    type Rsp (UpdateResource resource) = Maybe Bool

updateResource :: Proxy (UpdateResource resource)
updateResource = Proxy

data DeleteResource resource
instance Identify (DeleteResource resource)
instance 
  ( Typeable resource, ToJSON (Context resource), FromJSON (Context resource)
  ) => Request (DeleteResource resource) 
  where
    type Req (DeleteResource resource) = (Int,(Context resource,Name resource))
    type Rsp (DeleteResource resource) = Maybe Bool

deleteResource :: Proxy (DeleteResource resource)
deleteResource = Proxy

data ReadProduct resource
instance Identify (ReadProduct resource)
instance 
  ( Typeable resource, ToJSON (Context resource), FromJSON (Context resource)
  ) => Request (ReadProduct resource) 
  where
    type Req (ReadProduct resource) = (Int,(Context resource,Name resource))
    type Rsp (ReadProduct resource) = Maybe (Product resource)

readProduct :: Proxy (ReadProduct resource)
readProduct = Proxy

data ReadPreview resource
instance Identify (ReadPreview resource)
instance 
  ( Typeable resource, ToJSON (Context resource), FromJSON (Context resource)
  ) => Request (ReadPreview resource) 
  where
    type Req (ReadPreview resource) = (Int,(Context resource,Name resource))
    type Rsp (ReadPreview resource) = Maybe (Preview resource)

readPreview :: Proxy (ReadPreview resource)
readPreview = Proxy

data ReadListing resource
instance Identify (ReadListing resource)
instance 
  ( Typeable resource, ToJSON (Context resource), FromJSON (Context resource)
  ) => Request (ReadListing resource) 
  where
    type Req (ReadListing resource) = (Int,Context resource)
    type Rsp (ReadListing resource) = Maybe [(Name resource,Preview resource)]

readListing :: Proxy (ReadListing resource)
readListing = Proxy

type PublishingAPI resource = 
  '[ CreateResource resource
   , ReadResource resource
   , UpdateResource resource
   , DeleteResource resource
   ]

type ReadingAPI resource =
  '[ ReadProduct resource
   , ReadPreview resource
   , ReadListing resource
   ]

publishingAPI 
  :: forall resource. 
    ( Typeable resource
    , ToJSON (Context resource), FromJSON (Context resource)
    ) => API '[] (PublishingAPI resource)
publishingAPI = api msgs reqs
  where
    msgs = WS.none
    reqs = createResource @resource
       <:> readResource @resource
       <:> updateResource @resource
       <:> deleteResource @resource
       <:> WS.none

readingAPI 
  :: forall resource. 
    ( Typeable resource
    , ToJSON (Context resource), FromJSON (Context resource)
    ) => API '[] (ReadingAPI resource)
readingAPI = api msgs reqs
  where
    msgs = WS.none
    reqs = readProduct @resource
       <:> readPreview @resource
       <:> readListing @resource
       <:> WS.none
