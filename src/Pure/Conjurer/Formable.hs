module Pure.Conjurer.Formable (Formable(..)) where

import Pure.Conjurer.Fieldable
import Pure.Elm.Component
import Pure.Hooks

import Data.Typeable
import GHC.Generics as G
import GHC.TypeLits
import Unsafe.Coerce

class Formable rec where
  form :: (rec -> IO ()) -> rec -> View
  default form :: (Generic rec, GFormable (Rep rec)) => (rec -> IO ()) -> rec -> View
  form f = gform (f . G.to) . G.from

class GFormable f where
  gform :: (forall x. f x -> IO ()) -> f x -> View

instance GFormable x => GFormable (M1 D m x) where
  gform f (M1 x) = gform (f . M1) x

data FormableState a = FS !(forall x. (a x))

instance 
  ( KnownSymbol name, Typeable x, GFormable x
  ) => GFormable (M1 C (MetaCons name _fix True) x) 
  where
    gform f (M1 x) = 
      useState (FS (unsafeCoerce x :: x y)) $ \State {..} ->
        let FS x = state
        in Div <| Class (toTxt (symbolVal @name Proxy)) |>
            [ H2 <||> [ txt (symbolVal @name Proxy) ]
            , gform (\r -> modify (\_ -> FS (unsafeCoerce r))) x
            , Button <| OnClick (\_ -> f (unsafeCoerce x)) |> [ "Submit" ]
            ]

instance 
  ( Typeable x, GFormable x
  ) => GFormable (M1 C (MetaCons name _fix False) x) 
  where
    gform f (M1 x) = gform (f . M1) x

-- Without the strictness annotations, we end up unsafely observing 
-- thunked laziness as a result of the coercions!
data FormableProductState a b = FPS !(forall x. a x) !(forall x. b x) 

-- Sadly, this instance induces nesting of sequential record fields.
instance 
  ( Typeable a, Typeable b
  , GFormable a, GFormable b
  ) => GFormable ((:*:) a b) 
  where
    gform f (a :*: b) =
      useState (FPS (unsafeCoerce a :: a x) (unsafeCoerce b :: b x)) $ \State {..} -> 
        let FPS l r = state 
        in Div <||>
            [ gform (\l -> f (unsafeCoerce l :*: unsafeCoerce r) >> modify (\(FPS _ r) -> FPS (unsafeCoerce l) r)) a
            , gform (\r -> f (unsafeCoerce l :*: unsafeCoerce r) >> modify (\(FPS l _) -> FPS l (unsafeCoerce r))) b
            ]

instance 
  ( GFieldable (M1 S (MetaSel (Just name) _u _s _s') x)
  , KnownSymbol name
  ) => GFormable (M1 S (MetaSel (Just name) _u _s _s') x) 
  where
    gform f x = 
      Div <| Class (toTxt (symbolVal @name Proxy)) |>
        [ Label <||> [ txt (symbolVal @name Proxy) ]
        , gfield f x
        ]

instance GFormable x => GFormable (M1 S (MetaSel Nothing _u _s _s') x) where
  gform f (M1 x) = gform (f . M1) x

instance Formable x => GFormable (K1 r x) where
  gform f (K1 x) = form (f . K1) x


