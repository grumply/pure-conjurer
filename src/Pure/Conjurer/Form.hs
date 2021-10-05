module Pure.Conjurer.Form 
  ( Form(..)
  ) where

import Pure.Conjurer.Field
import Pure.Elm.Component
import Pure.Hooks

import Data.Typeable
import GHC.Generics as G
import GHC.TypeLits
import Unsafe.Coerce

class Form rec where
  form :: (rec -> IO ()) -> rec -> View
  default form :: (Generic rec, GForm (Rep rec)) => (rec -> IO ()) -> rec -> View
  form f = gform (f . G.to) . G.from

class GForm f where
  gform :: (forall x. f x -> IO ()) -> f x -> View

instance GForm x => GForm (M1 D m x) where
  gform f (M1 x) = gform (f . M1) x

data FormState a = FormState !(forall x. (a x))

-- NOTE: this instance is for records with selector fields!
instance (KnownSymbol name, Typeable x, GForm x) => GForm (M1 C (MetaCons name _fix True) x) where
  gform f (M1 x) = 
    useState (FormState (unsafeCoerce x :: x y)) $ \State {..} ->
      let FormState x = state
      in Div <| Class (toTxt (symbolVal @name Proxy)) |>
          [ H2 <||> [ txt (symbolVal @name Proxy) ]
          , gform (\r -> modify (\_ -> FormState (unsafeCoerce r))) x
          , Button <| OnClick (\_ -> f (unsafeCoerce x)) |> [ "Submit" ]
          ]

instance (Typeable x, GForm x) => GForm (M1 C (MetaCons name _fix False) x) where
  gform f (M1 x) = gform (f . M1) x

-- Without the strictness annotations, we end up unsafely observing laziness because of the coercions!
data FormProductState a b = FormProductState !(forall x. a x) !(forall x. b x) 

-- The downside of this instance is that it induces nesting
instance (Typeable a, Typeable b, GForm a, GForm b) => GForm ((:*:) a b) where
  gform f (a :*: b) =
    useState (FormProductState (unsafeCoerce a :: a x) (unsafeCoerce b :: b x)) $ \State {..} -> 
      let FormProductState l r = state 
      in Div <||>
          [ gform (\l -> f (unsafeCoerce l :*: unsafeCoerce r) >> modify (\(FormProductState _ r) -> FormProductState (unsafeCoerce l) r)) a
          , gform (\r -> f (unsafeCoerce l :*: unsafeCoerce r) >> modify (\(FormProductState l _) -> FormProductState l (unsafeCoerce r))) b
          ]

instance (GField (M1 S (MetaSel (Just name) _u _s _s') x), KnownSymbol name) => GForm (M1 S (MetaSel (Just name) _u _s _s') x) where
  gform f x = 
    Div <| Class (toTxt (symbolVal @name Proxy)) |>
      [ Label <||> [ txt (symbolVal @name Proxy) ]
      , gfield f x
      ]

instance GForm x => GForm (M1 S (MetaSel Nothing _u _s _s') x) where
  gform f (M1 x) = gform (f . M1) x

instance Form x => GForm (K1 r x) where
  gform f (K1 x) = form (f . K1) x


