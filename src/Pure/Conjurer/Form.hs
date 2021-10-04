module Pure.Conjurer.Form 
  ( Form(..)
  , Field(..)
  ) where

import Pure.Conjurer.Form.Field
import Pure.Elm.Component
import Pure.Hooks

import Data.Typeable
import GHC.Generics as G
import GHC.TypeLits
import Unsafe.Coerce

class Form rec where
  form :: (rec -> IO ()) -> View
  default form :: (Generic rec, GForm (Rep rec)) => (rec -> IO ()) -> View
  form f = gform (f . G.to)

class GForm f where
  gform :: (forall x. f x -> IO ()) -> View

instance GForm x => GForm (M1 D m x) where
  -- Ignore datatype meta information here and dispatch on the wrapped data type.
  -- This could be useful for contextualizing form errors, I guess.
  gform f = gform (f . M1)

data FormState a = FormState (forall x. Maybe (a x))

-- NOTE: this instance is for records with selector fields!
instance (KnownSymbol name, Typeable x, GForm x) => GForm (M1 C (MetaCons name _fix True) x) where
  gform f = 
    useState (FormState @x Nothing) $ \State {..} ->
      let FormState x = state
      in Div <| Class (toTxt (symbolVal @name Proxy)) |>
          [ H2 <||> [ txt (symbolVal @name Proxy) ]
          , gform @x (\r -> modify (\_ -> FormState (Just (unsafeCoerce (M1 r)))))
          , Button <| OnClick (\_ -> maybe def (f . unsafeCoerce) x) |> [ "Submit" ]
          ]

instance (Typeable x, GForm x) => GForm (M1 C (MetaCons name _fix False) x) where
  gform f = gform (f . M1)

-- Without the strictness annotations, we end up unsafely observing laziness because of the coercions!
data FormProductState a b = FormProductState !(forall x. a x) !(forall x. b x) 

-- The downside of this instance is that it induces nesting
instance (Typeable a, Typeable b, GDefault a, GDefault b, GForm a, GForm b) => GForm ((:*:) a b) where
  gform f =
    useState (FormProductState (gdef @a) (gdef @b)) $ \State {..} -> 
      let FormProductState l r = state 
      in Div <||>
          [ gform @a (\l -> f (unsafeCoerce l :*: unsafeCoerce r) >> modify (\(FormProductState _ r) -> FormProductState (unsafeCoerce l) r))
          , gform @b (\r -> f (unsafeCoerce l :*: unsafeCoerce r) >> modify (\(FormProductState l _) -> FormProductState l (unsafeCoerce r)))
          ]

instance (GField (M1 S (MetaSel (Just name) _u _s _s') x), GDefault (M1 S (MetaSel (Just name) _u _s _s') x), KnownSymbol name) => GForm (M1 S (MetaSel (Just name) _u _s _s') x) where
  gform f = 
    Div <| Class (toTxt (symbolVal @name Proxy)) |>
      [ Label <||> [ txt (symbolVal @name Proxy) ]
      , gfield f (gdef @(M1 S (MetaSel (Just name) _u _s _s') x))
      ]

instance GForm x => GForm (M1 S (MetaSel Nothing _u _s _s') x) where
  gform f = gform (f . M1)

instance Form x => GForm (K1 r x) where
  gform f = form (f . K1)


