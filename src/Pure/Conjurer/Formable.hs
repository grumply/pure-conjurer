module Pure.Conjurer.Formable (Formable(..),overwriteTitle) where

import Pure.Conjurer.Fieldable
import Pure.Elm.Component
import Pure.Hooks

import Data.Typeable
import GHC.Generics as G
import GHC.TypeLits
import Unsafe.Coerce

overwriteTitle :: Txt -> CSS ()
overwriteTitle new =
  void do
    has (tag H2) do
      visibility =: hidden
      position =: relative
      after do
        visibility =: visible
        position =: absolute
        left =: 0
        content =: ("'" <> new <> "'")

data PreviewingForm x = PreviewingForm 
  { onSubmit  :: x -> IO ()
  , onPreview :: x -> IO View
  , runForm :: (x -> IO ()) -> x -> View
  , initial :: x
  }

instance Typeable x => Component (PreviewingForm x) where
  data Model (PreviewingForm x) = Model
    { current :: x
    , preview :: Maybe View
    }
    
  initialize PreviewingForm {..} = pure Model 
    { current = initial
    , preview = Nothing
    }

  data Msg (PreviewingForm x)
    = Edit | Preview | Submit | Update x
 
  upon msg PreviewingForm {..} mdl@Model {..} =
    case msg of
      Edit ->
        pure mdl { preview = Nothing }
        
      Preview -> do
        v <- onPreview current
        pure mdl { preview = Just v }

      Submit -> do
        onSubmit current
        pure mdl

      Update x -> do
        pure mdl { current = x }
        
  view PreviewingForm {..} Model {..} 
    | Just p <- preview =
      Div <||>
        [ p
        , Div <||>
          [ Button <| OnClick (\_ -> command Edit)   |> [ "Edit"   ]
          , Button <| OnClick (\_ -> command Submit) |> [ "Submit" ]
          ]
        ]
    | otherwise =
      Div <||>
        [ runForm (command . Update) current
        , Div <||>
          [ Button <| OnClick (\_ -> command Preview) |> [ "Preview" ]
          , Button <| OnClick (\_ -> command Submit)  |> [ "Submit"  ]
          ]
        ]

class Formable rec where
  form :: (rec -> IO ()) -> (rec -> IO View) -> rec -> View
  default form :: (Typeable rec, Generic rec, GFormable (Rep rec)) => (rec -> IO ()) -> (rec -> IO View) -> rec -> View
  form onSubmit onPreview initial = 
    View PreviewingForm 
      { runForm = \f x -> gform (f . G.to) (G.from x)
      , ..
      }

instance {-# INCOHERENT #-} (Typeable x, Generic x, GFormable (Rep x)) => Formable x 

class GFormable f where
  gform :: (forall x. f x -> IO ()) -> f x -> View

instance (Typeable x, GFormable x) => GFormable (M1 D (MetaData name _m _p _nt) x) where
  gform f (M1 x) = gform (f . M1) x

instance (KnownSymbol name, Typeable x, GFormable x) => GFormable (M1 C (MetaCons name _fix True) x) where
  gform f (M1 x) = 
    Div <| Class (toTxt (symbolVal @name Proxy)) |>
      [ H2 <||> [ txt (symbolVal @name Proxy) ]
      , gform (f . M1) x
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



