module Pure.Conjurer.Form.Field where

import Pure.Elm.Component
import Pure.Data.Txt (Txt)

import GHC.Generics as G
import Text.Read (readMaybe)

class Field x where
  field :: (x -> IO ()) -> x -> View
  default field :: (Generic x, GField (Rep x)) => (x -> IO ()) -> x -> View
  field onchange initial = gfield (onchange . G.to) (G.from initial)

class GField f where
  gfield :: forall x. (f x -> IO ()) -> f x -> View

instance GField s => GField (M1 i m s) where
  gfield onchange (M1 s) = gfield (onchange . M1) s

instance Field a => GField (K1 r a) where
  gfield onchange (K1 a) = field (onchange . K1) a

instance Field Txt where
  field onchange initial =
    Input <| Type "text" . OnInput (withInput onchange) . Value initial

instance Field Bool where
  field onchange initial =
    -- Keep an eye on this; checked status does not fire change events.
    Input <| Type "checkbox" . OnInput (withChecked onchange) . Checked (if initial then "true" else "false")

instance Field Int where
  field onchange initial =
    Input <| Type "number" . OnInput (withInput (maybe def onchange . readMaybe . fromTxt)) . Value (toTxt initial)

instance Field String where
  field onchange initial =
    Input <| Type "text" . OnInput (withInput (onchange . fromTxt)) . Value (toTxt initial)

