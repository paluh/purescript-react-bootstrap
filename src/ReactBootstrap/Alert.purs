module ReactBootstrap.Alert where

import Prelude

import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import Effect (Effect)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import React.HTMLAttributes (HTMLAttributes, HTMLAttributes')
import ReactBootstrap.Types (Variant)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type AlertProps = HTMLAttributes' +
  ( "data-testId" :: Opt String
  , transition :: Opt Boolean
  , onClose :: Opt (Effect Unit)
  , closeLabel :: Opt String
  , dismissible :: Opt Boolean
  , show :: Opt Boolean
  , variant :: Opt Variant
  )

foreign import alertImpl :: { | AlertProps } -> Array JSX -> JSX

alert
  :: forall props
   . NoProblem.Coerce { | props } { | AlertProps }
  => { | props }
  -> Array JSX
  -> JSX
alert props children = do
  let
    props' = NoProblem.coerce props
  alertImpl props' children

