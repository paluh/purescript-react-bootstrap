module ReactBootstrap.Form.Range where

import Prelude

import React.HTMLAttributes (InputHTMLAttributes')
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import Effect (Effect)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type Props_range = InputHTMLAttributes' + ()

foreign import _Range :: ReactComponent { | Props_range }

range
  :: forall attrs attrs_ props
   . NoProblem.Coerce { | props } { | Props_range }
  => { | props }
  -> JSX
range props = element _Range (NoProblem.coerce props)

