module ReactBootstrap.Form
  ( module Exports
  , form
  , Props_form
  ) where

import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import ReactBootstrap.Form.Check (check) as Exports
import ReactBootstrap.Form.Control (control, textInput, textArea) as Exports
import ReactBootstrap.Form.Group (group) as Exports
import ReactBootstrap.Form.Label (label) as Exports
import ReactBootstrap.Form.Select (select) as Exports
import React.HTMLAttributes (HTMLAttributes')
import Record as Record
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

-- export interface FormProps extends React.FormHTMLAttributes<HTMLFormElement>, AsProp {
--     validated?: boolean;
-- }

type Props_form =
  HTMLAttributes' +
    ( validated :: Opt Boolean -- boolean
    )

foreign import _Form :: ReactComponent { | Props_form }

_internalform
  :: forall props
   . NoProblem.Coerce { | props } { | Props_form }
  => { | props } -> JSX
_internalform props = do
  let
    props' = NoProblem.coerce props
  element _Form props'

form
  :: forall jsx props
   . ToJSX jsx
  => Row.Lacks "children" props
  => Row.Cons "children" (Array JSX) props (children :: Array JSX | props)
  => NoProblem.Coerce { children :: Array JSX | props } { | Props_form }
  => { | props } -> jsx -> JSX
form props children = do
  let
    props' = Record.insert (Proxy :: Proxy "children") (toJSX children) props
  _internalform props'
