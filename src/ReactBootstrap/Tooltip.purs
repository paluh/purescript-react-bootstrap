module ReactBootstrap.Tooltip where

import Data.Nullable (Nullable)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, Ref, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import ReactBootstrap.Types (Placement)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Node)

foreign import data ArrowProps :: Type
foreign import data Popper :: Type

type TooltipPropsRow =
  ( arrowProps :: ArrowProps
  , hasDoneInitialMeasure :: Boolean
  , placement :: Opt Placement
  , popper :: Popper
  , ref :: Ref (Nullable Node)
  , show :: Opt Boolean
  , children :: Array JSX
  , className :: Opt String
  )


foreign import _Tooltip :: ReactComponent { | TooltipPropsRow }

_internaltooltip :: forall attrs attrs_. Row.Union attrs attrs_ TooltipPropsRow => ReactComponent { | attrs }
_internaltooltip = unsafeCoerce _Tooltip

tooltip
  :: forall attrsNoChildren attrsWithDuplicate attrs jsx
   . ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => NoProblem.Coerce { | attrs } { | TooltipPropsRow }
  => Record attrsNoChildren
  -> jsx
  -> JSX
tooltip attrsNoChildren children = element _internaltooltip props
  where
  attrs :: { | attrs }
  attrs = Record.merge { children: toJSX children } attrsNoChildren

  props :: { | TooltipPropsRow }
  props = NoProblem.coerce attrs
