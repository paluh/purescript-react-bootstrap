module ReactBootstrap.OverlayTrigger where

import Prelude

import Data.Nullable (Nullable)
import Data.Undefined.NoProblem (Opt)
import Effect (Effect)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, Ref, element)
import React.Basic.Events (EventHandler)
import ReactBootstrap.Tooltip (TooltipPropsRow)
import ReactBootstrap.Types (Placement)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Node)

foreign import data OverlayDelay :: Type

overlayDelay
  :: { number :: Number -> OverlayDelay
     , showHide ::
         { hide :: Number
         , show :: Number
         }
         -> OverlayDelay
     }
overlayDelay =
  { number: unsafeCoerce :: Number -> OverlayDelay
  , showHide: unsafeCoerce :: { show :: Number, hide :: Number } -> OverlayDelay
  }

foreign import data OverlayTriggerType :: Type

overlayTriggerType
  :: { click :: OverlayTriggerType
     , focus :: OverlayTriggerType
     , hover :: OverlayTriggerType
     }
overlayTriggerType =
  { hover: unsafeCoerce "hover" :: OverlayTriggerType
  , click: unsafeCoerce "click" :: OverlayTriggerType
  , focus: unsafeCoerce "focus" :: OverlayTriggerType
  }

type SubcomponentProps =
  { onBlur :: EventHandler
  , onFocus :: EventHandler
  , onMouseOut :: EventHandler
  , onMouseOver :: EventHandler
  , ref :: Ref (Nullable Node)
  }

type Props_overlayTrigger =
  ( children :: SubcomponentProps -> JSX
  , defaultShow :: Boolean
  , delay :: OverlayDelay
  , flip :: Boolean
  , onToggle :: Boolean -> Effect Unit
  , overlay :: { | TooltipPropsRow } -> JSX
  , placement :: Opt Placement
  -- , popperConfig
  , trigger :: Array OverlayTriggerType
  )

foreign import _OverlayTrigger :: ReactComponent { | Props_overlayTrigger }

_internalOverlayTrigger :: forall attrs attrs_. Row.Union attrs attrs_ Props_overlayTrigger => ReactComponent { | attrs }
_internalOverlayTrigger = unsafeCoerce _OverlayTrigger

overlayTrigger
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_
   . Row.Union attrs attrs_ Props_overlayTrigger
  => Row.Union (children :: SubcomponentProps -> JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: SubcomponentProps -> JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> (SubcomponentProps -> JSX)
  -> JSX
overlayTrigger props children = element _internalOverlayTrigger propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children } props
