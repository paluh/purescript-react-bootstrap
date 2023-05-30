module ReactBootstrap
  ( module Alert
  , module OverlayTrigger
  , module Tooltip
  , ssrProvider
  ) where

import React.Basic (JSX, ReactComponent, element)
import ReactBootstrap.Alert (alert) as Alert
import ReactBootstrap.OverlayTrigger (overlayTrigger) as OverlayTrigger
import ReactBootstrap.Tooltip (tooltip) as Tooltip

type Props_ssrprovider = ( children :: Array JSX )

foreign import _SSRProvider :: ReactComponent { | Props_ssrprovider }

ssrProvider :: Array JSX -> JSX
ssrProvider children = element _SSRProvider { children: children }
