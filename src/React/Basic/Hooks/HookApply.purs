module React.Basic.Hooks.HookApply where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..), Seconds, fromDuration)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Ref as Ref
import Effect.Timer (clearInterval, clearTimeout, setInterval, setTimeout)
import Halogen.Subscription (Emitter, subscribe, unsubscribe) as HS
import React.Basic.Hooks (Hook, Ref, UseEffect, UseRef, UseState, useEffect, useRef, useState, writeRef)
import React.Basic.Hooks (Render) as RB.Hooks
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, useAff)

type HookApply hooks (newHook :: Type -> Type) = newHook hooks

-- | Applies a new hook to a hook chain, with the innermost hook as the left argument.
-- | This allows hook chains to be written in reverse order, aligning them with the
-- | order they appear when actually used in do-notation.
-- | ```purescript
-- | type UseCustomHook hooks = UseEffect String (UseState Int hooks)
-- | type UseCustomHook' = UseState Int & UseEffect String
-- | ```
infixl 0 type HookApply as &

