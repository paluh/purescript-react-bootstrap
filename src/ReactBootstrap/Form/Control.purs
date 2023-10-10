module ReactBootstrap.Form.Control where


import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import React.HTMLAttributes (HTMLAttributes', InputHTMLAttributes')
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Value :: Type

value
  :: { array :: Array String -> Value
     , number :: Number -> Value
     , string :: String -> Value
     }
value =
  { "string": unsafeCoerce :: String -> Value
  , "number": unsafeCoerce :: Number -> Value
  , "array": unsafeCoerce :: Array String -> Value
  }

type BaseProps controlValue extraProps = HTMLAttributes' + InputHTMLAttributes' +
  ( htmlSize :: Opt Int
  , as :: Opt String
  , size :: Opt String
  , plaintext :: Opt Boolean
  , readOnly :: Opt Boolean
  , disabled :: Opt Boolean
  , value :: Opt controlValue
  , onChange :: Opt EventHandler
  , name :: Opt String
  , isValid :: Opt Boolean
  , isInvalid :: Opt Boolean
  | extraProps
  )

-- | Direct translation from ts to purescript
-- | We probably don't really need `control` element
-- | definition as we provide `textInput` and `textArea`.
type Props_control = BaseProps String ("type" :: String)

foreign import _Control :: ReactComponent { | Props_control }

_internalcontrol
  :: forall props
   . NoProblem.Coerce { | props } { | Props_control }
  => { | props } -> JSX
_internalcontrol props = do
  let
    props' = NoProblem.coerce props
  element _Control props'

control
  :: forall props
   . NoProblem.Coerce { | props } { | Props_control }
  => { | props }
  -> JSX
control props = _internalcontrol props

type Props_inputtext = BaseProps String ()

textInput
  :: forall props props'
   . NoProblem.Coerce { | props' } { | Props_control }
  => Row.Union props (type :: String, "as" :: String) (type :: String, "as" :: String | props)
  => Row.Nub (type :: String, "as" :: String | props) props'
  => { | props }
  -> JSX
textInput props = _internalcontrol props'
  where
  props' :: { | props' }
  props' = Record.merge props { "type": "text", "as": "input" }

-- There is a bug in react-bootstrap type definitions because
-- types miss the "rows" and "cols" attributes.
type Props_textArea = BaseProps String (rows :: Opt Int, cols :: Opt Int)

_TextArea :: ReactComponent { | Props_textArea }
_TextArea = unsafeCoerce _Control

_internalTextArea
  :: forall props
   . NoProblem.Coerce { | props } { | Props_textArea }
  => { | props }
  -> JSX
_internalTextArea props = do
  let
    props' = NoProblem.coerce props
  element _TextArea props'

textArea
  :: forall props props'
   . NoProblem.Coerce { | props' } { | Props_textArea }
  => Row.Union props ("as" :: String) ("as" :: String | props)
  => Row.Nub ("as" :: String | props) props'
  => { | props }
  -> JSX
textArea props = _internalTextArea props'
  where
  props' :: { | props' }
  props' = Record.merge props { "as": "textarea" }

