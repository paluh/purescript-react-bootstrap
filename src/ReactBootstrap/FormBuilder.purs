module ReactBootstrap.FormBuilder where

import Prelude

import Control.Alternative as Alternative
import Control.Monad.Reader (ReaderT, runReaderT, withReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import ConvertableOptions (class Defaults, defaults)
import Data.Array as Array
import Data.Array.ArrayAL (ArrayAL)
import Data.Array.ArrayAL as ArrayAL
import Data.Bifunctor (bimap, lmap)
import Data.Date (Date)
import Data.DateTime (DateTime(..), Hour, Minute, Time(..))
import Data.DateTime.ISO (parseISODate)
import Data.Decimal (Decimal)
import Data.Either (Either(..), either, note)
import Data.Enum (class BoundedEnum, toEnum, upFromIncluding)
import Data.Foldable (all, fold, foldMap, null, traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FormURLEncoded.Query as FormURLEncoded
import Data.Formatter.Parser.Number (parseDigit)
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Monoid as Monoid
import Data.Newtype (class Newtype, un, unwrap)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, right)
import Data.String as String
import Data.Tuple (fst)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem as NoProblem
import Parsing (Parser, runParser) as Parsing
import Parsing (fail)
import Parsing.Combinators (optional, try) as Parsing
import Parsing.String (char) as Parsing
import Polyform (Validator)
import Polyform.Batteries as Batteries
import Polyform.Batteries.Decimal as Batteries.Decimal
import Polyform.Batteries.Int as Batteries.Int
import Polyform.Batteries.Number as Batteries.Number
import Polyform.Batteries.UrlEncoded as UrlEncoded
import Polyform.Batteries.UrlEncoded as UrleEncoded
import Polyform.Batteries.UrlEncoded.Duals as UrlEncoded.Duals
import Polyform.Batteries.UrlEncoded.Types.Errors (ErrorId(..), Errors(..))
import Polyform.Dual as Polyform.Dual
import Polyform.Validator (liftFnEither)
import Polyform.Validator as Validator
import Polyform.Validator.Dual as Polyform.Validator.Dual
import Prim.Row as Row
import React.Basic (JSX)
import React.Basic.DOM (div_, li_, text, ul_) as DOOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (type (/\), (/\))
import React.Basic.Hooks.UseForm (Form(..), InputState)
import React.Basic.Hooks.UseForm as UseForm
import ReactBootstrap.Form as Bootstrap.Form
import ReactBootstrap.Form as Form
import ReactBootstrap.Form.Check as Check
import ReactBootstrap.Form.Control as Form.Control
import Record as Record
import Type.Prelude (Proxy(..))

choiceOpt :: forall a b p. Choice p => Profunctor p => p a b -> p (Maybe a) (Maybe b)
choiceOpt p = dimap (note unit) (either (const Nothing) Just) (right p)

requiredV :: forall t c d m. Monad m => t -> Validator m (Array t) c d -> Validator m (Array t) (Maybe c) d
requiredV msg v = v <<< Validator.liftFnEither (note [ msg ])

requiredV' :: forall c d m. Monad m => Validator m (Array String) c d -> Validator m (Array String) (Maybe c) d
requiredV' v = requiredV "This field is required" v

-- The current default rendering uses plain JSX and not react-bootstrap
-- for labels and other pieces beside `Form.Control`.
-- TODO: check if the migration to react-bootstrap is worth it.
type FormElement = JSX

-- TODO [optimization]: we can replace `Array` in here by the array-builder
-- because we work with small arrays.
type BootstrapForm validatorM = Form validatorM (Array FormElement) String

type IdCounter = Int
type Prefix = String
-- We should probably use RWST here and store all the generated ids because
-- these can be pretty useful in the context of multifield etc.
type FormBuilderM builderM = ReaderT (Maybe Prefix) (StateT IdCounter builderM)
-- | The `T` suffix is an overstatement in here because we don't provide
-- | a `MonadTrans` instance for `FormBuilderT`. We even don't provide
-- | a `Monad` instance for it.
newtype FormBuilderT builderM validatorM i o = FormBuilderT
  (Compose (FormBuilderM builderM) (BootstrapForm validatorM i) o)

type FormBuilder = FormBuilderT Identity

derive instance Newtype (FormBuilderT builderM validatorM i o) _
derive newtype instance (Applicative builderM, Applicative validatorM) => Functor (FormBuilderT builderM validatorM i)
derive newtype instance (Monad builderM, Monad validatorM) => Apply (FormBuilderT builderM validatorM i)
derive newtype instance (Monad builderM, Monad validatorM) => Applicative (FormBuilderT builderM validatorM i)
instance (Monad validatorM, Monad builderM) => Semigroupoid (FormBuilderT builderM validatorM) where
  compose (FormBuilderT (Compose builder1)) (FormBuilderT (Compose builder2)) = formBuilderT do
    form1 <- builder1
    form2 <- builder2
    pure $ compose form1 form2

instance (Monad builderM, Monad validatorM) => Category (FormBuilderT builderM validatorM) where
  identity = formBuilderT $ pure identity

formBuilderT
  :: forall builderM i o validatorM
   . FormBuilderM builderM (BootstrapForm validatorM i o)
  -> FormBuilderT builderM validatorM i o
formBuilderT = FormBuilderT <<< Compose

liftBuilderM
  :: forall builderM validatorM i o
   . Monad builderM
  => builderM (BootstrapForm validatorM i o)
  -> FormBuilderT builderM validatorM i o
liftBuilderM = formBuilderT <<< lift <<< lift

unFormBuilder
  :: forall builderM i o validatorM
   . FormBuilderT builderM validatorM i o
  -> FormBuilderM builderM (BootstrapForm validatorM i o)
unFormBuilder (FormBuilderT (Compose builder)) = builder

type FormBuilderT' builderM validatorM o = FormBuilderT builderM validatorM Query o

type FormBulider = FormBuilderT Identity
type FormBuilder' validatorM o = FormBuilder validatorM Query o

evalBuilderT
  :: forall a builderM validatorM
   . Functor builderM
  => Maybe Prefix
  -> FormBuilderT' builderM validatorM a
  -> builderM (BootstrapForm validatorM Query a)
evalBuilderT possiblePrefix (FormBuilderT (Compose m)) = flip evalStateT 0 <<< runReaderT m $ possiblePrefix

evalBuilderT'
  :: forall a builderM validatorM
   . Functor builderM
  => FormBuilderT' builderM validatorM a
  -> builderM (BootstrapForm validatorM Query a)
evalBuilderT' = evalBuilderT Nothing

evalBuilder
  :: forall a validatorM
   . Maybe Prefix
  -> FormBuilder' validatorM a
  -> BootstrapForm validatorM Query a
evalBuilder possiblePrefix = un Identity <<< evalBuilderT possiblePrefix

evalBuilder'
  :: forall a validatorM
   . FormBuilder' validatorM a
  -> BootstrapForm validatorM Query a
evalBuilder' = evalBuilder Nothing

genId
  :: forall builderM
   . Monad builderM
  => FormBuilderM builderM String
genId = do
  possiblePrefix <- ask
  let
    prefix str = case possiblePrefix of
      Nothing -> str
      Just pref -> pref <> "-" <> str
  counter <- get
  put (counter + 1)
  let id = prefix $ show counter
  pure id

_genFieldId
  :: forall builderM r
   . Monad builderM
  => { name :: Maybe FieldId | r }
  -> FormBuilderM builderM FieldId
_genFieldId props = do
  case props.name of
    Just name -> pure name
    Nothing -> FieldId <$> genId

fieldValidity
  :: forall a
   . Eq a
  => Boolean
  -> a
  -> Maybe (Array a /\ Array a)
  -> { errors :: Array a
     , isInvalid :: Boolean
     , isValid :: Boolean
     }
fieldValidity touched value errors = do
  let
    validatedValue = do
      _ /\ vals <- errors
      Array.head vals
    errors' = fold $ map fst errors
    isInvalid = touched && Just value == validatedValue && not (null errors')
    isValid = Just [] == map fst errors
  { errors: errors', isInvalid, isValid }

type TextInputOptionalPropsRow r =
  ( label :: Maybe JSX
  , name :: Maybe FieldId
  , initial :: String
  , layout :: FieldLayout
  , helpText :: Maybe JSX
  , missingError :: String
  , placeholder :: String
  , "type" :: String
  , touched :: Boolean

  -- These make no sens in the context of a text input.
  -- FIXME: We should switch to an easier to compose
  -- representation of optional props (undefined-is-not-a-problem?)
  , max :: Opt Number
  , min :: Opt Number
  , sizing :: Maybe FormControlSizing
  , step :: Opt Number
  | r
  )

type TextInputOptionalProps = { | TextInputOptionalPropsRow () }

defaultTextInputProps :: TextInputOptionalProps
defaultTextInputProps =
  { label: Nothing
  , layout: MultiColumn
      { sm: Col3Label
      , md: Col3Label
      , lg: Col3Label
      }
  , missingError: "This field is required"
  , name: Nothing
  , initial: ""
  , placeholder: ""
  , helpText: Nothing
  , "type": "text"
  , touched: false
  , max: NoProblem.undefined
  , min: NoProblem.undefined
  , sizing: Nothing
  , step: NoProblem.undefined
  }

type TextInputProps m a =
  { validator :: Batteries.Validator m String (Maybe String) a
  | TextInputOptionalPropsRow ()
  }

data FormControlSizing = FormControlSm | FormControlLg

data LabelSpacing
  = Col1Label
  | Col2Label
  | Col3Label
  | Col4Label
  | Col5Label
  | Col6Label
derive instance Eq LabelSpacing
derive instance Ord LabelSpacing

type LabelSpacings =
  { sm :: LabelSpacing
  , md :: LabelSpacing
  , lg :: LabelSpacing
  }

col3spacings :: LabelSpacings
col3spacings =
  { sm: Col3Label
  , md: Col3Label
  , lg: Col3Label
  }

data FieldLayout
  = Inline
  | MultiColumn LabelSpacings
derive instance Eq FieldLayout
derive instance Ord FieldLayout

data Breakpoint = Sm | Md | Lg

breakpointToString :: Breakpoint -> String
breakpointToString = case _ of
  Sm -> "sm"
  Md -> "md"
  Lg -> "lg"

labelSpacingToClasses :: Breakpoint -> LabelSpacing -> { labelColClass :: String, inputColClass :: String }
labelSpacingToClasses breakpoint = do
  let
    breakpointStr = breakpointToString breakpoint
  case _ of
    Col1Label -> { labelColClass: "col-" <> breakpointStr <> "-1", inputColClass: "col-" <> breakpointStr <> "-11" }
    Col2Label -> { labelColClass: "col-" <> breakpointStr <> "-2", inputColClass: "col-" <> breakpointStr <> "-10" }
    Col3Label -> { labelColClass: "col-" <> breakpointStr <> "-3", inputColClass: "col-" <> breakpointStr <> "-9" }
    Col4Label -> { labelColClass: "col-" <> breakpointStr <> "-4", inputColClass: "col-" <> breakpointStr <> "-8" }
    Col5Label -> { labelColClass: "col-" <> breakpointStr <> "-5", inputColClass: "col-" <> breakpointStr <> "-7" }
    Col6Label -> { labelColClass: "col-" <> breakpointStr <> "-6", inputColClass: "col-" <> breakpointStr <> "-6" }

labelSpacingsToClasses :: LabelSpacings -> { labelColClass :: String, inputColClass :: String }
labelSpacingsToClasses { sm, md, lg } = do
  let
    smClasses = labelSpacingToClasses Sm sm
    mdClasses = labelSpacingToClasses Md md
    lgClasses = labelSpacingToClasses Lg lg
  { labelColClass: smClasses.labelColClass <> " " <> mdClasses.labelColClass <> " " <> lgClasses.labelColClass
  , inputColClass: smClasses.inputColClass <> " " <> mdClasses.inputColClass <> " " <> lgClasses.inputColClass
  }

isInline :: FieldLayout -> Boolean
isInline = eq Inline

-- Rendering helper used by fields constructors below.
-- Currently we are following the form layout described here:
--  https://getbootstrap.com/docs/5.0/forms/layout/#horizontal-form
-- TODO: Make it a default prop of the final field
-- constructor.
renderTextInput
  :: { layout :: FieldLayout
     , possibleHelpText :: Maybe JSX
     , possibleLabel :: Maybe JSX
     , name :: FieldId
     , placeholder :: String
     , type :: String
     , sizing :: Maybe FormControlSizing
     , max :: Opt Number
     , min :: Opt Number
     , step :: Opt Number
     }
  -> InputState String
  -> FormElement
renderTextInput
  props@{ layout, possibleLabel, possibleHelpText, name, placeholder, "type": type_, sizing }
  { value, errors, onChange, touched } = do
  let
    nameStr = un FieldId name
    label = flip foldMap possibleLabel \labelJSX -> case layout of
      Inline -> DOM.label {} [ labelJSX ]
      MultiColumn spacings -> do
        let { labelColClass } = labelSpacingsToClasses spacings
        DOM.label { className: labelColClass <> " col-form-label-sm" } [ labelJSX ]
    body = do
      let
        { errors: errors', isValid, isInvalid } = fieldValidity touched value errors
        className = String.joinWith " " $ Array.catMaybes
          [ if isInline layout then Just "mb-md-1" else Nothing
          , case sizing of
              Nothing -> Nothing
              Just FormControlSm -> Just "form-control-sm"
              Just FormControlLg -> Just "form-control-lg"
          ]
        input = Form.textInput
          { className
          , name: nameStr
          , placeholder
          , value
          , onChange: handler targetValue (onChange <<< fromMaybe "")
          , isValid
          , isInvalid
          , "type": type_
          , step: props.step
          , min: props.min
          , max: props.max
          }
      case layout of
        Inline -> case possibleHelpText of
          Nothing -> input
          Just _ -> DOOM.div_
            [ input
            , renderPossibleHelpText possibleHelpText
            ]
        MultiColumn spacing -> do
          let { inputColClass } = labelSpacingsToClasses spacing
          DOM.div { className: inputColClass } $
            input
              <> do
                Monoid.guard isInvalid do
                  DOM.div { className: "invalid-feedback" }
                    [ DOOM.ul_ $ map (DOOM.li_ <<< Array.singleton <<< DOOM.text) errors' ]
              <> do
                Monoid.guard (not isInvalid) do
                  renderPossibleHelpText possibleHelpText

  if isInline layout then DOM.div { className: "col-12 flex-fill" } [ label, body ]
  else DOM.div { className: "row mb-2" } [ label, body ]

textInput
  :: forall a builderM props validatorM
   . Monad builderM
  => Monad validatorM
  => Defaults TextInputOptionalProps { | props } (TextInputProps validatorM a)
  => { | props }
  -> FormBuilderT' builderM validatorM a
textInput props = formBuilderT do
  let
    props' = defaults defaultTextInputProps props
  name <- _genFieldId props'
  let
    form :: BootstrapForm validatorM Query a
    form = UseForm.input
      name
      props'.initial
      ( Array.singleton <<< renderTextInput
          { layout: props'.layout
          , possibleLabel: props'.label
          , possibleHelpText: props'.helpText
          , name
          , placeholder: props'.placeholder
          , "type": props'."type"
          , max: props'.max
          , min: props'.min
          , sizing: props'.sizing
          , step: props'.step
          }
      )
      props'.touched
      props'.validator
  pure form

_validator = (Proxy :: Proxy "validator")
_type = (Proxy :: Proxy "type")

_typedTextInput
  :: forall a builderM props validatorM
   . Monad builderM
  => Monad validatorM
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps
       { "type" :: String, validator :: Batteries.Validator validatorM String (Maybe String) a | props }
       (TextInputProps validatorM a)
  => { | props }
  -> String
  -> Batteries.Validator validatorM String (Maybe String) a
  -> FormBuilderT' builderM validatorM a
_typedTextInput props type_ validator = do
  textInput props''
  where
  props'' = Record.insert _validator validator $ Record.insert _type type_ $ props

type NumberInputOptionalPropsRow r =
  ( max :: Int
  | TextInputOptionalPropsRow r
  )

numberInput
  :: forall builderM props validatorM
   . Monad validatorM
  => Monad builderM
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps
       { "type" :: String, validator :: Batteries.Validator validatorM String (Maybe String) Number | props }
       (TextInputProps validatorM Number)
  => { | props }
  -> FormBuilderT' builderM validatorM Number
numberInput props = _typedTextInput props "number" $ requiredV' validator
  where
  validator :: Batteries.Validator validatorM String String Number
  validator = Batteries.stringifyValidator Batteries.Number.validator

intInput
  :: forall builderM props validatorM
   . Monad validatorM
  => Monad builderM
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps
       { "type" :: String, validator :: Batteries.Validator validatorM String (Maybe String) Int | props }
       (TextInputProps validatorM Int)
  => { | props }
  -> FormBuilderT' builderM validatorM Int
intInput props = _typedTextInput props "number" $ requiredV' validator
  where
  validator :: Batteries.Validator validatorM String String Int
  validator = Batteries.stringifyValidator Batteries.Int.validator

decimalInput
  :: forall a builderM props validatorM
   . Monad builderM
  => Monad validatorM
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps
       { "type" :: String, validator :: Batteries.Validator validatorM String (Maybe String) a | props }
       (TextInputProps validatorM a)
  => { validator :: Batteries.Validator validatorM String (Maybe Decimal) a | props }
  -> FormBuilderT' builderM validatorM a
decimalInput props = _typedTextInput props' "text" $ validator
  where
  decimalValidator = Record.get _validator props
  props' = Record.delete _validator props

  formatting = Batteries.Decimal.formatting { decimalSeparator: Just ".", separators: [ " ", "," ] }

  validator :: Batteries.Validator validatorM String (Maybe String) a
  validator = decimalValidator <<< choiceOpt do
    Batteries.stringifyValidator $ Batteries.Decimal.validator formatting

dateInput
  :: forall a builderM props validatorM
   . Monad builderM
  => Monad validatorM
  => Row.Lacks "type" props
  => Row.Lacks "validator" props
  => Defaults
       TextInputOptionalProps
       { "type" :: String
       , validator :: Batteries.Validator validatorM String (Maybe String) a
       | props
       }
       (TextInputProps validatorM a)
  => { validator :: Batteries.Validator validatorM String (Maybe Date) a | props }
  -> FormBuilderT' builderM validatorM a
dateInput props = _typedTextInput props_ "date" validator
  where
  props_ = Record.delete _validator props
  dateValidator = Record.get _validator props

  validator :: Batteries.Validator validatorM String (Maybe String) a
  validator = dateValidator <<< choiceOpt do
    liftFnEither \str -> do
      let
        res = Parsing.runParser str parseISODate
      lmap (const [ "Invalid date" ]) $ res

timeInput
  :: forall a builderM props validatorM
   . Monad builderM
  => Monad validatorM
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps
       { "type" :: String, validator :: Batteries.Validator validatorM String (Maybe String) a | props }
       (TextInputProps validatorM a)
  => { validator :: Batteries.Validator validatorM String (Maybe Time) a | props }
  -> FormBuilderT' builderM validatorM a
timeInput props = _typedTextInput props' "time" validator
  where
  props' = Record.delete _validator props
  timeValidator = Record.get _validator props

  parseTime :: Parsing.Parser String Time
  parseTime = do
    let
      noteExcept :: forall a'. String -> Maybe a' -> Parsing.Parser String a'
      noteExcept _ (Just a) = pure a
      noteExcept err Nothing = fail err

      parseDigits :: Parsing.Parser String Int
      parseDigits = do
        tens <- parseDigit
        ones <- parseDigit
        pure $ 10 * tens + ones
    (hh :: Hour) <- parseDigits >>= toEnum >>> noteExcept "Invalid hour"
    _ <- colon
    (mm :: Minute) <- parseDigits >>= toEnum >>> noteExcept "Invalid minute"

    pure $ Time hh mm bottom bottom
    where
    colon = Parsing.optional $ Parsing.try $ Parsing.char ':'

  validator :: Batteries.Validator validatorM String (Maybe String) a
  validator = timeValidator <<< liftFnEither case _ of
    Nothing -> Right Nothing
    Just str -> do
      let
        res = Parsing.runParser str parseTime
      bimap (const [ "Invalid time" ]) Just $ res

type MultiFieldIds = { multi :: FieldId, sub :: Array FieldId }

multiField
  :: forall builderM i o validatorM
   . Monad builderM
  => Maybe JSX
  -> Maybe JSX
  -> (FieldId -> FormBuilderT builderM validatorM i o)
  -> FormBuilderT builderM validatorM i o
multiField possibleLabel possibleHelpText fieldsFormBuilder = formBuilderT do
  prefix <- genId
  form <- withReaderT (const $ Just prefix) $
    unFormBuilder (fieldsFormBuilder $ FieldId prefix)
  let
    Form (formRecord@{ render }) = form
  -- errorId = Safe.Coerce.coerce fieldId
  pure $ Form formRecord
    { render = \state -> do
        let
          prefixPattern = String.Pattern prefix

          possibleFieldErrors :: Maybe (Map ErrorId (Array String))
          possibleFieldErrors = do
            errors /\ validationQuery <- (state.errors :: Maybe (UrleEncoded.Errors String /\ Query))
            let
              isPrefixOf p = isJust <<< String.stripPrefix p

              filterKeyFn :: forall n. Newtype n String => n -> Boolean
              filterKeyFn key = isPrefixOf prefixPattern (unwrap key)

              stateSubquery = Map.filterKeys filterKeyFn (un FormURLEncoded.Query state.query)
              validationSubquery = Map.filterKeys filterKeyFn (un FormURLEncoded.Query validationQuery)
            Alternative.guard (stateSubquery /= validationSubquery) $> do
              Map.filterKeys filterKeyFn (un Errors errors)

          label =
            DOM.label
              { className: "col-sm-3 col-form-label-sm" } $ fold possibleLabel

          body =
            DOM.div { className: "col-sm-9" } do
              [ DOM.div { className: "row row-cols-lg-auto align-items-center" }
                  $ render state
              , renderPossibleHelpText possibleHelpText
              ]
              <> flip foldMap possibleFieldErrors \fieldErrors ->
                flip foldMapWithIndex fieldErrors \fieldId errors -> do
                  let
                    errorId = un ErrorId fieldId
                  DOM.div
                    { className: "col-sm-9 offset-sm-3" }
                    [ DOM.div
                        { className: "invalid-feedback d-block"
                        , id: errorId
                        }
                        $ map (DOOM.div_ <<< Array.singleton <<< DOOM.text) errors
                    ]
        [ DOM.div { className: "row mb-lg-2 mb-md-1" } [ label, body ] ]
    }

-- TODO: Add help text support and field level error messages handling
dateTimeField
  :: forall a builderM validatorM
   . Monad builderM
  => Monad validatorM
  => Maybe JSX
  -> Maybe JSX
  -> Batteries.Validator validatorM String (Maybe DateTime) a
  -> FormBuilderT' builderM validatorM a
dateTimeField possibleLabel possibleHelpText dateTimeValidator = do
  let
    dateTimeValidationStep errorId = formBuilderT $ pure $ UseForm.liftValidator
      (UrlEncoded.fromValidator errorId dateTimeValidator)
    fieldsFormBuilder multiFieldErrorId = dateTimeValidationStep multiFieldErrorId <<< ado
      di <- dateInput
        { layout: Inline
        , validator: identity
        }

      ti <- timeInput
        { layout: Inline
        , initial: "00:00"
        , validator: identity
        }
      in
        DateTime <$> di <*> ti
  multiField possibleLabel possibleHelpText fieldsFormBuilder

type TextAreaOptionalPropsRow r =
  ( rows :: Int
  | TextInputOptionalPropsRow r
  )

type TextAreaOptionalProps = { | TextAreaOptionalPropsRow () }

defaultTextAreaProps :: TextAreaOptionalProps
defaultTextAreaProps =
  { label: Nothing
  , missingError: "This field is required"
  , name: Nothing
  , initial: ""
  , layout: MultiColumn col3spacings
  , placeholder: ""
  , rows: 3
  , helpText: Nothing
  -- FIXME: We should not use TextInput row as a baseline and we should drop this
  -- from TextArea row.
  , "type": "textarea"
  , touched: false
  , max: NoProblem.undefined
  , min: NoProblem.undefined
  , sizing: Nothing
  , step: NoProblem.undefined
  }

type TextAreaProps m a =
  { validator :: Batteries.Validator m String (Maybe String) a
  | TextAreaOptionalPropsRow ()
  }

renderPossibleHelpText :: Maybe JSX -> JSX
renderPossibleHelpText = foldMap \ht ->
  DOM.div { className: "m-1" } [ DOM.small { className: "form-text text-muted" } ht ]

-- TODO: This function is nearly identical to `renderTextInput` above.
-- We should find a way to smash them into a single one by introducing:
-- `data AnyTextInput = TextArea { rows: Int } | TextInput`.
renderTextArea
  :: { helpText :: Maybe JSX
     , layout :: FieldLayout
     , possibleLabel :: Maybe JSX
     , name :: FieldId
     , placeholder :: String
     , rows :: Int
     }
  -> InputState String
  -> FormElement
renderTextArea { possibleLabel, layout, helpText, name, placeholder, rows } { value, errors, onChange, touched } = do
  let
    nameStr = un FieldId name
    { labelColClass, inputColClass } = case layout of
      MultiColumn labelSpacings -> labelSpacingsToClasses labelSpacings
      Inline -> { labelColClass: "", inputColClass: "" }


    label = DOM.label { className: "col-form-label-sm " <> labelColClass, htmlFor: nameStr } $ possibleLabel `flip foldMap`
      \labelJsx ->
        [ labelJsx ]
    body = DOM.div { className: "col-sm-9 " <> inputColClass } do
      let
        { errors: errors', isValid, isInvalid } = fieldValidity touched value errors
      Form.Control.textArea
        { id: nameStr
        , name: nameStr
        , placeholder
        , value
        , onChange: handler targetValue (onChange <<< fromMaybe "")
        , rows
        , isValid
        , isInvalid
        }
        <> do
          fold
            [ Monoid.guard isInvalid do
                DOM.div { className: "invalid-feedback" }
                  [ DOOM.ul_ $ map (DOOM.li_ <<< Array.singleton <<< DOOM.text) errors' ]
            , Monoid.guard (isJust helpText && not isInvalid) $
                renderPossibleHelpText helpText
            ]
  DOM.div { className: "mb-2 row" } [ label, body ]

textArea
  :: forall a builderM props validatorM
   . Monad builderM
  => Monad validatorM
  => Defaults TextAreaOptionalProps { | props } (TextAreaProps validatorM a)
  => { | props }
  -> FormBuilderT' builderM validatorM a
textArea props = formBuilderT do
  name <- _genFieldId props'
  let
    form :: BootstrapForm validatorM Query a
    form = UseForm.input
      name
      props'.initial
      ( Array.singleton <<< renderTextArea
          { possibleLabel: props'.label
          , name
          , layout: props'.layout
          , helpText: props'.helpText
          , placeholder: props'.placeholder
          , rows: props'.rows
          }
      )
      props'.touched
      props'.validator
  pure form
  where
  props' = defaults defaultTextAreaProps props

-- Help texts can be specified for each choice
-- but also for the whole field.
type FieldChoice label =
  { disabled :: Boolean
  , helpText :: Maybe JSX
  , label :: Maybe label
  , value :: String
  }

type RadioFieldChoice = FieldChoice JSX

radioFieldChoice :: String -> JSX -> RadioFieldChoice
radioFieldChoice value label = { disabled: false, helpText: Nothing, label: Just label, value }

type SelectFieldChoice = FieldChoice String

selectFieldChoice :: String -> String -> SelectFieldChoice
selectFieldChoice label value = { disabled: false, helpText: Nothing, label: Just label, value }

data ChoiceFieldChoices
  = RadioButtonFieldChoices
      { switch :: Boolean
      , choices :: ArrayAL 1 RadioFieldChoice -- use `solo` / `solo'` to create
      }
  | SelectFieldChoices (ArrayAL 1 SelectFieldChoice) -- use `duet` / `duet'` to create

type ChoiceFieldOptionalPropsRow r =
  ( label :: Maybe JSX
  , helpText :: Maybe JSX
  , inline :: Boolean
  , missingError :: String
  , name :: Maybe FieldId
  , initial :: String
  , touched :: Boolean
  | r
  )

type ChoiceFieldOptionalProps = { | ChoiceFieldOptionalPropsRow () }

defaultChoiceFieldProps :: ChoiceFieldOptionalProps
defaultChoiceFieldProps =
  { label: Nothing
  , helpText: Nothing
  , missingError: "This field is required"
  , name: Nothing
  , initial: ""
  , inline: false
  , touched: false
  }

type ChoiceFieldProps validatorM a =
  { choices :: ChoiceFieldChoices
  , validator :: Batteries.Validator validatorM String (Maybe String) a
  | ChoiceFieldOptionalPropsRow ()
  }

renderChoiceField
  :: { choices :: ChoiceFieldChoices
     , inline :: Boolean
     , possibleLabel :: Maybe JSX
     , name :: FieldId
     , possibleHelpText :: Maybe JSX
     }
  -> InputState String
  -> Array FormElement
renderChoiceField
  { choices, inline, possibleHelpText, possibleLabel, name }
  { value: selectedValue, errors, onChange, touched } = do
  let
    nameStr = un FieldId name
    -- FIXME: We don't use `id` for label (htmlFor) yet.
    idStr = nameStr
    label = flip foldMap possibleLabel \labelJSX ->
      if inline then DOM.legend {} [ labelJSX ]
      else DOM.legend { className: "col-sm-3 col-form-label-sm" } [ labelJSX ]

    body = case choices of
      RadioButtonFieldChoices { switch, choices: choices' } -> do
        let
          noLabels = all (isNothing <<< _.label) choices'
          renderChoice { disabled, helpText, label: possibleLabel', value } = do
            let
              checked = value == selectedValue
              label'' = fold possibleLabel' <> renderPossibleHelpText helpText
              { isValid, isInvalid } = fieldValidity touched value errors

            Form.check
              { disabled
              , id: nameStr <> "-" <> value
              , label: label''
              , isValid
              , isInvalid
              , name: nameStr
              -- , feedback
              -- , feedbackTooltip
              , "type":
                  if switch then Check.checkType.switch
                  else Check.checkType.radio
              , value
              , checked
              , onChange: handler_ do
                  when (not checked) $ onChange value
              }
          className =
            if not inline then "form-check col-sm-9"
            else "form-check" <> (if noLabels then " pl-1" else "")
        DOM.div { className } $ map renderChoice (ArrayAL.toArray choices')

      SelectFieldChoices choices' -> do
        let
          renderOption { disabled, label: label', value } = do
            DOM.option { value, disabled } [ DOOM.text $ fold label' ]

          onChangeHandler = handler targetValue \val -> do
            (traverse_ onChange val)

          { isValid, isInvalid } = fieldValidity touched selectedValue errors

          select =
            Bootstrap.Form.select
              { onChange: onChangeHandler
              , className: if inline then "mb-md-1" else ""
              , value: selectedValue
              , id: idStr
              , name: nameStr
              , isValid
              , isInvalid
              } $
              map renderOption (ArrayAL.toArray choices')
        if inline then case possibleHelpText of
          Nothing -> select
          Just _ -> DOOM.div_
            [ select, renderPossibleHelpText possibleHelpText ]
        else DOM.div { className: "col-sm-9" } $
          [ select
          , renderPossibleHelpText possibleHelpText
          ]

  pure $
    if inline then DOM.div { className: "col-12 flex-fill" } [ label, body ]
    else DOM.div { className: "row mb-2" } [ label, body ]

choiceField
  :: forall a builderM props validatorM
   . Monad validatorM
  => Monad builderM
  => Defaults ChoiceFieldOptionalProps { | props } (ChoiceFieldProps validatorM a)
  => { | props }
  -> FormBuilderT' builderM validatorM a
choiceField props = formBuilderT do
  name <- _genFieldId props'
  let
    -- input name initial render touched validator = Form
    form = UseForm.input
      name
      props'.initial
      -- props'.missingError
      ( renderChoiceField
          { inline: props'.inline
          , possibleLabel: props'.label
          , name
          , choices: props'.choices
          , possibleHelpText: props'.helpText
          }
      )
      props'.touched
      props'.validator
  pure form
  where
  props' = defaults defaultChoiceFieldProps props

type ChoiceConfig doc =
  { disabled :: Boolean
  , label :: Maybe doc
  , helpText :: Maybe JSX
  }

data UseChoiceField a
  = UseRadioButtonField
      (a -> ChoiceConfig JSX)
  | UseSelectField
      (a -> ChoiceConfig String)

choiceField'
  :: forall a builderM props props' validatorM
   . Monad validatorM
  => Monad builderM
  => BoundedEnum a
  => Defaults ChoiceFieldOptionalProps { | props' } (ChoiceFieldProps validatorM a)
  => Row.Nub
       ( choices :: ChoiceFieldChoices
       , initial :: String
       , validator :: Batteries.Validator validatorM String (Maybe String) a
       | props
       )
       props'
  => UseChoiceField a
  -> Maybe (ArrayAL 1 a)
  -> { | props }
  -> FormBuilderT' builderM validatorM a
choiceField' useElement possibleArr props = do
  let
    dual :: Polyform.Validator.Dual.Dual validatorM _ _ _
    dual = Batteries.stringifyDual $ UrlEncoded.Duals.enum (Proxy :: Proxy a)
    validator = requiredV' $ Polyform.Dual.parser dual

    serialize :: a -> String
    serialize = Polyform.Validator.Dual.runSerializer dual

    asChoice :: forall doc. (a -> ChoiceConfig doc) -> a -> FieldChoice doc
    asChoice mkCfg a = do
      let
        value = serialize a

        cfg :: ChoiceConfig doc
        cfg = mkCfg a

      { label: cfg.label
      , value
      , disabled: cfg.disabled
      , helpText: cfg.helpText
      }

    arr = case possibleArr of
      Nothing -> upFromIncluding bottom :: ArrayAL 1 a
      Just arr' -> arr'
    initial = serialize (ArrayAL.head arr)

    choices = case useElement of
      UseRadioButtonField mkCfg -> RadioButtonFieldChoices
        { switch: true
        , choices: do
            let
              asChoice' = asChoice mkCfg
            map asChoice' arr
        }
      UseSelectField mkCfg -> SelectFieldChoices do
        let
          asChoice' = asChoice mkCfg
        map asChoice' arr

    props' :: { | props' }
    props' = Record.merge
      { choices
      , validator
      , initial
      }
      props
  choiceField props'
