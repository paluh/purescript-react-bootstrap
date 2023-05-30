module ReactBootstrap.Carousel where

import Prelude

import Data.Nullable (null)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import React.Basic.Events (SyntheticEvent)
import React.HTMLAttributes (HTMLAttributes')
import Record as Record
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.HTMLElement (HTMLElement)

-- export type CarouselVariant = 'dark' | string;
-- export interface CarouselRef {
--     element?: HTMLElement;
--     prev: (e?: React.SyntheticEvent) => void;
--     next: (e?: React.SyntheticEvent) => void;
-- }
-- export interface CarouselProps extends BsPrefixProps, Omit<React.HTMLAttributes<HTMLElement>, 'onSelect'> {
--     slide?: boolean;
--     fade?: boolean;
--     controls?: boolean;
--     indicators?: boolean;
--     indicatorLabels?: string[];
--     activeIndex?: number;
--     onSelect?: (eventKey: number, event: Record<string, unknown> | null) => void;
--     defaultActiveIndex?: number;
--     onSlide?: (eventKey: number, direction: 'start' | 'end') => void;
--     onSlid?: (eventKey: number, direction: 'start' | 'end') => void;
--     interval?: number | null;
--     keyboard?: boolean;
--     pause?: 'hover' | false;
--     wrap?: boolean;
--     touch?: boolean;
--     prevIcon?: React.ReactNode;
--     prevLabel?: React.ReactNode;
--     nextIcon?: React.ReactNode;
--     nextLabel?: React.ReactNode;
--     ref?: React.Ref<CarouselRef>;
--     variant?: CarouselVariant;
-- }
-- declare const _default: BsPrefixRefForwardingComponent<"div", CarouselProps> & {
--     Caption: BsPrefixRefForwardingComponent<"div", unknown>;
--     Item: BsPrefixRefForwardingComponent<"div", import("./CarouselItem").CarouselItemProps>;
-- };

foreign import data CarouselVariant :: Type

carouselVariant
  :: { dark :: CarouselVariant
     , string :: String -> CarouselVariant
     }
carouselVariant =
  { dark: unsafeCoerce "dark" :: CarouselVariant
  , string: unsafeCoerce
  }

type CarouselRef =
  { element :: Opt HTMLElement
  , prev :: Opt SyntheticEvent -> Effect Unit
  , next :: Opt SyntheticEvent -> Effect Unit
  }

foreign import data Direction :: Type

start :: Direction
start = unsafeCoerce "start"

end :: Direction
end = unsafeCoerce "end" :: Direction

foreign import data Interval :: Type

interval ::
  { null :: Interval
  , number :: Int -> Interval
  }
interval =
  { null: unsafeCoerce null :: Interval
  , number: unsafeCoerce
  }

instance Eq Direction where
  eq d1 d2 = (unsafeCoerce d1 :: String) == (unsafeCoerce d2 :: String)

type Props_carousel =
  HTMLAttributes' +
    ( slide :: Opt Boolean -- boolean
    , fade :: Opt Boolean -- boolean
    , controls :: Opt Boolean -- boolean
    , indicators :: Opt Boolean -- boolean
    , indicatorLabels :: Opt (Array String) -- string[]
    , activeIndex :: Opt Int -- number
    , onSelect :: Opt (EffectFn2 Int {} Unit) -- (eventKey: number, event: Record<string, unknown> | null) => void
    , defaultActiveIndex :: Opt Int -- number
    , onSlide :: Opt (EffectFn2 Int Direction Unit) -- (eventKey: number, direction: 'start' | 'end') => void
    , onSlid :: Opt (EffectFn2 Int Direction Unit) -- (eventKey: number, direction: 'start' | 'end') => void
    , interval :: Opt Interval
    , keyboard :: Opt Boolean -- boolean
    , pause :: Opt String -- 'hover' | false
    , wrap :: Opt Boolean -- boolean
    , touch :: Opt Boolean -- boolean
    , prevIcon :: Opt JSX -- React.ReactNode
    , prevLabel :: Opt JSX -- React.ReactNode
    , nextIcon :: Opt JSX -- React.ReactNode
    , nextLabel :: Opt JSX -- React.ReactNode
    , ref :: Opt CarouselRef -- React.Ref<CarouselRef>
    , variant :: Opt CarouselVariant -- CarouselVariant
    )

foreign import _Carousel :: ReactComponent { | Props_carousel }

_internalcarousel
  :: forall props
   . NoProblem.Coerce { | props } { | Props_carousel }
  => { | props } -> JSX
_internalcarousel props = do
  let
    props' = NoProblem.coerce props
  element _Carousel props'

carousel
  :: forall jsx props
   . ToJSX jsx
  => Row.Lacks "children" props
  => Row.Cons "children" (Array JSX) props (children :: Array JSX | props)
  => NoProblem.Coerce { children :: Array JSX | props } { | Props_carousel }
  => { | props } -> jsx -> JSX
carousel props children = do
  let
    props' = Record.insert (Proxy :: Proxy "children") (toJSX children) props
  _internalcarousel props'

-- export interface CarouselItemProps extends BsPrefixProps, React.HTMLAttributes<HTMLElement> {
--     interval?: number;
-- }

type Props_carouselItem =
  HTMLAttributes' +
    ( interval :: Opt Interval -- number
    )

foreign import _CarouselItem :: ReactComponent { | Props_carouselItem }

_internalitem
  :: forall props
   . NoProblem.Coerce { | props } { | Props_carouselItem }
  => { | props } -> JSX
_internalitem props = do
  let
    props' = NoProblem.coerce props
  element _CarouselItem props'

item
  :: forall jsx props
   . ToJSX jsx
  => Row.Lacks "children" props
  => Row.Cons "children" (Array JSX) props (children :: Array JSX | props)
  => NoProblem.Coerce { children :: Array JSX | props } { | Props_carouselItem }
  => { | props } -> jsx -> JSX
item props children = do
  let
    props' = Record.insert (Proxy :: Proxy "children") (toJSX children) props
  _internalitem props'

type Props_carouselCaption = HTMLAttributes' + ()

foreign import _CarouselCaption :: ReactComponent { | Props_carouselCaption }

caption
  :: forall jsx props
   . ToJSX jsx
  => Row.Lacks "children" props
  => Row.Cons "children" (Array JSX) props (children :: Array JSX | props)
  => NoProblem.Coerce { children :: Array JSX | props } { | Props_carouselCaption }
  => { | props }
  -> jsx
  -> JSX
caption props children = do
  let
    props' = Record.insert (Proxy :: Proxy "children") (toJSX children) props
  element _CarouselCaption (NoProblem.coerce props' :: { | Props_carouselCaption })
