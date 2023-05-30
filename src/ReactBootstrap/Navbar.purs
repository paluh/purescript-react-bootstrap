module ReactBootstrap.Navbar where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import ReactBootstrap.Types (SelectCallback)
import React.HTMLAttributes (HTMLAttributes)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (HTMLElement)

-- import * as React from 'react';
-- import { SelectCallback } from '@restart/ui/types';
-- import { BsPrefixProps, BsPrefixRefForwardingComponent } from './helpers';
-- export interface NavbarProps extends BsPrefixProps, Omit<React.HTMLAttributes<HTMLElement>, 'onSelect'> {
--     variant?: 'light' | 'dark' | string;
--     expand?: boolean | string | 'sm' | 'md' | 'lg' | 'xl' | 'xxl';
--     bg?: string;
--     fixed?: 'top' | 'bottom';
--     sticky?: 'top';
--     onToggle?: (expanded: boolean) => void;
--     onSelect?: SelectCallback;
--     collapseOnSelect?: boolean;
--     expanded?: boolean;
-- }


foreign import data Variant :: Type

variant
  :: { dark :: Variant
     , light :: Variant
     }
variant =
  { "light": unsafeCoerce "light"
  , "dark": unsafeCoerce "dark"
  }

-- expand?: boolean | string | 'sm' | 'md' | 'lg' | 'xl' | 'xxl';

foreign import data Expand :: Type

expand
  :: { boolean :: Boolean -> Expand
     , sm :: Expand
     , md :: Expand
     , lg :: Expand
     , xl :: Expand
     , xxl :: Expand
     }
expand =
  { "boolean": unsafeCoerce
  , "sm": unsafeCoerce "sm"
  , "md": unsafeCoerce "md"
  , "lg": unsafeCoerce "lg"
  , "xl": unsafeCoerce "xl"
  , "xxl": unsafeCoerce "xxl"
  }

-- fixed?: 'top' | 'bottom';

foreign import data Fixed :: Type

fixed
  :: { top :: Fixed
     , bottom :: Fixed
     }
fixed =
  { "top": unsafeCoerce "top"
  , "bottom": unsafeCoerce "bottom"
  }

-- sticky?: 'top';

foreign import data Sticky :: Type

stickyTop :: Sticky
stickyTop = unsafeCoerce "top"

type OnToggle = Boolean -> Effect Unit

-- export interface NavbarProps extends BsPrefixProps, Omit<React.HTMLAttributes<HTMLElement>, 'onSelect'> {
--     variant?: 'light' | 'dark' | string;
--     expand?: boolean | string | 'sm' | 'md' | 'lg' | 'xl' | 'xxl';
--     bg?: string;
--     fixed?: 'top' | 'bottom';
--     sticky?: 'top';
--     onToggle?: (expanded: boolean) => void;
--     onSelect?: SelectCallback;
--     collapseOnSelect?: boolean;
--     expanded?: boolean;
-- }


type Props_navbar =
  HTMLAttributes +
    ( variant :: Variant
    , expand :: Expand
    , bg :: String
    , fixed :: Fixed
    , sticky :: Sticky
    , onToggle :: OnToggle
    , onSelect :: SelectCallback
    , collapseOnSelect :: Boolean
    , expanded :: Boolean
    )


foreign import _Navbar :: ReactComponent { | Props_navbar }

_internalnavbar :: forall attrs attrs_. Row.Union attrs attrs_ Props_navbar => ReactComponent { | attrs }
_internalnavbar = unsafeCoerce _Navbar

navbar
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_navbar
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
navbar props children = element _internalnavbar propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props


-- declare const _default: BsPrefixRefForwardingComponent<"nav", NavbarProps> & {
--     Brand: BsPrefixRefForwardingComponent<"a", import("./Brand").BrandProps>;
--     Collapse: React.ForwardRefExoticComponent<import("./Collapse").CollapseProps & React.RefAttributes<HTMLDivElement>>;
--     Offcanvas: React.ForwardRefExoticComponent<Pick<import("./NavbarOffcanvas").NavbarOffcanvasProps, keyof import("./Offcanvas").OffcanvasProps> & React.RefAttributes<HTMLDivElement>>;
--     Text: BsPrefixRefForwardingComponent<"span", unknown>;
--     Toggle: BsPrefixRefForwardingComponent<"button", import("./Toggle").ToggleProps>;
-- };

-- import * as React from 'react';
-- import { BsPrefixProps, BsPrefixRefForwardingComponent } from './helpers';
-- export interface BrandProps extends BsPrefixProps, React.HTMLAttributes<HTMLElement> {
--     href?: string;
-- }
-- declare const Brand: BsPrefixRefForwardingComponent<'a', BrandProps>;
-- export default Brand;

type Props_brand =
  HTMLAttributes +
    ( href :: String
    )

foreign import _Brand :: ReactComponent { | Props_brand }

_internalbrand :: forall attrs attrs_. Row.Union attrs attrs_ Props_brand => ReactComponent { | attrs }
_internalbrand = unsafeCoerce _Brand

brand
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_brand
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
brand props children = element _internalbrand propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props


-- type Dimension = 'height' | 'width';
-- export interface CollapseProps extends TransitionCallbacks, Pick<React.HTMLAttributes<HTMLElement>, 'role'> {
--     className?: string;
--     in?: boolean;
--     mountOnEnter?: boolean;
--     unmountOnExit?: boolean;
--     appear?: boolean;
--     timeout?: number;
--     dimension?: Dimension | (() => Dimension);
--     getDimensionValue?: (dimension: Dimension, element: HTMLElement) => number;
--     children: React.ReactElement;
-- }
-- declare const Collapse: React.ForwardRefExoticComponent<CollapseProps & React.RefAttributes<Transition<any>>>;
-- export default Collapse;

foreign import data Dimension :: Type

dimension
  :: { height :: Dimension
     , width :: Dimension
     }
dimension =
  { "height": unsafeCoerce "height"
  , "width": unsafeCoerce "width"
  }

type Props_collapse =
  -- TransitionCallbacks +
    (
    -- role?: AriaRole | undefined;
      className :: String
    , in :: Boolean
    , id :: String
    , mountOnEnter :: Boolean
    , unmountOnExit :: Boolean
    , appear :: Boolean
    , timeout :: Number
    , dimension :: Dimension
    , getDimensionValue :: EffectFn2 Dimension HTMLElement Number
    , children :: Array JSX
    )

foreign import _Collapse :: ReactComponent { | Props_collapse }

_internalcollapse :: forall attrs attrs_. Row.Union attrs attrs_ Props_collapse => ReactComponent { | attrs }
_internalcollapse = unsafeCoerce _Collapse

collapse
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_collapse
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
collapse props children = element _internalcollapse propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props


-- export interface ToggleProps extends BsPrefixProps, React.HTMLAttributes<HTMLElement> {
--     label?: string;
-- }

type Props_toggle =
  HTMLAttributes +
    ( ariaControls :: String
    , label :: String
    )

foreign import _Toggle :: ReactComponent { | Props_toggle }

_internaltoggle :: forall attrs attrs_. Row.Union attrs attrs_ Props_toggle => ReactComponent { | attrs }
_internaltoggle = unsafeCoerce _Toggle

toggle
  :: forall attrs attrs_
   . Row.Union attrs attrs_ Props_toggle
  => Record attrs
  -> JSX
toggle props = element _internaltoggle props
