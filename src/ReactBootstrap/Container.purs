module ReactBootstrap.Container where

import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import React.HTMLAttributes (HTMLAttributes)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- import * as React from 'react';
-- import { BsPrefixProps, BsPrefixRefForwardingComponent } from './helpers';
-- export interface ContainerProps extends BsPrefixProps, React.HTMLAttributes<HTMLElement> {
--     fluid?: boolean | string | 'sm' | 'md' | 'lg' | 'xl' | 'xxl';
-- }
-- declare const Container: BsPrefixRefForwardingComponent<'div', ContainerProps>;
-- export default Container;

foreign import data Fluid :: Type

fluid
  :: { boolean :: Boolean -> Fluid
     , sm :: Fluid
     , md :: Fluid
     , lg :: Fluid
     , xl :: Fluid
     , xxl :: Fluid
     }
fluid =
  { "boolean": unsafeCoerce
  , "sm": unsafeCoerce "sm"
  , "md": unsafeCoerce "md"
  , "lg": unsafeCoerce "lg"
  , "xl": unsafeCoerce "xl"
  , "xxl": unsafeCoerce "xxl"
  }

type Props_container =
  HTMLAttributes +
    ( fluid :: Fluid
    )

foreign import _Container :: ReactComponent { | Props_container }

_internalcontainer :: forall attrs attrs_. Row.Union attrs attrs_ Props_container => ReactComponent { | attrs }
_internalcontainer = unsafeCoerce _Container

container
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_container
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
container props children = element _internalcontainer propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

