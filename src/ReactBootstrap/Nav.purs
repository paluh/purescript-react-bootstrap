module ReactBootstrap.Nav where


import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.Generated (Props_a)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import ReactBootstrap.Types (EventKey, SelectCallback)
import React.HTMLAttributes (HTMLAttributes)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Variant :: Type

-- variant?: 'tabs' | 'pills' | string;

variant
  :: { tabs :: Variant
     , pills :: Variant
     , string :: String -> Variant
     }
variant =
  { "tabs": unsafeCoerce "tabs"
  , "pills": unsafeCoerce "pills"
  , "string": unsafeCoerce
  }

-- @restart/ui/esm/Nav.d.ts
-- export interface NavProps extends Omit<React.HTMLAttributes<HTMLElement>, 'onSelect'> {
--     /**
--      * Key for the currently active NavItem.
--      */
--     activeKey?: EventKey;
--     /**
--      * Element used to render the component.
--      */
--     /**
--      * A callback fired when a NavItem has been selected.
--      */
--     onSelect?: SelectCallback;
-- }

type Props_baseNav others =
  ( activeKey :: EventKey
  --  as?: React.ElementType;
  -- , as :: String
  , onSelect :: SelectCallback
  | others
  )

type Props_nav =
  HTMLAttributes +
    Props_baseNav +
    ( navbarBsPrefix :: String
    , cardHeaderBsPrefix :: String
    , variant :: Variant
    , defaultActiveKey :: EventKey
    , fill :: Boolean
    , justify :: Boolean
    , navbar :: Boolean
    , navbarScroll :: Boolean
    )

foreign import _Nav :: ReactComponent { | Props_nav }

_internalnav :: forall attrs attrs_. Row.Union attrs attrs_ Props_nav => ReactComponent { | attrs }
_internalnav = unsafeCoerce _Nav

nav
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_nav
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
nav props children = element _internalnav propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

-- @restart/ui/esm/NavItem.d.ts
-- export interface NavItemProps extends React.HTMLAttributes<HTMLElement> {
--     /**
--      * Highlight the NavItem as active.
--      */
--     active?: boolean;
--     /**
--      * Element used to render the component.
--      */
--     as?: React.ElementType;
--     /**
--      * Disable the NavItem, making it unselectable.
--      */
--     disabled?: boolean;
--     /**
--      * Value passed to the `onSelect` handler, useful for identifying the selected NavItem.
--      */
--     eventKey?: EventKey;
--     /**
--      * HTML `href` attribute corresponding to `a.href`.
--      */
--     href?: string;
-- }

type Props_navItem others =
  -- HTMLAttributes +
  ( active :: Boolean
  , disabled :: Boolean
  , eventKey :: EventKey
  , href :: String
  | others
  )

type Props_link =
  Props_navItem
    + Props_a

foreign import _Link :: ReactComponent { | Props_link }

_internallink :: forall attrs attrs_. Row.Union attrs attrs_ Props_link => ReactComponent { | attrs }

_internallink = unsafeCoerce _Link

link
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_link
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
link props children = element _internallink propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

