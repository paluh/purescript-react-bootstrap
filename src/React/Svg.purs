module React.Svg where

import Data.Newtype (class Newtype)
import Prim.Row as Row
import React.Basic (JSX)
import React.Basic.DOM (Props_img)
import React.Basic.DOM as DOM
import Unsafe.Coerce (unsafeCoerce)

newtype SvgUrl = SvgUrl String

derive instance Newtype SvgUrl _

svgImg
  :: forall attrs attrs' attrs_' attrs_
   . Row.Cons "src" SvgUrl attrs_ attrs
  => Row.Cons "src" String attrs_ attrs'
  => Row.Union attrs' attrs_' Props_img
  => { | attrs }
  -> JSX
svgImg props = do
  let
    props' :: { | attrs' }
    props' = unsafeCoerce props -- Record.modify (Proxy :: Proxy "src") (un SvgUrl) props
  DOM.img props'

