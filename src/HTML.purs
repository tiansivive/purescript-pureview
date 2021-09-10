module HTML (module V, div, span, button) where


import VDOM (Children, VirtualNode, Props, Attribute(..), createElement) as V


-- data Attrs = NoAttrs | Attrs V.Attrs
-- data Children 


div :: forall a. V.Props -> V.Children a -> V.VirtualNode a
div = V.createElement "div"


span :: forall a. V.Props -> V.Children a -> V.VirtualNode a
span = V.createElement "span"


button :: forall a. V.Props -> V.Children a -> V.VirtualNode a
button = V.createElement "button"