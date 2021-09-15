module HTML (module V, div, span, button) where


import VDOM2 (VirtualNode, Attrs, h) as V


-- data Attrs = NoAttrs | Attrs V.Attrs
-- data Children 



div :: forall l v. V.Attrs -> Array (V.VirtualNode l v) -> V.VirtualNode l v
div = V.h "div"


span :: forall l v. V.Attrs -> Array (V.VirtualNode l v) -> V.VirtualNode l v
span = V.h "span"


button :: forall l v. V.Attrs -> Array (V.VirtualNode l v) -> V.VirtualNode l v
button = V.h "button"