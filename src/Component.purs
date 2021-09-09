module Component where

import VDOM as V

type Handlers e = Array e

data Dispatcher a = Dispatch a

data Component p s a e l = C {
    initialState :: p -> s,
    handlers :: Handlers e,
    update :: a -> s -> s,
    render :: p -> s -> Handlers e -> V.VirtualNode l
}


view :: forall p s e a l. Component p s e a l -> V.Props -> V.Children l -> V.VirtualNode l
view cp p c = V.Element {
    tag: "a",
    props: p,
    listeners: [],
    children: c
}