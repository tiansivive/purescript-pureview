module Component where

import Effect.Class (class MonadEffect)

import Prelude (pure, unit, map, discard)
import VDOM2 as V
import Web.Event.Event (EventType(..))

type Handlers e = Array e



data Component p s a e l v = C {
    initialState :: p -> s,
    handlers :: Handlers e,
    update :: a -> s -> s,
    render :: p -> s -> Handlers e -> V.VirtualNode l v
}

data EventHandler :: Type -> (Type -> Type) -> Type
data EventHandler a e
    = On EventType a
    | Perform EventType e



with :: ∀ l v a e. MonadEffect e => V.VirtualNode l v → Array (EventHandler a e) → V.VirtualNode l v
with n handlers = n
    where 
        toListener :: EventHandler a e -> V.EventListener l v
        toListener (On (EventType eType) action) = V.On eType \v -> pure unit
        toListener (Perform (EventType eType) eff) = V.On eType \v -> do 
            eff
            pure unit
        listeners = map toListener handlers


view :: forall p s e a l v. Component p s e a l v -> V.Attrs -> Array (V.VirtualNode l v) -> (V.VirtualNode l v)
view cp p c = V.Element {
    name: "a",
    attrs: p,
    listeners: [],
    children: c
}