module VDOM2 where

import Data.Tuple (Tuple(..))

import Control.Applicative ((<#>))
import Control.Apply ((<$>))
import Data.Array (length, (!!), (..)) 
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence_)
import Effect (Effect)
import Foreign.Object as FO
import Prelude (class Show, Unit, bind, discard, flip, map, pure, unit, when, ($), (-), (/=), (<<<), (<>), (>), (>=>))

type Attrs = FO.Object String



data EventListener :: forall k. k -> Type -> Type
data EventListener l v = On String (v → Effect Unit)

-- | The type of virtual DOM nodes. Use either `h` or `text` to create
-- | them.
data VirtualNode :: forall k. k -> Type -> Type
data VirtualNode l v
  = Element
    { name :: String
    , attrs :: Attrs 
    , listeners :: Array (EventListener l v)
    , children :: Array (VirtualNode l v)
    }
  | Text String

instance showVirtualNode :: Show (VirtualNode l v) where
  show (Element n) = "<VirtualNode:" <> n.name <> ">"
  show (Text t) = "\"" <> t <> "\""

-- | Create a virtual DOM element, using a Hyperscript like interface.
h :: ∀ l v. String → Attrs → Array (VirtualNode l v) → VirtualNode l v
h name attrs children = Element {name, attrs, children, listeners: []}

-- | Create a virtual DOM text node.
text :: ∀ l v. String → VirtualNode l v
text = Text 

-- | A shorthand for making `Attrs`: turns a list of tuples into a `Attrs`
-- | value.
-- |
-- | Example:
-- |
-- |     prop [ "class" /\ "caturday"
-- |          , "href" /\ "http://caturday.tumblr.com/"
-- |          ]
prop :: Array (Tuple String String) → Attrs 
prop =  FO.fromFoldable


-- | Attach event listeners to a virtual DOM element.
-- |
-- | These will be installed on the actual DOM element the first time
-- | it's created, and only then.
with :: ∀ l v. VirtualNode l v → Array (EventListener l v) → VirtualNode l v
with (Element n) listeners = Element $ n {listeners = listeners}
with n _ = n



-- | An API interface for DOM operations. The `Data.VirtualDOM.DOM` module
-- | provides an implementation for the regular DOM. You could provide
-- | your own, for things like server side rendering.
type DOM  l v =
  { createElement :: String → Effect l
  , createElementNS :: String → String → Effect l
  , createTextNode :: String → Effect l
  , replaceChild :: l → l → l → Effect Unit
  , removeChild :: l → l → Effect Unit
  , appendChild :: l → l → Effect Unit
  , childCount :: l → Effect Int
  , childAt :: Int → l → Effect (Maybe l)
  , setTextContent :: String → l → Effect Unit
  , setAttribute :: String → String → l → Effect Unit
  , removeAttribute :: String → l → Effect Unit
  , addEventListener :: String → (v → Effect Unit) → l → Effect Unit
  }


asArray :: Attrs -> Array (Tuple String String) 
asArray = FO.toUnfoldable


createElement :: ∀ l v. DOM l v → VirtualNode l v → Effect l
createElement api (Element e) = do
  el ← api.createElement e.name
  sequence_ $ asArray e.attrs <#> \(Tuple k v) → api.setAttribute k v el
  sequence_ $ e.listeners <#> addListener api el
  sequence_ $ e.children <#> (createElement api >=> flip api.appendChild el) 
  pure el 
createElement api (Text t) = api.createTextNode t

addListener :: ∀ l v. DOM l v → l → EventListener l v → Effect Unit
addListener api target (On name handler) = api.addEventListener name handler target

changed :: ∀ l v. VirtualNode l v → VirtualNode l v → Boolean
changed (Element e1) (Element e2) = e1.name /= e2.name
changed (Text t1) (Text t2) = t1 /= t2
changed _ _ = true

updateAttrs :: ∀ l v. DOM l v → l → Attrs → Attrs → Effect Unit
updateAttrs api target old new =
  sequence_ (update <$> FO.keys (FO.union old new))
  where
    update key =
      case FO.lookup key old, FO.lookup key new of
        Nothing, Just value → api.setAttribute key value target
        Just _, Nothing → api.removeAttribute key target
        Just prev, Just next → when (prev /= next) $ api.setAttribute key next target
        Nothing, Nothing → pure unit



-- | Given a `DOM` interface, a target node, and two virtual DOM nodes,
-- | update the contents of the target node to reflect the differences
-- | between the virtual DOM nodes.
-- |
-- | To use this, call it the first time with `Nothing` as the old node.
-- | This will do nothing but create the new node. Then, each subsequent
-- | update, call it with the previous new node as the old node.
-- |
-- | An example, using `Signal`, and assuming a `Signal (VirtualNode l v)`
-- | as input, and `api` from `Data.VirtualDOM.DOM`:
-- |
-- |     render :: ∀ e. Node → Signal (VirtualNode e Node Event) → Effect (dom :: DOM | e) Unit
-- |     render target input =
-- |       runSignal $ withPrevious ~> patchDOM
-- |       where
-- |         withPrevious = foldp go (Tuple Nothing Nothing) input
-- |         go next (Tuple _ prev) = Tuple prev next
-- |         patchDOM (Tuple prev next) = patch api target prev next
patch :: ∀ l v. DOM l v → l → Maybe (VirtualNode l v) → Maybe (VirtualNode l v) → Effect Unit
patch api target' old' new' = patchIndexed target' old' new' 0
  where
    patchIndexed :: l → Maybe (VirtualNode l v) → Maybe (VirtualNode l v) → Int → Effect Unit
    patchIndexed _ Nothing Nothing _ = pure unit
    
    patchIndexed parent Nothing (Just new) _ = do
      el ← createElement api new
      api.appendChild el parent

    patchIndexed parent (Just _) Nothing index = do
      child ← api.childAt index parent
      case child of
        Just n → api.removeChild n parent
        Nothing → pure unit

    patchIndexed parent (Just (Text old)) (Just (Text new)) index =
      when (old /= new) do
        me ← api.childAt index parent
        maybe (pure unit) (\t → api.setTextContent new t) me

    patchIndexed parent (Just old) (Just new) index = do
      me' ← api.childAt index parent
      case me' of
        Nothing → pure unit
        Just me →
          if (changed old new) then do
            n ← createElement api new
            api.replaceChild n me parent
          else do
            case old, new of
              Element {attrs: oldAttrs}, Element {attrs: newAttrs} →
                updateAttrs api me oldAttrs newAttrs
              _, _ → pure unit
            walkChildren me old new

    walkChildren :: l → VirtualNode l v → VirtualNode l v → Effect Unit
    walkChildren target (Element old) (Element new) = do
        if (oldLength > newLength)
          then do
            walkIndexes (0 .. (newLength - 1)) -- walk up to last child of new
            walkIndexes ((oldLength - 1) .. newLength) -- delete children backwards from end
          else do
            walkIndexes (0 .. (newLength - 1))
      where
        walkIndexes = sequence_ <<< map (\i → patchIndexed target (old.children !! i) (new.children !! i) i)
        oldLength = length old.children
        newLength = length new.children
    walkChildren _ _ _ = pure unit