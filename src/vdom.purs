module VDOM where

import Data.Tuple

import Control.Apply (void, (<$>))
import Data.Array (difference, length, replicate, zipWith)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Traversable (sequence_, traverse_)
import Effect (Effect)
import Prelude (class Eq, class Show, Unit, bind, discard, flap, flip, map, otherwise, pure, show, unit, ($), (&&), (-), (/=), (<<<), (<=), (<>), (==), (>>=), (>>>))
import Web.DOM.ChildNode (remove)
import Web.DOM.Document as D
import Web.DOM.Element (Element, fromNode, removeAttribute, setAttribute, toChildNode, toNode)
import Web.DOM.Node (Node, appendChild, childNodes, parentNode, replaceChild)
import Web.DOM.NodeList (toArray)
import Web.DOM.Text (toNode) as T
import Web.Event.Internal.Types (Event)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)


type Props = Array Attribute
type EventListener a = Event -> Effect a
type Children a = Array (VNode a)


data Attribute = Attr String String
data VNode a = VNode 
    { tag :: String
    , props :: Props
    , listeners :: Array (EventListener a)
    , children :: Array (VNode a)
    } 
    | VTextNode String


instance showAttr :: Show Attribute where
    show (Attr key val) = key <> " = " <> val 

instance showVNode :: Show (VNode a) where
    show (VTextNode text) = "\n{ VTextNode: " <> text <> " }\n"
    show (VNode { tag, props, children }) = "\n{\n\tVNode: " <> tag 
                                <> "\n\tProps: " <> show props
                                <> "\n\tChildren: " <> show children <> "\n}\n"



instance eqAttribute :: Eq Attribute where
    eq (Attr a1 b1) (Attr a2 b2) = a1 == a2 && b1 == b2 


mount :: forall a. VNode a -> Element -> Effect Node  
mount app root = render app >>= appendTo root


render :: forall a. VNode a -> Effect Node
render (VTextNode text) = toTextNode text
render n@(VNode { tag, props, children }) = do
    el <- toElement tag

    traverse_ (setAttr el) props
    traverse_ (\child -> render child >>= appendTo el)  children

    pure $ toNode el
  

diff :: forall a. Maybe (VNode a) -> Maybe (VNode a) -> Node -> Effect Unit
diff _ Nothing = removeNode
diff (Just (VTextNode oldText)) (Just new@(VTextNode newText))
    | oldText == newText = pure >>> void
    | otherwise = replaceWith new

diff (Just o@(VNode old)) (Just n@(VNode new))  
    | old.tag /= new.tag = replaceWith n
    | otherwise = \node -> do
        let (Tuple oldChildren newChildren) = normalizeLength old.children new.children     
        _ <- diffAttrs old.props new.props node
        children <- childNodes node >>= toArray
        sequence_ $ (diff <$> oldChildren <*> newChildren <*> children)

diff _ (Just new) = appendNewNode new


diffAttrs :: Props -> Props -> Node -> Effect Unit
diffAttrs old new =
    let 
        additions = map (patch setAttr) new
        deletions = map (patch removeAttr) $ difference old new
    in 
        sequence_ <<< flap (deletions <> additions)
    where 
        patch action attr node = case fromNode node of
            Just el -> action el attr
            Nothing -> pure unit



-- UTILITIES --

createElement :: forall a. String -> Props -> Children a -> VNode a
createElement tag props children = VNode
    { tag
    , props
    , children
    , listeners: []
    }
      
getDocument :: Effect D.Document
getDocument = window >>= document >>= pure <<< toDocument

toElement :: String -> Effect Element
toElement tag = getDocument >>= D.createElement tag

toTextNode :: String -> Effect Node
toTextNode text = getDocument >>= D.createTextNode text >>= pure <<< T.toNode 

setAttr :: Element -> Attribute -> Effect Unit
setAttr el (Attr key val) = setAttribute key val el

removeAttr :: Element -> Attribute -> Effect Unit
removeAttr el (Attr key _) = removeAttribute key el

appendTo :: Element -> Node -> Effect Node
appendTo = flip appendChild <<< toNode

removeNode :: Node -> Effect Unit
removeNode = fromNode >>> case _ of
    Nothing -> pure unit
    Just el -> remove $ toChildNode el 

appendNewNode :: forall a. VNode a -> Node -> Effect Unit
appendNewNode vChild parent = void do
    child <- render vChild
    appendChild child parent

replaceWith :: forall a. VNode a -> Node -> Effect Unit
replaceWith new old = do
    newNode <- render new
    parent <- parentNode old
    case parent of
        Just node -> void $ replaceChild newNode old node
        Nothing -> pure unit
    
normalizeLength :: forall a. Array a -> Array a -> Tuple (Array (Maybe a)) (Array (Maybe a))
normalizeLength a b = 
    if sizeDiff <= 0
        then Tuple justA (justB <> replicate n Nothing)
        else Tuple (justA <> replicate n Nothing) justB
    where
      sizeDiff = length a - length b
      n = abs sizeDiff
      justA = map Just a
      justB = map Just b


zipApply :: forall a b. Array (a -> b) -> Array a -> Array b
zipApply = zipWith ($)
infixl 4 zipApply as <*>