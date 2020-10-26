module Main where


import Data.Maybe
import Prelude

import Control.Apply (void)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Class.Console (log)
import Prelude (Unit, bind, map, ($), (>>=), (>>>))
import VDOM as V
import Web.DOM.Document as D
import Web.DOM.Element as E
import Web.DOM.Node (Node, appendChild)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Text (toNode) as T
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (toDocument, toNonElementParentNode)
import Web.HTML.Window (document)



data State = St {
  text:: String,
  value:: Int
}

data Action = NewText String | NewVal Int

type Props = { state:: State, dispatch:: Action -> Effect Unit }


initState :: State
initState = St { text: "some stuff eh?", value: 0}

dispatch:: State -> Action -> State
dispatch (St prev) (NewText str) = St $ prev { text = str }
dispatch (St prev) (NewVal v) = St $ prev { value = v }


vApp :: forall a. V.VNode a
vApp = V.createElement "div" [V.Attr "id" "vApp"] 
    [ V.createElement "span" [] 
      [ V.VTextNode "some text "]
    , V.createElement "hr" [] []
    , V.createElement "img" [V.Attr "src" "https://media.giphy.com/media/cuPm4p4pClZVC/giphy.gif"] []
    ] 


component :: forall a. Props -> V.VNode a
component {state, dispatch } = V.createElement "div" [V.Attr "id" "vApp2"] children 
    where
      (St st) = state
      words = split (Pattern " ") st.text
      children = map (\word -> V.createElement "p" [] [ V.VTextNode word ]) words




run :: forall a. Node -> V.VNode a -> State -> Effect Unit
run node prev state = do
  let updater = run node prev <<< dispatch state
  let app = component { state: initState, dispatch: updater }
  V.diff (Just prev) (Just app) node
  


main :: Effect Unit
main = do
  doc <- window >>= document
  maybeEl <- getElementById "root" $ toNonElementParentNode doc

  _ <- case maybeEl of 
    Just root -> do
      
      -- let up = component { state: initState, dispatch: dispatch initState }
      -- void $ V.mount vApp root
      -- node <- V.mount vApp root
      -- V.diff (Just vApp) (Just up) node
      _ <- V.mount vApp root
      pure unit
    Nothing -> void $ noRootElemError doc

  log "done"
  

noRootElemError :: HTMLDocument -> Effect Node
noRootElemError doc = do
    let d = toDocument doc
    el <- D.createElement "span" d
    text <- D.createTextNode "Could not find root element" d
    appendChild (T.toNode text) (E.toNode el)
    -- appendChild (E.toNode el) (D.toNode d)
    
