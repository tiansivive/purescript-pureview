module Main where


import Data.Maybe

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


vApp :: forall a. V.VNode a
vApp = V.createElement "div" [V.Attr "id" "vApp"] 
    [ V.createElement "span" [] 
      [ V.VTextNode "some text "]
    , V.createElement "hr" [] []
    , V.createElement "img" [V.Attr "src" "https://media.giphy.com/media/cuPm4p4pClZVC/giphy.gif"] []
    ] 


newVApp :: forall a. String -> V.VNode a
newVApp = split (Pattern " ") 
  >>> map (\word -> V.createElement "p" [] [ V.VTextNode word ]) 
  >>> V.createElement "div" [V.Attr "id" "vApp2"]

  
noRootElemError :: HTMLDocument -> Effect Node
noRootElemError doc = do
    let d = toDocument doc
    el <- D.createElement "span" d
    text <- D.createTextNode "Could not find root element" d
    appendChild (T.toNode text) (E.toNode el)
    -- appendChild (E.toNode el) (D.toNode d)
    


main :: Effect Unit
main = do
  doc <- window >>= document
  maybeEl <- getElementById "root" $ toNonElementParentNode doc

  _ <- case maybeEl of 
    Just root -> do
      let update = newVApp "Will this work?"
      -- void $ V.mount vApp root
      node <- V.mount vApp root
      V.diff (Just vApp) (Just update) node
    Nothing -> void $ noRootElemError doc

  log "done"
  


-- test :: Array Int -> Array Int -> Array (Array Int)
-- test old new = do
--   o <- old
