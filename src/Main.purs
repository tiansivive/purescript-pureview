module Main where


import Component as PV
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import HTML as HTML
import Prelude (Unit, bind, pure, show, unit, void, ($), (+), (-), (>>=))
import VDOM as V
import Web.DOM.Document as D
import Web.DOM.Element as E
import Web.DOM.Node (appendChild)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Text (toNode) as T
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (toDocument, toNonElementParentNode)
import Web.HTML.Window (document)






vApp :: forall a. V.VirtualNode a
vApp = V.createElement "div" [V.Attr "id" "vApp"] 
    [ V.createElement "span" [] 
      [ V.Text "some text "]
    , V.createElement "hr" [] []
    , V.createElement "img" [V.Attr "src" "https://media.giphy.com/media/cuPm4p4pClZVC/giphy.gif"] []
    ] 

data Actions = Increment | Decrement
counter :: forall p l e. PV.Component p Int Actions e l
counter = PV.C {
  initialState: \_ -> 0,
  handlers: [],
  update,
  render
} 
  where
    update Increment = (+) 1
    update Decrement = (-) 1

    --render :: p -> Int -> PV.Handlers e -> V.VirtualNode l
    render _ n _ =
      HTML.div [V.Attr "class" "counter"] [
        HTML.button [V.Handler "on-click" $ Increment] [V.Text "+1"]
        HTML.button [V.Handler "on-click" $ Decrement] [V.Text "-1"]
        HTML.span [] [V.Text $ show n]
      ]




componentApp :: forall a. V.VirtualNode a
componentApp = 
  HTML.div [HTML.Attr "id" "root"] [
    PV.view counter [] [] 
  ]


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
      _ <- V.mount componentApp root
      pure unit
    Nothing -> void $ noRootElemError doc

  log "done"
  

noRootElemError :: HTMLDocument -> Effect Unit
noRootElemError doc = do
    let d = toDocument doc
    el <- D.createElement "span" d
    text <- D.createTextNode "Could not find root element" d
    appendChild (T.toNode text) (E.toNode el)

    
