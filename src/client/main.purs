module DeanList.Client.Main where

import Data.Array (map, (..))
import qualified VirtualDOM as VDom
import VirtualDOM.VTree (VTree(..), vtext, vnode)
import DOM (DOM(..), Node(..))
import qualified Control.Monad.ST as ST
import Control.Monad.Eff (Eff(..))

type Book = { path :: String }
type AppState = { books :: [Book] }
type UiState = { state :: AppState, virtual :: VTree, dom :: Node }

initAppState :: AppState
initAppState = { books: [
    { path: "./books/away_from_sequential_tarpit.pdf" },
    { path: "./books/cspbook.pdf" }
  ]}

render :: AppState -> VTree
render state = vnode "div" {className: "container"} [title, bookList]
  where
    title = vnode "h1" {} [vtext "Dean's List"]
    bookList = vnode "ul" {} bookItems
    bookItems = map (\b -> vnode "li" {} [vnode "a" {href: b.path} [vtext b.path]]) state.books

initUiState :: AppState -> UiState
initUiState state = { state: state, virtual: virtual, dom: dom }
  where
    virtual = render state
    dom = VDom.createElement virtual 
    
patchUiState :: forall e. UiState -> AppState -> Eff (dom :: DOM | e) UiState
patchUiState uiState appState = do 
  dom <- VDom.patch diff uiState.dom
  return { state: appState, virtual: newVirtual, dom: dom }
  where
    newVirtual = render appState
    diff = VDom.diff uiState.virtual newVirtual

updateState :: forall a b. ST.STRef a UiState -> (AppState -> AppState) -> Eff (st :: ST.ST a, dom :: DOM | b) Unit
updateState uiStateRef updateAppState = do 
  uiState <- ST.readSTRef uiStateRef
  newUiState <- patchUiState uiState $ updateAppState uiState.state
  newUiStateRef <- ST.writeSTRef uiStateRef newUiState
  return unit

foreign import unsafeAppendToBody
  """
  function unsafeAppendToBody(child) {
    document.body.appendChild(child);
  }""" :: forall eff. Node -> (Eff (dom :: DOM | eff) Unit)
  
main :: forall a b c. Eff (dom :: DOM, st :: ST.ST a | b) (Eff (dom :: DOM | c) Unit)
main = do
  uiStateRef <- ST.newSTRef uiState
  return $ unsafeAppendToBody uiState.dom
  where
    uiState = initUiState initAppState
  
