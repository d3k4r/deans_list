module DeanList.Client.Main where

import Debug.Trace (trace)
import Data.Array (map, (..))
import Data.Either (Either(Left, Right))
import Data.Foreign (F(..))
import Data.Foreign.Class (readJSON)
import qualified VirtualDOM as VDom
import VirtualDOM.VTree (VTree(..), vtext, vnode)
import DOM (DOM(..), Node(..))
import qualified Control.Monad.ST as ST
import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff(..), launchAff)
import Network.HTTP.Affjax (AJAX(..), affjax, defaultRequest)
import Network.HTTP.Method (Method(GET))

type Book = String
type AppState = { books :: [Book] }
type UiState = { state :: AppState, virtual :: VTree, dom :: Node }

render :: AppState -> VTree
render state = vnode "div" {className: "container"} [title, bookList]
  where
    title = vnode "h1" {} [vtext "Dean's List"]
    bookList = vnode "ul" {} bookItems
    bookItems = map (\b -> vnode "li" {} [vnode "a" {href: b} [vtext b]]) state.books

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

parseBooks :: F [Book] -> [Book]
parseBooks (Left _) = []
parseBooks (Right books) = books

getBooks :: forall a. Aff (ajax :: AJAX | a) [Book]
getBooks = do
  res <- affjax $ defaultRequest { url = "/books", method = GET }
  return $ parseBooks (readJSON res.response :: F [Book])

main = launchAff $ do
  books <- getBooks
  let uiState = initUiState {books: books}
  uiStateRef <- liftEff $ ST.newSTRef uiState
  return $ unsafeAppendToBody uiState.dom
  
