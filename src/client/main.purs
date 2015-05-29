module DeanList.Client.Main where

import DeanList.Client.Html (div, pureUnit, h1, a)

import qualified VirtualDOM as VDom
import qualified Control.Monad.ST as ST

import Debug.Trace (trace)
import Data.Array (map, (..))
import Data.Either (Either(Left, Right), either)
import Data.JSON (FromJSON, JValue(JObject), eitherDecode, (.:))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import VirtualDOM.VTree (VTree(..), vtext, vnode)
import DOM (DOM(..), Node(..))
import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff(..), launchAff)
import Network.HTTP.Affjax (AJAX(..), affjax, defaultRequest)
import Network.HTTP.Method (Method(GET))

data Book = Book {title :: String, url :: String}

instance bookFromJSON :: FromJSON Book where
  parseJSON (JObject o) = do
    title <- o .: "title"
    url <- o .: "url"
    return $ Book { title: title, url: url }

type AppState = { books :: [Book] }
type UiState = { state :: AppState, virtual :: VTree, dom :: Node }

render :: AppState -> VTree
render state = div {className: "container pure-g"} grids
  where
    grids = [h1 {} [], pureUnit "1-5" [], pureUnit "3-5" [title, bookList], pureUnit "1-5" []]
    title = h1 {className: "title"} [vtext "Dean's List"]
    bookList = div {} bookItems
    bookItems = map (\(Book b) -> div {className: "book"} [a {href: b.url} [vtext b.title]]) state.books

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

getBooks :: forall a. Aff (ajax :: AJAX | a) [Book]
getBooks = do
  res <- affjax $ defaultRequest { url = "books/", method = GET }
  let books = eitherDecode res.response
  return $ either (\e -> [Book {title: "failed: " ++ (show e), url: ""}]) (\s -> s) books

main = launchAff $ do
  books <- getBooks
  let uiState = initUiState {books: books}
  uiStateRef <- liftEff $ ST.newSTRef uiState
  return $ unsafeAppendToBody uiState.dom
  
