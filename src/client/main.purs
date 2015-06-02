module DeanList.Client.Main where

import DeanList.Book (Book(Book))
import DeanList.Client.Html (div, pureUnit, h1, a)

import qualified VirtualDOM as VDom
import qualified Control.Monad.ST as ST
import qualified Data.String as Str
import qualified Data.String.Regex as Regex

import Debug.Trace (trace)
import Data.Array (map, (..), (!!))
import Data.Either (either)
import Data.JSON (eitherDecode)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import VirtualDOM.VTree (VTree(..), vtext, vnode)
import DOM (DOM(..), Node(..))
import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff(..), launchAff)
import Network.HTTP.Affjax (AJAX(..), affjax, defaultRequest)
import Network.HTTP.Method (Method(GET))

type AppState = { books :: [Book] }
type UiState = { state :: AppState, virtual :: VTree, dom :: Node }

formatTrailingTheTitle :: String -> Maybe String
formatTrailingTheTitle t = do
  matches <- Regex.match (Regex.regex "^(.*), The$" Regex.noFlags) t
  mainTitle <- matches !! 1
  return $ "The " ++ mainTitle

formatBookTitle :: String -> String
formatBookTitle t = fromMaybe t (formatTrailingTheTitle t)

formatBookLink :: Book -> String
formatBookLink (Book b) = if Str.null b.subtitle
  then formattedTitle ++ " (" ++ b.author ++ ")"
  else formattedTitle ++ " - " ++ b.subtitle ++ " (" ++ b.author ++ ")"
  where
    formattedTitle = formatBookTitle b.title

render :: AppState -> VTree
render state = div {className: "container pure-g"} grids
  where
    grids = [h1 {} [], pureUnit "1-5" [], pureUnit "3-5" [title, bookList], pureUnit "1-5" []]
    title = h1 {className: "title"} [vtext "Dean's List"]
    bookList = div {} bookItems
    bookItems = map (\(Book b) -> div {className: "book"} [a {href: b.uri} [vtext $ formatBookLink (Book b)]]) state.books

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
  return $ either (\e -> []) (\s -> s) books

main = launchAff $ do
  books <- getBooks
  let uiState = initUiState {books: books}
  uiStateRef <- liftEff $ ST.newSTRef uiState
  return $ unsafeAppendToBody uiState.dom
  
