module DeanList.Server.Main where 

import Control.Apply (lift2)
import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error(..), message)
import Control.MonadPlus (guard)
import Debug.Trace (trace, print)
import Data.Array (map, catMaybes)
import Data.Foldable (foldl)
import Data.Function (Fn3(..))
import Data.Foreign.EasyFFI (unsafeForeignFunction)
import Data.JSON (JValue(..), JObject(..), JArray(..), ToJSON, encode, decode, object, Pair(..))
import Data.Map (Map(..), lookup, toList, fromList)
import Data.Maybe (Maybe(Just, Nothing), maybe, isJust)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX(..), affjax, defaultRequest)
import Network.HTTP.Method (Method(GET))
import Node.Encoding (Encoding(UTF8))
import Node.Express.App (App(..), listenHttp, use, useExternal, useOnError, get)
import Node.Express.Handler (Handler(..), setStatus, send, sendJson, getOriginalUrl, next, capture)
import Node.Express.Types (Request(..), Response(..), ExpressM(..))
import Node.FS.Sync (readdir, readTextFile)

foreign import staticMiddleware "var staticMiddleware = require('express').static"
    :: String -> Fn3 Request Response (ExpressM Unit) (ExpressM Unit)

data Book = Book {title :: String, url :: String}

instance bookToJSON :: ToJSON Book where
  toJSON (Book b) = object [Tuple "title" (JString b.title), Tuple "url" (JString b.url)]
  
book :: String -> String -> Book
book title url = Book {title: title, url: url}

booksUrl = "http://localhost:3456/uri/URI%3ADIR2%3Arilot7zn3ycl6lmngkd5iab3pm%3Ahzko5cxtvwevu33qbyv72b3mn5tfzl7l7igllceqiax6akkc6clq/?t=json"

logger :: Handler
logger = do
    url <- getOriginalUrl
    liftEff $ trace url
    next
    
parseBooks :: String -> [Book]
parseBooks dirInfoResponse = maybe [] parseResponse maybeJson
  where
    maybeJson = decode dirInfoResponse :: Maybe JArray
    parseResponse :: JArray -> [Book]
    parseResponse (_:dirInfo:_) = parseDirInfo dirInfo
    parseResponse _ = []
    parseDirInfo :: JValue -> [Book]
    parseDirInfo (JObject dirInfo) = parseChildren $ lookup "children" dirInfo
    parseDirInfo _ = []
    parseChildren :: Maybe JValue -> [Book]
    parseChildren (Just (JObject j)) = catMaybes $ map parseChild $ toList j
    parseChildren _ = []
    parseChild :: Tuple String JValue -> Maybe Book
    parseChild (Tuple title (JArray j)) = book <$> (Just title) <*> (parseBookUrl j)
    parseChild _ = Nothing
    parseBookUrl :: JArray -> Maybe String
    parseBookUrl (_:(JObject fileInfo):_) = maybeUrl $ lookup "ro_uri" fileInfo
    parseBookUrl _ = Nothing
    maybeUrl :: Maybe JValue -> Maybe String
    maybeUrl (Just (JString s)) = Just s
    maybeUrl _ = Nothing
    
getBooks :: Handler
getBooks = do
  dirInfo <- liftEff $ readTextFile UTF8 "dummy_response2"
  sendJson $ encode $ parseBooks dirInfo
    -- callback <- capture (\s -> sendJson $ parseLafsToBooks s)
    -- liftEff $ unsafeHttpGet booksUrl callback
    
errorHandler :: Error -> Handler
errorHandler err = do
    setStatus 400
    sendJson {error: message err}
            
appSetup :: App
appSetup = do
    use logger
    useExternal $ staticMiddleware "public"
    useOnError errorHandler
    get "/books" getBooks
    
main = do
  port <- unsafeForeignFunction [""] "process.env.PORT || 8080"
  listenHttp appSetup port \_ -> trace $ "Server running on localhost:" ++ show port

foreign import unsafeHttpGet
"""
function unsafeHttpGet(url) {
  return function(onResponse){
  return function() {
    var request = require('request');
    request(url, function(error, response, body){
      console.log('Got body');
      console.log(body);
      onResponse(body)();
    });
  }
  }
}""" :: forall eff. String -> (String -> Eff ( | eff) Unit) -> Eff ( | eff) Unit
