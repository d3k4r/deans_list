module DeanList.Server.Main where 

import Control.Apply (lift2)
import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error(..), message)
import Control.MonadPlus (guard)
import Debug.Trace (trace, print)
import Data.Array (map, catMaybes, mapMaybe, (!!))
import Data.Foldable (foldl)
import Data.Function (Fn3(..))
import Data.Foreign.EasyFFI (unsafeForeignFunction)
import Data.JSON (JValue(..), JObject(..), JArray(..), ToJSON, encode, decode, object, Pair(..))
import Data.Map (Map(..), lookup, toList, fromList)
import Data.Maybe (Maybe(Just, Nothing), maybe, isJust, fromMaybe)
import Data.Tuple (Tuple(..), uncurry)
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

booksUrl = "http://localhost:3456/uri/URI%3ADIR2%3Arilot7zn3ycl6lmngkd5iab3pm%3Ahzko5cxtvwevu33qbyv72b3mn5tfzl7l7igllceqiax6akkc6clq/?t=json"

asObject :: JValue -> Maybe JObject
asObject (JObject j) = (Just j)
asObject _ = Nothing

asString :: JValue -> Maybe String
asString (JString s) = (Just s)
asString _ = Nothing

logger :: Handler
logger = do
    url <- getOriginalUrl
    liftEff $ trace url
    next
    
parseBook :: String -> JValue -> Maybe Book
parseBook title (JArray bookInfoArray) = do
  fileInfo <- (bookInfoArray !! 1)
  fileInfoObj <- asObject fileInfo
  bookUrl <- lookup "ro_uri" fileInfoObj
  bookUrl' <- asString bookUrl
  return $ Book {title: title, url: bookUrl'}
parseBook _ _ = Nothing
    
parseBooks :: String -> Maybe [Book]
parseBooks response = do
  json <- decode response :: Maybe JArray
  dirInfo <- (json !! 1)
  dirInfoObj <- asObject dirInfo
  children <- lookup "children" dirInfoObj
  childrenObj <- asObject children
  childrenList <- Just $ toList childrenObj
  books <- Just $ mapMaybe (uncurry parseBook) childrenList
  return books
    
getBooks :: Handler
getBooks = do
  dirInfo <- liftEff $ readTextFile UTF8 "dummy_response2"
  let books = fromMaybe [] $ parseBooks dirInfo
  sendJson $ encode books
    
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
