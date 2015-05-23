module DeanList.Server.Main where 

import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error(..), message)
import Debug.Trace (trace, print)
import Data.Array (map)
import Data.Foldable (foldl)
import Data.Function (Fn3(..))
import Data.Foreign.EasyFFI (unsafeForeignFunction)
import Data.JSON (JValue(JObject, JArray, JString), JObject(..), JArray(..), decode)
import Data.Map (lookup, toList)
import Data.Maybe (Maybe(Just, Nothing))
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

type Book = String

booksUrl = "http://localhost:3456/uri/URI%3ADIR2%3Arilot7zn3ycl6lmngkd5iab3pm%3Ahzko5cxtvwevu33qbyv72b3mn5tfzl7l7igllceqiax6akkc6clq/?t=json"

logger :: Handler
logger = do
    url <- getOriginalUrl
    liftEff $ trace url
    next
    
parseLafsToBooks :: String -> String
parseLafsToBooks s = show $ parseResponse json
  where
    json = decode s :: Maybe JArray
    parseResponse :: Maybe JArray -> String
    parseResponse (Just (_:dirInfo:_)) = parseDirInfo dirInfo
    parseResponse _ = "unparseable resp"
    parseDirInfo :: JValue -> String
    parseDirInfo (JObject dirInfo) = parseChildren $ lookup "children" dirInfo
    parseDirInfo _ = "unparseable dir"
    parseChildren :: Maybe JValue -> String
    parseChildren (Just (JObject j)) = foldl (\all s -> all ++ " " ++ s) "" $ map parseChild $ toList j
    parseChildren _ = "unparseable children"
    parseChild :: Tuple String JValue -> String
    parseChild (Tuple title (JArray j)) = title ++ " " ++ (parseBookData j)
    parseChild _ = "unparseable child"
    parseBookData :: JArray -> String
    parseBookData (_:(JObject fileInfo):_) = parseBookLink $ lookup "ro_uri" fileInfo
    parseBookData _ = "unparseable book data"
    parseBookLink :: Maybe JValue -> String
    parseBookLink (Just (JString s)) = s
    parseBookLink _ = "unparseable book link"
    
books :: Handler
books = do
  files <- liftEff $ readTextFile UTF8 "dummy_response2"
  send $ parseLafsToBooks files
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
    get "/books" books
    
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
