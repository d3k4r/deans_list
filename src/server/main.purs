module DeanList.Server.Main where 

import Debug.Trace (trace, print)
import Data.Foldable (foldl)
import Data.Function (Fn3(..))
import Data.Foreign.EasyFFI (unsafeForeignFunction)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error(..), message)
import Node.Express.Types (Request(..), Response(..), ExpressM(..))
import Node.Express.App (App(..), listenHttp, use, useExternal, useOnError, get)
import Node.Express.Handler (Handler(..), setStatus, sendJson, getOriginalUrl, next)
import Node.FS.Sync (readdir)
import Network.HTTP.Affjax (AJAX(..), affjax, defaultRequest)
import Network.HTTP.Method (Method(GET))
import Control.Monad.Eff (Eff(..))

foreign import staticMiddleware "var staticMiddleware = require('express').static"
    :: String -> Fn3 Request Response (ExpressM Unit) (ExpressM Unit)

type Book = String

booksUrl = "http://localhost:3456/uri/URI%3ADIR2%3Arilot7zn3ycl6lmngkd5iab3pm%3Ahzko5cxtvwevu33qbyv72b3mn5tfzl7l7igllceqiax6akkc6clq/?t=json"

logger :: Handler
logger = do
    url <- getOriginalUrl
    liftEff $ trace url
    next
    
books :: Handler
books = do
    files <- liftEff $ readdir "public/books"
    sendJson files
    
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
  unsafeHttpGet booksUrl onResponse
  listenHttp appSetup port \_ -> trace $ "Server running on localhost:" ++ show port
  where
    --onResponse :: forall eff. String -> Eff ( | eff) Unit
    onResponse s = trace ("response: " ++ s)

foreign import unsafeHttpGet
"""
function unsafeHttpGet(url) {
  return function(onResponse){
  return function() {
  var http = require('http');
  http.get(url, function(res) {
    console.log('Got response:\n' + res.statusCode);
    res.setEncoding('utf-8');
    res.on('data', function(chunk) {
      console.log('Got chunk, pass to ps');
      console.log('onResponse:', onResponse);
      onResponse(chunk)();
    });
  }).on('error', function(e) {
    console.log('Got error: ' + e.message);
  });
  };
  };
}""" :: forall eff. String -> (String -> Eff ( | eff) Unit) -> (Eff ( | eff) Unit)

--getBooks :: forall a. Aff (ajax :: AJAX | a) [String]
--getBooks = do
--  res <- affjax $ defaultRequest { url = booksUrl, method = GET }
--  liftEff $ print res.response
--  let booksOrError = readJSON res.response :: F [String]
--  return $ either (\e -> []) (\b -> b) booksOrError

--main = launchAff $ do
--  books <- getBooks
--  liftEff $ trace $ show books
