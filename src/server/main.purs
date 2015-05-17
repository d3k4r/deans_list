module DeanList.Server.Main where 

import Debug.Trace (trace)
import Data.Foldable (foldl)
import Data.Function (Fn3(..))
import Data.Foreign.EasyFFI (unsafeForeignFunction)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error(..), message)
import Node.Express.Types (Request(..), Response(..), ExpressM(..))
import Node.Express.App (App(..), listenHttp, use, useExternal, useOnError, get)
import Node.Express.Handler (Handler(..), setStatus, sendJson, getOriginalUrl, next)
import Node.FS.Sync (readdir)

foreign import staticMiddleware "var staticMiddleware = require('express').static"
    :: String -> Fn3 Request Response (ExpressM Unit) (ExpressM Unit)

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
  listenHttp appSetup port \_ -> trace $ "Server running on localhost:" ++ show port
