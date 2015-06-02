module DeanList.Book where

import Control.MonadPlus (guard)
import Data.Array (mapMaybe, (!!))
import Data.String (indexOf, length)
import Data.JSON (JValue(..), JObject(..), JArray(..), ToJSON, FromJSON, encode, decode, object, (.:))
import Data.Map (lookup, toList)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(..), uncurry)

data Book = Book {title :: String, url :: String}

instance bookFromJSON :: FromJSON Book where
  parseJSON (JObject o) = do
    title <- o .: "title"
    url <- o .: "url"
    return $ Book { title: title, url: url }

instance bookToJSON :: ToJSON Book where
  toJSON (Book b) = object [Tuple "title" (JString b.title), Tuple "url" (JString b.url)]

asObject :: JValue -> Maybe JObject
asObject (JObject j) = (Just j)
asObject _ = Nothing

asString :: JValue -> Maybe String
asString (JString s) = (Just s)
asString _ = Nothing

stringEndsWith :: String -> String -> Boolean
stringEndsWith end str = (indexOf end str) == ((length str) - (length end))

isFileOfType :: String -> String -> Boolean
isFileOfType ext filename = stringEndsWith ("." ++ ext) filename

isBookFormat :: String -> Boolean
isBookFormat filename = (isFileOfType "epub" filename) || (isFileOfType "pdf" filename)

parseBook :: String -> JValue -> Maybe Book
parseBook fileName (JArray bookInfoArray) = do
  guard $ isBookFormat fileName
  fileInfo <- (bookInfoArray !! 1)
  fileInfoObj <- asObject fileInfo
  bookUrl <- lookup "ro_uri" fileInfoObj
  bookUrl' <- asString bookUrl
  return $ Book {title: fileName, url: bookUrl'}
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
