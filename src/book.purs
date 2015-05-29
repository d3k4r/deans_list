module DeanList.Book where

import Data.Array (mapMaybe, (!!))
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
