module DeanList.Book where

import Control.Alt ((<|>))
import Control.MonadPlus (guard)
import Data.Array (mapMaybe, (!!), length, map, filter)
import Data.JSON (JValue(..), JObject(..), JArray(..), ToJSON, FromJSON, encode, decode, object, (.:))
import Data.Map (lookup, toList)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(..), uncurry, fst)
import qualified Data.String.Regex as Regex
import qualified Data.String as Str
import qualified Data.Set as Set

data Book = Book {
  title :: String,
  author :: String,
  subtitle :: String,
  format :: String,
  image :: String,
  uri :: String
  }

makeBook :: String -> String -> String -> Maybe Book
makeBook fileName image uri = do
  matches <- Regex.match (Regex.regex "(.*) - (.*)\\.(.*)" Regex.noFlags) fileName
  guard $ (length matches) == 4
  title <- matches !! 1
  author <- matches !! 2
  format <- matches !! 3
  return $ Book {title: title, author: author, subtitle: "", format: format, image: image, uri: uri}

makeBookWithSubtitle :: String -> String -> String -> Maybe Book
makeBookWithSubtitle fileName image uri = do
  matches <- Regex.match (Regex.regex "(.*)_ (.*) - (.*)\\.(.*)" Regex.noFlags) fileName
  guard $ (length matches) == 5
  title <- matches !! 1
  subtitle <- matches !! 2
  author <- matches !! 3
  format <- matches !! 4
  return $ Book {title: title, author: author, subtitle: subtitle, format: format, image: image, uri: uri}

instance bookFromJSON :: FromJSON Book where
  parseJSON (JObject o) = do
    title <- o .: "title"
    author <- o .: "author"
    subtitle <- o .: "subtitle"
    format <- o .: "format"
    image <- o .: "image"
    uri <- o .: "uri"
    return $ Book {title: title, author: author, subtitle: subtitle, format: format, image: image, uri: uri}

instance bookToJSON :: ToJSON Book where
  toJSON (Book b) = object [
    Tuple "title" (JString b.title),
    Tuple "author" (JString b.author),
    Tuple "subtitle" (JString b.subtitle),
    Tuple "format" (JString b.format),
    Tuple "image" (JString b.image),
    Tuple "uri" (JString b.uri)
    ]

asObject :: JValue -> Maybe JObject
asObject (JObject j) = (Just j)
asObject _ = Nothing

asString :: JValue -> Maybe String
asString (JString s) = (Just s)
asString _ = Nothing

stringEndsWith :: String -> String -> Boolean
stringEndsWith end str = (Str.indexOf end str) == ((Str.length str) - (Str.length end))

isFileOfType :: String -> String -> Boolean
isFileOfType ext filename = stringEndsWith ("." ++ ext) filename

isBookFormat :: String -> Boolean
isBookFormat filename = (isFileOfType "epub" filename) || (isFileOfType "pdf" filename)

isImageFormat :: String -> Boolean
isImageFormat filename = isFileOfType "jpg" filename

parseBook :: Set.Set String -> String -> JValue -> Maybe Book
parseBook imageFiles fileName (JArray bookInfoArray) = do
  guard $ isBookFormat fileName
  fileInfo <- (bookInfoArray !! 1)
  fileInfoObj <- asObject fileInfo
  bookUri <- lookup "ro_uri" fileInfoObj
  bookUri' <- asString bookUri
  imageName <- Just $ if (Set.member imageName imageFiles) then imageName else ""
  book <- makeBookWithSubtitle fileName imageName bookUri' <|> makeBook fileName imageName bookUri'
  return book
  where
    imageName = Str.replace "epub" "jpg" fileName
parseBook _ _ _ = Nothing
    
parseBooks :: String -> Maybe [Book]
parseBooks response = do
  json <- decode response :: Maybe JArray
  dirInfo <- (json !! 1)
  dirInfoObj <- asObject dirInfo
  children <- lookup "children" dirInfoObj
  childrenObj <- asObject children
  childrenList <- Just $ toList childrenObj
  imageFiles <- Just $ Set.fromList $ filter isImageFormat $ map fst childrenList
  books <- Just $ mapMaybe (uncurry $ parseBook imageFiles) childrenList
  return books
