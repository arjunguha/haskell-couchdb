-- |Helps prevent injection attacks.  At the time of writing, there
-- is no official specification of the naming conventions.  So, this is
-- overly 
module Database.CouchDB.Safety 
  ( DB
  , db
  , isDBString
  , Doc
  , doc
  , isDocString
  ) where

import Data.List (elem)

-- |Database name
data DB = DB String

instance Show DB where
  show (DB s) = s

isDBChar ch = (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') 
    || (ch >= '0' && ch <= '9')

isFirstDocChar = isDBChar

isDocChar ch = (ch >= 'A' && ch <='Z') || (ch >= 'a' && ch <= 'z') 
  || (ch >= '0' && ch <= '9') || ch `elem` "@."

isDBString :: String -> Bool
isDBString [] =  False
isDBString s = and (map isDBChar s)

-- |Returns a safe database name.  Signals an error if the name is
-- invalid.
db :: String -> DB
db dbName =  case isDBString dbName of
  True -> DB dbName
  False -> error $ "db :  invalid dbName (" ++ dbName ++ ")"


-- |Document name
data Doc = Doc String

instance Show Doc where
  show (Doc s) = s

doc :: String -> Doc
doc docName = case isDocString docName of
  True -> Doc docName
  False -> error $ "doc : invalid docName (" ++ docName ++ ")"

isDocString :: String -> Bool
isDocString [] = False
isDocString (first:rest) = isFirstDocChar first && and (map isDocChar rest)
