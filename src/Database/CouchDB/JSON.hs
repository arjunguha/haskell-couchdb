-- |Convenient functions for parsing JSON responses.  Use these
-- functions to write the 'readJSON' method of the 'JSON' class.
module Database.CouchDB.JSON
  ( jsonString
  , jsonInt
  , jsonObject
  , jsonField
  , jsonBool
  , jsonIsTrue
  ) where

import Text.JSON
import Data.Ratio (numerator,denominator)

jsonString :: JSValue -> Result String
jsonString (JSString s) = return (fromJSString s)
jsonString _ = fail "expected a string"

jsonInt :: (Integral n) => JSValue -> Result n
jsonInt (JSRational r) = case (numerator r, denominator r) of
  (n,1) -> return (fromIntegral n)
  otherwise -> fail "expected an integer; got a rational"
jsonInt _ = fail "expected an integer"

jsonObject :: JSValue -> Result [(String,JSValue)]
jsonObject (JSObject obj) = return (fromJSObject obj)
jsonObject v = fail $ "expected an object, got " ++ (show v)

jsonBool :: JSValue -> Result Bool
jsonBool (JSBool b) = return b
jsonBool v = fail $ "expected a boolean value, got " ++ show v

-- |Extract a field as a value of type 'a'.  If the field does not
-- exist or cannot be parsed as type 'a', fail.
jsonField :: JSON a => String -> [(String,JSValue)] -> Result a
jsonField field obj = case lookup field obj of
  Just v -> readJSON v
  Nothing -> fail $ "could not find the field " ++ field

-- |'True' when the field is defined and is true.  Otherwise, 'False'.
jsonIsTrue :: String -> [(String,JSValue)] -> Result Bool
jsonIsTrue field obj = case lookup field obj of
  Just (JSBool True) -> return True
  otherwise -> return False
