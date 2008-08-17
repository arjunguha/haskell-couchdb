module Database.CouchDB.JSON
  ( jsonString
  , jsonInt
  , jsonObject
  , jsonField
  , jsonBool
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

jsonField field obj = case lookup field obj of
  Just v -> return v
  Nothing -> fail $ "could not find the field " ++ field

