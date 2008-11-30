{-# OPTIONS_GHC -XFlexibleInstances #-}
module Database.CouchDB.Tests ( main, allTests) where

import Test.HUnit
import Database.CouchDB
import Database.CouchDB.JSON
import Text.JSON

-- ----------------------------------------------------------------------------
-- Helper functions
--

assertDBEqual :: (Eq a, Show a) => String -> a -> CouchMonad a -> Assertion
assertDBEqual msg v m = do
  v' <- runCouchDB' m
  assertEqual msg v' v

instance Assertable (Either String a) where
  assert (Left s) = assertFailure s
  assert (Right _) = return ()

assertRight :: (Either String a) -> IO a
assertRight (Left s) = assertFailure s >> fail "assertion failed"
assertRight (Right a) = return a

instance Assertable (Maybe a) where
  assert Nothing = assertFailure "expected (Just ...), got Nothing"
  assert (Just a) = return ()

assertJust :: Maybe a -> IO a
assertJust (Just v) = return v
assertJust Nothing = do
  assertFailure "expected (Just ...), got Nothing"
  fail "assertion failed"

main = do
  putStrLn "Running CouchDB test suite..."
  runTestTT allTests
  putStrLn "Testing complete."
  return ()

-- -----------------------------------------------------------------------------
-- Data definitions for testing
--

data Age = Age
  { ageName :: String
  , ageValue :: Int
  } deriving (Eq,Show)

instance JSON Age where

  showJSON (Age name val) = JSObject $ toJSObject
    [ ("name", showJSON name)
    , ("age", showJSON val)
    ]

  readJSON val = do
    obj <- jsonObject val
    name <- jsonField "name" obj
    age <- jsonField "age" obj
    return (Age name age)

-- ----------------------------------------------------------------------------
-- Test cases
--


testCreate = TestCase $ assertDBEqual "create/drop database" True $ do
  createDB "test1"
  dropDB "test1" -- returns True since the database exists.

people = [ Age "Arjun" 18, Age "Alex" 17 ]

testNamedDocs = TestCase $ assertDBEqual "add named documents" people $ do
  createDB "test1"
  let mydb = db "test1"
  newNamedDoc mydb (doc "arjun") (people !! 0)
  newNamedDoc mydb (doc "alex") (people !! 1)
  Just (_,_,v1) <- getDoc mydb (doc "arjun")
  Just (_,_,v2) <- getDoc mydb (doc "alex") 
  dropDB "test1"
  return [v1, v2]
  


allTests = TestList [ testCreate, testNamedDocs ]

