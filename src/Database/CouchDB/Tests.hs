module Database.CouchDB.Tests where

import Test.HUnit
import Database.CouchDB

assertDBEqual :: (Eq a, Show a) => String -> a -> CouchMonad a -> Assertion
assertDBEqual msg v m = do
  v' <- runCouchDB' m
  assertEqual msg v' v

testCreate = TestCase $ assertDBEqual "create/drop database" True $ do
  createDB "test1"
  dropDB "test1" -- returns True since the database exists.

allTests = TestList [ testCreate ]

main = do
  putStrLn "Running CouchDB test suite..."
  runTestTT allTests
  putStrLn "Testing complete."
  return ()
