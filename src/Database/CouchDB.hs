-- |Interface to CouchDB.
module Database.CouchDB 
  ( -- * Initialization
    CouchMonad
  , runCouchDB
  , runCouchDB'
  -- * Databases
  , DB
  , db
  , isDBString
  , createDB
  , dropDB
  -- * Documents
  , Doc
  , Rev
  , doc
  , isDocString
  , newNamedDoc
  , newDoc
  , updateDoc
  , deleteDoc
  , forceDeleteDoc
  , getDocPrim
  , getDoc
  , getAndUpdateDoc
  , getAllDocIds
  -- * Views
  -- $views
  , CouchView (..)
  , newView
  , queryView
  , queryViewKeys
  ) where

import Database.CouchDB.HTTP
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust,mapMaybe,maybeToList)
import Text.JSON
import Data.List (elem)
import Data.Maybe (mapMaybe)

import Database.CouchDB.Unsafe (CouchView (..))
import qualified Data.List as L
import qualified Database.CouchDB.Unsafe as U

-- |Database name
data DB = DB String

instance Show DB where
  show (DB s) = s

instance JSON DB where
  readJSON val = do
    s <- readJSON val
    case isDBString s of
      False -> fail "readJSON: not a valid database name"
      True -> return (DB s)

  showJSON (DB s) = showJSON s


isDBChar ch = (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') 
    || (ch >= '0' && ch <= '9')

isFirstDocChar = isDBChar

isDocChar ch = (ch >= 'A' && ch <='Z') || (ch >= 'a' && ch <= 'z') 
  || (ch >= '0' && ch <= '9') || ch `elem` "-@._"

isDBString :: String -> Bool
isDBString [] =  False
isDBString s = and (map isDBChar s)

-- |Returns a safe database name.  Signals an error if the name is
-- invalid.
db :: String -> DB
db dbName =  case isDBString dbName of
  True -> DB dbName
  False -> error $ "db :  invalid dbName (" ++ dbName ++ ")"

-- |Document revision number.
data Rev = Rev { unRev :: JSString } deriving (Eq,Ord)

instance Show Rev where
  show (Rev s) = fromJSString s

-- |Document name
data Doc = Doc { unDoc :: JSString } deriving (Eq,Ord)

instance Show Doc where
  show (Doc s) = fromJSString s

instance JSON Doc where
  readJSON (JSString s) | isDocString (fromJSString s) = return (Doc s)
  readJSON _ = fail "readJSON: not a valid document name"

  showJSON (Doc s) = showJSON s

instance Read Doc where
  readsPrec _ str = maybeToList (parseFirst str) where
    parseFirst "" = Nothing
    parseFirst (ch:rest) 
      | isFirstDocChar ch =
          let (chs',rest') = parseRest rest
            in Just (Doc $ toJSString $ ch:chs',rest)
      | otherwise = Nothing
    parseRest "" = ("","")
    parseRest (ch:rest) 
      | isDocChar ch =
          let (chs',rest') = parseRest rest
            in (ch:chs',rest')
      | otherwise =
          ("",ch:rest)

-- |Returns a safe document name.  Signals an error if the name is
-- invalid.
doc :: String -> Doc
doc docName = case isDocString docName of
  True -> Doc (toJSString docName)
  False -> error $ "doc : invalid docName (" ++ docName ++ ")"

isDocString :: String -> Bool
isDocString [] = False
isDocString (first:rest) = isFirstDocChar first && and (map isDocChar rest)



-- |Creates a new database.  Throws an exception if the database already
-- exists. 
createDB :: String -> CouchMonad ()
createDB = U.createDB

dropDB :: String -> CouchMonad Bool -- ^False if the database does not exist
dropDB = U.dropDB

newNamedDoc :: (JSON a)
            => DB -- ^database name
            -> Doc -- ^document name
            -> a -- ^document body
            -> CouchMonad (Either String Rev)
            -- ^Returns 'Left' on a conflict.
newNamedDoc dbName docName body = do
  r <- U.newNamedDoc (show dbName) (show docName) body
  case r of
    Left s -> return (Left s)
    Right rev -> return (Right $ Rev rev)

updateDoc :: (JSON a)
          => DB -- ^database
          -> (Doc,Rev) -- ^document and revision
          -> a -- ^ new value
          -> CouchMonad (Maybe (Doc,Rev)) 
updateDoc db (doc,rev) val = do
  r <- U.updateDoc (show db) (unDoc doc, unRev rev) val
  case r of
    Nothing -> return Nothing
    Just (_,rev) -> return $ Just (doc,Rev rev)


-- |Delete a doc by document identifier (revision number not needed).  This
-- operation first retreives the document to get its revision number.  It fails
-- if the document doesn't exist or there is a conflict.
forceDeleteDoc :: DB -- ^ database
               -> Doc -- ^ document identifier
               -> CouchMonad Bool
forceDeleteDoc db doc = U.forceDeleteDoc (show db) (show doc)

deleteDoc :: DB  -- ^database
          -> (Doc,Rev)
          -> CouchMonad Bool
deleteDoc db (doc,rev) = U.deleteDoc (show db) (unDoc doc,unRev rev)

newDoc :: (JSON a)
       => DB -- ^database name
      -> a       -- ^document body
      -> CouchMonad (Doc,Rev) -- ^ id and rev of new document
newDoc db body = do
  (doc,rev) <- U.newDoc (show db) body
  return (Doc doc,Rev rev)
    
getDoc :: (JSON a)
       => DB -- ^database name
       -> Doc -- ^document name
       -> CouchMonad (Maybe (Doc,Rev,a)) -- ^'Nothing' if the 
                                         -- doc does not exist
getDoc db doc = do
  r <- U.getDoc (show db) (show doc)
  case r of
    Nothing -> return Nothing
    Just (_,rev,val) -> return $ Just (doc,Rev rev,val)
-- |Gets a document as a raw JSON value.  Returns the document id,
-- revision and value as a 'JSObject'.  These fields are queried lazily,
-- and may fail later if the response from the server is malformed.
getDocPrim :: DB -- ^database name
           -> Doc -- ^document name
           -> CouchMonad (Maybe (Doc,Rev,[(String,JSValue)]))
           -- ^'Nothing' if the document does not exist.
getDocPrim db doc = do
  r <- U.getDocPrim (show db) (show doc)
  case r of
    Nothing -> return Nothing
    Just (_,rev,obj) -> return $ Just (doc,Rev rev,obj)

getAndUpdateDoc :: (JSON a)
                => DB -- ^database
                -> Doc -- ^document name
                -> (a -> IO a) -- ^update function
                -> CouchMonad (Maybe Rev) -- ^If the update succeeds,
                                          -- return the revision number
                                          -- of the result.
getAndUpdateDoc db docId fn = do
  r <- U.getAndUpdateDoc (show db) (show docId) fn
  case r of
    Nothing -> return Nothing
    Just rev -> return $ Just (Rev $ toJSString rev)

getAllDocIds ::DB -- ^database name
             -> CouchMonad [Doc]
getAllDocIds db = do
  allIds <- U.getAllDocIds (show db)
  return (map Doc allIds)

--
-- $views
-- Creating and querying views
--

newView :: String -- ^database name
        -> String -- ^view set name
        -> [CouchView] -- ^views
        -> CouchMonad ()
newView = U.newView

queryView :: (JSON a)
          => DB  -- ^database
          -> Doc  -- ^design
          -> Doc  -- ^view
          -> [(String, JSValue)] -- ^query parameters
          -- |Returns a list of rows.  Each row is a key, value pair.
          -> CouchMonad [(Doc, a)]
queryView db viewSet view args = do
  rows <- U.queryView (show db) (show viewSet) (show view) args
  return $ map (\(doc,val) -> (Doc doc,val)) rows

-- |Like 'queryView', but only returns the keys.  Use this for key-only
-- views where the value is completely ignored.
queryViewKeys :: DB  -- ^database
            -> Doc  -- ^design
            -> Doc  -- ^view
            -> [(String, JSValue)] -- ^query parameters
            -> CouchMonad [Doc]
queryViewKeys db viewSet view args = do
  rows <- U.queryViewKeys (show db) (show viewSet) (show view) args
  return $ map (Doc . toJSString) rows
