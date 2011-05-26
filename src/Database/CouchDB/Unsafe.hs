-- | an unsafe interface to CouchDB.  Database and document names are not
-- sanitized.
module Database.CouchDB.Unsafe
  (
  -- * Databases
    createDB
  , dropDB
  , getAllDBs
  -- * Documents
  , newNamedDoc
  , newDoc
  , updateDoc
  , deleteDoc
  , forceDeleteDoc
  , getDocPrim
  , getDocRaw
  , getDoc
  , getAndUpdateDoc
  , getAllDocIds
  , getAllDocs
  -- * Views
  -- $views
  , CouchView (..)
  , newView
  , queryView
  , queryViewKeys
  ) where

import Database.CouchDB.HTTP
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust,mapMaybe)
import Text.JSON
import qualified Data.List as L

assertJSObject :: JSValue -> CouchMonad JSValue
assertJSObject v@(JSObject _) = return v
assertJSObject o = fail $ "expected a JSON object; received: " ++ encode o

couchResponse :: String -> [(String,JSValue)]
couchResponse respBody = case dec respBody of
  Error s -> error $ "couchResponse: s"
  Ok r -> fromJSObject r

request' :: String -> RequestMethod -> CouchMonad (Response String)
request' path method = request path [] method [] ""

-- |Creates a new database.  Throws an exception if the database already
-- exists. 
createDB :: String -> CouchMonad ()
createDB name = do
  resp <- request' name PUT
  unless (rspCode resp == (2,0,1)) $
    error (rspReason resp)

dropDB :: String -> CouchMonad Bool -- ^False if the database does not exist
dropDB name = do
  resp <- request' name DELETE
  case rspCode resp of
    (2,0,0) -> return True
    (4,0,4) -> return False
    otherwise -> error (rspReason resp)

getAllDBs :: CouchMonad [JSString]
getAllDBs = do
  response <- request' "_all_dbs" GET
  case rspCode response of
    (2,0,0) ->
      case dec (rspBody response) of
        Ok (JSArray dbs) -> return [db | JSString db <- dbs]
        otherwise        -> error "Unexpected couch response"
    otherwise -> error (show response)

newNamedDoc :: (JSON a)
            => String -- ^database name
            -> String -- ^document name
            -> a -- ^document body
            -> CouchMonad (Either String JSString)
            -- ^Returns 'Left' on a conflict.  Returns 'Right' with the
            -- revision number on success.
newNamedDoc dbName docName body = do
  obj <- assertJSObject (showJSON body)
  r <- request (dbName ++ "/" ++ docName) [] PUT [] (enc obj)
  case rspCode r of
    (2,0,1) -> do
      let result = couchResponse (rspBody r)
      let (JSString rev) = fromJust $ lookup "rev" result
      return (Right rev)
    (4,0,9) ->  do
      let result = couchResponse (rspBody r)
      let (JSObject errorObj) = fromJust $ lookup "error" result
      let (JSString reason) = 
            fromJust $ lookup "reason" (fromJSObject errorObj)
      return $ Left (fromJSString reason)
    otherwise -> error (show r)


updateDoc :: (JSON a)
          => String -- ^database
          -> (JSString,JSString) -- ^document and revision
          -> a -- ^ new value
          -> CouchMonad (Maybe (JSString,JSString)) 
updateDoc db (doc,rev) val = do
  let (JSObject obj) = showJSON val
  let doc' = fromJSString doc
  let obj' = ("_id",JSString doc):("_rev",JSString rev):(fromJSObject obj)
  r <- request (db ++ "/" ++ doc') [] PUT [] (enc $ toJSObject obj')
  case rspCode r of
    (2,0,1) ->  do
      let result = couchResponse (rspBody r)
      let (JSString rev) = fromJust $ lookup "rev" result
      return $ Just (doc,rev)
    (4,0,9) ->  return Nothing
    otherwise -> 
      error $ "updateDoc error.\n" ++ (show r) ++ rspBody r


-- |Delete a doc by document identifier (revision number not needed).  This
-- operation first retreives the document to get its revision number.  It fails
-- if the document doesn't exist or there is a conflict.
forceDeleteDoc :: String -- ^ database
               -> String -- ^ document identifier
               -> CouchMonad Bool
forceDeleteDoc db doc = do
  r <- getDocPrim db doc
  case r of
    Just (id,rev,_) -> deleteDoc db (id,rev)
    Nothing -> return False

deleteDoc :: String  -- ^database
          -> (JSString,JSString) -- ^document and revision
          -> CouchMonad Bool
deleteDoc db (doc,rev) = do 
  r <- request (db ++ "/" ++ (fromJSString doc)) [("rev",fromJSString rev)]
         DELETE [] ""
  case rspCode r of
    (2,0,0) -> return True
    -- TODO: figure out which error codes are normal (delete conflicts)
    otherwise -> fail $ "deleteDoc failed: " ++ (show r)
      

newDoc :: (JSON a)
       => String -- ^database name
      -> a       -- ^document body
      -> CouchMonad (JSString,JSString) -- ^ id and rev of new document
newDoc db doc = do
  obj <- assertJSObject (showJSON doc)
  r <- request db [] POST [] (enc obj)
  case rspCode r of
    (2,0,1) -> do
      let result = couchResponse (rspBody r)
      let (JSString rev) = fromJust $ lookup "rev" result
      let (JSString id) = fromJust $ lookup "id" result
      return (id,rev)
    otherwise -> error (show r)
    
getDoc :: (JSON a)
       => String -- ^database name
       -> String -- ^document name
       -> CouchMonad (Maybe (JSString,JSString,a)) -- ^'Nothing' if the 
                                                   -- doc does not exist
getDoc dbName docName = do
  r <- request' (dbName ++ "/" ++ docName) GET
  case rspCode r of
    (2,0,0) -> do
      let result = couchResponse (rspBody r)
      let (JSString rev) = fromJust $ lookup "_rev" result
      let (JSString id) = fromJust $ lookup "_id" result
      case readJSON (JSObject $ toJSObject result) of
        Ok val -> return $ Just (id, rev, val)
        val -> fail $ "error parsing: " ++ encode (toJSObject result)
    (4,0,4) -> return Nothing -- doc does not exist
    otherwise -> error (show r)

-- |Gets a document as a raw JSON value.  Returns the document id,
-- revision and value as a 'JSObject'.  These fields are queried lazily,
-- and may fail later if the response from the server is malformed.
getDocPrim :: String -- ^database name
           -> String -- ^document name
           -> CouchMonad (Maybe (JSString,JSString,[(String,JSValue)]))
           -- ^'Nothing' if the document does not exist.
getDocPrim db doc = do
  r <- request' (db ++ "/" ++ doc) GET
  case rspCode r of
    (2,0,0) -> do
      let obj = couchResponse (rspBody r)
      let ~(JSString rev) = fromJust $ lookup "_rev" obj
      let ~(JSString id) = fromJust $ lookup "_id" obj
      return $ Just (id,rev,obj)
    (4,0,4) -> return Nothing -- doc does not exist
    code -> fail $ "getDocPrim: " ++ show code ++ " error"

-- |Gets a document as a Maybe String.  Returns the raw result of what 
-- couchdb returns.  Returns Nothing if the doc does not exist.
getDocRaw :: String -> String -> CouchMonad (Maybe String)
getDocRaw db doc = do
  r <- request' (db ++ "/" ++ doc) GET
  case rspCode r of
    (2,0,0) -> do
      return $ Just (rspBody r)
    (4,0,4) -> return Nothing -- doc does not exist
    code -> fail $ "getDocRaw: " ++ show code ++ " error"



getAndUpdateDoc :: (JSON a)
                => String -- ^database
                -> String -- ^document name
                -> (a -> IO a) -- ^update function
                -> CouchMonad (Maybe String) -- ^If the update succeeds,
                                             -- return the revision number
                                             -- of the result.
getAndUpdateDoc db docId fn = do
  r <- getDoc db docId
  case r of
    Just (id,rev,val) -> do
      val' <- liftIO (fn val)
      r <- updateDoc db (id,rev) val'
      case r of
        Just (id,rev) -> return (Just $ fromJSString rev)
        Nothing -> return Nothing
    Nothing -> return Nothing


allDocRow :: JSValue -> Maybe JSString
allDocRow (JSObject row) = case lookup "key" (fromJSObject row) of
  Just (JSString s) -> let key = fromJSString s
                         in case key of
                              '_':_ -> Nothing
                              otherwise -> Just s
  Just _ -> error $ "key not a string in row " ++ show row
  Nothing -> error $ "no key in a row " ++ show row
allDocRow v = error $ "expected row to be an object, received " ++ show v

getAllDocIds ::String -- ^database name
             -> CouchMonad [JSString]
getAllDocIds db = do
  response <- request' (db ++ "/_all_docs") GET
  case rspCode response of
    (2,0,0) -> do
      let result = couchResponse (rspBody response)
      let (JSArray rows) = fromJust $ lookup "rows" result
      return $ mapMaybe allDocRow rows
    otherwise -> error (show response)

--
-- $views
-- Creating and querying views
--

data CouchView = ViewMap String String
               | ViewMapReduce String String String

couchViewToJSON :: CouchView -> (String,JSValue)
couchViewToJSON (ViewMap name fn) = (name,JSObject $ toJSObject fn') where
  fn' = [("map", JSString $ toJSString fn)]
couchViewToJSON (ViewMapReduce name m r) =
  (name, JSObject $ toJSObject obj) where
    obj = [("map", JSString $ toJSString m),
           ("reduce", JSString $ toJSString r)]

newView :: String -- ^database name
        -> String -- ^view set name
        -> [CouchView] -- ^views
        -> CouchMonad ()
newView dbName viewName views = do
  let body = toJSObject 
        [("language", JSString $ toJSString "javascript"),
         ("views", JSObject $ toJSObject (map couchViewToJSON views))]
  result <- newNamedDoc dbName ("_design/" ++ viewName) 
             (JSObject body)
  case result of
    Right _ -> return ()
    Left err -> error err

toRow :: JSON a => JSValue -> (JSString,a)
toRow (JSObject objVal) = (key,value) where
   obj = fromJSObject objVal
   key = case lookup "id" obj of
     Just (JSString s) -> s
     Just v -> error $ "toRow: expected id to be a string, got " ++ show v
     Nothing -> error $ "toRow: row does not have an id field in " 
                        ++ show obj
   value = case lookup "value" obj of
     Just v -> case readJSON v of
       Ok v' -> v'
       Error s -> error s
     Nothing -> error $ "toRow: row does not have a value in " ++ show obj
toRow val =
  error $ "toRow: expected row to be an object, received " ++ show val


getAllDocs :: JSON a
           => String -- ^databse
           -> [(String, JSValue)] -- ^query parameters
          -- |Returns a list of rows.  Each row is a key, value pair.
          -> CouchMonad [(JSString, a)]
getAllDocs db args = do
  let args' = map (\(k,v) -> (k,encode v)) args
  let url' = concat [db, "/_all_docs"]
  r <- request url' args' GET [] ""
  case rspCode r of
    (2,0,0) -> do
      let result = couchResponse (rspBody r)
      let (JSArray rows) = fromJust $ lookup "rows" result
      return $ map toRowDoc rows
    otherwise -> error $ "getAllDocs: " ++ show r


toRowDoc :: JSON a => JSValue -> (JSString,a)
toRowDoc (JSObject objVal) = (key,value) where
   obj = fromJSObject objVal
   key = case lookup "id" obj of
     Just (JSString s) -> s
     Just v -> error $ "toRowDoc: expected id to be a string, got " ++ show v
     Nothing -> error $ "toRowDoc: row does not have an id field in " 
                        ++ show obj
   value = case lookup "doc" obj of
     Just v -> case readJSON v of
       Ok v' -> v'
       Error s -> error s
     Nothing -> error $ "toRowDoc: row does not have a value in " ++ show obj
toRowDoc val =
  error $ "toRowDoc: expected row to be an object, received " ++ show val
           

queryView :: (JSON a)
          => String  -- ^database
          -> String  -- ^design
          -> String  -- ^view
          -> [(String, JSValue)] -- ^query parameters
          -- |Returns a list of rows.  Each row is a key, value pair.
          -> CouchMonad [(JSString, a)]
queryView db viewSet view args = do
  let args' = map (\(k,v) -> (k,encode v)) args
  let url' = concat [db, "/_design/", viewSet, "/_view/", view]
  r <- request url' args' GET [] ""
  case rspCode r of
    (2,0,0) -> do
      let result = couchResponse (rspBody r)
      let (JSArray rows) = fromJust $ lookup "rows" result
      return $ map toRow rows
    otherwise -> error (show r)

-- |Like 'queryView', but only returns the keys.  Use this for key-only
-- views where the value is completely ignored.
queryViewKeys :: String  -- ^database
            -> String  -- ^design
            -> String  -- ^view
            -> [(String, JSValue)] -- ^query parameters
            -> CouchMonad [String]
queryViewKeys db viewSet view args = do
  let args' = map (\(k,v) -> (k,encode v)) args
  let url' = concat [db, "/_design/", viewSet, "/_view/", view]
  r <- request url' args' GET [] ""
  case rspCode r of
    (2,0,0) -> do
      let result = couchResponse (rspBody r)
      case lookup "rows" result of
        Just (JSArray rows) -> liftIO $ mapM rowKey rows
        otherwise -> fail $ "queryView: expected rows"
    otherwise -> error (show r)

rowKey :: JSValue -> IO String
rowKey (JSObject obj) = do
  let assoc = fromJSObject obj
  case lookup "id" assoc of
    Just (JSString s) -> return (fromJSString s)
    v -> fail "expected id"
rowKey v = fail "expected id"

enc :: (JSON a) => a -> String
enc = encodeString . encode

dec :: (JSON a) => String -> Result a
dec = decode . decodeString
