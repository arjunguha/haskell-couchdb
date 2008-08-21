-- |Maintains a persistent HTTP connection to a CouchDB database server.
-- CouchDB enjoys closing the connection if there is an error (document
-- not found, etc.)  In such cases, 'CouchMonad' will automatically
-- reestablish the connection.
module Database.CouchDB.HTTP 
  ( request
  , RequestMethod (..)
  , CouchMonad
  , Response (..)
  , runCouchDB
  , runCouchDB'
  ) where

import System.Log.Logger (errorM,debugM)
import Network.TCP
import Network.Stream
import Network.HTTP
import Network.URI
import Control.Monad.Trans (MonadIO (..))

data CouchConn = CouchConn 
  { ccConn :: Connection
  , ccURI :: URI
  , ccHostname :: String
  , ccPort :: Int
  }

data CouchMonad a = CouchMonad (CouchConn -> IO (a,CouchConn))

instance Monad CouchMonad where

  return a = CouchMonad $ \conn -> return (a,conn)

  (CouchMonad m) >>= k = CouchMonad $ \conn -> do
    (a,conn') <- m conn
    let (CouchMonad m') = k a
    m' conn'

instance MonadIO CouchMonad where

  liftIO m = CouchMonad $ \conn -> m >>= \a -> return (a,conn)

makeURL :: String -- ^path
        -> [(String,String)]
        -> CouchMonad URI
makeURL path query = CouchMonad $ \conn -> do
  return ( (ccURI conn) { uriPath = '/':path
                        , uriQuery = '?':(urlEncodeVars query) 
                        }
         ,conn )

getConn :: CouchMonad Connection
getConn = CouchMonad $ \conn -> return (ccConn conn,conn)

reopenConnection :: CouchMonad ()
reopenConnection = CouchMonad $ \conn -> do
  liftIO $ close (ccConn conn) -- prevent memory leak
  connection <- liftIO $ openTCPPort (ccHostname conn) (ccPort conn)
  return ((), conn {ccConn = connection})

makeHeaders bodyLen =
  [ Header HdrContentType "application/json"
  , Header HdrConnection "keep-alive"
  , Header HdrContentLength (show bodyLen)
  ]

request :: String 
       -> [(String,String)]
       -> RequestMethod 
       -> [Header] 
       -> String 
       -> CouchMonad Response
request path query method headers body = do
  url <- makeURL path query
  let allHeaders = (makeHeaders (length body)) ++ headers 
  conn <- getConn
  liftIO $ debugM "couchdb.http" $ concat [show url," ", show method]
  response <- liftIO $ sendHTTP conn (Request url method allHeaders body)
  case response of
    Left ErrorClosed -> do
      liftIO $ errorM "couchdb.http" "connection closed; reopening"
      reopenConnection
      request path query method headers body
    Left connErr -> do
      liftIO $ errorM "couchdb.http" ("send failed: " ++ show connErr)
      fail (show connErr)
    Right response -> return response

runCouchDB :: String -> Int -> CouchMonad a -> IO a
runCouchDB hostname port (CouchMonad m) = do
  let uriAuth = URIAuth "" hostname (':':(show port))
  let baseURI = URI "http:" (Just uriAuth) "" "" ""
  conn <- openTCPPort hostname port
  (a,_) <- m (CouchConn conn baseURI hostname port)
  close conn
  return a

-- |Connects to the CouchDB server at localhost:5984
runCouchDB' = runCouchDB "127.0.0.1" 5984
