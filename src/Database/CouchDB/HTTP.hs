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

-- |Describes a connection to a CouchDB database.  This type is
-- encapsulated by 'CouchMonad'.
data CouchConn = CouchConn 
  { ccConn :: Connection 
  , ccURI :: URI
  , ccHostname :: String
  , ccPort :: Int
  }

-- |A computation that interacts with a CouchDB database.  This monad
-- encapsulates the 'IO' monad, a persistent HTTP connnection  to a
-- CouchDB database and enough information to re-open the connection
-- if it is closed.
data CouchMonad a = CouchMonad (CouchConn -> IO (a,CouchConn))

instance Monad CouchMonad where

  return a = CouchMonad $ \conn -> return (a,conn)

  (CouchMonad m) >>= k = CouchMonad $ \conn -> do
    (a,conn') <- m conn
    let (CouchMonad m') = k a
    m' conn'

  fail msg = CouchMonad $ \conn -> do
    errorM "couchdb" msg
    fail "internal error"   

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

-- |Send a request to the database.  If the connection is closed, it is
-- reopened and the request is resent.  On other errors, we raise an
-- exception.
request :: String -- ^path of the request
       -> [(String,String)] -- ^dictionary of GET parameters
       -> RequestMethod 
       -> [Header] 
       -> String -- ^body of the request
       -> CouchMonad Response
request path query method headers body = do
  url <- makeURL path query
  let allHeaders = (makeHeaders (length body)) ++ headers 
  conn <- getConn
  liftIO $ debugM "couchdb.http" $ concat [show url," ", show method]
  response <- liftIO $ sendHTTP conn (Request url method allHeaders body)
  case response of
    Left connErr -> do
      liftIO $ errorM "couchdb.http" ("request failed: " ++ show connErr)
      fail "server error"
    Right response -> return response

runCouchDB :: String -- ^hostname
           -> Int -- ^port
           -> CouchMonad a 
           -> IO a
runCouchDB hostname port (CouchMonad m) = do
  let uriAuth = URIAuth "" hostname (':':(show port))
  let baseURI = URI "http:" (Just uriAuth) "" "" ""
  conn <- openTCPPort hostname port
  (a,_) <- m (CouchConn conn baseURI hostname port)
  close conn
  return a

-- |Connects to the CouchDB server at localhost:5984.
runCouchDB' :: CouchMonad a -> IO a
runCouchDB' = runCouchDB "127.0.0.1" 5984
