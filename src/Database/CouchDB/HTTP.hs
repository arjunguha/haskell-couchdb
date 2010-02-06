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
  , CouchConn()
  , createCouchConn
  , runCouchDBWith
  , closeCouchConn
  ) where

import Data.IORef
import Control.Concurrent
import Network.TCP
import Network.HTTP
import Network.URI
import Control.Exception (finally)
import Control.Monad.Trans (MonadIO (..))

-- |Describes a connection to a CouchDB database.  This type is
-- encapsulated by 'CouchMonad'.
data CouchConn = CouchConn 
  { ccConn :: IORef (HandleStream String) 
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
    fail $ "internal error: " ++ msg   

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

getConn :: CouchMonad (HandleStream String)
getConn = CouchMonad $ \conn -> do
  r <- readIORef (ccConn conn)
  return (r,conn)

reopenConnection :: CouchMonad ()
reopenConnection = CouchMonad $ \conn -> do
  c <- liftIO $ readIORef (ccConn conn) >>= close
  connection <- liftIO $ openTCPConnection (ccHostname conn) (ccPort conn)
  writeIORef (ccConn conn) connection
  return ((), conn)

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
       -> CouchMonad (Response String)
request path query method headers body = do
  url <- makeURL path query
  let allHeaders = (makeHeaders (length body)) ++ headers 
  conn <- getConn
  let req = Request url method allHeaders body
  let retry 0 = do
        fail $ "server error: " ++ show req
      retry n = do
        response <- liftIO $ sendHTTP conn req
        case response of
          Left err -> do
            reopenConnection
            retry (n-1)
          Right val -> return val
  retry 2


runCouchDB :: String -- ^hostname
           -> Int -- ^port
           -> CouchMonad a 
           -> IO a
runCouchDB hostname port (CouchMonad m) = do
  let uriAuth = URIAuth "" hostname (':':(show port))
  let baseURI = URI "http:" (Just uriAuth) "" "" ""
  c <- openTCPConnection hostname port
  conn <- newIORef c
  (a,_) <- m (CouchConn conn baseURI hostname port)
           `finally` (do c <- readIORef conn
                         close c)
  return a

-- |Connects to the CouchDB server at localhost:5984.
runCouchDB' :: CouchMonad a -> IO a
runCouchDB' = runCouchDB "127.0.0.1" 5984

-- |Run a CouchDB computation with an existing CouchDB connection.
runCouchDBWith :: CouchConn -> CouchMonad a -> IO a
runCouchDBWith conn (CouchMonad f) = fmap fst $ f conn

-- |Create a CouchDB connection for use with runCouchDBWith.
createCouchConn :: String -- ^hostname
                -> Int    -- ^port
                -> IO (CouchConn)
createCouchConn hostname port = do
  let uriAuth = URIAuth "" hostname (':':(show port))
  let baseURI = URI "http:" (Just uriAuth) "" "" ""
  c <- openTCPConnection hostname port
  conn <- newIORef c
  return (CouchConn conn baseURI hostname port)

-- |Closes an open CouchDB connection
closeCouchConn :: CouchConn -> IO ()
closeCouchConn (CouchConn conn _ _ _) = readIORef conn >>= close
