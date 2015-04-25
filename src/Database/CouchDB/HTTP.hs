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
  , runCouchDBURI
  , CouchConn()
  , createCouchConn
  , createCouchConnFromURI
  , runCouchDBWith
  , closeCouchConn
  ) where

import Data.IORef
import Control.Concurrent
import Network.TCP
import Network.HTTP
import Network.URI
import Control.Exception (bracket)
import Control.Monad.Trans (MonadIO (..))
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS (ByteString, length)
import qualified Data.ByteString.UTF8 as UTF8 (fromString, toString)
import Network.HTTP.Auth
import Control.Applicative
import Control.Monad (ap)

-- |Describes a connection to a CouchDB database.  This type is
-- encapsulated by 'CouchMonad'.
data CouchConn = CouchConn 
  { ccConn :: IORef (HandleStream BS.ByteString) 
  , ccURI :: URI
  , ccHostname :: String
  , ccPort :: Int
  , ccAuth :: Maybe Authority -- ^login credentials, if needed.
  }

-- |A computation that interacts with a CouchDB database.  This monad
-- encapsulates the 'IO' monad, a persistent HTTP connnection  to a
-- CouchDB database and enough information to re-open the connection
-- if it is closed.
data CouchMonad a = CouchMonad (CouchConn -> IO (a,CouchConn))

instance Applicative CouchMonad where
instance Functor CouchMonad where
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

getConn :: CouchMonad (HandleStream BS.ByteString)
getConn = CouchMonad $ \conn -> do
  r <- readIORef (ccConn conn)
  return (r,conn)
  
getConnAuth :: CouchMonad (Maybe Authority)
getConnAuth = CouchMonad $ \conn -> return ((ccAuth conn),conn)

reopenConnection :: CouchMonad ()
reopenConnection = CouchMonad $ \conn -> do
  c <- liftIO $ readIORef (ccConn conn) >>= close
  connection <- liftIO $ openTCPConnection (ccHostname conn) (ccPort conn)
  writeIORef (ccConn conn) connection
  return ((), conn)

makeHeaders bodyLen =
  [ Header HdrContentType "application/json"
  , Header HdrContentEncoding "UTF-8"
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
  let body' = UTF8.fromString body
  url <- makeURL path query
  let allHeaders = (makeHeaders (BS.length body')) ++ headers 
  conn <- getConn
  auth <- getConnAuth
  let req' = Request url method allHeaders body'
  let req = maybe req' (fillAuth req') auth
  let retry 0 = do
        fail $ "server error: " ++ show req
      retry n = do
        response <- liftIO $ sendHTTP conn req
        case response of
          Left err -> do
            reopenConnection
            retry (n-1)
          Right val -> return (unUTF8 val)
  retry 2
  where
    unUTF8 :: Response BS.ByteString -> Response String
    unUTF8 (Response c r h b) = Response c r h (UTF8.toString b)

fillAuth :: Request a -> Authority -> Request a
fillAuth req auth = req { rqHeaders = new : rqHeaders req }
  where new = Header HdrAuthorization (withAuthority auth req)

runCouchDBURI :: URI -- ^URI to connect
              -> CouchMonad a
              -> IO a
runCouchDBURI uri act = bracket
                        (createCouchConnFromURI uri)
                        closeCouchConn
                        (flip runCouchDBWith act)

runCouchDB :: String -- ^hostname
           -> Int -- ^port
           -> CouchMonad a 
           -> IO a
runCouchDB hostname port act = bracket
                               (createCouchConn hostname port)
                               closeCouchConn
                               (flip runCouchDBWith act)

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
createCouchConn hostname port = createCouchAuthConn hostname port Nothing

-- |Create a CouchDB connection with password authentication for use
-- with runCouchDBWith.
createCouchAuthConn :: String          -- ^hostname
                    -> Int             -- ^port
                    -> Maybe Authority -- ^Login credentials
                    -> IO (CouchConn)
createCouchAuthConn hostname port auth = do
  let uriAuth = URIAuth "" hostname (':':(show port))
  let baseURI = URI "http:" (Just uriAuth) "" "" ""
  c <- openTCPConnection hostname port
  conn <- newIORef c
  return (CouchConn conn baseURI hostname port auth)

-- |Create a CouchDB from an URI connection for use with runCouchDBWith.
createCouchConnFromURI :: URI -- ^URI with possible login credentials
                       -> IO (CouchConn)
createCouchConnFromURI baseURI = do
  createCouchAuthConn hostname port auth
  where
    ua = fromJust $ uriAuthority baseURI
    hostname = uriRegName ua
    port = uriAuthPort (Just baseURI) ua
    ua2 = (fromJust.parseURIAuthority.uriToAuthorityString) baseURI
    auth = (Just AuthBasic)
           `ap` (return "")
           `ap` (user ua2)
           `ap` (password ua2)
           `ap` (return baseURI)

-- |Closes an open CouchDB connection
closeCouchConn :: CouchConn -> IO ()
closeCouchConn (CouchConn conn _ _ _ _ ) = readIORef conn >>= close
