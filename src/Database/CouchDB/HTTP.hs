module Database.CouchDB.HTTP 
  ( request
  , RequestMethod (..)
  , CouchMonad
  , Response (..)
  , runCouchDB
  ) where

import Network.TCP
import Network.Stream
import Network.HTTP
import Network.URI
import Control.Monad.Trans (MonadIO (..))

data CouchConn = CouchConn 
  { ccConn :: Connection
  , ccURI :: URI
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

makeHeaders bodyLen =
  [ Header HdrContentType "application/json"
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
  let request = Request url method allHeaders body
  conn <- getConn
  response <- liftIO $ sendHTTP conn request
  case response of
    Left connErr -> error (show connErr)
    Right response -> return response

runCouchDB :: String -> Int -> CouchMonad a -> IO a
runCouchDB hostname port (CouchMonad m) = do
  let uriAuth = URIAuth "" hostname (show port)
  let baseURI = URI "http:" (Just uriAuth) "" "" ""
  conn <- openTCPPort hostname port
  (a,_) <- m (CouchConn conn baseURI)
  close conn
  return a

