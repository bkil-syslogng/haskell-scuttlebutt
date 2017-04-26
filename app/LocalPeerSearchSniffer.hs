import Network.Socket

main :: IO ()
main = withSocketsDo $ do
  (server:_) <- getAddrInfo Nothing (Just "localhost") (Just "8008")
  s <- socket (addrFamily server) Datagram defaultProtocol
  bindSocket s (addrAddress server) >> return s
  putStrLn "Server started ..."
  handleConnections s

handleConnections :: Socket -> IO ()
handleConnections conn = do
  (text, _, _) <- recvFrom conn 1024
  putStrLn text
  handleConnections conn
