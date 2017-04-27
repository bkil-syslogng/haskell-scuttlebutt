import Network.Socket

main :: IO ()
main = withSocketsDo $ do
  addresses <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "8008")
  print addresses
  let server = head addresses
  s <- socket (addrFamily server) Datagram defaultProtocol
  bindSocket s (addrAddress server)
  putStrLn "Server started ..."
  handleConnections s

handleConnections :: Socket -> IO ()
handleConnections conn = do
  stuff <- recvFrom conn 1
  print stuff
  handleConnections conn
