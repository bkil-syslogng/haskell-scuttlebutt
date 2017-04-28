import Network.Socket.ByteString
import Network.Socket hiding (recvFrom)
import Ssb.Discovery

main :: IO ()
main = withSocketsDo $ do
  addresses <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "8008")
  print addresses
  let server = head addresses
  s <- socket (addrFamily server) Datagram defaultProtocol
  bind s (addrAddress server)
  putStrLn "Server started ..."
  handleConnections s

handleConnections :: Socket -> IO ()
handleConnections conn = do
  (stuff, _) <- recvFrom conn 1024
  let maybePeerInfo = parseDiscoveryMessage stuff
  maybe (putStrLn "Malformed discovery message received.")
        print
        maybePeerInfo
  handleConnections conn
