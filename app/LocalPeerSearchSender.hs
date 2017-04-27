import Network.Socket

main :: IO ()
main = withSocketsDo $ do
  (server:_) <- getAddrInfo Nothing (Just "255.255.255.255") (Just "8008")
  s <- socket (addrFamily server) Datagram defaultProtocol
  ret <- sendTo s "Hello" (addrAddress server)
  print ret
