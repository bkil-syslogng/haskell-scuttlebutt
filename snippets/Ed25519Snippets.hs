module Main where

import Data.ByteString hiding (pack)
import Data.ByteString.Char8 (pack)
import Crypto.Sign.Ed25519

serialize :: SecretKey -> ByteString
serialize = unSecretKey

deserialize :: ByteString -> SecretKey
deserialize = SecretKey

main :: IO ()
main = do
  (publicKey, secretKey) <- createKeypair
  let secret = serialize secretKey
  print $ secretKey
  let signedMessage = sign secretKey (pack "Hello")
  print signedMessage
  print $ verify publicKey signedMessage
  (anotherPublicKey, _) <- createKeypair
  print $ verify anotherPublicKey signedMessage
