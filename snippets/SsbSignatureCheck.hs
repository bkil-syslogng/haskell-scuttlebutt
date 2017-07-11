{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString as BS
import Data.ByteString.Char8 as BS8
import Data.ByteString.Base64
import qualified Crypto.Sign.Ed25519 as Ed
import qualified Data.Aeson
import Ssb
import Ssb.Message
import qualified Data.ByteString.Lazy         as LBS

parsePublicKey :: ByteString -> Maybe Ed.PublicKey
parsePublicKey encodedPublicKey =
  let decodedPublicKey = decode encodedPublicKey
      maybeEd25519PublicKey =
        either (const Nothing)
               (Just . Ed.PublicKey)
               decodedPublicKey
  in maybeEd25519PublicKey

parseSignature :: ByteString -> Maybe Ed.Signature
parseSignature encodedSig = let decodedSig = decode encodedSig
                                maybeSig = either (const Nothing)
                                                  (Just . Ed.Signature)
                                                  decodedSig
                             in maybeSig

cutSignature :: ByteString -> ByteString
cutSignature msg = let
 val0 = Prelude.init $ Prelude.init $ BS8.lines msg
 init0 = Prelude.init val0
 last0 = Prelude.last val0
 val =  init0 ++ [BS.init last0]
 in BS.init $ BS8.unlines $ val ++ ["}"]

originalMessage = "{\n\
                    \  \"previous\": \"%3AWRZYdDHKOmLOWvzHbwJFjx9g8hOQH/NXZkwciA63Y=.sha256\",\n\
                    \  \"author\": \"@EMovhfIrFk4NihAKnRNhrfRaqIhBv1Wj8pTxJNgvCCY=.ed25519\",\n\
                    \  \"sequence\": 184,\n\
                    \  \"timestamp\": 1449954503740,\n\
                    \  \"hash\": \"sha256\",\n\
                    \  \"content\": {\n\
                    \    \"type\": \"post\",\n\
                    \    \"text\": \"@johnny use https://github.com/ssbc/ssb-msgs and https://github.com/ssbc/ssb-ref is probably useful too.\",\n\
                    \    \"root\": \"%rf1JvoFg1pHE6TkuuMxsjBNevFck7LQvXGhkLMNlaYs=.sha256\",\n\
                    \    \"branch\": \"%G37a4PUTbysiETQazyYATwLkfn/ZzigmIheAx905MLU=.sha256\",\n\
                    \    \"mentions\": [\n\
                    \      {\n\
                    \        \"link\": \"@dnr1swLSAgf36g+FzGjNLgmytj2IIyDaYeKZ7F5GdzY=.ed25519\",\n\
                    \        \"name\": \"johnny\"\n\
                    \      }\n\
                    \    ]\n\
                    \  },\n\
                    \  \"signature\": \"28GiO52GFjJnrwpKo359FamEes7JB9gTiiZaidKLL1C1NRueqGq2IAYQ1V+T2AnBgUJLRZUIyNtLTlBcx4RGAA==.sig.ed25519\"\n\
                    \\
                    \}"

dropAfterChar ch str = Prelude.init $ Prelude.reverse $ Prelude.dropWhile (/= ch) $ Prelude.reverse str

main :: IO ()
main = do
  let msg = cutSignature originalMessage
  BS.putStrLn originalMessage
  BS.putStrLn msg

  let lbsmessage = (LBS.fromStrict originalMessage) :: LBS.ByteString
  let decoded = Data.Aeson.eitherDecode lbsmessage :: Either String (Ssb.Message Data.Aeson.Object)
  case decoded of
    Right dec -> do
      let signatureField = BS8.pack $ signature dec
      let (Just sig) = parseSignature signatureField

      let publicKey = BS8.pack $ dropAfterChar '.' $ Prelude.tail $ author dec
      Prelude.putStrLn $ show publicKey

      let (Just pkey) = parsePublicKey publicKey
      Prelude.putStrLn $ show pkey
      Prelude.putStrLn $ show sig

      Prelude.putStrLn $ show $ Ed.dverify pkey msg sig
    Left msg ->
      Prelude.putStrLn msg
