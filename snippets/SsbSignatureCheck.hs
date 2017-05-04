{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString
import Data.ByteString.Base64
import qualified Crypto.Sign.Ed25519 as Ed

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


msg = "{\n  \"previous\": \"%3AWRZYdDHKOmLOWvzHbwJFjx9g8hOQH/NXZkwciA63Y=.sha256\",\n  \"author\": \"@EMovhfIrFk4NihAKnRNhrfRaqIhBv1Wj8pTxJNgvCCY=.ed25519\",\n  \"sequence\": 184,\n  \"timestamp\": 1449954503740,\n  \"hash\": \"sha256\",\n  \"content\": {\n    \"type\": \"post\",\n    \"text\": \"@johnny use https://github.com/ssbc/ssb-msgs and https://github.com/ssbc/ssb-ref is probably useful too.\",\n    \"root\": \"%rf1JvoFg1pHE6TkuuMxsjBNevFck7LQvXGhkLMNlaYs=.sha256\",\n    \"branch\": \"%G37a4PUTbysiETQazyYATwLkfn/ZzigmIheAx905MLU=.sha256\",\n    \"mentions\": [\n      {\n        \"link\": \"@dnr1swLSAgf36g+FzGjNLgmytj2IIyDaYeKZ7F5GdzY=.ed25519\",\n        \"name\": \"johnny\"\n      }\n    ]\n  }\n}"

originalMessage = "{\n  \"previous\": \"%3AWRZYdDHKOmLOWvzHbwJFjx9g8hOQH/NXZkwciA63Y=.sha256\",\n  \"author\": \"@EMovhfIrFk4NihAKnRNhrfRaqIhBv1Wj8pTxJNgvCCY=.ed25519\",\n  \"sequence\": 184,\n  \"timestamp\": 1449954503740,\n  \"hash\": \"sha256\",\n  \"content\": {\n    \"type\": \"post\",\n    \"text\": \"@johnny use https://github.com/ssbc/ssb-msgs and https://github.com/ssbc/ssb-ref is probably useful too.\",\n    \"root\": \"%rf1JvoFg1pHE6TkuuMxsjBNevFck7LQvXGhkLMNlaYs=.sha256\",\n    \"branch\": \"%G37a4PUTbysiETQazyYATwLkfn/ZzigmIheAx905MLU=.sha256\",\n    \"mentions\": [\n      {\n        \"link\": \"@dnr1swLSAgf36g+FzGjNLgmytj2IIyDaYeKZ7F5GdzY=.ed25519\",\n        \"name\": \"johnny\"\n      }\n    ]\n  },\n  \"signature\": \"28GiO52GFjJnrwpKo359FamEes7JB9gTiiZaidKLL1C1NRueqGq2IAYQ1V+T2AnBgUJLRZUIyNtLTlBcx4RGAA==.sig.ed25519\"\n}"

signature = "28GiO52GFjJnrwpKo359FamEes7JB9gTiiZaidKLL1C1NRueqGq2IAYQ1V+T2AnBgUJLRZUIyNtLTlBcx4RGAA=="

publicKey = "EMovhfIrFk4NihAKnRNhrfRaqIhBv1Wj8pTxJNgvCCY="

main :: IO ()
main = do
  Data.ByteString.putStrLn originalMessage
  Data.ByteString.putStrLn msg
  let (Just pkey) = parsePublicKey publicKey
  let (Just sig) = parseSignature signature
  print pkey
  print sig
  print $ Ed.dverify pkey msg sig
