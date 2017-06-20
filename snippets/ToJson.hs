{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson
import Ssb
import Ssb.Message
import qualified Data.ByteString.Lazy         as LBS
import Data.Maybe(fromJust)

originalMessage = "{\n  \"previous\": \"%3AWRZYdDHKOmLOWvzHbwJFjx9g8hOQH/NXZkwciA63Y=.sha256\",\n  \"author\": \"@EMovhfIrFk4NihAKnRNhrfRaqIhBv1Wj8pTxJNgvCCY=.ed25519\",\n  \"sequence\": 184,\n  \"timestamp\": 1449954503740,\n  \"hash\": \"sha256\",\n  \"content\": {\n    \"type\": \"post\",\n    \"text\": \"@johnny use https://github.com/ssbc/ssb-msgs and https://github.com/ssbc/ssb-ref is probably useful too.\",\n    \"root\": \"%rf1JvoFg1pHE6TkuuMxsjBNevFck7LQvXGhkLMNlaYs=.sha256\",\n    \"branch\": \"%G37a4PUTbysiETQazyYATwLkfn/ZzigmIheAx905MLU=.sha256\",\n    \"mentions\": [\n      {\n        \"link\": \"@dnr1swLSAgf36g+FzGjNLgmytj2IIyDaYeKZ7F5GdzY=.ed25519\",\n        \"name\": \"johnny\"\n      }\n    ]\n  },\n  \"signature\": \"28GiO52GFjJnrwpKo359FamEes7JB9gTiiZaidKLL1C1NRueqGq2IAYQ1V+T2AnBgUJLRZUIyNtLTlBcx4RGAA==.sig.ed25519\"\n}"

main :: IO ()
main = do
  let lbsmessage = (LBS.fromStrict originalMessage) :: LBS.ByteString
  let decoded = Data.Aeson.eitherDecode lbsmessage :: Either String (Ssb.Message Data.Aeson.Object)
  case decoded of
    Right dec ->
      LBS.putStrLn $ Data.Aeson.encode dec
    Left msg ->
      putStrLn msg
