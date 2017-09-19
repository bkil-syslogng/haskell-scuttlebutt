{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MessageSpec where

import Test.Hspec
import Ssb.Message
import Ssb.Patchwork
import GHC.Generics
import Data.Aeson
import Data.Maybe

import Prelude hiding (sequence)

originalMessage = "{\n  \"previous\": \"%3AWRZYdDHKOmLOWvzHbwJFjx9g8hOQH/NXZkwciA63Y=.sha256\",\n  \"author\": \"@EMovhfIrFk4NihAKnRNhrfRaqIhBv1Wj8pTxJNgvCCY=.ed25519\",\n  \"sequence\": 184,\n  \"timestamp\": 1449954503740,\n  \"hash\": \"sha256\",\n  \"content\": {\n    \"type\": \"post\",\n    \"text\": \"@johnny use https://github.com/ssbc/ssb-msgs and https://github.com/ssbc/ssb-ref is probably useful too.\",\n    \"root\": \"%rf1JvoFg1pHE6TkuuMxsjBNevFck7LQvXGhkLMNlaYs=.sha256\",\n    \"branch\": \"%G37a4PUTbysiETQazyYATwLkfn/ZzigmIheAx905MLU=.sha256\",\n    \"mentions\": [\n      {\n        \"link\": \"@dnr1swLSAgf36g+FzGjNLgmytj2IIyDaYeKZ7F5GdzY=.ed25519\",\n        \"name\": \"johnny\"\n      }\n    ]\n  },\n  \"signature\": \"28GiO52GFjJnrwpKo359FamEes7JB9gTiiZaidKLL1C1NRueqGq2IAYQ1V+T2AnBgUJLRZUIyNtLTlBcx4RGAA==.sig.ed25519\"\n}"

data Content = Content {root :: String} deriving (Generic, Eq, Show)
instance FromJSON Content

spec :: Spec
spec =
  do 
      describe "message" $ do
         it "extracts message content" $ do
            let (Just message) = (decode originalMessage) :: Maybe (Message Content)
            previous message `shouldBe` (Just "%3AWRZYdDHKOmLOWvzHbwJFjx9g8hOQH/NXZkwciA63Y=.sha256")
            author message `shouldBe` "@EMovhfIrFk4NihAKnRNhrfRaqIhBv1Wj8pTxJNgvCCY=.ed25519"
            sequence message `shouldBe` 184
            timestamp message `shouldBe` 1449954503740
            hash message `shouldBe` "sha256"
            content message `shouldBe` Content "%rf1JvoFg1pHE6TkuuMxsjBNevFck7LQvXGhkLMNlaYs=.sha256"
            signature message `shouldBe` "28GiO52GFjJnrwpKo359FamEes7JB9gTiiZaidKLL1C1NRueqGq2IAYQ1V+T2AnBgUJLRZUIyNtLTlBcx4RGAA==.sig.ed25519"

         it "is a functor" $ do
            let message = fromJust $ decode originalMessage :: Message Value
            let patchworkMessage = fmap parsePatchwork message :: Message (Maybe PatchworkMessage)
            isJust (content patchworkMessage) `shouldBe` True

      describe "patchwork parse" $ do
        it "should not attempt to parse JSON values other than Object" $ do
                        parsePatchwork ( Bool True) `shouldBe` Nothing

        it "parses the text from a post" $ do
            let expectedText ="@johnny use https://github.com/ssbc/ssb-msgs and https://github.com/ssbc/ssb-ref is probably useful too."
            let (Just postContent) = decode "{\n    \"type\": \"post\",\n    \"text\": \"@johnny use https://github.com/ssbc/ssb-msgs and https://github.com/ssbc/ssb-ref is probably useful too.\",\n    \"root\": \"%rf1JvoFg1pHE6TkuuMxsjBNevFck7LQvXGhkLMNlaYs=.sha256\",\n    \"branch\": \"%G37a4PUTbysiETQazyYATwLkfn/ZzigmIheAx905MLU=.sha256\",\n    \"mentions\": [\n      {\n        \"link\": \"@dnr1swLSAgf36g+FzGjNLgmytj2IIyDaYeKZ7F5GdzY=.ed25519\",\n        \"name\": \"johnny\"\n      }\n    ]\n  }" :: Maybe Value
            let (Just parsed) = parsePatchwork postContent
            text parsed `shouldBe` expectedText 

        it "does not parse content that is not a post" $ do
            let (Just postContent) = decode "{\n    \"type\": \"somethingelse\",\n    \"text\": \"@johnny use https://github.com/ssbc/ssb-msgs and https://github.com/ssbc/ssb-ref is probably useful too.\",\n    \"root\": \"%rf1JvoFg1pHE6TkuuMxsjBNevFck7LQvXGhkLMNlaYs=.sha256\",\n    \"branch\": \"%G37a4PUTbysiETQazyYATwLkfn/ZzigmIheAx905MLU=.sha256\",\n    \"mentions\": [\n      {\n        \"link\": \"@dnr1swLSAgf36g+FzGjNLgmytj2IIyDaYeKZ7F5GdzY=.ed25519\",\n        \"name\": \"johnny\"\n      }\n    ]\n  }" :: Maybe Value
            let parsed = parsePatchwork postContent
            parsed `shouldBe` Nothing
