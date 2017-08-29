{-# LANGUAGE OverloadedStrings #-}

module FeedSpec where

import Ssb.Key
import Test.Hspec
import Ssb.Feed as Feed
import Ssb.Message
import Data.Map

spec :: Spec
spec = do
        let emptyFeed = Feed.empty "kutyus"
        let message = Message Nothing "kutyus" 0 0 "hash3" "content" "sig" :: Message String
        let message2 = Message (Just "hash3") "harcsabajusz" 1 0 "hash2" "content2" "sig" :: Message String
        let newFeed = add emptyFeed message
        let newFeed2 = add newFeed message2

        describe "message addition" $ do
          it "should be able to add single message to feed" $ do
            Feed.get newFeed (hash message) `shouldBe` (Just message)


        describe "get ordered content" $ do
          it "empty feed should return empty list" $ do
            getOrderedContents emptyFeed `shouldBe` ([] :: [String])

          it "feed with single message should return message" $ do
            getOrderedContents newFeed `shouldBe` [content message]

          it "feed with two messages should return messages in order" $ do
            getOrderedContents newFeed2 `shouldBe` [content message, content message2]

        describe "get feed length content" $ do
           it "empty feed should return 0" $ do
             Feed.length emptyFeed `shouldBe` 0

           it "single element feed should return 1" $ do
             Feed.length newFeed `shouldBe` 1

           it "two element feed should return 2" $ do
             Feed.length newFeed2 `shouldBe` 2
