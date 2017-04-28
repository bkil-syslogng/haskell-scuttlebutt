{-# LANGUAGE OverloadedStrings #-}

module KeySpec where

import Ssb.Key
import Test.Hspec

base64PublicKey = "opBinLmYiID9SMEoZJPH60LmduSYAR7J00P7/Gj9vHw="

spec :: Spec
spec =
  describe "public key" $ do
    it "can be deserialized and serialized from base64 bytestrings" $ do
      let (Just publicKey) = parsePublicKey base64PublicKey
      (exportPublicKey publicKey) `shouldBe` base64PublicKey
