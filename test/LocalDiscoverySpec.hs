{-# LANGUAGE OverloadedStrings #-}

module LocalDiscoverySpec where

import Test.Hspec
import Ssb.Discovery
import Ssb.Address
import Ssb.Key
import Data.Maybe
import Data.ByteString

peerDiscoveryMessage = "net:192.168.25.11:8008~shs:opBinLmYiID9SMEoZJPH60LmduSYAR7J00P7/Gj9vHw="

spec :: Spec
spec =
  describe "LocalPeerDiscovery" $ do
    it "should be able to parse a local peer discovery broadcast message" $ do
      let (Just discovery) = parseDiscoveryMessage peerDiscoveryMessage
      (address discovery) `shouldBe` (Address "192.168.25.11" 8008)
      (exportPublicKey $ publicKey discovery) `shouldBe` "opBinLmYiID9SMEoZJPH60LmduSYAR7J00P7/Gj9vHw="
