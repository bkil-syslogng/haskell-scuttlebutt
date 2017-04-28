module AddressSpec where

import Ssb.Address
import Test.Hspec

spec :: Spec
spec = describe "address" $ do
         it "holds the dns or the ip address and the port" $ do
           let address = Address "localhost" 8008
           host address `shouldBe` "localhost"
           port address `shouldBe` 8008
