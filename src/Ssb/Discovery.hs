module Ssb.Discovery
    ( parseDiscoveryMessage
    , DiscoveryMessage(..)
    ) where

import Prelude hiding (split)
import Data.ByteString (ByteString)
import Ssb.Address
import Ssb.Key
import Data.Char
import Data.ByteString.Char8 (split, unpack)
import Data.ByteString.Conversion

data DiscoveryMessage = DiscoveryMessage
                      { address :: Address
                      , publicKey :: PublicKey
                      } deriving Show



parseAddress :: [ByteString] -> Maybe Address
parseAddress [] = Nothing
parseAddress (addressPart:_) = let addressTokens = split ':' addressPart
                                in parseTokens addressTokens
  where parseTokens :: [ByteString] -> Maybe Address
        parseTokens (protocol:host:port:_) = let maybePort = fromByteString port
                                      in (Address $ unpack host) <$> maybePort
        parseTokens _ = Nothing

parseKey :: [ByteString] -> Maybe PublicKey
parseKey (_:keyPart:_) = let keyTokens = split ':' keyPart
                         in parseTokens keyTokens
  where parseTokens :: [ByteString] -> Maybe PublicKey
        parseTokens (_:key:_) = parsePublicKey key
        parseTokens _ = Nothing
parseKey _ = Nothing


parseDiscoveryMessage :: ByteString -> Maybe DiscoveryMessage
parseDiscoveryMessage discoveryData =
  let splittedPayload = split '~' discoveryData
      maybeAddress = parseAddress splittedPayload
      maybeKey = parseKey splittedPayload
  in DiscoveryMessage <$> maybeAddress <*> maybeKey
