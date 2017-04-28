{-# LANGUAGE OverloadedStrings #-}

module Ssb.Key ( PublicKey
               , parsePublicKey
               , exportPublicKey
               ) where

import Data.ByteString
import Data.ByteString.Base64
import qualified Crypto.Sign.Ed25519 as Ed

newtype PublicKey = PublicKey {key :: Ed.PublicKey} deriving Show

parsePublicKey :: ByteString -> Maybe PublicKey
parsePublicKey encodedPublicKey =
  let decodedPublicKey = decode encodedPublicKey
      maybeEd25519PublicKey =
        either (const Nothing)
               (Just . Ed.PublicKey)
               decodedPublicKey
  in PublicKey <$> maybeEd25519PublicKey

exportPublicKey :: PublicKey -> ByteString
exportPublicKey publicKey =
  let extractedEdKey = key publicKey
      serializedKey = Ed.unPublicKey extractedEdKey
  in encode serializedKey
