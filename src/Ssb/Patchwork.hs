{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Ssb.Patchwork (
  PatchworkMessage(..),
  parsePatchwork
  ) where
import GHC.Generics
import Data.Aeson (Value(..), decode, Object)
import Data.Text
import Data.HashMap.Lazy (lookup)
import Prelude hiding (lookup)
data PatchworkMessage = Post {text :: Text} deriving (Show, Eq, Generic)

parsePatchwork :: Value -> Maybe PatchworkMessage
parsePatchwork (Object o) = Post <$> getText o
  where getText o = case lookup "text" o of
                      Just (String s) -> Just s
                      _ -> Nothing
parsePatchwork _ = Nothing

