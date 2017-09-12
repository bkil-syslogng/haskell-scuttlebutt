module Ssb.Patchwork (
  PatchworkMessage,
  parsePatchwork
  ) where

import Data.Aeson (Value)

data PatchworkMessage = Post deriving (Show)

parsePatchwork :: Value -> Maybe PatchworkMessage
parsePatchwork _ = Just Post

