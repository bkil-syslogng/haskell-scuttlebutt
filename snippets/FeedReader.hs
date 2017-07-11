{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Main where

import GHC.Generics
import System.Environment
import Ssb.Message
import Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson

type Timestamp = Integer
data Frame = Frame { key :: HashType,
                     value :: (Message Value),
                     timestamp :: Timestamp
                   }
    deriving (Show, Eq, Generic)
instance FromJSON Frame

parseFrames :: ByteString -> Either String [Frame]
parseFrames str =
  eitherDecode str

main :: IO ()
main = do
  args <- getArgs
  chain <- LBS.readFile $ Prelude.head args
  print $ parseFrames chain