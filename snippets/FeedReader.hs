{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Main where

import GHC.Generics
import System.Environment
import Ssb.Message
import Data.ByteString.Lazy.Char8 as LBS hiding (foldl)
import Data.Aeson
import qualified Data.Map as Map

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

type FeedMap = Map.Map HashType (Message Value)
type Feeds = Map.Map FeedLink FeedMap

buildFeedMap :: [Frame] -> Feeds
buildFeedMap = foldl mergeStuff (Map.fromList [])
  where mergeStuff :: Feeds -> Frame -> Feeds
        mergeStuff feeds frame = let feedId = author $ value frame
                                 in Map.alter (updateFeed frame) feedId feeds
        updateFeed :: Frame -> Maybe FeedMap -> Maybe FeedMap
        updateFeed frame (Just feed) = Just $ Map.insert (key frame) (value frame) feed
        updateFeed frame Nothing = let message = value frame
                                    in Just $ Map.fromList [(key frame, message)]

printFeedReport :: FeedLink -> FeedMap -> IO ()
printFeedReport feedId feed = Prelude.putStrLn $ "feed: " ++ feedId ++ " " ++ (show $ Map.size feed)

main :: IO ()
main = do
  args <- getArgs
  chain <- LBS.readFile $ Prelude.head args
  let feeds = either (const (Map.fromList [])) buildFeedMap (parseFrames chain)
  let numberOfFeeds = Map.size feeds
  Prelude.putStrLn $ "Number of feeds: " ++ (show numberOfFeeds)
  Map.traverseWithKey printFeedReport feeds
  return ()
