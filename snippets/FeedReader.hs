{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Main where

import Data.Int (Int64)
import GHC.Generics
import System.Environment
import Ssb.Message
import Data.ByteString.Lazy.Char8 as LBS hiding (foldl)
import Data.Aeson
import qualified Data.Map as Map
import Prelude hiding (sequence)

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
type LastIds = Map.Map FeedLink (Int64, HashType)

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

printLastIds :: FeedLink -> (Int64, HashType) -> IO ()
printLastIds feedId (seq, mid) = Prelude.putStrLn $ "feed: " ++ feedId ++ " " ++ (show $ seq) ++ " " ++ mid

calculateLastIds :: Feeds -> LastIds
calculateLastIds = fmap calculateLastId
  where calculateLastId :: FeedMap -> (Int64, HashType)
        calculateLastId = Map.foldWithKey keepLast (0, "")

        keepLast :: HashType -> (Message Value) -> (Int64, HashType) -> (Int64, HashType)
        keepLast messageId message old@(lastSeq, _)
          | lastSeq < sequence message = (sequence message, messageId)
          | otherwise = old

main :: IO ()
main = do
  args <- getArgs
  chain <- LBS.readFile $ Prelude.head args
  let feeds = either (const (Map.fromList [])) buildFeedMap (parseFrames chain)
  let numberOfFeeds = Map.size feeds
  Prelude.putStrLn $ "Number of feeds: " ++ (show numberOfFeeds)
  Map.traverseWithKey printFeedReport feeds

  let lastIds = calculateLastIds feeds
  Map.traverseWithKey printLastIds lastIds
  return ()
