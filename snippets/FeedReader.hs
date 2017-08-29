{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Main where

import Data.Int (Int64)
import GHC.Generics
import System.Environment
import Ssb.Message
import Data.ByteString.Lazy.Char8 as LBS hiding (foldl)
import Data.Aeson
import Ssb.Feed as Feed
import qualified Data.Map as Map
import Prelude hiding (sequence)
import Data.Maybe(catMaybes)

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

--type FeedMap = Map.Map HashType (Message Value)
type Feeds = Map.Map FeedLink (Feed Value)
type LastIds = [(Int64, HashType)]

buildFeedMap :: [Frame] -> Feeds
buildFeedMap = foldl mergeStuff (Map.fromList [])
  where mergeStuff :: Feeds -> Frame -> Feeds
        mergeStuff feeds frame = let feedId = author $ value frame
                                 in Map.alter (updateFeed frame) feedId feeds
        updateFeed :: Frame -> Maybe (Feed Value) -> Maybe (Feed Value)
        updateFeed frame (Just feed) = Just $ add feed (value frame)
        updateFeed frame Nothing = let message = value frame
                                    in Just $ Feed.empty $ author message

printFeedReport :: FeedLink -> Feed Value -> IO ()
printFeedReport feedId feed = Prelude.putStrLn $ "feed: " ++ feedId ++ " " ++ (show $ Feed.length feed)

printLastIds :: FeedLink -> (Int64, HashType) -> IO ()
printLastIds feedId (seq, mid) = Prelude.putStrLn $ "feed: " ++ feedId ++ " " ++ (show $ seq) ++ " " ++ mid

calculateLastIds :: Feeds -> LastIds
calculateLastIds feeds = catMaybes $ (snd <$> Map.toList maybeIds) where
  maybeIds = fmap getLastMessage feeds
  getLastMessage feed = fmap (\msg -> (sequence msg, author msg)) (Feed.lastMessage feed)

main :: IO ()
main = do
  args <- getArgs
  chain <- LBS.readFile $ Prelude.head args
  let feeds = either (const (Map.fromList [])) buildFeedMap (parseFrames chain)

  let lastIds = calculateLastIds feeds
  mapM_ print lastIds
  return ()
