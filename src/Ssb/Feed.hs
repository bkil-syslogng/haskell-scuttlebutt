{-# LANGUAGE OverloadedStrings #-}

module Ssb.Feed (empty, add, get, getOrderedContents, Feed) where

import Ssb.Message
import qualified Data.Map as Map

data Feed a = Feed { name :: FeedLink,
                   messages :: Map.Map HashType (Message a)}
                   deriving (Show, Eq)

empty :: FeedLink -> Feed a
empty name = Feed name (Map.fromList [])

add :: Feed a -> Message a -> Feed a
add feed msg = feed { messages = Map.insert (hash msg) msg (messages feed)}

get :: Feed a -> MessageLink -> Maybe (Message a)
get feed link = Map.lookup link (messages feed)

getOrderedContents :: Feed a -> [a]
getOrderedContents feed = fmap (content.snd) $ Map.toList $ messages feed

