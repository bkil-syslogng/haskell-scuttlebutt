{-# LANGUAGE OverloadedStrings #-}

module Ssb.Feed (empty, add, get, getOrderedContents, Feed) where

import Ssb.Message
import qualified Data.Map as Map
import Prelude hiding (last, sequence)

data Feed a = Feed { name :: FeedLink
                   , messages :: Map.Map HashType (Message a)
                   , last :: Maybe (Message a)
                   }
                   deriving (Show, Eq)

empty :: FeedLink -> Feed a
empty name = Feed name (Map.fromList []) Nothing

add :: Feed a -> Message a -> Feed a
add feed msg = feed { messages = Map.insert (hash msg) msg (messages feed)
                    , last = Just $ older (last feed) msg
                    }
               where older :: (Maybe (Message a)) -> (Message a) -> (Message a)
                     older Nothing x = x
                     older (Just l) r = if sequence l < sequence r then r else l

get :: Feed a -> MessageLink -> Maybe (Message a)
get feed link = Map.lookup link (messages feed)

getOrderedContents :: Feed a -> [a]
getOrderedContents feed = retrieveFrom (last feed) []
                          where retrieveFrom Nothing acc = acc
                                retrieveFrom (Just message) acc =
                                  let currentContent = (content message):acc
                                  in case previous message of
                                    Nothing -> currentContent
                                    Just prevId -> retrieveFrom (get feed prevId) currentContent
