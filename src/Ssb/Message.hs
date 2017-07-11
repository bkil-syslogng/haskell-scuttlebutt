{-# LANGUAGE DeriveGeneric #-}

module Ssb.Message where

import GHC.Generics
import Data.Aeson
import Data.Int (Int64)

type MessageLink = String
type FeedLink = String
type HashType = String
type Signature = String

data Message a = Message
  { previous :: Maybe MessageLink
  , author :: FeedLink
  , sequence :: Int64
  , timestamp :: Int64
  , hash :: HashType
  , content :: a
  , signature :: Signature
  } deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (Message a)
instance ToJSON a => ToJSON (Message a)

