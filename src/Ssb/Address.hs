module Ssb.Address where

data Address = Address {host :: String, port :: Int} deriving (Show, Eq)

