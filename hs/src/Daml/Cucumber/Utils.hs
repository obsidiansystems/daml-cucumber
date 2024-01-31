module Daml.Cucumber.Utils (safeHead) where

safeHead :: [a] -> Maybe a
safeHead (a:_) = Just a
safeHead _ = Nothing
