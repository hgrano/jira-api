{-# LANGUAGE OverloadedStrings #-}

module Jira.API.Types.Status where

import           Data.Aeson
import qualified Data.CaseInsensitive as CI

data Status = Open
            | Backlog
            | InProgress
            | Resolved
            | Closed
            | CustomStatus String
            deriving (Eq, Ord)

instance Show Status where
  show Open             = "Open"
  show Backlog          = "Backlog"
  show InProgress       = "In Progress"
  show Resolved         = "Resolved"
  show Closed           = "Closed"
  show (CustomStatus s) = s

instance FromJSON Status where
  parseJSON = withObject "Expected object" $ \o -> do
    statusName <- o .: "name"
    return $ case CI.mk statusName of
      "Open"        -> Open
      "Backlog"     -> Backlog
      "In Progress" -> InProgress
      "Resolved"    -> Resolved
      "Closed"      -> Closed
      _             -> CustomStatus statusName
