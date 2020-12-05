{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Jira.API.Types.User where

import           Jira.API.Types.Avatar

import           Control.Lens
import           Data.Aeson

newtype AccountId = AccountId String deriving (Show, Eq)

data User = User { _userId          :: AccountId
                 , _userAvatars     :: AvatarUrls
                 , _userDisplayName :: String
                 , _userIsActive    :: Bool
                 }

makeLenses ''User

instance Eq User where
  a == b = (a^.userId) == (b^.userId)

instance Ord User where
  a `compare` b = (a^.userDisplayName) `compare` (b^.userDisplayName)

instance Show User where
  show u = u^.userDisplayName

instance FromJSON User where
  parseJSON = withObject "Expected object" $ \o ->
    User <$> (AccountId <$> o .: "accountId")
         <*> o .: "avatarUrls"
         <*> o .: "displayName"
         <*> o .: "active"
