{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Jira.API.Types.Project where

import           Jira.API.Types.Avatar
import           Jira.API.Types.Classes

import           Control.Lens           (makeLenses)
import           Data.Aeson
import           Data.String (IsString)
import           Text.Read               (readMaybe)

newtype ProjectId = ProjectId Int deriving (Show, Eq, Ord)

instance FromJSON ProjectId where
  parseJSON v = do
    s <- parseJSON v
    i <- maybe (fail "Invalid issue ID") return $ readMaybe s
    return $ ProjectId i

instance ToJSON ProjectId where
  toJSON (ProjectId i) = toJSON $ show i

class UrlIdentifier a => ProjectIdentifier a where
  projectId :: a -> String
  projectId = urlId

  projectIdType :: IsString s => a -> s

instance UrlIdentifier ProjectId where
  urlId (ProjectId i) = show i

instance ProjectIdentifier ProjectId where
  projectIdType _ = "id"

newtype ProjectKey = ProjectKey String deriving (Show, Eq, Ord)

instance FromJSON ProjectKey where
  parseJSON v = ProjectKey <$> parseJSON v

instance ToJSON ProjectKey where
  toJSON (ProjectKey k) = toJSON k

instance UrlIdentifier ProjectKey where
  urlId (ProjectKey k) = k

instance ProjectIdentifier ProjectKey where
  projectIdType _ = "key"

data Project = Project { _pId      :: ProjectId
                       , _pKey     :: ProjectKey
                       , _pName    :: String
                       , _pAvatars :: AvatarUrls
                       } deriving (Eq, Show)

instance FromJSON Project where
  parseJSON = withObject "Expected Object" $ \o ->
    Project <$> o .: "id"
            <*> o .: "key"
            <*> o .: "name"
            <*> o .: "avatarUrls"

makeLenses ''Project
