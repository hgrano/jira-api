{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Jira.API.Types.Issue where

import           Jira.API.Types.Classes
import           Jira.API.Types.IssueType
import           Jira.API.Types.Project
import           Jira.API.Types.Status
import           Jira.API.Types.User

import           Control.Lens             (makeLenses, non, to, (^.))
import           Data.Aeson
import           Data.List                (elemIndex, intercalate)
import           Text.Read                (readMaybe)

class UrlIdentifier a => IssueIdentifier a where
  issueId :: a -> String
  issueId = urlId

newtype IssueId = IssueId Int deriving (Show, Eq, Ord)

instance FromJSON IssueId where
  parseJSON v = do
    s <- parseJSON v
    i <- maybe (fail "Invalid issue ID") return $ readMaybe s
    return $ IssueId i

instance UrlIdentifier IssueId where
  urlId (IssueId n) = show n

instance IssueIdentifier IssueId

newtype IssueNumber = IssueNumber Int deriving (Show, Eq, Ord)

instance UrlIdentifier IssueNumber where
  urlId (IssueNumber n) = show n

data IssueKey = IssueKey ProjectKey IssueNumber deriving (Eq, Ord)

instance FromJSON IssueKey where
  parseJSON v = do
    k <- parseJSON v
    hyphenIdx <- getOrInvalidProjectKey $ elemIndex '-' k
    let (proj, n) = splitAt hyphenIdx k
    num <- getOrInvalidProjectKey (if null n then Nothing else readMaybe $ tail n)
    return $ IssueKey (ProjectKey proj) (IssueNumber num)

    where getOrInvalidProjectKey = maybe (fail "Invalid project key") return

instance Show IssueKey where
  show (IssueKey (ProjectKey key) (IssueNumber n)) =
    key ++ "-" ++ show n

instance UrlIdentifier IssueKey where
  urlId = show

instance IssueIdentifier IssueKey

type Label = String

data IssueCreationData p = IssueCreationData { _icProject :: p
                                             , _icType    :: IssueTypeIdentifier
                                             , _icSummary :: String
                                             , _icLabels  :: [Label]
                                             } deriving (Show, Eq)

makeLenses ''IssueCreationData

instance ProjectIdentifier p => ToJSON (IssueCreationData p) where
 toJSON issueCreation = object [ "fields" .= fields ]
   where fields = object [ "project"   .= object [ projectIdType proj .= projectId proj ]
                         , "issuetype" .= (issueCreation^.icType)
                         , "summary"   .= (issueCreation^.icSummary)
                         , "labels"    .= toJSON (issueCreation^.icLabels)
                         ]
         proj = issueCreation^.icProject

data IssueCreationResponse = IssueCreationResponse { _icrId  :: IssueId
                                                   , _icrKey :: IssueKey
                                                   } deriving (Show, Eq)

makeLenses ''IssueCreationResponse

instance FromJSON IssueCreationResponse where
  parseJSON = withObject "Expected object" $ \o ->
    IssueCreationResponse <$> o .: "id"
                          <*> o .: "key"

data Issue = Issue { _iId          :: IssueId
                   , _iKey         :: IssueKey
                   , _iType        :: IssueType
                   , _iProject     :: Project
                   , _iSummary     :: String
                   , _iDescription :: Maybe String
                   , _iAssignee    :: Maybe User
                   , _iReporter    :: User
                   , _iStatus      :: Status
                   , _iLabels      :: [Label]
                   }

makeLenses ''Issue

instance Show Issue where
  show i = unlines
    [ "Id: " ++ urlId (i^.iId)
    , "Project: " ++ i^.iProject.pName
    , "Type: " ++ i^.iType.itName
    , "Summary: " ++ i^.iSummary
    , "Description: " ++ i^.iDescription.non "(No description)"
    , "Assignee: " ++ i^.iAssignee.to (maybe "Unassigned" show)
    , "Reporter: " ++ i^.iReporter.to show
    , "Status: " ++ i^.iStatus.to show
    , "Labels: " ++ intercalate "," (i^.iLabels)
    ]

instance Eq Issue where
  a == b = (a^.iId) == (b^.iId)

instance Ord Issue where
  compare a b = (a^.iKey) `compare` (b^.iKey)

instance FromJSON Issue where
  parseJSON = withObject "Expected object" $ \o -> do
    fields <- o .: "fields"
    Issue <$> o .: "id"
          <*> o .: "key"
          <*> fields .: "issuetype"
          <*> fields .: "project"
          <*> fields .: "summary"
          <*> fields .: "description"
          <*> fields .: "assignee"
          <*> fields .: "reporter"
          <*> fields .: "status"
          <*> fields .: "labels"

newtype IssuesResponse = IssuesResponse [Issue]

instance FromJSON IssuesResponse where
  parseJSON = withObject "Expected object" $ \o ->
    IssuesResponse <$> o .: "issues"

newtype CreateIssueMetadata = CreateIssueMetadata [(Project, [IssueType])]
                              deriving (Show, Eq)

instance FromJSON CreateIssueMetadata where
  parseJSON = withObject "Expected object" $ \o -> do
    projects <- o .: "projects"
    CreateIssueMetadata <$> mapM parseProject projects
    where
      parseProject po = do
        issueType <- po .: "issuetypes"
        project   <- parseJSON (Object po)
        return (project, issueType)
