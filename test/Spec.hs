module Main (main) where

import           Jira.API

import           Control.Lens
import qualified Data.UUID              as UUID
import           System.Random          (randomIO)
import qualified Test.HUnit             as HUnit

import           Control.Monad.IO.Class (liftIO)
import           System.Exit            (exitFailure, exitSuccess)
import           System.Environment     (getEnv)

projectKey :: ProjectKey
projectKey = ProjectKey "TEST"

testCreateIssue :: [Label] -> JiraConfig -> HUnit.Test
testCreateIssue labels config = HUnit.TestLabel "issue" . HUnit.TestCase $ do
  let issueTypeName = IssueTypeName "Bug"
      summary = "summary of the issue"
  (issueInfo, issueById, issueByKey) <- runJira' config $ do
    issueInfo <- createIssue $ IssueCreationData projectKey issueTypeName summary labels
    issueById <- getIssue $ issueInfo ^. icrId
    issueByKey <- getIssue $ issueInfo ^. icrKey
    return (issueInfo, issueById, issueByKey)
  let checkIssue (identifier, issue) = do
        let message = "Returned issue by " ++ identifier
        HUnit.assertEqual (message ++ " (id)") (issueInfo ^. icrId) (issue ^. iId)
        HUnit.assertEqual (message ++ " (key)") (issueInfo ^. icrKey) (issue ^. iKey)
        let IssueKey resultProjectKey _ = issueInfo ^. icrKey
        HUnit.assertEqual (message ++ " (project key)") resultProjectKey (ProjectKey "TEST")
        HUnit.assertEqual (message ++ " (type)") "Bug" (issue ^. (iType . itName))
        HUnit.assertEqual (message ++ " (project)") "TEST" (issue ^. (iProject . pName))
        HUnit.assertEqual (message ++ " (summary)") summary (issue ^. iSummary)
        HUnit.assertEqual (message ++ " (description)") Nothing (issue ^. iDescription)
        HUnit.assertEqual (message ++ " (status)") Backlog (issue ^. iStatus)
        HUnit.assertEqual (message ++ " (labels)") labels (issue ^. iLabels)
  mapM_ checkIssue [("ID", issueById), ("key", issueByKey)]
  runJira' config $ deleteIssue (issueById ^. iId)

testSearchIssues :: JiraConfig -> HUnit.Test
testSearchIssues config =
  HUnit.TestLabel "searchIssues" . HUnit.TestCase . runJira' config $ do
    label1 <- liftIO $ UUID.toString <$> randomIO
    label2 <- liftIO $ UUID.toString <$> randomIO
    withLabelInfo <- createIssue $ IssueCreationData projectKey (IssueTypeName "Bug") "summary" [label1, label2]
    issuesLabel1 <- searchIssues' ("label in " ++ label1)
    liftIO $ HUnit.assertEqual "Search issue by label (1)" [withLabelInfo ^. icrId] ((^. iId) <$> issuesLabel1)
    deleteIssue (withLabelInfo ^. icrId)

main :: IO ()
main = do
  jiraUrl <- getEnv "JIRA_URL"
  jiraUsername <- getEnv "JIRA_USERNAME"
  jiraPassphrase <- getEnv "JIRA_PASSPHRASE"
  let jiraConfig = JiraConfig jiraUrl $ BasicAuthConfig jiraUsername jiraPassphrase
  counts <- HUnit.runTestTT $ HUnit.TestList [
      testCreateIssue [] jiraConfig,
      testCreateIssue ["label1"] jiraConfig,
      testSearchIssues jiraConfig
    ]
  if HUnit.errors counts > 0 || HUnit.failures counts > 0 then
    exitFailure
  else
    exitSuccess
