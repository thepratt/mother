module Mother.Internal.Types where

import qualified Data.Aeson                as JSON
import qualified Data.Text                 as Tx
import           Network.HTTP.Client       (HttpExceptionContent)
import           Network.HTTP.Types.Status (Status)

data Method
  = GET
  | POST
  | PUT

  deriving (Eq, Show)

data Step
  = Step
      { sTitle  :: Tx.Text
      , sMethod :: Method
      , sUrl    :: Tx.Text
      , sBody   :: Maybe JSON.Object
      }

  deriving (Eq, Show)

type Schedule = Tx.Text

data Config
  = Config
      { cSchedule       :: Schedule
      , cHealthChecks   :: [Tx.Text]
      , cUserStorySteps :: [Step]
      }

  deriving (Eq, Show)

data RequestFailure
  = RequestFailed HttpExceptionContent
  | InvalidUrl Tx.Text Tx.Text

  deriving (Show)

type ErrorOrStatusResponse
  = Either RequestFailure Status
