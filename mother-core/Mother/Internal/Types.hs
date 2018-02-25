module Mother.Internal.Types where

import qualified Data.Aeson                as JSON
import qualified Data.Text                 as Tx
import           Network.HTTP.Client       (HttpExceptionContent)
import           Network.HTTP.Types.Status (Status)

type Token
  = Tx.Text

data Method
  = GET
  | POST
  | PUT

  deriving (Eq, Show)

type Url
  = Tx.Text

data Step
  = Step
      { sTitle  :: Tx.Text
      , sMethod :: Method
      , sUrl    :: Url
      , sBody   :: Maybe JSON.Object
      }

  deriving (Eq, Show)

type Schedule = Tx.Text

data Authentication
  = BearerAuthentication BearerAuthenticationContent

  deriving (Eq, Show)

data BearerAuthenticationContent
  = BearerAuthenticationContent
      { bacUrl       :: Url
      , bacBody      :: JSON.Object
      , bacAccessKey :: Tx.Text
      }

  deriving (Eq, Show)

data Config
  = Config
      { cSchedule       :: Schedule
      , cAuthentication :: Maybe Authentication
      , cHealthChecks   :: [Url]
      , cUserStorySteps :: [Step]
      }

  deriving (Eq, Show)

data RequestFailure
  = RequestFailed HttpExceptionContent
  | InvalidUrl Tx.Text Tx.Text

  deriving (Show)

type ErrorOrStatusResponse
  = Either RequestFailure Status
