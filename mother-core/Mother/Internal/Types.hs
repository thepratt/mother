module Mother.Internal.Types where

import qualified Data.Aeson as JSON
import qualified Data.Text  as Tx


data Method
  = GET
  | POST
  | PUT

  deriving (Eq, Show)

data Step
  = Step
      { title  :: Tx.Text
      , method :: Method
      , url    :: Tx.Text
      , body   :: Maybe JSON.Object
      }

  deriving (Eq, Show)

data Config
  = Config
      { statusChecks :: [Tx.Text]
      , steps        :: [Step]
      -- , frequency :: Int
      }

  deriving (Eq, Show)
