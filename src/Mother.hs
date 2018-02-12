{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Mother
  ( Method (..)
  , Step (..)
  , Config (..)

  , call
  , parse
  ) where

import qualified Control.Exception         as E
import           Control.Monad.Trans       (liftIO)
import qualified Data.Aeson                as JSON
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BS.L
import qualified Data.Text                 as Tx
import           Data.Void
import qualified Data.Yaml                 as Y
import           Data.Yaml                 ((.:), (.:?))
import           Control.Applicative
import           Network.Wreq              hiding (
                                             get, post, put,
                                             delete, statusCode,
                                             responseStatus)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Network.Wreq.Session
import           Text.RawString.QQ

import Prelude

data Method
  = GET
  | POST
  | PUT

  deriving (Eq, Show)

instance Y.FromJSON Method where
  parseJSON (Y.String v)
    = case v of
        "GET"    -> pure GET
        "POST"   -> pure POST
        "PUT"    -> pure PUT

  parseJSON _
    = fail "Invalid method value"

data Step
  = Step
      { title  :: Tx.Text
      , method :: Method
      , url    :: Tx.Text
      , body   :: Maybe JSON.Object
      }

  deriving (Eq, Show)

instance Y.FromJSON Step where
  parseJSON (Y.Object v)
    =   Step
    <$> v .: "title"
    <*> v .: "method"
    <*> v .: "url"
    <*> v .:? "body"

  parseJSON _
    = fail "Missing parameter for step"

data Config
  = Config
      { statusChecks :: [Tx.Text]
      , steps        :: [Step]
      -- , frequency :: Int
      }

  deriving (Eq, Show)

instance Y.FromJSON Config where
  parseJSON (Y.Object v)
    =   Config
    <$> v .: "status_checks"
    <*> v .: "steps"

  parseJSON _
    = fail "Missing parameter for config"

config :: BS.ByteString
config = [r|
status_checks:
  - https://google.com
  - https://habito.com
  - https://yahoo.com
steps:
  - title: Check google up
    method: GET
    url: https://google.com
  - title: Check google can thingo
    method: POST
    url: https://google.com
    body:
      thingo: 2
      important_information:
        - 1
        - 2
        - 3
|]

parse :: Maybe Config
parse
  = Y.decode config

call
  :: Session
  -> Tx.Text
  -> Method
  -> Tx.Text
  -> Maybe JSON.Object
  -> IO (Either String Int)

call session title method url body
  = do
      let sUrl = Tx.unpack url

      print title
      (Right <$> do
        req <- case method of
          GET  -> get session  sUrl
          POST -> post session  sUrl (JSON.toJSON body)
          PUT  -> put session  sUrl (JSON.toJSON body)

        pure $ statusCode $ responseStatus req)
      `E.catch` handler
  where
    handler :: HttpException -> IO (Either String Int)
    handler _ = pure $ Left "Shit broke man"
