{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Mother.Internal.Operations where

import Mother.Internal.Types
import Mother.Internal.Instances ()
import Mother.Logger

import qualified Control.Exception         as E
import qualified Data.Aeson                as JSON
import qualified Data.ByteString           as BS
import qualified Data.Text                 as Tx
import qualified Data.Yaml                 as Y
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Network.Wreq.Session

parse :: BS.ByteString -> Maybe Config
parse
  = Y.decode

call
  :: Session
  -> Tx.Text
  -> Method
  -> Tx.Text
  -> Maybe JSON.Object
  -> IO ErrorOrStatusResponse

call sess _ mtd url body
  = do
      let u  = Tx.unpack url
          jb = JSON.toJSON body

      Right <$> do
        res <- case mtd of
          GET  -> get sess u
          POST -> post sess u jb
          PUT  -> put sess u jb

        pure $ responseStatus res
      `E.catch` handler
  where
    handler :: HttpException -> IO (Either RequestFailure Status)
    handler =
      \case
        HttpExceptionRequest _ cxt  -> pure $ Left $ RequestFailed cxt
        InvalidUrlException fUrl why -> pure $ Left $ InvalidUrl (Tx.pack fUrl) (Tx.pack why)

loggableCall
  :: Loggable
  -> Session
  -> Tx.Text
  -> Method
  -> Tx.Text
  -> Maybe JSON.Object
  -> IO ErrorOrStatusResponse

loggableCall lg sess title mtd url body
  = lg =<< call sess title mtd url body
