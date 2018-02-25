{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Mother.Internal.Operations where

import Mother.Internal.Types
import Mother.Internal.Instances ()
import Mother.Logger

import qualified Control.Exception         as E
import           Control.Lens              ((^?), (&), (?~))
import qualified Data.Aeson                as JSON
import qualified Data.Aeson.Lens           as JSON.L
import qualified Data.ByteString           as BS
import qualified Data.Text                 as Tx
import qualified Data.Text.Encoding        as Tx
import qualified Data.Yaml                 as Y
import           Network.HTTP.Client       hiding (responseBody)
import           Network.HTTP.Types.Status
import qualified Network.Wreq              as Wq
import           Network.Wreq              hiding (getWith, post, postWith, putWith, responseStatus)
import           Network.Wreq.Session

parse :: BS.ByteString -> Maybe Config
parse
  = Y.decode

call
  :: Session
  -> Maybe Tx.Text
  -> Method
  -> Url
  -> Maybe JSON.Object
  -> IO ErrorOrStatusResponse

call sess token mtd url body
  = do
      let u    = Tx.unpack url
          jb   = JSON.toJSON body
          opts = case token of
            Just t  -> defaults & auth ?~ oauth2Bearer (Tx.encodeUtf8 t)
            Nothing -> defaults

      Right <$> do
        res <- case mtd of
          GET  -> getWith opts sess u
          POST -> postWith opts sess u jb
          PUT  -> putWith opts sess u jb

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
  -> Maybe Tx.Text
  -> Method
  -> Url
  -> Maybe JSON.Object
  -> IO ErrorOrStatusResponse

loggableCall lg sess token mtd url body
  = lg =<< call sess token mtd url body

authenticate :: Url -> JSON.Object -> Tx.Text -> IO (Maybe Tx.Text)
authenticate url body accessKey
  = do
      req <- Wq.post (Tx.unpack url) (JSON.toJSON body)
      pure $ req ^? responseBody . JSON.L.key accessKey . JSON.L._String
