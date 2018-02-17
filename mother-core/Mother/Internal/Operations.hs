module Mother.Internal.Operations where

import Mother.Internal.Types
import Mother.Internal.Instances ()

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
  -> IO (Either String Int)

call session _title _method _url _body
  = do
      let url = Tx.unpack _url
          jb   = JSON.toJSON _body

      print _title
      (Right <$> do
        req <- case _method of
          GET  -> get session  url
          POST -> post session  url jb
          PUT  -> put session  url jb

        pure $ statusCode $ responseStatus req)
      `E.catch` handler
  where
    handler :: HttpException -> IO (Either String Int)
    handler _ = pure $ Left "Shit broke man"
