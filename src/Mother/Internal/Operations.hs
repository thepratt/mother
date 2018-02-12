module Mother.Internal.Operations where

import Mother.Internal.Types
import Mother.Internal.Instances


import qualified Control.Exception         as E
import qualified Data.Aeson                as JSON
import qualified Data.ByteString           as BS
import qualified Data.Text                 as Tx
import qualified Data.Yaml                 as Y
import           Network.Wreq              hiding (
                                             get, post, put,
                                             delete, statusCode,
                                             responseStatus)
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
