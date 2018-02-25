{-# LANGUAGE RecordWildCards #-}

module Mother.Queue
  ( schedule
  ) where

import Mother.Internal.Types
import Mother.Internal.Operations
import Mother.Logger

import           Control.Monad        (void)
import qualified Data.Text            as Tx
import qualified Network.Wreq.Session as HTTP.S
import qualified System.Cron          as Sys.C

schedule :: Loggable -> [Config] -> IO ()
schedule lg config
  = sequence_ $ queue <$> config
  where
    queue Config{..}
      = do
          tk <- maybe (pure Nothing)
            (\(BearerAuthentication BearerAuthenticationContent{..}) ->
              authenticate bacUrl bacBody bacAccessKey
            )
            cAuthentication

          void $ Sys.C.execSchedule $ do
            let sh = Tx.unpack cSchedule

            Sys.C.addJob (healthCheckJob lg cHealthChecks) sh
            Sys.C.addJob (userStoriesJob lg tk cUserStorySteps) sh

healthCheckJob :: Loggable -> [Tx.Text] -> IO ()
healthCheckJob lg b
  = void $ HTTP.S.withSession $ \sess ->
      traverse (\url ->
        loggableCall lg sess Nothing GET url Nothing) b

userStoriesJob :: Loggable -> Maybe Tx.Text -> [Step] -> IO ()
userStoriesJob lg tk b
  = void $ HTTP.S.withSession $ \sess ->
      traverse (\Step{..} ->
        loggableCall lg sess tk sMethod sUrl sBody) b
