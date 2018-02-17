{-# LANGUAGE RecordWildCards #-}

module Mother.Queue
  ( schedule
  ) where

import Mother.Internal.Types
import Mother.Internal.Operations

import           Control.Monad        (void)
import qualified Data.Text            as Tx
import qualified Data.Time            as T
import qualified Network.Wreq.Session as HTTP.S
import qualified System.Cron          as Sys.C

schedule :: [Config] -> IO ()
schedule config
  = sequence_ $ queue <$> config
  where
    queue Config{..}
      = void $ Sys.C.execSchedule $ do
          let sh = Tx.unpack cSchedule

          Sys.C.addJob (healthCheckJob cHealthChecks) sh
          Sys.C.addJob (userStoriesJob cUserStorySteps) sh

healthCheckJob :: [Tx.Text] -> IO ()
healthCheckJob b
  = void $ do
      now <- T.getCurrentTime

      putStrLn $ concat
        [ "["
        , show now
        , "] :: "
        , "Running health checks"
        ]

      HTTP.S.withSession $ \sess ->
        traverse (\url -> call sess url GET url Nothing) b

userStoriesJob :: [Step] -> IO ()
userStoriesJob b
  = void $ do
      now <- T.getCurrentTime

      putStrLn $ concat
        [ "["
        , show now
        , "] :: "
        , "Running user stories"
        ]

      HTTP.S.withSession $ \sess ->
        traverse (\Step{..} -> call sess sTitle sMethod sUrl sBody) b
