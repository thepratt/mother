{-# LANGUAGE LambdaCase #-}

module Mother.Logger
  ( Loggable

  , logFailure
  ) where

import Mother.Internal.Types

import Control.Monad.Trans (liftIO)
import qualified Data.Time            as T

type Loggable
  = ErrorOrStatusResponse -> IO ErrorOrStatusResponse

logFailure :: Loggable
logFailure
  = \case
      sc@(Right _) -> pure sc
      rf@(Left e)  -> do
        now <- T.getCurrentTime
        liftIO $ putStrLn (show now ++ " :: " ++ show e)
        pure rf
