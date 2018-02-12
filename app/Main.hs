{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Mother               as M

import           Control.Monad
import           Control.Monad.Trans
import           Data.Traversable
import qualified Data.Text            as Tx
import           Data.Void
import qualified Network.Wreq.Session as HTTP.S

main :: IO ()
main
  = do
      config <- pure M.parse

      case config of
        Nothing               -> liftIO $ print "I have nothing to do!"
        Just M.Config{..} ->
          HTTP.S.withSession $ \session -> do
            void $
              traverse (\url -> do
                res <- M.call session (Tx.append "Check status of " url) M.GET url Nothing
                print res
              ) statusChecks

            void $
              traverse (\M.Step{..} -> do
                res <- M.call session title method url body
                print res
              ) steps

      print "Happy mother?!"
