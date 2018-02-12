{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Mother               as M

import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString      as BS
import           Data.Traversable
import qualified Data.Text            as Tx
import           Data.Void
import qualified Network.Wreq.Session as HTTP.S
import           Text.RawString.QQ

main :: IO ()
main
  = do
      config <- pure $ M.parse config

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