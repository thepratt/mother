{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Mother               as M

import           Control.Monad        (void)
import           Control.Monad.Trans  (liftIO)
import qualified Data.ByteString      as BS
import qualified Data.Text            as Tx
import qualified Network.Wreq.Session as HTTP.S
import qualified System.Exit          as Sys.Ex
import qualified System.Environment   as Sys.Env

main :: IO ()
main
  = Sys.Env.getArgs >>= parseArgs >>= BS.readFile >>= run

parseArgs :: [String] -> IO String
parseArgs
  = \case
      ["-h"] -> usage >> exit
      [f]    -> pure f
      _      -> usage >> exit
  where
    usage
      = putStrLn "Usage: mother -- [file]"

    exit
      = Sys.Ex.exitWith Sys.Ex.ExitSuccess

run :: BS.ByteString -> IO ()
run config
  = do
      config <- pure $ M.parse config

      case config of
        Nothing           -> liftIO $ print "I have nothing to do!"
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
