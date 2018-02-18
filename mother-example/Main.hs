{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Mother              as M

import           Control.Monad       (void)
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString     as BS
import qualified System.Exit         as Sys.Ex
import qualified System.Environment  as Sys.Env

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
      cfg <- pure $ M.parse config

      case cfg of
        Nothing -> liftIO $ putStrLn "I have nothing to do!"
        Just c  -> M.schedule M.logFailure [c]

      putStrLn "Press any key to exit."
      void $ liftIO getChar
      putStrLn "Exiting."
