module Main where

import Control.Monad
import Control.Monad.Trans
import Data.Traversable
import Data.Void
import qualified Network.Wreq.Session as HTTP.S

-- import Control.Monad.IO.Class (liftIO)

import qualified Mother as M

main :: IO ()
main
  = do
      config <- pure M.parse

      case config of
        Nothing               -> liftIO $ print "I have nothing to do!"
        Just (M.Config steps) ->
          HTTP.S.withSession $ \session ->
            void $
              traverse (\s@(M.Step title _ _ _) -> do
                res <- M.parent session s
                print res
              ) steps

      print "Happy mother?!"
