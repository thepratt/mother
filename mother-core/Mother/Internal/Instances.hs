{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mother.Internal.Instances where

import Mother.Internal.Types

import qualified Data.Yaml as Y
import           Data.Yaml ((.:), (.:?))

instance Y.FromJSON Method where
  parseJSON (Y.String v)
    = case v of
        "GET"    -> pure GET
        "POST"   -> pure POST
        "PUT"    -> pure PUT
        _        -> fail "Invalid method"

  parseJSON _
    = fail "Invalid method value"

instance Y.FromJSON Step where
  parseJSON (Y.Object v)
    =   Step
    <$> v .: "title"
    <*> v .: "method"
    <*> v .: "url"
    <*> v .:? "body"

  parseJSON _
    = fail "Missing parameter for step"

instance Y.FromJSON Config where
  parseJSON (Y.Object v)
    =   Config
    <$> v .: "schedule"
    <*> v .: "health_checks"
    <*> v .: "user_story_steps"

  parseJSON _
    = fail "Missing parameter for config"
