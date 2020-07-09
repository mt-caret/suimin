{-# LANGUAGE DeriveGeneric #-}

module Config where

import qualified Data.Text as T
import qualified Dhall as D
import Development.Shake.FilePath
import GHC.Generics

data Config = Config
  { port :: Maybe D.Natural,
    enableCategories :: Bool,
    enableTags :: Bool,
    enableBacklinks :: Bool,
    blogName :: String,
    hostName :: String,
    relativePath :: String
  }
  deriving (Generic, Show)

instance D.FromDhall Config

readConfig :: FilePath -> IO Config
readConfig = D.input D.auto . T.pack

blogRoot :: Config -> FilePath
blogRoot config = hostName config </> relativePath config
