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
    siteName :: String,
    hostName :: String,
    relativePath :: Maybe String
  }
  deriving (Generic, Show)

instance D.FromDhall Config

readConfig :: FilePath -> IO Config
readConfig = D.input D.auto . T.pack

siteRoot :: Config -> FilePath
siteRoot config =
  case relativePath config of
    Nothing -> hostName config
    Just relPath -> hostName config </> relPath
