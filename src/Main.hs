{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import qualified Dhall as D
import GHC.Generics
import qualified Network.Wai.Application.Static as WS
import Network.Wai.Handler.Warp (run)
import qualified Text.Atom.Feed as A
import qualified Text.Atom.Feed.Export as AE
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Shared as PS

unwrap :: (Show e, MonadFail m) => Either e a -> m a
unwrap (Left error) = fail $ show error
unwrap (Right x) = return x

runPandocIO :: MonadIO m => P.PandocIO a -> m a
runPandocIO io =
  liftIO $ P.runIO io >>= unwrap

readerOptions :: P.ReaderOptions
readerOptions =
  P.def
    { P.readerExtensions = P.pandocExtensions
    }

writerOptions :: P.Template T.Text -> P.WriterOptions
writerOptions template =
  P.def
    { P.writerTemplate = Just template
    }

readMetadata :: P.ReaderOptions -> FilePath -> Action P.Meta
readMetadata readerOptions srcPath = runPandocIO $ do
  content <- liftIO $ T.readFile srcPath
  (P.Pandoc metadata _) <- P.readMarkdown readerOptions content
  return metadata

isDraft :: P.Meta -> Bool
isDraft metadata =
  case P.lookupMeta (T.pack "draft") metadata of
    Nothing -> False
    Just (P.MetaBool b) -> b
    Just m -> error $ "expected MetaBool for 'draft' but found: " ++ (show m)

canPublish :: P.Meta -> Bool
canPublish = not . isDraft

getCategory :: P.Meta -> String
getCategory metadata =
  case P.lookupMeta (T.pack "category") metadata of
    Nothing -> "uncategorized"
    Just (P.MetaString category) -> T.unpack category
    Just (P.MetaInlines inlines) -> T.unpack $ PS.stringify inlines
    Just m -> error $ "expected MetaString for 'category' but found: " ++ (show m)

extractTag :: P.MetaValue -> String
extractTag (P.MetaString tag) = T.unpack tag
extractTag (P.MetaInlines inlines) = T.unpack $ PS.stringify inlines

getTags :: P.Meta -> [String]
getTags metadata =
  case P.lookupMeta (T.pack "tags") metadata of
    Nothing -> []
    Just (P.MetaList tagMetavalues) -> extractTag <$> tagMetavalues
    Just m -> error $ "expected MetaList for 'tags' but found: " ++ (show m)

writePandoc :: P.WriterOptions -> FilePath -> P.Pandoc -> P.PandocIO ()
writePandoc writerOptions dstPath document = do
  html <- P.writeHtml5String writerOptions document
  liftIO $ T.writeFile dstPath html

buildPost :: P.ReaderOptions -> P.WriterOptions -> FilePath -> FilePath -> Action ()
buildPost readerOptions writerOptions srcPath dstPath = runPandocIO $ do
  content <- liftIO $ T.readFile srcPath
  document@(P.Pandoc metadata _) <- P.readMarkdown readerOptions content
  liftIO $ print metadata
  writePandoc writerOptions dstPath document

compileTemplate :: P.PandocMonad m => FilePath -> m (P.Template T.Text)
compileTemplate path = do
  template <- P.getTemplate path
  (P.runWithPartials $ P.compileTemplate path template) >>= \case
    Left error -> throwError $ P.PandocTemplateError (T.pack error)
    Right x -> return x

buildIndexMetadata :: String -> (P.Meta -> Bool) -> [(FilePath, P.Meta)] -> P.Meta
buildIndexMetadata title predicate =
  P.Meta
    . M.insert (T.pack "title") (P.MetaString (T.pack title))
    . M.singleton (T.pack "posts")
    . P.MetaList
    . fmap
      ( \(path, meta) ->
          P.MetaMap
            . M.insert (T.pack "href") (P.MetaString (T.pack path))
            . P.unMeta
            $ meta
      )
    . filter (predicate . snd)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

toEntry :: FilePath -> P.Meta -> A.Entry
toEntry fullPath metadata =
  ( A.nullEntry
      (T.pack fullPath)
      (A.TextString (PS.stringify (P.docTitle metadata)))
      (PS.stringify (P.docDate metadata))
  )
    { A.entryAuthors =
        (\author -> A.nullPerson {A.personName = PS.stringify author})
          <$> P.docAuthors metadata
    }

buildFeed :: Config -> FilePath -> [P.Meta] -> [FilePath] -> A.Feed
buildFeed config atomPath metadata postPaths =
  ( A.nullFeed
      (T.pack (blogRoot config </> atomPath))
      (A.TextString (T.pack (blogName config)))
      (fromMaybe (T.pack "") . safeHead . fmap (PS.stringify . P.docDate) $ metadata)
  )
    { A.feedEntries = zipWith toEntry fullPostPaths metadata,
      A.feedLinks = [A.nullLink (T.pack (blogRoot config))]
    }
  where
    fullPostPaths = map (\p -> blogRoot config </> p) postPaths

writeFeed :: MonadIO m => FilePath -> A.Feed -> m ()
writeFeed path = liftIO . T.writeFile path . TL.toStrict . fromJust . AE.textFeed

data Config = Config
  { port :: Maybe D.Natural,
    enableCategories :: Bool,
    enableTags :: Bool,
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

uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

rules :: Rules ()
rules = do
  let base = "_build"
  let getPostPaths = getDirectoryFiles "" ["posts//*.md"]

  getConfig <- newCache $ \() -> do
    let configPath = "./config.dhall"
    need [configPath]
    liftIO $ readConfig configPath

  action $ getPostPaths >>= need . map (\p -> base </> p -<.> "html")

  action $ do
    config <- getConfig ()
    when (enableCategories config) $ do
      postPaths <- getPostPaths
      categories <-
        uniq . map getCategory
          <$> traverse (readMetadata readerOptions) postPaths
      let bp c = base </> "category" </> c
      need $ categories >>= (\c -> [bp c <.> "html", bp c <.> "xml"])
    when (enableTags config) $ do
      postPaths <- getPostPaths
      tags <-
        uniq . concat . map getTags
          <$> traverse (readMetadata readerOptions) postPaths
      let bp t = base </> "tag" </> t
      need $ tags >>= (\t -> [bp t <.> "html", bp t <.> "xml"])

  want . fmap (base </>) $ ["index.html", "atom.xml"]

  getTemplate <- newCache $
    runPandocIO
      . \case
        Nothing -> P.compileDefaultTemplate $ T.pack "html5"
        Just path -> compileTemplate path

  (base </> "posts/*.html") %> \out -> do
    let src = dropDirectory1 $ out -<.> "md"
    let templatePath = "templates/post.html"
    need [src, templatePath]
    template <- getTemplate (Just templatePath)
    buildPost readerOptions (writerOptions template) src out

  (base </> "index.html") %> \out -> do
    let templatePath = "templates/index.html"
    postPaths <- getPostPaths
    need $ templatePath : postPaths
    template <- getTemplate $ Just templatePath
    metadata <-
      (buildIndexMetadata "index" (const True) . filter (canPublish . snd))
        <$> traverse
          (\path -> (,) (path -<.> "html") <$> readMetadata readerOptions path)
          postPaths
    let document = P.Pandoc metadata []
    runPandocIO $ writePandoc (writerOptions template) out document

  (base </> "category/*.html") %> \out -> do
    let templatePath = "templates/index.html"
    let category = takeBaseName out
    postPaths <- getPostPaths
    need $ templatePath : postPaths
    template <- getTemplate $ Just templatePath
    metadata <-
      (buildIndexMetadata category ((== category) . getCategory) . filter (canPublish . snd))
        <$> traverse
          (\path -> (,) ("../" </> path -<.> "html") <$> readMetadata readerOptions path)
          postPaths
    let document = P.Pandoc metadata []
    runPandocIO $ writePandoc (writerOptions template) out document

  (base </> "category/*.xml") %> \out -> do
    let category = takeBaseName out
    postPaths <- reverse . sort <$> getPostPaths
    need postPaths
    metadata <-
      filter (\m -> canPublish m && category == getCategory m)
        <$> traverse (readMetadata readerOptions) postPaths
    config <- getConfig ()
    let feed = buildFeed config ("category" </> category <.> "xml") metadata postPaths
    writeFeed out feed

  (base </> "tag/*.html") %> \out -> do
    let templatePath = "templates/index.html"
    let tag = takeBaseName out
    postPaths <- getPostPaths
    need $ templatePath : postPaths
    template <- getTemplate $ Just templatePath
    metadata <-
      (buildIndexMetadata tag (any (== tag) . getTags) . filter (canPublish . snd))
        <$> traverse
          (\path -> (,) ("../" </> path -<.> "html") <$> readMetadata readerOptions path)
          postPaths
    let document = P.Pandoc metadata []
    runPandocIO $ writePandoc (writerOptions template) out document

  (base </> "tag/*.xml") %> \out -> do
    let tag = takeBaseName out
    postPaths <- reverse . sort <$> getPostPaths
    need postPaths
    metadata <-
      filter (\m -> canPublish m && any (== tag) (getTags m))
        <$> traverse (readMetadata readerOptions) postPaths
    config <- getConfig ()
    let feed = buildFeed config ("tag" </> tag <.> "xml") metadata postPaths
    writeFeed out feed

  (base </> "atom.xml") %> \out -> do
    postPaths <- reverse . sort <$> getPostPaths
    need postPaths
    metadata <-
      filter canPublish
        <$> traverse (readMetadata readerOptions) postPaths
    config <- getConfig ()
    let feed = buildFeed config ("atom.xml") metadata postPaths
    writeFeed out feed

  phony "clean" $ do
    putInfo "Cleaning files in _build"
    removeFilesAfter base ["//*.html", "//*.xml"]

  phony "serve" $ do
    config <- getConfig ()
    let httpServerPort = fromMaybe 8000 . fmap fromIntegral $ port config
    liftIO . putStrLn $ "Running HTTP server on port " ++ show httpServerPort
    liftIO . run httpServerPort . WS.staticApp $ WS.defaultFileServerSettings base

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = "_shake"} rules

{- TODO:
 - * [x] show index of all posts
 - * [ ] static assets (css/images/js/etc.)
 - * [x] create atom feed
 - * [ ] new post generation
 - * [ ] syntax highlighting?
 - * [x] drafts
 - * [ ] watch
 - * [x] categories
 - * [x] tags
 -}
