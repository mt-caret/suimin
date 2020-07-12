{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Config (Config (..), readConfig)
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake
import Development.Shake.FilePath
import Feed (buildFeed, writeFeed)
import qualified Network.Wai.Application.Static as WS
import Network.Wai.Handler.Warp (run)
import Post
  ( Graph,
    PostData,
    addLinksToMetadata,
    buildLinkGraph,
    buildSlugLookup,
    expandLinks,
    getPostData,
    queryLinkGraph,
  )
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Shared as PS
import Text.Show.Pretty (pPrint, ppShow)
import Util
  ( canPublish,
    readDoc,
    readMetadata,
    runPandocIO,
    traceM,
    uniqAsc,
  )

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

getCategory :: P.Meta -> String
getCategory metadata =
  case P.lookupMeta (T.pack "category") metadata of
    Nothing -> "uncategorized"
    Just (P.MetaString category) -> T.unpack category
    Just (P.MetaInlines inlines) -> T.unpack $ PS.stringify inlines
    Just m -> error $ "expected MetaString for 'category' but found: " ++ ppShow m

getTags :: P.Meta -> [String]
getTags = extractTags . P.lookupMeta (T.pack "tags")
  where
    extractTags :: Maybe P.MetaValue -> [String]
    extractTags = \case
      Nothing -> []
      Just (P.MetaList tagMetavalues) -> extractTag <$> tagMetavalues
      Just m -> error $ "expected MetaList for 'tags' but found: " ++ ppShow m
    extractTag :: P.MetaValue -> String
    extractTag = \case
      (P.MetaString tag) -> T.unpack tag
      (P.MetaInlines inlines) -> T.unpack $ PS.stringify inlines
      m -> error $ "expected string-like value for tag but found: " ++ ppShow m

writePandoc :: P.WriterOptions -> FilePath -> P.Pandoc -> P.PandocIO ()
writePandoc writerOpts dstPath document = do
  html <- P.writeHtml5String writerOpts document
  liftIO $ T.writeFile dstPath html

buildPost ::
  P.ReaderOptions ->
  P.WriterOptions ->
  Graph [PostData] ->
  (P.Pandoc -> Action P.Pandoc) ->
  FilePath ->
  FilePath ->
  Action ()
buildPost readerOpts writerOpts links walk srcPath dstPath = do
  (P.Pandoc metadata blocks) <- readDoc readerOpts srcPath >>= walk
  let newMetadata = addLinksToMetadata links metadata
  liftIO $ pPrint newMetadata
  runPandocIO $ writePandoc writerOpts dstPath (P.Pandoc newMetadata blocks)

compileTemplate :: P.PandocMonad m => FilePath -> m (P.Template T.Text)
compileTemplate path = do
  template <- P.getTemplate path
  P.runWithPartials (P.compileTemplate path template) >>= \case
    Left e -> throwError $ P.PandocTemplateError (T.pack e)
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

filterPublishable :: (FilePath -> Action P.Meta) -> [FilePath] -> Action [FilePath]
filterPublishable getMetadata sourcePaths =
  map fst . filter (canPublish . snd) . zip sourcePaths
    <$> traverse getMetadata sourcePaths

--TODO: this sorts correctly because we put the date first in the filename,
--possibly should sort by date in metadata?
getPublishableSourcePaths :: (FilePath -> Action P.Meta) -> Action [FilePath]
getPublishableSourcePaths getMetadata = do
  sourcePaths <- getDirectoryFiles "" ["posts//*.md"]
  filterPublishable getMetadata sourcePaths

rules :: Rules ()
rules = do
  let base = "_build"

  getConfig <- newCache $ \() -> do
    let configPath = "./config.dhall"
    need [configPath]
    liftIO $ readConfig configPath

  getMetadata <- newCache $ readMetadata readerOptions
  let getMetadatas :: [FilePath] -> Action [P.Meta]
      getMetadatas = traverse getMetadata

  action $ do
    sourcePaths <- getPublishableSourcePaths getMetadata
    need $ map (\p -> base </> p -<.> "html") sourcePaths

  action $ do
    config <- getConfig ()
    when (enableCategories config) $ do
      sourcePaths <- getPublishableSourcePaths getMetadata
      categories <- uniqAsc . map getCategory <$> getMetadatas sourcePaths
      let bp c = base </> "category" </> c
      need $ concatMap (\c -> [bp c <.> "html", bp c <.> "xml"]) categories
    when (enableTags config) $ do
      sourcePaths <- getPublishableSourcePaths getMetadata
      tags <- uniqAsc . concatMap getTags <$> getMetadatas sourcePaths
      let bp t = base </> "tag" </> t
      need $ concatMap (\t -> [bp t <.> "html", bp t <.> "xml"]) tags

  want . fmap (base </>) $ ["index.html", "atom.xml"]

  getTemplate <- newCache $
    runPandocIO
      . \case
        Nothing -> P.compileDefaultTemplate $ T.pack "html5"
        Just path -> compileTemplate path

  getSlugLookup <- newCache $ \() -> do
    sourcePaths <- getPublishableSourcePaths getMetadata
    traceM $ buildSlugLookup getMetadata sourcePaths

  getLinkGraph <- newCache $ \() -> do
    sourcePaths <- getPublishableSourcePaths getMetadata
    slugLookup <- getSlugLookup ()
    traceM $ buildLinkGraph getMetadata slugLookup readerOptions sourcePaths

  (base </> "posts/*.html") %> \out -> do
    let source = dropDirectory1 $ out -<.> "md"
    need [source]
    template <- getTemplate $ Just "templates/post.html"
    postData <- getPostData getMetadata source
    links <- queryLinkGraph postData <$> getLinkGraph ()
    liftIO . putStrLn $ out ++ " -> " ++ ppShow links
    slugLookup <- getSlugLookup ()
    buildPost
      readerOptions
      (writerOptions template)
      links
      (expandLinks slugLookup)
      source
      out

  (base </> "index.html") %> \out -> do
    let templatePath = "templates/index.html"
    template <- getTemplate $ Just templatePath
    sourcePaths <- getPublishableSourcePaths getMetadata
    let relativeSourcePaths = map (-<.> "html") sourcePaths
    let buildIndex = buildIndexMetadata "index" (const True)
    metadata <-
      buildIndex . zip relativeSourcePaths <$> getMetadatas sourcePaths
    let document = P.Pandoc metadata []
    runPandocIO $ writePandoc (writerOptions template) out document

  (base </> "category/*.html") %> \out -> do
    let category = takeBaseName out
    template <- getTemplate $ Just "templates/index.html"
    sourcePaths <- getPublishableSourcePaths getMetadata
    let relativeSourcePaths =
          map (\path -> "../" </> path -<.> "html") sourcePaths
    let buildIndex = buildIndexMetadata category ((== category) . getCategory)
    metadata <-
      buildIndex . zip relativeSourcePaths <$> getMetadatas sourcePaths
    let document = P.Pandoc metadata []
    runPandocIO $ writePandoc (writerOptions template) out document

  (base </> "category/*.xml") %> \out -> do
    let category = takeBaseName out
    sourcePaths <- getPublishableSourcePaths getMetadata
    metadata <-
      filter ((== category) . getCategory) <$> getMetadatas sourcePaths
    config <- getConfig ()
    let feed =
          buildFeed
            config
            ("category" </> category <.> "xml")
            metadata
            sourcePaths
    writeFeed out feed

  (base </> "tag/*.html") %> \out -> do
    let tag = takeBaseName out
    template <- getTemplate $ Just "templates/index.html"
    sourcePaths <- getPublishableSourcePaths getMetadata
    let relativeSourcePaths =
          map (\path -> "../" </> path -<.> "html") sourcePaths
    let buildIndex = buildIndexMetadata tag (elem tag . getTags)
    metadata <-
      buildIndex . zip relativeSourcePaths <$> getMetadatas sourcePaths
    let document = P.Pandoc metadata []
    runPandocIO $ writePandoc (writerOptions template) out document

  (base </> "tag/*.xml") %> \out -> do
    let tag = takeBaseName out
    sourcePaths <- getPublishableSourcePaths getMetadata
    metadata <-
      filter (elem tag . getTags) <$> getMetadatas sourcePaths
    config <- getConfig ()
    let feed = buildFeed config ("tag" </> tag <.> "xml") metadata sourcePaths
    writeFeed out feed

  (base </> "atom.xml") %> \out -> do
    sourcePaths <- getPublishableSourcePaths getMetadata
    metadata <- getMetadatas sourcePaths
    config <- getConfig ()
    let feed = buildFeed config "atom.xml" metadata sourcePaths
    writeFeed out feed

  phony "clean" $ do
    putInfo "Cleaning files in _build"
    removeFilesAfter base ["//*.html", "//*.xml"]

  phony "serve" $ do
    config <- getConfig ()
    let httpServerPort = maybe 8000 fromIntegral $ port config
    liftIO $ do
      putStrLn $ "Running HTTP server on port " ++ show httpServerPort
      run httpServerPort . WS.staticApp $ WS.defaultFileServerSettings base

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = "_shake"} rules
