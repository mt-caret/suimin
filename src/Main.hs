{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Config (Config (..), readConfig)
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import qualified Control.Monad.Writer.Strict as W
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import qualified Dhall as D
import Feed (buildFeed, writeFeed)
import qualified Network.Wai.Application.Static as WS
import Network.Wai.Handler.Warp (run)
import qualified Post
import Post (Graph, PostData, addLinksToMetadata, buildLinkGraph, buildSlugLookup, expandLinks, getPostData, queryLinkGraph)
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Shared as PS
import qualified Text.Pandoc.Walk as PW
import Util ((.*), getTitleText, runPandocIO, safeHead, traceM, uniqAsc, uniqDesc, unwrap)
import Text.Show.Pretty (pPrint, ppShow)

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

readDoc :: P.ReaderOptions -> FilePath -> Action P.Pandoc
readDoc readerOptions srcPath = runPandocIO $ do
  content <- liftIO $ T.readFile srcPath
  P.readMarkdown readerOptions content

documentToMetadata :: P.Pandoc -> P.Meta
documentToMetadata (P.Pandoc metadata _) = metadata

readMetadata :: P.ReaderOptions -> FilePath -> Action P.Meta
readMetadata = fmap documentToMetadata .* readDoc

isDraft :: P.Meta -> Bool
isDraft metadata =
  case P.lookupMeta (T.pack "draft") metadata of
    Nothing -> False
    Just (P.MetaBool b) -> b
    Just m -> error $ "expected MetaBool for 'draft' but found: " ++ (ppShow m)

canPublish :: P.Meta -> Bool
canPublish = not . isDraft

getCategory :: P.Meta -> String
getCategory metadata =
  case P.lookupMeta (T.pack "category") metadata of
    Nothing -> "uncategorized"
    Just (P.MetaString category) -> T.unpack category
    Just (P.MetaInlines inlines) -> T.unpack $ PS.stringify inlines
    Just m -> error $ "expected MetaString for 'category' but found: " ++ (ppShow m)

extractTag :: P.MetaValue -> String
extractTag (P.MetaString tag) = T.unpack tag
extractTag (P.MetaInlines inlines) = T.unpack $ PS.stringify inlines

getTags :: P.Meta -> [String]
getTags metadata =
  case P.lookupMeta (T.pack "tags") metadata of
    Nothing -> []
    Just (P.MetaList tagMetavalues) -> extractTag <$> tagMetavalues
    Just m -> error $ "expected MetaList for 'tags' but found: " ++ (ppShow m)

writePandoc :: P.WriterOptions -> FilePath -> P.Pandoc -> P.PandocIO ()
writePandoc writerOptions dstPath document = do
  html <- P.writeHtml5String writerOptions document
  liftIO $ T.writeFile dstPath html

buildPost ::
  P.ReaderOptions ->
  P.WriterOptions ->
  Graph [PostData] ->
  (P.Pandoc -> Action P.Pandoc) ->
  FilePath ->
  FilePath ->
  Action ()
buildPost readerOptions writerOptions links walk srcPath dstPath = do
  (P.Pandoc metadata blocks) <- readDoc readerOptions srcPath >>= walk
  let newMetadata = addLinksToMetadata links metadata
  liftIO $ pPrint newMetadata
  runPandocIO $ writePandoc writerOptions dstPath (P.Pandoc newMetadata blocks)

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

--TODO: this sorts correctly because we put the date first in the filename,
--possibly should sort by date in metadata?
getPostPaths :: Action [FilePath]
getPostPaths = reverse . sort <$> getDirectoryFiles "" ["posts//*.md"]

rules :: Rules ()
rules = do
  let base = "_build"

  getConfig <- newCache $ \() -> do
    let configPath = "./config.dhall"
    need [configPath]
    liftIO $ readConfig configPath

  action $ getPostPaths >>= need . map (\p -> base </> p -<.> "html")

  getMetadata <- newCache $ readMetadata readerOptions
  let getMetadatas :: [FilePath] -> Action [P.Meta]
      getMetadatas = traverse getMetadata

  action $ do
    config <- getConfig ()
    when (enableCategories config) $ do
      postPaths <- getPostPaths
      categories <- uniqAsc . map getCategory <$> getMetadatas postPaths
      let bp c = base </> "category" </> c
      need $ categories >>= (\c -> [bp c <.> "html", bp c <.> "xml"])
    when (enableTags config) $ do
      postPaths <- getPostPaths
      tags <- uniqAsc . concat . map getTags <$> getMetadatas postPaths
      let bp t = base </> "tag" </> t
      need $ tags >>= (\t -> [bp t <.> "html", bp t <.> "xml"])

  want . fmap (base </>) $ ["index.html", "atom.xml"]

  getTemplate <- newCache $
    runPandocIO
      . \case
        Nothing -> P.compileDefaultTemplate $ T.pack "html5"
        Just path -> compileTemplate path

  getSlugLookup <- newCache $ \() -> do
    postPaths <- getPostPaths
    traceM $ buildSlugLookup getMetadata postPaths

  getLinkGraph <- newCache $ \() -> do
    postPaths <- getPostPaths
    slugLookup <- getSlugLookup ()
    traceM $ buildLinkGraph getMetadata slugLookup readerOptions postPaths

  (base </> "posts/*.html") %> \out -> do
    let src = dropDirectory1 $ out -<.> "md"
    need [src]
    template <- getTemplate $ Just "templates/post.html"
    config <- getConfig ()
    postData <- getPostData getMetadata src
    links <- queryLinkGraph postData <$> getLinkGraph ()
    liftIO . putStrLn $ out ++ " -> " ++ ppShow links
    slugLookup <- getSlugLookup ()
    buildPost readerOptions (writerOptions template) links (expandLinks slugLookup) src out

  (base </> "index.html") %> \out -> do
    let templatePath = "templates/index.html"
    template <- getTemplate $ Just templatePath
    postPaths <- getPostPaths
    let relPaths = map (\path -> path -<.> "html") postPaths
    let buildIndex = buildIndexMetadata "index" (const True)
    metadata <-
      buildIndex . filter (canPublish . snd) . zip relPaths
        <$> getMetadatas postPaths
    let document = P.Pandoc metadata []
    runPandocIO $ writePandoc (writerOptions template) out document

  (base </> "category/*.html") %> \out -> do
    let category = takeBaseName out
    template <- getTemplate $ Just "templates/index.html"
    postPaths <- getPostPaths
    let relPaths = map (\path -> "../" </> path -<.> "html") postPaths
    let buildIndex = buildIndexMetadata category ((== category) . getCategory)
    metadata <-
      buildIndex . filter (canPublish . snd) . zip relPaths
        <$> getMetadatas postPaths
    let document = P.Pandoc metadata []
    runPandocIO $ writePandoc (writerOptions template) out document

  (base </> "category/*.xml") %> \out -> do
    let category = takeBaseName out
    postPaths <- getPostPaths
    metadata <-
      filter (\m -> canPublish m && category == getCategory m)
        <$> getMetadatas postPaths
    config <- getConfig ()
    let feed = buildFeed config ("category" </> category <.> "xml") metadata postPaths
    writeFeed out feed

  (base </> "tag/*.html") %> \out -> do
    let tag = takeBaseName out
    template <- getTemplate $ Just "templates/index.html"
    postPaths <- getPostPaths
    let relPaths = map (\path -> "../" </> path -<.> "html") postPaths
    let buildIndex = buildIndexMetadata tag (any (== tag) . getTags)
    metadata <-
      buildIndex . filter (canPublish . snd) . zip relPaths
        <$> getMetadatas postPaths
    let document = P.Pandoc metadata []
    runPandocIO $ writePandoc (writerOptions template) out document

  (base </> "tag/*.xml") %> \out -> do
    let tag = takeBaseName out
    postPaths <- getPostPaths
    metadata <-
      filter (\m -> canPublish m && any (== tag) (getTags m))
        <$> getMetadatas postPaths
    config <- getConfig ()
    let feed = buildFeed config ("tag" </> tag <.> "xml") metadata postPaths
    writeFeed out feed

  (base </> "atom.xml") %> \out -> do
    postPaths <- getPostPaths
    metadata <- filter canPublish <$> getMetadatas postPaths
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
