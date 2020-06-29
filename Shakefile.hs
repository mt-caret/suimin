{-# Language LambdaCase #-}
{-# Language ApplicativeDo #-}
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except (throwError)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import qualified Text.Atom.Feed as A
import qualified Text.Pandoc as P

unwrap :: (Show e, MonadFail m) => Either e a -> m a
unwrap (Left error) = fail $ show error
unwrap (Right x) = return x

runPandocIO :: MonadIO m => P.PandocIO a -> m a
runPandocIO io =
  liftIO $ P.runIO io >>= unwrap

readerOptions :: P.ReaderOptions
readerOptions = P.def
  { P.readerExtensions = P.pandocExtensions
  }

writerOptions :: P.Template T.Text -> P.WriterOptions
writerOptions template = P.def
  { P.writerTemplate = Just template
  }

readMetadata :: P.ReaderOptions -> FilePath -> Action P.Meta
readMetadata readerOptions srcPath = runPandocIO $ do
  content <- liftIO $ T.readFile srcPath
  (P.Pandoc metadata _) <- P.readMarkdown readerOptions content
  return metadata

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

buildIndexMetadata :: [ (FilePath, P.Meta) ] -> P.Meta
buildIndexMetadata=
  P.Meta
    . M.insert (T.pack "title") (P.MetaString (T.pack "index"))
    . M.singleton (T.pack "posts")
      . P.MetaList
      . fmap (\(path, meta) ->
          P.MetaMap
          . M.insert (T.pack "href") (P.MetaString (T.pack path))
          . P.unMeta $ meta)

main :: IO ()
main = do
  let base = "_build"
  let getPostPaths = getDirectoryFiles "" [ "posts//*.md" ]
  shakeArgs shakeOptions { shakeFiles = "_shake" } $ do
    action $ do
      postPaths <- getPostPaths
      need $ map (\postPath -> base </> postPath -<.> "html") postPaths
    want [ base </> "index.html" ]

    getTemplate <- newCache $ runPandocIO .
      \case
        Nothing -> P.compileDefaultTemplate $ T.pack "html5"
        Just path -> compileTemplate path

    (base </> "posts/*.html") %> \out -> do
      let src = dropDirectory1 $ out -<.> "md"
      need [ src ]
      template <- getTemplate Nothing
      buildPost readerOptions (writerOptions template) src out

    (base </> "index.html") %> \out -> do
      let templatePath = "index-template.html"
      postPaths <- getPostPaths
      need (templatePath : postPaths)
      template <- getTemplate $ Just templatePath
      metadata <-
        buildIndexMetadata <$> traverse
          (\path -> (,) (path -<.> "html") <$> readMetadata readerOptions path) postPaths
      let document = P.Pandoc metadata []
      runPandocIO $ writePandoc (writerOptions template) out document

    phony "clean" $ do
        putInfo "Cleaning files in _build"
        removeFilesAfter base ["//*.html"]

    phony "serve" $ do
      cmd_ "python -m http.server --directory _build"

{- TODO:
 - * [x] show index of all posts
 - * [ ] static assets (css/images/js/etc.)
 - * [ ] create atom feed
 - * [ ] new post generation
 - * [ ] syntax highlighting?
 - * [ ] drafts
 - * [ ] watch
 -}
