module Util where

import Control.Monad.IO.Class
import qualified Data.Set as S
import qualified Data.Text as T
import Development.Shake
import qualified Data.Text.IO as T
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Shared as PS
import Text.Show.Pretty (ppShow)

unwrap :: (Show e, MonadFail m) => Either e a -> m a
unwrap (Left e) = fail $ show e
unwrap (Right x) = return x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

uniqAsc :: Ord a => [a] -> [a]
uniqAsc = S.toAscList . S.fromList

uniqDesc :: Ord a => [a] -> [a]
uniqDesc = S.toDescList . S.fromList

infix 8 .* -- weaker than (.)

(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*) = (.) . (.)

getTitleText :: P.Meta -> T.Text
getTitleText = PS.stringify . P.docTitle

getDateText :: P.Meta -> T.Text
getDateText = PS.stringify . P.docDate

documentToMetadata :: P.Pandoc -> P.Meta
documentToMetadata (P.Pandoc metadata _) = metadata

runPandocIO :: MonadIO m => P.PandocIO a -> m a
runPandocIO io =
  liftIO $ P.runIO io >>= unwrap

readDoc :: P.ReaderOptions -> FilePath -> Action P.Pandoc
readDoc readerOpts srcPath = runPandocIO $ do
  content <- liftIO $ T.readFile srcPath
  P.readMarkdown readerOpts content

readMetadata :: P.ReaderOptions -> FilePath -> Action P.Meta
readMetadata = fmap documentToMetadata .* readDoc

isDraft :: P.Meta -> Bool
isDraft metadata =
  case P.lookupMeta (T.pack "draft") metadata of
    Nothing -> False
    Just (P.MetaBool b) -> b
    Just m -> error $ "expected MetaBool for 'draft' but found: " ++ ppShow m

canPublish :: P.Meta -> Bool
canPublish = not . isDraft

traceM :: Show a => Action a -> Action a
traceM mx = do
  x <- mx
  putVerbose $ ppShow x
  return x
