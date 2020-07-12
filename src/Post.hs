{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Post where

import qualified Control.Monad.Writer.Strict as W
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import qualified Network.URI.Encode as URI
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Shared as PS
import qualified Text.Pandoc.Walk as PW
import Util ((.*), readDoc, uniqDesc)

newtype Title = Title [P.Inline] deriving (Eq, Ord)

instance Show Title where
  show (Title inline) = "Title \"" ++ T.unpack (PS.stringify inline) ++ "\""

data PostData = PostData
  { postDataTitle :: Title,
    postDataSlug :: Maybe String
  }
  deriving (Show, Eq, Ord)

postDataToMetaValue :: PostData -> P.MetaValue
postDataToMetaValue (PostData (Title inlines) maybeSlug) =
  P.MetaMap . M.fromList $
    (T.pack "title", P.MetaInlines inlines)
      : case maybeSlug of
        Nothing -> []
        Just slug -> [(T.pack "slug", P.MetaString . T.pack $ slug)]

getPostData :: (FilePath -> Action P.Meta) -> FilePath -> Action PostData
getPostData getMetadata postPath = do
  let slug = takeBaseName postPath
  title <- Title . P.docTitle <$> getMetadata postPath
  return $ PostData title (Just slug)

getSlug :: PostData -> String
getSlug = \case
  (PostData _ (Just slug)) -> slug
  (PostData (Title inlines) Nothing) ->
    URI.encode . T.unpack $ PS.stringify inlines

getSrcPath :: PostData -> FilePath
getSrcPath postData = "posts" </> getSlug postData <.> "md"

getOutPath :: PostData -> FilePath
getOutPath postData = "posts" </> getSlug postData <.> "html"

type LinkMapping = M.Map PostData [PostData]

data Graph a = Graph
  { forward :: a,
    backward :: a
  }
  deriving (Show)

createLinkGraph :: LinkMapping -> Graph LinkMapping
createLinkGraph links =
  Graph links backlinks
  where
    backlinks = M.fromListWith (++) . map (fmap uniqDesc) $ do
      (post, linkedPosts) <- M.toList links
      linkedPost <- linkedPosts
      return (linkedPost, [post])

queryLinkGraph :: PostData -> Graph LinkMapping -> Graph [PostData]
queryLinkGraph postData (Graph links backlinks) =
  Graph
    { forward = M.findWithDefault [] postData links,
      backward = M.findWithDefault [] postData backlinks
    }

linksToMetaValue :: Graph [PostData] -> P.MetaValue
linksToMetaValue (Graph links backlinks) =
  P.MetaMap . M.fromList $
    [ (T.pack "links", P.MetaList $ map postDataToMetaValue links),
      (T.pack "backlinks", P.MetaList $ map postDataToMetaValue backlinks)
    ]

type SlugLookup = M.Map Title String

buildSlugLookup :: (FilePath -> Action P.Meta) -> [FilePath] -> Action SlugLookup
buildSlugLookup getMetadata postPaths = do
  M.fromList . map toTitleSlugPair <$> traverse (getPostData getMetadata) postPaths
  where
    toTitleSlugPair :: PostData -> (Title, String)
    toTitleSlugPair postData = (postDataTitle postData, getSlug postData)

type AccumLinks a = W.WriterT [PostData] Action a

liftAction :: Action a -> AccumLinks a
liftAction = W.WriterT . fmap (,mempty)

processLinks :: SlugLookup -> P.Pandoc -> AccumLinks P.Pandoc
processLinks slugLookup = PW.walkM expandLinkInline
  where
    expandLinkInline :: P.Inline -> AccumLinks P.Inline
    expandLinkInline = \case
      (P.Link alt inlines (url, title_)) | T.null url -> do
        W.tell [postData]
        return $ P.Link alt inlines (link, title_)
        where
          title = Title inlines
          slug = slugLookup !? title
          postData = PostData title slug
          link = T.pack . dropDirectory1 $ getOutPath postData
      x -> return x

expandLinks :: SlugLookup -> P.Pandoc -> Action P.Pandoc
expandLinks = fmap fst . W.runWriterT .* processLinks

extractLinks :: SlugLookup -> P.ReaderOptions -> FilePath -> Action [PostData]
extractLinks slugLookup readerOpts srcPath =
  readDoc readerOpts srcPath >>= extract
  where
    extract :: P.Pandoc -> Action [PostData]
    extract = fmap (uniqDesc . snd) . W.runWriterT . processLinks slugLookup

buildLinkGraph ::
  (FilePath -> Action P.Meta) ->
  SlugLookup ->
  P.ReaderOptions ->
  [FilePath] ->
  Action (Graph LinkMapping)
buildLinkGraph getMetadata slugLookup readerOpts postPaths = do
  postDatas <- traverse (getPostData getMetadata) postPaths
  links <- traverse (extractLinks slugLookup readerOpts) postPaths
  return . createLinkGraph . M.fromList $ zip postDatas links

addLinksToMetadata :: Graph [PostData] -> P.Meta -> P.Meta
addLinksToMetadata links =
  P.Meta . update . P.unMeta
  where
    update = M.insert (T.pack "links") $ linksToMetaValue links
