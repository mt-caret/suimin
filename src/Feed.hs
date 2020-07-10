module Feed where

import Config (Config (..), blogRoot)
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake.FilePath
import qualified Data.Text.Lazy as TL
import qualified Text.Atom.Feed as A
import qualified Text.Atom.Feed.Export as AE
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Shared as PS
import Util (getTitleText, safeHead)

toEntry :: FilePath -> P.Meta -> A.Entry
toEntry fullPath metadata =
  ( A.nullEntry
      (T.pack fullPath)
      (A.TextString (getTitleText metadata))
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
