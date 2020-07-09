module Util where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Shared as PS

unwrap :: (Show e, MonadFail m) => Either e a -> m a
unwrap (Left error) = fail $ show error
unwrap (Right x) = return x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

uniqAsc :: Ord a => [a] -> [a]
uniqAsc = S.toAscList . S.fromList

uniqDesc :: Ord a => [a] -> [a]
uniqDesc = S.toDescList . S.fromList

(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*) = (.) . (.)

getTitle :: P.Meta -> T.Text
getTitle = PS.stringify . P.docTitle
