-- |
-- Module      :  Robotics.ROS.Pkg.Parser
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- This module contains simple 'TagSoup' based XML parser.
-- It used for parsing @package.xml@ file with common
-- ROS package information.
--
{-# LANGUAGE CPP #-}
module Robotics.ROS.Pkg.Parser (parse) where

import Text.StringLike (StringLike, toString)
import Data.ByteString as BS (readFile)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory)
import Text.HTML.TagSoup
import Data.Text (Text)

import Robotics.ROS.Pkg.Types

#ifdef FAST_PARSER
import Text.HTML.TagSoup.Fast
#else
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)

-- | Parse with default UTF8 encoding
parseTagsT :: ByteString -> [Tag Text]
parseTagsT = fmap (fmap decodeUtf8) . parseTags
#endif

-- | Parse package.xml file
parse :: FilePath -> IO (Either String Package)
parse pkgFile = do
    exist <- doesFileExist pkgFile
    if not exist
    then return (Left $ "No such file: " ++ pkgFile)
    else do content <- parseTagsT <$> BS.readFile pkgFile
            return (Package pkgDir <$> packageMeta content)
  where pkgDir = takeDirectory pkgFile

-- | Tag-based parser
packageMeta :: [Tag Text] -> Either String PackageMeta
packageMeta tags =
    PackageMeta <$> takeText "name"
                <*> takeText "version"
                <*> takeText "description"
                <*> takeText "license"
                <*> takeTexts "build_depend"
                <*> takeTexts "run_depend"
  where takeText   = fmap innerText . slice1 
        takeTexts  = fmap (fmap innerText) . sliceN tags 
        slice1     = fmap snd . slice' tags 
        sliceN t n = case slice' t n of
                        Left _ -> return []
                        Right (xs, r) -> do 
                                r' <- sliceN xs n
                                return (r : r')

-- | Slice tags from Open-tag to Close-tag with same name
slice' :: StringLike a => [Tag a] -> a -> Either String ([Tag a], [Tag a])
slice' tags tagName | length sliceTags > 0 = Right (freeTags, sliceTags) 
                    | otherwise = Left $ "Not found tag name: "
                                        ++ toString tagName
  where sliceTags = takeTags (dropTags tags) 
        freeTags  = drop (length sliceTags) (dropTags tags)
        dropTags  = dropWhile (not . isTagOpenName tagName)
        takeTags  = takeWhile (not . isTagCloseName tagName)
