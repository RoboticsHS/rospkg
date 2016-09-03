module Robotics.ROS.Pkg
  ( package
  , packageList
  , PackageMeta(..)
  , Package(..)
  , PkgName
  ) where

import Control.Concurrent.Async (mapConcurrently)
import System.Directory (getDirectoryContents)
import System.Environment (getEnv)
import System.FilePath (joinPath)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Data.Either (rights)
import Data.Text (unpack)

import Robotics.ROS.Pkg.Parser
import Robotics.ROS.Pkg.Types

-- |Take a package by name
package :: PkgName -> IO (Maybe Package)
package name = listToMaybe <$> find (const (return [unpack name])) 

-- |Take a full package list
packageList :: IO [Package]
packageList = find getDirectoryContents

-- |Take a package list with content getter 
find :: (FilePath -> IO [FilePath]) -> IO [Package] 
find content = do
    paths <- splitOn ":" <$> getEnv "ROS_PACKAGE_PATH"
    concat <$> mapM go paths
  where package_xml b d = joinPath [b, d, "package.xml"]
        go d = do candidates <- fmap (package_xml d) <$> content d
                  rights <$> mapConcurrently parse candidates
