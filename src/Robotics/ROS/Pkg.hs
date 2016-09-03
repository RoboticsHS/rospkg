module Robotics.ROS.Pkg
  ( packageList
  , package
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

import Robotics.ROS.Pkg.Parser
import Robotics.ROS.Pkg.Types

-- |Take a full package list
packageList :: IO [Package]
packageList = do
    paths <- splitOn ":" <$> getEnv "ROS_PACKAGE_PATH"
    concat <$> mapM search paths
  where package_xml b d = joinPath [b, d, "package.xml"]
        search d = do
            candidates <- fmap (package_xml d) <$> getDirectoryContents d
            rights <$> mapConcurrently parse candidates

-- |Take a package by name
package :: PkgName -> IO (Maybe Package)
package name = listToMaybe . filter comp <$> packageList
  where comp p = pkgName (meta p) == name
