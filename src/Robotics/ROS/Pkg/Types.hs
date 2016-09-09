-- |
-- Module      :  Robotics.ROS.Pkg.Types
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- This module contains common used types.
--
module Robotics.ROS.Pkg.Types where

import Data.Text (Text)

-- | ROS package information
data Package = Package
  { path :: FilePath    -- ^ Package absolute path
  , meta :: PackageMeta -- ^ Package meta information 
  } deriving (Show, Eq, Ord)

-- | Package name type
type PkgName = Text

-- | Common used package information
data PackageMeta = PackageMeta 
  { pkgName         :: PkgName   -- ^ Name
  , pkgVersion      :: Text      -- ^ Version
  , pkgDescription  :: Text      -- ^ Description
  , pkgLicense      :: Text      -- ^ License
  , pkgBuildDeps    :: [PkgName] -- ^ List of build dependencies
  , pkgRunDeps      :: [PkgName] -- ^ List of run dependencies
  } deriving (Show, Eq, Ord)
