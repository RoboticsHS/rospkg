module Robotics.ROS.Pkg.Types where

import Data.Text (Text)

data Package = Package
  { path :: FilePath
  , meta :: PackageMeta 
  } deriving (Show, Eq, Ord)

type PkgName = Text

data PackageMeta = PackageMeta 
  { pkgName         :: PkgName
  , pkgVersion      :: Text
  , pkgDescription  :: Text
  , pkgLicense      :: Text
  , pkgBuildDeps    :: [PkgName]
  , pkgRunDeps      :: [PkgName]
  } deriving (Show, Eq, Ord)
