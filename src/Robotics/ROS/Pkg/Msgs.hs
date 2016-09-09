-- |
-- Module      :  Robotics.ROS.Pkg.Msgs
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- ROS messages placed on "msg" directory in package root. This
-- package contains its directory observer for given package.
--
module Robotics.ROS.Pkg.Msgs (
    -- ** Single package request
    pkgMessages
    -- ** All-in-one request
  , messageList
  ) where

import System.FilePath (takeExtension, joinPath)
import System.Directory (doesDirectoryExist,
                         getDirectoryContents)

import Robotics.ROS.Pkg.Finder
import Robotics.ROS.Pkg.Types

-- | Take message files of given package
pkgMessages :: Package -> IO [FilePath]
pkgMessages pkg = do
    msgDirExist <- doesDirectoryExist msgDir
    if msgDirExist
    then do msgFiles <- getDirectoryContents msgDir
            return $ filter ((== ".msg") . takeExtension) msgFiles
    else return []
  where msgDir = joinPath [path pkg, "msg"]

-- | Search message files for all packages.
--
-- Like to
--
-- @
--     packageList >>= mapM pkgMessages 
-- @
--
messageList :: IO [(Package, [FilePath])]
messageList = do
    pkgs <- packageList
    msgs <- mapM pkgMessages pkgs
    return (zip pkgs msgs)
