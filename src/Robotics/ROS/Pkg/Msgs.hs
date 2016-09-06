module Robotics.ROS.Pkg.Msgs where

import System.FilePath (takeExtension, joinPath)
import System.Directory (doesDirectoryExist,
                         getDirectoryContents)

import Robotics.ROS.Pkg.Finder
import Robotics.ROS.Pkg.Types

-- |Take message files of given package
pkgMessages:: Package -> IO [FilePath]
pkgMessages pkg = do
    msgDirExist <- doesDirectoryExist msgDir
    if msgDirExist
    then do msgFiles <- getDirectoryContents msgDir
            return $ filter ((== ".msg") . takeExtension) msgFiles
    else return []
  where msgDir = joinPath [path pkg, "msg"]

-- |Search message files for all packages
messageList :: IO [(Package, [FilePath])]
messageList = do
    pkgs <- packageList
    msgs <- mapM pkgMessages pkgs
    return (zip pkgs msgs)
