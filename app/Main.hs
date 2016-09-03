module Main (main) where

import System.Environment (getArgs)
import Data.Text (pack, unpack)
import Robotics.ROS.Pkg

main :: IO ()
main = do
    args <- getArgs
    let arg1 = args !! 1
    case take 1 args of
        ["list"] -> do
            pkgs <- packageList
            mapM_ (\p -> putStrLn (unpack (pkgName (meta p)) ++ " " ++ path p)) pkgs 
        ["find"] -> do
            pkg <- package (pack arg1)
            case pkg of
                Just p -> putStrLn (path p)
                Nothing -> putStrLn $ "Error: package '" ++ arg1 ++ "' not found"
        ["deps"] -> do
            pkg <- package (pack arg1)
            case pkg of
                Just p -> do
                    putStrLn "Build depend:"
                    print (pkgBuildDeps (meta p))
                    putStrLn "Run depend:"
                    print (pkgRunDeps (meta p))
                Nothing -> putStrLn $ "Error: package '" ++ arg1 ++ "' not found"
        _ -> putStrLn "USAGE: rospkg (<list> | <find> [package] | <deps> [package])"
