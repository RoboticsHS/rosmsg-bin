{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main (main) where

import System.Environment
import Robotics.ROS.Pkg
import Data.Text (pack)
import System.IO.Temp
import Template
import Stack

main :: IO ()
main = withSystemTempDirectory "genhs" $ \tmpDir -> do
    args <- getArgs
    case args of
        [name] -> do
            putStr $ "Search package `" ++ name ++"`..."
            Just pkg <- package (pack name)
            putStrLn "DONE"

            msgs     <- pkgMessages pkg
            putStrLn "Found messages:"
            mapM_ (putStrLn . (" - " ++)) msgs

            template <- newTemplate tmpDir pkg msgs
            putStrLn $ "Template created: " ++ template

            msgPkg <- newPackage tmpDir (pkgName (meta pkg)) template
            putStrLn $ "Package created: " ++ msgPkg
        _ -> putStrLn "USAGE: genhs [PACKAGE_NAME]"
