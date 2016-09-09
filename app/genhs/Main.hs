{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main (main) where

import Robotics.ROS.Pkg
import System.IO.Temp
import Template
import Stack

main :: IO ()
main = withSystemTempDirectory "genhs" $ \tmpDir -> do
    Just pkg <- package "geometry_msgs"
    msgs     <- pkgMessages pkg
    template <- newTemplate tmpDir pkg msgs
    let packageName = pkgName (meta pkg)
     in newPackage tmpDir packageName template
