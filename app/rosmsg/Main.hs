module Main (main) where

import Robotics.ROS.Pkg (Package(..), PackageMeta(..), messageList)
import System.FilePath (takeBaseName)
import System.Environment (getArgs)
import Data.Text (unpack)

main :: IO ()
main = do
    args <- getArgs
    let arg1 = args !! 1
    case take 1 args of
        ["list"] -> messageList >>= mapM_ printMessages
        _ -> putStrLn "USAGE: rosmsg <list>"
  where printMessages (pkg, msgs) = mapM_ (putStrLn . format pkg) msgs
        format p m = unpack (pkgName (meta p)) ++ "/" ++ takeBaseName m
