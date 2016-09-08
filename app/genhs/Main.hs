{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main (main) where

import Robotics.ROS.Pkg

import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad.Logger
import Stack.Types.PackageName
import Stack.Types.StackT
import Stack.Types.Config
import Stack.Config
import Stack.New

createPackage :: PkgName -> IO ()
createPackage name = do
    manager  <- newTLSManager
    config   <- runStackLoggingT manager LevelDebug False False $
        loadConfig mempty Nothing Nothing 

    runStackT manager LevelDebug (lcConfig config) False False $ do
        n <- parsePackageName (T.map sanitize name)
        p <- new (NewOpts n False Nothing mempty) False 
        liftIO (print p)
  where sanitize '_' = '-'
        sanitize x   = x

main :: IO ()
main = do
    Just pkg <- package "std_msgs"
    msgs     <- pkgMessages pkg
    createPackage (pkgName (meta pkg))

