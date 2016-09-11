{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Stack (newPackage)  where

import qualified Data.Text as T
import Control.Monad.Logger

import Stack.Types.TemplateName
import Stack.Types.PackageName
import Stack.Types.StackT
import Stack.Types.Config
import Stack.Config
import Stack.New

newPackage :: FilePath -> T.Text -> String -> IO FilePath
newPackage dir name templatePath = do
    case parseTemplateNameFromString templatePath of
        Left e -> error e
        Right template -> do manager <- newTLSManager
                             config  <- (loadConfigWith manager :: IO Config) 
                             runStackWith manager config $ do
                                n <- parsePackageName (sanitize name)
                                new (NewOpts n False (Just template) mempty) False 
                                return (T.unpack (sanitize name))
  where
    loadConfigWith m = lcConfig <$>
        runStackLoggingT m logging False False
            (loadConfig mempty Nothing Nothing) 
    runStackWith m c = runStackT m logging c False False
    logging  = LevelDebug
    sanitize = T.replace "_" "-"
