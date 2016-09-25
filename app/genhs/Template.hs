{-# LANGUAGE OverloadedStrings #-}
module Template (newTemplate) where

import Data.Text (Text, unpack, pack,
                  toUpper, replace, takeEnd)
import qualified Data.Text as T
import Data.Text.Lazy.IO as TL
import Data.Text.Lazy.Builder
import Data.Maybe (catMaybes)
import Control.Monad (forM)
import System.FilePath
import Data.Monoid

import Robotics.ROS.Msg.Parser
import Robotics.ROS.Msg.Types
import Robotics.ROS.Pkg

newTemplate :: FilePath -> Package -> [FilePath] -> IO FilePath
newTemplate dir pkg msgFiles = do
    deps <- forM msgFiles $ \f -> do
        let msgPath = joinPath [path pkg, "msg", f]
        Done _ msg <- parse rosmsg <$> TL.readFile msgPath
        depPkgs    <- catMaybes <$> mapM package (pkgBuildDeps (meta pkg))
        deps       <- zip depPkgs <$> mapM pkgMessages depPkgs
        return $ concatMap (mkDeps msg) ((pkg, msgFiles) : deps)

    TL.writeFile tFileName $ toLazyText $ tBuilder deps
    return tFileName
  where
    tFileName     = joinPath [dir, unpack (pkgName (meta pkg)) ++ ".hsfiles"]
    tBuilder deps = cabalBuilder pkg msgFiles
                 <> mconcat (msgBuilder pkg <$> zip msgFiles deps)
                 <> stackBuilder pkg

upperFirst :: Text -> Text
upperFirst x  = toUpper (T.take 1 x) <> T.drop 1 x

-- List of custom type names in Message
customTypes :: MsgDefinition -> [Text]
customTypes = fmap pkgInTypeHook . catMaybes . fmap go
  where go (Variable (t, _))   = custom t
        go (Constant (t, _) _) = custom t
        custom (Custom t)                = Just t
        custom (Array (Custom t))        = Just t
        custom (FixedArray _ (Custom t)) = Just t
        custom  _ = Nothing
        pkgInTypeHook = last . T.split (== '/')

-- Associate used in message custom type with package
mkDeps :: MsgDefinition -> (Package, [FilePath]) -> [(Package, FilePath)]
mkDeps msg (pkg, files) = catMaybes (go <$> files)
  where go m | isCustomType m = Just (pkg, m)
             | otherwise      = Nothing
        messageName    = pack . takeWhile (/= '.')
        isCustomType m = messageName m `elem` customTypes msg

cabalBuilder :: Package -> [FilePath] -> Builder
cabalBuilder pkg msgs =
       "{-# START_FILE " <> fromText (sanitize name) <> ".cabal #-}"
    <> "\nname:                " <> fromText (sanitize name)
    <> "\nversion:             0.1"
    <> "\nsynopsis:            Autogenerated " <> fromText name <> " ROS messages"
    <> "\ndescription:         " <> fromText description
    <> "\nlicense:             BSD3"
    <> "\ncategory:            Robotics"
    <> "\nbuild-type:          Simple"
    <> "\ncabal-version:       >=1.10"
    <> "\n"
    <> "\nlibrary"
    <> "\n  hs-source-dirs:      src"
    <> "\n  default-language:    Haskell2010"
    <> "\n  build-depends:       base >= 4.7 && < 5"
    <> "\n                     , binary"
    <> "\n                     , pureMD5"
    <> "\n                     , bytestring"
    <> "\n                     , data-default"
    <> "\n                     , lens-family-core"
    <> mconcat (msgBuildDeps <$> pkgBuildDeps (meta pkg))
    <> "\n  exposed-modules:     "
    <> msgMainModule <> "." <> messageName (head msgs)
    <> mconcat (msgModule <$> drop 1 msgs)
    <> "\n\n{-# START_FILE Setup.hs #-}"
    <> "\nimport Distribution.Simple"
    <> "\nmain = defaultMain\n"

  where name        = pkgName (meta pkg)
        description = replace "\n" "" $ pkgDescription (meta pkg)
        sanitize    = replace "_" "-"
        messageName = fromString . takeWhile (/= '.')
        space       = "                     "
        msgBuildDeps "message_generation" = "\n" <> space <> ", rosmsg"
        msgBuildDeps dep =
            case takeEnd 4 dep of
                "msgs" -> "\n" <> space <> ", " <> fromText (sanitize dep)
                _      -> mempty
        msgMainModule = "Robotics.ROS." <> fromText (upperFirst name)
        msgModule m  = "\n" <> space <> ", " <> msgMainModule <> "."
                            <> messageName m

msgBuilder :: Package -> (FilePath, [(Package, FilePath)]) -> Builder
msgBuilder pkg (msgFile, deps) =
       "\n{-# START_FILE src/Robotics/ROS/" <> packageName pkg
                                            <> "/"
                                            <> messageName msgFile
                                            <> ".hs #-}"
    <> "\n{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric"
    <> "\n  , DeriveDataTypeable, QuasiQuotes, OverloadedStrings #-}"
    <> "\nmodule " <> msgMainModule <> "." <> messageName msgFile <> " where\n"
    <> "\nimport           Data.Default (Default(..))"
    <> "\nimport           Data.Typeable (Typeable)"
    <> "\nimport           GHC.Generics (Generic)"
    <> "\nimport           Data.Binary (Binary)"
    <> "\nimport           Data.Data (Data)"
    <> "\nimport qualified Data.ByteString as BS"
    <> "\nimport qualified Data.Word as W"
    <> "\nimport qualified Data.Int as I"
    <> "\nimport qualified Prelude as P"
    <> "\n"
    <> mconcat (msgImport <$> deps)
    <> "\n"
    <> "\nimport           Robotics.ROS.Msg.TH"
    <> "\nimport           Robotics.ROS.Msg"
    <> "\n"
    <> "\n[rosmsgFrom|" <> fromString (joinPath [path pkg, "msg", msgFile])
                        <> "|]\n"
  where packageName      = fromText . upperFirst . pkgName . meta
        messageName      = fromString . takeWhile (/= '.')
        msgMainModule    = "Robotics.ROS." <> packageName pkg
        msgImport (p, m) =
            "\nimport qualified Robotics.ROS."
                <> packageName p
                <> "."
                <> messageName m
                <> " as "
                <> messageName m

stackBuilder :: Package -> Builder
stackBuilder pkg =
    "\n{-# START_FILE stack.yaml #-}"
  <> "\nresolver: lts-6.16"
  <> "\npackages:"
  <> "\n- '.'"
  <> mconcat ((\p -> "\n- '../" <> fromText (sanitize p) <> "'") <$> deps)
  <> "\nextra-deps:"
  <> "\n- rosmsg-0.5.2.0"
  <> "\nflags: {}"
  <> "\nextra-package-dbs: []"
  where deps     = filter (elem "msgs" . T.tails) (pkgBuildDeps (meta pkg)) 
        sanitize = replace "_" "-"
