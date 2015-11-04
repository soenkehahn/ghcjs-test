{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified GHC.Generics
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           System.Environment
import           System.FilePath
import qualified System.Logging.Facade as Log
import           WaiAppStatic.Types
import           WithCli

data Options
  = Options {
    production :: Bool
  }
  deriving (GHC.Generics.Generic, HasArguments)

instance Generic Options
instance HasDatatypeInfo Options

main :: IO ()
main = withCli $ \ options -> do
  jsDir <- (</> "ghcjs-test-client.jsexe") <$>
    dropFileName <$> getExecutablePath
  Log.info ("serving " ++ jsDir)
  let port = if production options then 80 else 8080
      settings =
        setPort port $
        setBeforeMainLoop (Log.info ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings $ app jsDir

app :: FilePath -> Application
app dir = staticApp $ defaultWebAppSettings dir
