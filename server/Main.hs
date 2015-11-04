{-# LANGUAGE OverloadedStrings #-}

import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           System.Environment
import           System.FilePath
import qualified System.Logging.Facade as Log
import           WaiAppStatic.Types

main = do
  jsDir <- (</> "ghcjs-test-client.jsexe") <$>
    dropFileName <$> getExecutablePath
  Log.info ("serving " ++ jsDir)
  let settings =
        setPort 8080 $
        setBeforeMainLoop (Log.info "listening on port 8080") $
        defaultSettings
  runSettings settings $ app jsDir

app :: FilePath -> Application
app dir = staticApp $ defaultWebAppSettings dir
