{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Lib
    ( server, Config(..)
    ) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Text.RawString.QQ
import Data.Text.Lazy (Text, pack)
import Network.Wai.Parse (fileName, FileInfo (fileContent))
import System.FilePath.Posix.ByteString (decodeFilePath)
import System.FilePath ((</>), takeDirectory)
import qualified Data.ByteString.Lazy
import System.Directory (createDirectoryIfMissing)

newtype Config = Config {
    cTargetDirectory :: FilePath
}

mainpageTemplate :: Text
mainpageTemplate = [r|<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>SendMeFiles</title>
</head>
<body>
<form method="post" action="/upload" enctype="multipart/form-data">
    <input name="files" type="file" value="Send me files" multiple>
    <input name="directories" type="file" value="Send me a directory" webkitdirectory>
    <input type="submit">
</form>
</html>
|]

mainpage :: ActionM ()
mainpage = do
    html mainpageTemplate

saveFile :: FilePath -- ^ target directory
            -> File
            -> ActionM ()
saveFile targetDir mfile = do
    let fileinfo = snd mfile
    let filecontent = fileContent fileinfo
    let filename = (decodeFilePath . fileName) fileinfo
    let path = targetDir </> filename
    liftIO $ putStrLn ("Saving " ++ path)
    liftIO $ createDirectoryIfMissing True (takeDirectory path)
    liftIO $ Data.ByteString.Lazy.writeFile path filecontent


upload :: Config -> ActionM ()
upload config = do
    let targetDir = cTargetDirectory config
    fileList <- files
    mapM_ (saveFile targetDir) fileList
    text (pack "Success")


server :: Config -> ScottyM ()
server c = do
    get "/" mainpage
    post "/upload" (upload c)
