module Main (main) where

import Lib
import Web.Scotty (scotty)
import System.Console.ParseArgs
import System.Directory (getCurrentDirectory)


arguments :: FilePath -> [Arg Int]
arguments workingDirectory = [
    Arg 0 (Just 'p') (Just "port")
        (argDataDefaulted "port" ArgtypeInt 3000)
        "port to bind the server to",
    Arg 1 (Just 'd') (Just "directory")
        (argDataDefaulted "directory" ArgtypeString workingDirectory)
        "Directory in which to put the uploaded files. Default is the current directory"
    ]

main :: IO ()
main = do
    cwd <- getCurrentDirectory

    a <- parseArgsIO ArgsComplete (arguments cwd)
    let port = getRequiredArg a 0 :: Int
    let targetDirectory = getRequiredArg a 1 :: FilePath

    let config = Config targetDirectory
    scotty port (server config)
