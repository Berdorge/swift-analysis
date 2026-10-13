module Main (main) where

import Control.Monad (forM_)
import Swift.Parser (parseSwift)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    paths <- getArgs
    if null paths
        then hPutStrLn stderr "Usage: swift-analysis-exe FILE..." >> exitFailure
        else forM_ paths $ \path -> do
            source <- readFile path
            case parseSwift path source of
                Left err -> hPutStrLn stderr err >> exitFailure
                Right ast -> print ast
