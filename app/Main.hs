{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad.Except (liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Swift.Cfg (buildProgramCfg)
import Swift.Ddg (buildProgramDdg)
import Swift.Dot (cfgToDot, ddgToDot)
import Swift.Parser (parseSwift)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess)

main :: IO ()
main = do
    result <- runExceptT $ do
        path <- liftIO getArgs >>= \case
            []    -> throwError "Usage: swift-analysis <file>"
            p : _ -> return p
        source <- liftIO $ readFile path
        parsedFile <- liftEither (parseSwift path source)
        let cfg = buildProgramCfg parsedFile
        let ddg = buildProgramDdg cfg
        let cfgDot = cfgToDot cfg
        let ddgDot = ddgToDot ddg
        liftIO $ render (path ++ ".cfg") cfgDot
        liftIO $ render (path ++ ".ddg") ddgDot
    case result of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right () -> return ()

render :: FilePath -> String -> IO ()
render prefix dot = do
    writeFile (prefix ++ ".dot") dot
    callProcess "dot" ["-Tpng", prefix ++ ".dot", "-o", prefix ++ ".png"]
    putStrLn $ "Wrote " ++ prefix ++ ".png"
