{-# LANGUAGE OverloadedStrings #-}

{- import modules -}
import System.Directory
import System.IO
import Data.Time.Clock
import Data.Time.Format
import System.FilePath ((</>))
import Control.Monad (when, unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

versionDir :: FilePath
versionDir = ".versions"

getTimestamp :: IO String
getTimestamp = formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getCurrentTime

{- validates if file is missing, otherwise it commits the file, generates a timestamp, and generates a message -}
commit :: FilePath -> IO ()
commit file = do
    exists <- doesFileExist file
    unless exists $ putStrLn "File does not exist!" >> return ()
    
    createDirectoryIfMissing True versionDir
    timestamp <- getTimestamp
    let commitFile = versionDir </> (timestamp ++ "_" ++ file)
    B.readFile file >>= B.writeFile commitFile
    putStrLn $ "Committed " ++ file ++ " as " ++ commitFile

{- generates all previous committed file versions, if there is none, it generates the message "No commits found." -}
listCommits :: IO ()
listCommits = do
    exists <- doesDirectoryExist versionDir
    if exists then do
        files <- listDirectory versionDir
        mapM_ putStrLn files
    else
        putStrLn "No commits found."

{- reverts a file to a previously commited version, overwriting it
   if the commit does not exist, it generates a message "Commit not found." -}
revert :: FilePath -> String -> IO ()
revert file timestamp = do
    let commitFile = versionDir </> (timestamp ++ "_" ++ file)
    exists <- doesFileExist commitFile
    if exists then do
        B.readFile commitFile >>= B.writeFile file
        putStrLn $ "Reverted " ++ file ++ " to " ++ timestamp
    else
        putStrLn "Commit not found."

{- actively waits for user input, 
   once recieved, it hands the commands for committing a file, lists saved versions, and reverts the file 'til ["exit"] is reached -}
main :: IO ()
main = do
    putStrLn "Simple Version Control in Haskell"
    putStrLn "Commands: commit <file>, list, revert <file> <timestamp>"
    loop
  where
    loop = do
        putStr "> "
        hFlush stdout
        cmd <- words <$> getLine
        case cmd of
            ["commit", file] -> commit file
            ["list"] -> listCommits
            ["revert", file, timestamp] -> revert file timestamp
            ["exit"] -> putStrLn "Exiting..."
            _ -> putStrLn "Invalid command"
        unless (cmd == ["exit"]) loop
