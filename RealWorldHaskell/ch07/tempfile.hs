-- ch07/tempfile.hs
import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception(catch, finally)

-- entry point
main :: IO ()
main = withTempFile "mytemp.txt" myAction

-- myAction
myAction :: FilePath -> Handle -> IO ()
myAction tempname temph =
    do
        putStrLn "Welcome to tempfile.hs"
        putStrLn $ "I have a temporary file at " ++ tempname
        
        -- initial position
        pos <- hTell temph
        putStrLn $ "Initial positoin: " ++ show pos
        
        -- write some data to the file
        let tempData = show [1..10]
        putStrLn $ "Writing one line containing " ++ show (length tempData) ++ " bytes: " ++ tempData
        hPutStrLn temph tempData
        
        -- new position
        pos <- hTell temph
        putStrLn $ "After writing, new position is " ++ show pos
        
        -- seek to the beginning
        putStrLn $ "File contents: "
        
        hSeek temph AbsoluteSeek 0
        c <- hGetContents temph
        putStrLn c
        
        -- displaying it as a Haskell literal
        putStrLn "As a Haskell Literal: "
        print c

-- withTempFile
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern fn =
    do
        tempDir <- getTemporaryDirectory
        (tempFile, temph) <- openTempFile tempDir pattern
        finally (fn tempFile temph)
                (do hClose temph
                    removeFile tempFile)
                    
        