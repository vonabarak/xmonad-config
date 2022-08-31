module Runner (switch, restart, respawn) where

import System.Directory ( removeFile )
import System.Process ( getPid, spawnCommand, ProcessHandle )
import System.FilePath ( (<.>), (</>) )
import System.Posix
    ( ProcessID, sigKILL, signalProcess, getRealUserID )
import Control.Exception ( try, SomeException )
import Control.Exception.Extra ( ignore )
import XMonad ( liftIO )
import Control.Monad.IO.Class ( MonadIO(..) )


switch :: MonadIO m => String -> m ()
switch command = liftIO $ switch' command

restart :: MonadIO m => String -> m ()
restart command = liftIO $ restart' command

respawn :: MonadIO m => String -> m ()
respawn command = liftIO $ respawn' command


switch' :: String -> IO()
switch' command = do
    maybePid <- readPid command
    case maybePid of
        Nothing -> spawn command
        Just pid -> do
            result <- try $ signalProcess sigKILL pid :: IO (Either SomeException ())
            getPidFilePath command >>= removeFile
            case result of
                Left _ -> spawn command
                Right _ -> return ()


restart' :: String -> IO()
restart' command = do
    maybePid <- readPid command
    case maybePid of
        Nothing -> spawn command
        Just pid -> do
            ignore $ signalProcess sigKILL pid
            spawn command


respawn' :: String -> IO()
respawn' command = do
    isCommandRunning <- isRunning command
    if isCommandRunning
        then putStrLn $ "Command " ++ command ++ " is already running."
        else spawn command


spawn :: String -> IO()
spawn command = spawnCommand command >>= writePid command
    

isRunning :: String -> IO Bool
isRunning command = do
    maybePid <- readPid command
    case maybePid of
        Nothing -> return False
        Just pid -> do
            result <- try $ signalProcess 0 pid :: IO (Either SomeException ())
            case result of
                Left _ -> return False
                Right _ -> return True


readPid :: String -> IO (Maybe ProcessID)
readPid command = do
    strPid <- try $ getPidFilePath command >>= readFile :: IO (Either SomeException String)
    case strPid of
        Left _    -> return Nothing
        Right val -> do
            let intVal = read val :: Integer
            return $ Just (fromIntegral intVal)


writePid :: String -> ProcessHandle -> IO()
writePid command handler = do
    Just pid <- getPid handler
    path <- getPidFilePath command
    writeFile path (show pid)


getPidFilePath :: String -> IO String
getPidFilePath command = do
    uid <- getRealUserID
    return ("/run/user/" </> show uid </> command <.> "pid")
