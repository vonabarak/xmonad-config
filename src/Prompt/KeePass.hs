{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Prompt.KeePass
    ( passPrompt
    , passUsernamePrompt
    , passOTPPrompt
    , getPasswords
    , KeePassConf (..)
    ) where


import XMonad (Default(def), uninstallSignalHandlers)
import XMonad.Core ( io, X )
import XMonad.Util.Run ( runProcessWithInput )
import XMonad.Prelude ( void )
import XMonad.Prompt ( XPrompt
                     , showXPrompt
                     , commandToComplete
                     , nextCompletion
                     , getNextCompletion
                     , XPConfig
                     , mkXPrompt
                     , searchPredicate)

import Control.Concurrent (forkIO, MVar, newEmptyMVar, tryTakeMVar, putMVar)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.List (nub)
import System.Posix (createSession, forkProcess)


data KeePassConf = KeePassConf
  { exe         :: FilePath
  , database    :: FilePath
  , keyfile     :: Maybe FilePath
  , password    :: Maybe String
  , extraArgs   :: [String]
  , clipTimeout :: Integer
  , xpConfig    :: XPConfig
  }

instance Default KeePassConf where
  def = KeePassConf
    { exe         = "/usr/bin/keepassxc-cli"
    , database    = "Passwords.kdbx"
    , keyfile     = Nothing
    , password    = Nothing
    , extraArgs   = []
    , clipTimeout = 10
    , xpConfig    = def
    }

-- | Return a tuple of command, argv and input string
--
keepassArgv :: KeePassConf -> String -> [String] -> (String, [String], String)
keepassArgv conf command args =
  case password conf of
    Just pwd -> (exe conf, [command, database conf] ++ keyfileArgs ++ extraArgs conf ++ args ++ clipTimeoutArgs, pwd)
    Nothing  -> (exe conf, [command, database conf, "--no-password"] ++ keyfileArgs ++ extraArgs conf ++ args ++ clipTimeoutArgs, [])
  where
    keyfileArgs = case keyfile conf of
      Just value -> ["-k", value]
      Nothing    -> []
    clipTimeoutArgs = case command of
      "clip"     -> [show (clipTimeout conf)]
      _          -> []


-- | Run keepassxc-cli with given command and arguments and return its output
keepass' :: String -> [String] -> KeePassConf -> IO String
keepass' command arguments conf = runProcessWithInput cmd args input
  where
    (cmd, args, input) = keepassArgv conf command arguments


-- | Fork a process to run keepassxc-cli because it's waiting (10 sec by 
--   default) for clipboard to be cleared
keepass :: String -> [String] -> KeePassConf -> IO ()
keepass command arguments conf = void $ forkProcess $ do
  uninstallSignalHandlers
  _ <- createSession
  void $ keepass' command arguments conf

type Predicate = String -> String -> Bool

getPassCompl :: [String] -> Predicate -> String -> IO [String]
getPassCompl compls p s = return $ filter (p s) compls

type PromptLabel = String

newtype Pass = Pass PromptLabel

instance XPrompt Pass where
  showXPrompt       (Pass prompt) = prompt ++ ": "
  commandToComplete _ c           = c
  nextCompletion      _           = getNextCompletion


-- | A pass prompt factory.
--
mkPassPrompt :: PromptLabel -> (String -> X ()) -> KeePassConf -> X ()
mkPassPrompt promptLabel passwordFunction config = do
  passwords <- io $ getPasswords config
  mkXPrompt (Pass promptLabel) (xpConfig config) (getPassCompl passwords $ searchPredicate (xpConfig config)) passwordFunction

passPrompt :: KeePassConf -> X ()
passPrompt conf = mkPassPrompt "Select password" f conf where
   f passLabel = io $ keepass "clip" [passLabel, "-a", "password"] conf

passUsernamePrompt :: KeePassConf -> X ()
passUsernamePrompt conf  = mkPassPrompt "Select username" f conf where
  f passLabel = io $ keepass "clip" [passLabel, "-a", "username"] conf

passOTPPrompt :: KeePassConf -> X ()
passOTPPrompt conf = mkPassPrompt "Select OTP" f conf where
  f passLabel = io $ keepass "clip" [passLabel, "-t"] conf

-- | Retrieve the list of passwords
--
getPasswords' :: KeePassConf -> IO [String]
getPasswords' conf = do
  out <- keepass' "ls" ["-f", "-R"] conf
  return $ nub $ lines out

type Cache = MVar (UTCTime, [String])
cache :: Cache
{-# NOINLINE cache #-}
cache = unsafePerformIO newEmptyMVar


-- | Check cache and return value instantly if cache is still valid, refresh cache in the background.
--   Wait for refresh if cache is expired.
getCached :: Cache -> NominalDiffTime -> IO [String] -> IO [String]
getCached c timeout refresh = do
  cachedData  <- tryTakeMVar c
  currentTime <- getCurrentTime
  case cachedData of
    Just (oldTime, oldValue) ->
      if diffUTCTime currentTime oldTime >= timeout
      then do
        newValue <- refresh
        putMVar c (currentTime, newValue)
        return newValue
      else do
        forkIO $ do
          newValue <- refresh
          time <- getCurrentTime
          putMVar c (time, newValue)
        return oldValue
    Nothing -> do
      newValue <- refresh
      putMVar c (currentTime, newValue)
      return newValue

getPasswords :: KeePassConf -> IO [String]
getPasswords conf = getCached cache (60*60*24) $ getPasswords' conf
