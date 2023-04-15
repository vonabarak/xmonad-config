module Prompt.Run
    (runPrompt,
     RunPrompt,
    ) where

import XMonad hiding (config)
import XMonad.Prelude (isNothing, isSuffixOf)
import XMonad.Prompt
    ( mkXPrompt, XPConfig(searchPredicate), XPrompt(showXPrompt) )
import XMonad.Prompt.Shell ( getCommands, getShellCompl )
import Runner (sspawn)

import System.Directory (doesDirectoryExist, doesFileExist, executable, findExecutable, getPermissions)


data RunPrompt = RunPrompt
instance XPrompt RunPrompt where
    showXPrompt RunPrompt = "Run: "

runPrompt :: XPConfig -> X ()
runPrompt c = do cmds <- io getCommands
                 mkXPrompt RunPrompt c (getShellCompl cmds $ searchPredicate c) open
open :: String -> X ()
open path = io (isNormalFile path) >>= \b ->
            if b
            then spawn $ "xdg-open \"" ++ path ++ "\""
            else sspawn path
    where
      isNormalFile f = do
          notCommand <- isNothing <$> findExecutable f -- not a command (executable in $PATH)
          exists <- or <$> sequence [doesDirExist f, doesFileExist f]
          case (notCommand, exists) of
              (True, True) -> notExecutable f -- not executable as a file in current dir
              _            -> pure False
      notExecutable = fmap (not . executable) . getPermissions
      doesDirExist f = ("/" `isSuffixOf` f &&) <$> doesDirectoryExist f
