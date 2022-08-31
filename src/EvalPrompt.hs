module EvalPrompt (evalPrompt) where

import XMonad
import XMonad.Prompt
import Data.List ( intercalate )
import qualified Language.Haskell.Interpreter  as I

-- Eval prompt.
-- evaluate any haskell expression by pressing mod-shift-x
data EvalPrompt = EvalPrompt

instance XPrompt EvalPrompt where
  showXPrompt = const "haskell> "
  commandToComplete _ = id

evalComplFunction :: MonadIO m => String -> m [String]
evalComplFunction s = io $ do
    res <- I.runInterpreter $ do 
        I.setImports ["Prelude"]
        I.eval s
    case res of
        Left  err    -> return [show err]
        Right result -> return (lines result)

showResult :: MonadIO m => String -> m ()
showResult s = io $ do
    result <- evalComplFunction s
    spawn $ "notify-send " ++ intercalate "\n" result

evalPrompt :: XPConfig -> X ()
evalPrompt myXPConfig = do
    uninstallSignalHandlers
    mkXPrompt EvalPrompt myXPConfig evalComplFunction showResult
    installSignalHandlers
