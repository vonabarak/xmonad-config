import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import Runner ( respawn, restart, switch )

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ("respawn":command) = respawn $ unwords command
parseArgs ("restart":command) = restart $ unwords command
parseArgs ("switch":command)  = switch  $ unwords command
parseArgs _                   = usage >> exitFailure

usage :: IO ()
usage = putStrLn "Usage: runner (respawn|restart|switch) command [arguments]"
