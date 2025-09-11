module Main where

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

data TimeoutOptions = TimeoutOptions
  { optForeground :: Bool,
    optKillAfter :: Maybe String,
    optPreserveStatus :: Bool,
    optSignal :: Maybe String,
    optVerbose :: Bool,
    optHelp :: Bool,
    optVersion :: Bool
  }
  deriving (Show)

defaultOptions :: TimeoutOptions
defaultOptions =
  TimeoutOptions
    { optForeground = False,
      optKillAfter = Nothing,
      optPreserveStatus = False,
      optSignal = Nothing,
      optVerbose = False,
      optHelp = False,
      optVersion = False
    }

options :: [OptDescr (TimeoutOptions -> TimeoutOptions)]
options =
  [ Option
      ['f']
      ["foreground"]
      (NoArg (\opts -> opts {optForeground = True}))
      "allow COMMAND to read from TTY and get TTY signals",
    Option
      ['k']
      ["kill-after"]
      (ReqArg (\dur opts -> opts {optKillAfter = Just dur}) "DURATION")
      "also send KILL signal after DURATION",
    Option
      ['p']
      ["preserve-status"]
      (NoArg (\opts -> opts {optPreserveStatus = True}))
      "exit with same status as COMMAND",
    Option
      ['s']
      ["signal"]
      (ReqArg (\sig opts -> opts {optSignal = Just sig}) "SIGNAL")
      "specify signal to send on timeout",
    Option
      ['v']
      ["verbose"]
      (NoArg (\opts -> opts {optVerbose = True}))
      "diagnose to stderr any signal sent",
    Option
      []
      ["help"]
      (NoArg (\opts -> opts {optHelp = True}))
      "display this help and exit",
    Option
      []
      ["version"]
      (NoArg (\opts -> opts {optVersion = True}))
      "output version information and exit"
  ]

parseArgs :: [String] -> IO (TimeoutOptions, [String])
parseArgs argv = case getOpt Permute options argv of
  (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> do
    hPutStrLn stderr (concat errs)
    hPutStrLn stderr "Try '--help' for more information."
    exitFailure

showHelp :: IO ()
showHelp = do
  progName <- getProgName
  let header = "Usage: " ++ progName ++ " [OPTION] DURATION COMMAND [ARG]..."
  putStrLn (usageInfo header options)

showVersion :: IO ()
showVersion = putStrLn "timeout (Haskell implementation) 1.0"

main :: IO ()
main = do
  args <- getArgs
  (opts, nonOpts) <- parseArgs args

  if optHelp opts
    then showHelp >> exitSuccess
    else
      if optVersion opts
        then showVersion >> exitSuccess
        else putStrLn $ "Parsed options: " ++ show opts ++ "\nNon-options: " ++ show nonOpts
