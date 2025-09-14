module Main where

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, proc, waitForProcess)

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

parseArgs :: [String] -> IO (TimeoutOptions, String, String, [String])
parseArgs argv = case getOpt RequireOrder options argv of
  (o, n, []) -> do
    let opts = foldl (flip id) defaultOptions o
    case n of
      [] -> do
        hPutStrLn stderr "missing operand"
        hPutStrLn stderr "Try '--help' for more information."
        exitFailure
      [duration] -> do
        hPutStrLn stderr "missing command"
        hPutStrLn stderr "Try '--help' for more information."
        exitFailure
      duration : cmd : args -> return (opts, duration, cmd, args)
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
  (opts, duration, cmd, cmdArgs) <- parseArgs args

  if optHelp opts
    then showHelp >> exitSuccess
    else
      if optVersion opts
        then showVersion >> exitSuccess
        else do
          (_, _, _, ph) <- createProcess (proc cmd cmdArgs)
          exitCode <- waitForProcess ph
          case exitCode of
            ExitSuccess -> exitSuccess
            ExitFailure code -> exitWith (ExitFailure code)
