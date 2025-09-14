module Main where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, threadDelay, tryTakeMVar)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (Signal, signalProcess, sigTERM, sigKILL, sigINT, sigHUP, sigUSR1, sigUSR2)
import System.Process (createProcess, proc, waitForProcess, getPid, ProcessHandle)
import System.Posix.Types (CPid)

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
        exitWith (ExitFailure 125)
      [_] -> do
        hPutStrLn stderr "missing command"
        hPutStrLn stderr "Try '--help' for more information."
        exitWith (ExitFailure 125)
      duration : cmd : args -> return (opts, duration, cmd, args)
  (_, _, errs) -> do
    hPutStrLn stderr (concat errs)
    hPutStrLn stderr "Try '--help' for more information."
    exitWith (ExitFailure 125)

showHelp :: IO ()
showHelp = do
  progName <- getProgName
  let header = "Usage: " ++ progName ++ " [OPTION] DURATION COMMAND [ARG]..."
  putStrLn (usageInfo header options)

showVersion :: IO ()
showVersion = putStrLn "timeout (Haskell implementation) 1.0"

parseDuration :: String -> Maybe Int
parseDuration s = case reads s of
  [(n, "s")] -> Just (n * 1000000)
  [(n, "m")] -> Just (n * 60000000)
  [(n, "h")] -> Just (n * 3600000000)
  [(n, "")] -> Just (n * 1000000)
  _ -> Nothing

parseSignal :: String -> Maybe Signal
parseSignal "TERM" = Just sigTERM
parseSignal "KILL" = Just sigKILL
parseSignal "INT" = Just sigINT
parseSignal "HUP" = Just sigHUP
parseSignal "USR1" = Just sigUSR1
parseSignal "USR2" = Just sigUSR2
parseSignal s = case reads s of
  [(n :: Int, "")] -> Just (fromIntegral n)
  _ -> Nothing

-- Safe wrapper for getPid that throws a proper error on failure
getProcessId :: ProcessHandle -> IO CPid
getProcessId ph = do
  mpid <- getPid ph
  case mpid of
    Just pid -> return pid
    Nothing -> do
      hPutStrLn stderr "Failed to get process ID"
      exitWith (ExitFailure 125)

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
          case parseDuration duration of
            Nothing -> do
              hPutStrLn stderr $ "invalid time interval: '" ++ duration ++ "'"
              exitWith (ExitFailure 125)
            Just micros -> do
              (_, _, _, ph) <- createProcess (proc cmd cmdArgs)

              pid <- getProcessId ph

              let signal = case optSignal opts of
                    Just sigStr -> case parseSignal sigStr of
                      Just sig -> sig
                      Nothing -> sigTERM  -- Default to SIGTERM on parse failure
                    Nothing -> sigTERM    -- Default signal

              timeoutOccurred <- newEmptyMVar

              _ <- forkIO $ do
                threadDelay micros
                putMVar timeoutOccurred True
                signalProcess signal pid

              exitCode <- waitForProcess ph

              timeoutHappened <- tryTakeMVar timeoutOccurred

              case (timeoutHappened, exitCode) of
                (Just True, _) ->
                  if optPreserveStatus opts
                    then exitWith exitCode
                    else exitWith (ExitFailure 124)
                (_, ExitSuccess) -> exitSuccess
                (_, ExitFailure code) -> exitWith (ExitFailure code)
