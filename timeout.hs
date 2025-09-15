module Main where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, threadDelay, tryTakeMVar)
import Control.Exception (SomeException, catch)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (Signal, sigHUP, sigINT, sigKILL, sigTERM, sigUSR1, sigUSR2, signalProcess)
import System.Posix.Types (CPid)
import System.Process (ProcessHandle, createProcess, getPid, proc, waitForProcess)

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
parseArgs argv =
  let helpMsg = "\nTry '--help' for more information."
  in case getOpt RequireOrder options argv of
    (o, n, []) -> do
      let opts = foldl (flip id) defaultOptions o
      case n of
        [] -> error $ "missing operand" ++ helpMsg
        [_] -> error $ "missing command" ++ helpMsg
        duration : cmd : args -> return (opts, duration, cmd, args)
    (_, _, errs) -> error (concat errs ++ helpMsg)

showHelp :: IO ()
showHelp = do
  progName <- getProgName
  let header = "Usage: " ++ progName ++ " [OPTION] DURATION COMMAND [ARG]..."
  putStrLn (usageInfo header options)

showVersion :: IO ()
showVersion = putStrLn "timeout (Haskell implementation) 0.0.1"

parseDuration :: String -> IO Int
parseDuration s = case reads s of
  [(n, "s")] -> return (n * 1000000)
  [(n, "m")] -> return (n * 60000000)
  [(n, "h")] -> return (n * 3600000000)
  [(n, "")] -> return (n * 1000000)
  _ -> error $ "invalid time interval: '" ++ s ++ "'\nTry '--help' for more information."

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

getProcessId :: ProcessHandle -> IO CPid
getProcessId ph = do
  mpid <- getPid ph
  case mpid of
    Just pid -> return pid
    Nothing -> error "Failed to get process ID"

determineSignal :: TimeoutOptions -> Signal
determineSignal opts = case optSignal opts of
  Just sigStr -> case parseSignal sigStr of
    Just sig -> sig
    Nothing -> sigTERM
  Nothing -> sigTERM

run :: IO ExitCode
run = do
  args <- getArgs
  (opts, duration, cmd, cmdArgs) <- parseArgs args

  if optHelp opts
    then showHelp >> return ExitSuccess
    else
      if optVersion opts
        then showVersion >> return ExitSuccess
        else do
          micros <- parseDuration duration
          killMicros <- maybe (return Nothing) (fmap Just . parseDuration) (optKillAfter opts)
          (_, _, _, ph) <- createProcess (proc cmd cmdArgs)

          pid <- getProcessId ph

          let signal = determineSignal opts
          timeoutOccurred <- newEmptyMVar

          _ <- forkIO $ do
            threadDelay micros
            putMVar timeoutOccurred True
            signalProcess signal pid

            case killMicros of
              Just killDelay -> do
                threadDelay killDelay
                signalProcess sigKILL pid
              Nothing -> return ()

          exitCode <- waitForProcess ph

          timeoutHappened <- tryTakeMVar timeoutOccurred

          case (timeoutHappened, exitCode) of
            (Just True, _) ->
              if optPreserveStatus opts
                then return exitCode
                else return (ExitFailure 124)
            (_, ExitSuccess) -> return ExitSuccess
            (_, ExitFailure code) -> return (ExitFailure code)

main :: IO ()
main = do
  exitCode <-
    run `catch` \e -> do
      hPutStrLn stderr (show (e :: SomeException))
      return (ExitFailure 125)
  exitWith exitCode
