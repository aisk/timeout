{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, threadDelay, tryTakeMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (Signal, sigHUP, sigINT, sigKILL, sigTERM, sigUSR1, sigUSR2, signalProcess)
import System.Posix.Types (CPid)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, proc, std_err, std_in, std_out, waitForProcess)

exitTimeout :: Int
exitTimeout = 124

exitTimeoutFailure :: Int
exitTimeoutFailure = 125

exitCommandNotExecutable :: Int
exitCommandNotExecutable = 126

exitCommandNotFound :: Int
exitCommandNotFound = 127

exitKilledByKillSignal :: Int
exitKilledByKillSignal = 137

data TimeoutOptions = TimeoutOptions
  { foreground :: Bool,
    killAfter :: Maybe String,
    preserveStatus :: Bool,
    signal :: Maybe String,
    verbose :: Bool,
    help :: Bool,
    version :: Bool
  }
  deriving (Show)

defaultOptions :: TimeoutOptions
defaultOptions =
  TimeoutOptions
    { foreground = False,
      killAfter = Nothing,
      preserveStatus = False,
      signal = Nothing,
      verbose = False,
      help = False,
      version = False
    }

options :: [OptDescr (TimeoutOptions -> TimeoutOptions)]
options =
  [ Option
      ['f']
      ["foreground"]
      (NoArg (\opts -> opts {foreground = True}))
      "allow COMMAND to read from TTY and get TTY signals",
    Option
      ['k']
      ["kill-after"]
      (ReqArg (\dur opts -> opts {killAfter = Just dur}) "DURATION")
      "also send KILL signal after DURATION",
    Option
      ['p']
      ["preserve-status"]
      (NoArg (\opts -> opts {preserveStatus = True}))
      "exit with same status as COMMAND",
    Option
      ['s']
      ["signal"]
      (ReqArg (\sig opts -> opts {signal = Just sig}) "SIGNAL")
      "specify signal to send on timeout",
    Option
      ['v']
      ["verbose"]
      (NoArg (\opts -> opts {verbose = True}))
      "diagnose to stderr any signal sent",
    Option
      []
      ["help"]
      (NoArg (\opts -> opts {help = True}))
      "display this help and exit",
    Option
      []
      ["version"]
      (NoArg (\opts -> opts {version = True}))
      "output version information and exit"
  ]

parseArgs :: [String] -> IO (TimeoutOptions, String, String, [String])
parseArgs argv =
  let helpMsg = "\nTry '--help' for more information."
   in case getOpt RequireOrder options argv of
        (o, n, []) -> do
          let opts = foldl (flip id) defaultOptions o
          if opts.help || opts.version
            then return (opts, "", "", [])
            else case n of
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
showVersion = putStrLn "timeout (Haskell implementation) 0.1.0"

parseDuration :: String -> IO Int
parseDuration s = case reads s of
  [(n :: Double, "ms")] -> return (round (n * 1000))
  [(n :: Double, "s")] -> return (round (n * 1000000))
  [(n :: Double, "m")] -> return (round (n * 60000000))
  [(n :: Double, "h")] -> return (round (n * 3600000000))
  [(n :: Double, "d")] -> return (round (n * 86400000000))
  [(n :: Double, "")] -> return (round (n * 1000000))
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
determineSignal opts = case opts.signal of
  Just sigStr -> case parseSignal sigStr of
    Just sig -> sig
    Nothing -> sigTERM
  Nothing -> sigTERM

buildProcessConfig :: TimeoutOptions -> String -> [String] -> CreateProcess
buildProcessConfig opts cmd cmdArgs =
  if opts.foreground
    then (proc cmd cmdArgs) {std_in = Inherit, std_out = Inherit, std_err = Inherit}
    else proc cmd cmdArgs

startProcess :: CreateProcess -> IO ProcessHandle
startProcess processConfig = do
  (_, _, _, ph) <- createProcess processConfig
  return ph

startTimeoutThread :: Int -> Maybe Int -> TimeoutOptions -> CPid -> MVar Bool -> IO ()
startTimeoutThread micros killMicros opts pid timeoutOccurred = do
  let signal = determineSignal opts
  _ <- forkIO $ do
    threadDelay micros
    putMVar timeoutOccurred True
    when opts.verbose $ hPutStrLn stderr $ "sending signal " ++ show signal ++ " to process " ++ show pid
    signalProcess signal pid

    case killMicros of
      Just killDelay -> do
        threadDelay killDelay
        when opts.verbose $ hPutStrLn stderr $ "sending signal KILL to process " ++ show pid
        signalProcess sigKILL pid
      Nothing -> return ()
  return ()

handleExitCode :: TimeoutOptions -> Maybe Bool -> ExitCode -> ExitCode
handleExitCode opts timeoutHappened exitCode = case (timeoutHappened, exitCode) of
  (Just True, _) ->
    if opts.preserveStatus
      then exitCode
      else ExitFailure exitTimeout
  (_, ExitSuccess) -> ExitSuccess
  (_, ExitFailure code) -> ExitFailure code

runTimeout :: TimeoutOptions -> String -> String -> [String] -> IO ExitCode
runTimeout opts duration cmd cmdArgs = do
  micros <- parseDuration duration
  killMicros <- maybe (return Nothing) (fmap Just . parseDuration) opts.killAfter

  let processConfig = buildProcessConfig opts cmd cmdArgs
  ph <- startProcess processConfig
  pid <- getProcessId ph

  timeoutOccurred <- newEmptyMVar
  startTimeoutThread micros killMicros opts pid timeoutOccurred

  exitCode <- waitForProcess ph
  timeoutHappened <- tryTakeMVar timeoutOccurred

  return $ handleExitCode opts timeoutHappened exitCode

run :: IO ExitCode
run = do
  args <- getArgs
  (opts, duration, cmd, cmdArgs) <- parseArgs args

  case () of
    _
      | opts.help -> showHelp >> return ExitSuccess
      | opts.version -> showVersion >> return ExitSuccess
      | otherwise -> runTimeout opts duration cmd cmdArgs

main :: IO ()
main = do
  exitCode <-
    run `catch` \e -> do
      hPutStrLn stderr (show (e :: SomeException))
      return (ExitFailure exitTimeoutFailure)
  exitWith exitCode
