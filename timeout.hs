{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, swapMVar, threadDelay, tryTakeMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (void, when)
import Data.Char (toUpper)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import qualified System.Posix.Signals as Signals
import System.Posix.Types (CPid)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, getProcessExitCode, proc, std_err, std_in, std_out, waitForProcess)

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

data TimeoutStatus = TimedOut | KilledAfterTimeout

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

parseSignal :: String -> Signals.Signal
parseSignal s =
  let upper = map toUpper s
      name = fromMaybe upper (stripPrefix "SIG" upper)
   in case name of
        "ABRT" -> Signals.sigABRT
        "ALRM" -> Signals.sigALRM
        "BUS" -> Signals.sigBUS
        "CHLD" -> Signals.sigCHLD
        "CONT" -> Signals.sigCONT
        "FPE" -> Signals.sigFPE
        "HUP" -> Signals.sigHUP
        "ILL" -> Signals.sigILL
        "INT" -> Signals.sigINT
        "KILL" -> Signals.sigKILL
        "PIPE" -> Signals.sigPIPE
        "PROF" -> Signals.sigPROF
        "QUIT" -> Signals.sigQUIT
        "SEGV" -> Signals.sigSEGV
        "STOP" -> Signals.sigSTOP
        "SYS" -> Signals.sigSYS
        "TERM" -> Signals.sigTERM
        "TRAP" -> Signals.sigTRAP
        "TSTP" -> Signals.sigTSTP
        "TTIN" -> Signals.sigTTIN
        "TTOU" -> Signals.sigTTOU
        "URG" -> Signals.sigURG
        "USR1" -> Signals.sigUSR1
        "USR2" -> Signals.sigUSR2
        "VTALRM" -> Signals.sigVTALRM
        "XCPU" -> Signals.sigXCPU
        "XFSZ" -> Signals.sigXFSZ
        _ -> case reads s of
          [(n :: Int, "")] -> fromIntegral n
          _ -> error $ "invalid signal: '" ++ s ++ "'\nTry '--help' for more information."

getProcessId :: ProcessHandle -> IO CPid
getProcessId ph = do
  mpid <- getPid ph
  case mpid of
    Just pid -> return pid
    Nothing -> error "Failed to get process ID"

determineSignal :: TimeoutOptions -> Signals.Signal
determineSignal opts = maybe Signals.sigTERM parseSignal opts.signal

buildProcessConfig :: TimeoutOptions -> String -> [String] -> CreateProcess
buildProcessConfig opts cmd cmdArgs =
  if opts.foreground
    then (proc cmd cmdArgs) {std_in = Inherit, std_out = Inherit, std_err = Inherit}
    else (proc cmd cmdArgs) {create_group = True}

startProcess :: CreateProcess -> IO ProcessHandle
startProcess processConfig = do
  (_, _, _, ph) <- createProcess processConfig
  return ph

sendSignal :: TimeoutOptions -> Signals.Signal -> CPid -> IO ()
sendSignal opts signal pid =
  if opts.foreground
    then Signals.signalProcess signal pid
    else Signals.signalProcessGroup signal pid

sendTimeoutSignal :: TimeoutOptions -> Signals.Signal -> CPid -> IO ()
sendTimeoutSignal opts signal pid = do
  sendSignal opts signal pid
  when (signal /= Signals.sigKILL && signal /= Signals.sigCONT) $ sendSignal opts Signals.sigCONT pid

startTimeoutThread :: Int -> Maybe Int -> TimeoutOptions -> CPid -> ProcessHandle -> MVar TimeoutStatus -> IO ()
startTimeoutThread micros killMicros opts pid ph timeoutOccurred = do
  let signal = determineSignal opts
  _ <- forkIO $ do
    threadDelay micros
    putMVar timeoutOccurred TimedOut
    when opts.verbose $ hPutStrLn stderr $ "sending signal " ++ show signal ++ " to process " ++ show pid
    sendTimeoutSignal opts signal pid `catch` \(_ :: SomeException) -> return ()

    case killMicros of
      Just killDelay -> do
        threadDelay killDelay
        mExitCode <- getProcessExitCode ph
        case mExitCode of
          Nothing -> do
            when opts.verbose $ hPutStrLn stderr $ "sending signal KILL to process " ++ show pid
            void $ swapMVar timeoutOccurred KilledAfterTimeout
            sendSignal opts Signals.sigKILL pid `catch` \(_ :: SomeException) -> return ()
          Just _ -> return ()
      Nothing -> return ()
  return ()

handleExitCode :: TimeoutOptions -> Maybe TimeoutStatus -> ExitCode -> ExitCode
handleExitCode opts timeoutHappened exitCode = case (timeoutHappened, exitCode) of
  (Just KilledAfterTimeout, _) -> ExitFailure exitKilledByKillSignal
  (Just TimedOut, _) ->
    if opts.preserveStatus
      then exitCode
      else ExitFailure exitTimeout
  (_, ExitSuccess) -> ExitSuccess
  (_, ExitFailure code) -> ExitFailure code

runTimeout :: TimeoutOptions -> String -> String -> [String] -> IO ExitCode
runTimeout opts duration cmd cmdArgs =
  do
    micros <- parseDuration duration
    parsedKillMicros <- maybe (return Nothing) (fmap Just . parseDuration) opts.killAfter
    let killMicros = parsedKillMicros >>= \delay -> if delay == 0 then Nothing else Just delay

    let processConfig = buildProcessConfig opts cmd cmdArgs
    ph <- startProcess processConfig
    pid <- getProcessId ph

    timeoutOccurred <- newEmptyMVar
    when (micros /= 0) $ startTimeoutThread micros killMicros opts pid ph timeoutOccurred

    exitCode <- waitForProcess ph
    timeoutHappened <- tryTakeMVar timeoutOccurred

    return $ handleExitCode opts timeoutHappened exitCode
    `catch` \(e :: IOError) -> do
      if isDoesNotExistError e
        then return $ ExitFailure exitCommandNotFound
        else
          if isPermissionError e
            then return $ ExitFailure exitCommandNotExecutable
            else return $ ExitFailure exitTimeoutFailure

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
