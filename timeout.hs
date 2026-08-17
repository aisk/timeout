{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Concurrent (MVar, forkIO, modifyMVar, modifyMVar_, newMVar, threadDelay, withMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (unless, void, when)
import Data.Char (toUpper)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, stripPrefix)
import Data.Maybe (fromMaybe)
import Foreign.C.Types (CInt (..))
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import qualified System.Posix.Process as Process
import System.Posix.Resource (Resource (..), ResourceLimit (..), ResourceLimits (..), setResourceLimit)
import qualified System.Posix.Signals as Signals
import System.Posix.Types (CPid)
import System.Process (ProcessHandle, createProcess, getPid, proc, waitForProcess)

exitTimeout :: Int
exitTimeout = 124

exitTimeoutFailure :: Int
exitTimeoutFailure = 125

exitCommandNotExecutable :: Int
exitCommandNotExecutable = 126

exitCommandNotFound :: Int
exitCommandNotFound = 127

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

data PidState = NotSpawned | Spawned CPid | Reaped

data Env = Env
  { opts :: TimeoutOptions,
    command :: String,
    termSignal :: IORef Signals.Signal,
    timedOut :: IORef Bool,
    killDelay :: IORef (Maybe Int),
    childPid :: MVar PidState
  }

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

signalTable :: [(String, Signals.Signal)]
signalTable =
  [ ("ABRT", Signals.sigABRT),
    ("ALRM", Signals.sigALRM),
    ("BUS", Signals.sigBUS),
    ("CHLD", Signals.sigCHLD),
    ("CONT", Signals.sigCONT),
    ("FPE", Signals.sigFPE),
    ("HUP", Signals.sigHUP),
    ("ILL", Signals.sigILL),
    ("INT", Signals.sigINT),
    ("KILL", Signals.sigKILL),
    ("PIPE", Signals.sigPIPE),
    ("PROF", Signals.sigPROF),
    ("QUIT", Signals.sigQUIT),
    ("SEGV", Signals.sigSEGV),
    ("STOP", Signals.sigSTOP),
    ("SYS", Signals.sigSYS),
    ("TERM", Signals.sigTERM),
    ("TRAP", Signals.sigTRAP),
    ("TSTP", Signals.sigTSTP),
    ("TTIN", Signals.sigTTIN),
    ("TTOU", Signals.sigTTOU),
    ("URG", Signals.sigURG),
    ("USR1", Signals.sigUSR1),
    ("USR2", Signals.sigUSR2),
    ("VTALRM", Signals.sigVTALRM),
    ("XCPU", Signals.sigXCPU),
    ("XFSZ", Signals.sigXFSZ)
  ]

parseSignal :: String -> Signals.Signal
parseSignal s =
  let upper = map toUpper s
      name = fromMaybe upper (stripPrefix "SIG" upper)
   in case lookup name signalTable of
        Just sig -> sig
        Nothing -> case reads s of
          [(n :: Int, "")] -> fromIntegral n
          _ -> error $ "invalid signal: '" ++ s ++ "'\nTry '--help' for more information."

signalName :: Signals.Signal -> String
signalName sig = maybe (show sig) fst (find ((== sig) . snd) signalTable)

-- Signals whose default action terminates the process, mirroring
-- coreutils' term-sig.h (minus signals the unix package doesn't expose).
termSigs :: [Signals.Signal]
termSigs =
  [ Signals.sigALRM,
    Signals.sigINT,
    Signals.sigQUIT,
    Signals.sigHUP,
    Signals.sigTERM,
    Signals.sigPIPE,
    Signals.sigUSR1,
    Signals.sigUSR2,
    Signals.sigILL,
    Signals.sigTRAP,
    Signals.sigABRT,
    Signals.sigBUS,
    Signals.sigFPE,
    Signals.sigSEGV,
    Signals.sigXCPU,
    Signals.sigXFSZ,
    Signals.sigSYS,
    Signals.sigVTALRM,
    Signals.sigPROF,
    Signals.sigPOLL
  ]

getProcessId :: ProcessHandle -> IO CPid
getProcessId ph = do
  mpid <- getPid ph
  case mpid of
    Just pid -> return pid
    Nothing -> error "Failed to get process ID"

determineSignal :: TimeoutOptions -> Signals.Signal
determineSignal opts = maybe Signals.sigTERM parseSignal opts.signal

startProcess :: String -> [String] -> IO ProcessHandle
startProcess cmd cmdArgs = do
  (_, _, _, ph) <- createProcess (proc cmd cmdArgs)
  return ph

ignoringExceptions :: IO () -> IO ()
ignoringExceptions act = act `catch` \(_ :: SomeException) -> return ()

foreign import ccall unsafe "timeout_signal_was_ignored"
  c_signalWasIgnored :: CInt -> IO CInt

-- The GHC runtime replaces some inherited dispositions (notably SIGINT)
-- before main runs, so the truth comes from a snapshot taken by a C
-- constructor in cbits.c
signalIsIgnored :: Signals.Signal -> IO Bool
signalIsIgnored sig = (/= 0) <$> c_signalWasIgnored sig

warn :: String -> IO ()
warn msg = do
  progName <- getProgName
  hPutStrLn stderr (progName ++ ": " ++ msg)

-- Forward a received signal (or the timeout, arriving as SIGALRM) to the
-- child, following coreutils' cleanup().
cleanup :: Env -> Signals.Signal -> IO ()
cleanup env received = do
  sig <-
    if received == Signals.sigALRM
      then do
        writeIORef env.timedOut True
        readIORef env.termSignal
      else return received
  withMVar env.childPid $ \pidState -> case pidState of
    Spawned pid -> do
      delay <- atomicModifyIORef' env.killDelay (\d -> (Nothing, d))
      case delay of
        Just killMicros -> do
          -- once armed, the next expiry escalates to KILL
          writeIORef env.termSignal Signals.sigKILL
          void $ forkIO $ threadDelay killMicros >> cleanup env Signals.sigALRM
        Nothing -> return ()
      when env.opts.verbose $
        warn $
          "sending signal " ++ signalName sig ++ " to command '" ++ env.command ++ "'"
      -- signal the child directly in case it changed its process group
      ignoringExceptions $ Signals.signalProcess sig pid
      unless env.opts.foreground $ do
        -- ignore the signal ourselves so signalling our own group can't loop
        when (sig /= Signals.sigKILL && sig /= Signals.sigSTOP) $
          void $
            Signals.installHandler sig Signals.Ignore Nothing
        pgid <- Process.getProcessGroupID
        ignoringExceptions $ Signals.signalProcessGroup sig pgid
        when (sig /= Signals.sigKILL && sig /= Signals.sigCONT) $ do
          ignoringExceptions $ Signals.signalProcess Signals.sigCONT pid
          ignoringExceptions $ Signals.signalProcessGroup Signals.sigCONT pgid
    NotSpawned -> Process.exitImmediately (ExitFailure (128 + fromIntegral sig))
    Reaped -> return ()

installCleanup :: Env -> Signals.Signal -> IO ()
installCleanup env termSig =
  -- the unix package defines signals missing on a platform as -1
  -- (e.g. sigPOLL on macOS)
  mapM_ install (filter (> 0) termSigs ++ [termSig])
  where
    install sig
      | sig == Signals.sigKILL || sig == Signals.sigSTOP = return ()
      | otherwise = ignoringExceptions $ do
          needed <- sigNeedsHandling sig
          if needed
            then void $ Signals.installHandler sig (Signals.Catch (cleanup env sig)) Nothing
            else
              -- re-assert the inherited SIG_IGN, which the GHC top
              -- handler may have replaced (SIGINT)
              void $ Signals.installHandler sig Signals.Ignore Nothing
    -- shells set SIG_IGN on INT/QUIT for background jobs; keep those ignored
    sigNeedsHandling sig
      | sig == Signals.sigALRM || sig == termSig = return True
      | otherwise = not <$> signalIsIgnored sig

disableCoreDumps :: IO Bool
disableCoreDumps =
  (setResourceLimit ResourceCoreFileSize limits >> return True)
    `catch` \(_ :: SomeException) -> return False
  where
    limits = ResourceLimits (ResourceLimit 0) (ResourceLimit 0)

handleExitCode :: TimeoutOptions -> Bool -> ExitCode -> IO ExitCode
handleExitCode opts didTimeOut exitCode = case exitCode of
  ExitFailure n
    | n < 0 -> do
        -- the child was killed by signal (-n)
        let sig = fromIntegral (negate n)
        unless didTimeOut $
          -- die by the same signal so our parent sees a signal death,
          -- but only after making sure we won't dump core ourselves
          ignoringExceptions $ do
            disabled <- disableCoreDumps
            when disabled $ do
              ignoringExceptions $ void $ Signals.installHandler sig Signals.Default Nothing
              Signals.raiseSignal sig
        let preserve = opts.preserveStatus || (didTimeOut && sig == Signals.sigKILL)
        return $
          if didTimeOut && not preserve
            then ExitFailure exitTimeout
            else ExitFailure (128 + negate n)
  _ ->
    return $
      if didTimeOut && not opts.preserveStatus
        then ExitFailure exitTimeout
        else exitCode

runTimeout :: TimeoutOptions -> String -> String -> [String] -> IO ExitCode
runTimeout opts duration cmd cmdArgs =
  do
    micros <- parseDuration duration
    parsedKillMicros <- maybe (return Nothing) (fmap Just . parseDuration) opts.killAfter
    let killMicros = parsedKillMicros >>= \delay -> if delay == 0 then Nothing else Just delay
    let termSig = determineSignal opts

    env <-
      Env opts cmd
        <$> newIORef termSig
        <*> newIORef False
        <*> newIORef killMicros
        <*> newMVar NotSpawned

    -- become a group leader so the whole job can be signalled;
    -- the child inherits this group
    unless opts.foreground $
      ignoringExceptions $ do
        pid <- Process.getProcessID
        void $ Process.createProcessGroupFor pid

    installCleanup env termSig

    -- hold the pid slot while spawning so a signal arriving mid-spawn
    -- waits for the pid instead of missing the child (GNU blocks signals
    -- around fork for the same reason)
    ph <- modifyMVar env.childPid $ \_ -> do
      ph <- startProcess cmd cmdArgs
      pid <- getProcessId ph
      return (Spawned pid, ph)
    -- don't stop if a background child needs the tty
    void $ Signals.installHandler Signals.sigTTIN Signals.Ignore Nothing
    void $ Signals.installHandler Signals.sigTTOU Signals.Ignore Nothing

    when (micros /= 0) $
      void $
        forkIO $
          threadDelay micros >> cleanup env Signals.sigALRM

    exitCode <- waitForProcess ph
    modifyMVar_ env.childPid (\_ -> return Reaped)
    didTimeOut <- readIORef env.timedOut
    handleExitCode opts didTimeOut exitCode
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
