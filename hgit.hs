#!/usr/bin/env runghc

-- ^ I have a limited knowledge of the git command line interface.
-- This haskell type encapsulates it, and provides an avenue for
-- extending it.

{-# LANGUAGE LambdaCase, OverloadedLists, OverloadedStrings, ScopedTypeVariables #-}

import Control.Monad
import Data.Bool
import Data.ByteString (hPutStr, putStr)
import Data.Maybe
import Data.Set as Set
import Data.Typeable (typeOf)
import Options.Applicative as Opt
import Prelude hiding (putStr)
import System.Exit
import System.Environment
import System.IO (stderr)
import System.Process (CreateProcess, proc)
import System.Process.ByteString
import System.Process.Common (showCreateProcessForUser)
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Control.Applicative.HT

main :: IO ()
main = do
  args <- getArgs
  git <- withArgs args (execParser (info gitP fullDesc))
  putStrLn ("-- " <> show git <> " :: " <> show (typeOf git))
  gitRun git

-- | gitCreateProcess (withArgs ["--help"] (execParser (info gitAdd fullDesc)))
gitRun :: Git -> IO ()
gitRun git@(Git gitopts _)
  | member GitDryRun gitopts = do
      putStrLn (show git)
      putStrLn (show (gitCreateProcess git))
      putStrLn (showCreateProcessForUser (gitCreateProcess git))
gitRun git = do
  let p = gitCreateProcess git
  putStrLn ("-- " <> showCreateProcessForUser p <> " :: " <> show (typeOf p))
  readCreateProcessWithExitCode p "" >>= \case
    (ExitSuccess, out, "") -> putStr out
    (code@(ExitFailure n), "" , err) -> hPutStr stderr err >> exitWith code
    (ExitSuccess, out, err) -> do
      putStrLn ("err=" <> show err)
      putStr out
    (code, out, err) -> do
      putStrLn ("out=" <> show out)
      putStrLn ("err=" <> show err)
      exitWith code

-- * Parsers

-- | Global git parser
gitP :: Parser Git
gitP = Git <$> gitOptionsP <*> optional gitCommandP

data Git = Git (Set GitOption) (Maybe GitCommand) deriving Show

gitOptionsP :: Parser (Set GitOption)
gitOptionsP =
  lift3 (\a b c -> a <> b <> c)
    (toOption GitHelp <$> switch (long "help"))
    (toOption GitDryRun <$> switch (long "dry-run" <> short 'n'))
    -- (toOption GitVerbose <$> switch (long "verbose"))
    (toOption GitVersion <$> switch (long "version"))

data GitOption =
    GitHelp
  | GitDryRun
  | GitVersion
  deriving (Show, Eq, Ord)

instance Pretty GitOption where
  pPrint GitHelp = text "--help"
  pPrint GitDryRun = text "--dry-run"
  pPrint GitVersion = text "--version"

-- | Git sub-command parser
gitCommandP :: Parser GitCommand
gitCommandP = subparser $
       mconcat
       [ command "add" (info gitAddP (progDesc "git add command"))
       , command "clone" (info gitCloneP (progDesc "git clone command"))
       , command "checkout" (info gitCheckoutP (progDesc "git checkout command"))
       , command "pull" (info gitPullP (progDesc "git pull command"))
       , command "push" (info gitPushP (progDesc "git push command"))
       , command "reset" (info gitResetP (progDesc "git reset command"))
       , command "show" (info gitShowP (progDesc "git show command"))
       ]

data GitCommand =
    Add (Set AddOption) [PathSpec]
  | Checkout FilePath
  | Clone Repository (Maybe Directory)
  | Pull
  | Push
  | Reset (Set ResetOption)
  | Show String
  deriving Show

gitCreateProcess :: Git -> CreateProcess
gitCreateProcess (Git opts mcmd) =
  proc "git" (fmap prettyShow (Set.toList opts) <> maybe [] args mcmd)
  where
    args (Add os ps) =
      "add" : fmap prettyShow (Set.toList os) <> fmap prettyShow ps
    args (Checkout path) = ["checkout", path]
    args (Clone repo mdir) =
      ["clone", repo] <> maybe [] (: []) mdir
    args Pull = ["pull"]
    args Push = ["push"]
    args (Reset opts) = ("reset" : fmap prettyShow (Set.toList opts))
    args (Show s) = ["show", s]

-- * Sub-parsers

-- | Add
-- > > withArgs ["--patch", "file1", "file2"] (execParser (info gitAdd fullDesc))
-- > Add (fromList [Patch]) [PathSpecPath "file1",PathSpecPath "file2"]
gitAddP :: Parser GitCommand
gitAddP = Add <$> opts <*> many pathSpec
  where
    opts :: Parser (Set AddOption)
    opts = lift2 (\a b -> a <> b)
             (toOption AddPatch <$> switch (long "patch" <> short 'p'))
             (toOption AddHelp <$> switch (long "help"))

data AddOption = AddPatch | AddHelp deriving (Show, Eq, Ord)

instance Pretty AddOption where
  pPrint AddPatch = text "--patch"
  pPrint AddHelp = text "--help"

-- | Clone
gitCloneP :: Parser GitCommand
gitCloneP =
  Clone <$> repository <*> optional directory
  where
    repository = argument str (metavar "URL")
    directory = argument str (metavar "PATH")

type Repository = String
type Directory = String

-- | Checkout
gitCheckoutP :: Parser GitCommand
gitCheckoutP = Checkout <$> argument str (metavar "PATH")

-- | Pull
gitPullP :: Parser GitCommand
gitPullP = pure Pull

-- | Push
gitPushP :: Parser GitCommand
gitPushP = pure Push

-- | Reset
gitResetP :: Parser GitCommand
gitResetP = Reset <$> opts
  where
    opts :: Parser (Set ResetOption)
    opts = toOption ResetHard <$> switch (long "hard")

data ResetOption = ResetHard deriving (Show, Eq, Ord)

instance Pretty ResetOption where pPrint ResetHard = text "hard"

-- | Show
gitShowP :: Parser GitCommand
gitShowP = Show <$> argument str (metavar "COMMIT")

-- * Argument types

data PathSpec = PathSpecPath FilePath deriving (Show, Eq, Ord)
instance Pretty PathSpec where pPrint (PathSpecPath path) = text path

pathSpec :: Parser PathSpec
pathSpec = PathSpecPath <$> argument str (metavar "PATH")

-- * Utilities

toOption :: a -> Bool -> Set a
toOption a = bool Set.empty (singleton a)
