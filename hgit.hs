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
import Options.Applicative as Opt
import Prelude hiding (putStr)
import System.Exit
import System.Environment
import System.IO (stderr)
import System.Process (CreateProcess, proc)
import System.Process.ByteString
import System.Process.Common (showCreateProcessForUser)
import Text.PrettyPrint.HughesPJClass hiding ((<>))

toOption :: a -> Bool -> Set a
toOption a = bool Set.empty (singleton a)

data Git =
    Add (Set AddOption) [PathSpec]
  | Checkout FilePath
  | Clone Repository (Maybe Directory)
  | Pull
  | Push
  | Reset (Set ResetOptions)
  | Show Commit
  deriving Show

data PathSpec = PathSpecPath FilePath deriving Show
instance Pretty PathSpec where pPrint (PathSpecPath path) = text path

pathSpec :: Opt.Parser PathSpec
pathSpec = PathSpecPath <$> argument str (metavar "PATH")

data AddOption = AddPatch | AddHelp | AddDryRun deriving (Show, Eq, Ord)

instance Pretty AddOption where
  pPrint AddPatch = text "--patch"
  pPrint AddHelp = text "--help"
  pPrint AddDryRun = text "-n"

gitP :: Opt.Parser Git
gitP = subparser $
       mconcat
       [ command "add" (info gitAdd (progDesc "git add"))
       , command "clone" (info gitClone (progDesc "git clone")) ]

-- | The git add command.
-- > > withArgs ["--patch", "file1", "file2"] (execParser (info gitAdd fullDesc))
-- > Add (fromList [Patch]) [PathSpecPath "file1",PathSpecPath "file2"]
gitAdd :: Opt.Parser Git
gitAdd =
  Add <$> opts <*> many pathSpec
  where
    opts :: Opt.Parser (Set AddOption)
    opts = liftA3 (\a b c -> a <> b <> c)
             (toOption AddPatch <$> switch (long "patch" <> short 'p'))
             (toOption AddHelp <$> switch (long "help"))
             (toOption AddDryRun <$> switch (long "dry-run" <> short 'n'))

type Repository = String
type Directory = String

gitClone :: Opt.Parser Git
gitClone =
  Clone <$> repository <*> optional directory
  where
    repository = argument str (metavar "URL")
    directory = argument str (metavar "PATH")

{-
options :: Opt.Parser Git
options =
  command
  <$> argument str (metavar "COMMAND")
  <*> (\case
          "add" -> gitAdd
          "checkout" -> gitCheckout
          "clone" -> gitClone
          "pull" -> gitPull
          "push" -> gitPush
          "reset" -> gitReset
          "show" -> gitShow
          _ -> gitHelp)
 where
   command :: String -> (String -> Git) -> Git
  
  fromMaybe help $ msum [add, checkout, clone, pull, push, reset, show]
  where
    add :: Opt.Parser (Maybe Git)
    add

test :: String -> IO String
test cmd =
  withArgs (words cmd) (execParser (info (argument str (metavar "COMMAND")) fullDesc))
-}

newtype RepositoryURL = RepositoryURL String deriving Show
type Commit = ()

data ResetOptions = Hard deriving Show

gitCreateProcess :: Git -> CreateProcess
gitCreateProcess (Add os ps) =
  proc "git" ("add" : fmap prettyShow (Set.toList os) <> fmap prettyShow ps)
gitCreateProcess x = error (show x)

-- | gitCreateProcess (withArgs ["--help"] (execParser (info gitAdd fullDesc)))
gitRun :: Git -> IO ()
gitRun git@(Add opts args)
  | member AddDryRun opts = do
      putStrLn (show git)
      putStrLn (show (gitCreateProcess git))
      putStrLn (showCreateProcessForUser (gitCreateProcess git))
gitRun git =
  readCreateProcessWithExitCode (gitCreateProcess git) "" >>= \case
    (ExitSuccess, out, "") -> putStr out
    (code@(ExitFailure n), "" , err) -> hPutStr stderr err >> exitWith code
    (ExitSuccess, out, err) -> do
      putStrLn ("err=" <> show err)
      putStr out
    (code, out, err) -> do
      putStrLn ("out=" <> show out)
      putStrLn ("err=" <> show err)
      exitWith code

main :: IO ()
main = do
  args <- getArgs
  withArgs (tail args) (execParser (info gitAdd fullDesc)) >>= gitRun
