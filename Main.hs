module Mainabc where
import System.CPUTime
import Data.Tuple (swap)
import System.Environment
import System.Console.GetOpt
import Checkhearse
import Control.Monad
import System.Console.ANSI

-- Options record
data Options = Options {
    optHelp              :: Bool
} deriving Show

defaultOptions :: Options
defaultOptions = Options {
    optHelp = False
}


options :: [OptDescr (Options -> Options)]
options = [
    Option ['h'] ["help"] (NoArg (\opts -> opts { optHelp = True })) "Print a help message and exit."
    ]

-- Main IO function
main :: IO()
main = do
  args <- getArgs
  (opts, errs) <- compilerOpts args
  if not (null errs) 
  then do
         mapM putStrLn errs
         return ()
  else if optHelp opts 
       then helpIO
       else putStrLn $ "Yea!! It compliles! Give yourself a pat on the back. So proud!!!"

-- Return the list of flags
compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
     (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: classifier [OPTION...]"

-- Print help
helpIO :: IO()
helpIO = putStrLn $ usageInfo usage options
    where usage = "Usage: classifier [OPTION...]"