module Main (main) where

import Options.Applicative.Simple
import Smile.App
import Smile.Prelude

parse :: IO Options
parse = do
  (options, ()) <- simpleOptions
    "0.1"
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  pure options

run :: LogC env m => m ()
run = do
  logInfo "We're inside the application!"

main :: IO ()
main = do
  options <- parse
  exe options (flip runRIO run)
