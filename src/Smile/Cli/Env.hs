module Smile.Cli.Env where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified RIO.Text as Text
import Smile.Cli.Args
import Smile.Prelude
import qualified System.Environment as SysEnv

type Args = Seq Arg
type Vars = Map Text Text

getArgs :: IO Args
getArgs = Seq.fromList . fmap (textToArg . Text.pack) <$> SysEnv.getArgs

getVars :: IO Vars
getVars = Map.fromList . fmap (\(a, b) -> (Text.pack a, Text.pack b)) <$> SysEnv.getEnvironment
