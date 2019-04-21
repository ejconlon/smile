module Smile.Options
    ( CoreOptions (..)
    , Options (..)
    , coreOptionsParser
    , optionsParser
    ) where

import Options.Applicative
import Smile.Prelude

data CoreOptions = CoreOptions
    { verbose :: !Bool
    } deriving (Generic, Eq, Show)

data Options a = Options
    { coreOptions :: CoreOptions
    , appOptions :: a
    } deriving (Generic, Eq, Show)

coreOptionsParser :: Parser CoreOptions
coreOptionsParser =
    (CoreOptions
        <$> switch (
            long "verbose"
            <> short 'v'
            <> help "Verbose output?"
            )
    )

optionsParser :: Parser a -> Parser (Options a)
optionsParser parser = Options <$> coreOptionsParser <*> parser
