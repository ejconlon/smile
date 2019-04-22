module Smile.Options where

import Options.Applicative
import Smile.Prelude

data CoreOptions = CoreOptions
    { _verbose :: !Bool
    } deriving (Eq, Show)

$(makeSmileLenses ''CoreOptions)

data Options a = Options
    { _coreOptions :: CoreOptions
    , _appOptions :: a
    } deriving (Eq, Show)

$(makeSmileLenses ''Options)

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
