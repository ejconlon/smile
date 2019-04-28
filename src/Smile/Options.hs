module Smile.Options where

import Smile.Cli.Parser
import Smile.Prelude

data StatsServerOptions = StatsServerOptions
    { _serverEnabled :: Bool
    , _serverHostname :: Text
    , _serverPort :: Int
    } deriving (Eq, Show)

$(makeSmileLenses ''StatsServerOptions)

-- data StatsCollectorOptions = StatsCollectorOptions
--     { _collectorEnabled :: Bool
--     , _collectorHostname :: Text
--     , _collectorPort :: Int
--     } deriving (Eq, Show)

-- $(makeSmileLenses ''StatsCollectorOptions)

data CoreOptions = CoreOptions
    { _verbose :: Bool
    , _gcMetrics :: Bool
    , _statsServerOptions :: StatsServerOptions
    -- , _statsCollectorOptions :: StatsCollectorOptions
    } deriving (Eq, Show)

$(makeSmileLenses ''CoreOptions)

data Options a = Options
    { _coreOptions :: !CoreOptions
    , _appOptions  :: !a
    } deriving (Eq, Show)

$(makeSmileLenses ''Options)

-- statsServerOptionsParser :: EnvParser StatsServerOptions
-- statsServerOptionsParser = do
--     enabledEnv <- environFlag "SMILE_STATS_SERVER_ENABLED"
--     hostnameEnv <- environDefault "SMILE_STATS_SERVER_HOSTNAME" "localhost"
--     portEnv <- environDefault "SMILE_STATS_SERVER_PORT" 8080
--     pure (StatsServerOptions
--         <$> switch
--             ( long "stats-server-enabled"
--             <> enabledEnv
--             )
--         <*> option auto
--             ( long "stats-server-hostname"
--             <> hostnameEnv
--             )
--         <*> option auto
--             ( long "server-stats-port"
--             <> portEnv
--             )
--         )

-- statsCollectorOptionsParser :: EnvParser StatsCollectorOptions
-- statsCollectorOptionsParser =
--     StatsCollectorOptions
--         <$> empty
--         <*> empty
--         <*> empty

-- coreOptionsParser :: EnvParser CoreOptions
-- coreOptionsParser = do
--     verboseEnv <- environFlag "SMILE_VERBOSE"
--     let verboseParser =
--             switch
--             ( long "verbose"
--             <> short 'v'
--             <> verboseEnv
--             <> help "Verbose output?"
--             )
--     gcMetricsEnv <- environFlag "SMILE_GC_METRICS"
--     let gcMetricsParser =
--             switch
--             ( long "gc-metrics"
--             <> short 'g'
--             <> gcMetricsEnv
--             <> help "Register GC metrics?"
--             )
--     serverParser <- statsServerOptionsParser
--     pure (CoreOptions
--         <$> verboseParser
--         <*> gcMetricsParser
--         <*> serverParser
--         )

coreOptionsParser :: Parser CoreOptions
coreOptionsParser = error "TODO coreOptionsParser"

optionsParser :: Parser a -> Parser (Options a)
optionsParser parser = Options <$> coreOptionsParser <*> parser
