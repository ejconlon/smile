module Main (main) where

import Options.Applicative
import Smile.App
import Smile.Core          (Core)
import Smile.Exe           (exe)
import Smile.Logging       (LogR)
import Smile.Prelude
import Smile.Refs          (readRef)
import Smile.Stats

data Config = Config
    { _param :: Int
    } deriving (Eq, Show)

$(makeSmileLenses ''Config)

data Metrics = Metrics
    { _someCounter :: Counter
    }

$(makeSmileLenses ''Metrics)

data Domain = Domain
    { _signal :: IORef Int
    }

$(makeSmileLenses ''Domain)

newtype MyApp = MyApp { _unMyApp :: App Metrics Domain }

$(makeSmileLenses ''MyApp)

instance HasApp MyApp Metrics Domain where
    appLens = unMyAppField

instance HasDomain MyApp where
    domainLens = unMyAppField . domainField

instance HasMetrics MyApp where
    metricsLens = unMyAppField . metricsField

configParser :: Parser Config
configParser =
    (Config
        <$> option auto (
            long "param"
            <> short 'p'
            <> help "some param"
            )
    )

initApp :: Config -> Core -> IO MyApp
initApp config core = do
    metrics <- Metrics <$> newCounter
    domain <- Domain <$> newIORef (_param config)
    pure (MyApp (App core metrics domain))

prepare :: (HasStore env, HasMetrics env, LogR env) => RIO env ()
prepare = do
    logInfo "Registering metrics"
    _ <- registerCounter "some.counter" (metricsLens . someCounterField)
    logInfo "Starting server"
    _ <- forkServer "localhost" 8000
    pure ()

run :: (HasDomain env, LogR env) => RIO env ()
run = do
    logInfo "We're inside the application!"
    sigVal <- readRef (domainLens . signalField)
    logInfo ("Got signal " <> display sigVal)
    threadDelay 50000000

main :: IO ()
main = exe configParser initApp (flip runRIO (prepare >> run))
