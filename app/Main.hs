module Main (main) where

import Options.Applicative
import Smile.App
import Smile.Core          (Core)
import Smile.Exe           (exe)
import Smile.Logging       (LogR)
import Smile.Prelude
import Smile.Refs          (readRef)
import Smile.Stats         (HasStore (..), forkServer)

data Config = Config
    { _param :: Int
    } deriving (Eq, Show)

$(makeSmileLenses ''Config)

data Domain = Domain
    { _signal :: IORef Int
    } deriving (Generic)

$(makeSmileLenses ''Domain)

newtype MyApp = MyApp { _unMyApp :: App Domain }

$(makeSmileLenses ''MyApp)

instance HasApp MyApp Domain where
    appLens = unMyAppField

instance HasDomain MyApp where
    domainLens = unMyAppField . restField

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
    signal <- newIORef (_param config)
    pure (MyApp (App (Domain signal) core))

prepare :: (HasStore env, LogR env) => RIO env ()
prepare = do
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
