module Main (main) where

import Options.Applicative
import Smile.App
import Smile.Core          (Core)
import Smile.Exe           (exe)
import Smile.Logging       (LogR)
import Smile.Prelude
import Smile.Refs          (readRef)
import System.Metrics      (Store)
import System.Remote.Monitoring (forkServerWith)

data Config = Config
  { _param :: Int
  } deriving (Eq, Show)

$(makeSmileLenses ''Config)

data Value = Value
  { _signal :: IORef Int
  } deriving (Generic)

$(makeSmileLenses ''Value)

-- BEGIN TemplateHaskell candidate section
newtype MyApp = MyApp { _unMyApp :: App Value }

$(makeSmileLenses ''MyApp)

instance Has Core MyApp where
  hasLens = _unMyAppLens . hasLens

instance Has Value MyApp where
  hasLens = _unMyAppLens . _restLens
-- END TemplateHaskell candidate section

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
  pure (MyApp (App (Value signal) core))

prepare :: (Has Store env, LogR env) => RIO env ()
prepare = do
  logInfo "Starting server"
  store <- view hasLens
  _ <- liftIO (forkServerWith store "0.0.0.0" 8000)
  pure ()

run :: (Has Value env, LogR env) => RIO env ()
run = do
  logInfo "We're inside the application!"
  sigVal <- readRef _signalLens
  logInfo ("Got signal " <> display sigVal)
  threadDelay 50000000

main :: IO ()
main = exe configParser initApp (flip runRIO (prepare >> run))
