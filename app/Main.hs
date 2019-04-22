module Main (main) where

import Options.Applicative
import Smile.App
import Smile.Core (Core)
import Smile.Exe (exe)
import Smile.Logging (LogC)
import Smile.Refs (readRef)
import Smile.Prelude

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

run :: (Has Value env, LogC env m) => m ()
run = do
  logInfo "We're inside the application!"
  sigVal <- readRef (hasLens . _signalLens)
  logInfo ("Got signal " <> display sigVal)

main :: IO ()
main = exe configParser initApp (flip runRIO run)
