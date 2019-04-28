module Smile.Cli.Args where

import qualified RIO.Text as Text
import Smile.Prelude

data Arg = Arg Int (Maybe Text) deriving (Eq, Show)

textToArg :: Text -> Arg
textToArg t = go 0 t where
    go i u =
        case Text.uncons u of
            Nothing -> Arg i Nothing
            Just (v, w) ->
                case v of
                    '-' -> go (i+1) w
                    _ -> Arg i (Just u)

argToText :: Arg -> Text
argToText (Arg i m) = (Text.replicate i "-") <> (fromMaybe "" m)

data ArgPat =
      PositionalPat Text
    | ShortPat Text
    | LongPat Text
    | DoubleDashPat
    deriving (Eq, Show)

argToPat :: Arg -> Maybe ArgPat
argToPat a =
    case a of
        Arg 0 (Just u) -> Just (PositionalPat u)
        Arg 1 (Just u) -> Just (ShortPat u)
        Arg 2 (Just u) -> Just (LongPat u)
        Arg 2 Nothing -> Just DoubleDashPat
        _ -> Nothing

patToArg :: ArgPat -> Arg
patToArg p =
    case p of
        PositionalPat u -> Arg 0 (Just u)
        ShortPat u -> Arg 1 (Just u)
        LongPat u -> Arg 2 (Just u)
        DoubleDashPat -> Arg 2 Nothing
