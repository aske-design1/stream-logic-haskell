module Stream.Types where

import Stream.Verdict ( Verdict )

import qualified Data.IntMap.Strict as M

data Out = Str String | Ver Verdict | Num Int
    deriving (Show, Eq)

-- Types
type EIoT = Maybe (String, Int, Bool)

type StreamD = Int -> EIoT -> Int -> Out
type StreamO = Int -> Verdict
type StreamDI = Int -> M.IntMap Verdict

type Env = (M.IntMap StreamO, M.IntMap StreamDI, M.IntMap StreamD)


-- Helper Functions
getVerSafe :: Out -> Maybe Verdict
getVerSafe (Ver v) = Just v
getVerSafe _ = Nothing

getStrSafe :: Out -> Maybe String
getStrSafe (Str v) = Just v
getStrSafe _ = Nothing

getNumSafe :: Out -> Maybe Int
getNumSafe (Num v) = Just v
getNumSafe _ = Nothing
