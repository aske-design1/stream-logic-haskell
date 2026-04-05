module Stream.Types where

import Stream.Verdict ( Verdict (..), Verdict(TTrue) )

import qualified Data.IntMap.Strict as IntM
import Data.Sequence as S

import GHC.Stack (HasCallStack)

data SOutput = Str String | Ver Verdict | Num Int
    deriving (Show, Eq)

instance Num SOutput where
    (Num a) + (Num b) = Num (a + b)
    (Ver a) + (Num b) 
        | a == TTrue = Num $ 1 + b
        | otherwise = Num b
    (Num a) + (Ver b) 
        | b == TTrue = Num $ 1 + a
        | otherwise = Num a

    _ + _ = undefined

    (Num a) - (Num b) = Num (a - b)
    _ - _ = undefined

    (Num a) * (Num b) = Num (a * b)
    (Ver a) * b
        | a == TTrue = b
        | otherwise = Num 0
    a * (Ver b) 
        | b == TTrue = a
        | otherwise = Num 0
    _ * _ = undefined

    abs = undefined
    signum (Num a) = Num (signum a)
    signum _ = undefined
    
    fromInteger = Num . fromIntegral

    negate (Num a) = Num (negate a)
    negate _ = undefined
    

-- Types
type IoT = (String, Int, Verdict)
type EIoT = Maybe IoT 

type StreamIoT = Int -> [IoT]
type Devices = S.Seq [IoT] 

type SDIState = IntM.IntMap Verdict
data StreamOutState = State {
    verdicts :: SDIState,
    insert :: SDIState -> Int -> SDIState,
    update :: SDIState -> Devices  -> Int -> SDIState,
    getColVerdict :: SDIState -> Verdict,
    cleanUp :: SDIState -> SDIState
}
type StreamO = StreamOutState
type StreamD = Int -> Devices -> EIoT -> Int -> SOutput

type Env = (IntM.IntMap StreamO, IntM.IntMap StreamD)


-- Helper Functions
getVerSafe :: SOutput -> Maybe Verdict
getVerSafe (Ver v) = Just v
getVerSafe _ = Nothing

getStrSafe :: SOutput -> Maybe String
getStrSafe (Str v) = Just v
getStrSafe _ = Nothing

getNumSafe :: SOutput -> Maybe Int
getNumSafe (Num v) = Just v
getNumSafe _ = Nothing


getVerUnsafe :: HasCallStack => SOutput -> Verdict
getVerUnsafe (Ver v) = v
getVerUnsafe x       = error $ "Typecheck error: Expected Verdict, got " ++ show x

getStrUnsafe :: HasCallStack => SOutput -> String
getStrUnsafe (Str v) = v
getStrUnsafe x = error $ "Typecheck error: Expected String, got " ++ show x

getNumUnsafe :: HasCallStack => SOutput -> Int
getNumUnsafe (Num v) = v
getNumUnsafe x = error $ "Typecheck error: Expected Number, got " ++ show x

toVerdictCoercion :: SOutput -> Verdict
toVerdictCoercion (Num n) 
    | n == 0 = FFalse
    | otherwise = TTrue

toVerdictCoercion (Ver v) = v

toVerdictCoercion x = error $ "Typecheck error: Expected Verdict or Number, got " ++ show x

toNumCoercion :: SOutput -> Int
toNumCoercion (Num n) = n
toNumCoercion (Ver v)
    | v == TTrue = 1
    | otherwise = 0
toNumCoercion x = error $ "Typecheck error: Expected Verdict or Number, got " ++ show x