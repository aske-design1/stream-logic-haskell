module Stream.Verdict where

-- Verdict Behaviour
data Verdict = TTrue | FFalse | Undecided
    deriving (Show, Eq)

(&&&) :: Verdict -> Verdict -> Verdict
FFalse &&& _ = FFalse
_ &&& FFalse = FFalse
TTrue &&& TTrue = TTrue
_ &&& _ = Undecided

(|||) :: Verdict -> Verdict -> Verdict
TTrue ||| _ = TTrue
_ ||| TTrue = TTrue
FFalse ||| FFalse = FFalse
_ ||| _ = Undecided

nnot :: Verdict -> Verdict
nnot TTrue = FFalse
nnot FFalse = TTrue
nnot Undecided = Undecided

asVerdict :: Bool -> Verdict
asVerdict True = TTrue
asVerdict False = FFalse


vConjunction :: [Verdict] -> Verdict
vConjunction = foldr (&&&) TTrue
vDisjunction :: [Verdict] -> Verdict
vDisjunction = foldr (|||) FFalse
