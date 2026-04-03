module Examples.Example1 where

--todo: Look into this 
{-

import Set_up
    ( Verdict(..), StreamV, StreamZ, vConjunction, vDisjunction, vOr, asVerdict, monitor )

-- Property = Always[0,8](Eventually[2,5] t = 3)
sDISequence :: [[Integer]]
sDISequence = scanl step [0] [1..]
  where
    step previousState t
        | t <= 8    = previousState ++ [t]
        | otherwise = previousState


-- To get sDI for t=10, you just grab the 10th element (0-indexed)
sDI :: Integer -> [Integer]
sDI t = sDISequence !! fromInteger t

sO :: Integer -> Verdict
sO t = vConjunction $ map (\t' -> s1 t' Nothing t) $ sDI t

s1 :: StreamV
s1 t d t' = vDisjunction (map (s2 t d) [t..min t' (t+5)]) `vOr` cases
    where 
        cases 
            | t' < t + 5 = Undecided
            | otherwise = FFalse 


s2 :: StreamV
s2 t d t' = asVerdict $ s3 t d t' == s4 t d t'

s3 :: StreamZ
s3 _ _ t' = t'
s4 :: StreamZ
s4 _ _ _ = 3

-- Use the same Verdict and EIoT definitions from your previous code...

-- The "Step" function: Takes (Current Active t's, Current Time t) 
-- Returns (Updated Active t's, Overall Verdict for this step)

runMonitor :: Integer -> [([Integer], Verdict)]
runMonitor maxT = monitor maxT (\t -> t >= 0 && t <= 8) s1 


property = "Property: Always[0,8](Eventually[2,5] t % 3 = 0)"
-}