module Examples.Example2 where
--todo: Look into this 
{-
import Set_up
    ( Verdict(..), StreamV, StreamZ, StreamS, vConjunction, vDisjunction, vOr, asVerdict, monitor )



sIoT :: [[(String, (Integer, Bool))]]
sDI :: Integer -> [Integer]
sO :: Integer -> Verdict
s1 :: StreamV
s2 :: StreamZ
s3 :: StreamZ
s4 :: StreamZ
s5 :: StreamZ
s6 :: StreamV
s7 :: StreamS
s8 :: StreamS
s9 :: StreamZ

sIoT = zipWith (\ p1 p2 -> [("Roomba", (p1, True)), ("Fridge", (p2, True))]) device1 device2
    where
        device1 = [50 | x <- [1..]]
        device2 = [200 + 2*x | x <- [1..]]

-- Property = Always[0,8](Eventually[2,5] t = 3)
sDISequence :: [[Integer]]
sDISequence = scanl step [0] [1..]
  where
    step previousState t = previousState ++ [t]


sDI t = sDISequence !! fromInteger t
sO t = vConjunction $ map (\t' -> s1 t' Nothing t) $ sDI t


s1 t d t' = asVerdict $ s2 t d t' < s9 t d t'

s2 t d t' = sum $ map (s3 t d) [t..t']
s3 t d t' = sum $ map (\d' -> s4 t (Just d') t') (sIoT !! fromInteger t')
s4 t d t' = s5 t d t' * toInteger (fromEnum (s6 t d t' == TTrue))
s5 _ d _ = maybe 0 (fst . snd) d
s6 t d t' = asVerdict $ s7 t d t' == s8 t d t'
s7 _ d _ = maybe "Unknown" fst d
s8 _ _ _ = "Roomba"
s9 _ _ _ = 200




runMonitor :: Integer -> [([Integer], Verdict)]
runMonitor maxT = monitor maxT (>= 0) s1



property = "always(sumtime(power * name = Roomba) < 200Ws)"

-}