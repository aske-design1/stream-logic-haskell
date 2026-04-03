module Examples.SetUp where

--todo: Look into this 
{-
import Example1 ( sDI, sO, property, runMonitor )
import Example2 as E ( sDI, sO, property, runMonitor )
import Stream.Verdict (Verdict, EIoT)

main :: IO ()
main = do 
    putStrLn E.property

    putStrLn "Without Deletion"
    runPropertyTest E.sO E.sDI
    putStrLn "\nWith Deletion"
    runPropertyTest2 E.runMonitor



runPropertyTest :: (Integer -> Verdict) -> (Integer -> [Integer]) -> IO ()
runPropertyTest sOFunc sDIFunc = do
    putStrLn " t  | sDI(t)                  | sO(t) Verdict"
    putStrLn "--------------------------------------------"
    -- Run for t from 0 to 20
    mapM_ runStep [0..10]
  where
    runStep t = do
        let result = sOFunc t
        let history = sDIFunc t
        -- Formatting the output for clarity
        putStrLn $ pad 2 (show t) ++ " | " ++ 
                   pad 22 (show history) ++ " | " ++ 
                   show result

    -- Simple helper to align columns
    pad n str = str ++ replicate (n - length str) ' '



runPropertyTest2 :: (Integer -> [([Integer], Verdict)]) -> IO ()
runPropertyTest2 monitorFunc = do
    let tLimit = 20
    let results = tail (monitorFunc tLimit) -- Tail skips the initial empty state
    putStrLn " t  | Active Tasks (t')      | Step Verdict"
    putStrLn "-------------------------------------------"
    
    mapM_ printRow (zip [0..tLimit] results)
  where
    printRow (t, (pending, verdict)) = 
        putStrLn $ pad 3 (show t) ++ " | " ++ 
                   pad 22 (show (reverse pending)) ++ " | " ++ 
                   show verdict

    pad n str = str ++ replicate (n - length str) ' '


-}