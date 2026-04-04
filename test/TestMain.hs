module Main where

import Test.HUnit
import StreamTests.VerdictTest ( verdictTests )
import StreamTests.ExpressionTests ( expressionTests )
import StreamTests.PropertyTests ( propertyTests )
import System.IO ( stdout )
import System.Exit (exitSuccess, exitFailure)


allTests :: Test
allTests = TestList 
    [ 
        TestLabel "Verdict Logic" verdictTests,
        TestLabel "Expression Logic" expressionTests,
        TestLabel "Property Logic" propertyTests
    ]


main :: IO ()
main = do
    -- 1. Run the tests and get the report
    (counts, _) <- runTestText (putTextToHandle stdout True) allTests
    
    -- 2. Calculate Succeded
    -- Successes = Tried - (Errors + Failures)
    let totalTried = tried counts
    let totalFailed = errors counts + failures counts
    let totalPassed = totalTried - totalFailed

    -- 3. Print your custom summary
    putStrLn "\n\ESC[32m--- TEST SUMMARY ---\ESC[0m"
    putStrLn $ "Total Tests Run: " ++ show totalTried
    putStrLn $ "Succeeded:    " ++ show totalPassed
    putStrLn $ "Failed:       " ++ show totalFailed
    putStrLn "--------------------\n"

    if totalFailed > 0 then exitFailure else exitSuccess