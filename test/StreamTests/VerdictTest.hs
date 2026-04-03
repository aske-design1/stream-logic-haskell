module StreamTests.VerdictTest where

import Stream.Verdict 
import Test.HUnit 


--- Conjunction, Disjunction
testConjunction = TestCase $ do
    let t = TTrue
    let f = FFalse
    let d = Undecided
    assertEqual "Should be True" (vConjunction [t, t, t, t, t, t, t, t, t, t, t]) t
    assertEqual "Should be False" (vConjunction [t, t, t, f, t, t, t, t, t, t, t]) f
    assertEqual "Should be Undecided" (vConjunction [t, t, t, t, t, t, d, t, t, t, t]) d
    assertEqual "Should be False" (vConjunction [t, t, f, t, t, t, d, t, t, t, t]) f


testDisjunction = TestCase $ do
    let t = TTrue
    let f = FFalse
    let d = Undecided

    assertEqual "Should be False" (vDisjunction [f, f, f, f, f, f, f, f, f, f]) f
    assertEqual "Should be True" (vDisjunction [f, f, t, f, f, f, f, f, f, f]) t
    assertEqual "Should be Undecided" (vDisjunction [f, f, f, d, f, f, f, f, f, f]) d
    assertEqual "Should be True" (vDisjunction [f, f, f, t, f, d, f, f, f, f]) t

verdictTests :: Test
verdictTests = TestList 
    [
        TestLabel "Conjunction" testConjunction,
        TestLabel "Disjunction" testDisjunction
    ]