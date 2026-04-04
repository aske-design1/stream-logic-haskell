module StreamTests.PropertyTests where

import Debug.Trace (trace)
import Test.HUnit
import Program.CST
import Stream.CSTConversion (convertCSTToStreams)
import Data.IntMap as M ( (!), empty, size, toList, map, mapWithKey, foldlWithKey )

import Stream.Types as So
import Stream.Verdict
    ( Verdict(Undecided), Verdict(FFalse), Verdict(TTrue) )
import qualified Stream.Types as SO
import Data.Sequence as S (empty)
import Stream.Rules.Expression (evalExpr)
import Data.Function (on)

testStreamOutState = TestCase $
    let
        n = Val . VNum
        expr = BinOp LessThan (Val VTime) (n 50)
        prop = Prop Always None

        s = (M.! 0) . fst . convertCSTToStreams $ prop expr

        verdictsUnder50 =  foldl (So.insert s) M.empty ([0..49] :: [Int])
        verdictsWith50 = So.insert s verdictsUnder50 50

        updateFunc state = So.update s state S.empty

        cleanUpState = (So.cleanUp s .) . updateFunc
        cleanUpBefore50 =  cleanUpState verdictsUnder50 49
        cleanUpAfter50 = cleanUpState verdictsWith50 50
    in
        -- Insert
        assertEqual "Check Size of verdict" 50 (M.size verdictsUnder50) >>
        assertEqual "Check whether all elements are undecided" True (all ((== Undecided) . snd) (toList verdictsUnder50)) >>
        assertEqual "Check whether all elements are undecided at time 49" True (all ((== Undecided) . snd) (toList verdictsUnder50)) >>
        -- Update
        assertEqual "Check that all elements are true at time 49" True (all ((== TTrue) . snd) (M.toList $ updateFunc verdictsUnder50 49)) >>
        assertEqual "Check that all elements are false at time 50" True (all ((== FFalse) . snd) (M.toList $ updateFunc verdictsWith50 50)) >>
        -- Clean up
        assertEqual "Check Size after clean up at t = 49" 0 (M.size cleanUpBefore50) >>
        assertEqual "Check Size after clean up at t = 50" 0 (M.size cleanUpAfter50) >>
        -- Verdicts
        assertEqual "Check that the final verdict before being evaluated" Undecided (getColVerdict s verdictsUnder50) >>
        assertEqual "Check that the final verdict before 50" TTrue (getColVerdict s $ updateFunc verdictsUnder50 49) >>
        assertEqual "Check that the final verdict after 50" FFalse (getColVerdict s $ updateFunc verdictsWith50 50)

testMultipleProps = TestCase $
    let
        n = Val . VNum
        expr = BinOp LessThan (Val VTime) (n 50)
        prop = Prop Always None expr

        propComp = Comp prop prop
        env = convertCSTToStreams propComp
    in
        assertEqual "Check key val after evaluating" 8 (let (s1, s2) = env in M.size s1 + M.size s2) >>
        assertEqual "Check StreamO's" 2 (M.size $ fst env)



propertyTests :: Test
propertyTests = TestList
    [
        TestLabel "Testing the Out Stream" testStreamOutState,
        TestLabel "Testing The Composition Function" testMultipleProps
    ]