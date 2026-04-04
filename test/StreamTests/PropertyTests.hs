module StreamTests.PropertyTests where

import Test.HUnit
import Program.CST
import Stream.CSTConversion (convertCSTToStreams)
import Data.IntMap as M ( (!), empty, size, toList, map, mapWithKey, foldlWithKey )

import Stream.Types as So
import Stream.Verdict (Verdict(Undecided), vConjunction)
import qualified Stream.Types as SO

import Data.Sequence as S (empty)
import Stream.Verdict (Verdict(FFalse))

testStreamOutState = TestCase $
    let
        n = Val . VNum
        expr = BinOp LessThan (n 50) (Val VTime)
        prop = Prop Always None

        s = (M.! 0) . fst . convertCSTToStreams $ prop expr

        verdictsUnde = foldl (So.insert s) M.empty ([0..49] :: [Int])

        updateFunc state t = So.update s state S.empty t 

    in
        assertEqual "Check Size of verdict" 50 (M.size verdictsUnde) >>
        assertEqual "Check whether all elements are undecided" True (all ((== Undecided) . snd) (toList verdictsUnde)) >>
        assertEqual "Check whether all elements are undecided at time 49" True (all ((== Undecided) . snd) (toList (verdicts s))) >>
        assertEqual "Check that all elements are false at time 50" True (all ((== FFalse) . snd) (M.toList $ updateFunc verdictsUnde 50))


propertyTests :: Test
propertyTests = TestList
    [
        TestLabel "Testing the Out Stream" testStreamOutState
    ]