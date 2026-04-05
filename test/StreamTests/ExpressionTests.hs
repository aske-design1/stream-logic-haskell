module StreamTests.ExpressionTests where

import Test.HUnit
import Program.CST

import qualified Data.IntMap as M
import Stream.Rules.Expression
import Stream.Types
import Stream.Verdict (Verdict(FFalse, TTrue, Undecided))
import Data.Sequence as S

devices = S.empty
initExprEval expr = evalExpr expr 0 M.empty
s idx = (M.! idx) . fst . initExprEval

testTimeVal = TestCase $
    let
        eval = Val VTime
        (funcMap, _) = evalExpr eval 0 M.empty
    in
        assertEqual "Time at 2 should return 2" ((funcMap M.! 0) 0 devices Nothing 2) (Num 2) >>
        assertEqual "Time at 5 should return 5" ((funcMap M.! 0) 3 devices Nothing 5) (Num 5)

testConstantVal = TestCase $
    let
        (funcMap1, _) = evalExpr (Val (VNum 30)) 0 M.empty
        (funcMap2, _) = evalExpr (Val (VNum 45)) 1 funcMap1
        (funcMap3, _) = evalExpr (Val (VStr "Hello World!")) 2 funcMap2
    in
        assertEqual "Should return 30"                  (Num 30) ((funcMap3 M.! 0) 0 devices Nothing 25)   >>
        assertEqual "Should return 30"                  (Num 30) ((funcMap3 M.! 0) 2 devices Nothing 50)   >>
        assertEqual "Should return 45"                  (Num 45) ((funcMap3 M.! 1) 0 devices Nothing 670)  >>
        assertEqual "Should return \"Hello World!\""    (Str "Hello World!") ((funcMap3 M.! 2) 0 devices Nothing 3000)

testMembers = TestCase $
    let
        expr = Val . Member 
        device = Just ("Roomba", 50, TTrue)
    in
        assertEqual "Should return device name"                     (Num 30) (s 0 (expr Name) 0 devices device 25) >>
        assertEqual "Should return the device power"                (Num 30) (s 0 (expr Power) 0 devices device 25) >>
        assertEqual "Should return whether device is active or not" (Num 45) (s 0 (expr Active) 0 devices device 25)

testBinaryOperationPlus = TestCase $
    let
        valExpr = Val (VNum 30)
        expr = BinOp Plus valExpr valExpr
    in
        assertEqual "Should return 30" (Num 30) (s 1 expr 0 devices Nothing 25)  >>
        assertEqual "Should return 30" (Num 30) (s 2 expr 0 devices Nothing 25)  >>
        assertEqual "Should return 60" (Num 60) (s 0 expr 0 devices Nothing 25)

testBinaryOperationLogicalOr = TestCase $
    let
        n = Val . VNum
        expr1 = BinOp LessThan (Val VTime) (n 50) 

        lt = BinOp LessThan
        numVal = Val . VNum
        tt = lt (numVal 0) (numVal 5)
        ff = lt (numVal 5) (numVal 5)
        expr = BinOp LogicalOr
    in
        assertEqual "Check whether expression holds before 50" (Ver TTrue) (s 0 expr1 0 devices Nothing 49) >>
        assertEqual "Check whether expression holds at 50" (Ver FFalse) (s 0 expr1 0 devices Nothing 50) >>
        assertEqual "Check whether expression holds after 50" (Ver FFalse) (s 0 expr1 0 devices Nothing 51) >>
        assertEqual "Should be false" (Ver FFalse) (s 0 (expr ff ff) 0 devices Nothing  25) >>
        assertEqual "Should be true" (Ver TTrue) (s 0 (expr ff tt) 0 devices Nothing 25)

testBinaryOperations = TestCase $
    let
        num = Val . VNum
        bool = Val . VBool
        bo = BinOp

        -- Arithmetic
        minus  = bo Minus    (num 30) (num 15)
        mult   = bo Mult     (num 2)  (num 8)
        divide = bo Division (num 20) (num 4)
        modulo = bo Modulo   (num 10) (num 3)

        -- Logic / Comparison
        lAnd   = bo LogicalAnd    (bool True)  (bool False) 
        lEq    = bo LogicalEq     (num 5)  (num 5)
        lNEq   = bo LogicalNotEq  (num 5)  (num 10)
        ltOrEq = bo LessThanOrEq  (num 5)  (num 10)
        
        run expr = s 0 expr 0 devices Nothing 10
    in do
        assertEqual "Check minus"  (Num 15)     (run minus)
        assertEqual "Check mult"   (Num 16)     (run mult)
        assertEqual "Check div"    (Num 5)      (run divide)
        assertEqual "Check mod"    (Num 1)      (run modulo)
        assertEqual "Check And"    (Ver FFalse) (run lAnd)
        assertEqual "Check Eq"     (Ver TTrue)  (run lEq)
        assertEqual "Check NEq"    (Ver TTrue)  (run lNEq)
        assertEqual "Check LTe"    (Ver TTrue)  (run ltOrEq)

testStreamAmount = TestCase $
    let
        getKey = snd . initExprEval
        lt = BinOp LessThan
        numVal = Val . VNum
        tt = lt (numVal 0) (numVal 5)
        expr = BinOp LogicalOr tt tt
    in
        assertEqual "Expect 3" 3 (getKey tt) >>
        assertEqual "Expect 7" 7 (getKey expr)

testUnaryOperationLogicalNot = TestCase $
    let
        lt = BinOp LessThan
        numVal = Val . VNum
        tt = lt (numVal 0) (numVal 5)
        notExpr = UnOp LogicalNot
        expr1 = notExpr tt
        expr2 = notExpr expr1
    in
        assertEqual "Should be false" (Ver FFalse) (s 0 expr1 0 devices Nothing 25)  >>
        assertEqual "Should be true" (Ver TTrue) (s 0 expr2 0 devices Nothing 25)

testUnaryOperationNegate = TestCase $
    let
        numVal = Val . VNum
        expr = UnOp Negate . numVal
    in
        assertEqual "Should be false" (Num (-30)) (s 0 (expr 30) 0 devices Nothing 25)  >>
        assertEqual "Should be true" (Num 20) (s 0 (expr (-20)) 0 devices Nothing 25)

testAlwaysExpression = TestCase $
    let
        gUnbounded = MTLExpr Always None
        gBounded = MTLExpr Always (Range 0 30)

        lt = BinOp LessThan
        numVal = Val . VNum
        boolExpr = lt (Val VTime) (numVal 40)

        expr1 = gUnbounded boolExpr
        expr2 = gBounded boolExpr
    in
        assertEqual "Should be undecided"   (Ver Undecided) (s 0 expr1 0 devices Nothing 39)  >>
        assertEqual "Should be False"       (Ver FFalse)    (s 0 expr1 0 devices Nothing 45)  >>
        assertEqual "Should be False"       (Ver FFalse)    (s 0 expr1 40 devices Nothing 45) >>

        assertEqual "Should be true"        (Ver TTrue)     (s 0 expr2 0 devices Nothing 31)  >>
        assertEqual "Should be Undecided"   (Ver Undecided) (s 0 expr2 10 devices Nothing 39) >>
        assertEqual "Should be False"       (Ver FFalse)    (s 0 expr2 20 devices Nothing 49) >>
        assertEqual "Should be False"       (Ver FFalse)    (s 0 expr2 20 devices Nothing 60)

testEventuallyExpression = TestCase $
    let
        gUnbounded = MTLExpr Eventually None
        gBounded = MTLExpr Eventually (Range 0 30)

        lt = BinOp LessThan
        numVal = Val . VNum
        boolExpr = lt (numVal 39) (Val VTime)

        expr1 = gUnbounded boolExpr
        expr2 = gBounded boolExpr
    in
        assertEqual "Should be undecided" (Ver Undecided) (s 0 expr1 0 devices Nothing 39)  >>
        assertEqual "Should be True"      (Ver TTrue)     (s 0 expr1 0 devices Nothing 40)  >>
        assertEqual "Should be True"      (Ver TTrue)     (s 0 expr1 39 devices Nothing 40) >>

        assertEqual "Should be False"     (Ver FFalse)    (s 0 expr2 0 devices Nothing 30)  >>
        assertEqual "Should be Undecided" (Ver Undecided) (s 0 expr2 10 devices Nothing 39) >>
        assertEqual "Should be True"      (Ver TTrue)     (s 0 expr2 10 devices Nothing 40) >>
        assertEqual "Should be True"      (Ver TTrue)     (s 0 expr2 20 devices Nothing 60)

testSumExpression = TestCase $
    let sumExpr = Sum $ BinOp Plus (Val . VNum $ 5) (Val . Member $ Power)
        devices = S.fromList [[(undefined, 15, undefined), (undefined, 10, undefined), (undefined, 5, undefined)]]
    in assertEqual "Check Sum of Devices" (20 + 15 + 10) (s 0 sumExpr 0 devices Nothing 0) 

testForeachExpression = TestCase $
    let 
        foreachExpr = Foreach $ BinOp LessThanOrEq (Val . VNum $ 6) (Val . Member $ Power)
        devices =  S.fromList [[(undefined, 15, undefined), (undefined, 10, undefined), (undefined, 5, undefined)]]
        devices2 = S.fromList [[(undefined, 15, undefined), (undefined, 10, undefined), (undefined, 6, undefined)]]
    in 
        assertEqual "Check foreach device when one is false" (Ver FFalse) (s 0 foreachExpr 0 devices Nothing 0) >>
        assertEqual "Check foreach device when all are true" (Ver TTrue) (s 0 foreachExpr 0 devices2 Nothing 0) 

testSumtimeExpression = TestCase $
    let
        sumtimeExpr = Sumtime (Val . Member $ Power)
        devices = S.fromList [[(undefined, 15, undefined), (undefined, 10, undefined), (undefined, x, undefined)] | x <- [0..30]]
    in 
        assertEqual "Check Sumtime from t = 0..30" (Num (25*31 + (31 * (0+30)) `div` 2)) (s 0 sumtimeExpr 0 devices Nothing 30) >>
        assertEqual "Check Sumtime from t = 15..30" (Num (25*16 + (16 * (15+30)) `div` 2)) (s 0 sumtimeExpr 15 devices Nothing 30)

expressionTests :: Test
expressionTests = TestList
    [
        TestLabel "Time Constant Test" testTimeVal,
        TestLabel "Constant Val Test" testConstantVal,
        TestLabel "Binary Operation Plus Test" testBinaryOperationPlus,
        TestLabel "Binary Operation Logical Or Test" testBinaryOperationLogicalOr,
        TestLabel "Check Remaining Binary Operations" testBinaryOperationLogicalOr,
        TestLabel "Check whether correct stream amount is created" testStreamAmount,
        TestLabel "Unary Operation Logical not Test" testUnaryOperationLogicalNot,
        TestLabel "Unary Operation Negate Test" testUnaryOperationNegate,
        TestLabel "Always Expression Test" testAlwaysExpression,
        TestLabel "Eventually Expression Test" testEventuallyExpression,
        TestLabel "Sum Expression Test" testSumExpression,
        TestLabel "Foreach Expression Test" testForeachExpression,
        TestLabel "Sumtime expression Test" testSumtimeExpression
    ]