module Stream.Rules.Expression where

import qualified Data.IntMap as M

import Program.CST
import Stream.Types
import Stream.Verdict
import Data.Function (on)
import qualified Data.Sequence as S

type StreamDMap = M.IntMap StreamD

evalExpr :: Expr -> Int -> StreamDMap -> (StreamDMap, Int)

-- Base Cases: Constant, Member & Time
evalExpr (Val vType) key streamDEnv = (M.insert key f streamDEnv, key + 1)
    where
        f = valType vType

        valType (VNum v)        = \_ _ _ _  -> Num v
        valType (VStr v)        = \_ _ _ _  -> Str v
        valType (VBool v)       = \_ _ _ _  -> Ver . asVerdict $ v
        valType VTime           = \_ _ _ t' -> Num t'
        valType (Member field) = \_ _ m _ -> case m of
            Nothing -> error $ "A device was not supplied to allow for " ++ show field
            Just (n, p, a) -> case field of
                Name   -> Str n
                Power  -> Num p
                Active -> Ver a

-- Binary Operation
evalExpr (BinOp binOp e1 e2) k streamDEnv = (M.insert k f env2, k'')
    where
        (env1, k') = evalExpr e1 (k+1) streamDEnv
        (env2, k'') = evalExpr e2 k' env1

        s1 = env2 M.! (k+1)
        s2 = env2 M.! k'

        f t ds d t' = streamOp binOp (s1 t ds d t') (s2 t ds d t')

        unpackAndOperate binOperator wrapper unwrap = (wrapper .) . binOperator `on` unwrap

        streamOp Plus = (+)
        streamOp Minus = (-)
        streamOp Mult = (*)
        streamOp Division = unpackAndOperate div Num toNumCoercion
        streamOp Modulo = unpackAndOperate mod Num toNumCoercion

        streamOp LogicalEq = ((Ver . asVerdict) .) . (==)
        streamOp LogicalNotEq = ((Ver . asVerdict) .) . (/=)
        streamOp LogicalOr = unpackAndOperate (|||) Ver toVerdictCoercion
        streamOp LogicalAnd = unpackAndOperate (&&&) Ver toVerdictCoercion
        streamOp LessThan = unpackAndOperate (<) (Ver . asVerdict) toNumCoercion
        streamOp LessThanOrEq = unpackAndOperate (<=) (Ver . asVerdict) toNumCoercion

-- Unary
evalExpr (UnOp op e) k env = (M.insert k f env', k')
    where
        (env', k') = evalExpr e (k+1) env

        s1 = env' M.! (k+1)

        f = (((streamOp op .) .) .) . s1

        streamOp Negate = negate
        streamOp LogicalNot = Ver . nnot . getVerUnsafe


-- Expr Always & Eventually
evalExpr (MTLExpr mtl bounds e) k env = (M.insert k f env', k')
    where
        (env', k') = evalExpr e (k+1) env

        verdictLogic Always = vConjunction
        verdictLogic Eventually = vDisjunction

        isFinalVerdict Always p = if p then Undecided else TTrue
        isFinalVerdict Eventually p = if p then Undecided else FFalse

        f t ds d t' = Ver giveVerdict
            where
                s1 = env' M.! (k+1)

                (a, b) = case bounds of
                    None -> (0, t'+1)
                    Range r1 r2 -> (r1, r2)

                lower = a + t
                upper = min t' (t + b)

                finalVer = isFinalVerdict mtl (t' < t + b)
                giveVerdict = verdictLogic mtl (finalVer : [toVerdictCoercion (s1 i ds d t') | i <- [lower..upper]])

-- Sum
evalExpr (Sum e) k env = (M.insert k f env', k')
    where
        (env', k') = evalExpr e (k+1) env

        s1 = env' M.! (k+1)

        f t ds _ t' = Num . sum $ [toNumCoercion $ s1 t ds (Just d') t' | d' <- S.index ds t'] 

-- Foreach
evalExpr (Foreach e) k env = (M.insert k f env', k')
    where
        (env', k') = evalExpr e (k+1) env

        s1 = env' M.! (k+1)

        f t ds _ t' = Ver . vConjunction $ [toVerdictCoercion $ s1 t ds (Just d') t' | d' <- S.index ds t']

evalExpr (Sumtime e) k env = (M.insert k f env', k')
    where
        (env', k') = evalExpr (Sum e) (k+1) env

        s1 = env' M.! (k+1)

        f t ds d t' = sum [s1 t ds d i | i <- [t..t']]