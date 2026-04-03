{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--todo: Set up expression rules
module Stream.Rules.Expression where

import qualified Data.IntMap as M

import Program.CST 
import Stream.Types

type StreamDMap = M.IntMap StreamD


evalExpr :: Expr -> Int -> StreamDMap -> (StreamDMap, Int)

-- Current Time
evalExpr (Val VTime) key streamDEnv = (M.insert key (\_ _ t' -> Num t') streamDEnv, key + 1)

-- Constant
evalExpr (Val (VConst n)) key streamDEnv = 
    let f = case n of 
            VNum v -> (\_ _ _ -> Num v)
            VStr v -> (\_ _ _ -> Str v)
    in (M.insert key f streamDEnv, key + 1)
 
--exprRules (Value n) map key = 