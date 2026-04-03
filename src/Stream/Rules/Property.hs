{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Stream.Rules.Property where

import qualified Data.IntMap as M

import Program.CST
import Stream.Types
import Stream.Rules.Expression (evalExpr)

--todo: temp
initEnv :: Env
initEnv = (M.empty, M.empty, M.empty) :: Env

--todo: Set up Property Rules
evalProp :: Property -> Int -> Env -> (Env, Int)

evalProp (Comp p1 p2) k env = evalProp p2 k' env'
    where
        (env', k') = evalProp p1 k env

evalProp (Prop Always bounds e) k (so, sdi, sd) = undefined
    where
        (sd', k') = evalExpr e (k + 2) sd

        --todo: add logic - This one's tough

