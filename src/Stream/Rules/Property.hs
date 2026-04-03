module Stream.Rules.Property where

import qualified Data.IntMap as M

import Program.CST as C 
import Stream.Types as T

--todo: temp
initEnv :: Env
initEnv = (M.empty, M.empty, M.empty) :: Env

--todo: Set up Property Rules
evalProp :: Property -> (Env, Int)
evalProp _ = (initEnv, 0)


