module Stream.CSTConversion where

import qualified Data.IntMap as M
import Stream.Rules.Property ( evalProp )

import Program.CST ( Program )  
import Stream.Types ( StreamD, Env )

type StreamDMap = M.IntMap StreamD

initEnv :: Env
initEnv = (M.empty, M.empty) :: Env

convertCSTToStreams :: Program -> Env
convertCSTToStreams prog = fst $ evalProp prog 0 initEnv 

