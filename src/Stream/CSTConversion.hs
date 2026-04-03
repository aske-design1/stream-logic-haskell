module Stream.CSTConversion where

import qualified Data.IntMap as M

import Program.CST as C 
import Stream.Types as T

type StreamDMap = M.IntMap StreamD

initEnv :: Env
initEnv = (M.empty, M.empty, M.empty) :: Env

-- todo: Set up func to start conversion
convertCSTToStreams :: Program -> Env
convertCSTToStreams _ = initEnv

