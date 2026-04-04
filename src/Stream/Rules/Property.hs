module Stream.Rules.Property where

import qualified Data.IntMap as M

import Program.CST
    ( Property(..),
      MTLBound(Range, None),
      MTLElement(Eventually, Always) )
import Stream.Types
    ( Env,
      getVerUnsafe,
      StreamOutState(State, cleanUp, verdicts, insert, update,
                     getColVerdict), SDIState, Devices )
import Stream.Rules.Expression (evalExpr)
import Stream.Verdict (Verdict(..), (&&&), (|||))

evalProp :: Property -> Int -> Env -> (Env, Int)

evalProp (Comp p1 p2) k env = evalProp p2  k' env'
    where
        (env', k') = evalProp p1 k env

evalProp (Prop mtl bounds e) k (so, sd) = ((M.insert k streamO so, sd'), k')
    where
        (sd', k') = evalExpr e (k + 1) sd

        insertFunc None stream t   = M.insert t Undecided stream
        insertFunc (Range r1 r2) stream t = if r1 <= t && t <= r2 then M.insert t Undecided stream else stream

        updateFunc s so' ds t = M.mapWithKey (\i _ -> getVerUnsafe $ s i ds Nothing t) so'
        
        getColVerdictFunc Always = M.foldl (&&&) TTrue
        getColVerdictFunc Eventually = M.foldl (|||) FFalse

        streamO = State {
            verdicts = M.empty,
            insert = insertFunc bounds,
            update = updateFunc (sd' M.! (k+1)) ,
            getColVerdict = getColVerdictFunc mtl,
            cleanUp = M.filter (/= Undecided)
        }