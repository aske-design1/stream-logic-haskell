module Monitor where

import Stream.Types ( StreamD, getVerSafe )
import Stream.Verdict ( Verdict (Undecided, TTrue), vConjunction, )
import Data.Maybe


-- Utility to run the monitor over a range [0..n]
monitor :: Int -> (Int -> Bool) -> StreamD -> [([Int], Verdict)]
monitor maxT bounds entryStream = scanl (\(tasks, _) t -> stepMonitor tasks t) ([], TTrue) [0..maxT]
    where
        stepMonitor :: [Int] -> Int -> ([Int], Verdict)
        stepMonitor activeTasks t =
            let
                -- 1. Insertion: Insert into S_DI stream
                tasks = if bounds t then t : activeTasks else activeTasks

                -- 2. EVALUATION: Calculates the values for the output streams
                results = [(t', entryStream t' Nothing t) | t' <- tasks]
                -- 3. Calculate Verdict: Finds the Verdict for the Streams at time t
                verdict = vConjunction .  mapMaybe (getVerSafe . snd) $ results

                -- 3. REMOVAL: Clean-up
                remainingTasks = [t' | (t', v) <- results, getVerSafe v == Just Undecided || getVerSafe v == Just TTrue]

            in
                (remainingTasks, verdict)

