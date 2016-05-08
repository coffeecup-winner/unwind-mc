import Data.Graph.Inductive.Graph
import Text.Printf (printf)
import qualified System.Environment as System

import Unwind.Analysis
import Unwind.Asm
import Unwind.PE
import Unwind.Types

main :: IO ()
main = do
    [filepath] <- System.getArgs
    pe <- loadPEFile filepath
    let gr = decompile pe
    putStrLn . showGraph $ gr
    print . findFunctions $ gr
