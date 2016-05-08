import Data.Graph.Inductive.Graph
import Text.Printf (printf)
import qualified System.Environment as System

import Unwind.Analysis
import Unwind.Asm
import Unwind.PE
import Unwind.Types

import Data.Graph.Inductive.Dot
import Text.Printf (printf)

main :: IO ()
main = do
    [filepath] <- System.getArgs
    pe <- loadPEFile filepath
    let gr = decompile pe
    putStrLn . showGraph $ gr
    let functions = findFunctions $ gr
    let formatName = printf "sub_%08x" . start
    writeFile "functions.dot" . showDot $ fglToDotGeneric functions formatName (const "") id
    mapM_ (putStrLn . (\f -> (formatName f) ++ ":\n" ++ (show $ body f) ++ "\n")) . map snd . labNodes $ functions
