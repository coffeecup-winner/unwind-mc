import Data.Graph.Inductive.Graph
import Text.Printf (printf)
import qualified System.Environment as System

import Unwind.Asm
import Unwind.PE

main :: IO ()
main = do
    [filepath] <- System.getArgs
    pe <- loadPEFile filepath
    putStrLn . prettify . nmap show . decompile $ pe
