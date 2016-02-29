module Unwind.Asm ( decompile
                  ) where

import Data.Array.IArray
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Text as T
import Data.Word (Word64)
import Hdis86 hiding (Instruction, disassemble)
import qualified Hdis86 as X86

import Unwind.PE
import Unwind.Types

decompile :: PEFile -> Gr Instruction ()
decompile = runUnwind analyze . buildControlFlowGraph . wrapInstructions . disassemble

disassemble :: PEFile -> [X86.Metadata]
disassemble pe = disassembleMetadata format $ textData pe
    where format = intel32 { cfgSyntax = SyntaxIntel
                           , cfgOrigin = fromIntegral $ imageBase pe + textOffset pe
                           }

wrapInstructions :: [X86.Metadata] -> Array Int Instruction
wrapInstructions metadata = listArray (0, length metadata - 1) $ fmap toInstruction metadata
    where toInstruction inst = Instruction { offset = mdOffset inst
                                           , hex = T.pack . mdHex $ inst
                                           , assembly = T.pack . mdAssembly $ inst
                                           , instr = mdInst inst
                                           }

buildControlFlowGraph :: Array Int Instruction -> Gr Instruction ()
buildControlFlowGraph instrs = mkGraph (assocs instrs) []

analyze = return ()
