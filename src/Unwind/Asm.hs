module Unwind.Asm  where

import Data.Graph.Inductive.Graph
import qualified Data.Text as T
import Hdis86 hiding (Instruction, disassemble)
import qualified Hdis86 as X86

import Unwind.Analysis
import Unwind.PE
import Unwind.Types

decompile :: PEFile -> InstructionGraph
decompile = runUnwind analyze . buildControlFlowGraph . wrapInstructions . disassemble

disassemble :: PEFile -> [X86.Metadata]
disassemble pe = disassembleMetadata format $ textData pe
    where format = intel32 { cfgSyntax = SyntaxIntel
                           , cfgOrigin = fromIntegral $ imageBase pe + textOffset pe
                           }

wrapInstructions :: [X86.Metadata] -> [Instruction]
wrapInstructions metadata = fmap toInstruction metadata
    where toInstruction inst = Instruction { offset = mdOffset inst
                                           , hex = T.pack . mdHex $ inst
                                           , assembly = T.pack . mdAssembly $ inst
                                           , instr = mdInst inst
                                           }

buildControlFlowGraph :: [Instruction] -> InstructionGraph
buildControlFlowGraph instrs = newGraph (zip ((fromIntegral . offset) <$> instrs) instrs) []
