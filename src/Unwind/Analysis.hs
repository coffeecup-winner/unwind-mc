module Unwind.Analysis ( analyze
                       ) where

import Control.Monad.State.Strict
import Data.Graph.Inductive.Graph
import Data.Word (Word64)
import Hdis86 hiding (Instruction)
import qualified Hdis86 as X86

import Unwind.Types

analyze :: Unwind ()
analyze = do
    gr <- get
    let nodes = labNodes gr
        targets = (getControlFlowTargets . snd) <$> nodes
        offsets = fst <$> nodes
        offsetsAndTargets = flatten $ zip3 offsets (tail offsets) targets
    modify $ insEdges [(node, next, ()) | (node, next, NextTarget) <- offsetsAndTargets]
    modify $ insEdges [(node, next + fromIntegral o, ()) | (node, next, RelativeTarget o) <- offsetsAndTargets]
    -- memory and register targets require more analysis

flatten :: [(a, b, [c])] -> [(a, b, c)]
flatten [] = []
flatten ((a, b, []):xs) = flatten xs
flatten ((a, b, c:cs):xs) = (a, b, c) : flatten ((a, b, cs):xs)

data ControlFlowTarget = NextTarget
                       | RelativeTarget Word64
                       | MemoryTarget Word64
                       | RegisterTarget Register
                       deriving (Show)

getControlFlowTargets :: Instruction -> [ControlFlowTarget]
getControlFlowTargets (Instruction _ _ _ instr) = go instr
    where
        go (Inst _ Ija    o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijae   o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijb    o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijbe   o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijg    o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijge   o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijl    o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijle   o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijp    o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijnp   o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijs    o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijns   o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijo    o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijno   o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijz    o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijnz   o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijcxz  o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijecxz o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijrcxz o) = [NextTarget, getJumpTarget o]
        go (Inst _ Ijmp   o) = [getJumpTarget o]
        go (Inst _ Icall  o) = [NextTarget, getJumpTarget o]
        go (Inst _ Iret   _) = []
        go _                 = [NextTarget]

getJumpTarget :: [Operand] -> ControlFlowTarget
getJumpTarget = go
    where
        go [Jump (Immediate _ value)] = RelativeTarget $ fromIntegral value
        go [Mem m] = MemoryTarget $ error (show m)
        go [Reg r] = RegisterTarget r
        go o = error $ show o
