{-# LANGUAGE TupleSections #-}
module Unwind.Analysis ( analyze
                       ) where

import Control.Monad.State.Strict (get, modify)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS (xdfsWith)
import Data.List (nub)
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
        offsetsAndTargets = concat $ zipWith3 (\a b vs -> (a, b, ) <$> vs) offsets (tail offsets) targets
    modify $ insEdges [(node, next, ()) | (node, next, NextTarget) <- offsetsAndTargets]
    modify $ insEdges [(node, next + fromIntegral o, ()) | (node, next, RelativeTarget o) <- offsetsAndTargets]
    gr <- get
    let registerTargets = concat [(node, ) <$> getPossibleRegisterValues gr node r | (node, _, RegisterTarget r) <- offsetsAndTargets]
    modify $ insEdges [(node, fromIntegral next, ()) | (node, Constant next) <- registerTargets]
    modify $ insNodes $ map (\n -> (fromIntegral n, fakeInstruction n "ext")) . nub $ [node | (_, Relative node) <- registerTargets]
    modify $ insEdges [(node, fromIntegral next, ()) | (node, Relative next) <- registerTargets]
    -- memory targets require more analysis

data PossibleValue = Constant Word64
                   | Relative Word64
                   deriving (Show)

getPossibleRegisterValues :: DynGraph g => g Instruction () -> Int -> Register -> [PossibleValue]
getPossibleRegisterValues gr node reg = concat $ xdfsWith traceback getAssignments [node] gr
    where traceback (ein, n, i, _) = if null $ getRegisterAssignments gr n i reg then map snd ein else []
          getAssignments (_, n, i, _) = getRegisterAssignments gr n i reg

getRegisterAssignments :: DynGraph g => g Instruction () -> Int -> Instruction -> Register -> [PossibleValue]
getRegisterAssignments gr node (Instruction _ _ _ instr) reg = go instr
    where
        go (Inst _ Imov [Mem _, _]) = []
        go (Inst _ Imov [Reg r, _]) | r /= reg = []
        go (Inst _ Imov [Reg r, Mem (Memory Bits32 RegNone RegNone 0 (Immediate Bits32 m))]) = [Relative $ fromIntegral m]
        go (Inst _ Imov [Reg r, Mem (Memory Bits32 b RegNone 0 (Immediate Bits0 0))]) =
            if null [v | Relative v <- baseAssignments]
            then [Relative v | Constant v <- baseAssignments]
            else (error "Cannot check 2nd relative assignment")
            where baseAssignments = getPossibleRegisterValues gr node b
        go (Inst _ Imov [Reg r, Imm (Immediate Bits32 m)]) = [Constant $ fromIntegral m]
        go (Inst _ Imov o) = error $ show o
        -- TODO: prohibit modifications of reg
        go _ = []

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
