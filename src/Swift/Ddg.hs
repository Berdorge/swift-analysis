{-# LANGUAGE LambdaCase #-}

module Swift.Ddg
    ( buildProgramDdg
    , ProgramDdg (..)
    , FunctionDdg (..)
    , FunctionNode (..)
    , Edge (..)
    ) where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Swift.Cfg as Cfg
import Swift.Cfg (FunctionCallEdge, FunctionCfg, Node (..), NodeData (..), NodeId, ProgramCfg (..),
                  cfgNodes, findPredecessors)
import Swift.Syntax (Arg (Arg), Expr (..), Id, Stmt (..))

buildProgramDdg :: ProgramCfg -> ProgramDdg
buildProgramDdg (ProgramCfg functions callEdges) =
    let cfgToDdgNode fNode@(Cfg.FunctionNode _ _ cfg) = FunctionNode fNode (buildDdg cfg)
    in ProgramDdg (map cfgToDdgNode functions) callEdges

data ProgramDdg = ProgramDdg [FunctionNode] [FunctionCallEdge]

data FunctionNode = FunctionNode Cfg.FunctionNode FunctionDdg

buildDdg :: FunctionCfg -> FunctionDdg
buildDdg cfg =
    let reachingDefs = fixpoint (propagateReachingDefs cfg) Map.empty
        ddgEdges = getDdgEdges cfg reachingDefs
    in FunctionDdg (cfgNodes cfg) ddgEdges

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint step current =
    let next = step current in
    if current == next
        then current
        else fixpoint step next

propagateReachingDefs :: FunctionCfg -> ReachingDefs -> ReachingDefs
propagateReachingDefs cfg reachingDefs =
    foldr (propagateNodeReachingDefs cfg) reachingDefs (cfgNodes cfg)

propagateNodeReachingDefs :: FunctionCfg -> Node -> ReachingDefs -> ReachingDefs
propagateNodeReachingDefs cfg (Node nodeId nodeData) reachingDefs =
    let defsBefore = getDefsBefore cfg nodeId reachingDefs
        markNodeAssigns varId = Map.insert varId (Set.singleton nodeId)
        defsAfter = foldr markNodeAssigns defsBefore (nodeDefines nodeData)
    in Map.insert nodeId defsAfter reachingDefs

nodeDefines :: NodeData -> [Id]
nodeDefines = \case
    (CfgEntryParams params)              -> params
    (CfgStmt (VarDecl varId _ _))        -> [varId]
    (CfgStmt (LetDecl varId _ _))        -> [varId]
    (CfgStmt (Assign varId _))           -> [varId]
    (CfgStmt (CompoundAssign _ varId _)) -> [varId]
    _                                    -> []

getDdgEdges :: FunctionCfg -> ReachingDefs -> [Edge]
getDdgEdges cfg reachingDefs =
    concatMap (\node -> getDdgIncomingEdges cfg node reachingDefs) (cfgNodes cfg)

getDdgIncomingEdges :: FunctionCfg -> Node -> ReachingDefs -> [Edge]
getDdgIncomingEdges cfg (Node nodeId nodeData) reachingDefs =
    let defsBefore = getDefsBefore cfg nodeId reachingDefs
        findReachingDefs usedId = Map.findWithDefault Set.empty usedId defsBefore
        deps = foldMap findReachingDefs (nodeUses nodeData)
    in map (`Edge` nodeId) $ Set.toList deps

nodeUses :: NodeData -> Set Id
nodeUses = \case
    CfgStmt (VarDecl _ _ expr)            -> getUsedIds expr
    CfgStmt (LetDecl _ _ expr)            -> getUsedIds expr
    CfgStmt (Assign _ expr)               -> getUsedIds expr
    CfgStmt (CompoundAssign _ varId expr) -> Set.insert varId $ getUsedIds expr
    CfgStmt (ExprStmt expr)               -> getUsedIds expr
    CfgStmt (Return (Just expr))          -> getUsedIds expr
    CfgCondition expr                     -> getUsedIds expr
    _                                     -> Set.empty

getDefsBefore :: FunctionCfg -> NodeId -> ReachingDefs -> NodeReachingDefs
getDefsBefore cfg nodeId reachingDefs =
    let predecessors = findPredecessors cfg nodeId in
        Map.unionsWith (<>) (mapMaybe (`Map.lookup` reachingDefs) predecessors)

getUsedIds :: Expr -> Set Id
getUsedIds = \case
    IntLit _ -> Set.empty
    BoolLit _ -> Set.empty
    Var varId -> Set.singleton varId
    Unary _ expr -> getUsedIds expr
    Binary _ lhs rhs -> getUsedIds lhs <> getUsedIds rhs
    Call _ args -> foldMap (\(Arg _ expr) -> getUsedIds expr) args

data FunctionDdg = FunctionDdg [Node] [Edge]
    deriving (Show, Eq)

data Edge = Edge NodeId NodeId
    deriving (Show, Eq)

type ReachingDefs = Map.Map NodeId NodeReachingDefs

type NodeReachingDefs = Map.Map Id (Set NodeId)
