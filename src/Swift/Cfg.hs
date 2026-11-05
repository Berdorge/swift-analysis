{-# LANGUAGE LambdaCase #-}

module Swift.Cfg
    ( buildProgramCfg
    , cfgEntryId
    , cfgNodes
    , cfgEdges
    , nodeSrc
    , nodeDst
    , findReachable
    , findPredecessors
    , findSuccessors
    , ProgramCfg (..)
    , FunctionCfg
    , FunctionId
    , FunctionNode (..)
    , FunctionCallEdge (..)
    , Node (..)
    , NodeId
    , NodeData (..)
    , Edge (..)
    , EdgeLabel (..)
    ) where

import Control.Arrow ((>>>))
import Control.Monad.State (State, forM, forM_, modify, runState, state, void)
import Data.Set (Set)
import qualified Data.Set as Set
import Swift.Syntax (Arg (Arg), Block, CondBlock (..),
                     Expr (Binary, BoolLit, Call, IntLit, Unary, Var), File (File),
                     Function (Function), Id, Param (Param), Stmt (..))

buildProgramCfg :: File -> ProgramCfg
buildProgramCfg (File functions) =
    let functionNodes = zipWith addFunction [0..] functions
        functionCalls = foldMap addFunctionCalls functionNodes
        callEdges = foldMap (resolveFunctionCallEdges functionNodes) functionCalls
    in ProgramCfg functionNodes (Set.toList callEdges)

addFunction :: FunctionId -> Function -> FunctionNode
addFunction fId f = FunctionNode fId f (buildFunctionCfg f)

addFunctionCalls :: FunctionNode -> [FunctionCall]
addFunctionCalls (FunctionNode fId _ cfg) = flip foldMap (cfgNodes cfg) $ \(Node nodeId nodeData) ->
    let calls = nodeCalls nodeData in
        map (FunctionCall fId nodeId) (Set.toList calls)

resolveFunctionCallEdges :: [FunctionNode] -> FunctionCall -> Set FunctionCallEdge
resolveFunctionCallEdges fNodes (FunctionCall callerId nodeId callee) =
    Set.fromList $ flip map (resolvePossibleTargets callee fNodes) $
        \(FunctionNode calleeId _ _) -> FunctionCallEdge callerId nodeId calleeId

resolvePossibleTargets :: Callee -> [FunctionNode] -> [FunctionNode]
resolvePossibleTargets callee = filter (isPossibleTarget callee)

isPossibleTarget :: Callee -> FunctionNode -> Bool
isPossibleTarget (Callee callName args) (FunctionNode _ (Function fName params _ _) _) =
    callName == fName && argsFitParams args params

argsFitParams :: [Maybe Id] -> [Param] -> Bool
argsFitParams argLabels = (== argLabels) . map (\(Param pLabel _ _) -> pLabel)

nodeCalls :: NodeData -> Set Callee
nodeCalls = \case
    CfgStmt (VarDecl _ _ expr)            -> getCallees expr
    CfgStmt (LetDecl _ _ expr)            -> getCallees expr
    CfgStmt (Assign _ expr)               -> getCallees expr
    CfgStmt (CompoundAssign _ _ expr) -> getCallees expr
    CfgStmt (ExprStmt expr)               -> getCallees expr
    CfgStmt (Return (Just expr))          -> getCallees expr
    CfgCondition expr                     -> getCallees expr
    _                                     -> mempty

getCallees :: Expr -> Set Callee
getCallees = \case
    IntLit _ -> mempty
    BoolLit _ -> mempty
    Var _ -> mempty
    Unary _ expr -> getCallees expr
    Binary _ lhs rhs -> getCallees lhs <> getCallees rhs
    Call name args -> let nestedCalls = foldMap (\(Arg _ expr) -> getCallees expr) args in
        Set.insert (Callee name $ map (\(Arg label _) -> label) args) nestedCalls

data ProgramCfg = ProgramCfg [FunctionNode] [FunctionCallEdge]

type FunctionId = Int

data FunctionCallEdge = FunctionCallEdge FunctionId NodeId FunctionId
    deriving (Eq, Ord, Show)

data FunctionNode = FunctionNode FunctionId Function FunctionCfg

data FunctionCall = FunctionCall FunctionId NodeId Callee

data Callee = Callee Id [Maybe Id]
    deriving (Show, Ord, Eq)

buildFunctionCfg :: Function -> FunctionCfg
buildFunctionCfg function =
    let (entryId, finalState) = runState (buildFunction function) (FunctionCfgState 0 [] [] [] [] [])
    in FunctionCfg entryId (nodes finalState) (edges finalState)

data FunctionCfg = FunctionCfg { cfgEntryId :: NodeId, cfgNodes :: [Node], cfgEdges :: [Edge] }
    deriving (Show, Eq)

data Node = Node NodeId NodeData
    deriving (Show, Eq)

type NodeId = Int

data NodeData = CfgEntryParams [Id]
    | CfgStmt Stmt
    | CfgCondition Expr
    deriving (Show, Eq)

data Edge = Edge EdgeLabel NodeId NodeId
    deriving (Show, Eq)

nodeSrc :: Edge -> NodeId
nodeSrc (Edge _ from _) = from

nodeDst :: Edge -> NodeId
nodeDst (Edge _ _ to) = to

findReachable :: FunctionCfg -> Set NodeId
findReachable cfg =
    let recordReachable nodeId visited =
            if Set.member nodeId visited
                then visited
                else foldr recordReachable (Set.insert nodeId visited) (findSuccessors cfg nodeId)
    in recordReachable (cfgEntryId cfg) Set.empty

findPredecessors :: FunctionCfg -> NodeId -> [NodeId]
findPredecessors cfg nodeId = map nodeSrc $ filter (nodeDst >>> (== nodeId)) (cfgEdges cfg)

findSuccessors :: FunctionCfg -> NodeId -> [NodeId]
findSuccessors cfg nodeId = map nodeDst $ filter (nodeSrc >>> (== nodeId)) (cfgEdges cfg)

data EdgeLabel = Unconditional | IfTrue | IfFalse
    deriving (Show, Eq)

buildFunction :: Function -> State FunctionCfgState NodeId
buildFunction (Function _ params _ statements) = do
    nodeId <- appendNode (CfgEntryParams $ map (\(Param _ paramId _) -> paramId) params)
    addPendingConnection nodeId
    buildBlock statements
    return nodeId

buildBlock :: Block -> State FunctionCfgState ()
buildBlock = mapM_ buildStmt

buildStmt :: Stmt -> State FunctionCfgState ()
buildStmt = \case
    If firstCond elseIfs elseBranch -> do
        nestedPending <- forM (firstCond : elseIfs) $ \(CondBlock condition branch) -> do
            nodeId <- appendNode (CfgCondition condition)
            addLabeledPendingConnection nodeId IfTrue
            buildBlock branch
            endBlockPending <- popAllPendingConnections
            addLabeledPendingConnection nodeId IfFalse
            return endBlockPending
        forM_ elseBranch buildBlock
        modify $ \cfgState -> cfgState { pendingConnections = concat nestedPending ++ pendingConnections cfgState }

    While (CondBlock condition nested) -> do
        nodeId <- appendNode (CfgCondition condition)
        parentBreaks <- popAllPendingBreaks
        parentContinues <- popAllPendingContinues
        addLabeledPendingConnection nodeId IfTrue

        buildBlock nested

        consumeAndSetPendingContinues parentContinues
        consumePendingConnections nodeId

        consumeAndSetPendingBreaks parentBreaks
        addLabeledPendingConnection nodeId IfFalse

    RepeatWhile (CondBlock condition nested) -> do
        condId <- nextNodeId
        parentBreaks <- popAllPendingBreaks
        parentContinues <- popAllPendingContinues

        addLabeledPendingConnection condId IfTrue
        buildBlock nested

        consumeAndSetPendingContinues parentContinues
        addNode condId (CfgCondition condition)

        consumeAndSetPendingBreaks parentBreaks
        addLabeledPendingConnection condId IfFalse

    statement@(Return _) ->
        void $ appendNode (CfgStmt statement)

    Break -> do
        pending <- popAllPendingConnections
        addPendingBreaks pending

    Continue -> do
        pending <- popAllPendingConnections
        addPendingContinues pending

    statement -> do
        nodeId <- appendNode (CfgStmt statement)
        addPendingConnection nodeId

appendNode :: NodeData -> State FunctionCfgState NodeId
appendNode nodeData = do
    nodeId <- nextNodeId
    addNode nodeId nodeData
    return nodeId

nextNodeId :: State FunctionCfgState NodeId
nextNodeId = state $ \cfgState ->
    (nodeIdCounter cfgState, cfgState { nodeIdCounter = nodeIdCounter cfgState + 1 })

addNode :: NodeId -> NodeData -> State FunctionCfgState ()
addNode nodeId nodeData = do
    modify $ \cfgState -> cfgState { nodes = Node nodeId nodeData : nodes cfgState }
    consumePendingConnections nodeId

addPendingConnection :: NodeId -> State FunctionCfgState ()
addPendingConnection nodeId = addLabeledPendingConnection nodeId Unconditional

addLabeledPendingConnection :: NodeId -> EdgeLabel -> State FunctionCfgState ()
addLabeledPendingConnection nodeId label = modify $ \cfgState ->
    cfgState { pendingConnections = (nodeId, label) : pendingConnections cfgState }

popAllPendingConnections :: State FunctionCfgState [(NodeId, EdgeLabel)]
popAllPendingConnections = state $ \cfgState ->
    (pendingConnections cfgState, cfgState { pendingConnections = [] })

addPendingBreaks :: [(NodeId, EdgeLabel)] -> State FunctionCfgState ()
addPendingBreaks pending = modify $ \cfgState ->
    cfgState { pendingBreaks = pending ++ pendingBreaks cfgState }

addPendingContinues :: [(NodeId, EdgeLabel)] -> State FunctionCfgState ()
addPendingContinues pending = modify $ \cfgState ->
    cfgState { pendingContinues = pending ++ pendingContinues cfgState }

popAllPendingBreaks :: State FunctionCfgState [(NodeId, EdgeLabel)]
popAllPendingBreaks = state $ \cfgState ->
    (pendingBreaks cfgState, cfgState { pendingBreaks = [] })

popAllPendingContinues :: State FunctionCfgState [(NodeId, EdgeLabel)]
popAllPendingContinues = state $ \cfgState ->
    (pendingContinues cfgState, cfgState { pendingContinues = [] })

consumePendingConnections :: NodeId -> State FunctionCfgState ()
consumePendingConnections nodeId = do
    pending <- popAllPendingConnections
    let newEdges = map (\(sourceId, label) -> Edge label sourceId nodeId) pending
    modify $ \cfgState -> cfgState { edges = newEdges ++ edges cfgState }

consumeAndSetPendingBreaks :: [(NodeId, EdgeLabel)] -> State FunctionCfgState ()
consumeAndSetPendingBreaks parentBreaks = modify $ \cfgState -> cfgState
    { pendingBreaks = parentBreaks
    , pendingConnections = pendingBreaks cfgState ++ pendingConnections cfgState
    }

consumeAndSetPendingContinues :: [(NodeId, EdgeLabel)] -> State FunctionCfgState ()
consumeAndSetPendingContinues parentContinues = modify $ \cfgState -> cfgState
    { pendingContinues = parentContinues
    , pendingConnections = pendingContinues cfgState ++ pendingConnections cfgState
    }

data FunctionCfgState = FunctionCfgState
    { nodeIdCounter      :: NodeId
    , nodes              :: [Node]
    , edges              :: [Edge]
    , pendingConnections :: [(NodeId, EdgeLabel)]
    , pendingBreaks      :: [(NodeId, EdgeLabel)]
    , pendingContinues   :: [(NodeId, EdgeLabel)]
    }
