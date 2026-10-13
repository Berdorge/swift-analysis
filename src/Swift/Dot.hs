{-# LANGUAGE LambdaCase #-}

module Swift.Dot
    ( cfgToDot
    , ddgToDot
    ) where

import Data.List (find, intercalate)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Swift.Cfg as Cfg
import Swift.Cfg (EdgeLabel (..), FunctionCallEdge (..), FunctionId, Node (..), NodeData (..),
                  NodeId, ProgramCfg (..), cfgEdges, cfgEntryId, cfgNodes, findReachable)
import qualified Swift.Ddg as Ddg
import Swift.Ddg (FunctionDdg (..), ProgramDdg (..))
import Swift.Syntax (Arg (..), BinOp (..), Expr (..), Function (..), Param (..), Stmt (..),
                     Type (..), UnOp (..))

cfgToDot :: ProgramCfg -> String
cfgToDot (ProgramCfg functions callEdges) = showWholeGraph "cfg" $
    cfgStatements functions callEdges False

ddgToDot :: ProgramDdg -> String
ddgToDot (ProgramDdg functions callEdges) =
    let cfgFunctionNodes = (map (\(Ddg.FunctionNode cfgNode _) -> cfgNode) functions)
        cfgStmts = cfgStatements cfgFunctionNodes callEdges True
    in showWholeGraph "ddg" (cfgStmts ++ concatMap ddgFunctionStatements functions)

cfgStatements :: [Cfg.FunctionNode] -> [FunctionCallEdge] -> Bool -> [String]
cfgStatements functions callEdges isDdg =
    let edgeLabel = \case
            Unconditional -> []
            IfTrue -> [Attribute "label" "true"]
            IfFalse -> [Attribute "label" "false"]
        showFunction (Cfg.FunctionNode fId function cfg) =
            let reachable = findReachable cfg
                styleAttrs from = if isDdg
                    then [Attribute "style" "invis"]
                    else reachabilityAttrs reachable from
                showCfgEdge (Cfg.Edge label from to) =
                    showEdge (GlobalNodeId fId from)
                        (GlobalNodeId fId to)
                        (styleAttrs from ++ edgeLabel label)
            in showFunctionGraph fId function
                (map (showNode fId reachable) (cfgNodes cfg) ++ map showCfgEdge (cfgEdges cfg))
        showCallEdge (FunctionCallEdge callerId nodeId calleeId) =
            let styleAttrs = if isDdg
                then [Attribute "style" "invis", Attribute "constraint" "false"]
                else [Attribute "color" "blue", Attribute "constraint" "false"]
            in fromMaybe "" $ flip fmap (findFunction calleeId functions) $
                \(Cfg.FunctionNode _ _ calleeCfg) -> showEdge (GlobalNodeId callerId nodeId)
                    (GlobalNodeId calleeId (cfgEntryId calleeCfg))
                    styleAttrs
    in (map showFunction functions ++ map showCallEdge callEdges)

ddgFunctionStatements :: Ddg.FunctionNode -> [String]
ddgFunctionStatements (Ddg.FunctionNode (Cfg.FunctionNode fId _ cfg) (FunctionDdg _ edges)) =
    let reachable = findReachable cfg
        showDdgEdge (Ddg.Edge from to) = showEdge (GlobalNodeId fId from)
            (GlobalNodeId fId to)
            (reachabilityAttrs reachable from ++ [Attribute "constraint" "false"])
    in map showDdgEdge edges

findFunction :: FunctionId -> [Cfg.FunctionNode] -> Maybe Cfg.FunctionNode
findFunction fId = find (\(Cfg.FunctionNode consideredId _ _) -> fId == consideredId)

showGlobalNodeId :: GlobalNodeId -> String
showGlobalNodeId (GlobalNodeId fId nodeId) = "f" ++ show fId ++ "n" ++ show nodeId

showWholeGraph :: String -> [String] -> String
showWholeGraph name statements = concat $
    ["digraph ", name, "{node", showAttrs [Attribute "shape" "box"], ";"]
    ++ statements
    ++ ["}"]

showFunctionGraph :: FunctionId -> Function -> [String] -> String
showFunctionGraph fId function statements = concat $
    ["subgraph cluster_", show fId, "{", showAttr (Attribute "label" (functionLabel function)), ";"]
    ++ statements
    ++ ["}"]

showNode :: FunctionId -> Set NodeId -> Node -> String
showNode fId reachable (Node nodeId nodeData) =
    let shape = case nodeData of
            (CfgCondition _)   -> [Attribute "shape" "diamond"]
            (CfgEntryParams _) -> [Attribute "shape" "oval"]
            CfgRepeatStart     -> [Attribute "shape" "circle"]
            _                  -> []
        color = if Set.member nodeId reachable
            then []
            else [Attribute "style" "filled", Attribute "color" "gray"]
        label = [Attribute "label" $ nodeLabel nodeData]
    in showGlobalNodeId (GlobalNodeId fId nodeId) ++ showAttrs (shape ++ color ++ label) ++ ";"

showEdge :: GlobalNodeId -> GlobalNodeId -> [Attribute] -> String
showEdge from to attrs =
    showGlobalNodeId from ++ "->" ++ showGlobalNodeId to ++ showAttrs attrs ++ ";"

showAttrs :: [Attribute] -> String
showAttrs attrs = "[" ++ intercalate "," (map showAttr attrs) ++ "]"

showAttr :: Attribute -> String
showAttr (Attribute k v) = k ++ "=" ++ quote v

reachabilityAttrs :: Set NodeId -> NodeId -> [Attribute]
reachabilityAttrs reachable from = [Attribute "style" "dashed" | not (Set.member from reachable)]

quote :: String -> String
quote s =
    let escape = \case
            '"' -> "\\\""
            '\\' -> "\\\\"
            c -> [c]
    in "\"" ++ concatMap escape s ++ "\""

functionLabel :: Function -> String
functionLabel (Function fName params _ _) = fName
    ++ "("
    ++ (intercalate ", " $ map (\(Param label name _) -> fromMaybe "_" label ++ " " ++ name) params)
    ++ ")"

nodeLabel :: NodeData -> String
nodeLabel = \case
    CfgEntryParams params -> "entry(" ++ intercalate ", " params ++ ")"
    CfgStmt stmt          -> showStmt stmt
    CfgCondition expr     -> showExpr expr
    CfgRepeatStart        -> "repeat"

showStmt :: Stmt -> String
showStmt = \case
    VarDecl varId ty expr        -> "var " ++ varId ++ showAnnotation ty ++ " = " ++ showExpr expr
    LetDecl varId ty expr        -> "let " ++ varId ++ showAnnotation ty ++ " = " ++ showExpr expr
    Assign varId expr            -> varId ++ " = " ++ showExpr expr
    CompoundAssign op varId expr -> varId ++ " " ++ showBinOp op ++ "= " ++ showExpr expr
    ExprStmt expr                -> showExpr expr
    Return Nothing               -> "return"
    Return (Just expr)           -> "return " ++ showExpr expr
    If {}                        -> "if"
    While _                      -> "while"
    RepeatWhile _                -> "repeat"

showAnnotation :: Maybe Type -> String
showAnnotation = maybe "" ((": " ++) . showType)

showType :: Type -> String
showType = \case
    TInt      -> "Int"
    TBool     -> "Bool"
    TString   -> "String"
    TArray ty -> "[" ++ showType ty ++ "]"

showExpr :: Expr -> String
showExpr = \case
    IntLit n          -> show n
    BoolLit b         -> if b then "true" else "false"
    Var varId         -> varId
    Unary op expr     -> showUnOp op ++ showOperand expr
    Binary op lhs rhs -> showOperand lhs ++ " " ++ showBinOp op ++ " " ++ showOperand rhs
    Call name args    -> name ++ "(" ++ intercalate ", " (map showArg args) ++ ")"

showOperand :: Expr -> String
showOperand = \case
    expr@(Binary {}) -> "(" ++ showExpr expr ++ ")"
    expr             -> showExpr expr

showArg :: Arg -> String
showArg (Arg label expr) = maybe "" (++ ": ") label ++ showExpr expr

showUnOp :: UnOp -> String
showUnOp = \case
    Negate -> "-"
    Not    -> "!"

showBinOp :: BinOp -> String
showBinOp = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Rem -> "%"
    Eq  -> "=="
    Neq -> "!="
    Lt  -> "<"
    Leq -> "<="
    Gt  -> ">"
    Geq -> ">="
    And -> "&&"
    Or  -> "||"

data Attribute = Attribute String String

data GlobalNodeId = GlobalNodeId FunctionId NodeId
