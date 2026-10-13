{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}

module Swift.CheckAst
    ( checkAst
    ) where

import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Type.Equality (TestEquality (testEquality), (:~:) (Refl))
import qualified Swift.Syntax as S
import qualified Swift.Typed as T
import Swift.Typed (BoolBinop (..), Comparison (..), IntBinop (..), Witness (..), witnessToType)

type VarTypes = Map T.Id WitnessWrapper

checkAst :: S.File -> Either String T.File
checkAst (S.File functions) = T.File <$> mapM checkFunction functions

checkFunction :: S.Function -> Either String T.Function
checkFunction (S.Function name params returnType block) =
    mapM checkParam params >>= \checkedParams ->
    mapM checkType returnType >>= \checkedType ->
    mapM checkStatement block <&> \checkedBlock ->
        T.Function name checkedParams checkedType checkedBlock

checkParam :: S.Param -> Either String T.Param
checkParam = pure . pureCheckParam

pureCheckParam :: S.Param -> T.Param
pureCheckParam (S.Param label name paramType) =
    T.Param label name (pureCheckType paramType)

checkType :: S.Type -> Either String T.Type
checkType = pure . pureCheckType

pureCheckType :: S.Type -> T.Type
pureCheckType = \case
    S.TInt -> T.TInt
    S.TBool -> T.TBool
    S.TString -> T.TString
    S.TArray arrayType -> T.TArray (pureCheckType arrayType)

checkStatement :: VarTypes -> S.Stmt -> Either String T.Statement
checkStatement types = \case
    S.VarDecl name t expr -> do
        expression <- checkExpr expr
        mapM checkType t >>= \case
            Nothing -> pure ()
            (Just ty) -> when (ty /= getType expression)
                (Left "Declared type != expression type")
        -- checkExpr expr <&> \checkedExpr ->
        -- T.VarStatement name checkedType checkedExpr
    S.LetDecl name t expr -> mapM checkType t >>= \checkedType ->
        checkExpr expr <&> \checkedExpr ->
        T.LetStatement name checkedType checkedExpr
    S.Assign name expr -> checkExpr expr <&> \checkedExpr ->
        T.AssignStatement name checkedExpr

checkExpr :: VarTypes -> S.Expr -> Either String ExpWrapper
checkExpr types = \case
    S.IntLit i -> pure $ ExpWrapper T.IntWitness $ T.IntLiteral i
    S.BoolLit b -> pure $ ExpWrapper T.BoolWitness $ T.BoolLiteral b
    S.Var id -> case Map.lookup id types of
        Just (WitnessWrapper witness) -> pure $ ExpWrapper witness $ T.IdExpression id witness
        Nothing                       -> Left $ "No variable named " ++ id
    S.Unary op expr -> checkExpr types expr >>= case op of
        S.Negate -> checkUnaryOp IntWitness T.IntNegate
        S.Not    -> checkUnaryOp BoolWitness T.BoolNot
    S.Binary op expr1 expr2 -> case classify op of
        IntClass intOp -> do
            lhs <- checkExpr types expr1 >>= verifyType IntWitness
            rhs <- checkExpr types expr2 >>= verifyType IntWitness
            return (ExpWrapper IntWitness $ T.IntBinop intOp lhs rhs)
        ComparisonClass comp -> do
            lhs <- checkExpr types expr1 >>= verifyType IntWitness
            rhs <- checkExpr types expr2 >>= verifyType IntWitness
            return (ExpWrapper BoolWitness $ T.Comparison comp lhs rhs)
        BoolClass boolOp -> do
            lhs <- checkExpr types expr1 >>= verifyType BoolWitness
            rhs <- checkExpr types expr2 >>= verifyType BoolWitness
            return (ExpWrapper BoolWitness $ T.BoolBinop boolOp lhs rhs)
    S.Call 

checkUnaryOp :: Witness a -> (T.Expression a -> T.Expression a) -> ExpWrapper -> Either String ExpWrapper
checkUnaryOp witness mapper = fmap (ExpWrapper witness . mapper) . verifyType witness

verifyType :: T.Witness a -> ExpWrapper -> Either String (T.Expression a)
verifyType expectedWitness (ExpWrapper actualWitness expression) =
    case testEquality expectedWitness actualWitness of
        Just Refl -> pure expression
        Nothing -> Left $ "Expected " ++ (show $ witnessToType expectedWitness) ++ ", got " ++
            (show $ witnessToType actualWitness)

classify :: S.BinOp -> BinopClass
classify = \case
    S.Add -> IntClass T.Add
    S.Sub -> IntClass T.Subtract
    S.Mul -> IntClass T.Multiply
    S.Div -> IntClass T.Divide
    S.Rem -> IntClass T.Remainder
    S.Eq -> ComparisonClass T.Eq
    S.Neq -> ComparisonClass T.Neq
    S.Lt -> ComparisonClass T.Lt
    S.Leq -> ComparisonClass T.Leq
    S.Gt -> ComparisonClass T.Gt
    S.Geq -> ComparisonClass T.Geq
    S.And -> BoolClass T.And
    S.Or -> BoolClass T.Or

data BinopClass = IntClass IntBinop
    | ComparisonClass Comparison
    | BoolClass BoolBinop

data ExpWrapper = forall a . ExpWrapper (T.Witness a) (T.Expression a)

data WitnessWrapper = forall a . WitnessWrapper (T.Witness a)
