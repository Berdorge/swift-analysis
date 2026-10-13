{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module Swift.Typed
    ( Id
    , Type(..)
    , IntBinop(..)
    , Comparison(..)
    , BoolBinop(..)
    , Witness(..)
    , Expression(..)
    , CondBlock(..)
    , Statement(..)
    , Block
    , Param(..)
    , Function(..)
    , File(..)
    , witnessToType
    ) where

import Data.Type.Equality (TestEquality (testEquality), (:~:) (Refl))

type Id = String

data Type = TBool
    | TInt
    | TString
    | TArray Type
    deriving (Show, Eq)

data IntUnary = Negate

data BoolUnary = Not

data IntBinop = Add
    | Subtract
    | Multiply
    | Divide
    | Remainder

data Comparison = Eq
    | Neq
    | Lt
    | Leq
    | Geq
    | Gt

data BoolBinop = And
    | Or

data Witness a where
    BoolWitness :: Witness TBool
    IntWitness :: Witness TInt
    StringWitness :: Witness TString
    ArrayWitness :: Witness a -> Witness (TArray a)

instance TestEquality Witness where
    testEquality :: Witness a -> Witness b -> Maybe (a :~: b)
    testEquality BoolWitness BoolWitness             = Just Refl
    testEquality IntWitness IntWitness               = Just Refl
    testEquality StringWitness StringWitness         = Just Refl
    testEquality (ArrayWitness w1) (ArrayWitness w2) = fmap (\Refl -> Refl) (testEquality w1 w2)
    testEquality _ _                                 = Nothing

data Expression a where
    IntLiteral :: Integer -> Expression TInt
    BoolLiteral :: Bool -> Expression TBool
    IntNegate :: Expression TInt -> Expression TInt
    BoolNot :: Expression TBool -> Expression TBool
    IntBinop :: IntBinop -> Expression TInt -> Expression TInt -> Expression TInt
    Comparison :: Comparison -> Expression TInt -> Expression TInt -> Expression TBool
    BoolBinop :: BoolBinop -> Expression TBool -> Expression TBool -> Expression TBool
    IdExpression :: Id -> Witness a -> Expression a

data CondBlock = CondBlock (Expression TBool) Block

data Statement where
    VarStatement :: Id -> Expression a -> Statement
    LetStatement :: Id -> Expression a -> Statement
    AssignStatement :: Id -> Expression a -> Statement
    IntBinopAssign :: Id -> IntBinop -> Expression TInt -> Statement
    ExprStatement :: Expression a -> Statement
    IfStatement :: CondBlock -> [CondBlock] -> Maybe Block -> Statement
    WhileStatement :: CondBlock -> Statement
    RepeatStatement :: CondBlock -> Statement
    ReturnStatement :: Maybe (Expression a) -> Statement

type Block = [Statement]

data Param = Param (Maybe Id) Id Type

data Function = Function Id [Param] (Maybe Type) Block

data File = File [Function]

witnessToType :: Witness a -> Type
witnessToType = \case
    BoolWitness -> TBool
    IntWitness -> TInt
    StringWitness -> TString
    ArrayWitness w -> TArray (witnessToType w)
