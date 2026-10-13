-- | Untyped surface syntax produced by the parser.
--
-- Nothing here is checked: @x + true@ is a perfectly valid 'Expr'.
-- A separate checking pass is meant to turn this into the typed AST in "Swift.Typed".
module Swift.Syntax
    ( Id
    , Type(..)
    , UnOp(..)
    , BinOp(..)
    , Expr(..)
    , Arg(..)
    , CondBlock(..)
    , Stmt(..)
    , Block
    , Param(..)
    , Function(..)
    , File(..)
    ) where

type Id = String

data Type = TInt
    | TBool
    | TString
    | TArray Type
    deriving (Eq, Show)

data UnOp = Negate
    | Not
    deriving (Eq, Ord, Show)

data BinOp = Add
    | Sub
    | Mul
    | Div
    | Rem
    | Eq
    | Neq
    | Lt
    | Leq
    | Gt
    | Geq
    | And
    | Or
    deriving (Eq, Ord, Show)

data Expr = IntLit Integer
    | BoolLit Bool
    | Var Id
    | Unary UnOp Expr
    | Binary BinOp Expr Expr
    | Call Id [Arg]
    deriving (Eq, Show)

-- | A call argument with its optional label: @f(x, by: 2)@.
data Arg = Arg (Maybe Id) Expr
    deriving (Eq, Show)

data CondBlock = CondBlock Expr Block
    deriving (Eq, Show)

data Stmt = VarDecl Id (Maybe Type) Expr
    | LetDecl Id (Maybe Type) Expr
    | Assign Id Expr
    -- | @x op= e@, e.g. @sum += i@ is @CompoundAssign Add "sum" (Var "i")@.
    | CompoundAssign BinOp Id Expr
    | ExprStmt Expr
    -- | @if c1 {..} else if c2 {..} else {..}@
    | If CondBlock [CondBlock] (Maybe Block)
    | While CondBlock
    -- | @repeat {..} while c@: the body runs before the condition.
    | RepeatWhile CondBlock
    | Return (Maybe Expr)
    deriving (Eq, Show)

type Block = [Stmt]

-- | @label name: Type@. The label is 'Nothing' for @_@; when it is omitted in
-- the source, Swift uses the parameter name as the label.
data Param = Param (Maybe Id) Id Type
    deriving (Eq, Show)

data Function = Function Id [Param] (Maybe Type) Block
    deriving (Eq, Show)

newtype File = File [Function]
    deriving (Eq, Show)
