module Swift.Parser
    ( parseSwift
    , parseExpr
    , parseStmt
    ) where

import Control.Monad (void, when)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Void (Void)
import Swift.Syntax
import Text.Megaparsec (Parsec, between, choice, eof, errorBundlePretty, getSourcePos, many,
                        notFollowedBy, optional, runParser, sepBy, some, sourceLine, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parseSwift :: FilePath -> String -> Either String File
parseSwift = parseComplete (File <$> many pFunction)

parseExpr :: String -> Either String Expr
parseExpr = parseComplete pExpr "<expression>"

parseStmt :: String -> Either String Stmt
parseStmt = parseComplete pStmt "<statement>"

parseComplete :: Parser a -> FilePath -> String -> Either String a
parseComplete parser path input = case runParser (sc *> parser <* eof) path input of
    Left err    -> Left (errorBundlePretty err)
    Right value -> Right value

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifierChar :: Parser Char
identifierChar = alphaNumChar <|> char '_'

keywordText :: String -> Parser ()
keywordText word = void (try (string word <* notFollowedBy identifierChar))

keyword :: String -> Parser ()
keyword = lexeme . keywordText

identifier :: Parser Id
identifier = lexeme $ try $ do
    name <- (:) <$> (letterChar <|> char '_') <*> many identifierChar
    when (name `elem` reservedWords) (fail "reserved keyword cannot be an identifier")
    pure name

reservedWords :: [String]
reservedWords =
    [ "func", "var", "let", "if", "else", "while", "repeat", "return"
    , "true", "false", "for", "in", "break", "continue", "guard", "switch"
    , "case", "default", "defer", "throw", "throws", "try", "catch", "do"
    , "class", "struct", "enum", "protocol", "import", "nil", "inout"
    ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pType :: Parser Type
pType = choice
    [ TInt <$ keyword "Int"
    , TBool <$ keyword "Bool"
    , TString <$ keyword "String"
    , TArray <$> between (symbol "[") (symbol "]") pType
    ]

pFunction :: Parser Function
pFunction = do
    keyword "func"
    name <- identifier
    params <- parens (pParam `sepBy` symbol ",")
    result <- optional (symbol "->" *> pType)
    Function name params result <$> pBlock

pParam :: Parser Param
pParam = do
    first <- identifier
    second <- optional identifier
    void (symbol ":")
    paramType <- pType
    let name = maybe first id second
        label = if first == "_" then Nothing else Just first
    pure (Param label name paramType)

pBlock :: Parser Block
pBlock = between (symbol "{") (symbol "}") (many pStmt)

pStmt :: Parser Stmt
pStmt = choice
    [ pDecl "var" VarDecl
    , pDecl "let" LetDecl
    , pIf
    , While <$> (keyword "while" *> pCondBlock)
    , pRepeatWhile
    , pReturn
    , try pAssignment
    , ExprStmt <$> pExpr
    ] <* optional (symbol ";")

pDecl :: String -> (Id -> Maybe Type -> Expr -> Stmt) -> Parser Stmt
pDecl word constructor = do
    keyword word
    name <- identifier
    annotation <- optional (symbol ":" *> pType)
    void (operator "=")
    constructor name annotation <$> pExpr

pAssignment :: Parser Stmt
pAssignment = do
    name <- identifier
    constructor <- choice
        [ CompoundAssign op name <$ operator token
        | (token, op) <- [("+=", Add), ("-=", Sub), ("*=", Mul), ("/=", Div), ("%=", Rem)]
        ] <|> (Assign name <$ operator "=")
    constructor <$> pExpr

pCondBlock :: Parser CondBlock
pCondBlock = CondBlock <$> pExpr <*> pBlock

pIf :: Parser Stmt
pIf = do
    keyword "if"
    first <- pCondBlock
    rest <- many (try (keyword "else" *> keyword "if") *> pCondBlock)
    fallback <- optional (keyword "else" *> pBlock)
    pure (If first rest fallback)

pRepeatWhile :: Parser Stmt
pRepeatWhile = do
    keyword "repeat"
    body <- pBlock
    keyword "while"
    condition <- pExpr
    pure (RepeatWhile (CondBlock condition body))

pReturn :: Parser Stmt
pReturn = do
    keywordText "return"
    end <- getSourcePos
    sc
    next <- getSourcePos
    -- Other tokens discard newlines; return needs one to end a bare return.
    value <- if sourceLine next > sourceLine end then pure Nothing else optional pExpr
    pure (Return value)

-- Backtrack a token's boundary check, so '=' cannot consume part of '=='.
operator :: String -> Parser String
operator token = lexeme (try (string token <* notFollowedBy (char '=')))

pExpr :: Parser Expr
pExpr = makeExprParser pPrimary
    [ [Prefix pPrefixes]
    , [binaryL "*" Mul, binaryL "/" Div, binaryL "%" Rem]
    , [binaryL "+" Add, binaryL "-" Sub]
    , [ binaryN "==" Eq, binaryN "!=" Neq
      , binaryN "<=" Leq, binaryN "<" Lt, binaryN ">=" Geq, binaryN ">" Gt
      ]
    , [binaryL "&&" And]
    , [binaryL "||" Or]
    ]

pPrefixes :: Parser (Expr -> Expr)
pPrefixes = do
    ops <- some ((Negate <$ operator "-") <|> (Not <$ operator "!"))
    pure (\expr -> foldr Unary expr ops)

binaryL :: String -> BinOp -> Operator Parser Expr
binaryL token op = InfixL (Binary op <$ operator token)

binaryN :: String -> BinOp -> Operator Parser Expr
binaryN token op = InfixN (Binary op <$ operator token)

pPrimary :: Parser Expr
pPrimary = choice
    [ IntLit <$> lexeme (L.decimal <* notFollowedBy identifierChar)
    , BoolLit True <$ keyword "true"
    , BoolLit False <$ keyword "false"
    , parens pExpr
    , pVariableOrCall
    ]

pVariableOrCall :: Parser Expr
pVariableOrCall = do
    name <- identifier
    args <- optional (parens (pArg `sepBy` symbol ","))
    pure (maybe (Var name) (Call name) args)

pArg :: Parser Arg
pArg = Arg <$> optional (try (identifier <* symbol ":")) <*> pExpr
