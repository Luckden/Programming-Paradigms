{-# OPTIONS_GHC -Wno-missing-export-lists #-}
--This module contains the skeleton code for the assignment.
--
-- Please do not change the names of the parseExerciseX functions, as they
-- are used by the test suite.
--
-- You may, and are highly encouraged, to create your functions.
module Assignment where

import Instances
import Parser hiding (char)
import Control.Applicative
data ADT = JInt Int  -- Defining ADT types
        | SomeChar Char
        | Return ADT
        | Function ADT ADT ADT
        | Parameters [ADT]
        | FunctionCall ADT ADT
        | Statements [ADT]
        | Statement ADT
        | Variable ADT ADT
        | JIf ADT ADT
        | Parens ADT
        | JIfElse ADT ADT
        | Block [ADT]
        | VarName String
        | Ternary ADT ADT ADT
        | Not ADT
        | And ADT ADT
        | Or ADT ADT
        | Plus ADT ADT
        | Minus ADT ADT
        | Times ADT ADT
        | Power ADT ADT
        | Divide ADT ADT
        | Equal ADT ADT
        | NotEqual ADT ADT
        | LessThan ADT ADT
        | GreaterThan ADT ADT
        | Number Int
        | JString String
        | JBool
        | JTrue
        | JFalse
        | JArray [ADT]
        | Empty

  deriving (Eq, Show)

-- | Exercise A
parseInt :: Parser ADT
parseInt = JInt <$> int

--Helper function for parseString
quoteString :: Parser String
quoteString = charTok '"' *> many (isNot '"') <* charTok '"'

parseString :: Parser ADT
parseString = JString <$> quoteString

parseTrue :: Parser ADT
parseTrue = JTrue <$ stringTok "true"

parseFalse :: Parser ADT
parseFalse = JFalse <$ stringTok "false"

parseBool :: Parser ADT
parseBool = parseTrue <|> parseFalse

-- Helper function for sepBy
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = p <:> many (sep *> p)
  where
    (<:>) :: Parser a -> Parser [a] -> Parser [a]
    (<:>) = liftA2 (:)

-- Helper function for sepBy1 and parseArray
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

jValue :: Parser ADT
jValue = spaces *> (parseFunctionCall <|> parseBool <|> parseArray <|> parseString <|> parseInt <|> parseVarName) <* spaces

parseArray :: Parser ADT
parseArray = JArray <$> (is '[' *> (jValue `sepBy` commaTok) <* is ']')

parens :: Parser ADT -> Parser ADT -- this is to allow enclosed brackets and to enfoece parenthesis around expressions, function calls, etc.
parens p = Parens <$> (spaces *> charTok '(' *> spaces *> p <* spaces <* charTok ')' <* spaces)

-- parser for operators

op :: String -> Parser String
op s = spaces *> stringTok s <* spaces  -- general

parseAnd :: Parser (ADT -> ADT -> ADT)
parseAnd = op "&&" >> pure And

parseOr :: Parser (ADT -> ADT -> ADT)
parseOr = op "||" >> pure Or

parseNot :: Parser (ADT -> ADT)
parseNot = op "!" >> pure Not

parsePlus :: Parser (ADT -> ADT -> ADT)
parsePlus = op "+" >> pure Plus

parseMinus :: Parser (ADT -> ADT -> ADT)
parseMinus = op "-" >> pure Minus

parseTimes :: Parser (ADT -> ADT -> ADT)
parseTimes = op "*" >> pure Times

parsePower :: Parser (ADT -> ADT -> ADT)
parsePower = op "**" >> pure Power

parseDivide :: Parser (ADT -> ADT -> ADT)
parseDivide = op "/" >> pure Divide

parseEqual :: Parser (ADT -> ADT -> ADT)
parseEqual = op "===" >> pure Equal

parseNotEqual :: Parser (ADT -> ADT -> ADT)
parseNotEqual = op "!==" >> pure NotEqual

parseLessThan :: Parser (ADT -> ADT -> ADT)
parseLessThan = op "<" >> pure LessThan

parseGreaterThan :: Parser (ADT -> ADT -> ADT)
parseGreaterThan = op ">" >> pure GreaterThan

unaryExpr :: Parser ADT
unaryExpr = parens (do -- enforces a parenthesis around the expression
  spaces
  e <- parseNot
  spaces
  n <- parseExerciseA
  spaces
  return (e n))

binaryExpr :: Parser ADT
binaryExpr = parens (do
  spaces
  n1 <- parseExerciseA
  spaces
  e <- parseAnd <|> parseOr <|> parsePlus <|> parseMinus <|>parsePower <|> parseTimes  <|> parseDivide <|> parseEqual <|> parseNotEqual <|> parseLessThan <|> parseGreaterThan
  spaces
  n2 <- parseExerciseA
  spaces
  return (e n1 n2))

parseExpr :: Parser ADT
parseExpr = spaces *> (binaryExpr <|> unaryExpr <|> parseTernary) <* spaces

parseTernary :: Parser ADT
parseTernary = parens (do  -- enforces a parenthesis around the expression
  n1 <- parseExerciseA
  spaces
  charTok '?'
  spaces
  n2 <- parseExerciseA
  spaces
  charTok ':'
  spaces
  n3 <- parseExerciseA
  return (Ternary n1 n2 n3))

parseExerciseA :: Parser ADT
parseExerciseA = spaces *> parseTernary <|> parseExpr <|> parens parseExerciseA <|> jValue

prettyPrintExerciseA :: ADT -> String
prettyPrintExerciseA Empty = ""
prettyPrintExerciseA (FunctionCall a b) = prettyPrintExerciseA a ++ prettyPrintExerciseC b
prettyPrintExerciseA (JInt n) = show n
prettyPrintExerciseA (Parens a) = "( " ++ prettyPrintExerciseA a ++ " )"
prettyPrintExerciseA JTrue = "true"
prettyPrintExerciseA JFalse = "false"
prettyPrintExerciseA (JString s) = show s
prettyPrintExerciseA (VarName s) = s
prettyPrintExerciseA (JArray a) = case a of
  [] -> "[]"
  _ -> "[" ++ foldl (\acc x -> acc ++ ", " ++ prettyPrintExerciseA x) (prettyPrintExerciseA (head a)) (tail a) ++ "]"
prettyPrintExerciseA (Not n) = "!" ++ prettyPrintExerciseA n
prettyPrintExerciseA (And n1 n2) = prettyPrintExerciseA n1 ++ " && " ++ prettyPrintExerciseA n2
prettyPrintExerciseA (Or n1 n2) = prettyPrintExerciseA n1 ++ " || " ++ prettyPrintExerciseA n2
prettyPrintExerciseA (Plus n1 n2) = prettyPrintExerciseA n1 ++ " + " ++ prettyPrintExerciseA n2
prettyPrintExerciseA (Minus n1 n2) = prettyPrintExerciseA n1 ++ " - " ++ prettyPrintExerciseA n2
prettyPrintExerciseA (Times n1 n2) = prettyPrintExerciseA n1 ++ " * " ++ prettyPrintExerciseA n2
prettyPrintExerciseA (Power n1 n2) = prettyPrintExerciseA n1 ++ " ** " ++ prettyPrintExerciseA n2
prettyPrintExerciseA (Divide n1 n2) = prettyPrintExerciseA n1 ++ " / " ++ prettyPrintExerciseA n2
prettyPrintExerciseA (Equal n1 n2) = prettyPrintExerciseA n1 ++ " === " ++ prettyPrintExerciseA n2
prettyPrintExerciseA (NotEqual n1 n2) = prettyPrintExerciseA n1 ++ " !== " ++ prettyPrintExerciseA n2
prettyPrintExerciseA (LessThan n1 n2) = prettyPrintExerciseA n1 ++ " < " ++ prettyPrintExerciseA n2
prettyPrintExerciseA (GreaterThan n1 n2) = prettyPrintExerciseA n1 ++ " > " ++ prettyPrintExerciseA n2
prettyPrintExerciseA (Ternary n1 n2 n3) =  case length (prettyPrintExerciseA n1 ++ " ? " ++ prettyPrintExerciseA n2 ++ " : " ++ prettyPrintExerciseA n3) > 42 of
  True -> prettyPrintExerciseA n1 ++ " ?\n" ++ prettyPrintExerciseA n2 ++ "\n: " ++ prettyPrintExerciseA n3
  False -> prettyPrintExerciseA n1 ++ " ? " ++ prettyPrintExerciseA n2 ++ " : " ++ prettyPrintExerciseA n3

-- | Exercise B

parseVarName :: Parser ADT
parseVarName = VarName <$> (spaces *> some (alpha <|> digit <|> charTok '_') <* spaces)

parseVar :: Parser ADT
parseVar = do
  spaces
  stringTok "const"
  n1 <- parseVarName
  spaces
  charTok '='
  spaces
  n2 <- (parseExerciseA)
  return (Variable n1 n2)

-- generalised parser for statement /function body
parseBlock :: Parser ADT
parseBlock  = do
  spaces
  charTok '{'
  spaces
  n1 <- many (parseStatement <|> parseBlock)  -- there can be blocks or statements inside the body of statements or functions 
  spaces
  charTok '}'
  spaces
  return (Block n1)

parseIf :: Parser ADT
parseIf = do
  spaces
  stringTok "if"
  spaces
  n1 <- parens parseExerciseA  -- condition, enforces a parenthesis around the condition
  spaces
  n2 <- parseBlock  -- statement body
  return (JIf n1 n2)

parseIfElse :: Parser ADT
parseIfElse = do
  spaces
  n1 <- parseIf
  spaces
  stringTok "else"
  spaces
  n3 <- parseBlock  -- statement body
  spaces
  return (JIfElse n1 n3)

parseStatement :: Parser ADT -- Introduced Statement ADT to specify that many statement can appear in a block or outside
parseStatement = Statement <$> (spaces *> ( parseIfElse <|> parseIf <|> ((parseReturn <|> parseVar <|> parseFunctionCall) <* charTok ';') ) <* spaces)

parseStatements :: Parser ADT -- specifies the 'many' part of it.
parseStatements = Statements <$> many parseStatement

parseExerciseB :: Parser ADT -- parses the entire exercise B
parseExerciseB = parseBlock <|> parseStatements <|> parseExerciseA

prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB (Statement a) = case a of
  Return n -> prettyPrintExerciseB (Return n) ++ ";"  -- return is a statement that requires a semicolon
  Variable n1 n2 -> prettyPrintExerciseB (Variable n1 n2) ++ ";"  -- same
  FunctionCall n1 n2 -> prettyPrintExerciseB (FunctionCall n1 n2) ++ ";"
  JIf n1 n2 -> prettyPrintExerciseB (JIf n1 n2) -- if is a statement but does not need a semicolon
  JIfElse n1 n2 -> prettyPrintExerciseB (JIfElse n1 n2)  -- important to differentiate statements
prettyPrintExerciseB (Statements a) = case length a of
  0 -> ""
  1 -> prettyPrintExerciseB (head a)
  _ -> foldl (\acc x -> acc ++ "\n" ++ prettyPrintExerciseB x) (prettyPrintExerciseB (head a)) (tail a) ++ "\n"
prettyPrintExerciseB (JIf a b) = "if " ++ prettyPrintExerciseB a ++ " " ++ prettyPrintExerciseB b
prettyPrintExerciseB (JIfElse a b) = prettyPrintExerciseB a ++ " else " ++ prettyPrintExerciseB b
prettyPrintExerciseB (Variable a b) = "const " ++ prettyPrintExerciseB a ++ " = " ++ prettyPrintExerciseB b
prettyPrintExerciseB (Block a) = case length a of
  0 -> "{}"
  _ -> "{\n\t" ++ foldl (\acc x -> acc ++ "\n\t" ++ prettyPrintExerciseB x) (prettyPrintExerciseB (head a)) (tail a) ++ "\n}"
prettyPrintExerciseB (Return a) = "return " ++ prettyPrintExerciseC a
prettyPrintExerciseB a = prettyPrintExerciseA a

-- | Exercise C

-- a parser for parameters in function calls/ function definitions
parameters :: Parser ADT
parameters = Parameters <$> (parseExerciseA `sepBy` commaTok)

parseFunctionCall :: Parser ADT
parseFunctionCall = do
  spaces
  n1 <- parseVarName
  spaces
  n2 <-  parens parameters -- enforces a parenthesis around the parameters
  spaces
  return (FunctionCall n1 n2)

parseFunction :: Parser ADT
parseFunction = do
  spaces
  stringTok "function"
  spaces
  n1 <- parseVarName
  spaces
  n2 <- parens parameters -- enforces a parenthesis around the parameters
  spaces
  n3 <- parseBlock -- function body
  spaces
  return (Function n1 n2 n3)

parseReturn :: Parser ADT
parseReturn = do
  spaces
  stringTok "return"
  spaces
  n1 <- parseExerciseA
  spaces
  return (Return n1)


-- This function should determine if the given code is a tail recursive function
isTailRecursive :: String -> Bool
isTailRecursive input = case parse parseFunction input of  -- parses the input string as a function
  Result _ (Function (VarName a) _ body) -> case parse parseReturns (prettyPrintExerciseC body) of  -- parses the body of the function as a list of ADTs
      Result _ returns -> (countReturns returns >= 2) -- checks if the function has at least 2 return statements
        && (countReturnsSelf returns a == 1) -- 1 of which is a return statement of a self function call
          && isTailReturnSelf body a  -- and the final statement is a return statement of a self function call
      _ -> False
  _ -> False


-- Helper functions for isTailRecursive
-- Determines if the final statement in block is a return statemenet of self function call
isTailReturnSelf :: ADT -> String -> Bool
isTailReturnSelf body a = case body of
  Block x -> case x of
    [] -> False
    _ -> case last x of
      Statement (Return (FunctionCall (VarName b) _)) -> a == b
      _ -> False


-- Used to determine the number of return statemetns in the function.
countReturns :: [ADT] -> Int  -- counts the number of returns in the list of ADTs
countReturns = length . filter isReturn
  where
    isReturn (Return _) = True
    isReturn _ = False

-- Used to determine the number of self function calls.
countReturnsSelf :: [ADT] -> String -> Int  -- counts the number of returns has matching function call to the name specified as input
countReturnsSelf adts s = length (filter isReturnVarName adts)
  where
    isReturnVarName (Return (FunctionCall (VarName a) _)) = a == s
    isReturnVarName _ = False

-- Parser for any character
anyChar :: Parser Char
anyChar = Parser f
  where
    f "" = Error UnexpectedEof
    f (x:xs) = Result xs x

-- parses only Returns, ignoring anything else (hence also any ADT nested in the return statements)
parseReturns :: Parser [ADT]
parseReturns = do
  n <- many ((parseReturn <* charTok ';') <|> (SomeChar <$> anyChar))
  return (concatMap extractReturns n)
  where
    extractReturns :: ADT -> [ADT]
    extractReturns (Return n) = [Return n]
    extractReturns _ = []

parseExerciseC :: Parser ADT
parseExerciseC = parseFunction <|> parseExerciseB <|> parseExerciseA

-- pretty printing
prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC (Parens a) = "(" ++ prettyPrintExerciseC a ++ ")"
prettyPrintExerciseC (Function a b c) = case isTailRecursive ("function " ++ prettyPrintExerciseC a ++ prettyPrintExerciseC b ++ prettyPrintExerciseC c) of
  True -> "function " ++ prettyPrintExerciseC a ++ prettyPrintExerciseC b ++ prettyPrintOptimised b c
  False -> "function " ++ prettyPrintExerciseC a ++ prettyPrintExerciseC b ++ prettyPrintExerciseC c
prettyPrintExerciseC (Parameters a) = case length a of
  0 -> ""
  _ -> foldl (\acc x -> acc ++ ", " ++ prettyPrintExerciseC x) (prettyPrintExerciseC (head a)) (tail a)
prettyPrintExerciseC a = prettyPrintExerciseB a

-- pretty printing for last assessed part, prints optimised function if it is tail recursive
prettyPrintOptimised :: ADT -> ADT -> String
prettyPrintOptimised (Parens params) (Block statements) = case last statements of
  Statement (Return (FunctionCall _ (Parens recurParams))) -> "{\n\twhile (true) {\n\t" ++ prettyPrintExerciseB (Statements (init statements)) ++ "\n" ++ "[" ++ prettyPrintExerciseC params ++ "] = [" ++ prettyPrintExerciseC recurParams ++ "];\n\t}\n}"

-- Extras

-- Converts if else statements to ternary statements
-- ghci> parse (prettyTernaryAlt <$> parseTernaryAlt) "if ( (true && false) ){const a = 1;} else {const b = 2;if ( true ) { const c = (b + 1); }}"
-- Result >< "((true && false)) ?\n{\n\tconst a = 1;\n}\n: {\n\tconst b = 2;\n\tif ( true ) {\n\tconst c = ( b + 1 );\n}\n}"
parseTernaryAlt :: Parser ADT
parseTernaryAlt = do
  spaces
  stringTok "if"
  spaces
  n1 <- parseExerciseA  -- condition
  spaces
  n2 <- parseBlock
  spaces
  stringTok "else"
  spaces
  n3 <- parseBlock
  spaces
  return (Ternary n1 n2 n3)

prettyTernaryAlt :: ADT -> String
prettyTernaryAlt (Ternary n1 n2 n3) = case length (prettyPrintExerciseB n1 ++ " ? " ++ prettyPrintExerciseB n2 ++ " : " ++ prettyPrintExerciseB n3) > 42 of
  True -> prettyPrintExerciseB n1 ++ " ?\n" ++ prettyPrintExerciseB n2 ++ "\n: " ++ prettyPrintExerciseB n3
  False -> prettyPrintExerciseB n1 ++ " ? " ++ prettyPrintExerciseB n2 ++ " : " ++ prettyPrintExerciseB n3