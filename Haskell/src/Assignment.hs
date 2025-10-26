module Assignment (bnfParser, generateHaskellCode, validate, ADT, getTime) where

import Instances (Parser (..))
import Parser (is, isNot, string, spaces, alpha, digit, eof)
import Control.Applicative (many, some, (<|>))
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)
import Data.Char (toUpper, toLower)


--------------------------------
-------- ADT Definition --------
--------------------------------
-- BNF Grammar can be represented as one or more "rules"
-- E.g. "grammar ::= [Rule "number" [...], Rule "expression" [...]]"
data ADT = Grammar [Rule]
    deriving (Show)

-- Each rule is defined by a name and a list of alternatives (expressions containing their Terminals and Non-terminals values)
-- E.g. "rule ::= Rule "number" [Alternative [...], Alternative [...]]" or "rule ::= Rule "expression" [Alternative [...], Alternative [...]]"
data Rule = Rule String [Alternative]
    deriving (Show)

-- An alternative is a sequence of elements (Terminals and Non-terminals)
-- E.g. "alternative ::= Alternative [Terminal "0", Terminal "1"]" or "alternative ::= Alternative [NonTerminal "expression", Terminal "+", NonTerminal "term"]"
data Alternative = Alternative [Element]
    deriving (Show)

-- An element can be either a Terminal (literal string) or a Non-terminal (reference to another rule)
-- E.g. "element ::= Terminal "0"" or "element ::= NonTerminal "expression""
data Element 
    = Terminal String
    | NonTerminal String
    | Macro MacroType
    deriving (Show)

-- Macros allow to differentiate special elements in the grammar, e.g. integers, strings, newlines
-- E.g. "element ::= Macro IntMacro" or "element ::= Macro NewlineMacro
data MacroType = IntMacro | StringMacro | NewlineMacro
    deriving (Show)

-- Parser for the BNF grammar
bnfParser :: Parser ADT
bnfParser = do
        rules <- many ruleParser    -- Parse multiple rules
        _ <- spaces                 -- Discard any trailing spaces
        _ <- eof                    -- Ensure we reach the end of input
        return $ Grammar rules
    where
        ruleParser = spaces *> rule <* spaces   -- Parse a rule with surrounding spaces (or blank)

--------------------------------
------- Helper functions -------
--------------------------------
isSingle :: [a] -> Bool
isSingle [[_]] = True
isSingle _ = False

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

uncapitalize :: String -> String
uncapitalize "" = ""
uncapitalize (c:cs) = toLower c : cs

returnElementParser :: Element -> String
returnElementParser (Nonterminal n) = uncapitalize n
returnElementParser (Terminal s) = "(string " ++ show s ++ ")"
returnElementParser (Macro IntMacro) = "int"
returnElementParser (Macro AlphaMacro) = "(some alpha)"
returnElementParser (Macro NewlineMacro) = "(is '\\n')"

returnElementType :: Element -> String
returnElementType (Nonterminal n) = capitalize n
returnElementType (Terminal _) = "String"
returnElementType (Macro IntMacro) = "Int"
returnElementType (Macro AlphaMacro) = "String"
returnElementType (Macro NewlineMacro) = "Char"


---------------------------------------
------- Haskell Code Generation -------
---------- Type Definitions -----------
---------------------------------------

-- Generates a Haskell type constructor for a given alternative
-- Example: Name: Expression, Alternative Index: 2, Alternative: [ NonTerminal "number", Terminal "+", NonTerminal "expression" ] to
-- Expression2 Number String Expression
generateConstructor :: String -> Int -> Alternative -> String
generateConstructor typeName index (Alternative elements) =
    typeName ++ show index ++ " " ++ concatMap (\e -> " " ++ returnElementType e) elements --For each alternative, increment name by 1 (e.g., Expr1, Expr2, etc. ) and then list the types of its elements

-- Generates an Haskell data type with one or more alternatives, each with a unique name and constructor
-- Converts inputs:Name: expression, Alternatives: [ Alternative [ NonTerminal "term" ], Alternative [ NonTerminal "term", Terminal "+", NonTerminal "expression" ] ]
-- data Expression = Expression1 Term
--                 | Expression2 Term String Expression
--    deriving (Show)
generateData :: Rule -> String
generateData (Rule name alternatives) = 
        "data " ++ typeName ++ " = " ++ firstConstructor ++ "\n" ++ restConstructors ++ "\n" ++
        "    deriving (Show)\n"
    where
        typeName = capitalize name
        firstConstructor = generateConstructor typeName 1 $ head alternatives   --Generate the first constructor without the "|" operator
        restConstructors = concatMap
            (\(i, alt) -> "                | " ++ generateConstructor typeName i alt ++ "\n")
            (zip [2..] (tail alternatives)) --For each alternative after the first, generate a constructor with the "|" operator prefixed onto it

-- Generates a Haskell newtype for a single alternative consisting of a single element
-- Convert "number", [ Alternative[ Macro IntMacro ] ] to
-- newtype Number = Number Int
--    deriving (Show)
generateNewType :: Rule -> String
generateNewType (Rule name [Alternative [element]]) = 
        "newtype" ++ capitalize name ++ " = " ++ capitalize name ++ " " ++ returnElementType element ++ "\n" ++ "    deriving (Show)\n"

-- For a single rule, generate the corresponding Haskell type definition
-- Convert Rule "number" [ Alternative[ Macro IntMacro ] ] to
-- newtype Number = Number Int
--   deriving Show
-- or
-- Convert Rule "expression" [ Alternative[ Nonterminal "number" ], Alternative[ Nonterminal "number", Terminal "+", Nonterminal "expression" ] ] to the Haskell
-- data Expression = Expression1 Number
--                 | Expression2 Number String Expression
--   deriving Show
generateType :: Rule -> String
generateType rule@(Rule _ alternatives) 
    | isSingleMacro alternatives = generateNewType rule
    | otherwise = generateData rule

-- generateTypes takes a list of Grammar rules and produces Haskell code as a String
generateTypes :: [Rule] -> String
generateTypes rules = unlines $ map generateType rules -- Unlines takes a list of strings and concatenates them with newline characters


---------------------------------------
------- Haskell Code Generation -------
--------- Parser Definitions ----------
---------------------------------------
-- Generates a Haskell parser function for a data type alternative consisting of multiple elements
-- Example: Name: Expression, Alternative Index: 2, Alternative: [ NonTerminal "number", Terminal "+", NonTerminal "expression" ] to
-- "Expression2 <$> number <*> (string "+") <*> expression"
generateDataParser :: String -> Int -> Alternative -> String
generateDataParser typeName index [] = typeName ++ show index
generateDataParser typeName index (element:rest) =
    typeName ++ show index ++ " <$> " ++ elementParser element ++ concatMap (\e -> " <*> " ++ elementParser e) rest
    
-- Generates a Haskell parser specifically for a newtype alternative consisting of a single element
-- Example: Name: number, Alternative: [ Macro IntMacro ] to
-- number = Number <$> int
generateNewTypeParser :: String -> Alternative -> String
generateNewTypeParser typeName (Alternative [element]) = 
        typeName ++ " <$> " ++ elementParser element
-- Generates a Haskell parser function for a single alternative
-- Example: Name: Expression, Alternative Index: 2, Alternative: [ NonTerminal "number", Terminal "+", NonTerminal "expression" ] to
-- "Expression2 <$> number <*> (string \"+\") <*> expression"
generateAlternativeParser :: String -> Int -> Alternative -> String
generateAlternativeParser typeName index (Alternative elements) =
    if isSingle elements then
        generateNewTypeParser typeName elements
    else
        generateDataParser typeName index elements

-- Parses multiple alternatives to the generateAlternativeParser function
-- Example: Name: Expression, Alternatives: [ Alternative [ NonTerminal "term" ], Alternative [ NonTerminal "term", Terminal "+", NonTerminal "expression" ] ] to
-- "Expression1 <$> termParser
--     <|> Expression2 <$> termParser <*> (string "+") <*> expressionParser"
generateAlternatives :: Rule -> String
generateAlternatives (Rule name [alternative]) =
    generateAlternativeParser name 1 alternative ++ "\n"

generateAlternatives (Rule name (alternative:rest)) =
    generateAlternativeParser name 1 alternative ++ "\n" ++
    concatMap
        (\(i, alt) -> "    <|> " ++ generateAlternativeParser name i alt ++ "\n")
        (zip [2..] rest)

-- Generates a Haskell parser function for a single rule
-- Example: Rule "number" [Alternative [Macro IntMacro]] to
-- number :: Parser Number
-- number = Number <$> int
generateParser :: Rule -> String
generateParser (Rule name alternatives) = 
        uncapitalize name ++ " :: Parser " ++ typeName ++ "\n" ++
        uncapitalize name ++ " = " ++ generateAlternatives $ Rule typeName alternatives
    where
        typeName = capitalize name


-- Accepts a list of Grammar rules and produces Haskell parser code as a String
-- [ Rule "number" [Alternative [Macro IntMacro]]
-- , Rule "expression" 
--     [ Alternative [Nonterminal "number"]
--     , Alternative [Nonterminal "number", Terminal "+", Nonterminal "expression"]
--     ]
-- ] to
-- number :: Parser Number
-- number = Number <$> int
--
-- expression :: Parser Expression
-- expression = Expression1 <$> number
--              <|> Expression2 <$> number <*> (string "+") <*> expression
generateParsers :: [Rule] -> String
generateParsers rules = unlines (map generateParser rules)


-- generateHaskellCode takes the Grammar Rules formed by the BNF and produces Haskell code as a String
--
-- Example BNF Grammar:
-- <number> ::= [int]
-- <expression> ::= <number> | <number> "+" <expression>
--
-- to Grammar:
-- Grammar 
--   [ Rule "number" 
--       [Alternative [Macro IntMacro]]
--   , Rule "expression"
--       [Alternative [Nonterminal "number"],
--        Alternative [Nonterminal "number", Terminal "+", Nonterminal "expression"]]
--   ]

-- Example output:
-- expression :: Parser Expression
-- expression = Expression1 <$> number
--              <|> Expression2 <$> number <*> (string "+") <*> expression
generateHaskellCode :: ADT -> String
generateHaskellCode (Grammar rules) = 
    typeDefs ++ "\n" ++ parserDefs
    where
        typeDefs = generateTypes rules
        parserDefs = generateParsers rules

validate :: ADT -> [String]
validate _ = ["If i change these function types, I will get a 0 for correctness"]

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime


-- | -------------------------------------------------
-- | -------------- Custom BNF parsers ---------------
-- | -------------------------------------------------

-- Parses nonterminal expressions e.g. <expr>
nonterminal :: Parser String
nonterminal = do
  _ <- is '<'                                                         -- discard the opening angle bracket
  name <- some (alpha <|> digit <|> is '_')                           -- parse the name of the nonterminal
  _ <- is '>'                                                         -- discard the closing angle bracket   
  return name

-- Parses terminal expressions e.g. "+"
terminal :: Parser String
terminal = do
  _ <- is '"'                                                         -- discard the opening quote
  content <- many (isNot '"')                                         -- parse the content of the terminal
  _ <- is '"'                                                         -- discard the closing quote
  return content

-- Parses macro expressions e.g. [int], [alpha], [newline]
macro :: Parser MacroType
macro = do
  _ <- is '['                                                         -- discard the opening square bracket
  macroType <- string "int" <|> string "alpha" <|> string "newline"   -- parse the macro type
  _ <- is ']'                                                         -- discard the closing square bracket
  return $ case macroType of
    "int"     -> IntMacro
    "alpha"   -> StringMacro
    "newline" -> NewlineMacro
    _         -> error "Unrecognized macro type"

-- Elements consist of nonterminals, terminals, and macros
element :: Parser Element
element = spaces *> (ntElement <|> tElement <|> mElement) <* spaces   -- discard surrounding spaces and parse the element
    where
        ntElement = NonTerminal <$> nonterminal
        tElement = Terminal <$> terminal
        mElement = Macro <$> macro

-- An alternative consist of one or more elements
alternative :: Parser Alternative
alternative = Alternative <$> some element                            -- used some to ensure at least one element is parsed

-- A list of alternatives separated by '|'
alternatives :: Parser [Alternative]
alternatives = do
    first <- alternative                                              -- parse the first alternative     
    rest <- many (spaces *> is '|' *> spaces *> alternative)          -- used many to allow zero or more additional alternatives if more available
    return $ first : rest

-- Parse the full rule (<name> ::= <alternatives>)
rule :: Parser Rule
rule = do
  _ <- spaces
  name <- nonterminal                                                 -- Parse <name>
  _ <- spaces *> string "::=" <* spaces                               -- Parse ::= separator
  alts <- alternatives                                                -- Parse alternatives
  _ <- spaces
  return $ Rule name alts
