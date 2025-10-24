module Assignment (bnfParser, generateHaskellCode, validate, ADT, getTime) where

import Instances (Parser (..))
import Parser (Parser, is, isNot, string, spaces, alpha, digit, (<|>), some, many)
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)

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
    "alpha"   -> AlphaMacro
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

-- Parse the ::= separator
separator :: Parser Char
separator = spaces *> string "::=" <* spaces

-- Parse the full rule (<name> ::= <alternatives>)
rule :: Parser Rule
rule = do
  _ <- spaces
  name <- nonterminal                                                 -- Parse <name>
  _ <- separator                                                      -- Parse ::= separator
  alts <- alternatives                                                -- Parse alternatives
  _ <- spaces
  return $ Rule name alts


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
        return $ Grammar rules
    where
        ruleParser = spaces *> rule <* spaces   -- Parse a rule with surrounding spaces (or blank)

generateHaskellCode :: ADT -> String
generateHaskellCode _ = "-- But I cannot change the type of these three functions."

validate :: ADT -> [String]
validate _ = ["If i change these function types, I will get a 0 for correctness"]

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime
