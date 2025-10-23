module Assignment (bnfParser, generateHaskellCode, validate, ADT, getTime) where

import Instances (Parser (..))
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)
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

bnfParser :: Parser ADT
bnfParser = pure (TODO "I can change the ADT type however I like")

generateHaskellCode :: ADT -> String
generateHaskellCode _ = "-- But I cannot change the type of these three functions."

validate :: ADT -> [String]
validate _ = ["If i change these function types, I will get a 0 for correctness"]

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime
