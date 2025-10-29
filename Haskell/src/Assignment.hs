module Assignment (bnfParser, generateHaskellCode, validate, ADT(..), Rule(..), Alternative(..), Element(..), MacroType(..), getTime, getFirstParserName, generateHaskellCodeWithTimestamp) where

import Instances (Parser (..))
import Parser (is, isNot, string, spaces, alpha, digit, eof, inlineSpaces, lower)
import Control.Applicative (many, some, (<|>), optional)
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
-- Added polymorphism:
-- Accepts a list of type parameters after the name of the rule
-- E.g. "rule ::= Rule "list" ["a"] [Alternative [...], Alternative [...]]"
data Rule = Rule String [String] [Alternative]
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
    | Modified Modifier Element     --Elements can be wrapped with a modifier
    | ParameterRef String
    | ParameterizedCall String [Element]
    deriving (Show)

-- Part C: Extended Elements with Modifiers
data Modifier
    = TokModifier           --(Tok) Remove trailing whitespace
    | StarModifier          --(*) Zero or more
    | PlusModifier          --(+) One or more
    | QuestionModifier      --(?) Zero or one (optional)
    deriving (Show)

-- Macros allow to differentiate special elements in the grammar, e.g. integers, strings, newlines
-- E.g. "element ::= Macro IntMacro" or "element ::= Macro NewlineMacro
data MacroType = IntMacro | StringMacro | NewlineMacro
    deriving (Show)

--------------------------------
------- Helper functions -------
--------------------------------
-- Checks if a list has exactly one element
isSingle :: [a] -> Bool
isSingle [_] = True
isSingle _ = False

-- Capitalizes the first letter of a string
capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

-- Uncapitalizes the first letter of a string
uncapitalize :: String -> String
uncapitalize "" = ""
uncapitalize (c:cs) = toLower c : cs

-- Get the name of the first parser (for filename)
getFirstParserName :: ADT -> String
getFirstParserName (Grammar []) = "Output"                          --Default name if no rules
getFirstParserName (Grammar (Rule name _ _ : _)) = uncapitalize name

sortAlternativesByLength :: [Alternative] -> [Alternative]
sortAlternativesByLength = foldr insertByLength []
  where
    insertByLength alt [] = [alt]
    insertByLength alt (x:xs)
        | getLength alt >= getLength x = alt : x : xs
        | otherwise = x : insertByLength alt xs
    
    getLength (Alternative elems) = length elems

----------------------------------------------------
------ Parser and Type Definition Generators -------
----------------------------------------------------

-- Generates the parser code for a given element
elementParser :: Element -> String
elementParser (NonTerminal n) = uncapitalize n
elementParser (Terminal s) = "(string " ++ show s ++ ")"
elementParser (Macro IntMacro) = "int"
elementParser (Macro StringMacro) = "(some alpha)"
elementParser (Macro NewlineMacro) = "(is '\\n')"
elementParser (Modified mod elem) = modifiedParser mod elem
elementParser (ParameterRef param) = param
elementParser (ParameterizedCall name args) =
    "(" ++ uncapitalize name ++ " " ++ unwords (map elementParser args) ++ ")"

-- Gets the parser code for a given modifier and element
modifiedParser :: Modifier -> Element -> String
modifiedParser StarModifier e = "many (" ++ elementParser e ++ ")"
modifiedParser PlusModifier e = "some (" ++ elementParser e ++ ")"
modifiedParser QuestionModifier e = "optional (" ++ elementParser e ++ ")"
modifiedParser TokModifier e = "(tok " ++ elementParser e ++ ")"

-- Gets the Haskell type for a given element
elementType :: Element -> String
elementType (NonTerminal n) = capitalize n
elementType (Terminal _) = "String"
elementType (Macro IntMacro) = "Int"
elementType (Macro StringMacro) = "String"
elementType (Macro NewlineMacro) = "Char"
-- Modifiers
elementType (Modified StarModifier e) = "[" ++ elementType e ++ "]"
elementType (Modified PlusModifier e) = "[" ++ elementType e ++ "]"
elementType (Modified QuestionModifier e) = "Maybe " ++ elementType e
elementType (Modified QuestionModifier e) =
    case e of
        Macro IntMacro -> "Maybe Int"
        Macro StringMacro -> "Maybe String"
        Macro NewlineMacro -> "Maybe Char"
        Terminal _ -> "Maybe String"
        ParameterRef p -> "Maybe " ++ p
        _ -> "(Maybe " ++ elementType e ++ ")"
-- Parameterization
elementType (ParameterRef param) = param
elementType (ParameterizedCall name args) =
    "(" ++ capitalize name ++ " " ++ unwords (map elementTypeArg args) ++ ")"
  where
    -- Wrap complex types in parentheses
    elementTypeArg e@(ParameterizedCall _ _) = "(" ++ elementType e ++ ")"
    elementTypeArg e = elementType e


---------------------------------------
------- Haskell Code Generation -------
---------- Type Definitions -----------
---------------------------------------

-- Generates a Haskell type constructor for a given alternative
-- Example: Name: Expression, Alternative Index: 2, Alternative: [ NonTerminal "number", Terminal "+", NonTerminal "expression" ] to
-- Expression2 Number String Expression
generateConstructor :: String -> Int -> Alternative -> String
generateConstructor typeName index (Alternative elements) =
    typeName ++ show index ++ concatMap (\e -> " " ++ elementType e) elements --For each alternative, increment name by 1 (e.g., Expr1, Expr2, etc. ) and then list the types of its elements

-- Generates an Haskell data type with one or more alternatives, each with a unique name and constructor
-- Converts inputs:Name: expression, Alternatives: [ Alternative [ NonTerminal "term" ], Alternative [ NonTerminal "term", Terminal "+", NonTerminal "expression" ] ]
-- data Expression = Expression1 Term
--                 | Expression2 Term String Expression
--     deriving Show
generateData :: Rule -> String
generateData (Rule name params alts) =
    let sortedAlts = sortAlternativesByLength alts  -- Sort alternatives by length (longest first)
    in case sortedAlts of
        [] -> error "generateData: Invalid Alternatives"
        (first:rest) ->
            "data " ++ typeName ++ typeParams ++ " = " ++ firstConstructor ++ "\n" ++
            restConstructors ++
            "    deriving Show"
            where
                typeName = capitalize name
                typeParams = if null params then "" else " " ++ unwords params
                firstConstructor = generateConstructor typeName 1 first           --Generate the first constructor without the "|" operator
                restConstructors = concatMap
                    (\(i, alt) -> "          | " ++ generateConstructor typeName i alt ++ "\n")
                    (zip [2..] rest)                                             --For each alternative after the first, generate a constructor with the "|" operator prefixed onto it

-- Generates a Haskell newtype for a single alternative consisting of a single element
-- Convert "number", [ Alternative [ Macro IntMacro ] ] to
-- newtype Number = Number Int
--    deriving Show
generateNewType :: Rule -> String
generateNewType (Rule name params [Alternative [element]]) =
        "newtype " ++ typeName ++ typeParams ++ " = " ++ typeName ++
        " " ++ elementType element ++ "\n" ++
        "    deriving Show"
    where
        typeName = capitalize name
        typeParams = if null params then "" else " " ++ unwords params
generateNewType _ = error "generateNewType: Invalid Rule"

-- Generates a Haskell parameterized data type with one or more alternatives
-- Converts inputs:Name: list, Params: ["a"], Alternatives: [ Alternative [ ParameterRef "a" ], Alternative [ ParameterRef "a", Terminal ",", ParameterizedCall "list" ["a"] ] ]
-- data List a = List1 a
--              | List2 a String (List a)
--     deriving Show
generateParameterizedData :: String -> [String] -> [Alternative] -> String
generateParameterizedData name params alts =
    "data " ++ typeName ++ " " ++ typeParams ++ " = " ++ constructors ++ "\n" ++
    "    deriving Show"
    where
        typeName = capitalize name
        typeParams = unwords params
        constructors = case alts of
            [] -> error "generateParameterizedData: Invalid Alternatives"
            (first:rest) ->
                generateConstructor typeName 1 first ++
                concatMap (\(i, alt) -> "\n          | " ++ 
                    generateConstructor typeName i alt) 
                    (zip [2..] rest)

-- For a single rule, generate the corresponding Haskell type definition
-- Convert Rule "number" [ Alternative [ Macro IntMacro ] ] to
-- newtype Number = Number Int
--   deriving Show
-- or
-- Convert Rule "expression" [ Alternative [ Nonterminal "number" ], Alternative[ Nonterminal "number", Terminal "+", Nonterminal "expression" ] ] to the Haskell
-- data Expression = Expression1 Number
--                 | Expression2 Number String Expression
--   deriving Show
generateType :: Rule -> String
generateType rule@(Rule name params [Alternative elements])
    | null params && isSingle elements = generateNewType rule      -- Single alternative with single element -> newtype
    | otherwise = generateData rule                 -- Single alternative with multiple elements -> data

generateType rule = generateData rule               -- Multiple alternatives -> data

-- generateTypes takes a list of Grammar rules and produces Haskell type definitions as a String
-- Example Input:
-- Grammar 
--   [ Rule "number" 
--       [Alternative [Macro IntMacro]]
--   , Rule "expression"
--       [Alternative [Nonterminal "number"],
--        Alternative [Nonterminal "number", Terminal "+", Nonterminal "expression"]]
--   ]
-- Example Output:
-- newtype Number = Number Int
--     deriving Show
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
generateDataParser _ _ (Alternative []) =
    error "generateDataParser: Alternative is empty"
generateDataParser typeName index (Alternative (element:rest)) =
    typeName ++ show index ++ " <$> " ++ elementParser element ++ 
    concatMap (\e -> " <*> " ++ elementParser e) rest
    
-- Generates a Haskell parser function for a newtype alternative consisting of a single element
-- Example: Name: Number, Alternative: [ Macro IntMacro ] to
-- number = Number <$> int
generateNewTypeParser :: String -> Alternative -> String
generateNewTypeParser typeName (Alternative [element]) = 
        typeName ++ " <$> " ++ elementParser element
generateNewTypeParser _ _ = error "generateNewTypeParser: Invalid Alternative"

-- Generates parser code for all alternatives in a rule
-- Example: Name: Expression, Alternative Index: 2, Alternative: [ NonTerminal "number", Terminal "+", NonTerminal "expression" ] to
-- "Expression2 <$> number <*> (string \"+\") <*> expression"
generateAlternativeParser :: String -> Int -> Alternative -> String
generateAlternativeParser typeName index (Alternative [element]) =
    typeName ++ show index ++ " <$> " ++ elementParser element
generateAlternativeParser typeName index alternative =
    generateDataParser typeName index alternative

generateParameterizedParser :: String -> [String] -> [Alternative] -> String
generateParameterizedParser name params alternatives =
        funcName ++ " :: " ++ funcType ++ "\n" ++
        funcName ++ " " ++ paramNames ++ " = " ++ parserDef
    where
        funcName = uncapitalize name
        typeName = capitalize name
        paramNames = unwords params
        funcType = concatMap (\p -> "Parser " ++ p ++ " -> ") params ++
                   "Parser (" ++ typeName ++ " " ++ unwords params ++ ")"
        parserDef = generateAlternatives (Rule name params alternatives)

-- Generates parser code for all alternatives in a rule
-- Example: Name: Rule "Expression" Alternatives: [ Alternative [ NonTerminal "term" ], Alternative [ NonTerminal "term", Terminal "+", NonTerminal "expression" ] ] to
-- "Expression1 <$> term
--     <|> Expression2 <$> term <*> (string "+") <*> expression"
generateAlternatives :: Rule -> String
generateAlternatives (Rule name params [alternative@(Alternative elements)]) =
    if null params && isSingle elements then
        generateNewTypeParser (capitalize name) alternative
    else
        generateAlternativeParser (capitalize name) 1 alternative

generateAlternatives (Rule name params alternatives) =
    let sortedAlts = sortAlternativesByLength alternatives
    in case sortedAlts of
        [] -> error "generateAlternatives: No alternatives"
        (first:rest) ->
            generateAlternativeParser (capitalize name) 1 first ++
            concatMap
                (\(i, alt) -> "\n     <|> " ++ generateAlternativeParser (capitalize name) i alt)
                (zip [2..] rest)

-- Generates a Haskell parser function for a single rule
-- Example: Rule "number" [Alternative [Macro IntMacro]] to
-- number :: Parser Number
-- number = Number <$> int
generateParser :: Rule -> String
generateParser rule@(Rule name params alternatives) =
    if null params then
        uncapitalize name ++ " :: Parser " ++ typeName ++ "\n" ++
        uncapitalize name ++ " = " ++ alts
    else
        generateParameterizedParser name params alternatives
    where
        typeName = capitalize name
        alts = generateAlternatives rule


-- Generates parser functions for all rules
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


-- Generates Haskell code for a given parsed BNF Grammar.
-- Returns both the type definitions and parser functions
--
-- Input: ADT (parsed BNF Grammar)
-- Output Code: Haskell code as a string

-- Example BNF Grammar:
-- <number> ::= [int]
-- <expression> ::= <number> | <number> "+" <expression>
--
-- Example ADT:
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
generateHaskellCode adt =
    let validatedAdt@(Grammar rules) = cleanupGrammar adt
        typeDefs = generateTypes rules
        parserDefs = generateParsers rules
    in typeDefs ++ "\n" ++ parserDefs


-- | -------------------------------------------------
-- | ----------------- BNF parsers -------------------
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

-- Elements consist of nonterminals, terminals, macros with modifiers and parameterized types
element :: Parser Element
element = inlineSpaces *> elementParser <* inlineSpaces   -- discard surrounding spaces and parse the element
    where
        elementParser = tokElement <|> regElement

        tokElement = do
            _ <- string "tok"                               -- Check for 'tok' modifier BEFORE the element
            _ <- inlineSpaces                               -- Remove any spaces after 'tok'
            elem <- elementTypes
            return $ Modified TokModifier elem              -- Return the element wrapped in a TokModifier

        regElement = do
            elem <- paramRef <|> paramCall <|> regElements  -- Parse either a parameter type, parameterized element, or regular element
            modifier <- optional appendedModifiers             -- Then check for an optional modifier
            case modifier of
                Nothing -> return elem
                Just mod -> return $ Modified mod elem

        paramRef = do
            _ <- is '['                                 -- discard the opening square bracket
            param <- lower                              -- parse the parameter name
            _ <- is ']'                                 -- discard the closing square bracket
            return $ ParameterRef [param]              -- Return as ParameterRef (convert Char to String)

        paramCall = do
            _ <- is '<'                                 -- discard the opening angle bracket
            name <- some (alpha <|> digit <|> is '_')   -- parse the name of the nonterminal
            _ <- is '('                                 -- discard the opening parenthesis
            args <- argumentList                        -- parse the list of arguments
            _ <- is ')'                                 -- discard the closing parenthesis
            _ <- is '>'                                 -- discard the closing angle bracket
            return $ ParameterizedCall name args

        argumentList = do
            first <- inlineSpaces *> element <* inlineSpaces
            rest <- many (is ',' *> inlineSpaces *> element <* inlineSpaces)
            return (first : rest)

        elementTypes = regElements  -- The basic element types (no modifiers except tok)
        regElements = ntElement <|> tElement <|> mElement  -- Attempts each of the basic element types
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
    rest <- many (inlineSpaces *> is '|' *> inlineSpaces *> alternative)          -- used many to allow zero or more additional alternatives if more available
    return $ first : rest

-- Parse the full rule (<name> ::= <alternatives>) or parameterized rule (<name(a,b)> ::= <alternatives>)
rule :: Parser Rule
rule = parameterizedRule <|> simpleRule
    where
        simpleRule = Rule
            <$> nonterminal
            <*> pure []
            <*> (inlineSpaces *> string "::=" *> inlineSpaces *> alternatives)

        parameterizedRule = Rule
            <$> (is '<' *> some (alpha <|> digit <|> is '_') <* is '(')
            <*> (paramList <* is ')' <* is '>')
            <*> (inlineSpaces *> string "::=" *> inlineSpaces *> alternatives)

        paramList = (\first rest -> map (:[]) (first : rest))
            <$> (inlineSpaces *> lower)
            <*> (many (inlineSpaces *> is ',' *> inlineSpaces *> lower) <* inlineSpaces)

  -- Parser for the BNF grammar
bnfParser :: Parser ADT
bnfParser = do
        rules <- many ruleParser    -- Parse multiple rules
        _ <- spaces                 -- Discard any trailing spaces
        _ <- eof                    -- Ensure we reach the end of input
        return $ Grammar rules
    where
        ruleParser = spaces *> rule <* spaces   -- Parse a rule with surrounding spaces (or blank)

-- | -------------------------------------------------
-- | -------------- Modifier Parsers -----------------
-- | -------------------------------------------------
-- Attempts to pattern match the next char and returns the appropriate type of Modifier
appendedModifiers :: Parser Modifier
appendedModifiers =
    (is '*' >> return StarModifier) <|>
    (is '+' >> return PlusModifier) <|>
    (is '?' >> return QuestionModifier)

-- | -------------------------------------------------
-- | -------- File output + Save File ----------------
-- | -------------------------------------------------
generateHaskellCodeWithTimestamp :: ADT -> IO String
generateHaskellCodeWithTimestamp adt = do
    timestamp <- getTime
    let timestampComment = "-- Generated on: " ++ timestamp ++ "\n\n"
    let code = generateHaskellCode adt
    return $ timestampComment ++ code

-- | -------------------------------------------------
-- | -------------- Validation -----------------------
-- | -------------------------------------------------

-- Get list of all rule names from grammar
getRuleNames :: ADT -> [String]
getRuleNames (Grammar rs) = [n | Rule n _ _ <- rs]

-- Extract nonterminals from list of elements
getNonterminalRefs :: [Element] -> [String]
getNonterminalRefs elems = [n | NonTerminal n <- elems] ++
                           concatMap (getNonterminalRefs . (:[])) [e | Modified _ e <- elems] ++
                           [n | ParameterizedCall n _ <- elems]

-- Get all nonterminals used in a rule
getRefs :: Rule -> [String]
getRefs (Rule _ _ alts) = concatMap getRefs' alts
  where
    getRefs' (Alternative elems) = getNonterminalRefs elems

-- Find nonterminals that are used but never defined
findUndefined :: ADT -> [String]
findUndefined adt@(Grammar rs) =
    ["Undefined nonterminal: " ++ ref | ref <- used, ref `notElem` defined]
  where
    defined = getRuleNames adt
    used = concatMap getRefs rs

-- Find rules that are defined more than once
findDuplicates :: ADT -> [String]
findDuplicates (Grammar rs) = findDups (getRuleNames (Grammar rs))
  where
    findDups [] = []
    findDups (x:xs)
        | x `elem` xs && x `notElem` findDups xs = ("Duplicate rule: " ++ x) : findDups xs
        | otherwise = findDups xs

-- Check if rules have left recursion (direct or through cycle)
findLeftRecursive :: ADT -> [String]
findLeftRecursive adt@(Grammar rs) =
    ["Left recursion in: " ++ n | n <- getRuleNames adt, hasLeftRec n]
  where
    -- Check if rule is left recursive
    hasLeftRec :: String -> Bool
    hasLeftRec start = checkForCycle start [start]

    -- Check if we can reach any visited rule through leftmost nonterminals
    checkForCycle :: String -> [String] -> Bool
    checkForCycle curr visited =
        case lookup curr rulesMap of
            Nothing -> False
            Just leftNTs ->
                any (\nt -> nt `elem` visited || checkForCycle nt (nt:visited)) leftNTs

    -- Map each rule to its leftmost nonterminals
    rulesMap :: [(String, [String])]
    rulesMap = [(n, getLeftmost r) | r@(Rule n _ _) <- rs]

    -- Get leftmost nonterminal from each alternative (if it exists)
    getLeftmost :: Rule -> [String]
    getLeftmost (Rule _ _ alts) = concatMap getFirst alts
      where
        getFirst (Alternative []) = []
        getFirst (Alternative (NonTerminal nt : _)) = [nt]
        getFirst (Alternative (ParameterizedCall nt _ : _)) = [nt]
        getFirst (Alternative (Modified _ e : _)) = getFromElem e
        getFirst _ = []

        getFromElem (NonTerminal nt) = [nt]
        getFromElem (ParameterizedCall nt _) = [nt]
        getFromElem _ = []

-- Remove duplicate rules (keep first occurrence)
removeDups :: ADT -> ADT
removeDups (Grammar rs) = Grammar (go [] rs)
  where
    go _ [] = []
    go seen (r@(Rule n _ _):rest)
        | n `elem` seen = go seen rest
        | otherwise = r : go (n:seen) rest

-- Remove rules that have issues
removeInvalid :: ADT -> [String] -> ADT
removeInvalid (Grammar rs) warns = Grammar (filter ok rs)
  where
    ok (Rule n _ _) = not (any (matches n) warns)
    matches n w = ("Left recursion in: " ++ n) == w || ("Undefined nonterminal: " ++ n) == w

-- Keep validating until there's no more issues to fix
cleanupGrammar :: ADT -> ADT
cleanupGrammar adt =
    let dups = findDuplicates adt
        noDups = removeDups adt
        leftRec = findLeftRecursive noDups
        undef = findUndefined noDups
        allIssues = leftRec ++ undef
        cleaned = removeInvalid noDups allIssues
    in if null allIssues
       then cleaned
       else cleanupGrammar cleaned

-- Return warnings from first iteration only
validate :: ADT -> [String]
validate adt =
    let dups = findDuplicates adt
        noDups = removeDups adt  -- need to remove dups first
        leftRec = findLeftRecursive noDups
        undef = findUndefined noDups
    in dups ++ leftRec ++ undef

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime