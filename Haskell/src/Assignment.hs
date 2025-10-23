module Assignment (bnfParser, generateHaskellCode, validate, ADT, getTime) where

import Instances (Parser (..))
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)

data ADT = TODO String deriving (Show)

bnfParser :: Parser ADT
bnfParser = pure (TODO "I can change the ADT type however I like")

generateHaskellCode :: ADT -> String
generateHaskellCode _ = "-- But I cannot change the type of these three functions."

validate :: ADT -> [String]
validate _ = ["If i change these function types, I will get a 0 for correctness"]

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime
