{-# LANGUAGE OverloadedStrings #-}

-- | Main module for launching the web server that handles parsing using user-defined BNF grammars.
-- This server exposes an API that accepts a grammar, generates corresponding Haskell code,
-- compiles it, and runs a selected parser function on a given input string.
module Main (main) where

import Assignment (bnfParser, generateHaskellCode, validate)
import Control.Concurrent.QSemN (QSemN, newQSemN, signalQSemN, waitQSemN)
import Control.Exception (bracket_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Key (fromString)
import Data.Char (isLower)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Text.Lazy (Text, pack, unpack)
import Instances (ParseResult (..), Parser (..), parse)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeBaseName, takeDirectory, (</>))
import Unsafe.Coerce (unsafeCoerce)
import Web.Scotty (ActionM, formParam, json, post, scotty)

-- GHC API imports for dynamic compilation and reflection
-- Very Scary code.

import GHC
    ( GeneralFlag (Opt_BuildDynamicToo)
    , Ghc
    , GhcLink (LinkInMemory)
    , GhcMonad
    , InteractiveImport (IIDecl)
    , LoadHowMuch (LoadAllTargets)
    , Module
    , ModuleName (ModuleName)
    , RdrName (Qual)
    , compileParsedExpr
    , getModSummary
    , getModuleInfo
    , getSessionDynFlags
    , ghcLink
    , guessTarget
    , importPaths
    , load
    , mkHsString
    , mkModuleName
    , modInfoExports
    , ms_mod
    , nlHsApp
    , nlHsLit
    , nlHsVar
    , parseImportDecl
    , runGhc
    , setContext
    , setSessionDynFlags
    , setTargets
    )
import GHC.Driver.Session (gopt_set)
import GHC.Paths (libdir)
import GHC.Types.Name (getOccString, mkVarOcc)

-- #### BEGIN SCARY CODE ####

--------------------------------------------------------------------------------

-- | Get names of all exported (lowercase) functions from a given module
getCompiledFunctions :: FilePath -> IO [String]
getCompiledFunctions fileName = runGhc (Just libdir) $ do
    modSums <- initSession [fileName]
    maybe (return []) listExports $ listToMaybe modSums

--------------------------------------------------------------------------------

-- | Dynamically compile and run a parser function with the given input string.
runFunctionAgainstInput :: FilePath -> String -> String -> IO String
runFunctionAgainstInput _ "" _ = pure ""
runFunctionAgainstInput _ _ "" = pure ""
runFunctionAgainstInput fileName functionName inputString = runGhc (Just libdir) $ do
    _ <- initSession [fileName]

    let moduleName = takeBaseName fileName
    importDecl_RdrName <-
        parseImportDecl $ "import qualified " ++ moduleName ++ " as F"
    setContext [IIDecl importDecl_RdrName]

    hval <-
        compileParsedExpr $
            nlHsApp
                ( nlHsApp
                    (nlHsVar $ Qual (ModuleName "F") (mkVarOcc "runParser"))
                    (nlHsVar $ Qual (ModuleName "F") (mkVarOcc functionName))
                )
                (nlHsLit (mkHsString inputString))

    return (unsafeCoerce hval :: String)

--------------------------------------------------------------------------------

-- | Initializes GHC session and compiles the given files
initSession :: [FilePath] -> Ghc [Module]
initSession files = do
    dflags <- getSessionDynFlags

    -- I do not know what this all means. But I am scared - probably do not touch this.
    -- let ghcPkgFlag = ExposePackage "ghc" (PackageArg "ghc") (ModRenaming True [])
    let dflags' =
            dflags
                { -- packageFlags = ghcPkgFlag : packageFlags dflags,
                  ghcLink = LinkInMemory
                , importPaths = "src" : importPaths dflags
                }
    let dflags'' = gopt_set dflags' Opt_BuildDynamicToo

    _ <- setSessionDynFlags dflags''

    let guessTarget' s = guessTarget s Nothing Nothing
    targets <- mapM (guessTarget' . ("*" ++)) files

    setTargets targets
    _ <- load LoadAllTargets

    mapM ((ms_mod <$>) . getModSummary . mkModuleName) (takeBaseName <$> files)

-- | List exported names of this or a sibling module
listExports :: GhcMonad m => Module -> m [String]
listExports modules = do
    maybeModInfo <- getModuleInfo modules
    case maybeModInfo of
        (Just modInfo) ->
            pure $
                filter
                    (maybe False isLower . listToMaybe)
                    (map getOccString (modInfoExports modInfo))
        _ -> pure []

-- #### END SCARY CODE ####

--------------------------------------------------------------------------------

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
    json $
        object
            [ fromString key .= (pack value :: Text) | (key, value) <- pairs
            ]

--------------------------------------------------------------------------------

-- | Run something with a lock, ensuring only one instance of it is running at a time
withLock :: QSemN -> IO a -> IO a
withLock sem = bracket_ (waitQSemN sem 1) (signalQSemN sem 1)

-- | Entry point – launches Scotty web server and defines API route
main :: IO ()
main = newQSemN 1 >>= \sem -> scotty 3000 $ do
    post "/api/generate" $ do
        -- Extract POST parameters
        grammar <- formParam "grammar" :: ActionM Text
        string <- formParam "string" :: ActionM Text
        selectedParser <- formParam "selectedParser" :: ActionM Text

        -- Parse BNF grammar and generate Haskell code
        case parse bnfParser (unpack grammar) of
            r@(Error _) -> jsonResponse [("error", show r)]
            Result _ bnf -> do
                (fs, result) <- liftIO . withLock sem $ do
                  -- Read parser template and generate Haskell source file
                  template <- liftIO $ readFile ("template" </> "BNFParser.template")
                  let outputFile = "output" </> "Output.hs"
                  liftIO $ createDirectoryIfMissing True (takeDirectory outputFile)
                  liftIO $ writeFile outputFile $ template ++ "\n" ++ generateHaskellCode bnf

                  -- Compile the generated module and run selected parser
                  fs <- liftIO $ filter (/= "runParser") <$> getCompiledFunctions outputFile
                  result <-
                      liftIO $
                          runFunctionAgainstInput outputFile (unpack selectedParser) (unpack string)
                  return (fs, result)

                -- Return list of available parsers and the result
                let warnings = validate bnf
                jsonResponse
                    [ ("parsers", intercalate "," fs)
                    , ("result", result)
                    , ("warnings", intercalate "\n" warnings)
                    ]
