{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Parser where

import Control.Monad

import Data.Char
import Data.Either
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe
import GHC.Exts(groupWith)
import System.IO

import Ideas.Common.Library hiding (Sum)
import Ideas.Text.XML.Interface

import Domain.Scenarios.Condition
import Domain.Scenarios.ScenarioState
import Domain.Scenarios.Globals
import Domain.Scenarios.Scenario
import qualified Domain.Scenarios.DomainData as DD

type Script = Element

-- Functions to be exposed as an interface
----------------------------------------------------------------------------------------------------

-- | Parses the XML at the path to a Script
parseScript :: FilePath -> IO Script
parseScript filepath = withBinaryFile filepath ReadMode
        (hGetContents >=> (either fail return . parseXML))
        -- if parameter is Left a, do fail a, if it is Right b do (return . Script) . parseXML b

-- | Parses a scenario from a script element
parseScenario :: Script -> Scenario
parseScenario script = Scenario
        { scenarioDefinitions  = defs
        , scenarioMetaData     = parseMetaData defs script
        , scenarioTopDialogue  = parseDialogue defs script
        }
  where defs = parseDefinitions script

parseDefinitions :: Script -> Definitions
parseDefinitions script = Definitions
        { definitionsProperties = parseDefinitionList (getChild "properties" defsEl)
        , definitionsParameters = (fmap fst paramDefs, F.fold (fmap snd paramDefs))
        }
  where paramDefs = Usered
            { useredUserDefined = parseDefinitionList (getChild "userDefined" paramDefsEl)
            , useredFixed = parseDefinitionList (getChild "fixed" paramDefsEl)
            }
        paramDefsEl = getChild "parameters" defsEl
        defsEl = fromMaybe (error "Definitions not found") $
            findChild "definitions" script

parseDefinitionList :: Element -> ([Definition], TypeMap)
parseDefinitionList el = (defs, M.fromList (map toTypePair defs))
  where toTypePair def = (definitionId def, definitionType def)
        defs = map parseDefinition (children el)

parseDefinition :: Element -> Definition
parseDefinition defEl = Definition
        { definitionId           = getAttribute "id" defEl
        , definitionName         = getAttribute "name" defEl
        , definitionDescription  = getData <$> findChild "description" defEl
        , definitionType         = ty
        , definitionDefault      = parseDomainDataValue ty <$> maybeDefaultEl
        }
  where ty = parseDomainDataType typeEl
        typeEl = last (children defEl)
        maybeDefaultEl = findChild "default" typeEl

parseDomainDataType :: Element -> DD.Type
parseDomainDataType typeEl = case name typeEl of
    "typeBoolean" -> DD.TBoolean
    "typeInteger" -> DD.TInteger
    "typeString" -> DD.TString
    "typeEnumeration" -> DD.TString
    n -> error ("Could not parse " ++ n)

----------------------------------------------------------------------------------------------------

-- Functions to be used internally
----------------------------------------------------------------------------------------------------

-- MetaData Parser ---------------------------------------------------------------------------------

parseMetaData :: Definitions -> Script -> MetaData
parseMetaData defs script = MetaData
        { scenarioName                   = parseScenarioName                        script
        , scenarioDescription            = parseScenarioDescription                 script
        , scenarioDifficulty             = parseScenarioDifficulty                  script
        , scenarioInitialParameterValues = parseScenarioInitialParameterValues defs script
        , scenarioPropertyValues         = parseScenarioPropertyValues         defs script
        }

-- | Queries the given script for its name
parseScenarioName :: Script -> Name
parseScenarioName = getMetaDataString "name"

-- | Queries the given script for its description
parseScenarioDescription :: Script -> String
parseScenarioDescription = getMetaDataString "description"

-- | Queries the given script for its difficulty
parseScenarioDifficulty :: Script -> Maybe Difficulty
parseScenarioDifficulty script = readDifficulty difficultyString
 where
    difficultyString = getMetaDataString "difficulty" script

parseScenarioInitialParameterValues :: Definitions -> Script -> ParameterState
parseScenarioInitialParameterValues defs script = Usered
    { useredUserDefined = parseCharactereds valueParser M.fromList (getChild "userDefined" valsElem)
    , useredFixed = parseCharactereds valueParser M.fromList (getChild "fixed" valsElem)
    }
  where valsElem = fromMaybe (error "Initial parameter values not found") $
            findChild "metadata" script >>=
            findChild "initialParameterValues"
        valueParser = parseNamedDomainDataValue "parameter" (snd (definitionsParameters defs))

parseScenarioPropertyValues :: Definitions -> Script -> PropertyValues
parseScenarioPropertyValues defs script = fromMaybe (Charactered (Assocs []) M.empty) $
    parsePropertyValues defs <$> findChild "metadata" script

-- MetaData Parser END -----------------------------------------------------------------------------

-- Dialogue Parser ---------------------------------------------------------------------------------

parseDialogue :: Definitions -> Script -> TopDialogue
parseDialogue defs script = map (parseInterleaveLevel defs) interleaveElems
  where
    sequenceElem = getChild "sequence" script
    interleaveElems = findChildren "interleave" sequenceElem

parseInterleaveLevel :: Definitions -> Element -> InterleaveLevel
parseInterleaveLevel defs interleaveElem = map (parseTree defs) diaElems
  where
    diaElems = findChildren "dialogue" interleaveElem

parseTree :: Definitions -> Element -> Dialogue
parseTree defs diaElem =
    Dialogue
    { diaID         = getAttribute "id" diaElem
    , diaStartIDs   = map (getAttribute "idref") (children (getChild "starts" diaElem))
    , diaAtomic     = not (any statJumpPoint statements)
    , diaOptional   = tryParseBool (findAttribute "optional" diaElem)
    , diaStatements = statements
    }
  where statements = map (parseStatement defs) (children (getChild "statements" diaElem))

parseStatement :: Definitions -> Element -> Statement
parseStatement defs statElem =
    Statement
    { statID             = getAttribute "id"           statElem
    , statInfo           = parseStatementInfo defs     statElem
    , statPrecondition   = parseMaybePrecondition defs statElem
    , statParamEffects   = parseParameterEffects defs  statElem
    , statJumpPoint      = parseJumpPoint              statElem
    , statInits          = parseInits                  statElem
    , statEnd            = parseEnd                    statElem
    , statNextStatIDs    = parseNextStatIDs            statElem
    }

parseStatementInfo :: Definitions -> Element -> StatementInfo
parseStatementInfo defs statElem =
    StatementInfo
    {   statType           = parseType                statElem
    ,   statText           = parseText                statElem
    ,   statPropertyValues = parsePropertyValues defs statElem
    }

-- | Takes a statement and returns its type
parseType :: Element -> StatementType
parseType statElem = takeWhile isLower (name statElem)

-- | Takes a statement and returns its text
parseText :: Element -> StatementText
parseText statElem = getData (getChild "text" statElem)

-- | Takes a statement element and returns its precondition, if it has one
parseMaybePrecondition :: Definitions -> Element -> Maybe Condition
parseMaybePrecondition defs statElem =
    fmap (parseCondition defs . getExactlyOneChild) conditionElem
      where conditionElem = findChild "preconditions" statElem

-- | Takes a statement element and returns its effects
parseParameterEffects :: Definitions -> Element -> Usered (Charactered [Effect])
parseParameterEffects defs statElem = Usered
    { useredUserDefined = Charactered
        (map (parseParameterEffect defs) (children (getChild "userDefined" effectsElem)))
        M.empty
    , useredFixed = parseCharactereds (parseParameterEffect defs) id (getChild "fixed" effectsElem)
    }
  where effectsElem = getChild "parameterEffects" statElem

parseParameterEffect :: Definitions -> Element -> Effect
parseParameterEffect defs effectElem = Effect
    { effectIdref        = idref
    , effectAssignmentOp = parseAssignmentOperator effectElem
    , effectValue        = value
    }
  where idref = getAttribute "idref" effectElem
        errorDefault = error ("Value for unknown parameter " ++ idref)
        value = parseDomainDataValue
            (M.findWithDefault errorDefault idref (snd (definitionsParameters defs)))
            effectElem

-- | Parses an element to a Changetype
parseAssignmentOperator :: Element -> AssignmentOperator
parseAssignmentOperator effectElem = read (applyToFirst toUpper operatorStr)
  where operatorStr = getAttribute "operator" effectElem

parseJumpPoint :: Element -> Bool
parseJumpPoint statElem = tryParseBool (findAttribute "jumpPoint" statElem)

parseInits :: Element -> Bool
parseInits statElem = tryParseBool (findAttribute "inits" statElem)

parseEnd :: Element -> Bool
parseEnd statElem = tryParseBool (findAttribute "end" statElem)

-- | Takes a statement and returns the IDs of the statements following it
parseNextStatIDs :: Element -> [ID]
parseNextStatIDs element = errorOnFail errorMsg nextIDs
  where
    errorMsg = "Failed to get the nextIDs of: " ++ name element
    nextIDs = getResponses >>= getIdrefs
      where getIdrefs = mapM (findAttribute "idref")
            getResponses = children <$> findChild "responses" element

-- Dialogue Parser END -----------------------------------------------------------------------------

-- | Parses a Bool
parseBool :: String -> Bool
parseBool boolStr = read (applyToFirst toUpper boolStr) :: Bool

-- | Tries to parse bool from a string
tryParseBool :: Maybe String -> Bool
tryParseBool (Just boolStr) = parseBool boolStr
tryParseBool _              = False

-- | Parses a condition and recursively parses ands and ors. Used in both parsers (metadata and dialogue)
parseCondition :: Definitions -> Element -> Condition
parseCondition defs conditionElem = case name conditionElem of
    "and"       -> And (map (parseCondition defs) (children conditionElem))
    "or"        -> Or  (map (parseCondition defs) (children conditionElem))
    "condition" -> Condition
        ComparisonCondition
        { conditionIdref          = idref
        , conditionCharacterIdref = findAttribute "characteridref" conditionElem
        , conditionTest           = parseCompareOperator conditionElem
        , conditionValue          = value
        }
      where idref = getAttribute "idref" conditionElem
            errorDefault = error ("Condition for unknown parameter " ++ idref)
            ty = M.findWithDefault errorDefault idref (snd (definitionsParameters defs))
            value = parseDomainDataValue ty conditionElem
    _           -> error "no parse condition"

-- | Parses a compare operator. Gives an exception on invalid input.
parseCompareOperator :: Element -> CompareOperator
parseCompareOperator conditionElem = read (applyToFirst toUpper (getAttribute "operator" conditionElem))

-- | Parses property values from an element that has them
parsePropertyValues :: Definitions -> Element -> PropertyValues
parsePropertyValues defs propsElem = parseCharactereds
    (parseNamedDomainDataValue "property" (snd (definitionsProperties defs)))
    Assocs (getChild "propertyValues" propsElem)

parseNamedDomainDataValue :: String -> TypeMap -> Element -> (String, DD.Value)
parseNamedDomainDataValue valueKind typeMap propValEl = (idref, value)
  where
    idref = getAttribute "idref" propValEl
    errorDefault = error ("Value for unknown " ++ valueKind ++ " " ++ idref)
    value = parseDomainDataValue
      (M.findWithDefault errorDefault idref typeMap)
      propValEl

parseDomainDataValue :: DD.Type -> Element -> DD.Value
parseDomainDataValue ty el = case ty of
    DD.TBoolean     -> DD.VBoolean (read (applyToFirst toUpper (getData el)))
    DD.TInteger     -> DD.VInteger (read (getData el))
    DD.TString      -> DD.VString  (getData el)

parseCharactereds :: (Element -> a) -> ([a] -> b) -> Element -> Charactered b
parseCharactereds parseSub mkCollection valsElem = Charactered
    { characteredIndependent = mkCollection civs
    , characteredPerCharacter = M.fromList (map toPC (groupWith fst pcvs))
    }
  where
    vals = map (parseCharactered parseSub) (children valsElem)
    (civs, pcvs) = partitionEithers vals
    toPC vs = (fst (head vs), mkCollection (map snd vs))

parseCharactered :: (Element -> a) -> Element -> Either a (String, a)
parseCharactered parseSub valEl = case findAttribute "characteridref" valEl of
    Just characteridref -> Right (characteridref, val)
    Nothing             -> Left val
  where
    val = parseSub valEl

-- Functions that extend the XML parser
----------------------------------------------------------------------------------------------------

-- | Returns the child element with the given name out of the Monad defined in the framework
getChild :: Name -> Element -> Element
getChild elemName element = errorOnFail errorMsg mChild
  where
    errorMsg = "Failed to find child: " ++ elemName
    mChild = findChild elemName element

-- | Finds an attribute and gets it out of the Monad defined in the framework
getAttribute :: String -> Element -> String
getAttribute attributeName element = errorOnFail errorMsg mAttribute
  where
    errorMsg = "Failed to find attribute: " ++ attributeName
    mAttribute = findAttribute attributeName element

getExactlyOneChild :: Element -> Element
getExactlyOneChild element = case children element of
    []      -> error "no children found"
    [child] -> child
    _       -> error "multiple children found"

-- | Queries the given script for basic information. Which information being queried is specified
-- in the "metaDataName". This could be the name of the script, the difficulty etc.
getMetaDataString :: Name -> Script -> String
getMetaDataString metaDataName script = getData dataElem
  where
    metadata = getChild "metadata" script
    dataElem = getChild metaDataName metadata
