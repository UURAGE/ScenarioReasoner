{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Parser where

import Control.Monad

import Data.Char
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import GHC.Exts(groupWith)
import Text.Read(readMaybe)
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
        { scenarioMetaData     = parseMetaData defs script
        , scenarioTopDialogue  = parseDialogue defs script
        }
  where defs = parseDefinitions script

type Definitions = M.Map String DD.Type

parseDefinitions :: Script -> Definitions
parseDefinitions script = fromMaybe (error "Definitions not found") $
        M.fromList . map parseDefinition . children <$> propertiesElem
  where propertiesElem =
            findChild "metadata" script >>=
            findChild "definitions" >>=
            findChild "properties"

parseDefinition :: Element -> (String, DD.Type)
parseDefinition propEl = (getAttribute "id" propEl, parseDomainDataType (head (children propEl)))

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
        { scenarioName            = parseScenarioName                 script
        , scenarioDescription     = parseScenarioDescription          script
        , scenarioDifficulty      = parseScenarioDifficulty           script
        , scenarioParameters      = parseScenarioParameters           script
        , scenarioPropertyValues  = parseScenarioPropertyValues  defs script
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

-- | Queries the given script for its parameters
parseScenarioParameters :: Script -> [Parameter]
parseScenarioParameters script = map parseParameter (children parameterElem)
  where
    metaDataElem  = getChild "metadata" script
    definitionsElem = getChild "definitions" metaDataElem
    parameterElem = getChild "userDefined" (getChild "parameters" definitionsElem)

    -- | Parses a parameter Element inside the parameters inside the metadata of the script
    parseParameter :: Element -> Parameter
    parseParameter paramElem = Parameter
        { parameterId           = getAttribute "id" paramElem
        , parameterName         = getAttribute "name" paramElem
        , parameterInitialValue = findAttribute "initialValue" paramElem >>= readMaybe :: Maybe ParameterValue
        , parameterDescription  = fromMaybe "" (findAttribute "description" paramElem)
        , parameterMax          = findAttribute "maximum" paramElem >>= readMaybe :: Maybe ParameterValue
        , parameterMin          = findAttribute "minimum" paramElem >>= readMaybe :: Maybe ParameterValue
        }

parseScenarioPropertyValues :: Definitions -> Script -> PropertyValues
parseScenarioPropertyValues defs script = fromMaybe (PropertyValues (Assocs []) (Assocs [])) $
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
    { statID             = getAttribute "id"         statElem
    , statInfo           = parseStatementInfo defs   statElem
    , statPrecondition   = parseMaybePrecondition    statElem
    , statParamEffects   = parseParameterEffects     statElem
    , statJumpPoint      = parseJumpPoint            statElem
    , statInits          = parseInits                statElem
    , statEnd            = parseEnd                  statElem
    , statNextStatIDs    = parseNextStatIDs          statElem
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
parseMaybePrecondition :: Element -> Maybe Condition
parseMaybePrecondition statElem =
    fmap (parseCondition . getExactlyOneChild) conditionElem
      where conditionElem = findChild "preconditions" statElem

-- | Takes a statement element and returns its effects
parseParameterEffects :: Element -> [Effect]
parseParameterEffects statElem = map parseParameterEffect paramElems
  where parentElem = findChild "parameterEffects" statElem >>= findChild "userDefined"
        paramElems = emptyOnFail (children <$> parentElem)

parseParameterEffect :: Element -> Effect
parseParameterEffect effectElem = Effect
            { effectIdref        = getAttribute "idref" effectElem
            , effectAssignmentOp = parseAssignmentOperator      effectElem
            , effectValue        = getValue             effectElem
            }

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
parseCondition :: Element -> Condition
parseCondition conditionElem = case name conditionElem of
    "and"       -> And (map parseCondition (children conditionElem))
    "or"        -> Or  (map parseCondition (children conditionElem))
    "condition" -> Condition
        ComparisonCondition
        { conditionIdref = getAttribute "idref" conditionElem
        , conditionTest  = parseCompareOperator conditionElem
        , conditionValue = getValue             conditionElem
        }
    _           -> error "no parse condition"

-- | Parses a compare operator. Gives an exception on invalid input.
parseCompareOperator :: Element -> CompareOperator
parseCompareOperator conditionElem = read (applyToFirst toUpper (getAttribute "test"  conditionElem))

-- | Parses property values from an element that has them
parsePropertyValues :: Definitions -> Element -> PropertyValues
parsePropertyValues defs propsElem = PropertyValues
    { propValsIndependent = Assocs civs
    , propValsPerCharacter = Assocs (map toPC (groupWith fst pcvs))
    }
  where
    propVals = map (parsePropertyValue defs) (children (getChild "propertyValues" propsElem))
    (civs, pcvs) = partitionEithers propVals
    toPC vs = (fst (head vs), Assocs (map snd vs))

parsePropertyValue :: Definitions -> Element -> Either (String, DD.Value) (String, (String, DD.Value))
parsePropertyValue defs propValEl = case mCharacteridref of
    Just characteridref -> Right (characteridref, (idref, value))
    Nothing             -> Left (idref, value)
  where
    mCharacteridref = findAttribute "characteridref" propValEl
    idref = getAttribute "idref" propValEl
    errorDefault = error ("Value for unknown property " ++ idref)
    value = case M.findWithDefault errorDefault idref defs of
        DD.TBoolean     -> DD.VBoolean (read (getData propValEl))
        DD.TInteger     -> DD.VInteger (read (getData propValEl))
        DD.TString      -> DD.VString  (getData propValEl)

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

-- | Parses a value attribute from an element
getValue :: Element -> ParameterValue
getValue el = read (getAttribute "value" el) :: ParameterValue
