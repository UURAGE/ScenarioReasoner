{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Parser where

import Control.Monad

import Data.Char
import Data.Either
import qualified Data.Foldable as F
import Data.List
import qualified Data.Map as M
import Data.Maybe
import GHC.Exts(groupWith)
import System.IO

import Ideas.Common.Library hiding (Sum)
import Ideas.Text.XML.Interface

import Domain.Scenarios.Condition
import Domain.Scenarios.Expression
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
        , scenarioExpressions  = parseExpressions defs script
        , scenarioMetaData     = parseMetaData defs script
        , scenarioTopDialogue  = parseDialogue defs script
        }
  where defs = parseDefinitions script

parseDefinitions :: Script -> Definitions
parseDefinitions script = Definitions
        { definitionsCharacters = map parseCharacterDefinition (children (getChild "characters" defsEl))
        , definitionsProperties = parseDefinitionList (getChild "properties" defsEl)
        , definitionsParameters = (fmap fst paramDefs, F.fold (fmap snd paramDefs))
        }
  where paramDefs = Usered
            { useredUserDefined = parseDefinitionList (getChild "userDefined" paramDefsEl)
            , useredFixed = parseDefinitionList (getChild "fixed" paramDefsEl)
            }
        paramDefsEl = getChild "parameters" defsEl
        defsEl = fromMaybe (error "Definitions not found") $
            findChild "definitions" script

parseDefinitionList :: Element -> ([Definition ()], TypeMap)
parseDefinitionList el = (defs, M.fromList (map toTypePair defs))
  where toTypePair def = (definitionId def, definitionType def)
        defs = map (parseDefinition (const (const ()))) (children el)

parseDefinition :: (DD.Type -> Element -> a) -> Element -> Definition a
parseDefinition parseContent defEl = Definition
        { definitionId           = getAttribute "id" defEl
        , definitionName         = getAttribute "name" defEl
        , definitionDescription  = getData <$> findChild "description" defEl
        , definitionType         = ty
        , definitionDefault      = parseDomainDataValue ty <$> maybeDefaultEl
        , definitionContent      = parseContent ty defEl
        }
  where (ty, maybeDefaultEl) = parseDomainDataType (getChild "type" defEl)

parseCharacterDefinition :: Element -> CharacterDefinition
parseCharacterDefinition defEl = CharacterDefinition
        { characterDefinitionId   = getAttribute "id" defEl
        , characterDefinitionName = findAttribute "name" defEl
        }

parseDomainDataType :: Element -> (DD.Type, Maybe Element)
parseDomainDataType typeContainerEl = case name typeEl of
    "list" -> (DD.TList (fst (parseDomainDataType (getChild "itemType" typeEl))), simpleDefault)
    "attributeRecord" ->
        ( DD.TAttributeRecord
            (processItem <$> findChild "content" typeEl)
            (map processItem (findChildren "attribute" typeEl))
        , simpleDefault)
      where processItem itemEl =
                ( getAttribute "name" itemEl
                , fst (parseDomainDataType (getChild "type" itemEl))
                )
    "extension" -> parseDomainDataType (getChild "equivalentType" typeEl)
    _ -> (DD.TSimple (parseSimpleDomainDataType typeEl), simpleDefault)
  where typeEl = getExactlyOneChild typeContainerEl
        simpleDefault = findChild "default" typeEl

parseSimpleDomainDataType :: Element -> DD.SimpleType
parseSimpleDomainDataType typeEl = case name typeEl of
    "boolean" -> DD.TBoolean
    "integer" -> DD.TInteger mmin mmax
      where mmin = read <$> findAttribute "minimum" typeEl
            mmax = read <$> findAttribute "maximum" typeEl
    "string" -> DD.TString
    "enumeration" -> DD.TString
    n -> error ("Could not parse " ++ n)

----------------------------------------------------------------------------------------------------

-- Functions to be used internally
----------------------------------------------------------------------------------------------------

parseExpressions :: Definitions -> Element -> [Definition Expression]
parseExpressions defs = maybe [] (map (parseDefinition parseExpression) . children) .
    findChild "typedExpressions"
  where parseExpression ty = parseExpressionTyped defs ty .
            getExactlyOneChild .getChild "expression"

-- MetaData Parser ---------------------------------------------------------------------------------

parseMetaData :: Definitions -> Script -> MetaData
parseMetaData defs script = MetaData
        { scenarioName                   = parseScenarioName                        metadataEl
        , scenarioLanguage               = parseScenarioLanguage                    metadataEl
        , scenarioDescription            = parseScenarioDescription                 metadataEl
        , scenarioDifficulty             = parseScenarioDifficulty                  metadataEl
        , scenarioInitialParameterValues = parseScenarioInitialParameterValues defs metadataEl
        , scenarioPropertyValues         = parsePropertyValues                 defs metadataEl
        }
    where metadataEl = getChild "metadata" script

parseScenarioName :: Element -> Name
parseScenarioName = getData . getChild "name"

parseScenarioLanguage :: Element -> Maybe String
parseScenarioLanguage metadataEl = getAttribute "code" <$> findChild "language" metadataEl

parseScenarioDescription :: Element -> String
parseScenarioDescription = getData . getChild "description"

parseScenarioDifficulty :: Element -> Maybe Difficulty
parseScenarioDifficulty metadataEl = fromMaybe (error "parseScenarioDifficulty: no parse") . readDifficulty .
    getData <$> findChild "difficulty" metadataEl

parseScenarioInitialParameterValues :: Definitions -> Element -> ParameterState
parseScenarioInitialParameterValues defs metadataEl = Usered
    { useredUserDefined = parseCharactereds valueParser M.fromList (getChild "userDefined" valsElem)
    , useredFixed = parseCharactereds valueParser M.fromList (getChild "fixed" valsElem)
    }
  where valsElem = fromMaybe (error "Initial parameter values not found") $
            findChild "initialParameterValues" metadataEl
        valueParser = parseNamedDomainDataValue "parameter" (snd (definitionsParameters defs))

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
    ,   statCharacterIdref = parseCharacterIdref      statElem
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
            (DD.unrestrictType (M.findWithDefault errorDefault idref (snd (definitionsParameters defs))))
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
parseCondition defs conditionElem = case stripCharacterPrefix (name conditionElem) of
    "and"       -> And (map (parseCondition defs) (children conditionElem))
    "or"        -> Or  (map (parseCondition defs) (children conditionElem))
    "condition" -> Condition
        ComparisonCondition
        { conditionIdref          = idref
        , conditionCharacterIdref = parseCharacterIdref conditionElem
        , conditionTest           = parseCompareOperator conditionElem
        , conditionValue          = value
        }
      where idref = getAttribute "idref" conditionElem
            errorDefault = error ("Condition for unknown parameter " ++ idref)
            ty = M.findWithDefault errorDefault idref (snd (definitionsParameters defs))
            value = parseDomainDataValue (DD.unrestrictType ty) conditionElem
    _           -> error "no parse condition"

-- | Parses a compare operator. Gives an exception on invalid input.
parseCompareOperator :: Element -> CompareOperator
parseCompareOperator conditionElem = read (applyToFirst toUpper (getAttribute "operator" conditionElem))

parseExpressionTyped :: Definitions -> DD.Type -> Element -> Expression
parseExpressionTyped defs ty el = case stripCharacterPrefix (name el) of
    "literal" -> Literal (parseDomainDataValue ty el)
    "parameterReference" -> ParameterReference (getAttribute "idref" el) (parseCharacterIdref el)
    "sum" -> Sum (map (parseExpressionTyped defs ty) (children el))
    "scale" -> Scale
        (maybe 1 read (findAttribute "scalar" el))
        (maybe 1 read (findAttribute "divisor" el))
        (parseExpressionTyped defs ty (getExactlyOneChild el))
    "choose" -> Choose
        (map parseWhen (findChildren "when" el))
        (parseExpressionTyped defs ty (getExactlyOneChild (getChild "otherwise" el)))
      where
        parseWhen whenEl =
            ( parseCondition defs (getExactlyOneChild (getChild "condition" whenEl))
            , parseExpressionTyped defs ty (getExactlyOneChild (getChild "expression" whenEl))
            )
    n -> error ("parseExpressionTyped: not supported: " ++ n)

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
    DD.TSimple simpleType -> parseSimpleDomainDataValue simpleType (getData el)
    DD.TList itemType -> DD.VList (map (parseDomainDataValue itemType) (children el))
    DD.TAttributeRecord contentInfo attrs -> DD.VAttributeRecord
        (maybeToList (getContentValue <$> contentInfo) ++ map parseAttribute attrs)
      where getContentValue (contentName, contentType) =
              (contentName, parseDomainDataValue contentType el)
            parseAttribute (attrName, DD.TSimple attrType) =
              (attrName, parseSimpleDomainDataValue attrType (getAttribute attrName el))
            parseAttribute (attrName, attrType) =
              error ("Attribute " ++ attrName ++ " has invalid non-simple type " ++ show attrType)

parseSimpleDomainDataValue :: DD.SimpleType -> String -> DD.Value
parseSimpleDomainDataValue ty s = case ty of
    DD.TBoolean -> DD.VBoolean (read (applyToFirst toUpper s))
    DD.TInteger mmin mmax -> DD.VInteger (DD.clamp mmin mmax (read s))
    DD.TString -> DD.VString  s

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
parseCharactered parseSub valEl = case parseCharacterIdref valEl of
    Just characteridref -> Right (characteridref, val)
    Nothing             -> Left val
  where
    val = parseSub valEl

parseCharacterIdref :: Element -> Maybe String
parseCharacterIdref = findAttribute "characteridref"

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

stripCharacterPrefix :: String -> String
stripCharacterPrefix s = maybe s (applyToFirst toLower) (stripPrefix "character" s)
