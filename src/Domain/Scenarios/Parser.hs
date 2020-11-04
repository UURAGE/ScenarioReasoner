{- © Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Parser where

import Data.Char
import Data.Either
import qualified Data.Foldable as F
import Data.List
import qualified Data.List.NonEmpty as N
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid hiding (Sum)

import Ideas.Common.Library
import Ideas.Text.XML hiding (Name, BuildXML(..))

import Domain.Scenarios.Condition
import Domain.Scenarios.Expression
import Domain.Scenarios.ScenarioState
import Domain.Scenarios.Globals
import Domain.Scenarios.Scenario
import qualified Domain.Scenarios.DomainData as DD

-- Functions to be exposed as an interface
----------------------------------------------------------------------------------------------------

-- | Parses a scenario from a scenario element
parseScenario :: XML -> Scenario
parseScenario scenarioEl = Scenario
        { scenarioDefinitions  = defs
        , scenarioExpressions  = parseExpressions defs scenarioEl
        , scenarioMetaData     = parseMetaData defs scenarioEl
        , scenarioTopDialogue  = parseDialogue defs scenarioEl
        }
  where defs = parseDefinitions scenarioEl

parseDefinitions :: XML -> Definitions
parseDefinitions scenarioEl = Definitions
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
            findChild "definitions" scenarioEl

parseDefinitionList :: XML -> ([Definition ()], TypeMap)
parseDefinitionList el = (defs, M.fromList (map toTypePair defs))
  where toTypePair def = (definitionId def, definitionType def)
        defs = map (parseDefinition (const (const ()))) (children el)

parseDefinition :: (DD.Type -> XML -> a) -> XML -> Definition a
parseDefinition parseContent defEl = Definition
        { definitionId           = getAttribute "id" defEl
        , definitionName         = getAttribute "name" defEl
        , definitionDescription  = getData <$> findChild "description" defEl
        , definitionType         = ty
        , definitionDefault      = parseDomainDataValue ty <$> maybeDefaultEl
        , definitionContent      = parseContent ty defEl
        }
  where (ty, maybeDefaultEl) = parseDomainDataType (getChild "type" defEl)

parseCharacterDefinition :: XML -> CharacterDefinition
parseCharacterDefinition defEl = CharacterDefinition
        { characterDefinitionId   = getAttribute "id" defEl
        , characterDefinitionName = findAttribute "name" defEl
        }

parseDomainDataType :: XML -> (DD.Type, Maybe XML)
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

parseSimpleDomainDataType :: XML -> DD.SimpleType
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

parseExpressions :: Definitions -> XML -> [Definition Expression]
parseExpressions defs = maybe [] (map (parseDefinition parseExpression) . children) .
    findChild "typedExpressions"
  where parseExpression ty = parseExpressionTyped defs ty .
            getExactlyOneChild .getChild "expression"

-- MetaData Parser ---------------------------------------------------------------------------------

parseMetaData :: Definitions -> XML -> MetaData
parseMetaData defs scenarioEl = MetaData
        { scenarioName                   = parseScenarioName                        metadataEl
        , scenarioLanguage               = parseScenarioLanguage                    metadataEl
        , scenarioDescription            = parseScenarioDescription                 metadataEl
        , scenarioDifficulty             = parseScenarioDifficulty                  metadataEl
        , scenarioVersion                = parseScenarioVersion                     scenarioEl
        , scenarioInitialParameterValues = parseScenarioInitialParameterValues defs metadataEl
        , scenarioPropertyValues         = parsePropertyValues                 defs metadataEl
        }
    where metadataEl = getChild "metadata" scenarioEl

parseScenarioName :: XML -> Name
parseScenarioName = getData . getChild "name"

parseScenarioLanguage :: XML -> Maybe String
parseScenarioLanguage metadataEl = getAttribute "code" <$> findChild "language" metadataEl

parseScenarioDescription :: XML -> String
parseScenarioDescription = getData . getChild "description"

parseScenarioDifficulty :: XML -> Maybe Difficulty
parseScenarioDifficulty metadataEl = fromMaybe (error "parseScenarioDifficulty: no parse") . readDifficulty .
    getData <$> findChild "difficulty" metadataEl

parseScenarioVersion :: XML -> Maybe Int
parseScenarioVersion scenarioEl = read <$> findAttribute "version" scenarioEl

parseScenarioInitialParameterValues :: Definitions -> XML -> ParameterState
parseScenarioInitialParameterValues defs metadataEl = Usered
    { useredUserDefined = parseCharactereds valueParser M.fromList (getChild "userDefined" valsElem)
    , useredFixed = parseCharactereds valueParser M.fromList (getChild "fixed" valsElem)
    }
  where valsElem = fromMaybe (error "Initial parameter values not found") $
            findChild "initialParameterValues" metadataEl
        valueParser = parseNamedDomainDataValue "parameter" (snd (definitionsParameters defs))

-- MetaData Parser END -----------------------------------------------------------------------------

-- Dialogue Parser ---------------------------------------------------------------------------------

parseDialogue :: Definitions -> XML -> TopDialogue
parseDialogue defs scenarioEl = map (parseInterleaveLevel defs) interleaveElems
  where
    sequenceElem = getChild "sequence" scenarioEl
    interleaveElems = findChildren "interleave" sequenceElem

parseInterleaveLevel :: Definitions -> XML -> InterleaveLevel
parseInterleaveLevel defs interleaveElem = map (parseTree defs) diaElems
  where
    diaElems = findChildren "dialogue" interleaveElem

parseTree :: Definitions -> XML -> Dialogue
parseTree defs diaElem =
    Dialogue
    { diaID         = getAttribute "id" diaElem
    , diaStartIDs   = map (getAttribute "idref") (children (getChild "starts" diaElem))
    , diaAtomic     = not (any statAllowInterleave statements)
    , diaOptional   = tryParseBool (findAttribute "optional" diaElem)
    , diaStatements = statements
    }
  where statements = map (parseStatement defs) (children (getChild "statements" diaElem))

parseStatement :: Definitions -> XML -> Statement
parseStatement defs statElem =
    Statement
    { statID               = getAttribute "id"           statElem
    , statInfo             = parseStatementInfo defs     statElem
    , statPrecondition     = parseMaybePrecondition defs statElem
    , statParamEffects     = parseParameterEffects defs  statElem
    , statAllowInterleave  = parseAllowInterleave        statElem
    , statAllowDialogueEnd = parseAllowDialogueEnd       statElem
    , statEnd              = parseEnd                    statElem
    , statNextStatIDs      = parseNextStatIDs            statElem
    }

parseStatementInfo :: Definitions -> XML -> StatementInfo
parseStatementInfo defs statElem =
    StatementInfo
    {   statType           = parseType                statElem
    ,   statText           = parseText                statElem
    ,   statCharacterIdref = parseCharacterIdref      statElem
    ,   statPropertyValues = parsePropertyValues defs statElem
    }

-- | Takes a statement and returns its type
parseType :: XML -> StatementType
parseType statElem = takeWhile isLower (name statElem)

-- | Takes a statement and returns its text
parseText :: XML -> StatementText
parseText statElem = getData (getChild "text" statElem)

-- | Takes a statement element and returns its precondition, if it has one
parseMaybePrecondition :: Definitions -> XML -> Maybe Condition
parseMaybePrecondition defs statElem =
    fmap (parseCondition defs . getExactlyOneChild) conditionElem
      where conditionElem = findChild "preconditions" statElem

-- | Takes a statement element and returns its effects
parseParameterEffects :: Definitions -> XML -> Usered (Charactered [Effect])
parseParameterEffects defs statElem = Usered
    { useredUserDefined = Charactered
        (map (parseParameterEffect defs) (children (getChild "userDefined" effectsElem)))
        M.empty
    , useredFixed = parseCharactereds (parseParameterEffect defs) id (getChild "fixed" effectsElem)
    }
  where effectsElem = getChild "parameterEffects" statElem

parseParameterEffect :: Definitions -> XML -> Effect
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
parseAssignmentOperator :: XML -> AssignmentOperator
parseAssignmentOperator effectElem = read (applyToFirst toUpper operatorStr)
  where operatorStr = getAttribute "operator" effectElem

parseAllowInterleave :: XML -> Bool
parseAllowInterleave statElem = maybe False parseBool . getFirst $
    First (findAttribute "allowInterleave" statElem) <> First (findAttribute "jumpPoint" statElem)

parseAllowDialogueEnd :: XML -> Bool
parseAllowDialogueEnd statElem = maybe False parseBool . getFirst $
    First (findAttribute "allowDialogueEnd" statElem) <> First (findAttribute "inits" statElem)

parseEnd :: XML -> Bool
parseEnd statElem = tryParseBool (findAttribute "end" statElem)

-- | Takes a statement and returns the IDs of the statements following it
parseNextStatIDs :: XML -> [ID]
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
parseCondition :: Definitions -> XML -> Condition
parseCondition defs conditionElem = case stripCharacterPrefix (name conditionElem) of
    "and"       -> And (map (parseCondition defs) (children conditionElem))
    "or"        -> Or  (map (parseCondition defs) (children conditionElem))
    "condition" -> Condition
        ComparisonCondition
        { conditionIdref          = idref
        , conditionCharacterIdref = parseCharacterIdref conditionElem
        , conditionTest           = parseOperator conditionElem
        , conditionValue          = value
        }
      where idref = getAttribute "idref" conditionElem
            errorDefault = error ("Condition for unknown parameter " ++ idref)
            ty = M.findWithDefault errorDefault idref (snd (definitionsParameters defs))
            value = parseDomainDataValue (DD.unrestrictType ty) conditionElem
    "referenceCondition" -> ReferenceCondition
        UnaryCondition
        { unaryConditionIdref          = idref
        , unaryConditionCharacterIdref = parseCharacterIdref conditionElem
        , unaryConditionTest           = parseOperator conditionElem
        }
      where idref = getAttribute "idref" conditionElem
    _           -> error "no parse condition"

-- | Parses an operator. Gives an exception on invalid input.
parseOperator :: Read a => XML -> a
parseOperator conditionElem = read (applyToFirst toUpper (getAttribute "operator" conditionElem))

parseExpressionTyped :: Definitions -> DD.Type -> XML -> Expression
parseExpressionTyped defs ty el = case stripCharacterPrefix (name el) of
    "literal" -> Literal (parseDomainDataValue ty el)
    "parameterReference" -> ParameterReference
        (getAttribute "idref" el)
        (parseCharacterIdref el)
        (maybe CalculateValue parseCalculation (findAttribute "calculate" el))
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

parseCalculation :: String -> Calculation
parseCalculation "value" = CalculateValue
parseCalculation "percentage" = CalculatePercentage
parseCalculation c = error ("parseCalculation: not supported: " ++ c)

-- | Parses property values from an element that has them
parsePropertyValues :: Definitions -> XML -> PropertyValues
parsePropertyValues defs propsElem = parseCharactereds
    (parseNamedDomainDataValue "property" (snd (definitionsProperties defs)))
    Assocs (getChild "propertyValues" propsElem)

parseNamedDomainDataValue :: String -> TypeMap -> XML -> (String, DD.Value)
parseNamedDomainDataValue valueKind typeMap propValEl = (idref, value)
  where
    idref = getAttribute "idref" propValEl
    errorDefault = error ("Value for unknown " ++ valueKind ++ " " ++ idref)
    value = parseDomainDataValue
      (M.findWithDefault errorDefault idref typeMap)
      propValEl

parseDomainDataValue :: DD.Type -> XML -> DD.Value
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

parseCharactereds :: (XML -> a) -> ([a] -> b) -> XML -> Charactered b
parseCharactereds parseSub mkCollection valsElem = Charactered
    { characteredIndependent = mkCollection civs
    , characteredPerCharacter = M.fromList (map toPC (N.groupAllWith fst pcvs))
    }
  where
    vals = map (parseCharactered parseSub) (children valsElem)
    (civs, pcvs) = partitionEithers vals
    toPC vs = (fst (N.head vs), mkCollection (map snd (N.toList vs)))

parseCharactered :: (XML -> a) -> XML -> Either a (String, a)
parseCharactered parseSub valEl = case parseCharacterIdref valEl of
    Just characteridref -> Right (characteridref, val)
    Nothing             -> Left val
  where
    val = parseSub valEl

parseCharacterIdref :: XML -> Maybe String
parseCharacterIdref = findAttribute "characteridref"

-- Functions that extend the XML parser
----------------------------------------------------------------------------------------------------

-- | Returns the child element with the given name out of the Monad defined in the framework
getChild :: Name -> XML -> XML
getChild elemName element = errorOnFail errorMsg mChild
  where
    errorMsg = "Failed to find child: " ++ elemName
    mChild = findChild elemName element

-- | Finds an attribute and gets it out of the Monad defined in the framework
getAttribute :: String -> XML -> String
getAttribute attributeName element = errorOnFail errorMsg mAttribute
  where
    errorMsg = "Failed to find attribute: " ++ attributeName
    mAttribute = findAttribute attributeName element

getExactlyOneChild :: XML -> XML
getExactlyOneChild element = case children element of
    []      -> error "no children found"
    [child] -> child
    _       -> error "multiple children found"

stripCharacterPrefix :: String -> String
stripCharacterPrefix s = maybe s (applyToFirst toLower) (stripPrefix "character" s)
