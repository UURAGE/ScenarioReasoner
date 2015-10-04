------------------------------------------------------------------------------------
-- This program has been developed by students from the bachelor Computer Science
-- at Utrecht University within the Software and Game project course (2013-2015)
-- ©Copyright Utrecht University (Department of Information and Computing Sciences)
------------------------------------------------------------------------------------

module Domain.Scenarios.Parser where

import Control.Monad

import Data.Char
import Data.List
import Text.Read(readMaybe)

import System.IO
import System.IO.Unsafe

import Ideas.Common.Library hiding (Sum)
import Ideas.Text.XML.Interface

import Domain.Scenarios.ScoringFunction
import Domain.Scenarios.Condition
import Domain.Scenarios.ScenarioState
import Domain.Scenarios.Globals
import Domain.Scenarios.Scenario
import Data.Binary

type Script = Element

-- Functions to be exposed as an interface
----------------------------------------------------------------------------------------------------
   
-- | Parses the XML script at "filepath" to a Script. lazy.
parseScript :: FilePath -> IO Script
parseScript filepath = withBinaryFile filepath ReadMode 
        (hGetContents >=> (either fail return . parseXML))
        -- if parameter is Left a, do fail a, if it is Right b do (return . Script) . parseXML b

-- | Parses a script from a script element
parseScenario :: Script -> Scenario
parseScenario script = Scenario 
        { scenarioMetaData     = parseMetaData     script
        , scenarioFeedbackForm = parseFeedbackForm script
        , scenarioDialogue     = parseDialogue     script     
        } 
		
readBinaryScenario :: FilePath -> Scenario
readBinaryScenario path = unsafePerformIO $ decodeFile path
        
----------------------------------------------------------------------------------------------------        
 
-- Functions to be used internally
----------------------------------------------------------------------------------------------------

-- MetaData Parser ---------------------------------------------------------------------------------

parseMetaData :: Script -> MetaData
parseMetaData script = MetaData    
        { scenarioID              = parseScenarioID              script
        , scenarioName            = parseScenarioName            script
        , scenarioDescription     = parseScenarioDescription     script
        , scenarioDifficulty      = parseScenarioDifficulty      script
        , scenarioBannerImage     = parseScenarioBannerImage     script
        , scenarioCharacterImage  = parseScenarioCharacterImage  script 
        , scenarioModel           = parseScenarioModel           script
        , scenarioStartEmotion    = parseScenarioStartEmotion script
        , scenarioParameters      = parseScenarioParameters      script
        , scenarioLocation        = parseScenarioLocation        script
        , scenarioPet             = parseScenarioPet             script
        , scenarioToggles         = parseScenarioToggles         script
        , scenarioScoringFunction = parseScenarioScoringFunction script
        , scenarioScoreExtremes   = parseScenarioScoreExtremes   script
        }
        
-- | Queries the given script for its ID.
parseScenarioID :: Script -> ID
parseScenarioID = getMetaDataString "id"

-- | Queries the given script for its name.
parseScenarioName :: Script -> Name
parseScenarioName = getMetaDataString "name"

-- | Queries the given script for its description.
parseScenarioDescription :: Script -> String
parseScenarioDescription = getMetaDataString "description"

parseScenarioLocation :: Script -> Name
parseScenarioLocation = getMetaDataString "location"

parseScenarioPet :: Script -> Name
parseScenarioPet = getMetaDataString "pet"

-- | Queries the given script for its difficulty.
parseScenarioDifficulty :: Script -> Difficulty
parseScenarioDifficulty script = errorOnFail errorMsg (readDifficulty difficultyString)
 where 
    difficultyString = getMetaDataString "difficulty" script
    errorMsg = "Could not read difficulty: " ++ difficultyString

-- | Queries the given script for its banner image.
parseScenarioBannerImage :: Script -> Maybe ID
parseScenarioBannerImage script = nothingOnFail (
    findChild "metadata" script >>=
    findChild "bannerImage"         >>=
    findAttribute "extid")

-- | Queries the given script for its character image.
parseScenarioCharacterImage :: Script -> Maybe ID
parseScenarioCharacterImage script = nothingOnFail(
    findChild "metadata" script >>=
    findChild "characterImage"      >>=
    findAttribute "extid")

-- | Queries the given script for its model.
parseScenarioModel :: Script -> Maybe ID
parseScenarioModel script = nothingOnFail(
    findChild "metadata" script >>=
    findChild "model"               >>=
    findAttribute "extid")
    
-- | Queries the given script for its start emotion.
parseScenarioStartEmotion :: Script -> Maybe ID
parseScenarioStartEmotion script = case startEmotion of
	Nothing -> Nothing
	Just element -> Just (getData element)
  where startEmotion = nothingOnFail (findChild "metadata" script >>= findChild "startEmotion")

-- | Queries the given script for its parameters.
parseScenarioParameters :: Script -> [Parameter]
parseScenarioParameters script = map parseParameter (children parameterElem)
  where
    metaDataElem  = getChild "metadata" script
    parameterElem = getChild "parameters" metaDataElem
    
    -- | Parses a parameter Element inside the parameters inside the metadata of the script.
    parseParameter :: Element -> Parameter
    parseParameter paramElem = Parameter
        { parameterId           = getAttribute "id" paramElem
        , parameterName         = getAttribute "name" paramElem
        , parameterInitialValue = findAttribute "initialValue" paramElem >>= readMaybe :: Maybe ParameterValue
        , parameterDescription  = case nothingOnFail (findAttribute "parameterDescription" paramElem) of 
                                    Nothing    -> "" 
                                    Just descr -> descr
        , parameterScored       = tryParseBool (findAttribute "scored" paramElem)
        , parameterMax          = findAttribute "maximumScore" paramElem >>= readMaybe :: Maybe ParameterValue
        , parameterMin          = findAttribute "minimumScore" paramElem >>= readMaybe :: Maybe ParameterValue
        }

-- | Queries the given script for its defined toggles using the globally defined toggleNames variable.
parseScenarioToggles :: Script -> [Toggle]
parseScenarioToggles script = map parseToggle toggleNames
    where parseToggle toggleName = Toggle toggleName (parseBool (getMetaDataString toggleName script))
    
-- | Queries the given script for its scoring function.
parseScenarioScoringFunction :: Script -> ScoringFunction
parseScenarioScoringFunction script = parseScoringFunction (scoringFunctionChild (children scoringFunctionElem))
  where 
    metaDataElem = getChild "metadata" script
    scoringFunctionElem = getChild "scoringFunction" metaDataElem
    scoringFunctionChild [sf] = sf
    scoringFunctionChild _    = error "could not parse scoringFunction" 
    
-- | Parses a scoring function element.
parseScoringFunction :: Element -> ScoringFunction
parseScoringFunction scoringFunctionElem = case name scoringFunctionElem of
    "constant"           -> Constant            parseConstant
    "sum"                -> Sum                (map parseScoringFunction (children scoringFunctionElem))
    "scale"              -> Scale parseScalar  (parseScoringFunction paramElem)
    "paramRef"           -> ParamRef           (getAttribute "idref" scoringFunctionElem)
    _                    -> error "no parse scoringfunction element"
  where 
    parseConstant = read (getAttribute "value" scoringFunctionElem)  :: Score
    parseScalar   = read (getAttribute "scalar" scoringFunctionElem) :: Int
    paramElem     = getChild "paramRef"  scoringFunctionElem         :: Element

-- | Queries the given script for its score extremes.
parseScenarioScoreExtremes :: Script -> Maybe (Score, Score)
parseScenarioScoreExtremes script = 
    findChild "metadata" script >>=
    findChild "scoreExtremes"       >>= 
    \scoreExtremesElem -> do
    minimumValue <- findAttribute "minimum" scoreExtremesElem
    maximumValue <- findAttribute "maximum" scoreExtremesElem
    return (read minimumValue :: Score, read maximumValue :: Score)
        
    
-- MetaData Parser END -----------------------------------------------------------------------------
  
-- FeedbackForm Parser -----------------------------------------------------------------------------
  
parseFeedbackForm :: Script -> FeedbackForm
parseFeedbackForm script = map parseFeedbackFormEntry feedbackParamElems
  where     
    feedbackFormElem = getChild "feedbackform" script
    feedbackParamElems = children feedbackFormElem

-- Evaluates the feedbackform entry using the given parameter and the score on the parameter 
-- and then gives the appriopriate feedback   
parseFeedbackFormEntry :: Element -> FeedbackFormEntry
parseFeedbackFormEntry feedbackParamElem = FeedbackFormEntry
    { feedbackParamID    = paramID
    , feedbackConditions = map parseConditionedFeedback conditionedFeedbackElems
    , feedbackDefault    = nothingOnFail maybeDefaultFeedback
    }
  where 
    paramID = getAttribute "id" feedbackParamElem
    conditionedFeedbackElems = filter ((/= "default") . name) (children feedbackParamElem)
    maybeDefaultFeedback = findChild "default" feedbackParamElem >>= return . getData
    
    parseConditionedFeedback :: Element -> (Condition, String)
    parseConditionedFeedback condFeedbackElem = 
        (case maybeOtherOp of 
            Nothing        -> Condition
                ComparisonCondition
                { conditionIdref = paramID
                , conditionTest  = parseCompareOperator condFeedbackElem
                , conditionValue = getValue condFeedbackElem
                }
            Just "between" -> And 
                [ Condition 
                    ComparisonCondition
                    { conditionIdref = paramID
                    , conditionTest  = GreaterThanEqualTo
                    , conditionValue = read (getAttribute "lowerBound" condFeedbackElem) :: ParameterValue
                    }
                , Condition
                    ComparisonCondition
                    { conditionIdref = paramID
                    , conditionTest  = LessThanEqualTo
                    , conditionValue = read (getAttribute "upperBound" condFeedbackElem) :: ParameterValue
                    }
                ] 
            Just "max"     -> Condition
                ComparisonCondition
                    { conditionIdref = paramID
                    , conditionTest  = GreaterThanEqualTo
                    , conditionValue = getValue condFeedbackElem
                    }
            Just "min"     -> Condition
                ComparisonCondition
                    { conditionIdref = paramID
                    , conditionTest  = LessThanEqualTo
                    , conditionValue = getValue condFeedbackElem
                    }
            Just _         -> error "no parse conditionedfeedback"        
            
        , getData condFeedbackElem)
      where 
        maybeOtherOp = case getAttribute "test" condFeedbackElem of
            "between" -> Just "between" 
            "max"     -> Just "max"
            "min"     -> Just "min" 
            _         -> Nothing
            
-- FeedbackForm Parser END -------------------------------------------------------------------------
    
-- Dialogue Parser ---------------------------------------------------------------------------------

parseDialogue :: Script -> Dialogue
parseDialogue script = map parseInterleaveLevel interleaveElems
  where
    sequenceElem = getChild "sequence" script
    interleaveElems = findChildren "interleave" sequenceElem    
    
parseInterleaveLevel :: Element -> InterleaveLevel
parseInterleaveLevel interleaveElem = (read lvl :: Int, trees)
  where
    lvl = getAttribute "level" interleaveElem
    treeElems = findChildren "tree" interleaveElem 
    trees = map parseTree treeElems

parseTree :: Element -> Tree
parseTree treeElem = 
    Tree
    { treeID         = getAttribute "id" treeElem
    , treeStartIDs   = map (getAttribute "idref") (findChildren "start" treeElem)
    , treeAtomic     = null (filter jumpPoint statements)
    , treeOptional   = tryParseBool (findAttribute "optional" treeElem)
    , treeStatements = statements
    }
  where statements = parseStatements treeElem
  
parseStatements :: Element -> [Statement]
parseStatements treeElem = playerStats ++ computerStats ++ conversation
  where
    playerStats   = map parseStatement (findChildren "playerStatement"   treeElem)
    computerStats = map parseStatement (findChildren "computerStatement" treeElem)
    conversation  = map parseStatement (findChildren "conversation"      treeElem)

parseStatement :: Element -> Statement
parseStatement statElem = 
    Statement
    { statID             = getAttribute "id"         statElem
    , statInfo           = parseStatementInfo        statElem
    , statPrecondition   = parseMaybePrecondition    statElem
    , statParamEffects   = parseParameterEffects     statElem
    , statEmotionEffects = parseEmotionEffects       statElem
    , jumpPoint          = parseJumpPoint            statElem
    , statInits          = parseInits                statElem
    , nextStatIDs        = parseNextStatIDs          statElem
    }
    
parseStatementInfo :: Element -> StatementInfo
parseStatementInfo statElem =
    StatementInfo
    {   statType        = parseType     statElem
    ,   statText        = parseText     statElem
    ,   statIntents     = parseIntents  statElem
    ,   statFeedback    = parseFeedback statElem
    ,   statMedia       = parseMedia    statElem
    ,   statEnd         = parseEnd      statElem
    }

-- | Takes a statement and returns its type.
parseType :: Element -> StatementType
parseType statElem = takeWhile isLower (name statElem)

-- | Takes a statement and returns its text.
parseText :: Element -> StatementText
parseText statElem = case name statElem of
        "conversation" -> Right (map toConversationText (filterText conversationElems))
        _              -> Left (getData (getChild "text" statElem))
  where 
    toConversationText textEl = (map toLower (name textEl), getData textEl)
    conversationElems = children statElem
    filterText = filter (isSuffixOf "Text" . name)

-- | Takes a statement element and returns its precondition, if it has one.
-- | Uses the monadic findChild for Maybe Monad
parseMaybePrecondition :: Element -> Maybe Condition
parseMaybePrecondition statElem =
    fmap (parseCondition . getExactlyOneChild) conditionElem
      where conditionElem = findChild "preconditions" statElem

-- | Takes a statement element and returns its effects.
parseParameterEffects :: Element -> [Effect]
parseParameterEffects statElem = map parseParameterEffect paramElems
  where paramElems = emptyOnFail (liftM children (findChild "parameterEffects" statElem))
    
parseEmotionEffects :: Element -> [Effect]
parseEmotionEffects statElem = map parseEmotionEffect emotionElems
  where emotionElems = emptyOnFail (liftM children (findChild "emotionEffects" statElem))
  
parseParameterEffect :: Element -> Effect
parseParameterEffect effectElem = Effect
            { effectIdref      = getAttribute "idref" effectElem
            , effectChangeType = parseChangeType      effectElem
            , effectValue      = getValue             effectElem
            }
            
parseEmotionEffect :: Element -> Effect
parseEmotionEffect effectElem = Effect
            { effectIdref      = getAttribute "emotionid" effectElem
            , effectChangeType = parseChangeType      effectElem
            , effectValue      = getValue             effectElem
            }            
            
-- | Parses a string to a Changetype. Gives an exception on invalid input.
parseChangeType :: Element -> ChangeType
parseChangeType effectElem = read (applyToFirst toUpper changeTypeStr)
  where changeTypeStr = getAttribute "changeType" effectElem
      
parseJumpPoint :: Element -> Bool    
parseJumpPoint statElem = parseBool (getAttribute "jumpPoint" statElem)

parseInits :: Element -> Bool
parseInits statElem = tryParseBool (findAttribute "inits" statElem)

parseEnd :: Element -> Bool    
parseEnd statElem = parseBool (getAttribute "possibleEnd" statElem)

-- | Takes a statement and returns the IDs of the statements following it. TODO!!!!
parseNextStatIDs :: Element -> [ID]
parseNextStatIDs element = errorOnFail errorMsg nextIDs
  where 
    errorMsg = "Failed to get the nextIDs of: " ++ name element
    nextIDs =
        case name element of
            "conversation"      -> getResponses >>= getIdrefs
            "computerStatement" -> getResponses >>= getIdrefs
            "playerStatement"   -> getNextComputerStatements >>= getIdrefs
            _                   -> fail $
                "Cannot get nexts of statement represented by element named " ++ name element
      where getIdrefs = mapM (findAttribute "idref")
            getResponses = liftM children $ findChild "responses" element
            getNextComputerStatements =
                case findChild "nextComputerStatements" element of
                      Just nextElem -> return $ children nextElem
                      Nothing       -> liftM singleton $ findChild "nextComputerStatement" element         

-- | Parses media of the statement element
parseMedia :: Element -> MediaInfo
parseMedia statElem = MediaInfo parseMediaVisuals parseMediaAudios
  where 
    -- | Takes a statement and returns its visual media.
    parseMediaVisuals :: [(Name, ID)]
    parseMediaVisuals = map parseMediaVisual visualElems
      where 
        visualElems = 
            findChild "media" statElem >>= 
            findChild "visuals"        >>= 
            children
        -- | Parses video or image
        parseMediaVisual :: Element -> (Name, ID)
        parseMediaVisual e = (name e, getAttribute "extid" e)

    -- | Takes a statement and returns its audio.
    parseMediaAudios :: [ID]
    parseMediaAudios = map (getAttribute "extid") audioElems
      where audioElems =
                findChild "media" statElem >>= 
                findChild "audios"          >>= 
                children 
            
-- | Takes a statement and returns its intents.
parseIntents :: Element -> [String]
parseIntents statElem = map getData (findChild "intents" statElem >>= children)

-- | Parses the feedback of the given statement element
parseFeedback :: Element -> Maybe String
parseFeedback statElem = nothingOnFail (liftM getData (findChild "feedback" statElem))

-- Dialogue Parser END -----------------------------------------------------------------------------

-- | Parses a Bool.
parseBool :: String -> Bool
parseBool boolStr = read (applyToFirst toUpper boolStr) :: Bool

-- | Tries to parse bool, False on Nothing
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
--  in the "metaDataName". This could be the name of the script, the difficulty, location, etc.
getMetaDataString :: Name -> Script -> String
getMetaDataString metaDataName script = getData dataElem
  where 
    metadata = getChild "metadata" script
    dataElem = getChild metaDataName metadata
    
-- | Parses a value attribute from an element
getValue :: Element -> ParameterValue
getValue el = read (getAttribute "value" el) :: ParameterValue

