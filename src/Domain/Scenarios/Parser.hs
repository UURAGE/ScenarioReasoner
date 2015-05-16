module Domain.Scenarios.Parser where

import GHC.Exts (sortWith)

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe
import System.IO (withBinaryFile, hGetContents, IOMode(..))

import Ideas.Common.Library hiding (Sum)
import Ideas.Common.Utils (readM)
import Ideas.Text.XML.Interface

import Domain.Scenarios.ScoringFunction
import Domain.Scenarios.Condition
import Domain.Scenarios.ScenarioState
import Domain.Scenarios.Globals

data Scenario = Scenario 
        { scenarioMetaData :: MetaData
        , scenarioDialogue :: Dialogue        
        }
    deriving(Show)

data MetaData = MetaData    
        { scenarioID              :: ID
        , scenarioName            :: Name
        , scenarioDescription     :: String
        , scenarioDifficulty      :: Difficulty 
        , scenarioBannerImage     :: Maybe ID
        , scenarioCharacterImage  :: Maybe ID
        , scenarioModel           :: Maybe ID 
        , scenarioParameters      :: [Parameter]
        , scenarioLocation        :: Name
        , scenarioToggles         :: [Toggle]
        , scenarioScoringFunction :: ScoringFunction
        , scenarioScoreExtremes   :: Maybe (Score, Score)
        }
        
type Dialogue = [InterleaveLevel]

type InterleaveLevel = (Int, [Tree])

data Tree = Tree
        { treeID          :: ID
        , treeStartID     :: ID
        , treeAtomic      :: Bool -- for optimisation
        , treeOptional    :: Bool
        , treeStatements  :: [Statement]
        }        
        
data Statement = Statement
        { statID            :: ID
        , statType          :: StatementType
        , statText          :: Either String [(ConversationTextType, String)]
        , statPrecondition  :: Maybe Condition
        , statEffects       :: [Effect]
        , jumpPoint         :: Bool
        , statInits         :: Bool
        , endOfConversation :: Bool
        , nextStatIDs       :: [ID]
        , statMedia         :: MediaInfo
        , statIntents       :: [String]
        , statFeedback      :: Maybe String
        }
   
-- | A value describing the type of a statement element 
data StatementType = ComputerStatement | PlayerStatement | Conversation
    deriving (Show, Eq, Read)

-- | A value describing the type of a piece of text in a conversation
data ConversationTextType = PlayerText | ComputerText | SituationText
    deriving (Show, Eq, Read)
   

-- Functions to be exposed as an interface
----------------------------------------------------------------------------------------------------
   
-- | Parses the XML script at "filepath" to a Script. hGetContent is NOT lazy.
parseScript :: String -> IO Script
parseScript filepath = do
    withBinaryFile filepath ReadMode $ \h ->
      hGetContents h >>= either fail return . parseXML
      -- if parameter is Left a, do fail a, if it is Right b do (return . Script) . parseXML b

-- | Parses a script from a script element
parseScenario :: Script -> Scenario
parseScenario script = Scenario 
        { scenarioMetaData = parseMetaData script
        , scenarioDialogue = parseDialogue script     
        }
 
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
        , scenarioParameters      = parseScenarioParameters      script
        , scenarioLocation        = parseScenarioLocation        script
        , scenarioToggles         = parseScenarioToggles         script
        , scenarioScoringFunction = parseScenarioScoringFunction script
        , scenarioScoreExtremes   = parseScenarioScoreExtremes   script
        }
        
-- | Queries the given script for its ID.
parseScenarioID :: Script -> ID
parseScenarioID = parseMetaDataString "id"

-- | Queries the given script for its name.
parseScenarioName :: Script -> Name
parseScenarioName = parseMetaDataString "name"

-- | Queries the given script for its description.
parseScenarioDescription :: Script -> String
parseScenarioDescription = parseMetaDataString "description"

parseScenarioLocation :: Script -> Name
parseScenarioLocation = parseMetaDataString "location"

-- | Queries the given script for its difficulty.
parseScenarioDifficulty :: Script -> Difficulty
parseScenarioDifficulty script = errorOnFail errorMsg (readDifficulty difficultyString)
 where 
    difficultyString = parseMetaDataString "difficulty" script
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
        , parameterEmotion      = nothingOnFail (findAttribute "emotionid" paramElem)
        , parameterInitialValue = nothingOnFail (findAttribute "initialValue" paramElem >>= readM)
        , parameterScored       = tryParseBool (findAttribute "scored" paramElem)
        }

parseScenarioToggles :: Script -> [Toggle]
parseScenarioToggles script = map parseToggle toggleNames
    where parseToggle toggleName = Toggle toggleName (parseBool (parseMetaDataString toggleName script))

-- | Queries the given script for its scoring function.
parseScenarioScoringFunction :: Script -> ScoringFunction
parseScenarioScoringFunction script = parseScoringFunction (scoringFunctionChild (children scoringFunctionElem))
  where 
    metaDataElem = getChild "metadata" script
    scoringFunctionElem = getChild "scoringFunction" metaDataElem
    scoringFunctionChild [elem] = elem
    scoringFunctionChild _      = error "could not parse scoringFunction" 
    
-- | Parses a scoring function element.
parseScoringFunction :: Element -> ScoringFunction
parseScoringFunction scoringFunctionElem = case name scoringFunctionElem of
    "constant"           -> Constant            parseConstant
    "sum"                -> Sum                (map parseScoringFunction (children scoringFunctionElem))
    "scale"              -> Scale parseScalar  (parseScoringFunction paramElem)
    "paramRef"           -> ParamRef           (getAttribute "idref" scoringFunctionElem)
    "integeredCondition" -> IntegeredCondition (parseCondition conditionElem)
  where 
    parseConstant = read (getAttribute "value" scoringFunctionElem)  :: Score
    parseScalar   = read (getAttribute "scalar" scoringFunctionElem) :: Int
    conditionElem = getChild "condition" scoringFunctionElem         :: Element
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
   
    
-- Dialogue Parser ---------------------------------------------------------------------------------

parseDialogue :: Script -> Dialogue
parseDialogue script = map parseInterleaveLevel interleaveElems
  where
    sequenceElem = getChild "sequence" script
    interleaveElems = findChildren "interleave" sequenceElem    
    
parseInterleaveLevel :: Element -> InterleaveLevel
parseInterleaveLevel interleaveElem = (read level :: Int, trees)
  where
    level = getAttribute "level" interleaveElem
    treeElems = findChildren "tree" interleaveElem 
    trees = map parseTree treeElems

parseTree :: Element -> Tree
parseTree treeElem = 
    Tree
    { treeID         = getAttribute "id" treeElem
    , treeStartID    = (getAttribute "idref") (getChild "start" treeElem)
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
    { statID            = getAttribute "id"         statElem
    , statType          = parseType                 statElem       
    , statText          = parseText                 statElem
    , statPrecondition  = parseMaybePrecondition    statElem
    , statEffects       = parseEffects              statElem
    , jumpPoint         = parseJumpPoint            statElem
    , statInits         = parseInits                statElem
    , endOfConversation = parseEnd                  statElem
    , nextStatIDs       = parseNextStatIDs          statElem
    , statMedia         = parseMedia                statElem
    , statIntents       = parseIntents              statElem
    , statFeedback      = parseFeedback             statElem
    }

-- | Takes a statement and returns its type.
parseType :: Element -> StatementType
parseType statElem = read (applyToFirst toUpper (name statElem))

-- | Takes a statement and returns its text.
parseText :: Element -> Either String [(ConversationTextType, String)]
parseText statElem =
    case name statElem of
        "conversation" -> Right (map toConversationText (filter (isSuffixOf "Text" . name) (children statElem)))
        _              -> Left (getData (getChild "text" statElem))
  where toConversationText textEl = (read (applyToFirst toUpper (name textEl)), getData textEl)

-- | Takes a statement element and returns its precondition, if it has one.
-- | Uses the monadic findChild for Maybe Monad
parseMaybePrecondition :: Element -> Maybe Condition
parseMaybePrecondition statElem =
    maybe Nothing (Just . parseCondition . getExactlyOneChild) conditionElem
      where conditionElem = findChild "preconditions" statElem

-- | Takes a statement element and returns its effects.
parseEffects :: Element -> [Effect]
parseEffects statElem = map parseEffect effectElems 
  where effectElems = emptyOnFail (findChild "effects" statElem >>= return . children)

parseEffect :: Element -> Effect
parseEffect effectElem = Effect
            { effectIdref      = getAttribute "idref" effectElem
            , effectChangeType = parseChangeType      effectElem
            , effectValue      = parseValue           effectElem
            }
  where 
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
                "Cannot get nexts of statement represented by element named " ++ (name element)
      where getIdrefs = mapM (findAttribute "idref")
            getResponses = liftM children $ findChild "responses" element
            getNextComputerStatements =
                case findChild "nextComputerStatements" element of
                      Just nextElem -> return $ children nextElem
                      Nothing       -> liftM singleton $ findChild "nextComputerStatement" element         

-- | Parses media of the statement element
parseMedia :: Element -> MediaInfo
parseMedia statElem = MediaInfo (parseMediaVisuals statElem) (parseMediaAudios statElem)
  where 
    -- | Takes a statement and returns its visual media.
    parseMediaVisuals :: Element -> [(Name, ID)]
    parseMediaVisuals statElem = map parseMediaVisual visualElems
      where 
        visualElems = 
            findChild "media" statElem >>= 
            findChild "visuals"        >>= 
            children
        -- | Parses video or image
        parseMediaVisual :: Element -> (Name, ID)
        parseMediaVisual e = (name e, getAttribute "extid" e)

    -- | Takes a statement and returns its audio.
    parseMediaAudios :: Element -> [ID]
    parseMediaAudios statElem = map (getAttribute "extid") audioElems
      where audioElems =
                findChild "media" statElem >>= 
                findChild "audios"          >>= 
                children 
            
-- | Takes a statement and returns its intents.
parseIntents :: Element -> [String]
parseIntents statElem = map getData (findChild "intents" statElem >>= children)

-- | Parses the feedback of the given statement element
parseFeedback :: Element -> Maybe String
parseFeedback statElem = nothingOnFail (findChild "feedback" statElem >>= return . getData)

-- Dialogue Parser END -----------------------------------------------------------------------------


-- | Parses a condition and recursively parses ands and ors. Used in both parsers (metadata and dialogue)
parseCondition :: Element -> Condition
parseCondition conditionElem = case name conditionElem of
    "and"       -> And (map parseCondition (children conditionElem))
    "or"        -> Or  (map parseCondition (children conditionElem))
    "condition" -> Condition $ ComparisonCondition
        { conditionIdref = getAttribute "idref" conditionElem
        , conditionTest  = parseCompareOperator conditionElem
        , conditionValue = parseValue           conditionElem
        }
  where
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
    
-- | Parses a Bool.
parseBool :: String -> Bool
parseBool boolStr = read (applyToFirst toUpper boolStr) :: Bool

tryParseBool :: Maybe String -> Bool
tryParseBool (Just boolStr) = parseBool boolStr
tryParseBool _              = False

-- | Parses a value attribute from an element
parseValue :: Element -> ParameterValue
parseValue elem = read (getAttribute "value" elem) :: ParameterValue

-- | Queries the given script for basic information. Which information being queried is specified
--  in the "metaDataName". This could be the name of the script, the difficulty, location, etc.
parseMetaDataString :: Name -> Script -> String
parseMetaDataString metaDataName script = getData dataElem
  where 
    metadata = getChild "metadata" script
    dataElem = getChild metaDataName metadata
    
----------------------------------------------------------------------------------------------------
    
    
-- Show instances for the datatypes defined at the top of Parser.hs
instance Show MetaData where 
    show (MetaData id name desc diff bi ci model ps loc ts sf se) =
        "id: "              ++ show id    ++ "  name: " ++ show name         ++ "\n" ++ 
        "description: "     ++ show desc  ++ "\n" ++ 
        "difficulty: "      ++ show diff  ++ "\n" ++
        "bannerImage: "     ++ show bi    ++ "\n" ++ 
        "characterImage: "  ++ show ci    ++ "\n" ++ 
        "model: "           ++ show model ++ "\n" ++ 
        "parameters: "      ++ show ps    ++ "\n" ++ 
        "location: "        ++ show loc   ++ "\n" ++
        "toggles: "         ++ show ts    ++ "\n" ++ 
        "scoringFunction: " ++ show sf    ++ "\n" ++ 
        "scoreExtremes: "   ++ show se    ++ "\n"

instance Show Tree where
    show (Tree id start atom opt stats) = "\n" ++
        "tree: "        ++ show id    ++ 
        " start: "      ++ show start ++
        " atomic: "     ++ show atom  ++
        " optional: "   ++ show opt   ++
        " statements: " ++ show stats ++ "\n"
        
instance Show Statement where
    show (Statement id t desc pc es jp inits end nexts media is fb) = "\n  " ++
        "statement: "     ++ show id    ++ "\n\t" ++
        " type: "         ++ show t     ++ "\n\t" ++
        " description: "  ++ show desc  ++ "\n\t" ++ 
        " precondition: " ++ show pc    ++ "\n\t" ++
        " effects: "      ++ show es    ++ "\n\t" ++
        " jumpPoint: "    ++ show jp    ++ "\n\t" ++
        " inits: "        ++ show inits ++ "\n\t" ++
        " end: "          ++ show end   ++ "\n\t" ++
        " nextIDs: "      ++ show nexts ++ "\n\t" ++
        " media: "        ++ show media ++ "\n\t" ++
        " intents: "      ++ show is    ++ "\n\t" ++
        " feedback: "     ++ show fb    ++ "\n  " 