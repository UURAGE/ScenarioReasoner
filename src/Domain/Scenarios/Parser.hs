-----------------------------------------------------------------------------
{- |
Basic functions to query a ScriptElemLanguage XML script as specified by the
Communicate!-team.

This code doesn't do any error checking on the script. ScriptElems deviating
from the specifications may lead to unspecified behaviour (most likely
a Haskell-exception).
-}
-----------------------------------------------------------------------------
module Domain.Scenarios.Parser where

import GHC.Exts (sortWith)

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe
import System.IO (withBinaryFile, hGetContents, IOMode(..))

import Ideas.Common.Library hiding (Sum)
import Ideas.Common.Utils (readM)
import Ideas.Text.XML.Interface(parseXML, findChildren, findChild, findAttribute, children, name, getData, Element)

import Domain.Scenarios.ScoringFunction
import Domain.Scenarios.Condition
import Domain.Scenarios.ScriptState
import Domain.Scenarios.Globals

data Script = Script MetaData Dialogue

data MetaData = MetaData    
        { scriptID             :: ID
        , scriptName           :: Name
        , scriptDescription    :: String
        , scriptDifficulty     :: Difficulty 
        , scriptBannerImage    :: Maybe ID
        , scriptCharacterImage :: Maybe ID
        , scriptModel          :: Maybe ID 
        , scriptParameters     :: [Parameter]
        , scriptLocation       :: Name
        , scriptToggles        :: [Toggle]
        , scriptScoringFunction :: ScoringFunction
        , scriptScoreExtremes  :: Maybe (Score, Score)
        }
    deriving(Show)
        
type Dialogue = [InterleaveLevel]

type InterleaveLevel = (Int, [Tree])

data Tree = Tree
        { treeID          :: ID
        , treeStartID     :: ID
        , treeStatements  :: [Statement]
        }
    deriving(Eq)    
    
instance Show Tree where
    show (Tree id start stats) = 
        "tree: "        ++ show id    ++ 
        " start: "      ++ show start ++
        " statements: " ++ show stats ++ "\n"    
        
data Statement = Statement
        { statID            :: ID
        , statType          :: StatementType
        , statText          :: Either String [(ConversationTextType, String)]
        , statPrecondition  :: Maybe Condition
        , statEffects       :: [Effect]
        , jumpPoint         :: Bool
        , endOfConversation :: Bool
        , nextStatIDs       :: [ID]
        , statMedia         :: Media
        , statIntents       :: [String]
        , statFeedback      :: Maybe String
        }
    deriving(Eq)
    
data Media = Media [(Name, ID)] [ID] -- Media Visuals Audios
    deriving(Show,Eq)

instance Show Statement where
    show (Statement id t desc pc es jp end nexts media is fb) = 
        "statement: "     ++ show id    ++ 
        " type: "         ++ show t     ++ 
        " description: "  ++ show desc  ++
        " precondition: " ++ show pc    ++ 
        " effects: "      ++ show es    ++
        " jumpPoint: "    ++ show jp    ++
        " end: "          ++ show end   ++
        " nextIDs: "      ++ show nexts ++ 
        " media: "        ++ show media ++ 
        " intents: "      ++ show is    ++ 
        " feedback: "     ++ show fb    ++ "\n"
        
-- | A value describing the type of a statement element 
data StatementType = ComputerStatement | PlayerStatement | Conversation
    deriving (Show, Eq, Read)

-- | A value describing the type of a piece of text in a conversation
data ConversationTextType = PlayerText | ComputerText | SituationText
    deriving (Show, Eq, Read)
    
-- | Parses the XML script at "filepath" to a ScriptElement. hGetContent is NOT lazy.
parseScriptElement :: String -> IO ScriptElement
parseScriptElement filepath = do
    withBinaryFile filepath ReadMode $ \h ->
      hGetContents h >>= either fail return . parseXML
                        -- if parameter is Left a, do fail a, if it is Right b do (return . ScriptElement) . parseXML b
                        
-- | Parses the script element
parseScript :: ScriptElement -> Script
parseScript scriptElem = Script (parseMetaData scriptElem) (parseDialogue scriptElem)
    
parseMetaData :: ScriptElement -> MetaData
parseMetaData scriptElem = MetaData    
        { scriptID              = parseScriptID              scriptElem
        , scriptName            = parseScriptName            scriptElem
        , scriptDescription     = parseScriptDescription     scriptElem
        , scriptDifficulty      = parseScriptDifficulty      scriptElem
        , scriptBannerImage     = parseScriptBannerImage     scriptElem
        , scriptCharacterImage  = parseScriptCharacterImage  scriptElem 
        , scriptModel           = parseScriptModel           scriptElem
        , scriptParameters      = parseScriptParameters      scriptElem
        , scriptLocation        = parseScriptLocation        scriptElem
        , scriptToggles         = parseScriptToggles         scriptElem
        , scriptScoringFunction = parseScriptScoringFunction scriptElem
        , scriptScoreExtremes   = parseScriptScoreExtremes   scriptElem
        }
        
parseScriptToggles :: Element -> [Toggle]
parseScriptToggles scriptElem = map parseToggle toggleNames
    where parseToggle toggleName = Toggle toggleName (parseBool (parseMetaDataString toggleName scriptElem))
    
-- Functions to be exposed as an interface
-----------------------------------------------------

-- | Queries the given script for its ID.
parseScriptID :: ScriptElement -> ID
parseScriptID = parseMetaDataString "id"

-- | Queries the given script for its name.
parseScriptName :: ScriptElement -> Name
parseScriptName = parseMetaDataString "name"

-- | Queries the given script for its description.
parseScriptDescription :: ScriptElement -> String
parseScriptDescription = parseMetaDataString "description"

parseScriptLocation :: ScriptElement -> Name
parseScriptLocation = parseMetaDataString "location"

-- | Queries the given script for its difficulty.
parseScriptDifficulty :: ScriptElement -> Difficulty
parseScriptDifficulty scriptElem = errorOnFail errorMsg (readDifficulty difficultyString)
 where 
    difficultyString = parseMetaDataString "difficulty" scriptElem
    errorMsg = "Could not read difficulty: " ++ difficultyString

-- | Queries the given script for its banner image.
parseScriptBannerImage :: ScriptElement -> Maybe ID
parseScriptBannerImage scriptElem = nothingOnFail (
    findChild "metadata" scriptElem >>=
    findChild "bannerImage"         >>=
    findAttribute "extid")

-- | Queries the given script for its character image.
parseScriptCharacterImage :: ScriptElement -> Maybe ID
parseScriptCharacterImage scriptElem = nothingOnFail(
    findChild "metadata" scriptElem >>=
    findChild "characterImage"      >>=
    findAttribute "extid")

-- | Queries the given script for its model.
parseScriptModel :: ScriptElement -> Maybe ID
parseScriptModel scriptElem = nothingOnFail(
    findChild "metadata" scriptElem >>=
    findChild "model"               >>=
    findAttribute "extid")

-- | Queries the given script for its startId.
parseScriptStartId :: ScriptElement -> ID
parseScriptStartId scriptElem = getAttribute "idref" startElem
  where
    metaDataElem = getChild "metadata" scriptElem
    startElem    = getChild "start" metaDataElem
    

-- | Queries the given script for its parameters.
parseScriptParameters :: ScriptElement -> [Parameter]
parseScriptParameters scriptElem = map parseParameter (children parameterElem)
  where
    metaDataElem  = getChild "metadata" scriptElem
    parameterElem = getChild "parameters" metaDataElem

-- | Queries the given script for its scoring function.
parseScriptScoringFunction :: ScriptElement -> ScoringFunction
parseScriptScoringFunction scriptElem = parseScoringFunction (scoringFunctionChild (children scoringFunctionElem))
  where 
    metaDataElem = getChild "metadata" scriptElem
    scoringFunctionElem = getChild "scoringFunction" metaDataElem
    scoringFunctionChild [elem] = elem
    scoringFunctionChild _      = error "could not parse scoringFunction" 

-- | Queries the given script for its score extremes.
parseScriptScoreExtremes :: ScriptElement -> Maybe (Score, Score)
parseScriptScoreExtremes scriptElem = 
    findChild "metadata" scriptElem >>=
    findChild "scoreExtremes"       >>= 
    \scoreExtremesElem -> do
    minimumValue <- findAttribute "minimum" scoreExtremesElem
    maximumValue <- findAttribute "maximum" scoreExtremesElem
    return (read minimumValue :: Score, read maximumValue :: Score)

-- | Takes a statement and returns its type.
parseType :: Element -> StatementType
parseType statElem = read (applyToFirst toUpper (name statElem))

-- | Takes a statement element and returns its precondition, if it has one.
-- | Uses the monadic findChild for Maybe Monad
parseMaybePrecondition :: Element -> Maybe Condition
parseMaybePrecondition statElem =
    maybe Nothing (Just . parseConditionRoot) (findChild "preconditions" statElem)

-- | Parses media of the statement element
parseMedia :: Element -> Media
parseMedia statElem = Media (parseMediaVisuals statElem) (parseMediaAudios statElem)
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

    -- | Takes a statement and returns its visual media.
    parseMediaAudios :: Element -> [ID]
    parseMediaAudios statElem = map (getAttribute "extid") audioElems
      where audioElems =
                findChild "media" statElem >>= 
                findChild "audio"          >>= 
                children 
            
-- | Takes a statement and returns its intents.
parseIntents :: Element -> [String]
parseIntents statElem = map getData (findChild "intents" statElem >>= children)

-- | Takes a statement and returns its text.
parseText :: Element -> Either String [(ConversationTextType, String)]
parseText statElem =
    case name statElem of
        "conversation" -> Right (map toConversationText (filter (isSuffixOf "Text" . name) (children statElem)))
        _              -> Left (getData (getChild "text" statElem))
  where toConversationText textEl = (read (applyToFirst toUpper (name textEl)), getData textEl)

-- | Parses the feedback of the given statement element
parseFeedback :: StatElem -> Maybe String
parseFeedback statElem = nothingOnFail (findChild "feedback" statElem >>= return . getData)
 
-- | Takes a statement and returns the IDs of the statements following it. TODO!!!!
parseNextStatIDs :: StatElem -> [ID]
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

-- Functions to be used internally
------------------------------------------------------
parseDialogue :: ScriptElement -> Dialogue
parseDialogue scriptElem = map parseInterleaveLevel interleaveElems
  where
    sequenceElem = getChild "sequence" scriptElem
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
    , treeStartID    = getAttribute "idref" (getChild "start" treeElem)
    , treeStatements = parseStatements treeElem
    }

parseStatements :: Element -> [Statement]
parseStatements treeElem = playerStats ++ computerStats ++ conversation
  where
    playerStats   = map parseStatement (findChildren "playerStatement"   treeElem)
    computerStats = map parseStatement (findChildren "computerStatement" treeElem)
    conversation  = map parseStatement (findChildren "conversation"      treeElem)

parseStatement :: StatElem -> Statement
parseStatement statElem = 
    Statement
    { statID            = getAttribute "id"         statElem
    , statType          = parseType                 statElem       
    , statText          = parseText                 statElem
    , statPrecondition  = parseMaybePrecondition    statElem
    , statEffects       = parseEffects              statElem
    , jumpPoint         = parseJumpPoint            statElem
    , endOfConversation = parseEnd                  statElem
    , nextStatIDs       = parseNextStatIDs          statElem
    , statMedia         = parseMedia                statElem
    , statIntents       = parseIntents              statElem
    , statFeedback      = parseFeedback             statElem
    }

parseJumpPoint :: StatElem -> Bool    
parseJumpPoint statElem = parseBool (getAttribute "jumpPoint" statElem)

parseEnd :: StatElem -> Bool    
parseEnd statElem = parseBool (getAttribute "possibleEnd" statElem)

-- | Takes a statement element and returns its effects.
parseEffects :: StatElem -> [Effect]
parseEffects statElem = map parseEffect effectElems 
  where effectElems = children (getChild "effects" statElem)

parseEffect :: Element -> Effect
parseEffect effectElem = Effect
            { effectIdref      = getAttribute "idref" effectElem
            , effectChangeType = parseChangeType      effectElem
            , effectValue      = parseCompareValue    effectElem
            }

-- | Parses a string to a Changetype. Gives an exception on invalid input.
parseChangeType :: Element -> ChangeType
parseChangeType effectElem = read (applyToFirst toUpper changeTypeStr)
    where changeTypeStr = getAttribute "changeType" effectElem

-- | Parses a condition root if it contains exactly one condition.
parseConditionRoot :: Element -> Condition
parseConditionRoot conditionRootElem = parseCondition conditionElem
  where conditionElem = getChild "condition" conditionRootElem

-- | Parses a condition. Recursively parses Ands and Ors.
parseCondition :: Element -> Condition
parseCondition conditionElem = case name conditionElem of
    "and"       -> And (map parseCondition (children conditionElem))
    "or"        -> Or  (map parseCondition (children conditionElem))
    "condition" -> Condition $ ComparisonCondition
        { conditionIdref = getAttribute "idref" conditionElem
        , conditionTest  = parseCompareOperator conditionElem
        , conditionValue = parseCompareValue    conditionElem
        }

-- | Parses a compare operator. Gives an exception on invalid input.
parseCompareOperator :: Element -> CompareOperator
parseCompareOperator conditionElem = read (applyToFirst toUpper (getAttribute "test"  conditionElem))

-- | Parses a value in a condition.
parseCompareValue :: Element -> ParameterValue
parseCompareValue e = read (getAttribute "value" e)

-- | Parses a scoring function element.
parseScoringFunction :: Element -> ScoringFunction
parseScoringFunction scoringFunctionElem = case name scoringFunctionElem of
    "constant"           -> Constant            parseConstant
    "sum"                -> Sum                (map parseScoringFunction (children scoringFunctionElem))
    "scale"              -> Scale               parseScalar (parseScoringFunction paramElem)
    "paramRef"           -> ParamRef           (getAttribute "idref" scoringFunctionElem)
    "integeredCondition" -> IntegeredCondition (parseCondition conditionElem)
  where 
    parseConstant = read (getAttribute "value" scoringFunctionElem)  :: Score
    parseScalar   = read (getAttribute "scalar" scoringFunctionElem) :: Int
    conditionElem = getChild "condition" scoringFunctionElem         :: Element
    paramElem     = getChild "paramRef"  scoringFunctionElem         :: Element
            
-- | Parses a parameter Element inside the parameters inside the metadata of the script.
parseParameter :: Element -> Parameter
parseParameter paramElem = Parameter
        { parameterId           = getAttribute "id" paramElem
        , parameterName         = getAttribute "name" paramElem
        , parameterEmotion      = nothingOnFail (findAttribute "emotionid" paramElem)
        , parameterInitialValue = nothingOnFail (findAttribute "initialValue" paramElem >>= readM)
        , parameterScored       = parseMaybeBool (findAttribute "scored" paramElem)
        }
    
-- Functions that extend the XML parser
---------------------------------------------------------

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
    
    -- | Parses a Bool.
parseBool :: String -> Bool
parseBool boolStr = read (applyToFirst toUpper boolStr) :: Bool

parseMaybeBool :: Maybe String -> Bool
parseMaybeBool (Just boolStr) = parseBool boolStr
parseMaybeBool _              = False

-- | Queries the given script for basic information. Which information being queried is specified
--  in the "metaDataName". This could be the name of the script, the difficulty, date, etc.
parseMetaDataString :: Name -> ScriptElement -> String
parseMetaDataString metaDataName scriptElem = getData dataElem
  where 
    metadata = getChild "metadata" scriptElem
    dataElem = getChild metaDataName metadata



