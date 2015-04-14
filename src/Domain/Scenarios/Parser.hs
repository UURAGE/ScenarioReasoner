{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-} 
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
import Domain.Scenarios.TypeDefs

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
        }
        
data Toggle = Toggle Name Bool
    deriving (Show)

toggleNames :: [Name]
toggleNames = ["showscore"    --score at the end of the game
              ,"showfeedback" --feedback at the end of the game
              , "feedback"]   --feedback during the game
              
parseMetaData :: ScriptElem -> MetaData
parseMetaData scriptElem = MetaData    
        { scriptID             = parseScriptID              scriptElem
        , scriptName           = parseScriptName            scriptElem
        , scriptDescription    = parseScriptDescription     scriptElem
        , scriptDifficulty     = parseScriptDifficulty      scriptElem
        , scriptBannerImage    = parseScriptBannerImage     scriptElem
        , scriptCharacterImage = parseScriptCharacterImage  scriptElem 
        , scriptModel          = parseScriptModel           scriptElem
        , scriptParameters     = parseScriptParameters      scriptElem
        , scriptLocation       = parseScriptLocation        scriptElem
        , scriptToggles        = parseScriptToggles         scriptElem
        , scriptScoringFunction = 
        , scriptScoreExtremes =
        }
        
parseScriptToggles :: Element -> [Toggle]
parseScriptToggles scriptElem = map parseToggle toggleNames
    where parseToggle toggleName = Toggle toggleName (parseBool (parseMetaDataString toggleName scriptElem))
        
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
        , statFeedback      :: String
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

    
-- Functions to be exposed as an interface
-----------------------------------------------------

-- | Queries the given script for its ID.
parseScriptID :: ScriptElem -> ID
parseScriptID = parseMetaDataString "id"

-- | Queries the given script for its name.
parseScriptName :: ScriptElem -> Name
parseScriptName = parseMetaDataString "name"

-- | Queries the given script for its description.
parseScriptDescription :: ScriptElem -> String
parseScriptDescription = parseMetaDataString "description"

parseScriptLocation :: ScriptElem -> Name
parseScriptLocation = parseMetaDataString "location"

-- | Queries the given script for its difficulty.
parseScriptDifficulty :: ScriptElem -> Difficulty
parseScriptDifficulty scriptElem = errorOnFail errorMsg (readDifficulty difficultyString)
 where 
    difficultyString = parseMetaDataString "difficulty" scriptElem
    errorMsg = "Could not read difficulty: " ++ difficultyString

-- | Queries the given script for its banner image.
parseScriptBannerImage :: ScriptElem -> Maybe ID
parseScriptBannerImage scriptElem = nothingOnFail (
    findChild "metadata" scriptElem >>=
    findChild "bannerImage"         >>=
    findAttribute "extid")

-- | Queries the given script for its character image.
parseScriptCharacterImage :: ScriptElem -> Maybe ID
parseScriptCharacterImage scriptElem = nothingOnFail(
    findChild "metadata" scriptElem >>=
    findChild "characterImage"      >>=
    findAttribute "extid")

-- | Queries the given script for its model.
parseScriptModel :: ScriptElem -> Maybe ID
parseScriptModel scriptElem = nothingOnFail(
    findChild "metadata" scriptElem >>=
    findChild "model"               >>=
    findAttribute "extid")

-- | Queries the given script for its startId.
parseScriptStartId :: ScriptElem -> ID
parseScriptStartId scriptElem = getAttribute "idref" startElem
  where
    metaDataElem = getChild "metadata" scriptElem
    startElem    = getChild "start" metaDataElem
    

-- | Queries the given script for its parameters.
parseScriptParameters :: ScriptElem -> [Parameter]
parseScriptParameters scriptElem = map parseParameter (children parameterElem)
  where
    metaDataElem  = getChild "metadata" scriptElem
    parameterElem = getChild "parameters" metaDataElem

-- | Queries the given script for its scoring function.
parseScriptScoringFunction :: ScriptElem -> ScoringFunction
parseScriptScoringFunction scriptElem = parseScoringFunction (getChild "sum" scoringFunctionElem)
  where 
    metaDataElem = getChild "metadata" scriptElem
    scoringFunctionElem = getChild "scoringFunction" metaDataElem

-- | Queries the given script for its score extremes.
parseScriptScoreExtremes :: ScriptElem -> Maybe (Score, Score)
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
parseFeedback :: StatElem -> String
parseFeedback statElem = getData (getChild "feedback" statElem)
 
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


-- | Takes a script and a statement or conversation ID and
-- returns the corresponding element.
findStatement :: Monad m => ScriptElem -> String -> m Element
findStatement scriptElem idVar = if null foundElems
                                            then fail $ "Cannot find statement with ID " ++ idVar
                                            else return $ head foundElems
                                          where
                                            foundElems = concat [findStatementAt x idVar | x <- children scriptElem]

    {-if null foundElems
        then fail $ "Cannot find statement with ID " ++ idVar
        else return $ Statement (head foundElems)
    where foundElems = filter (idAttributeIs idVar) childElems
          childElems = children scriptElem
          idAttributeIs testId element = maybe False ((==)testId) (findAttribute "id" element)-}

findStatementAt :: Element -> String -> [Element]
findStatementAt scriptElem idVar =  if maybe False ((==) idVar) (findAttribute "id" scriptElem)
                                        then (scriptElem : filteredChildren)
                                        else filteredChildren
                                    where
                                        childElems = children scriptElem
                                        filteredChildren = if null childElems
                                                              then []
                                                              else concat [findStatementAt x idVar | x <- children scriptElem]

-- | Parses the XML script at "filepath" to a ScriptElem. hGetContent is NOT lazy.
parseScriptElem :: String -> IO ScriptElem
parseScriptElem filepath = do
    withBinaryFile filepath ReadMode $ \h ->
      hGetContents h >>= either fail return . parseXML
                        -- if parameter is Left a, do fail a, if it is Right b do (return . ScriptElem) . parseXML b
                        
-- | Parses the script element
parseScript :: ScriptElem -> Script
parseScript scriptElem = Script (parseMetaData scriptElem) (parseDialogue scriptElem)
                        

-- Functions to be used internally
------------------------------------------------------
parseDialogue :: ScriptElem -> Dialogue
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
    "scale"              -> Scale               parseScalar (parseScoringFunction childElem)
    "paramRef"           -> ParamRef           (getAttribute "idref" scoringFunctionElem)
    "integeredCondition" -> IntegeredCondition (parseCondition conditionElem)
  where 
    parseConstant = read (getAttribute "value" scoringFunctionElem)  :: Score
    parseScalar   = read (getAttribute "scalar" scoringFunctionElem) :: Int
    childElem     = getChild "scoringFunction" scoringFunctionElem   :: Element
    conditionElem = getChild "condition" scoringFunctionElem         :: Element
            
-- | Parses a parameter Element inside the parameters inside the metadata of the script.
parseParameter :: Element -> Parameter
parseParameter paramElem = Parameter
        { parameterId           = getAttribute "id" paramElem
        , parameterName         = getAttribute "name" paramElem
        , parameterEmotion      = nothingOnFail (findAttribute "emotionid" paramElem)
        , parameterInitialValue = read (getAttribute "initialValue" paramElem) :: Int
        , parameterScored       = parseMaybeBool (getAttribute "scored" paramElem)
        }

-- | Parses a Bool.
parseBool :: String -> Bool
parseBool boolStr = errorOnFail "Failed to parse bool" (readM boolStr)

parseMaybeBool :: String -> Bool
parseMaybeBool boolStr = fromMaybe False (Just (boolStr == "true"))

-- | Queries the given script for basic information. Which information being queried is specified
--  in the "metaDataName". This could be the name of the script, the difficulty, date, etc.
parseMetaDataString :: Name -> ScriptElem -> String
parseMetaDataString metaDataName scriptElem = getData dataElem
  where 
    metadata = getChild "metadata" scriptElem
    dataElem = getChild metaDataName metadata

    
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

-- Definitions of data structures and related functions
---------------------------------------------------------

type ScriptElem = Element
type TreeElem = Element
type StatElem = Element

instance HasId ScriptElem where
    getId scriptElem = either error id $ do
                let id = parseScriptID scriptElem
                let descr = parseScriptDescription scriptElem
                return $ describe descr $ "scenarios" # id
    changeId _ _ = error "The ID of a ScriptElem is determined externally."

instance HasId Script where
    getId (Script metadata _) = either error id $ do
                let id = scriptID metadata
                let descr = scriptDescription metadata
                return $ describe descr $ "scenarios" # id
    changeId _ _ = error "The ID of a ScriptElem is determined externally."

instance HasId Statement where
    getId statement = either error id $ do
                let statementId = statID statement
                let statementText = statText statement
                let statementDescription = either id (intercalate " // " . map snd) statementText
                return $ describe statementDescription $ newId statementId
    changeId _ _ = error "The ID of a Statement is determined externally."
    

-- | Creates the full ID for the given statement in the context of the given script.
createFullId :: Script -> Statement -> Id
createFullId script statement = scriptId # typeSegment # statId # interleaveSegment
  where 
    scriptId = getId script
    typeSegment = toIdTypeSegment $ statType statement
    statId = statID statement 
    
    nextIDs = nextStatIDs statement
    
    interleaveSegment | jumpPoint statement                                 = "interleaved"
                      | not (endOfConversation statement) && (null nextIDs) = "interleaved"
                      | otherwise                                           = ""
                      
-- | Returns the value to be used to represent a statement type in a rule ID.
toIdTypeSegment :: StatementType -> String
toIdTypeSegment = takeWhile isLower . applyToFirst toLower . show

-- | Extra error function for getting a type out of Monad
errorOnFail :: String -> Maybe a -> a
errorOnFail errorMsg ma = fromMaybe (error errorMsg) ma

emptyOnFail :: Maybe [a] -> [a]
emptyOnFail = fromMaybe []

nothingOnFail :: Maybe a -> Maybe a
nothingOnFail (Just a) = Just a
nothingOnFail _        = Nothing

-- | Applies a function to the first element of a list, if there is one.
applyToFirst :: (a -> a) -> [a] -> [a]
applyToFirst _ []     = []
applyToFirst f (x:xs) = (f x) : xs

findScript :: String -> [ScriptElem] -> Exercise a -> ScriptElem
findScript usage scripts ex =
    case filter (\testScript -> (getId testScript) == (getId ex)) scripts of
            [foundScript] -> foundScript
            _             ->
                error $ "Cannot " ++ usage ++ " exercise: exercise is apparently not a Scenario."

