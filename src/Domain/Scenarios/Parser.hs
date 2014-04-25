-----------------------------------------------------------------------------
{- |
Basic functions to query a ScriptLanguage XML script as specified by the
Communicate!-team.

This code doesn't do any error checking on the script. Scripts deviating
from the specifications may lead to unspecified behaviour (most likely
a Haskell-exception).
-}
-----------------------------------------------------------------------------
module Domain.Scenarios.Parser
    ( Script
    , getScriptId, getScriptName, getScriptDate, getScriptDescription, getScriptDifficulty
    , getScriptModel, getScriptBannerImage, getScriptCharacterImage
    , getScriptStartId, getScriptParameters
    , getScriptScoringFunction, getScriptScoreExtremes, getScriptStatements
    , getType, getMaybePrecondition, getMediaVisuals, getMediaAudios
    , getEffects, getIntents, getFeedback, getText, getNexts
    , findStatement
    , parseScript
    , createFullId
    ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.IO (withBinaryFile, hGetContents, IOMode(..))

import Ideas.Common.Library hiding (Sum)
import Ideas.Common.Utils (readM)
import Ideas.Text.XML.Interface

import Domain.Scenarios.Types

-- Functions to be exposed as an interface
-----------------------------------------------------

-- | Queries the given script for its ID.
getScriptId :: Monad m => Script -> m String
getScriptId = getMetaDataString "id"

-- | Queries the given script for its name.
getScriptName :: Monad m => Script -> m String
getScriptName = getMetaDataString "name"

-- | Queries the given script for its date.
getScriptDate :: Monad m => Script -> m String
getScriptDate = getMetaDataString "date"

-- | Queries the given script for its description.
getScriptDescription :: Monad m => Script -> m String
getScriptDescription = getMetaDataString "description"

-- | Queries the given script for its difficulty.
getScriptDifficulty :: Monad m => Script -> m Difficulty
getScriptDifficulty script = do
    difficultyString <- getMetaDataString "difficulty" script
    maybe (fail $ "Could not read difficulty " ++ difficultyString) return $ readDifficulty difficultyString

-- | Queries the given script for its banner image.
getScriptBannerImage :: Monad m => Script -> m (Maybe String)
getScriptBannerImage (Script scriptElem) = return $
    findChild "metadata" scriptElem >>=
    findChild "bannerImage" >>=
    findAttribute "extid"

-- | Queries the given script for its character image.
getScriptCharacterImage :: Monad m => Script -> m (Maybe String)
getScriptCharacterImage (Script scriptElem) = return $
    findChild "metadata" scriptElem >>=
    findChild "characterImage" >>=
    findAttribute "extid"

-- | Queries the given script for its model.
getScriptModel :: Monad m => Script -> m (Maybe String)
getScriptModel (Script scriptElem) = return $
    findChild "metadata" scriptElem >>=
    findChild "model" >>=
    findAttribute "extid"

-- | Queries the given script for its startId.
getScriptStartId :: Monad m => Script -> m String
getScriptStartId (Script scriptElem) = do
    metadata          <- findChild "metadata" scriptElem
    dataElem          <- findChild "start" metadata
    findAttribute "idref" dataElem

-- | Queries the given script for its parameters.
getScriptParameters :: Monad m => Script -> m [Parameter]
getScriptParameters (Script scriptElem) = do
    metadata          <- findChild "metadata" scriptElem
    parameterData     <- findChild "parameters" metadata
    return (map parseParameterAttributes (children parameterData))

-- | Queries the given script for its scoring function.
getScriptScoringFunction :: Monad m => Script -> m ScoringFunction
getScriptScoringFunction (Script scriptElem) =
    findChild "metadata" scriptElem >>=
    findChild "scoringFunction" >>=
    getExactlyOne "scoring function" . children >>=
    parseScoringFunction

-- | Queries the given script for its score extremes.
getScriptScoreExtremes :: Monad m => Script -> m (Maybe (Int, Int))
getScriptScoreExtremes (Script scriptElem) = return $
    findChild "metadata" scriptElem >>=
    findChild "scoreExtremes" >>= \scoreExtremesElem -> do
    minimumValue <- findAttribute "minimum" scoreExtremesElem
    maximumValue <- findAttribute "maximum" scoreExtremesElem
    return (read minimumValue, read maximumValue)

-- | Extracts all statements from the given script.
getScriptStatements :: Monad m => Script -> m [Statement]
getScriptStatements (Script scriptElem) = return $ catMaybes $ map getIfStatement $ children scriptElem
    where getIfStatement statement = getType (Statement statement) >> (Just $ Statement statement)

-- | Takes a statement and returns its type.
getType :: Monad m => Statement -> m StatementElementType
getType (Statement element) = readM . applyToFirst toUpper . name $ element

-- | Takes a statement and returns its precondition, if it has one.
getMaybePrecondition :: Monad m => Statement -> m (Maybe Condition)
getMaybePrecondition (Statement element) =
    maybe (return Nothing) (liftM Just . parseConditionRoot) $ findChild "preconditions" element

-- | Takes a statement and returns its visual media.
getMediaVisuals :: Monad m => Statement -> m [(String, String)]
getMediaVisuals (Statement element) = return $
    childrenNamed "media" element >>=
    findChild "visuals" >>=
    children >>=
    parseVisual

-- | Takes a statement and returns its visual media.
getMediaAudios :: Monad m => Statement -> m [String]
getMediaAudios (Statement element) = return $
    childrenNamed "media" element >>=
    findChild "audios" >>=
    children >>=
    findAttribute "extid"

-- | Takes a statement and returns its effects.
getEffects :: Monad m => Statement -> m [Effect]
getEffects (Statement element) = return $
    findChild "effects" element >>=
    children >>=
    return . parseEffect

-- | Takes a statement and returns its intents.
getIntents :: Monad m => Statement -> m [String]
getIntents (Statement element) = return $
    findChild "intents" element >>=
    children >>=
    return . getData

-- | Takes a statement and returns its text.
getText :: Monad m => Statement -> m (Either String [(ConversationTextType, String)])
getText (Statement element) =
    case name element of
        "conversation" -> return $ Right $
            map toConversationText $ filter (isSuffixOf "Text" . name) $ children element 
        _              -> do
            textElem <- findChild "text" element
            return $ Left $ getData textElem
    where toConversationText textEl =
            (read $ applyToFirst toUpper $ name textEl, getData textEl)

getFeedback :: Monad m => Statement -> m (Maybe String)
getFeedback (Statement element) = return $
    findChild "feedback" element >>=
    return . getData

-- | Takes a statement and returns the IDs of the statements following it.
getNexts :: Monad m => Statement -> m [String]
getNexts (Statement element) = do
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
                  Just nCSElem -> return $ children nCSElem
                  Nothing      -> liftM singleton $ findChild "nextComputerStatement" element

-- | returns the start nodes of al trees in a script grouped by interleave level.
getTrees :: Monad m => Script -> m [[String]]
getTrees = undefined

-- | Takes a script and a statement or conversation ID and 
-- returns the corresponding element.
findStatement :: Monad m => Script -> String -> m Statement
findStatement (Script scriptElem) idVar = if null foundElems
        then fail $ "Cannot find statement with ID " ++ idVar
        else return $ Statement (head foundElems)
    where foundElems = filter (idAttributeIs idVar) (children scriptElem)
          idAttributeIs testId element = maybe False ((==)testId) (findAttribute "id" element)

-- | Takes a script, a statement element type and a statement or conversation ID and
-- returns the corresponding element.
findTypedStatement :: Script -> StatementElementType -> String -> Element
findTypedStatement (Script scriptElem) statementType idVar = head (filter
        (idAttributeIs idVar)
        (childrenNamed elementName scriptElem))
    where elementName = applyToFirst toLower $ show statementType
          idAttributeIs testId element = maybe False ((==)testId) (findAttribute "id" element)

-- | Parses the XML script at "filepath" to a Script. hGetContent is NOT lazy.
parseScript :: String -> IO Script
parseScript filepath = do
    withBinaryFile filepath ReadMode $ \h ->
      hGetContents h >>= either fail (return . Script) . parseXML

-- Functions to be used internally
------------------------------------------------------

-- | Parses a visual (video or image).
parseVisual :: Monad m => Element -> m (String, String)
parseVisual element = do
    extid <- findAttribute "extid" element
    return (name element, extid)

-- | Parses an effect.
parseEffect :: Element -> Effect
parseEffect element = Effect
            { effectIdref      = head (findAttribute "idref" element)
            , effectChangeType = parseChangeType (head (findAttribute "changeType" element))
            , effectValue      = parseCompareValue (head (findAttribute "value" element))
            }

-- | Parses a string to a Changetype. Gives an exception on invalid input.
parseChangeType :: String -> ChangeType
parseChangeType = read . applyToFirst toUpper

-- | Parses a condition root if it contains exactly one condition.
parseConditionRoot :: Monad m => Element -> m Condition
parseConditionRoot conditionRootElem =
    getExactlyOne "condition" (children conditionRootElem) >>=
    return . parseCondition

-- | Parses a condition. Recursively parses Ands and Ors.
parseCondition :: Element -> Condition
parseCondition element = case name element of
    "and"       -> And $ map parseCondition (children element)
    "or"        -> Or  $ map parseCondition (children element)
    "condition" -> Condition $ ComparisonsCondition
            { conditionIdref = head (findAttribute "idref" element)
            , conditionTest  = parseCompareOperator (head (findAttribute "test" element))
            , conditionValue = parseCompareValue (head (findAttribute "value" element))
            }

-- | Parses a compare operator. Gives an exception on invalid input.
parseCompareOperator :: String -> CompareOperator
parseCompareOperator = read . applyToFirst toUpper

-- | Parses a value in a condition.
parseCompareValue :: String -> Int
parseCompareValue input = read input

-- | Parses a scoring function element.
parseScoringFunction :: Monad m => Element -> m ScoringFunction
parseScoringFunction scoringFunctionElem = case name scoringFunctionElem of
    "constant"           -> liftM Constant $ findAttribute "value" scoringFunctionElem >>= readM
    "sum"                -> liftM Sum $ mapM parseScoringFunction $ children scoringFunctionElem
    "scale"              -> do
            scalarEl          <- findAttribute "scalar" scoringFunctionElem
            scalar            <- readM scalarEl
            scoringFunctionEl <- getExactlyOne "scoringFunction" (children scoringFunctionElem)
            scoringFunction   <- parseScoringFunction scoringFunctionEl
            return $ Scale scalar scoringFunction
    "paramRef"           -> liftM ParamRef $ findAttribute "idref" scoringFunctionElem
    "integeredCondition" -> do
            conditionEl       <- getExactlyOne "condition" (children scoringFunctionElem)
            return $ IntegeredCondition $ parseCondition conditionEl
    otherName            -> fail $ "Cannot parse scoring function element named " ++ otherName

-- | Parses a parameter Element inside the parameters inside the metadata of the script.
parseParameterAttributes :: Element -> Parameter
parseParameterAttributes paraElem = Parameter
        { parameterId           = head (findAttribute "id" paraElem)
        , parameterName         = fromMaybe (head (findAttribute "id" paraElem)) (findAttribute "name" paraElem)
        , parameterEmotion      = findAttribute "emotionid" paraElem
        , parameterInitialValue = findAttribute "initialValue" paraElem >>= readM
        , parameterScored       = fromMaybe False $ findAttribute "scored" paraElem >>= parseBool
        }

-- | Parses a Bool.
parseBool :: String -> Maybe Bool
parseBool = readM . applyToFirst toUpper

-- | Queries the given script for basic information. Which information being queried is specified
--  in the "metaDataName". This could be the name of the script, the difficulty, date, etc.
getMetaDataString :: Monad m => String -> Script -> m String
getMetaDataString metaDataName (Script scriptElem) = do
    metadata          <- findChild "metadata" scriptElem
    dataElem          <- findChild metaDataName metadata
    return (getData dataElem)

-- | Gets exactly one item from a list and fails appropriately if there are more or fewer.
getExactlyOne :: Monad m => String -> [a] -> m a
getExactlyOne iDescription is = case is of
    [i] -> return i
    []   -> fail $ "needed exactly one " ++ iDescription ++ ", got zero"
    _    -> fail $ "needed exactly one " ++ iDescription ++ ", got " ++ (show (length is))

-- Functions that extend the XML parser
---------------------------------------------------------

-- | Returns all children of the given element with the given name.
childrenNamed :: String -> Element -> [Element]
childrenNamed s e = filter ((==s) . name) (children e)

-- Definitions of data structures and related functions
---------------------------------------------------------

newtype Script = Script Element
instance HasId Script where
    getId script = either error id $ do
                scriptId <- getScriptId script
                scriptDescription <- getScriptDescription script
                return $ describe scriptDescription $ "scenarios" # scriptId
    changeId _ _ = error "The ID of a Script is determined externally."

newtype Statement = Statement Element
instance HasId Statement where
    getId statement@(Statement element) = either error id $ do
                statementId <- findAttribute "id" element
                statementText <- getText statement
                let statementDescription = either id (intercalate " // " . map snd) statementText
                return $ describe statementDescription $ newId statementId
    changeId _ _ = error "The ID of a Statement is determined externally."

-- | Creates the full ID for the given statement in the context of the given script.
createFullId :: Script -> Statement -> Id
createFullId script statement = scriptId # typeSegment # idSegment
    where scriptId = getId script
          typeSegment = toIdTypeSegment $ fromJust $ getType statement
          idSegment = getId statement
