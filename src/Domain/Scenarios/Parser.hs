-----------------------------------------------------------------------------
{- |
Basic functions to query an XML script as specified by the Communicate!-team. Some things to note are:

- This code doesn't do any error checking on the script. IO-exceptions and scripts deviating from the specifications
will lead to unspecified behaviour (most likely a Haskell-exception).

To-do list:

-add a "getScriptModel" function. Need more information about possible models for that.
-}
-----------------------------------------------------------------------------
module Domain.Scenarios.Parser
    ( Script
    , getScriptId, getScriptName, getScriptDate, getScriptDescription
    , getScriptDifficulty, getScriptStartId, getScriptParameters
    , getScriptScoringFunction, getScriptStatements
    , getType, getMaybePrecondition, getMedia, getEffects, getIntents, getText, getNexts
    , findStatement
    , parseScript
    ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

import Ideas.Common.Library
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

-- | Extracts all statements from the given script.
getScriptStatements :: Monad m => Script -> m [Statement]
getScriptStatements (Script scriptElem) = return $ catMaybes $ map getIfStatement $ children scriptElem
    where getIfStatement statement = getType (Statement statement) >> (Just $ Statement statement)

-- | Takes a statement and returns its type.
getType :: Monad m => Statement -> m StatementElementType
getType (Statement elemVar) = readM . applyToFirst toUpper . name $ elemVar

-- | Takes a statement and returns its precondition, if it has one.
getMaybePrecondition :: Monad m => Statement -> m (Maybe Condition)
getMaybePrecondition (Statement elemVar) = do
        maybe (return Nothing) (liftM Just . parseConditionRoot) $
            findChild "preconditions" elemVar

-- | Takes a statement and a media type and returns the media of that type
-- associated with that element.
getMedia :: Monad m => String -> Statement -> m [String]
getMedia mediaType (Statement elemVar) = return $
        childrenNamed "media" elemVar >>=
        childrenNamed mediaType >>=
        findAttribute "extid"

-- | Takes a statement and returns its effects.
getEffects :: Monad m => Statement -> m [Effect]
getEffects (Statement elemVar) = return $
        findChild "effects" elemVar >>=
        children >>=
        return . parseEffect

-- | Takes a statement and returns its intents.
getIntents :: Monad m => Statement -> m [String]
getIntents (Statement elemVar) = return $
        findChild "intents" elemVar >>=
        children >>=
        return . getData

-- | Takes a statement and returns its text.
getText :: Monad m => Statement -> m (Either String [(ConversationTextType, String)])
getText (Statement elemVar) =
        case name elemVar of
            "conversation" -> return $ Right $
                map toConversationText $ filter (isSuffixOf "Text" . name) $ children elemVar 
            _              -> do
                textElem <- findChild "text" elemVar
                return $ Left $ getData textElem
    where toConversationText textEl =
            (read $ applyToFirst toUpper $ name textEl, getData textEl)

-- | Takes a statement and returns the IDs of the statements following it.
getNexts :: Monad m => Statement -> m [String]
getNexts (Statement elemVar) = do
        case name elemVar of
            "conversation"      -> getResponses >>= getIdrefs
            "computerStatement" -> getResponses >>= getIdrefs
            "playerStatement"   -> getNextComputerStatements >>= getIdrefs
            _                   -> fail $
                "Cannot get nexts of statement represented by element named " ++ (name elemVar)
        where getIdrefs = mapM (findAttribute "idref")
              getResponses = liftM children $ findChild "responses" elemVar
              getNextComputerStatements =
                  case findChild "nextComputerStatements" elemVar of
                      Just nCSElem -> return $ children nCSElem
                      Nothing      -> liftM singleton $ findChild "nextComputerStatement" elemVar

-- | Takes a script and a statement or conversation ID and 
-- returns the corresponding element.
findStatement :: Monad m => Script -> String -> m Statement
findStatement (Script scriptElem) idVar = if null foundElems
            then fail $ "Cannot find statement with ID " ++ idVar
            else return $ Statement (head foundElems)
        where foundElems = filter (idAttributeIs idVar) (children scriptElem)
              idAttributeIs testId elemVar = maybe False ((==)testId) (findAttribute "id" elemVar)

-- | Takes a script, a statement element type and a statement or conversation ID and
-- returns the corresponding element.
findTypedStatement :: Script -> StatementElementType -> String -> Element
findTypedStatement (Script scriptElem) statementType idVar = head (filter
            (idAttributeIs idVar)
            (childrenNamed elementName scriptElem))
        where elementName = applyToFirst toLower $ show statementType
              idAttributeIs testId elemVar = maybe False ((==)testId) (findAttribute "id" elemVar)

-- | Parses the XML script at "filepath" to a Script.
parseScript :: String -> IO Script
parseScript filepath = do
    text <- readFile filepath
    either fail (return . Script) (parseXML text)

-- Functions to be used internally
------------------------------------------------------

-- | Parses an effect.
parseEffect :: Element -> Effect
parseEffect elemVar = Effect
            { effectIdref      = head (findAttribute "idref" elemVar)
            , effectChangeType = parseChangeType (head (findAttribute "changeType" elemVar))
            , effectValue      = parseCompareValue (head (findAttribute "value" elemVar))
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
parseCondition elemVar = case name elemVar of
        "and"       -> And $ map parseCondition (children elemVar)
        "or"        -> Or  $ map parseCondition (children elemVar)
        "condition" -> Condition $ ComparisonsCondition
                { conditionIdref = head (findAttribute "idref" elemVar)
                , conditionTest  = parseCompareOperator (head (findAttribute "test" elemVar))
                , conditionValue = parseCompareValue (head (findAttribute "value" elemVar))
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
        , parameterInitialValue = findAttribute "initialValue" paraElem >>= read
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

-- Definitions of data structures
---------------------------------------------------------

newtype Script = Script Element
instance HasId Script where
    getId script = either error id $ do
                scriptId <- getScriptId script
                scriptDescription <- getScriptDescription script
                return $ describe scriptDescription $ newId ("scenarios." ++ scriptId)
    changeId _ _ = error "It wouldn't be right to change a script's ID."

-- TODO: Fix this instance to create the full listed ID immediately
newtype Statement = Statement Element
instance HasId Statement where
    getId (Statement elemVar) = either error id $ do
                statementId <- findAttribute "id" elemVar
                return $ newId statementId
    changeId _ _ = error "It wouldn't be right to change a statement's ID."
