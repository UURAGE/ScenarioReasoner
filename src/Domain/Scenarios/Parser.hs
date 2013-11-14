-----------------------------------------------------------------------------
{- |
Basic functions to query an XML script as specified by the Communicate!-team. Some things to note are:

- This code doesn't do any error checking on the script. IO-exceptions and scripts deviating from the specifications
will lead to unspecified behaviour (most likely a Haskell-exception).

- Currently, only the 6 basic emotions as specified by Paul Ekman are supported as emotionid in the parameters;
changing the available emotions requires changing the Emotion data-type and the parseEmotion function. However,
unrecognized emotionids will be ignored, and will not lead to exceptions.

To-do list:

-add a "getScriptModel" function. Need more information about possible models for that.
-}
-----------------------------------------------------------------------------
module Domain.Scenarios.Parser
    ( Script
    , getScriptId, getScriptName, getScriptDate, getScriptDescription
    , getScriptDifficulty, getScriptStartId, getScriptParameters
    , getPreconditions, getMaybeVideoId, getEffects, getText, getNexts, findStatement
    , parseScript
    ) where

import Data.Char

import Ideas.Common.Library
import Ideas.Common.Utils (readM)
import Ideas.Text.XML.Interface

--functions to be exposed as an interface
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

-- | Takes a statement and returns its preconditions.
getPreconditions :: Statement -> Precondition
getPreconditions (Statement elemVar) = do
        parsePreconditions (childrenNamed "preconditions" elemVar)

-- | Takes a statement and returns the ID of the video if there is one. Else it returns "Nothing".
getMaybeVideoId :: Monad m => Statement -> m (Maybe String)
getMaybeVideoId (Statement elemVar) = case childrenNamed "video" elemVar of
        []  -> return Nothing
        b:_ -> findAttribute "extid" b >>= return . Just

-- | Takes a statement and returns its effects.
getEffects :: Monad m => Statement -> m [Effect]
getEffects (Statement elemVar) = do
        effects <- findChild "effects" elemVar
        return (map parseEffect (children effects))

-- | Takes a statement and returns its text.
getText :: Monad m => Statement -> m String
getText (Statement elemVar) = do
        case name elemVar of
            "conversation" -> return ""
            _              -> do
                textElem <- findChild "text" elemVar
                return (getData textElem)

-- | Takes a statement and returns the statements following it.
getNexts :: Monad m => Statement -> m [String]
getNexts (Statement elemVar) = do
        case name elemVar of
            "conversation"      -> getResponseIds getResponses
            "computerStatement" -> getResponseIds getResponses
            "playerStatement"   -> getResponseIds getNextComputerStatements
            _                   -> fail $
                "Cannot get nexts of statement represented by element named " ++ (name elemVar)
        where getResponseIds = mapM (findAttribute "idref")
              getResponses = findChild "responses" elemVar >>= children
              getNextComputerStatements =
                  case findChild "nextComputerStatements" elemVar of
                      Just nCSElem -> children nCSElem
                      Nothing      -> findChild "nextComputerStatement" elemVar >>= singleton

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

--functions to be used internally
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

-- | Parses preconditions. Empty preconditions gives an always true. 
parsePreconditions :: [Element] -> Precondition
parsePreconditions elemVar = case elemVar of
        []  -> AlwaysTrue
        a:_ -> case children a of
                []  -> AlwaysTrue
                b:_ -> parsePrecondition b

-- | Parses a precondition. Recursively parses Ands and Ors. Empty Ands and Ors gives AlwaysTrue.
parsePrecondition :: Element -> Precondition
parsePrecondition elemVar = case name elemVar of
        "and"          | recResult == [] -> AlwaysTrue
                       | otherwise       -> And recResult
                where recResult = map parsePrecondition (children elemVar)
        "or"           | recResult == [] -> AlwaysTrue
                       | otherwise       -> Or recResult
                where recResult = map parsePrecondition (children elemVar)
        "precondition" -> Condition $ ComparisonsPrecondition 
                { preconditionIdref = head (findAttribute "idref" elemVar)
                , preconditionTest  = parseCompareOperator (head (findAttribute "test" elemVar))
                , preconditionValue = parseCompareValue (head (findAttribute "value" elemVar))
                }

-- | Parses a compare operator. Gives an exception on invalid input.
parseCompareOperator :: String -> CompareOperator
parseCompareOperator = read . applyToFirst toUpper

-- | Parses a value in the precondition. This can be a bool or an int. Left isn't an error message.
parseCompareValue :: String -> Either Bool Int
parseCompareValue input = case input of
        "true" -> Left True
        "false" -> Left False
        _       -> Right ((read input) :: Int)

-- | Parses a parameter Element inside the parameters inside the metadata of the script.
parseParameterAttributes :: Element -> Parameter
parseParameterAttributes paraElem = Parameter
        { parameterId      = head (findAttribute "id" paraElem)
        , parameterEmotion = findAttribute "emotionid" paraElem >>= parseEmotion
        }

-- | Parses an emotion.
parseEmotion :: String -> Maybe Emotion
parseEmotion = readM . applyToFirst toUpper

-- | Queries the given script for basic information. Which information being queried is specified
--  in the "metaDataName". This could be the name of the script, the difficulty, date, etc.
getMetaDataString :: Monad m => String -> Script -> m String
getMetaDataString metaDataName (Script scriptElem) = do
        metadata          <- findChild "metadata" scriptElem
        dataElem          <- findChild metaDataName metadata
        return (getData dataElem)

-- | Applies a function to the first element of a list, if there is one.
applyToFirst :: (a -> a) -> [a] -> [a]
applyToFirst f (x:xs) = (f x) : xs
applyToFirst _ [] = []

--functions added to the XML-parser
---------------------------------------------------------

-- | Returns all children of the given element with the given name.
childrenNamed :: String -> Element -> [Element]
childrenNamed s e = filter ((==s) . name) (children e)

--data structures definitions
---------------------------------------------------------

newtype Script = Script Element
newtype Statement = Statement Element

--datatypes used when parsing preconditions
data Precondition = And [Precondition] | Or [Precondition] | Condition ComparisonsPrecondition | AlwaysTrue deriving (Show, Eq)
data ComparisonsPrecondition = ComparisonsPrecondition
        { preconditionIdref :: String
        , preconditionTest  :: CompareOperator
        , preconditionValue :: ParameterValue
        } deriving (Show, Eq)
data CompareOperator = LessThan | LessThanEqualTo | EqualTo | GreaterThanEqualTo | GreaterThan deriving (Show, Eq, Read)
type ParameterValue = Either Bool Int

--datatypes used when parsing parameters
data Parameter = Parameter
        { parameterId      :: String
        , parameterEmotion :: Maybe Emotion
        } deriving (Show, Eq)
data Emotion =  Anger | Disgust | Fear | Happiness | Sadness | Surprise deriving (Show, Eq, Read)

--datatypes for statement elements
data StatementElementType = ComputerStatement | PlayerStatement | Conversation
    deriving (Show, Eq)

--datatypes used when parsing effects in the playerstatement
data Effect = Effect
        { effectIdref      :: String
        , effectChangeType :: ChangeType
        , effectValue      :: ParameterValue
        } deriving (Show, Eq)
data ChangeType = Set | Delta deriving (Show, Eq, Read)

-- code used for testing purposes only
---------------------------------------------------------
-- | The relative filepath to the test script XML file on my (wouters) computer.
testFilepath :: String
testFilepath = "exampleScript.xml"

getTestPreconditions :: IO Precondition
getTestPreconditions = do
        Script scriptElem <- parseScript testFilepath
        pStatement        <- findChild "computerStatement" scriptElem
        return (getPreconditions (Statement pStatement))
