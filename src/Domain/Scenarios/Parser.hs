module Domain.Scenarios.Parser
    ( Script
    , getScriptId, getScriptName, getScriptDate, getScriptDescription
    , getScriptDifficulty, getScriptStartId, getScriptParameters
    , getPreconditions, getMaybeVideoId, getEffects, getText, getNexts, findStatement
    , parseScript
    ) where

import Ideas.Common.Library
import Ideas.Text.XML.Interface

import Data.Char

{-
The purpose of this source-code document is to supply basic functions to query an XML script as specified
by the Communicate!-team. Some things to note are:

- this code doesn't do any error checking on the script. IO-exceptions and scripts deviating from the specifications
will lead to unspecified behaviour (most likely a Haskell-exception).
- currently only the 6 basic emotions as specified by Paul Ekman are supported as emotionid in the parameters,
changing the available emotions requires changing the Emotion data-type and the parseEmotion function. However
unrecognized emotionids will be ignored, and will not lead to exceptions.

to-do list:
-add a "getScriptModel" function. Need more information about possible models for that.

-}


--functions to be exposed as an interface
-----------------------------------------------------

--queries the given script for its ID.
getScriptId :: Monad m => Script -> m String
getScriptId = getMetaDataString "id"

--queries the given script for its name.
getScriptName :: Monad m => Script -> m String
getScriptName = getMetaDataString "name"

--queries the given script for its date.
getScriptDate :: Monad m => Script -> m String
getScriptDate = getMetaDataString "date"

--queries the given script for its description.
getScriptDescription :: Monad m => Script -> m String
getScriptDescription = getMetaDataString "description"

--queries the given script for its difficulty.
getScriptDifficulty :: Monad m => Script -> m Difficulty
getScriptDifficulty script = do
    difficultyString <- getMetaDataString "difficulty" script
    maybe (fail $ "Could not read difficulty " ++ difficultyString) return $ readDifficulty difficultyString

--queries the given script for its startId
getScriptStartId :: Monad m => Script -> m String
getScriptStartId (Script scriptElem) = do
        metadata          <- findChild "metadata" scriptElem
        dataElem          <- findChild "start" metadata
        findAttribute "idref" dataElem

--queries the given script for its parameters
getScriptParameters :: Monad m => Script -> m [Parameter]
getScriptParameters (Script scriptElem) = do
        metadata          <- findChild "metadata" scriptElem
        parameterData     <- findChild "parameters" metadata
        return (map parseParameterAttributes (children parameterData))

--Takes a playerstatement or an computerstatement element and returns the preconditions.
getPreconditions :: Statement -> Precondition
getPreconditions (Statement elemVar) = do
        parsePreconditions (findAllChildren "preconditions" elemVar)

--Returns the id of the video tag of the computer- or playerstatement if there is one. Else it returns "Nothing".
getMaybeVideoId :: Monad m => Statement -> m (Maybe String)
getMaybeVideoId (Statement elemVar) = return $ case findAllChildren "video" elemVar of
        []  -> Nothing
        b:_ -> findAttribute "extid" b >>= Just

--Returns the effects of a playerstatement
getEffects :: Monad m => Statement -> m [Effect]
getEffects (Statement elemVar) = do
        effects <- findChild "effects" elemVar
        return (map parseEffect (children effects))

--Returns the text of a playerstatement or a computerstatement
getText :: Monad m => Statement -> m String
getText (Statement elemVar) = do
        case name elemVar of
            "conversation" -> return ""
            _              -> do
                textElem <- findChild "text" elemVar
                return (getData textElem)

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

--Takes a script and a playerstatement id, a computerstatement id or a conversation id and then 
--returns the corresponding element.
findStatement :: Monad m => Script -> String -> m Statement
findStatement (Script scriptElem) idVar = if null foundElems
            then fail $ "Cannot find statement with ID " ++ idVar
            else return $ Statement (head foundElems)
        where foundElems = filter (idAttributeIs idVar) (children scriptElem)
              idAttributeIs testId elemVar = maybe False ((==)testId) (findAttribute "id" elemVar)

getTypedStatement :: Script -> StatementElementType -> String -> Element
getTypedStatement (Script scriptElem) statementType idVar = head (filter
            (idAttributeIs idVar)
            (findAllChildren elementName scriptElem))
        where elementName = applyToFirst toLower $ show statementType
              applyToFirst f (x:xs) = (f x) : xs
              applyToFirst _ [] = []
              idAttributeIs testId elemVar = (head (findAttribute "id" elemVar)) == testId

--parses the XML script at "filepath" to a Script.
parseScript :: String -> IO Script
parseScript filepath = do
    text <- readFile filepath
    either fail (return . Script) (parseXML text)

--functions to be used internally
------------------------------------------------------

--parses an effect.
parseEffect :: Element -> Effect
parseEffect elemVar = Effect parseIdref getChangetype parseValue
        where parseIdref      = head (findAttribute "idref" elemVar)
              getChangetype = parseChangeType (head (findAttribute "changeType" elemVar))
              parseValue      = parseCompareValue (head (findAttribute "value" elemVar))

--parses a string to a Changetype. Crashes on invalid input.
parseChangeType :: String -> ChangeType
parseChangeType input = case input of
        "set"   -> Set
        "delta" -> Delta

--parses preconditions. Empty preconditions gives an always true. 
parsePreconditions :: [Element] -> Precondition
parsePreconditions elemVar = case elemVar of
        []  -> AlwaysTrue
        a:_ -> case children a of
                []  -> AlwaysTrue
                b:_ -> parsePrecondition b

--parses a precondition. Recursively parses Ands and Ors. Empty Ands and Ors gives AlwaysTrue.
parsePrecondition :: Element -> Precondition
parsePrecondition elemVar = case name elemVar of
        "and"          | recResult == [] -> AlwaysTrue
                       | otherwise       -> And recResult
                where recResult = map parsePrecondition (children elemVar)
        "or"           | recResult == [] -> AlwaysTrue
                       | otherwise       -> Or recResult
                where recResult = map parsePrecondition (children elemVar)
        "precondition" -> Condition (ComparisonsPrecondition parseParameter parseTest parseValue)
          where parseParameter = head (findAttribute "idref" elemVar)
                parseTest      = parseCompareOperator (head (findAttribute "test" elemVar))
                parseValue     = parseCompareValue (head (findAttribute "value" elemVar))

--Parses a compare operator. Gives an exception on invalid input.
parseCompareOperator :: String -> CompareOperator
parseCompareOperator input = case input of
        "greaterThan"      -> GreaterThan
        "greaterThanEqual" -> GreaterThanEqualTo
        "equal"            -> EqualTo
        "lessThanEqual"    -> LessThanEqualTo
        "lessThan"         -> LessThan

-- Parses a value in the precondition. This can be a bool or an int. Left isn't an error message.
parseCompareValue :: String -> Either Bool Int
parseCompareValue input = case input of
        "true" -> Left True
        "false" -> Left False
        _       -> Right ((read input) :: Int)
--parses a parameter Element inside the parameters inside the metadata of the script.
parseParameterAttributes :: Element -> Parameter
parseParameterAttributes paraElem = Parameter parsedId parsedEmotionId
        where parsedId       = head (findAttribute "id" paraElem)
              parsedEmotionId = case findAttribute "emotionid" paraElem of
                []   -> Nothing
                b:_  -> parseEmotion b
                
parseEmotion :: String -> Maybe Emotion
parseEmotion emotionString = case emotionString of
        "Anger"     -> Just Anger
        "Disgust"   -> Just Disgust
        "Fear"      -> Just Fear
        "Happiness" -> Just Happiness
        "Sadness"   -> Just Sadness
        "Surprise"  -> Just Surprise
        _           -> Nothing

--queries the given script for basic information. Which information being queried is specified
--in the "metaDataName". This could be the name of the script, the difficulty, date, etc.
getMetaDataString :: Monad m => String -> Script -> m String
getMetaDataString metaDataName (Script scriptElem) = do
        metadata          <- findChild "metadata" scriptElem
        dataElem          <- findChild metaDataName metadata
        return (getData dataElem)

--functions added to the XML-parser
---------------------------------------------------------

findAllChildren :: String -> Element -> [Element]
findAllChildren s e = filter ((==s) . name) (children e)


--data structures definitions
---------------------------------------------------------

newtype Script = Script Element
newtype Statement = Statement Element

--datatypes used when parsing preconditions
data Precondition = And [Precondition] | Or [Precondition] | Condition ComparisonsPrecondition | AlwaysTrue deriving (Show, Eq)
data ComparisonsPrecondition = ComparisonsPrecondition
        { idref :: String
        , test  :: CompareOperator
        , value :: ParameterValue
        } deriving (Show, Eq)
data CompareOperator = LessThan | LessThanEqualTo | EqualTo | GreaterThanEqualTo | GreaterThan deriving (Show, Eq)
type ParameterValue = Either Bool Int

--datatypes used when parsing parameters
data Parameter = Parameter
   { id        :: String
   , emotionId :: Maybe Emotion
   } deriving (Show, Eq)
data Emotion =  Anger | Disgust | Fear | Happiness | Sadness | Surprise deriving (Show, Eq)

--datatypes for statement elements
data StatementElementType = ComputerStatement | PlayerStatement | Conversation
    deriving (Show, Eq)

--datatypes used when parsing effects in the playerstatement
data Effect = Effect
        { idrefEffect :: String
        , changeType  :: ChangeType
        , valueEffect :: ParameterValue
        } deriving (Show, Eq)
data ChangeType = Set | Delta deriving (Show, Eq)


-- code used for testing purposes only
---------------------------------------------------------
--the relative filepath to the test script XML file on my (wouters) computer
testFilepath :: String
testFilepath = "exampleScript.xml"

getTestPreconditions :: IO Precondition
getTestPreconditions = do
        Script scriptElem <- parseScript testFilepath
        pStatement        <- findChild "computerStatement" scriptElem
        return (getPreconditions (Statement pStatement))
