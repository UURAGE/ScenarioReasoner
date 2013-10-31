module CommunicateParser.Parser where

import Ideas.Text.XML.Interface


{-
The purpose of this source-code document is to supply basic functions to querry an xml-script as specified
by the communicate-team. Some things to note are:

- this code doesn't do any error checking on the script. IO-exceptions and scripts deviating from the specifications
will lead to unspecified behaviour (most likely a haskell-exception).
- Currently only the 6 basic emotions as specified by Paul Ekman are supported as emotionid in the parameters,
changing the availible emotions requires changing the Emotion data-type and the parseEmotion function. However
unrecognized emotionids will be ignored, and will not lead to exceptions.

to-do list:
-add a "getScriptModel" function. Need more information about possible models for that.

-}


--functions to be exposed as an interface
-----------------------------------------------------

--queries the script at the filepath for its name.
getScriptName :: String -> IO String
getScriptName = getMetaDataString "name"

--queries the script at the filepath for its date.
getScriptDate :: String -> IO String
getScriptDate = getMetaDataString "date"

--queries the script at the filepath for its description.
getScriptDescription :: String -> IO String
getScriptDescription = getMetaDataString "description"

--queries the script at the filepath for its difficulty.
getScriptDifficulty :: String -> IO String
getScriptDifficulty = getMetaDataString "difficulty"

--queries the script at the filepath for its startId
getScriptStartId :: String -> IO String
getScriptStartId filepath = do
        scriptElem <- script filepath
        metadata   <- findChild "metadata" scriptElem
        dataElem   <- findChild "start" metadata
        return (head (findAttribute "idref" dataElem))

--queries the script at the filepath for its parameters
getScriptParameters :: String -> IO [Parameter]
getScriptParameters filepath = do
        scriptElem    <- script filepath
        metadata      <- findChild "metadata" scriptElem
        parameterData <- findChild "parameters" metadata
        return (map parseParameterAttributes (children parameterData))

--Takes a playerstatement or an computerstatement element and returns the preconditions.
--The xml-parser forces the extra Monad in the result. This is unfortunate, but inevitable.
getPreconditions :: Element -> IO Precondition
getPreconditions elemVar = do
        return (parsePreconditions (findAllChildren "preconditions" elemVar))

--Returns the id of the video tag of the computer- or playerstatement if there is one. Else it returns "Nothing".
getMaybeVideoId :: Element -> IO (Maybe String)
getMaybeVideoId elemVar = case findAllChildren "video" elemVar of
        []   -> return Nothing
        b:_ -> do
           videoId <- findAttribute "extid" b
           return (Just videoId)

--Returns the effects of a playerstatement
getEffects :: Element -> IO [Effect]
getEffects elemVar = do
        effects <- findChild "effects" elemVar
        return (map parseEffect (children effects))

--Returns the text of a playerstatement or a computerstatement
getText :: Element -> IO String
getText elemVar = do
        textElem <- findChild "text" elemVar
        return (getData textElem)

--Takes a script and a playerstatement id, a compterstatement id or a conversation id and then 
--returns the corresponding element.
getStatement :: Element -> String -> Element
getStatement scriptVar idVar = head (filter (idAttributeIs idVar) (findAllChildren (getType idVar) scriptVar))
        where getType idVar2 = case idVar2 of
                'p':('a':_) -> "computerStatement"
                'p':('h':_) -> "playerStatement"
                'c':_       -> "conversation"
              idAttributeIs testId elemVar = (head (findAttribute "id" elemVar)) == testId


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
        

--parses a precondition. Recusivly parses Ands and Ors. Empty Ands and Ors gives AlwaysTrue.
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

--queries the xml-file at the filepath for basic information. Which information being queried is specified
--in the "metaDataName". This could be the name of the script, the difficulty, date, etc.
getMetaDataString :: String -> String -> IO String
getMetaDataString metaDataName filepath = do
        scriptElem  <- script filepath
        metadata    <- findChild "metadata" scriptElem
        dataElem    <- findChild metaDataName metadata
        return (getData dataElem)

--parses the xml script at "filepath" to a xml-object of type "Element".
--warning: crashes on invalid file or invalid xml.
script :: String -> IO Element
script filepath = do
    text <- readFile filepath
    return (forceRight (parseXML text))
        
-- Takes an either object. In the case of "right" data, returns the data. In the case of "left" data, crashes.
forceRight :: Either a b -> b
forceRight (Right elem) = elem


--functions added to the xml-parser
---------------------------------------------------------

findAllChildren :: String -> Element -> [Element]
findAllChildren s e = filter ((==s) . name) (children e)


--data structures definitions
---------------------------------------------------------

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

--datatypes used when parsing effects in the playerstatement
data Effect = Effect
        { idrefEffect :: String
        , changeType  :: ChangeType
        , valueEffect :: ParameterValue
        } deriving (Show, Eq)
data ChangeType = Set | Delta deriving (Show, Eq)


-- code used for testing purposes only
---------------------------------------------------------
--the relative filepath to the test script xml file on my (wouters) computer
testFilepath :: String
testFilepath = "exampleScript.xml"

getTestPreconditions :: IO Precondition
getTestPreconditions = do
        scriptElem       <- script testFilepath
        pStatement       <- findChild "computerStatement" scriptElem
        getPreconditions pStatement