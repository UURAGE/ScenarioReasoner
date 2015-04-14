{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-} 
module Domain.Scenarios.Types where

import Ideas.Service.Types

import Domain.Scenarios.Services.StatementsInfo(StatementInfo, StatementText, MediaInfo)
import Domain.Scenarios.Services.ScenarioInfo(ScenarioInfo, ParameterInfo)
import Domain.Scenarios.Services.Score(ScoreResult)

-- | Ideas Framework type definitions for sending an object through a service

-- ScenarioInfo types -----------------------------------------------------------------------------------------------------------
tScenarioInfo :: Type a ScenarioInfo
tScenarioInfo = 
    Iso ((<-!) pairify) (Pair (Tag "id"              tString)
                        (Pair (Tag "name"            tString)
                        (Pair (Tag "description"     tString)
                        (Pair                        tDifficulty    
                        (Pair (Tag "bannerImage"    (tMaybe tString))
                        (Pair (Tag "characterImage" (tMaybe tString))
                        (Pair (Tag "model"          (tMaybe tString))
                        (Pair (Tag "parameters"     (tList tParameterInfo))
                        (Pair (Tag "location"        tString)
                              (Tag "toggles"         tToggles))))))))))                              
        where pairify (ScenarioInfo a b c d e f g h i j k l) = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, (k, l)))))))))))
                       
tParameterInfo :: Type a ParameterInfo
tParameterInfo = Iso ((<-!) pairify) (Pair (Tag "id"       tString)
                                     (Pair (Tag "name"     tString)
                                           (Tag "emotion" (tMaybe tString))))                                           
        where pairify (ParameterInfo id name emotion) = (id, (name, emotion))
        
tToggles :: Type a [Toggle]
tToggles = Iso ((<-!) pairify) (pairToggleTags toggleTags)
  where 
    toggleTags = map (\name -> Tag name tBool) toggleList
    pairToggleTags (tt1:tt2:[]) = Pair tt1 tt2
    pairToggleTags (tt1:tts)    = Pair tt1 (pairToggleTags tts)
    pairify (Toggle name bool) = (name, bool)
    
-- ScoreResult type -------------------------------------------------------------------------------------------------------------
tScoreResult :: Type a ScoreResult
tScoreResult =
    Iso ((<-!) pairify) (Pair (Tag "mainscore"          tInt)
                        (Pair (Tag "subscores"         (tList (tTuple3 tString tString tInt)))
                              (Tag "extremes"          (tMaybe (tList tInt)))))
        where pairify (ScoreResult score subscores extremes) = (score, (subscores, extremes))        

-- StatementInfo types ----------------------------------------------------------------------------------------------------------
tStatementInfo :: Type a StatementInfo
tStatementInfo = 
    Iso ((<-!) pairify) (Pair (Tag "id"        tString)
                        (Pair (Tag "type"      tString)
                        (Pair (Tag "text"      tStatementText)
                        (Pair (Tag "intents"  (tList tString))
                        (Pair (Tag "feedback" (tMaybe tString))
                              (Tag "media"     tMediaInfo))))))
        where pairify (StatementInfo id tp text ints fb media) = (id, (tp, (text, (ints, (fb, media)))))

tStatementText :: Type a StatementText
tStatementText = tString :|: (tList (tPair tString tString))

tMediaInfo :: Type a MediaInfo
tMediaInfo = 
    Iso ((<-!) pairify) (Pair (Tag "visuals" (tList (tPair tString tString)))
                              (Tag "audios"  (tList tString)))
        where pairify (MediaInfo visuals audios) = (visuals, audios)
