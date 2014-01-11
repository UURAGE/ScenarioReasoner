-----------------------------------------------------------------------------
-- Copyright 2013, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Services using JSON notation
--
-----------------------------------------------------------------------------
module Ideas.Encoding.ModeJSON (processJSON) where

import Data.Char
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Common.Utils (Some(..), timedSeconds)
import Ideas.Encoding.DecoderJSON
import Ideas.Encoding.EncoderJSON
import Ideas.Encoding.Evaluator
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackScript.Syntax (Script)
import Ideas.Service.Request
import Ideas.Text.JSON
import System.Random hiding (getStdGen)

processJSON :: Maybe Int -> Bool -> DomainReasoner -> String -> IO (Request, String, String)
processJSON maxTime cgiMode dr input = do
   json <- either fail return (parseJSON input)
   req  <- jsonRequest json
   resp <- jsonRPC json $ \fun arg -> 
              maybe id timedSeconds maxTime (myHandler dr fun arg)
   let f   = if compactOutputDefault cgiMode req then compactJSON else show
       out = addVersion (version dr) (toJSON resp)
   return (req, f out, "application/json")

-- TODO: Clean-up code
extractExerciseId :: Monad m => JSON -> m Id
extractExerciseId json =
   case json of
      String s -> return (newId s)
      Array [String _, String _, a@(Array _)] -> extractExerciseId a
      Array [String _, String _, _, a@(Array _)] -> extractExerciseId a
      Array (String s:tl) | any p s -> extractExerciseId (Array tl)
      Array (hd:_) -> extractExerciseId hd
      _ -> fail "no code"
 where
   p c = not (isAlphaNum c || isSpace c || c `elem` ".-")

addVersion :: String -> JSON -> JSON
addVersion str json =
   case json of
      Object xs -> Object (xs ++ [info])
      _         -> json
 where
   info = ("version", String str)

jsonRequest :: Monad m => JSON -> m Request
jsonRequest json = do
   srv  <- case lookupM "method" json of
              Just (String s) -> return s
              _               -> fail "Invalid method"
   let a = lookupM "params" json >>= extractExerciseId
   enc  <- case lookupM "encoding" json of
              Nothing         -> return []
              Just (String s) -> readEncoding s
              _               -> fail "Invalid encoding"
   src  <- case lookupM "source" json of
              Nothing         -> return Nothing
              Just (String s) -> return (Just s)
              _               -> fail "Invalid source"
   return Request
      { service    = srv
      , exerciseId = a
      , source     = src
      , dataformat = JSON
      , encoding   = enc
      }

myHandler :: DomainReasoner -> RPCHandler IO
myHandler dr fun json = do
   srv <- findService dr (newId fun)
   Some ex <-
      if fun == "exerciselist"
      then return (Some emptyExercise)
      else extractExerciseId json >>= findExercise dr
   script <- defaultScript dr (getId ex)
   stdgen <- newStdGen
   evalService (jsonConverter script ex stdgen json) srv

jsonConverter :: Script -> Exercise a -> StdGen -> JSON -> Evaluator a JSON
jsonConverter script ex stdgen json = Evaluator 
   (runEncoderStateM jsonEncoder (String . prettyPrinter ex))  
   (\tp -> runEncoderStateM (jsonDecoder tp) jds json)
 where
   jds = JSONDecoderState ex script stdgen