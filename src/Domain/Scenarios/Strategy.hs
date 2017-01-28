{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Strategy where

import Prelude hiding (fail, sequence)

import Control.Monad hiding (fail, sequence)
import Data.List hiding (inits)
import qualified Data.Map as M

import Control.Monad.Trans.State hiding (state)

import Ideas.Common.Library
import Ideas.Common.Strategy.Combinators hiding (not)

import Domain.Scenarios.ScenarioState
import Domain.Scenarios.Scenario
import Domain.Scenarios.Condition(evaluateCondition)
import Domain.Scenarios.Globals

type StrategyMap = M.Map ID (Strategy ScenarioState)

-- | Make a strategy for each interleave level of a top-level dialogue
makeStrategy :: (Id, Scenario) -> TopDialogue -> Strategy ScenarioState
makeStrategy idscen = sequence . map (makeInterleaveStrategy idscen)

-- | Make a strategy for each dialogue in an interleave level
makeInterleaveStrategy :: (Id, Scenario) -> InterleaveLevel -> Strategy ScenarioState
makeInterleaveStrategy idscen = interleave . map (makeDialogueStrategy idscen)

-- | Make a strategy for a dialogue
makeDialogueStrategy :: (Id, Scenario) -> Dialogue -> Strategy ScenarioState
makeDialogueStrategy idscen dia = optioned (atomiced diaStrategy)
  where
    optioned = if diaOptional dia then option else id
    atomiced = if diaAtomic   dia then atomic else id
    diaStrategy = evalState (makeAlternativesStrategy dia idscen (diaStartIDs dia)) M.empty

-- | Recursively make a strategy for a dia of statements by making a strategy for the starting statement,
-- and then sequencing it to the strategy for the next statements.
makeStatementStrategy :: Dialogue -> (Id, Scenario) -> ID -> State StrategyMap (Strategy ScenarioState)
makeStatementStrategy dia idscen statementID = do
    strategyMap <- get
    -- If the strategy for the statement has already been computed, use that one otherwise compute it.
    case M.lookup statementID strategyMap of
        Just statementStrategy -> return statementStrategy
        Nothing                -> do
            statementStrategy <- if statEnd statement
                then return (rule !~> fail)
                else case statNextStatIDs statement of
                    []      -> return (toStrategy rule)
                    nextIDs -> do
                        -- Make a strategy of alternative strategies for the strategies following from the rule
                        nextStrategy <- makeAlternativesStrategy dia idscen nextIDs

                        -- Sequence the rule to the strategy following from the rule
                        return (sequenceRule statement dia rule nextStrategy)

            modify (M.insert statementID statementStrategy)
            return statementStrategy
          where
            -- Find the given statement in the dia with the statementID
            statementErrorMsg = "Could not find statement: " ++ statementID ++ " in dia: " ++ diaID dia
            maybeStatement = find (\stat -> statID stat == statementID) (diaStatements dia)
            statement = errorOnFail statementErrorMsg maybeStatement

            -- Make the rule for the current statement
            rule = makeGuardedRule idscen statement

-- | Make strategies for all the next statements
-- and combine them with the choice operator
makeAlternativesStrategy :: Dialogue -> (Id, Scenario) -> [ID] -> State StrategyMap (Strategy ScenarioState)
makeAlternativesStrategy dia idscen [singleStatID] =
    makeStatementStrategy dia idscen singleStatID
makeAlternativesStrategy dia idscen statIDs =
    choice <$> mapM (makeStatementStrategy dia idscen) statIDs

-- | Make a rule using all the specific properties for a scenario
makeGuardedRule :: (Id, Scenario) -> Statement -> Rule ScenarioState
makeGuardedRule (scenID, scen) statement = makeRule
    -- the identifier for the rule
    ("scenarios" # scenID # getId statement)
    -- the conditional effect of this rule on the state
    (\state -> do
        -- get the the relevant part of the state
        let (ScenarioState parameterState _) = state
        -- check if precondition is fulfilled
        guard (maybe True (evaluateCondition typeMap parameterState) (statPrecondition statement))
        -- apply the effects of the statement to the state
        Just (applyEffects typeMap state paramEffects info)
    )
  where
    info           = statInfo statement
    paramEffects   = statParamEffects statement
    typeMap        = snd (definitionsParameters (scenarioDefinitions scen))

-- | Sequence a rule to the strategy representing the next statements.
-- Use the atomic prefix combinator '!~>' if the entire dialogue is not atomic,
-- but the statement itself is, so it can not be interleaved.
-- Apply the inits operator if the dialogue can succeed here, so the strategy does not have to be finished.
sequenceRule :: Statement -> Dialogue -> Rule ScenarioState -> Strategy ScenarioState -> Strategy ScenarioState
sequenceRule statement dia rule nextStrategy =
    if statJumpPoint statement || diaAtomic dia
        then rule .*. processedNextStrategy
        else rule !~> processedNextStrategy
    where processedNextStrategy =
            if statInits statement
                then inits nextStrategy
                else nextStrategy
