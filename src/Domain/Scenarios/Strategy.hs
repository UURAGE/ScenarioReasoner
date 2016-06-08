{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Strategy where

import Prelude hiding (sequence)

import Control.Monad hiding (sequence)
import Data.List hiding (inits)
import qualified Data.Map as M

import Control.Monad.Trans.State hiding (state)

import Ideas.Common.Library
import Ideas.Common.Strategy.Combinators hiding (not)

import Domain.Scenarios.ScenarioState
import Domain.Scenarios.Scenario
import Domain.Scenarios.Condition(evaluateMaybeCondition)
import Domain.Scenarios.Globals

type StrategyMap = M.Map ID (Strategy ScenarioState)

-- | Make a strategy for each interleave level of a top-level dialogue
makeStrategy :: ID -> TopDialogue -> Strategy ScenarioState
makeStrategy scenID = sequence . map (makeInterleaveStrategy scenID)

-- | Make a strategy for each dialogue in an interleave level
makeInterleaveStrategy :: ID -> InterleaveLevel -> Strategy ScenarioState
makeInterleaveStrategy scenID = interleave . map (makeDialogueStrategy scenID)

-- | Make a strategy for a dialogue
makeDialogueStrategy :: ID -> Dialogue -> Strategy ScenarioState
makeDialogueStrategy scenID dia = optioned (atomiced diaStrategy)
  where
    optioned = if diaOptional dia then option else id
    atomiced = if diaAtomic   dia then atomic else id
    diaStrategy = evalState (makeAlternativesStrategy dia scenID (diaStartIDs dia)) M.empty

-- | Recursively make a strategy for a dia of statements by making a strategy for the starting statement,
-- and then sequencing it to the strategy for the next statements.
makeStatementStrategy :: Dialogue -> ID -> ID -> State StrategyMap (Strategy ScenarioState)
makeStatementStrategy dia scenID statementID = do
    strategyMap <- get
    -- If the strategy for the statement has already been computed, use that one otherwise compute it.
    case M.lookup statementID strategyMap of
        Just mStrategy -> return mStrategy
        Nothing        ->
            case statNextStatIDs statement of
                []       -> modify (M.insert statementID (toStrategy rule)) >> return (toStrategy rule)
                nextIDs  -> do
                    -- Make a strategy of alternative strategies for the strategies following from the rule
                    nextStrategy <- makeAlternativesStrategy dia scenID nextIDs

                    -- Sequence the rule to the strategy following from the rule
                    let statementStrategy = sequenceRule statement dia rule nextStrategy

                    modify (M.insert statementID statementStrategy)
                    return statementStrategy
          where
            -- Find the given statement in the dia with the statementID
            statementErrorMsg = "Could not find statement: " ++ statementID ++ " in dia: " ++ diaID dia
            maybeStatement = find (\stat -> statID stat == statementID) (diaStatements dia)
            statement = errorOnFail statementErrorMsg maybeStatement

            -- Make the rule for the current statement
            rule = makeGuardedRule scenID statement

-- | Make strategies for all the next statements
-- and combine them with the choice operator
makeAlternativesStrategy :: Dialogue -> ID -> [ID] -> State StrategyMap (Strategy ScenarioState)
makeAlternativesStrategy dia scenID [singleStatID] =
    makeStatementStrategy dia scenID singleStatID
makeAlternativesStrategy dia scenID statIDs =
    choice <$> mapM (makeStatementStrategy dia scenID) statIDs

-- | Make a rule using all the specific properties for a scenario
makeGuardedRule :: ID -> Statement -> Rule ScenarioState
makeGuardedRule scenID statement = guardedRule
    ("scenarios" # scenID # getId statement)              -- create an identifier for the rule
    (evaluateMaybeCondition (statPrecondition statement)) -- check if precondition is fulfilled
    (\state -> applyEffects state paramEffects info end)  -- apply the effects of a statement to the state
  where
    -- Make a rule with an identifier and a description,
    -- if the precondition is fulfilled given the state and apply the effects of the rule onto the state.
    guardedRule :: IsId a => a -> (ScenarioState -> Bool) -> (ScenarioState -> ScenarioState) -> Rule ScenarioState
    guardedRule ident precond applyEffs = makeRule ident (\state -> do guard $ precond state; Just $ applyEffs state)

    info           = statInfo statement
    end            = statEnd statement
    paramEffects   = statParamEffects statement

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
