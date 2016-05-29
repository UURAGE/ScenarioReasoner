{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Scenario where

import Ideas.Common.Library

import Domain.Scenarios.Globals
import Domain.Scenarios.ScoringFunction
import Domain.Scenarios.Condition
import Domain.Scenarios.ScenarioState(Effect)
import GHC.Generics
import Data.Binary

import System.IO.Unsafe

readBinaryScenario :: FilePath -> Scenario
readBinaryScenario path = unsafePerformIO $ decodeFile path

data Scenario = Scenario
        { scenarioMetaData     :: MetaData
        , scenarioDialogue     :: Dialogue
        }
    deriving (Show, Read, Generic)

instance Binary Scenario

data MetaData = MetaData
        { scenarioName            :: Name
        , scenarioDescription     :: String
        , scenarioDifficulty      :: Maybe Difficulty
        , scenarioParameters      :: [Parameter]
        , scenarioPropertyValues  :: PropertyValues
        , scenarioScoringFunction :: ScoringFunction
        }
 deriving (Show, Read, Generic)

instance Binary MetaData

deriving instance Generic Difficulty
instance Binary Difficulty

type Dialogue = [InterleaveLevel]

type InterleaveLevel = [Tree]

data Tree = Tree
        { treeID          :: ID
        , treeStartIDs    :: [ID]
        , treeAtomic      :: Bool
        , treeOptional    :: Bool
        , treeStatements  :: [Statement]
        }
 deriving (Show, Read, Generic)

instance Binary Tree

data Statement = Statement
        { statID            :: ID
        , statInfo          :: StatementInfo
        , statPrecondition  :: Maybe Condition
        , statParamEffects  :: [Effect]
        , statJumpPoint     :: Bool
        , statInits         :: Bool
        , statEnd           :: Bool
        , statNextStatIDs   :: [ID]
        }
 deriving (Show, Read, Generic)

instance Binary Statement

instance HasId Statement where
    getId statement = describe descr statId
      where
        statId = newId [statType (statInfo statement), statID statement]
        descr  = statText (statInfo statement)
    changeId _ _ = error "The ID of a Statement is determined externally."
