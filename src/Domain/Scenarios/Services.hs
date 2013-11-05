module Domain.Scenarios.Services where

import Ideas.Common.Library
import Ideas.Service.Types

customServices :: [Service]
customServices = [alldescriptionsS]

alldescriptionsS :: Service
alldescriptionsS = makeService "pharmacy.alldescriptions"
    "Returns the descriptions of all rules in the exercise." $
    alldescriptions ::: typed

alldescriptions :: Exercise a -> [(String, String)]
alldescriptions = map idAndDescription . ruleset
    where idAndDescription rule = (showId rule, description rule)