-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
--  $Id: Rule.hs 8740 2015-10-14 19:26:22Z bastiaan $

module Ideas.Common.Rule (module Export) where

import Ideas.Common.Rule.Abstract as Export
import Ideas.Common.Rule.EnvironmentMonad as Export
import Ideas.Common.Rule.Parameter as Export
import Ideas.Common.Rule.Recognizer as Export
import Ideas.Common.Rule.Transformation as Export