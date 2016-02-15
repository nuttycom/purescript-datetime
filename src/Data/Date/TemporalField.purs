module Data.Date.TemporalField where

import Data.Maybe
import Data.Date.ValueRange
import Data.Date.Locale
import Data.Date.TemporalUnit

import Control.Monad.Eff (Eff())

data TemporalBasis 
  = DateBasis
  | TimeBasis
  | OtherBasis

type TemporalField = 
  { diplayName :: forall e. Eff ( locale :: Locale | e) String -- ???
  , baseUnit :: TemporalUnit
  , rangeUnit :: TemporalUnit
  , range :: Maybe (ValueRange Int)
  , basis :: TemporalBasis
  }

