module Data.Date.ValueRange where

import Data.Maybe
import Prelude

data ValueRange a = VR
  { lowerMin :: a
  , lowerMax :: a
  , upperMin :: a
  , upperMax :: a
  }

valueRange :: forall a. (Ord a) => a -> a -> a -> a -> Maybe (ValueRange a)
valueRange lowerMin lowerMax upperMin upperMax = 
  if lowerMin > lowerMax || upperMin > upperMax || lowerMax > upperMax
     then Nothing
     else Just $ VR { lowerMin: lowerMin, lowerMax: lowerMax, upperMin: upperMin, upperMax: upperMax }

fixedValueRange :: forall a. (Ord a) => a -> a -> Maybe (ValueRange a)
fixedValueRange min max = 
  if min > max 
     then Nothing 
     else Just $ VR { lowerMin: min, lowerMax: min, upperMin: max, upperMax: max }

minFixedValueRange :: forall a. (Ord a) => a -> a -> a -> Maybe (ValueRange a)
minFixedValueRange min upperMin upperMax =
  if min > upperMin || min > upperMax
     then Nothing
     else Just $ VR { lowerMin: min, lowerMax: min, upperMin: upperMin, upperMax: upperMax }

isFixed :: forall a. (Eq a) => ValueRange a -> Boolean
isFixed (VR r) =
  r.lowerMin == r.lowerMax && r.upperMin == r.upperMax

isValid :: forall a. (Ord a) => ValueRange a -> a -> Boolean
isValid (VR r) n = n >= r.lowerMin && n <= r.upperMax

instance vrEq :: (Ord a) => Eq (ValueRange a) where
  eq (VR r) (VR r') = 
    r.lowerMin == r'.lowerMin
    && r.lowerMax == r'.lowerMax
    && r.upperMin == r'.upperMin
    && r.upperMax == r'.upperMax

instance vrShow :: (Show a, Eq a) => Show (ValueRange a) where
  show (VR r) = 
    show r.lowerMin
    <> (if r.lowerMin == r.lowerMax then "" else "/" <> show r.lowerMax)
    <> "-" <> show r.upperMin
    <> (if r.upperMin == r.upperMax then "" else "/" <> show r.upperMax)

