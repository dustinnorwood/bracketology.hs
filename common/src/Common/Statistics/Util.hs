module Common.Statistics.Util where

class ToDouble a where
  toDouble :: a -> Double

instance ToDouble Integer where
  toDouble = fromInteger

instance ToDouble Double where
  toDouble = id

instance ToDouble Int where
  toDouble = fromInteger . toInteger

class Monad m => HasRandom m where
  randomDouble :: m Double
  randomBool   :: m Bool