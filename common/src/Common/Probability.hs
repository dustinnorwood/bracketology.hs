module Common.Probability where

import           Control.Arrow      ((&&&), (***))
import           Data.Bool          (bool)
import qualified Data.List.NonEmpty as NE
import           System.Random

getCdfBinValue :: [Double] -> Double -> (Int, Double)
getCdfBinValue [] _ = (0, 0.0)
getCdfBinValue xs observation =
  let ys = toCdf xs
   in getIndex . NE.fromList $ zip [0..] ys
  where
    getIndex (iy NE.:| []) = iy
    getIndex ((i, y) NE.:| (z:zs)) =
      if observation < y  
        then (i, y)
        else getIndex (z NE.:| zs)

getCdfBin :: [Double] -> Double -> Int
getCdfBin xs = fst . getCdfBinValue xs

getCdfValue :: [Double] -> Double -> Double
getCdfValue xs = snd . getCdfBinValue xs

flatten :: (Double -> Bool) -> [Double] -> [Double]
flatten f = map (bool 0.0 1.0 . f)

toPmf :: [Double] -> [Double]
toPmf xs =
  let s = sum xs
   in if s == 0.0
        then xs
        else map (/s) xs

toCdf :: [Double] -> [Double]
toCdf [] = []
toCdf xs =
  let (y:ys) = toPmf xs
   in f y ys
  where f _ [] = []
        f z [y] = [y + z]
        f z (y:ys) = let yz = y + z in yz : f yz ys

givenCondition :: [Double] -> [Double] -> (Double -> Bool) -> ([Double], [Double])
givenCondition xs condition f = unzip . map (\(x,c) -> bool (0.0, 0.0) (x, c) $ f c) $ zip xs condition

given :: [Double] -> [Double] -> (Double -> Bool) -> [Double]
given xs condition = fst . givenCondition xs condition

givenPmfCondition :: [Double] -> [Double] -> (Double -> Bool) -> ([Double], [Double])
givenPmfCondition xs condition f =
  let ~(ys, yConds) = givenCondition xs condition f
   in (toPmf ys, yConds)

givenPmf :: [Double] -> [Double] -> (Double -> Bool) -> [Double]
givenPmf xs condition = fst . givenPmfCondition xs condition

autocorrelation :: [Double] -> Double
autocorrelation x = correlation x x

correlation :: [Double] -> [Double] -> Double
correlation x y = (covariance x y) / (standardDeviation x * standardDeviation y)

covariance :: [Double] -> [Double] -> Double
covariance x y = e $ (x `subtractDouble` e x) `multiplyDoubles` (y `subtractDouble` e y)

e :: [Double] -> Double
e = average

averageNonZero :: [Double] -> Double
averageNonZero [] = 0.0
averageNonZero xs' = go 0.0 0.0 xs'
  where go s c [] = if c > 0 then s / c else 0.0
        go s c (x:xs) =
          if x /= 0.0
            then go (s + x) (c + 1) xs
            else go s c xs

addDoubles :: [Double] -> [Double] -> [Double]
addDoubles = zipWith (+)

subtractDoubles :: [Double] -> [Double] -> [Double]
subtractDoubles = zipWith (-)

multiplyDoubles :: [Double] -> [Double] -> [Double]
multiplyDoubles = zipWith (*)

divideDoubles :: [Double] -> [Double] -> [Double]
divideDoubles = zipWith (\n d -> if d /= 0.0 then n/d else 0.0)

addDouble :: [Double] -> Double -> [Double]
addDouble xs y = map (+y) xs

subtractDouble :: [Double] -> Double -> [Double]
subtractDouble xs y = map (flip (-) y) xs

multiplyDouble :: [Double] -> Double -> [Double]
multiplyDouble xs y = map (*y) xs

divideDouble :: [Double] -> Double -> [Double]
divideDouble xs 0.0 = map (const 0.0) xs
divideDouble xs y = map (/y) xs

average :: [Double] -> Double
average [] = 0.0
average xs = sum xs / fromInteger (fromIntegral $ length xs)

variance :: [Double] -> Double
variance xs =
  let ave = average xs
      ~(num, den) = foldr (\x -> (+((x-ave)*(x-ave))) *** (+1)) (0.0, 0) xs
   in if den > 0
        then num / fromInteger den
        else 0.0

standardDeviation :: [Double] -> Double
standardDeviation = sqrt . variance

muSigma :: [Double] -> (Double, Double)
muSigma = average &&& standardDeviation


-- Normal distribution approximation
-- ---------------------------------
-- | Box-Muller method for generating two normally distributed
-- independent random values from two uniformly distributed
-- independent random values.
boxMuller :: Floating a => a -> a -> (a,a)
boxMuller u1 u2 = (r * cos t, r * sin t) where r = sqrt (-2 * log u1)
                                               t = 2 * pi * u2

-- | Convert a list of uniformly distributed random values into a
-- list of normally distributed random values. The Box-Muller
-- algorithms converts values two at a time, so if the input list
-- has an uneven number of element the last one will be discarded.
boxMullers :: Floating a => [a] -> [a]
boxMullers (u1:u2:us) = n1:n2:boxMullers us where (n1,n2) = boxMuller u1 u2
boxMullers _          = []


-- API
-- ===
-- | Takes a random number generator g, and returns a random value
-- normally distributed with mean 0 and standard deviation 1,
-- together with a new generator. This function is analogous to
-- 'Random.random'.
normal :: (RandomGen g, Random a, Floating a) => g -> (a,g)
normal g0 = (fst $ boxMuller u1 u2, g2)
  -- While The Haskell 98 report says "For fractional types, the
  -- range is normally the semi-closed interval [0,1)" we will
  -- specify the range explicitly just to be sure.
  where
     (u1,g1) = randomR (0,1) g0
     (u2,g2) = randomR (0,1) g1

-- | Plural variant of 'normal', producing an infinite list of
-- random values instead of returning a new generator. This function
-- is analogous to 'Random.randoms'.
normals :: (RandomGen g, Random a, Floating a) => g -> [a]
normals = boxMullers . randoms

-- | Creates a infinite list of normally distributed random values
-- from the provided random generator seed. (In the implementation
-- the seed is fed to 'Random.mkStdGen' to produce the random
-- number generator.)
mkNormals :: (Random a, Floating a) => Int -> [a]
mkNormals = normals . mkStdGen


-- | A variant of 'normal' that uses the global random number
-- generator. This function is analogous to 'Random.randomIO'.
normalIO :: (Random a, Floating a) => IO a
normalIO = do u1 <- randomRIO (0,1)
              u2 <- randomRIO (0,1)
              return $ fst $ boxMuller u1 u2

-- | Creates a infinite list of normally distributed random values
-- using the global random number generator. (In the implementation
-- 'Random.newStdGen' is used.)
normalsIO :: (Random a, Floating a) => IO [a]
normalsIO = fmap normals newStdGen


-- With mean and standard deviation
-- --------------------------------
-- | Analogous to 'normal' but uses the supplied (mean, standard
-- deviation).
normal' :: (RandomGen g, Random a, Floating a) => (a,a) -> g -> (a,g)
normal' (mean, sigma) g = (x * sigma + mean, g') where (x, g') = normal g

-- | Analogous to 'normals' but uses the supplied (mean, standard
-- deviation).
normals' :: (RandomGen g, Random a, Floating a) => (a,a) -> g -> [a]
normals' (mean, sigma) g = map (\x -> x * sigma + mean) (normals g)

-- | Analogous to 'mkNormals' but uses the supplied (mean, standard
-- deviation).
mkNormals' :: (Random a, Floating a) => (a,a) -> Int -> [a]
mkNormals' ms = normals' ms . mkStdGen


-- | Analogous to 'normalIO' but uses the supplied (mean, standard
-- deviation).
normalIO' ::(Random a, Floating a) => (a,a) -> IO a
normalIO' (mean,sigma) = fmap (\x -> x * sigma + mean) normalIO

-- | Analogous to 'normalsIO' but uses the supplied (mean, standard
-- deviation).
normalsIO' :: (Random a, Floating a) => (a,a) -> IO [a]
normalsIO' ms = fmap (normals' ms) newStdGen