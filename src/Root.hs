-- "Root of Haskell" (c) by Ignacio Slater M.

-- "Root of Haskell" is licensed under a
-- Creative Commons Attribution 4.0 International License.

-- You should have received a copy of the license along with this
-- work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

-- v1.0

module Root where

import           Test.QuickCheck
import           Test.Hspec

type Seed = Double
type Precision = Double
type Approximations = [Double]

-- region : apps
{-|
Creates succesive approximations of r given a seed a0.
-}
apps :: Seed -> Double -> Approximations
apps a0 r = a0 : apps an r where an = 0.5 * (a0 + r / a0)

{-| Checks that apps works properly.  -}
testApps :: Spec
testApps =
  describe "Generated approximations:"
    $ it "Generated approximations follow the expected sequence"
    $ property
    $ \n a0 r ->
        1
          <=  n
          &&  0
          <=  a0
          &&  0
          <=  r
          ==> (  (apps a0 r !! n)
              == 0.5
              *  ((apps a0 r !! (n - 1)) + r / (apps a0 r !! (n - 1)))
              )
-- endregion
-- region : approxLimit
{-|
Returns the first element of an infinite list that is at most at a distance e from the previous 
element
-}
approxLimit :: Double -> [Double] -> Double
approxLimit _ []  = -1  -- For test purposes
approxLimit _ [_] = -1  -- For test purposes  
approxLimit e xs  = if abs (head xs - head (tail xs)) <= e
  then head (tail xs)
  else approxLimit e (tail xs)

{-| Returns de index of an element on a list. -}
indexOf :: Eq a => [a] -> a -> Int -> Int
indexOf []       _ _   = error "Element wasn't found"
indexOf (x : xs) v idx = if x == v then idx else indexOf xs v (idx + 1)


{-| Auxiliary function to check if approxLimit terminates with a correct result.

    This function is only present for testing purposes. -}
validResult :: Double -> Bool
validResult v = v > 0

{-| Checks that appsLimit works properly. -}
testAppsLimit :: Spec
testAppsLimit =
  describe "Epsilon distance:"
    $ it "approxLimit's result is correct"
    $ property
    $ \e xs ->
        (e :: Double)
          >   0
          &&  length xs
          >   2
          &&  validResult (approxLimit e (xs :: [Double]))
          ==> abs
                ( (xs !! indexOf xs (approxLimit e (xs :: [Double])) 0)
                - (xs !! (indexOf xs (approxLimit e (xs :: [Double])) 0 - 1))
                )
          <=  e
-- endregion
-- region : approxSqrt
approxSqrt :: Double -> Seed -> Precision -> Double
approxSqrt v s p = approxLimit p xs where xs = apps s v

testApproxSqrt :: Spec
testApproxSqrt =
  describe "Square root approximation:" $ it "" $ property $ \v s p ->
    (v :: Double)
      >   0
      &&  (s :: Seed)
      >   0
      &&  (p :: Precision)
      >   0
      ==> abs (approxSqrt v s (p / 1000000) - sqrt v)
      <=  p / 1000000
-- endregion

main :: IO ()
main = hspec $ do
  testApps
  testAppsLimit
  testApproxSqrt
