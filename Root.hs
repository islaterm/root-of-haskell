-- "Root of Haskell" (c) by Ignacio Slater M.

-- "Root of Haskell" is licensed under a
-- Creative Commons Attribution 4.0 International License.

-- You should have received a copy of the license along with this
-- work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

-- v1.0.0-b.2
module Root where

type Seed = Double
type Approximations = [Double]

{-|
Creates succesive approximations of r given a seed a0.
-}
apps :: Seed -> Double -> Approximations
apps a0 r = a0 : apps an r where an = 0.5 * (a0 + r / a0)
