import Test.QuickCheck

import Test.QuickCheck.All
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Random
import Test.QuickCheck.Property
import Dolgii.ArbitraryPrecision

gen = resize 1000 (arbitrary :: Gen (Int, Int))

propAdd = forAll gen (\(a, b) -> add (show a) (show b) == show (a + b))

propSubtract = forAll gen (\(a, b) -> Dolgii.ArbitraryPrecision.subtract (show a) (show b) == show (a - b))

propMultiply = forAll gen (\(a, b) -> Dolgii.ArbitraryPrecision.multiply (show a) (show b) == show (a * b))

main :: IO ()
main = do
  quickCheck propAdd
  quickCheck propSubtract
  quickCheck propMultiply