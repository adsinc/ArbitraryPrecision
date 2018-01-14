module Dolgii.ArbitraryPrecision(
  add,
  multiply,
  subtract
  ) where

import Prelude hiding(subtract)
import Data.Char

add :: String -> String -> String
add = convert add'

subtract :: String -> String -> String
subtract = convert sub

multiply :: String -> String -> String
multiply = convert multiply'

-- знак
data Sign = Pos | Neg deriving (Eq, Show)
-- тип - пара знак + список
type AP = (Sign, [Int])

-- конвертит функцию, которая работает с нашим в представлением 
-- в функцию, которая работает со строками
convert :: (AP -> AP -> AP) -> (String -> String -> String)
convert f x y = fromAP $ f (toAP x) (toAP y)
  where
    fromAP (sgn, xs) =
      let
        num = removeZeros (map intToDigit xs)
        s =
            if sgn == Neg && num /= "0"
              then "-"
              else ""
      in s ++ num
      where
        removeZeros s =
          if all (== '0') s
            then "0"
            else dropWhile (== '0') s
    toAP as@(sgn:xs)
      | isDigit sgn = createAP Pos as
      | sgn == '+' = createAP Pos xs
      | sgn == '-' = createAP Neg xs
      | otherwise = error "Incorrect number"
      where
        createAP s xs = (s, map digitToInt xs)

add' :: AP -> AP -> AP
add' (Pos, a) (Pos, b) = (Pos, addNoSign a b)
add' x@(Pos, _) (Neg, ys) = sub x (Pos, ys)
add' (Neg, xs) y@(Pos, _) = sub y (Pos, xs)
add' (Neg, a) (Neg, b) = (Neg, addNoSign a b)

addNoSign :: [Int] -> [Int] -> [Int]
addNoSign a b =
  let lenDiff = length a - length b
      zeros = replicate lenDiff 0
  in if lenDiff < 0
       then addNoSign b a
       else convert $ addEqLen a (zeros ++ b)
  where
    convert (xs, 0) = xs
    convert (xs, 1) = 1 : xs

addEqLen ::[Int] -> [Int] -> ([Int], Int)
addEqLen a b = foldr fn ([], 0) (zip a b)
  where
    fn (x, y) (result, transfer) =
      let digitSum = x + y + transfer
          newTransfer = digitSum `div` 10
          digit = digitSum `mod` 10
      in (digit : result, newTransfer)

sub :: AP -> AP -> AP
sub (Pos, a) (Neg, b) = (Pos, addNoSign a b)
sub (Neg, a) (Pos, b) = (Neg, addNoSign a b)
sub (Neg, a) (Neg, b) = sub (Pos, b) (Pos, a)
sub (Pos, a) (Pos, b) = subInts a b 

subInts :: [Int] -> [Int] -> AP
subInts xs ys =
  let lenDiff = length xs - length ys
      zeros = replicate lenDiff 0
      ys' = zeros ++ ys
  in if lenDiff > 0 || lenDiff == 0 && checkGtEq xs ys'
    then (Pos, fst $ subEqLen xs ys')
    else (Neg, snd $ subInts ys xs)
  where checkGtEq (a:as) (b:bs)
          | a < b = False
          | a > b = True
          | otherwise = checkGtEq as bs
        checkGtEq [] [] = True

subEqLen :: [Int] -> [Int] -> ([Int], Int)
subEqLen a b = foldr fn ([], 0) (zip a b) 
  where
    fn (x, y) (result, transfer) 
      | x - transfer < y = 
        let digit = x - transfer + 10 - y
            in (digit : result, 1)
      | otherwise = ((x - transfer - y) : result, 0)

multiply' :: AP -> AP -> AP
multiply' (sa, a) (sb, b) =
  let sign = if sa == sb then Pos else Neg
  in (sign, multiplyNoSign a b)

multiplyNoSign :: [Int] -> [Int] -> [Int]
multiplyNoSign as bs = fst $ foldr f ([0], 0) bs
  where
    f :: Int -> ([Int], Int) -> ([Int], Int)
    f b (res, shift) =
      let p = multiplySingle as b
          shifted = p ++ replicate shift 0
      in (addNoSign shifted res, shift + 1)

multiplySingle :: [Int] -> Int -> [Int]
multiplySingle as b = convert $ foldr fn ([], 0) as
  where
    fn a (result, transfer) =
      let digitProd = a * b + transfer
          newTransfer = digitProd `div` 10
          digit = digitProd `mod` 10
      in (digit : result, newTransfer)
    convert (prod, 0) = prod
    convert (prod, transfer) = transfer : prod