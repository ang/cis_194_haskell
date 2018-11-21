{-
  Exercise 1
  We need to first find the digits of a number. Define the functions
  toDigits :: Integer -> [Integer]
  toDigitsRev :: Integer -> [Integer]
  toDigits should convert positive Integers to a list of digits. (For 0 or
  negative inputs, toDigits should return the empty list.) toDigitsRev should do
  the same, but with the digits reversed.
  Example: toDigits 1234 == [1,2,3,4]
  Example: toDigitsRev 1234 == [4,3,2,1]
  Example: toDigits 0 == []
  Example: toDigits (-17) == []
-}

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits(n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
-- Lol how does this work? It's a fold?
toDigitsRev n = (reverse . toDigits) n

{-
  Exercise 2 Once we have the digits in the proper order, we need to double
  every other one. Define a function doubleEveryOther :: [Integer] -> [Integer]
  Remember that doubleEveryOther should double every other number beginning from
  the right, that is, the second-to-last, fourth-to-last, ...numbers are
  doubled.
  Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
  Example: doubleEveryOther [1,2,3] == [1,4,3]
-}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs)
  | length(x:y:zs) `mod` 2 == 0 = [x * 2, y] ++ doubleEveryOther(zs)
  | length(x:y:zs) `mod` 2 /= 0 = [x, y * 2] ++ doubleEveryOther(zs)

{-
  Exercise 3 The output of doubleEveryOther has a mix of one-digit
  and two-digit numbers. Define the function
  sumDigits :: [Integer] -> Integer
  to calculate the sum of all digits.
  Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
-}
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:zs)
  | x >= 10 = sumDigits(toDigits(x)) + sumDigits(zs)
  | otherwise = x + sumDigits(zs)

{-
  Exercise 4 Define the function
  validate :: Integer -> Bool
  that indicates whether an Integer could be a valid credit card number.
  This will use all functions defined in the previous exercises.

  Double the value of every second digit beginning from the right.
  That is, the last digit is unchanged; the second-to-last digit is doubled; the
  third-to-last digit is unchanged; and so on. For example, [1,3,8,6] becomes
  [2,3,16,6].
  * Add the digits of the doubled values and the undoubled digits from the
  original number. For example, [2,3,16,6] becomes 2+3+1+6+6 = 18
  * Calculate the remainder when the sum is divided by 10. For the above
  example, the remainder would be 8. If the result equals 0, then the number is
  valid.

  Example: validate 4012888888881881 = True
  Example: validate 4012888888881882 = False
-}

validate :: Integer -> Bool
validate n
  | sumDigits(doubleEveryOther(toDigits(n))) `mod` 10 == 0 = True
  | otherwise = False

{-
  Exercise 5 The Towers of Hanoi is a classic puzzle with a solution that can be
  described recursively. Disks of different sizes are stacked on three pegs; the
  goal is to get from a starting configuration with all disks stacked on the
  first peg to an ending configuration with all disks stacked on the last peg.

  The only rules are
  * you may only move one disk at a time, and
  * a larger disk may never be stacked on top of a smaller one.
  For example, as the first move all you can do is move the topmost, smallest
  disk onto a different peg, since only one disk may be moved at a time.

  To move n discs (stacked in increasing size) from peg a to peg b using peg c
  as temporary storage,
  1. move n − 1 discs from a to c using b as temporary storage
  2. move the top disc from a to b
  3. move n − 1 discs from c to b using a as temporary storage.

  For this exercise, define a function hanoi with the following type:

  type Peg = String
  type Move = (Peg, Peg)
  hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

  Given the number of discs and names for the three pegs, hanoi should return a
  list of moves to be performed to move the stack of discs from the first peg to
  the second.
  Note that a type declaration, like type Peg = String above, makes a type
  synonym. In this case Peg is declared as a synonym for String, and the two
  names Peg and String can now be used interchangeably.  Giving more descriptive
  names to types in this way can be used to give shorter names to complicated
  types, or (as here) simply to help with documentation.
  Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
-}

type Peg = String
type Move = (Peg, Peg)

{-
  Three steps:
  1. Build the n-1 case, where n-1 will end up in the temp peg
  2. Move the last piece to the dest peg
  3. Move the n-1 case from the temp peg to the dest peg
-}
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numDiscs start dest temp
  | numDiscs == 0 = []
  | otherwise = hanoi (numDiscs - 1) start temp dest ++ [(start, dest)] ++ hanoi (numDiscs - 1) temp dest start

-- Question. The following line didn't work, and I'm not sure why. It complained
-- about the second half. Had to remove the parenthesis in order for it to work.
-- | numDiscs == 2 = [(start, dest)] ++ hanoi(numDiscs-1 start dest temp)

{-
 1. Build half (not counting last piece, round up) on temp1. We can use two temporary pegs.
 2. Build half (not counting last piece, round down) on temp2. We can only use ONE temporary peg.
 3. Move last piece to dest peg
 4. Move temp2 to dest, we can only use one temporary peg.
 5. Move temp to dest, we can use two temporary pegs.
-}
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 numDiscs start dest temp1 temp2
  | numDiscs == 0 = []
  | numDiscs == 1 = [(start, dest)]
  | otherwise = hanoi4 (ceiling(fromIntegral(numDiscs - 1) / 2)) start temp1 temp2 dest ++
                hanoi ((numDiscs - 1) `div` 2) start temp2 dest ++
                [(start, dest)] ++
                hanoi ((numDiscs - 1) `div` 2) temp2 dest start ++
                hanoi4 (ceiling(fromIntegral(numDiscs - 1) / 2)) temp1 dest start temp2

validateHanoi4 :: () -> Bool
validateHanoi4 () = hanoi4 0 "a" "d" "b" "c" == [] &&
                    hanoi4 1 "a" "d" "b" "c" == [("a", "d")] &&
                    hanoi4 2 "a" "d" "b" "c" == [("a","b"),("a","d"),("b","d")] &&
                    hanoi4 3 "a" "d" "b" "c" == [("a","b"),("a","c"),("a","d"),("c","d"),("b","d")] &&
                    hanoi4 4 "a" "d" "b" "c" == [("a","c"),("a","b"),("c","b"),("a","c"),("a","d"),("c","d"),("b","a"),("b","d"),("a","d")] &&
                    hanoi4 5 "a" "d" "b" "c" == [("a","c"),("a","b"),("c","b"),("a","d"),("a","c"),("d","c"),("a","d"),("c","a"),("c","d"),("a","d"),("b","a"),("b","d"),("a","d")] &&
                    hanoi4 6 "a" "d" "b" "c" == [("a","c"),("a","d"),("a","b"),("d","b"),("c","b"),("a","d"),("a","c"),("d","c"),("a","d"),("c","a"),("c","d"),("a","d"),("b","a"),("b","c"),("b","d"),("c","d"),("a","d")] &&
                    hanoi4 7 "a" "d" "b" "c" == [("a","c"),("a","d"),("a","b"),("d","b"),("c","b"),("a","c"),("a","d"),("c","d"),("a","c"),("d","a"),("d","c"),("a","c"),("a","d"),("c","d"),("c","a"),("d","a"),("c","d"),("a","c"),("a","d"),("c","d"),("b","a"),("b","c"),("b","d"),("c","d"),("a","d")]
