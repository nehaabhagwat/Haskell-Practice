-- Do the game fizzbuzz (http://en.wikipedia.org/wiki/Fizz_buzz).
-- Return a string counting from 1 to the specified number.
-- Replace numbers dvisible by 3 with "fizz" and numbers divisible
-- by 5 with "buzz".  If a number is divisible by both 3 and 5,
-- replace it with "fizzbuzz".

fizzbuzz :: Int -> String
fizzbuzz n|n<=0 = "Cannot play game for number less than 1"
fizzbuzz 1 = show 1
fizzbuzz n = fizzbuzz(n-1) ++ " " ++ if n `mod` 3 == 0 && n `mod` 5 == 0 then "FIZZBUZZ"
else if n `mod` 3 == 0 then "FIZZ"
else if n `mod` 5 == 0 then "BUZZ"
else show n


main :: IO ()
main = do
  print (fizzbuzz 1)
  print (fizzbuzz 7)
  print $ fizzbuzz 99
  print $ fizzbuzz 0
  print $ fizzbuzz (-2)