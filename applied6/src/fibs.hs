fibs :: (Eq t, Num t, Num a) => t -> a
fibs 0 = 1                       -- two base cases,
fibs 1 = 1                       -- resolved by pattern matching
fibs n = fibs (n-1) + fibs (n-2) -- recursive definition