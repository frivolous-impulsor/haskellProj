--Question 3
--a
t :: Int -> Int
t n
    | n == 0 = 0
    | n > 0 = n + t (n-1)
    | otherwise = error "No Negative Int Can Be Calculated For Number Of Balls!"

sq :: Int -> Int
sq n
    | n > 0 = n * n
    | otherwise = error "Only Postive Int Can Be Calculated For Sq"

--b
s :: Int -> Int
s n
    | n > 0 = t n + t (n-1)
    | otherwise = error "Only Positive Int Can Be Calculated For S"


--s(10) evaluates to 100
--sq(10) evaluates to 100 too
--Thus s(10) == sq(10) evaluates to True