-- 34
square x = x * x
xpisq x = 3.14 * square x
-- 35
pisq x = pi * square x
-- 38
-- 1. yes
-- 2. no
-- 3. yes
perimeter x y = x * 2 + y * 2
f x = x / (2 + 9)
-- 45
area x = 3.14 * (x * x) -- missing space after decimal, didn't even notice this in the text...
double x = x * 2
x = 7
y = 10 -- remove leading space
f2 = x+y
-- 59
-- 1. 5
-- 2. 25
-- 3. 30
-- 4. 6
where1 = x*3+y
    where x = 3 
          y = 1000

where2 = x*5
    where x = 10*5+y
          y = 10

where3 = z / x + y
    where x = 7
          y = -x
          z = y*10

-- chapter 2 exercises
  -- paranthesization

p1 = 2 + (2*3) - 1
p2 = (10^(1+1))
p3 = ((2^2)*(4^5))+1

  -- equivalent expressions

-- 1 and 2 only

  -- more fun with functions

--z = 7
--y = z + 8
--x = y ^ 2
--waxOn = x * 5

mff1a = 1135
mff1b = 1135
mff1c = -1110
mff1d = 1110

mff3 = 3375

waxOn = x * 5
    where x = y^2
          y = z+8
          z = 7

triple x = x * 3

waxOff x = triple x

