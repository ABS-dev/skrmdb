library(skrmdb)

dead    = c(1, 3, 6, 9, 10)
tested = c(10, 10, 10, 10, 10)
dils   = 1:5

dt = data.frame(y = dead, n = tested, x = dils)

## Calling

# Current accepted calls:
SpearKarb(y = dead, n = tested, x = dils)
SpearKarb(cbind(y, n) ~ x, dt)

# Newly Available calls
SpearKarb(dead + tested ~ dils)

# Question 1: Should one of f(y =, n = , x = ) or f(y + n ~ x, data) be depreciated?

## Data ordering
SpearKarb(y = dead, n = tested, x = dils)
SpearKarb(y = rev(dead), n = rev(tested), x = rev(dils))

alive = tested - dead
SpearKarb(y = rev(alive), n = rev(tested), x = rev(dils))
SpearKarb(y = alive, n = tested, x = dils)

# Question 2: Is the message desirable or necessary?




##


## Getting effective Dose




# Historical access:
# 351 fn(...)$ed
#  26 fn(...)@ed
#   4 fn(...)['ed']
# 180 fn(...)
#   1 getED() 

SpearKarb(dead + tested ~ dils)$ed
SpearKarb(dead + tested ~ dils)@ed #(Changed from S3 object)
SpearKarb(dead + tested ~ dils)['ed']
getED(SpearKarb(dead + tested ~ dils))

# Use Rev in function call
#  56 Yes
# 505 No

SpearKarb(rev(dead) + rev(tested) ~ rev(dils))
SpearKarb(dead + tested ~ dils)

# ways funciotns have been accessed
# 325 f(y, n, x)
# 225 f(fn, data)
#  11 f(fn, .)   doesn't work anymore.



SpearKarb(rev(y) + rev(n) ~ rev(x), dt)
SpearKarb(y + n ~ x, dt)

SpearKarb(y = dead, n = tested, x = dils)
SpearKarb(y = rev(dead), n = rev(tested), x = rev(dils))
SpearKarb(y = tested - dead, n = tested, x = dils)

SpearKarb(y + n ~ x, .)

SpearKarb(cbind(y) ~ x)

dead
tested
dils
lm(dead ~ dils, data = .)

SpearKarb(y = dead, n = tested, x = dils)
