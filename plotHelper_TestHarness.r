## Test 1, use defaults:
# INPUT:    1,    2,    3,    4,    5
# DIFF:     0,    0,    2,    2,    2
# GROWTH:   0.0,  0.0,  2.0,  1.0,  0.5

setwd("C:/Users/Rainy/Documents/Github/Economics")
source("plotHelper.r")

d.growth.test <- CalcYoY()
print(d.growth.test)
