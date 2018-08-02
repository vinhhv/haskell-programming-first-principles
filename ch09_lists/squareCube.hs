module SquareCube where

mySqr  = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

mySqrCube = [(x, y) | x <- mySqr,
                      y <- myCube]

mySqrCubeLT50 = [(x, y) | x <- mySqr,
                          y <- myCube,
                          x < 50,
                          y < 50]

len = length mySqrCubeLT50
