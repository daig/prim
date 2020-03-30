module Char where

gt y x = B# do gtChar# x y
ge y x = B# do geChar# x y
lt y x = B# do ltChar# x y
le y x = B# do leChar# x y
eq x y = B# do eqChar# x y
ne x y = B# do neChar# x y

fromInt :: Int -> Char
fromInt = chr#
toInt :: Char -> Int
toInt = ord#
