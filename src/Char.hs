module Char where

gt y x = gtChar# x y
ge y x = geChar# x y
lt y x = ltChar# x y
le y x = leChar# x y
eq x y = eqChar# x y
ne x y = neChar# x y

fromInt ∷ Int → Char
fromInt = chr#
toInt ∷ Char → Int
toInt = ord#
