module Cmp where

infix 4 >, >#, >=, >=#, <, <#, <=, <=# , ==, ==#, !=, !=#, `cmp`
class Eq# (a ∷ T r) where
  (==), (!=) ∷ a → a → B
  (==#), (!=#) ∷ a → a → B#
class Eq# a ⇒ Cmp# (a ∷ T r) where
  (>),(>=),(<),(<=) ∷ a → a → B
  (>#),(>=#),(<#),(<=#) ∷ a → a → B#
  cmp ∷ a → a → Ordering
  min,max ∷ a → a → a
