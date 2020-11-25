module B (B(B#,F,T), module B) where


infixl 3 ∧
infixl 2 ∨
(∧), (∨) ∷ B → B → B
(∧) = coerce andI#; (∨) = coerce orI#;
not ∷ B → B
not = coerce xorI# 1#
