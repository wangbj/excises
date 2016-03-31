module List where

data Bool : Set where
  true : Bool
  false : Bool

¬ : Bool -> Bool
¬ true = false
¬ false = true

_∨_ : Bool -> Bool -> Bool
false ∨ x = x
true ∨ _ = true

_∧_ : Bool -> Bool -> Bool
false ∧ _ = false
true ∧ x = x

if_then_else_ : {A : Set} -> Bool -> A -> A -> A
if true  then x else y = x
if false then x else y = y

data ℕ : Set where
  zero : ℕ
  succ : ℕ -> ℕ

one = succ zero
two = succ one
three = succ two
four = succ three
five = succ four

_+_ : ℕ -> ℕ -> ℕ
zero + m = m
(succ n) + m = succ (n + m)

_×_ : ℕ -> ℕ -> ℕ
zero × m = zero
(succ n) × m = m + (n × m)

infixl 60 _×_
infixl 40 _+_
infixr 20 _∨_
infixr 20 _∧_
infix  5 if_then_else_

identity : (A : Set) -> A -> A
identity A x = x

-- {} means implicit arguments
id : {A : Set} -> A -> A
id x = x

_∘_ : {A : Set} { B : A -> Set} { C : (x : A) -> B x -> Set}
      (f : {x : A}(y : B x) -> C x y) (g : (x : A) -> B x)
      (x : A) -> C x (g x)
(f ∘ g) x = f (g x)

apply : (A : Set) (B : A -> Set) -> ( (x : A) -> B x) -> (a : A) -> B a
apply A B f a = f a

data _✯ (α : Set) : Set where
  ε : α ✯
  _◁_ : α → α ✯ → α ✯

infixr 10 _◁_
infixr 5 _++_

map : {α β : Set} → (α → β) → α ✯ → β ✯
map f ε = ε
map f (x ◁ xs) = f x ◁ map f xs

_++_ : {α : Set} → α ✯ → α ✯ → α ✯
ε ++ ys = ys
(x ◁ xs) ++ ys = x ◁ (xs ++ ys)

null : {α : Set} → α ✯ → Bool
null ε = true
null _ = false

length : {α : Set} → α ✯ → ℕ
length ε = zero
length (_ ◁ xs) = succ (length xs)
