module Peano where

-- natural numbers
data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero + n = n
suc m + n = suc (m + n)

_×_ : ℕ → ℕ → ℕ
zero × n = zero
suc m × n = n + m × n

infixl 60 _×_
infixl 40 _+_

data _even : ℕ → Set where
  ZERO : zero even
  STEP : ∀ x → x even → suc (suc x) even

proof₁ : suc (suc (suc (suc zero))) even
proof₁ = STEP (suc (suc zero)) (STEP zero ZERO) -- prove by solve constraint

proof₂ : (suc (suc zero)) even → (suc (suc zero)) even
proof₂ ν = ν

proof₂′ : {α : Set} → α → α  -- identity
proof₂′ x = x

proof₂″ : (suc (suc zero)) even → (suc (suc zero)) even
proof₂″ x = proof₂′ x -- prove by identity

data _∧_ (α : Set) (β : Set) : Set where
  ∧-Introduction : α → β → (α ∧ β)

proof₃ : {α β : Set} → (α ∧ β) → α
proof₃ (∧-Introduction p q) = p

proof₃′ : {α β : Set} → (α ∧ β) → β
proof₃′ (∧-Introduction p q) = q

_⇔_ : (α : Set) → (β : Set) → Set
a ⇔ b = (a → b) ∧ (b → a)

∧-comm′ : {α β : Set} → (α ∧ β) → (β ∧ α)
∧-comm′ (∧-Introduction a b) = ∧-Introduction b a

∧-Communitive : {α β : Set} → (α ∧ β) ⇔ (β ∧ α)
∧-Communitive = ∧-Introduction ∧-comm′ ∧-comm′

-- http://learnyouanagda.liamoc.net/pages/proofs.html
