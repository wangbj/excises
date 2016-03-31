-- some excises for learning ttfp (type theory for functional programming).
-- most of the code is *borrowed* from learn you an agda shamelessly

-- Function composition
_∘_ : {A : Set} { B : A -> Set} { C : (x : A) -> B x -> Set}
      (f : {x : A}(y : B x) -> C x y) (g : (x : A) -> B x)
      (x : A) -> C x (g x)
(f ∘ g) x = f (g x)

-- Conjuntion, ∏-type in haskell

data _∧_ (α : Set) (β : Set) : Set where
  ∧-intro : α → β → (α ∧ β)

∧-elim₁ : {α β : Set} → (α ∧ β) → α
∧-elim₁ (∧-intro p q) = p

∧-elim₂ : {α β : Set} → (α ∧ β) → β
∧-elim₂ (∧-intro p q) = q

_⇔_ : (α : Set) → (β : Set) → Set
a ⇔ b = (a → b) ∧ (b → a)

∧-comm′ : {α β : Set} → (α ∧ β) → (β ∧ α)
∧-comm′ (∧-intro a b) = ∧-intro b a

∧-comm : {α β : Set} → (α ∧ β) ⇔ (β ∧ α)
∧-comm = ∧-intro ∧-comm′ ∧-comm′

-- type signature cannot be eliminated, was expecting type could be infered but it wasn't the case.
e₁e₁ : { P Q R : Set} → ( (P ∧ Q) ∧ R ) → P
e₁e₁ = ∧-elim₁ ∘ ∧-elim₁
e₂e₁ : { P Q R : Set} → ( (P ∧ Q) ∧ R) → Q
e₂e₁ = ∧-elim₂ ∘ ∧-elim₁

∧-assoc₁ : { P Q R : Set } → ((P ∧ Q) ∧ R) → (P ∧ (Q ∧ R))
∧-assoc₁ = λ x → ∧-intro (∧-elim₁ (∧-elim₁ x)) (∧-intro (∧-elim₂ (∧-elim₁ x)) (∧-elim₂ x))

∧-assoc₂ : {P Q R : Set} → (P ∧ (Q ∧ R)) → ((P ∧ Q) ∧ R)
∧-assoc₂ = λ x → ∧-intro (∧-intro (∧-elim₁ x) (∧-elim₁ (∧-elim₂ x))) (∧-elim₂ (∧-elim₂ x))

∧-assoc : {P Q R : Set} → ((P ∧ Q) ∧ R) ⇔ (P ∧ (Q ∧ R))
∧-assoc = ∧-intro ∧-assoc₁ ∧-assoc₂

-- Disjunctions, ∑-type in haskell

data _∨_ (P Q : Set) : Set where
  ∨-intro₁ : P → P ∨ Q
  ∨-intro₂ : Q → P ∨ Q

∨-elim : {A B C : Set} → (A ∨ B) → (A → C) → (B → C) → C
∨-elim (∨-intro₁ a) ac bc = ac a
∨-elim (∨-intro₂ b) ac bc = bc b

∨-comm′ : {P Q : Set} → (P ∨ Q) → (Q ∨ P)
∨-comm′ (∨-intro₁ p) = ∨-intro₂ p
∨-comm′ (∨-intro₂ q) = ∨-intro₁ q

∨-comm : {P Q : Set} → (P ∨ Q) ⇔ (Q ∨ P)
∨-comm = ∧-intro ∨-comm′ ∨-comm′
