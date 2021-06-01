Require Import Coq.Lists.List.
Import Coq.Lists.List.ListNotations.

Variable V : Set.

Inductive formula : Set :=
  variable : V -> formula
| implication : formula -> formula -> formula
| conjunction : formula -> formula -> formula
| disjunction : formula -> formula -> formula
| bottom : formula.

Inductive proof : list formula -> formula -> Set :=
  assumption : forall p : formula, proof [p] p
| weakening : forall p q ps, proof ps q -> proof (p :: ps) q
| contraction : forall p q ps, proof (p :: p :: ps) q -> proof (p :: ps) q
| permutation : forall p q r ps, proof (p :: q :: ps) r -> proof (q :: p :: ps) r
| implication_intro : forall p q ps, proof (p :: ps) q -> proof ps (implication p q)
| implication_elim : forall p q ps qs, proof ps p -> proof qs (implication p q) -> proof (ps ++ qs) q
| conjunction_intro : forall p q ps qs, proof ps p -> proof qs q -> proof (ps ++ qs) (conjunction p q)
| conjunction_elim1 : forall p q ps, proof ps (conjunction p q) -> proof ps p
| conjunction_elim2 : forall p q ps, proof ps (conjunction p q) -> proof ps q
| disjunction_intro1 : forall p q ps, proof ps p -> proof ps (disjunction p q)
| disjunction_intro2 : forall p q ps, proof ps q -> proof ps (disjunction p q)
| disjunction_elim : forall p q r ps qs rs, proof ps (disjunction p q) -> proof (p :: qs) r -> proof (q :: rs) r -> proof (ps ++ qs ++ rs) r.

Definition p_implies_p (p : formula) : proof [] (implication p p) := implication_intro p p [] (assumption p).

Definition p_entails_not_not_p (p : formula) : proof [p] (implication (implication p bottom) bottom) :=
  let not_p := implication p bottom in
  let contradiction := implication_elim p bottom [p] [not_p] (assumption p) (assumption not_p) in
  implication_intro not_p bottom [p] (permutation p not_p bottom [] contradiction).
