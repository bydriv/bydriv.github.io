Require Import ZArith.
Open Scope Z.

Axiom A : Set.
Axiom f : A -> A -> Z.

Axiom A1 : forall x y : A, x = y <-> f x y = 0.
Axiom A2 : forall x y : A, f x y = -(f y x).
Axiom A3 : forall x y z : A, Z.abs (f x y) + Z.abs (f y z) >= Z.abs (f x z).
Axiom A4 : forall x y z : A, f x y >= 0 /\ f y z >= 0 -> f x z >= 0.

Lemma L1 : forall i j : Z, i >= 0 -> j >= 0 -> i + j >= 0.
Proof.
  intros i j P Q.
  omega.
Qed.

Lemma L2 : forall i : Z, i >= 0 -> i = Z.abs i.
Proof.
  intros i P.
  assert (i >= 0 -> i = i). congruence.
  assert (i <= 0 -> i = -i). omega.
  apply (Zabs_ind (fun j => i = j) i H H0).
Qed.

Lemma L3 : forall x y : A, f x y <= 0 <-> f y x >= 0.
Proof.
  intros x y. rewrite A2. omega.
Qed.

Lemma L4 : forall i j : Z, i - j = i + (- j).
Proof.
  intros i j.
  omega.
Qed.

Lemma L5 : forall i j : Z, i + (- - j) = i + j.
Proof.
  intros i j.
  omega.
Qed.

Theorem T1 : forall x y z : A, f x y >= 0 -> f y z >= 0 -> f x y + f y z >= f x z /\ f x z >= 0.
Proof.
  intros x y z P Q.
  assert (f x z >= 0). apply (A4 x y z (conj P Q)).
  assert (f x y = Z.abs (f x y)). apply (L2 (f x y) P).
  assert (f y z = Z.abs (f y z)). apply (L2 (f y z) Q).
  assert (f x z = Z.abs (f x z)). apply (L2 (f x z) H).
  rewrite H0.
  rewrite H1.
  rewrite H2.
  split.
  apply (A3 x y z).
  omega.
Qed.

Theorem T2 : forall x y z : A, f x y <= 0 -> f y z <= 0 -> f x y + f y z <= f x z /\ f x z <= 0.
Proof.
  intros x y z P Q.
  assert (f z y + f y x >= f z x /\ f z x >= 0).
  apply (T1 z y x (proj1 (L3 y z) Q) (proj1 (L3 x y) P)).
  rewrite (A2 x y).
  rewrite (A2 y z).
  rewrite (A2 x z).
  omega.
Qed.

Theorem T3 : forall x y z : A, f x y >= 0 -> f z y <= 0 -> f x z >= 0.
Proof.
  intros x y z P Q.
  apply (A4 x y z (conj P (proj1 (L3 z y) Q))).
Qed.

Theorem T4 : forall x y z : A, f x y <= 0 -> f z y >= 0 -> f z x >= 0.
Proof.
  intros x y z P Q.
  apply (A4 z y x (conj Q (proj1 (L3 x y) P))).
Qed.

Theorem T5 : forall x y z : A, f x y <= 0 -> f y z <= 0 -> f x z <= 0.
Proof.
  intros x y z P Q.
  apply (L3 x z).
  apply (A4 z y x (conj (proj1 (L3 y z) Q) (proj1 (L3 x y) P))).
Qed.

Theorem T6 : forall x y z : A, f x y >= 0 -> f z y <= 0 -> f z x <= 0.
Proof.
  intros x y z P Q.
  apply (T5 z y x Q (proj2 (L3 y x) P)).
Qed.

Theorem T7 : forall x y z : A, f x y <= 0 -> f z y >= 0 -> f x z <= 0.
Proof.
  intros x y z P Q.
  apply (T5 x y z P (proj2 (L3 y z) Q)).
Qed.
