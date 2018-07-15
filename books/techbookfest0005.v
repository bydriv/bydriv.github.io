Require Import Coq.Arith.Peano_dec.
Require Import Coq.Arith.Lt.
Require Import Coq.Lists.ListSet.

Definition var : Type := nat.

Inductive term (v : Type) : Type :=
| term_var : v -> term v
| term_app : term v -> term v -> term v
| term_abs : (v -> term v) -> term v.

Fixpoint length (e1 : term var) : nat :=
  match e1 with
    | term_var _ => 1
    | term_app e2 e3 => length e2 + length e3
    | term_abs f => S (length (f 0))
  end.

Lemma term_len : forall e1, 0 < length e1.
Proof.
  intro e1.
  induction e1.
  (* e1 = term_var var v *)
  auto.
  (* e1 = term_app var e1_1 e1_2 *)
  simpl. rewrite (S_pred (length e1_1) 0 IHe1_1). rewrite (S_pred (length e1_2) 0 IHe1_2).
  simpl. apply neq_0_lt. auto.
  (* e1 = term_abs var t *)
  (* t : var -> term var *)
  simpl. apply neq_0_lt. auto.
Qed.

Fixpoint FV' (xn : var) (e1 : term var) : set var :=
  match e1 with
    | term_var x1 => set_add eq_nat_dec x1 (empty_set var)
    | term_app e2 e3 => set_union eq_nat_dec (FV' xn e2) (FV' xn e3)
    | term_abs f => set_remove eq_nat_dec xn (FV' (S xn) (f xn))
  end.

Definition FV : term var -> set var := FV' 0.

Fixpoint BV' (xn : var) (e1 : term var) : set var :=
  match e1 with
    | term_var x1 => empty_set var
    | term_app e2 e3 => set_union eq_nat_dec (BV' xn e2) (BV' xn e3)
    | term_abs f => set_add eq_nat_dec xn (BV' (S xn) (f xn))
  end.

Definition BV : term var -> set var := BV' 0.

Definition V (e1 : term var) : set var := set_union eq_nat_dec (FV e1) (BV e1).

Fixpoint subst (e1 : term var) (x1 : var) (e2 : term var) : term var :=
  match e2 with
    | term_var x2 =>
      if eq_nat_dec x1 x2 then
        e1
      else
        e2
    | term_app e3 e4 =>
      term_app var (subst e1 x1 e3) (subst e1 x1 e4)
    | term_abs f =>
      term_abs var (fun x3 => subst e1 x1 (f x3))
  end.

Lemma subst_len : forall x1 x2 e1, length (subst (term_var var x1) x2 e1) = length e1.
Proof.
  intros x1 x2 e1.
  induction e1.
  (* e1 = term_var v *)
  simpl. case eq_nat_dec.
  (* x2 = v *)
  simpl. congruence.
  (* x2 <> v *)
  simpl. congruence.
  (* e1 = term_app e1_1 e1_2 *)
  simpl. rewrite IHe1_1. rewrite IHe1_2. congruence.
  (* e1 = term_abs var t *)
  (* t : var -> term var *)
  simpl. apply f_equal. apply (H 0).
Qed.

Fixpoint alpha_congruence' (xn : var) (e1 : term var) (e2 : term var) : Prop :=
  match (e1, e2) with
    | (term_var x1, term_var x2) =>
      x1 = x2
    | (term_app e3 e4, term_app e5 e6) =>
      alpha_congruence' xn e3 e5 /\ alpha_congruence' xn e4 e6
    | (term_abs f, term_abs g) =>
      alpha_congruence' (S xn) (f xn) (g xn)
    | _ =>
      False
  end.

Definition alpha_congruence : term var -> term var -> Prop := alpha_congruence' 0.

Lemma alpha_refl : forall e1, alpha_congruence e1 e1.
Proof.
  intro e1.
  induction e1.
  (* e1 = term_var var v *)
  unfold alpha_congruence. simpl. reflexivity.
  (* e1 = term_app var e1_1 e1_2 *)
  unfold alpha_congruence. simpl. split. fold alpha_congruence. apply IHe1_1. fold alpha_congruence. apply IHe1_2.
  (* e1 = term_abs var t *)
  (* t : var -> term var *)
  unfold alpha_congruence. simpl. Check (H 0).
  (*remember (length e1) as n.
  generalize dependent e1.
  induction n.
  (* length e1 = 0 *)
  intros e1 Heqn. contradiction (lt_0_neq (length e1) (term_len e1)).
  (* length e1 = S n *)
  intros e1 Heqn. destruct e1.
  (* e1 = term_var var v *)
  unfold alpha_congruence. simpl. reflexivity.
  (* e1 = term_app var e1_1 e1_2 *)
  unfold alpha_congruence. simpl.*)
