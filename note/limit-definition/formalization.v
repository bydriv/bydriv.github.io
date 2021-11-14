Require Import Coq.Reals.Reals.
Require Import Coq.Lists.Streams.

Open Scope R.

Import Streams.

Definition lim (xs : Stream R) (x : R) :=
  forall epsilon : R,
    epsilon > 0 ->
    exists n0 : N,
      forall n : N,
        (n > n0)%N ->
        let xn := Str_nth (A:=R) (N.to_nat n) xs in
        R_dist xn x < epsilon.

Lemma const_stream : forall x : R, forall n : nat, Str_nth n (const x) = x.
Proof.
  intros x n. induction n as [ | k H ].
  unfold Str_nth, const. simpl. reflexivity.
  unfold Str_nth, const. simpl. apply H.
Qed.

Lemma x_minus_x_is_zero : forall x : R, x - x = 0.
Proof.
  unfold Rminus. apply Rplus_opp_r.
Qed.

Example ones : lim (const 1) 1.
Proof.
  intros epsilon epsilon_is_greater_than_0. unfold Rgt in epsilon_is_greater_than_0.
  apply ex_intro with 0%N.
  intros n n_is_greater_than_n0.
  rewrite (const_stream 1).
  rewrite (R_dist_eq 1).
  apply epsilon_is_greater_than_0.
Qed.

Lemma x_is_greater_than_0_implies_x_div_2_is_greater_than_0 : forall x : R, x > 0 -> x / 2 > 0.
  assert (two_is_INR_2 : 2 = INR 2). auto.
  assert (two_is_greater_than_0 : 2 > 0). rewrite two_is_INR_2. apply lt_0_INR. auto.
  assert (two_inv_is_greater_than_0 : / 2 > 0). rewrite two_is_INR_2. apply (Rinv_0_lt_compat (INR 2) two_is_greater_than_0).
Proof.
  intros x x_is_greater_than_0.
  rewrite <- (Rmult_0_l (/ 2)).
  unfold Rdiv.
  apply (Rmult_gt_compat_r (/ 2) x 0 two_inv_is_greater_than_0 x_is_greater_than_0).
Qed.

Theorem lim_add :
  forall xs ys : Stream R,
  forall x y : R,
    lim xs x ->
    lim ys y ->
    lim (zipWith (fun xn yn => xn + yn) xs ys) (x + y).
Proof.
  intros xs ys x y lim_xs_x lim_ys_y.
  intros epsilon epsilon_is_greater_than_0.

  assert (epsilon_div_2_is_greater_than_0 : epsilon / 2 > 0).
  apply (x_is_greater_than_0_implies_x_div_2_is_greater_than_0 epsilon epsilon_is_greater_than_0).

  destruct (lim_xs_x (epsilon / 2) epsilon_div_2_is_greater_than_0) as [ n1 H1 ].
  destruct (lim_ys_y (epsilon / 2) epsilon_div_2_is_greater_than_0) as [ n2 H2 ].

  apply ex_intro with (N.max n1 n2).
  intros n n_is_greater_than_n0.

  assert (n1_is_less_than_n : (n1 < n)%N).
  apply (N.le_lt_trans n1 (N.max n1 n2) n (N.le_max_l n1 n2) (N.gt_lt n (N.max n1 n2) n_is_greater_than_n0)).
  assert (n2_is_less_than_n : (n2 < n)%N).
  apply (N.le_lt_trans n2 (N.max n1 n2) n (N.le_max_r n1 n2) (N.gt_lt n (N.max n1 n2) n_is_greater_than_n0)).

  assert (abs_xn_minus_x_is_less_than_epsilon_div_2 : R_dist (Str_nth (N.to_nat n) xs) x < epsilon / 2).
  apply (H1 n (N.lt_gt n1 n n1_is_less_than_n)).
  assert (abs_yn_minus_y_is_less_than_epsilon_div_2 : R_dist (Str_nth (N.to_nat n) ys) y < epsilon / 2).
  apply (H2 n (N.lt_gt n2 n n2_is_less_than_n)).

  rewrite (Str_nth_zipWith (fun xn yn => xn + yn) (N.to_nat n) xs ys).
  rewrite (double_var epsilon).

  apply
    ( let Ax := Str_nth (N.to_nat n) xs in
      let Ay := Str_nth (N.to_nat n) ys in
      let A := R_dist (Ax + Ay) (x + y) in
      let Bx := R_dist Ax x in
      let By := R_dist Ay y in
      let B := Bx + By in
      let Cx := epsilon / 2 in
      let Cy := epsilon / 2 in
      let C := Cx + Cy in
      Rle_lt_trans
        A
        B
        C
        (R_dist_plus Ax x Ay y)
        (Rplus_lt_compat
           Bx Cx By Cy
           abs_xn_minus_x_is_less_than_epsilon_div_2
           abs_yn_minus_y_is_less_than_epsilon_div_2) ).
Qed.
