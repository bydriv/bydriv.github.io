Inductive A : Type :=
  | introA : ((A->Prop)->Prop) -> A.
