fun printMatrix A =
  RealMatrix.appRow(A, fn row =>
    let
      val s =
        String.concatWith "\t"
          (Vector.foldr (op ::) nil (Vector.map Real.toString row)) ^ "\n"
    in
      print s
    end)

val () = let
  open RealMatrix

  val A = RealMatrix.fromRows [
    [1.0, 2.0, 0.0],
    [0.0, 3.0, 0.0],
    [2.0, ~4.0, 2.0]
  ]

  val P = RealMatrix.fromRows [
    [~1.0, 0.0, ~1.0],
    [~1.0, 0.0, 0.0],
    [2.0, 1.0, 2.0]
  ]

  val P' = inverse P

  val hr = String.concat (List.tabulate(80, fn _ => "=")) ^ "\n"
in
  print hr;
  print "A =\n";
  print hr;
  printMatrix A;
  print hr;
  print "P =\n";
  print hr;
  printMatrix P;
  print hr;
  print "P^{-1}AP =\n";
  print hr;
  printMatrix (P' * A * P);
  print hr
end
