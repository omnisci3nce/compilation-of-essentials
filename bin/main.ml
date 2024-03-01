open Coe

(* let test_program_src = "(+ (+ (5) (read)) (6))" *)
let test_program_src = "(+ (read) (- (+ 5 3)))"

let () =
  print_endline test_program_src;
  let output = test_program_src
    |> R0.tokenise ~print:false
    |> R0.parse
    |> R0.interpret in
  Printf.printf "Output: %d\n" output