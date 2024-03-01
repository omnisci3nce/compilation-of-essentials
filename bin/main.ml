open Coe

let test_program_src = "(+ (+ (5) (read)) (6))"

let () =
  let output = test_program_src
    |> R0.tokenise
    |> R0.parse
    |> R0.interpret in
  Printf.printf "Output: %d\n" output