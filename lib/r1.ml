(* open Utils *)

type expr =
  Fixnum of int
  | Read
  | Add of expr * expr
  | Neg of expr
  | Var of string
  | Let of { label: string; initial: expr; body: expr }

let rec exp env = function
  Fixnum i -> i
  | Read ->
      print_string "read: ";
      read_line () |> int_of_string
  | Add (a, b) -> exp env a + exp env b
  | Neg e -> -(exp env e)
  | Var ident -> List.assoc ident env
  | Let binding ->
      (* We define a new environment that is the old one plus the new let binding *)
      let new_env = env @ [(binding.label, exp env binding.initial)] in
      exp new_env binding.body

module C0 = struct

end

module Asm = struct
  type register = Rsp | Rbp | Rax
  type arg = Int of int | Reg of register | Deref of register * int
  type instr =
    Add of arg * arg
    | Sub of arg * arg
    | Mov of arg * arg
    | Ret
    | Neg of arg
    | Call of string
    | Push of arg
    | Pop of arg

  type program = instr list
end

let preamble = ".globl main"
