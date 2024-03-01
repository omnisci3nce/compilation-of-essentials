let is_alpha c = let code = Char.code c in
  (code >= Char.code('A') && code <= Char.code('Z')) ||
  (code >= Char.code('a') && code <= Char.code('z'))

let is_digit c = let code = Char.code c in
  code >= Char.code('0') && code <= Char.code('9')

let is_alpha_numeric c = is_alpha c || is_digit c 