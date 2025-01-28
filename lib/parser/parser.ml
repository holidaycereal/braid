(* open Lexer *)
open Stdint

type num_kind = Signed | Unsigned | Floating
type width = W8 | W16 | W32 | W64

type terminal =
  | NumVal of (uint64 * num_kind * width)
  | BoolVal of bool
  | CharVal of uint32
  | StringVal of (string * int)
