open Ocorm
open Printf

let (@@) f g = f g
let spf = Printf.sprintf

type tvalue =
  | Tstr of string
  | Tint of int
  | Tfloat of float
  | Tbool of bool
  | Tlist of tvalue list
  | Tobj of (string * tvalue) list
  | Tnull

let rec string_of_tvalue = function
  | Tnull -> "null"
  | Tstr x -> "'" ^ x ^ "'"
  | Tint x -> string_of_int x
  | Tfloat x -> string_of_float x
  | Tbool x -> string_of_bool x
  | Tlist lst -> spf "[%s]" (String.concat ",\n" @@ List.map string_of_tvalue lst)
  | Tobj alist ->
    String.concat "" [
      "{";
      String.concat "," @@ List.map (fun (n, v) -> spf "%s:%s" n (string_of_tvalue v)) alist;
      "}";
    ]

module ObjFieldMap = struct
  type t = tvalue
  let null = Tnull
  let map_value property value =
    match property with
      | PrimaryKeyProperty -> Tint (int_of_string value)
      | StringProperty _ -> Tstr value
      | TextProperty _ -> Tstr value
      | IntProperty _ -> Tint (int_of_string value)
      | FloatProperty _ -> Tfloat (float_of_string value)
      | BooleanProperty _ -> Tbool (value = "t")
  let map_alist alist =   Tobj alist
  let map_list lst = Tlist lst
end

module TestUserObj = MakeRelationMap(ObjFieldMap)(Schema.TestUser)

let () =
  let rows = [
    [("user_id", "taro"); ("email", "taro@example.com"); ("age", "20")];
    [("user_id", "jiro"); ("email", "jiro@example.com"); ("age", "19")];
  ] in
  let objs = Tlist (List.map TestUserObj.objectify rows) in
  print_endline @@ string_of_tvalue objs



