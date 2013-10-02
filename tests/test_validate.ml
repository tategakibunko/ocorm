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
;;

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
  let map_list list = Tlist list
end

module TestUser = struct
  let table_name = "test_user"
  let props = [
    ("user_id", StringProperty [NotNull; MinLength 3; MaxLength 16]);
    ("email", StringProperty [NotNull]);
    ("age", IntProperty [NotNull]);
  ]
end

module TestUserObj = MakeRelationMap(ObjFieldMap)(TestUser)

let () =
  (try
     print_endline @@ spf "start validate for %s" TestUser.table_name;
     TestUserObj.validate "user_id" "someone";
     TestUserObj.validate "user_id" "a"
   with
       ValidationError(field, msg) ->
	 printf "validation works!\n error:field = %s, reason = %s\n" field msg
  )
;;



