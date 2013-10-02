open Ocorm
open Printf

let (@@) f g = f g
let spf = Printf.sprintf

module JsonFieldMap = struct
  type t = Json_type.t
  let null = Json_type.Null
  let map_value prop value =
    match prop with
      | PrimaryKeyProperty -> Json_type.Int (int_of_string value)
      | StringProperty _ -> Json_type.String value
      | TextProperty _ -> Json_type.String value
      | IntProperty _ -> if value = "" then Json_type.Null else Json_type.Int (int_of_string value)
      | FloatProperty _ -> if value = "" then Json_type.Null else Json_type.Float (float_of_string value)
      | BooleanProperty _ -> if value = "" then Json_type.Null else Json_type.Bool (bool_of_string value)
  let map_alist alist = Json_type.Object alist
  let map_list lst =  Json_type.Array lst
end
;;

module TestUserJson = MakeRelationMap(JsonFieldMap)(Schema.TestUser)

let () =
  let rows = [
    [("user_id", "taro"); ("email", "taro@example.com"); ("age", "20")];
    [("user_id", "jiro"); ("email", "jiro@example.com"); ("age", "19")];
  ] in
  let json = Json_type.Array (List.map TestUserJson.objectify rows) in
  print_endline @@ Json_io.string_of_json json
;;



