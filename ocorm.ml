(*
  ocorm.ml

  Copyright (c) 2011- by Masaki WATANABE <lambda.watanabe@gmail.com>
*)

open CamomileLibrary

let (@@) f g = f g
let (+>) f g = g f
let spf = Printf.sprintf

type prop_default = [
  `Int of int
| `Float of float 
| `String of string
| `Bool of bool
| `Null
]

type prop_min_max = [
  `Int of int
| `Float of float
]

type prop_constraint =
    NotNull
  | Default of prop_default
  | Min of prop_min_max
  | Max of prop_min_max
  | MaxLength of int
  | MinLength of int

type property =
    PrimaryKeyProperty
  | StringProperty of prop_constraint list
  | TextProperty of prop_constraint list
  | IntProperty of prop_constraint list
  | FloatProperty of prop_constraint list
  | BooleanProperty of prop_constraint list

type alias_name = string
type field_name = string
type field_value = string

exception ValidationError of field_name * string

module type Schema = sig
  val table_name : string
  val props : (field_name * property) list
end

module type FieldMap = sig
  type t
  val null : t
  val map_value : property -> field_value -> t
  val map_alist : (field_name * t) list -> t
  val map_list : t list -> t
end
;;

module type RelationMap = sig
  type t

  val fields :
    ?without : field_name list ->
    unit ->
    field_name list

  val objectify :
    ?joined_props:(field_name * property) list list ->
    ?alias_props:(alias_name * property) list ->
    (field_name * field_value) list -> t

  val validate : field_name -> field_value -> unit
end

module MakeRelationMap (F : FieldMap) (S : Schema) = struct
  type t = F.t
  let table_name = S.table_name
  let props = S.props
  let map_list = F.map_list
  let map_alist = F.map_alist
  let map_value = F.map_value

  let fields ?(without=[]) () =
    List.map fst props +>
      List.filter (fun field ->
	List.for_all ((<>) field) without
      )

  let find_property ?(joined_props=[]) ?(alias_props=[]) name =
    try List.assoc name props with Not_found ->
      (try List.assoc name @@ List.concat joined_props with Not_found ->
	(try List.assoc name alias_props with Not_found ->
	  failwith (spf "property %s is not defined in %s" name table_name)))

  let constraint_of = function
    | PrimaryKeyProperty -> []
    | StringProperty lst -> lst
    | TextProperty lst -> lst
    | IntProperty lst -> lst
    | FloatProperty lst -> lst
    | BooleanProperty lst -> lst

  let string_of_default = function
    | `Int i -> string_of_int i
    | `Float f -> string_of_float f
    | `Bool b -> string_of_bool b
    | `String s -> s
    | _ -> ""

  let get_default_value = function
    | PrimaryKeyProperty -> "null"
    | other ->
      let constr = constraint_of other in
      let rec iter = function
	| (Default x) :: rest -> string_of_default x
	| h :: rest -> iter rest
	| [] -> "null" in
      iter constr

  (* (name, value) -> (name, value, property) *)
  let append_property ?(joined_props=[]) ?(alias_props=[]) fields =
    List.map (fun (name, value) ->
      (name, value, find_property name ~joined_props ~alias_props)
    ) fields

  let objectify ?(joined_props=[]) ?(alias_props=[]) nv_alist =
    append_property nv_alist ~joined_props ~alias_props +>
      List.map (fun (name, value, prop) ->
	name, map_value prop value
      ) +> map_alist

  let error field_name msg =
    raise @@ ValidationError(field_name, msg)

  let validate_str name value cond =
    let len = UTF8.length value in
    match cond with
      | MinLength(minlen) ->
	if len < minlen then
	  error name @@ spf "too short (min %d, now %d)" minlen len
      | MaxLength(maxlen) ->
	if len > maxlen then
	  error name @@ spf "too long (max %d, now %d)" maxlen len
      | NotNull ->
	if value = "" then
	  error name "empty value not allowed"
      | _ -> ()

  let validate_int name value cond = 
    let num = int_of_string value in
    match cond with
      | Min(`Int minval) ->
	if num < minval then
	  error name @@ spf "too small (min %d, now %d)" minval num
      | Max(`Int maxval) ->
	if num > maxval then
	  error name @@ spf "too large (max %d, now %d)" maxval num
      | NotNull ->
	if value = "" then
	  error name "empty value not allowed"
      | _ -> ()

  let validate_float name value cond =
    let num = float_of_string value in
    match cond with
      | Min(`Float minval) ->
	if num < minval then
	  error name @@ spf "too small (min %f, now %f)" minval num
      | Max(`Float maxval) ->
	if num > maxval then
	  error name @@ spf "too large (max %f, now %f)" maxval num
      | NotNull ->
	if value = "" then
	  error name "empty value not allowed"
      | _ -> ()

  let validate name value = 
    match find_property name with
      | StringProperty(conds) ->
	List.iter (validate_str name value) conds
      | TextProperty(conds) ->
	List.iter (validate_str name value) conds
      | IntProperty(conds) ->
	List.iter (validate_int name value) conds
      | FloatProperty(conds) ->
	List.iter (validate_float name value) conds
      | _ -> ()
end
