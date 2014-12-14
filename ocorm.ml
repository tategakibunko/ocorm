(*
  ocorm.ml

  Copyright (c) 2011- by Masaki WATANABE <lambda.watanabe@gmail.com>
*)
open ExtLib

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
  | StringChoices of string list

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

type attribute = field_name * property

exception ValidationError of string

module type Schema = sig
  val table_name : string
  val props : attribute list
end

module type FieldMap = sig
  type t
  val null : t
  val map_value : property -> field_value -> t
  val map_alist : (field_name * t) list -> t
  val map_list : t list -> t
end

module type ValidateMap = sig
  val validate : field_name -> field_value -> unit
end

module type RelationMap = sig
  type t

  val fields :
    ?without : field_name list ->
    unit ->
    field_name list

  val objectify :
    ?joined_props:attribute list list ->
    ?alias_props:attribute list ->
    (field_name * field_value) list -> t

  val objectify_many :
    ?joined_props:(field_name * property) list list ->
    ?alias_props:(alias_name * property) list ->
    (field_name * field_value) list list -> t
end

let find_property ?(table_name="") ?(joined_props=[]) ?(alias_props=[]) name props =
  try Some (List.assoc name props) with Not_found ->
    (try Some (List.assoc name @@ List.concat joined_props) with Not_found ->
      (try Some (List.assoc name alias_props) with Not_found -> None))

(**
   (name, value) -> (name, value, property) 
*)
let append_property ?(table_name="") ?(joined_props=[]) ?(alias_props=[]) ?(table_name="") ?(props=[]) fields =
  List.map (fun (name, value) ->
    match find_property name props ~joined_props ~alias_props ~table_name with
      | Some property -> (name, value, property)
      | None -> failwith (spf "property %s not found" name)
  ) fields

module MakeValidateMap (S : Schema) = struct
  let table_name = S.table_name
  let props = S.props

  let validate_str name value cond =
    let len = UTF8.length value in
    match cond with
      | StringChoices(choices) ->
	if List.exists ((=) value) choices = false then
	  raise @@ ValidationError(spf "%s is not available value for %s" value name)
      | MinLength(minlen) ->
	if len < minlen then
	  raise @@ ValidationError(spf "%s too short (min %d, now %d)" name minlen len)
      | MaxLength(maxlen) ->
	if len > maxlen then
	  raise @@ ValidationError(spf "%s too long (max %d, now %d)" name maxlen len)
      | NotNull ->
	if value = "" then
	  raise @@ ValidationError(spf "%s empty value not allowed" name)
      | _ -> ()

  let validate_int name value cond = 
    let num = int_of_string value in
    match cond with
      | Min(`Int minval) ->
	if num < minval then
	  raise @@ ValidationError(spf "%s too small (min %d, now %d)" name minval num)
      | Max(`Int maxval) ->
	if num > maxval then
	  raise @@ ValidationError(spf "%s too large (max %d, now %d)" name maxval num)
      | NotNull ->
	if value = "" then
	  raise @@ ValidationError(spf "%s empty value not allowed" name)
      | _ -> ()

  let validate_float name value cond =
    let num = float_of_string value in
    match cond with
      | Min(`Float minval) ->
	if num < minval then
	  raise @@ ValidationError(spf "%s too small (min %f, now %f)" name minval num)
      | Max(`Float maxval) ->
	if num > maxval then
	  raise @@ ValidationError(spf "%s too large (max %f, now %f)" name maxval num)
      | NotNull ->
	if value = "" then
	  raise @@ ValidationError(spf "%s empty value not allowed" name)
      | _ -> ()

  let validate_property name value = function
    | StringProperty(conds) ->
      List.iter (validate_str name value) conds
    | TextProperty(conds) ->
      List.iter (validate_str name value) conds
    | IntProperty(conds) ->
      List.iter (validate_int name value) conds
    | FloatProperty(conds) ->
      List.iter (validate_float name value) conds
    | _ -> ()

  let validate name value =
    match find_property name props ~table_name with
      | Some property -> validate_property name value property
      | None -> ()
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

  let objectify ?(joined_props=[]) ?(alias_props=[]) nv_alist =
    append_property nv_alist ~joined_props ~alias_props ~table_name ~props +>
      List.map (fun (name, value, prop) ->
	name, map_value prop value
      ) +> map_alist

  let objectify_many ?(joined_props=[]) ?(alias_props=[]) nv_alist_list =
    map_list @@ List.map (objectify ~joined_props ~alias_props) nv_alist_list
end
