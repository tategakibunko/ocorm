(*
  ocorm.mli

  Copyright (c) 2011- by Masaki WATANABE <lambda.watanabe@gmail.com>
*)

type prop_default = [
  `Bool of bool
| `Float of float
| `Int of int
| `String of string 
| `Null
]

type prop_min_max = [
  `Float of float
| `Int of int 
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

module type RelationMap = sig
  type t

  val objectify :
    ?joined_props:(field_name * property) list list ->
    ?alias_props:(alias_name * property) list ->
    (field_name * field_value) list -> t

  val validate : field_name -> field_value -> unit
end


module MakeRelationMap (F : FieldMap) (S : Schema) : RelationMap with type t = F.t
(** Functor building an implementation of the model structure
    given a field mapper(map_value, map_alist, map_list)
    and schema definitions(table_name, props)
*)
