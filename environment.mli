type 'a t


val empty : 'a t
val add : Syntax.id -> 'a -> 'a t -> 'a t
val remove : Syntax.id -> 'a t -> 'a t
val find : Syntax.id -> 'a t -> 'a
val values : 'a t -> 'a list
