type inner = { buffer : Buffer.t }
type t = inner ref

(* {{The doc} https://v2.ocaml.org/api/Buffer.html} says to allocate 16 buffers
   if unsure. *)
let create () = ref { buffer = Buffer.create 16 }
let write t = Buffer.add_string !t.buffer
let contents t = Buffer.contents !t.buffer
