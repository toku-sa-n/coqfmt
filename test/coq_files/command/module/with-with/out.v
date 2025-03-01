Module Type Foo.
  Parameter A : Type.
End Foo.

Module Bar : Foo with Definition A := nat.
  Definition A := nat.
End Bar.
