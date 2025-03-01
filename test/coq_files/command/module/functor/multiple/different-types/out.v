Module Type Foo.
End Foo.

Module Type Bar.
End Bar.

Module Baz (F : Foo) (B : Bar).
End Baz.
