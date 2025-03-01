Module Type Foo. End Foo. Module Bar:Foo. End Bar. Module Baz(F1 F2:Foo). End Baz. Module Quux:=Baz Bar Bar.
