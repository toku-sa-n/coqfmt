Module Type Foo. End Foo. Module Bar:Foo. End Bar. Module Baz(F:Foo). End Baz. Module Quux:=Baz(Bar).
