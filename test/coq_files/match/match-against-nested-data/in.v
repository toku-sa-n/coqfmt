Definition aaa(x:nat):bool:=match x with O=>true|S O=>true|S (S O)=>true|_=>false end.
