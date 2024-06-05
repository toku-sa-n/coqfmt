Record R X {Y : _} := {
  rx : X;
  ry : Y;
}.

Definition x1 (r : @R unit unit) := r.(rx unit).

Definition x2 r := r.(@rx).

Definition x3 r := r.(@rx unit unit).

Definition x4 r := r.(rx unit (Y := unit)).
