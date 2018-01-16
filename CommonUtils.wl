(* ::Package:: *)

nextPower2[x_]:=2^Ceiling@Log2@x;


x/:{x_,___}.x:=x
y/:{_,y_,___}.y:=y
z/:{_,_,z_,___}.z:=z
