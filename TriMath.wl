(* ::Package:: *)

subdivide[Polygon[{a_,b_,c_}]]:=Module[{
ab=(a+b)/2,
ac=(a+c)/2,
bc=(b+c)/2},
Polygon/@{{ab,bc,ac},
{a,ab,ac},
{b,bc,ab},
{c,ac,bc}}]


maxBounds[Polygon[verts_]]:=Max[-Subtract@@@MinMax/@Transpose[Floor[verts]]]

lowerPoint[region_]:=RegionBounds[region][[All,1]];
upperPoint[region_]:=RegionBounds[region][[All,2]];
regionBoundsSize[region_]:=-Subtract@@@RegionBounds[region];
shiftRegion[region_,o_]:=TransformedRegion[region,TranslationTransform[o-lowerPoint[region]]];
shiftRegion[region_]:=shiftRegion[region,0];
niceDivide2[x_]:=If[Divisible[x,2],x/2,x]
SetAttributes[niceDivide2,Listable]
cubeSized[size_,pos_]:=Cuboid[pos,pos+size];





getPoints[Line[{low_,high_}]]:=Module[{grad,grady},
grady=high-low;
grad=Prepend[grady+0.5Sign[grady],1];
grad=1/TakeLargestBy[Select[grad,Abs[#]>$MachineEpsilon&],Abs,UpTo[Length[grady]]];
Flatten[Floor@Table[Sign[d]*p*grady+low,{d,grad},{p,0,Sign[d],d}],1]
];

integerBounds[region_]:=Module[{high,low,size},
{low,high}=Transpose@RegionBounds[region];
low=Floor[low];
high=Ceiling[high];
Transpose@{low,high}
];

getPoints[tri:Polygon[{s_,b_,c_}]]:=Module[{tria,trib,lines,dimension},
{dimension}=Ordering[regionBoundsSize[tri],-1];
{tria,trib}=splitTri[tri,dimension];
lines=Join[drawFlatTri[trib,dimension],drawFlatTri[tria,dimension],{
Line[{s,b}],
Line[{b,c}],
Line[{c,s}]
}];
points=Flatten[getPoints/@lines,1]
];


integerBoundsPow2Sized[region_]:=Module[{high,low,size},
{low,high}=Transpose@RegionBounds[region];
low=Floor[low];
size=nextPower2[high-low];
cubeSized[size,low]
];

octreePerms[size_,origin_]:=Module[{step=niceDivide2[size]},
Array[cubeSized[step, origin+step*({##}-1)]&,size/step]
]

octreeSubdivide[Cuboid[origin_,end_]]:=Module[{step=niceDivide2[(end-origin)]},
Array[cubeSized[step,origin+step*({##}-1)]&,(end-origin)/step]
]


regionPointsOctree[region_]:=Reap[regionPointsOctree[region,integerBoundsPow2Sized[region]]][[2,1]]

regionPointsOctree[tri_,cube_]:=Module[{hit,lastLevel},
hit=Not@RegionDisjoint[cube,tri];
lastLevel=Max[regionBoundsSize[cube]]<=1;
If[hit,
If[lastLevel,
Sow[lowerPoint[cube]],
Scan[regionPoints[tri,#]&,octreeSubdivide[cube],{-3}]
]
];
hit
]


drawFlatTri[Polygon[{s_,b_,c_}],dimension_]:=Module[{step,l,r},
l=b-s;
r=c-s;
step=1/Max[1,Abs@l[[dimension]]];
Flatten[Table[
Line[{(s+x *l),(s+x* r)}]
,{x,0,1,step}],1]
];


splitTri[Polygon[verts_],axis_]:=Module[{sorted,vt1,vt2,vt3,v4,l,s},
{vt1,vt2,vt3}=SortBy[verts,#[[axis]]&];
s=((vt2[[axis]]-vt1[[axis]])/(vt3[[axis]]-vt1[[axis]]));
l=s*(vt3-vt1);
v4=vt1+l;
{Polygon[{vt1,vt2,v4}],Polygon[{vt3,vt2,v4}]}
];


diceTri[p:Polygon[verts_]]:=Module[{maxSize,sizes,splitDir,splits},
sizes=-Subtract@@@MinMax/@Transpose[verts];
maxSize=Max[sizes];
If[maxSize<=1,
p,
Flatten[ diceTri/@subdivide[p]]
]
];


getTriPointsRaw[Polygon[verts_]]:=DeleteDuplicates[Round[verts]];
SetAttributes[getTriPointsRaw,Listable]
getPolyPoints[poly_]:=Flatten[getTriPointsRaw/@diceTri@poly,1]


pointTriangleIntersection[p_,Triangle[{v1_,v2_,v3_}]]:=
(*(Min[v1.x,v2.x,v3.x]\[LessEqual]p.x\[LessEqual]Max[v1.x,v2.x,v3.x])&&
(Min[v1.z,v2.z,v3.z]\[LessEqual]p.z\[LessEqual]Max[v1.z,v2.z,v3.z])&&
(Min[v1.y,v2.y,v3.y]\[LessEqual]p.y\[LessEqual]Max[v1.y,v2.y,v3.y])&&*)
Sign[Cross[v1-v2,v1-p]]==Sign[Cross[v2-v3,v2-p]]==Sign[Cross[v3-v1,v3-p]]
