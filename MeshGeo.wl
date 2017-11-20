(* ::Package:: *)

polyNormal[Polygon[{p1_,p2_,p3_}]]:=Normalize@Cross[p2-p1,p3-p1]


meshToGraphicsComplex[mesh_]:=Module[{faceNormal,faceIndexes,vertexNormals},
faceNormal=polyNormal/@MeshPrimitives[mesh,2];
faceIndexes=MeshCells[mesh,2][[All,1]];
vertexNormals=Merge[MapThread[Thread[#1->#2,List,1]&,{faceIndexes,faceNormal}],Mean];
vertexNormals=Table[Lookup[vertexNormals,v,{0,0,0}],{v,MeshCells[mesh,0][[All,1]]}];
GraphicsComplex[MeshCoordinates[mesh],Polygon[faceIndexes],VertexColors->(vertexNormals+1)/2,VertexNormals->vertexNormals]
];
