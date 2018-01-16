(* ::Package:: *)

MeshGenus::usage = "Compute Mesh Genus";

MeshGenus::meshRegion="Mesh regions have internal edges and stuff. If you get a odd Genus, that's why.";

MeshGenus[mesh_BoundaryMeshRegion]:=Module[{numVerts,numEdges,numfaces },
{numVerts,numEdges,numfaces}=Take[MeshCellCount[mesh],3];
1-(numVerts-numEdges+numfaces)/2
]

MeshGenus[mesh_MeshRegion]:=Module[{numVerts,numEdges,numfaces },
Message[MeshGenus::meshRegion];
{numVerts,numEdges,numfaces}=Take[MeshCellCount[mesh],3];
1-(numVerts-numEdges+numfaces)/2
]

MeshGenus[_]:=$Failed
