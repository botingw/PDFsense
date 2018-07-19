(* ::Package:: *)

(* ::Title:: *)
(*Mathematica package to manipulate and calculate with PDF files*)


(* ::Text:: *)
(*Version History :*)
(*6 Aug 2014-- Ben Clark, Creation of package to facilitate the use of LHA files with error calculation.*)
(*22 Sept 2014-- Eric Godat, updated usage and further combined LHAall into.*)
(*14 April 2015-- Eric Godat / Ben Clark updated usage and fully integrated with LHA&CTEQ reader packages*)
(*18 Nov 1015--DBC edited code to remove unnessasary comments and speed up calculations*)
(*6 Feb 2016--DBC master version1.0 of package  *)


(* ::Section:: *)
(*Setup Package*)


(* ::Input:: *)
BeginPackage["pdfCalc`"];

Print[" "];
Print[" - Required Package: pdfCalc --Loaded - "];
Print[" "];



(* ::Subsection:: *)
(*Usage*)


(* ::Input:: *)
(***********************************************************************)
pdfFunction::usage=
"pdfFunction[setNumber, flavor, x, Q]: This function returns the interpolated value of the PDF for the 
.pds/.dat file specified by \!\(\*StyleBox[\" setNumber \",\nFontSlant->\"Italic\"]\), for the given flavor and value 
of Bjorken \!\(\*StyleBox[\" x \",\nFontSlant->\"Italic\"]\) and scale \!\(\*StyleBox[\" Q \",\nFontSlant->\"Italic\"]\).

\!\(\*StyleBox[\" Warning \",\nFontSlant->\"Italic\"]\): The results of this function are only reliable between the 
maximum and minimum values of x and Q in the .pds/.dat file.
";

(***********************************************************************)
pdfLowFunction::usage=
"pdfLowFunction[setNumber, flavor, x, Q, [power]]: This function returns the value of the PDF as 
in pdfFunction, but with an extrapolation below the minimum x value that goes as \!\(\*FractionBox[\(1\), SuperscriptBox[\(x\), \(power\)]]\).
The optional input, \!\(\*StyleBox[\" power \",\nFontSlant->\"Italic\"]\), has a default value of power = 1.0.
";

(***********************************************************************)
pdfSetList::usage=
"pdfSetList: This global variable prints the list of the loaded .pds/.dat grid files, their 
\!\(\*StyleBox[\"setNumber\",\nFontSlant->\"Italic\"]\), maximal number of quark flavors, and number of 
valence flavors in each PDF set.
";

(***********************************************************************)
pdfSetListDisplay::usage=
"pdfSetListDisplay[ ]: This function provides the user a formatted table of the list of 
data sets stored in memory.

\!\(\*StyleBox[\" Note \",\nFontSlant->\"Italic\"]\): This function does not accept any inputs.
";

(***********************************************************************)
pdfReset::usage=
"pdfReset[ ]: This function deletes all .pds/.dat files from memory and resets all the internal 
variables in the package.

\!\(\*
StyleBox[\" Note \",\nFontSlant->\"Italic\"]\): This function does not accept any inputs.
";

(***********************************************************************)
pdfFlavor::usage=
"pdfFlavor[flavor]: This function accepts an integer \!\(\*StyleBox[\" flavor \",\nFontSlant->\"Italic\"]\) 
number and returns a string with the name of the flavor in the LHA scheme. 

Example:
pdfFlavor[0], pdfFlavor[1], pdfFlavor[2] will return \"gluon\", \"down\", \"up\" for the gluon, down quark, and up quark PDFs.
";

(***********************************************************************)
pdfXmin::usage=
"pdfXmin[setNumber]: This function returns the minimum x value in the PDF set 
\!\(\*StyleBox[\"setNumber\",\nFontSlant->\"Italic\"]\).
";

(***********************************************************************)
pdfGetXlist::usage=
"pdfGetXlist[setNumber, [Qpartition]]: This function returns the grid values in x from the PDF set 
\!\(\*StyleBox[\" setNumber \",\nFontSlant->\"Italic\"]\).

This function has the ability to see the grid values for data files with multiple grids.
For this use:
pdfGetXlist[setNumber, Qpartition]
";

(***********************************************************************)
pdfGetQlist::usage=
"pdfGetQlist[setNumber, [Qpartition]]: This function returns the grid values in Q from the 
PDF set \!\(\*StyleBox[\" setNumber \",\nFontSlant->\"Italic\"]\).

This function has the ability to see the grid values for data files with multiple grids.
For this use:
pdfGetQlist[setNumber, Qpartition]
";

(***********************************************************************)
pdfAlphaS::usage=
"pdfAlphaS[setNumber, Q]:This function returns the value of \!\(\*SubscriptBox[\(\[Alpha]\), \(S\)]\) 
at hard scattering energy \!\(\*StyleBox[\" Q \",\nFontSlant->\"Italic\"]\) when this information is available 
in the .pds or .info file. 

\!\(\*StyleBox[\"Warning\",\nFontSlant->\"Italic\"]\): This function will print a text message  and return 
a Null value if the \!\(\*SubscriptBox[\(\[Alpha]\), \(S\)]\) information is not available.
";

(***********************************************************************)
pdfNumQpartition::usage=
"pdfNumQpartition[setNumber]: This function returns the number of Q grids in the 
PDF set \!\(\*StyleBox[\" setNumber \",\nFontSlant->\"Italic\"]\).
";

(***********************************************************************)
pdfGetInfo::usage=
"pdfGetInfo[setNumber]: This function returns the information corresponding to set 
\!\(\*StyleBox[\" setNumber \",\nFontSlant->\"Italic\"]\) read from the .info file or generated from the header of a 
.pds file.

pdfGetInfo[setNumber, value]: This function accepts a string and returns the info corresponding to set 
\!\(\*StyleBox[\" setNumber \",\nFontSlant->\"Italic\"]\) read from the .info file or generated from the header of a 
.pds file for a specific \!\(\*StyleBox[\" value \",\nFontSlant->\"Italic\"]\).

Example:
pdfGetInfo[setNumber, \"Flavors\"] will return the quark flavor scheme for the info file if that information is available.

\!\(\*StyleBox[\"Note\",\nFontSlant->\"Italic\"]\): If the user is unaware of what is present in the info file, 
\!\(\*StyleBox[\"pdfGetInfo[setNumber]\",\nFontSlant->\"Italic\"]\) may still be used and displays the all values in the 
info file.
";

(***********************************************************************)
pdfLuminosity::usage=
"pdfLuminosity[setNumber,sqrtS,mX,flavor1,flavor2,[precisionGoal]]: This function returns the integrated parton-parton luminosity for collider energy \!\(\*StyleBox[\" sqrtS \",\nFontSlant->\"Italic\"]\) = \!\(\*SuperscriptBox[\(S\), \(1/2\)]\), particle mass \!\(\*StyleBox[\" mX \",\nFontSlant->\"Italic\"]\), and PDF flavors \!\(\*StyleBox[\" flavor1 \",\nFontSlant->\"Italic\"]\) and \!\(\*StyleBox[\" flavor2 \",\nFontSlant->\"Italic\"]\), for the set \!\(\*StyleBox[\" setNumber \",\nFontSlant->\"Italic\"]\).

The numerical integral is performed with the precision goal in the optional parameter \!\(\*StyleBox[\" precisionGoal \",\nFontSlant->\"Italic\"]\), which has a default value of \!\(\*StyleBox[\" precisionGoal \",\nFontSlant->\"Italic\"]\) = 3.

The parton luminosity is defined according to Eq.(46) in Campbell, Huston, Stirling, arXiv:hep-ph/0611148v1
";

(***********************************************************************)
propagateLists::usage=
"internal function for adding data from parsers to memory.";

(***********************************************************************)
pdfSetInterpolator::usage=
"pdfSetInterpolator[[key]]: This function selects the interpolation routine to use for pdfFunction.

Available functions include: \"MMA\", the default interpolation routine from Mathematica or 
\"ManeParse\", a custom cubic Lagrange interpolation routine.

The x-power for the ManeParse interpolation can be set with pdfSetXpower.

\!\(\*StyleBox[\"Note\",\nFontSlant->\"Italic\"]\): The input is optional for this function. No input will reset the default Mathematica interpolator."

(***********************************************************************)
pdfSetXpower::usage="pdfSetXpower[[power]]: This function sets the x-power to be used with the ManeParse interpolation 
routine.

The defult value of \!\(\*StyleBox[\" power \",\nFontSlant->\"Italic\"]\) = 1 will interpolate in \*SuperscriptBox[x,1]*pdf(x,Q).

\!\(\*StyleBox[\"Note\",\nFontSlant->\"Italic\"]\): The input is optional for this function. No input will reset the default value."


Begin["`Private`"];



(* ::Section:: *)
(*initialization*)


(* ::Input:: *)
nSetCount=0;
pdfSetList={};
Clear[lookup,pdfTableData,alphalist,pdfxmin,infoTable,alphaSFitParameter,interpol,xPower];
alphalist[_]=False;
pdfTableData[_]=False;
pdfxmin[_]=False;
infoTable[_]=False;
numQpart[_]=False;
interpol[x_,data_]:=Interpolation[data,x];(*Default interpolator*)
xPower=1;


(*propagateLists: Function to populate the lists in this package. This function will be called in the parse packages.*)
(*input is lists to add to dataTable etc. output is set number????*)
propagateLists[fname_,nfmax_,nfval_,info_,xList_,qList_,bigList_,xmin_,alphIn_,nQpart_:1]:=
Module[{},
nSetCount=nSetCount+1;
numQpart[nSetCount]=nQpart;
pdfxmin[nSetCount]=xmin;
pdfTableData[nSetCount]={xList,qList,bigList};
alphalist[nSetCount]=alphIn;
AppendTo[pdfSetList,{nSetCount,fname,nfmax,nfval}];
infoTable[nSetCount]=info;
ipartMod[nSetCount];
Return[nSetCount];
];(*End propagateLists...*)

(*ipartMod: Function to populate a list of partons in the order they apear in the grids.*)
(*input: set number output: list added to lookup global list, lookup.*)
ipartMod[iset_]:=
Module[{plabel,range},
plabel="Flavors"/.infoTable[iset];
range=Range[Length[plabel]];
Do[lookup[iset][plabel[[iparton]]]=range[[iparton]],
{iparton,1,Length[plabel]}
];(*End Do...*)
(*Deal with gluon:iparton=0 or 21*)
If[!FreeQ[plabel,21],
lookup[iset][0]=Position[plabel,21][[1,1]]
];(*End If...*)
If[!FreeQ[plabel,0],
lookup[iset][21]=Position[plabel,0][[1,1]]
];(*End If...*)
];(*End ipartMod...*)

(*partPos: input setNumber and partonNumber*)
(*output: position in list which contains that parton*)
partPos[iset_,iflav_]:=lookup[iset][iflav];



(* ::Section:: *)
(*user control functions*)


(* ::Input:: *)
(*pdfReset: no input, output is to reset memory and all global variables in the package*)
pdfReset[]:=
Module[{},
nSetCount=0;
pdfSetList={};
Clear[lookup,pdfxmin,pdfTableData,alphalist,infoTable,alphaSFitParameter,interpol,xPower];
alphalist[_]=False;
pdfxmin[_]=False;
pdfTableData[_]=False;
infoTable[_]=False;
numQpart[_]=False;
alphaSFitParameter[_]=False;
pdfSetInterpolator["MMA"];
xPower=1;

(* Fred: 28 Mar 2016: reset for updated alphas *)
Clear[pdfAlphaS,aSetCreated];
pdfAlphaS[iSet_,q_]:= pdfAlphaStmp[iSet,q];

Print["All internal variables have been reset."];
  ];(*End pdfReset...*)

(*pdfSetListDisplay: prints pdfSetList in a user readable format.*)
pdfSetListDisplay[]:=
Module[{titles,tmpList,out},
titles={"Set Number","File Name","Max Flavors","Valance Flavors"};
tmpList=pdfSetList;
Return[PrependTo[tmpList,titles]//Grid[#,Frame->All]&];
];(*End pdfSetListDisplay...*)

(*pdfSetInterpolator: sets the Interpolator function to be used by the package.*)
pdfSetInterpolator[key_:"MMA"]:=
Module[{},
If[!StringQ[key],
Print["Error: Input to pdfSetInterpolator must be a string."];
Return[Null];
];(*End pdfSetInterpolator*)
If[key=="MMA",
Clear[interpol];
interpol[x_,data_]:=Interpolation[data,x];
Print["Default Mathematica interpolator will be used."];
Return[Null];
];(*End If...*)
If[key=="ManeParse",
Clear[interpol];
interpol[x_,data_]:=interpol4[x,data];
Print["ManeParse cubic interpolation will be used."];
Print["The x-power of the interpolation is set to " ,xPower];
Return[Null],
Print["Error: Interpolator type not recognized."];
Return[Null]
];(*End If...*)
];(*End pdfSetInterpolator...*)

(*pdfSetXpower: Sets the x-power for the interpol4 function in the package.*)
pdfSetXpower[pow_:1]:=Module[{} ,
If[!NumericQ[pow],
Print["Error: Input to pdfSetXpower must be numeric."];
Return[Null],
xPower=pow;
(*Print["The x-power for the ManeParse interpolation is set to " , xPower];*)
pdfSetInterpolator["ManeParse"];
Return[Null];
];(*End If...*)
];(*End pdfSetXpower...*)



(* ::Section:: *)
(*The core functions: *)


(* ::Subsection:: *)
(*Utility functions :*)


(* ::Input:: *)
(* Same as built-in GreaterEqual, but make this Listable *)
bigger[xIn_,xintp_]=GreaterEqual[xIn,xintp];
SetAttributes[bigger,Listable];



(* ::Subsection:: *)
(*Indexing functions*)


(* ::Input:: *)
(*findSubList: internal funciton to return the index for multiple q-grids*)
(*input: set number and q, output: index of appropirate q-grid*)
findSubList[iset_?IntegerQ,q_?NumericQ]:=
Module[{minList,tmpList,pos},
Off[Last::nolast];
minList=Min /@ qList;
tmpList=bigger[q,#]& /@ minList;
pos=Position[tmpList,True] //Last //Last;
pos=Max[1,pos];(* Note; this order is important *)
pos=Min[Length[minList],pos];
On[Last::nolast];
Return[pos]
];(*End findSubList...*)



(* ::Input:: *)
(*findIndex: internal function to ind the index of the position just above x in a list.*)
(*input: x-value and a list of x-vlaues, output: the index, i, in list where list[[i]]\[GreaterEqual] x*)
findIndex[x_,list_]:=
Module[{tmp1,index,len},
Off[First::first]; 
len=Length[list];
tmp1=bigger[#,x]& /@ list;
 index=Position[tmp1,True]//First//First;
index=Min[len-1,index]; (* Note; this order is important for x>xMax *)
index=Max[3,index];(* Note; this order is important for x>xMax *)
On[First::first]; 
Return[index]
];(*End findIndex...*)



(* ::Subsection:: *)
(*PDF function tree:*)


(* ::Input:: *)
(*pdfFunction: user version of the pdfX function definition*)
pdfFunction[iset_?IntegerQ,iflav_?IntegerQ,xIn_?NumericQ,q_?NumericQ] :=
 Module[{x,xMin=1.0*10^-30},
If[pdfTableData[iset]==False,Return[Null];];(*End If...*)
If[!IntegerQ[partPos[iset,iflav]],Return[0.0];];(*End If...*)

x=Max[xMin,xIn]; (* Impose xMin to avoid divide by zero *)
Return[pdfX[iset,iflav,x,q]];
]; (* /;x<1;*) (*End pdfFunction...*)

pdfFunction[iset_?IntegerQ,iflav_?IntegerQ,x_?NumericQ,q_?NumericQ]:=
Return[0.]/;x>=1;(*End pdfFunction...*)
(*force PDF to zero for x>1*)



(* ::Input:: *)
(*pdfX:returns PDF for set, flavor, x, and Q*)
pdfX[iset_,iflav_,x_,q_]:=
Module[{output},
xList=pdfTableData[iset][[1]];(* viewable in private context *)
qList=pdfTableData[iset][[2]];(* viewable in private context *)
output=fullinterp[iset,iflav,x,q]/x; (* Return PDF, not x*PDF *)
Return[output];
];(*End pdfX*)



(* ::Input:: *)
(*fullinterp:does the interpolation over all the points in 4 directions outputs the value at that point*)
fullinterp[iset_,iflav_,x_,q_]:=
Module[{numQ,qPart,xi,qi,qData,ifColumn},
numQ=numQpart[iset];
If[
numQ==1,
qPart=1,
qPart=findSubList[iset,q];
]; (* Find the qPart sub-list to work with *)
ifColumn=partPos[iset,iflav]; (* Flavor Column *)
xi=findIndex[x,xList[[qPart]]];
qi=findIndex[q,qList[[qPart]]];
qData=doQinterp[iset,ifColumn,{x,xi},{q,qi,qPart}];
Return[interpol[q,qData]];
];(*End fullinterp...*)



(* ::Input:: *)
(*doQinterp: does the full interpolation over the 4 Q points starting from xi and qi for the flavor and grids *)
doQinterp[iset_,ifColumn_,{x_,xi_},{q_,qi_,qPart_}]:=
Module[{pVec,qVec},
pVec={
interpol[x,getXdata[iset,ifColumn,xi,qi-2,qPart]],
interpol[x,getXdata[iset,ifColumn,xi,qi-1,qPart]],
interpol[x,getXdata[iset,ifColumn,xi,qi,qPart]],
interpol[x,getXdata[iset,ifColumn,xi,qi+1,qPart]]
};(*End pVec...*)
qVec=qList[[qPart]][[qi-2;;qi+1]];
Return[{qVec,pVec}//Transpose];
];(*End doQinterp...*)



(* ::Input:: *)
(*getXdata && getQdata: construct the data lists for the interpolation for the appropriate points*)
getXdata[iset_,ifColumn_,xi_,qi_,qPart_]:=
Module[{output},
output=Transpose[
{
xList[[qPart]][[xi-2;;xi+1]],
pdfTableData[iset][[3,qPart]][[ifColumn,qi,xi-2;;xi+1]]
}
];
Return[output];
];(*End getXdata...*)



(* ::Subsection:: *)
(*This is a 4 - point (cubic) interpolation Function.*)


(* ::Input:: *)
(*interpol4: a four-point Lagrange interpolation used throughout this package-->input is a point and a list (data) of the 4 points in x *)
(*           and Q surrounding the point,output is the value interpolated at the input point *)
interpol4[x_,data_]:=
Module[{xVec,qVec,x0,x1,x2,x3,q0,q1,q2,q3,c0,c1,c2,c3,output},
{xVec,qVec}=Transpose[data];
{x0,x1,x2,x3}=xVec;
{q0,q1,q2,q3}=qVec*xVec^xPower;
c0=((x-x1)/(x0-x1))*((x-x2)/(x0-x2))*((x-x3)/(x0-x3));
c1=((x-x0)/(x1-x0))*((x-x2)/(x1-x2))*((x-x3)/(x1-x3));
c2=((x-x0)/(x2-x0))*((x-x1)/(x2-x1))*((x-x3)/(x2-x3));
c3=((x-x0)/(x3-x0))*((x-x1)/(x3-x1))*((x-x2)/(x3-x2));
output=(q0*c0+q1*c1+q2*c2+q3*c3)/x^xPower;
Return[output];
];(*End interpol4...*)



(* ::Section:: *)
(*Other Functions*)


(* ::Subsection:: *)
(*other user functions*)


(* ::Input:: *)
(*pdfLowFunction: user version of pdfX but with  an extrapolation below xmin that goes as x^-power*)
pdfLowFunction[iset_,ipart_,xIn_,q_,power_:1] :=
 Module[{x,xMin,xLim=1.0*10^-30},
If[
pdfTableData[iset]==False,
Return[Null];
];(*End If...*)  
xMin=pdfxmin[iset];
x=Max[xLim,xIn];
output=If[x>xMin,
Return[pdfX[iset,ipart,x,q]],
Return[pdfX[iset,ipart,xMin,q]*(xMin/x)^power]
];(*End If...*)
]/;xIn<1;(*End pdfLowFunction...*)

pdfLowFunction[iset_,ipart_,xIn_,q_,power_:1] :=
Return[0.]/;xIn>=1;(*End pdfLowFunction...*)(*force PDF to zero for x>1*)

(*pdfLuminosity: input is a set number, collider energy, scale, flavor number 1, flaver number 2, and precision goal*)
(*output is the integrated parton-parton luminosity*)
pdfLuminosity[iset_?IntegerQ,sqrts_?NumericQ,mX_?NumericQ,ipdf1_?IntegerQ,ipdf2_?IntegerQ,myPrecisionGoal_: 3]:=
Module[{s,\[Tau],output,x1,factor},
s=sqrts^2;
\[Tau]=mX^2/s;
factor=1;
If[ipdf1==ipdf2,factor=2];
output=1/s 1/factor * NIntegrate[(1/x1)*( pdfFunction[iset,ipdf1,x1,mX]*pdfFunction[iset,ipdf2,\[Tau]/x1,mX]+pdfFunction[iset,ipdf2,x1,mX]*pdfFunction[iset,ipdf1,\[Tau]/x1,mX]),
{x1,\[Tau],1},
PrecisionGoal->myPrecisionGoal
];(*End NIntegrate...*)
Return[output];
];(*End pdfLuminosity...*)



(* ::Section:: *)
(*Alpha_s implementation*)


(* ::Input:: *)
{
 {(*pdfAlphaS  Fred's new implementation *)},
 {
  pdfAlphaS[iSet_?IntegerQ,q_?NumericQ]:= pdfAlphaStmp[iSet,q];
  
  pdfAlphaStmp[iSet_?IntegerQ,q_?NumericQ]:=Module[{},
  (* If it  has not been created yet, this will do so and  return the value  *)
  pdfCreateAlphaS[iSet];
  Return[pdfAlphaS[iSet,q]];
  ];
  
  
  pdfCreateAlphaS[iSet_?IntegerQ]:=Module[{aData,qlist,alist},
  If[aSetCreated[iSet]==True,(*Print["we have already created aS fun for set= ",iSet]; *) Return[]];
  aSetCreated[iSet]=True;  (* we  only need to create once *)
  
  (* NOTE: aSinterTMP and aSmassGrid are GLOBAL VARIABLES  *)
  aDataTMP={"AlphaS_Qs","AlphaS_Vals"}/.pdfGetInfo[iSet];
  If[!(ListQ /@aDataTMP)=={True,True},
    pdfAlphaS[iSet,q_?NumericQ]=Null[];Return[]];  (*For PDFs without lists *)
  
  aData=aDataTMP //Transpose;
  {qlist,alist}=Transpose[aData];
  
  If[Length[aData]<=3,pdfAlphaS[iSet,q_?NumericQ]=Null[];Return[]];  (*For PDFs without lists *)
  
  If[DuplicateFreeQ[qlist],
  pdfCreateAlphaSoneGrid[iSet,aData],
  pdfCreateAlphaSmultiGrid[iSet,aData]];
  Return[];
  ];
  
  (*pdfCreateAlphaSoneGrid[iSet_,aData__]:=Module[{qlist,alist,sel,dupQs,pos,pos2,subList,mass},
  (* THIS IS GLOBAL *)
  pdfAlphaS[iSet,q_]:= Interpolation[aData][q];
  
  Print["Created pdfAlphaS for iSet = ",iSet];
  Print[iSet," has  1 sub-grid "];
  Return[iSet];
  ];
  *)
  
  pdfCreateAlphaSoneGrid[iSet_,aData__]:=Module[{qlist,alist,sel,dupQs,pos,pos2,subList,mass,logData},
  (* THIS IS GLOBAL *)
  
  (*  ERIC ORIG
  Data = Transpose[aData];
  Data = {Data[[1]], E^(1./Data[[2]])}; (*Modified for interpolation smoothness, EJG*)
  Data = Transpose[Data];
  pdfAlphaS[iSet,q_]:= 1./Log[Interpolation[Data][q]];
  *)
  
  {qlist,alist} = Transpose[aData];
  logData = Transpose[{qlist, E^(1./alist)}]; (*Modified for interpolation smoothness, EJG*)
  
  pdfAlphaS[iSet,q_]:= 1./Log[Interpolation[logData][q]];
  
  Print["Created pdfAlphaS for iSet = ",iSet];
  Print[iSet," has  1 sub-grid "];
  Return[iSet];
  ];
  
  
  pdfCreateAlphaSmultiGrid[iSet_,aData__]:=Module[{qlist,alist,sel,dupQs,pos,pos2,subList,mass},
  {qlist,alist}=Transpose[aData];
  sel= Select[Tally[qlist],(#[[2]]>= 2)&];
  dupQs=sel //Transpose//First;
  pos= Position[qlist,#]& /@ dupQs;
  pos2=Join[{1},pos,{Length[qlist]}] //Flatten //Partition[#,2]&;
  subList=Take[aData,#]& /@ pos2;
  (* THIS IS GLOBAL *)
  aSinterTMP[iSet]=Interpolation /@ subList; (* THIS IS GLOBAL *)
  mass=sel //Transpose //First //Round[#,0.01]&;
  aSmassGrid[iSet]=Join[{0},mass]; (* THIS IS GLOBAL *)
  aSfindQindex[iSet,q_]:=GreaterEqual[q,#]& /@ aSmassGrid[iSet]//Position[#,True]& //Last //Last;(* THIS IS GLOBAL *)
  pdfAlphaS[iSet,q_]:= aSinterTMP[iSet][[ aSfindQindex[iSet,q] ]][q];(* THIS IS GLOBAL *)
  
  Print["Created pdfAlphaS for iSet = ",iSet];
  Print["PDF Set = ",iSet," has  ",Length[aSmassGrid[iSet]]," sub-grids "];
  
  Return[iSet];
  ];}
}


(* ::Section:: *)
(*pdfGet Functions:*)


(* ::Subsection:: *)
(*user functions*)


(* ::Input:: *)
(*pdfGetXlist:input is set number,output is x grid from the associated PDF*)
pdfGetXlist[iset_?IntegerQ]:=
Module[{},
Return[pdfTableData[iset][[1]]];
];(*End pdfGetXlist...*)

(*pdfGetQlist:input is set number,output is Q grid from the associated PDF*)
pdfGetQlist[iset_?IntegerQ]:=
Module[{},
Return[pdfTableData[iset][[2]]];
];(*End pdfGetQlist...*)

(*pdfXmin:input is set number,ouput is the minimum value in the x grid in the assosiated PDF*)
pdfXmin[iset_?IntegerQ]:=
Module[{},
If[
pdfxmin[iset]==False,
Return[Null]
];(*End If...*)
Return[pdfxmin[iset]]
];(*End pdfXmin...*)

(*Function that finds the number of q grids in a given data set*)
pdfNumQpartition[iset_]:=numQpart[iset];(*End pdfNumQpartition...*)

(*pdfGetInfo: input: set number and optional info key*)
(*output: list of rules from PDF info file*)
pdfGetInfo[iset_, query_: "All"] :=
 Module[{},
If[infoTable[iset]==False,
Print["Info not available for this data set"];
Return[]
];(*End If...*)
If[query == "All",
Return[infoTable[iset] (*// TableForm*)];
];(*End If...*)
Return[query/.infoTable[iset]];
];(*End pdfGetInfo...*)

(*pdfFlavor:  User available function-->input is parton number in LHA scheme, output is string that is name of parton*)
pdfFlavor[i_:"Key"]:=
Module[{long,j,tmpOut,out,pdfFlavorTMP,tmpOut0},
pdfFlavorTMP={
"tbar",(*-6*)
"bbar",(*-5*)
"cbar",(*-4*)
"sbar",(*-3*)
"ubar",(*-2*)
"dbar",(*-1*)
"gluon",(*0 0r 21*)
"down",(*1*)
"up",(*2*)
"strange",(*3*)
"charm",(*4*)
"bottom",(*5*)
"top"(*6*)
};(*End pdfFlavorTMP...*)
long=Length[pdfFlavorTMP];
If[IntegerQ[i],
If[ Abs[i]>6 && i!=21,
Return[Null],(*For Error control.*)
If[i!=21,
Return[pdfFlavorTMP[[i+7]]],
Return[pdfFlavorTMP[[7]]](*location of gluon*)
];(*End If...*)
];(*End If...*)
];(*End If...*)
If[ StringQ[i] && i!="Key",
Return[Null]
];(*End If...*)
If[ i == "Key",
Do[
tmpOut0[j]=If[j==7,
{"0 or 21",pdfFlavorTMP[[j]]},
{ToString[j-7],pdfFlavorTMP[[j]]}
],(*End If...*)
{j,1,long}
];(*End Do...*)
tmpOut=Table[tmpOut0[k],{k,1,long}];
out=PrependTo[tmpOut,{"Flavor","Name"}]//Transpose//Grid[#,Frame->All]&;
Return[out];
];(*End If...*)
];(*End pdfFlavor...*)



(* ::Section:: *)
(*End Package*)


(* ::Input:: *)
End[];  (* End Private Context *)

Protect[
	pdfFunction,
	pdfLowFunction,
	pdfReset,
	pdfFlavor,
	pdfXmin,
	pdfGetXlist,
	pdfGetQlist,
(*	pdfAlphaS,  for speed, need to be re-defined as we read in pdf sets *)
	pdfSetXpower,
	pdfSetListDisplay,
	pdfSetInterpolator,
	pdfLuminosity,
	pdfNumQpartition,
	pdfGetInfo
];(*Protect all symbols...*)

EndPackage[]; (* End Package Context *)

