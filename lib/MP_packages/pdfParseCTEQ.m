(* ::Package:: *)

(* ::Title:: *)
(*Mathematica package to parse CTEQXX.pds files*)


(* ::Text:: *)
(*Original Version:*)
(*History : 15 Nov 2012-- v .0 .1, Ben Clark, Fred Olness*)
(*09 Jan 2013-- v .0 .2, Fred Olness, Olek Kusina Pass Iset, not whole list.Add miscList so we can deal with cases other than NF = 5.*)
(*15 Apr 2013-- v .0 .3, Pavel Nadolsky, implemented the new CT10 NNLO format, additional error control features, a flag for verbose printing in pdfParseCTEQ*)
(*20 Apr 2013-- v1 .0, Ben Clark, code reworked for consistancy and versions 0.1 and 0.2 merged, addtional error correction and alpha_s information included*)
(**)
(*New set of packages:*)
(*History: 30 October 2013- v1.1 Ben Clark, reworked to produce 3 packages. One to parse PDS files (This one), One to compute errors, and One to inport LHAgrid files for CTEQ*)
(*7 November 2013 v1.2 Ben Clark, rewrite whole package to do away with ifamily variable*)
(*6 Aug 2014 v1.3 Ben Clark, divide packages to produce a single package with all the common functions between the various packages.*)
(*19 Feb 2015 v1.4 Ben Clark, added the infor file for support with the LHA package.*)
(*18 Nov 2015 DBC making pass to increase speed of packages and remove old comments and code*)


(* ::Section:: *)
(*Setup Package*)


(* ::Input:: *)
BeginPackage["pdfParseCTEQ`","pdfCalc`"];
(*Package requires pdfCalc.m for intrepolation routine. see http://ncteq.hepforge.org/code/pdf.html*)

Print["==============================================================="];
Print[" "];
Print[" - pdfParseCTEQ - "];
Print["Version:  1.0"];
Print["Authors: D.B. Clark, E.J. Godat & F.I. Olness"];
Print[" "];
Print["Please cite: **************"];
Print["http://ncteq.hepforge.org/code/pdf.html"];
Print[" "];
Print["For a list of available commands, enter: ?pdf*"];
Print[" "];
Print["==============================================================="];



(* ::Input:: *)
(***********************************************************************)
pdfParseCTEQ::usage = 
  "pdfParseCTEQ[fileName, [verbose]]: This function reads an individual .pds file specified by \!\(\*StyleBox[\" fileName \",\nFontSlant->\"Italic\"]\) into memory.
  
  The function returns a set number that corresponds to the listing of the .pds file in \!\(\*StyleBox[\" pdfSetList \",\nFontSlant->\"Italic\"]\).
  
  The optional input allows the user to supress the output of this function by choosing \!\(\*StyleBox[\" verbose \",\nFontSlant->\"Italic\"]\) to be \!\(\*StyleBox[\" False \",\nFontSlant->\"Italic\"]\).
  ";

(***********************************************************************)
pdfFamilyParseCTEQ::usage = 
  "pdfFamilyParseCTEQ[path, [fileType]]: This function reads all the files of type \!\(\*StyleBox[\" fileType \",\nFontSlant->\"Italic\"]\) \
in the directory \!\(\*StyleBox[\" path \",\nFontSlant->\"Italic\"]\) and stores them in memory.
  
  The function returns a list of set numbers that can be used to define a list. These set numbers correspond to the listing of the .pds files in \!\(\*StyleBox[\" pdfSetList \",\nFontSlant->\"Italic\"]\).
  
  The optional input \!\(\*StyleBox[\" fileType \",\nFontSlant->\"Italic\"]\) has a default value of \"*.pds\".
  
  Example:
    pdfFamilyParseCTEQ[\"MyGrids\",\"ct10*pds\"] reads all .pds files in the subdirectory \"MyGrids\" beginning with \"ct10\" into memory.
  ";

(***********************************************************************)

Begin["`Private`"]; 



(* ::Section:: *)
(*Main Parsing Functions*)


(* ::Input:: *)
(*typer: input is a PDS file, output is a value to control the behaviour of the parseing routine (ipdsformat).*)
typer[stream_] := 
Module[{chk0, chk1, ipdsformat},
SetStreamPosition[stream, 0];
Read[stream, String];
chk0 = StringTake[Read[stream, Record, RecordSeparators -> ","], -3];
Read[stream, Record];
Read[stream, String];
chk1 = StringTake[Read[stream, Record, RecordSeparators -> ","], -3];
ipdsformat = Null; (* Use this if nothing matches *)
If[chk0 == "rdr" && chk1 != "fMx",ipdsformat = 6];(*CTEQ6 .6.pds format;alpha_s is not specified*)
If[chk0 == "ipk", ipdsformat = 12];(*CT12.pds format*)
If[chk1 == "Ipk", ipdsformat = 10 ];(*Pre-CT10 NNLO format*)
If[chk0 == "rdr" && chk1 == "fMx", ipdsformat = 99];(*LHA->PDS format for internal conversion routine*)
Return[ipdsformat];
];(*End typer...*)

(*makeFlavorList: input is max number of flavors and number of valance flavors. output is list of flavors like in LHAPDF6 info file*)
makeFlavorList[nfmax_?IntegerQ(*,nfval_?IntegerQ*)]:=
Module[{(*numFlav,*)tmp0,tmp1,out},
(*numFlav = nfmax+1+nfval;*)
tmp0=Range[-nfmax,-1];
tmp1=Range[1,nfmax];
out = {tmp0,tmp1,21}//Flatten;
Return[out];
];(*End makeFlavorList...*)

(*makeInfoList: input is list of strings (nameList), and a list of values (infList). output is the info list as in LHAPDF6*)
makeInfoList[nameList_?ListQ,infList_?ListQ,fileName_?StringQ]:=
Module[{long1,long2,outList},
long1=Length[nameList];
long2=Length[infList];
If[long1!=long2,
Print["Cannot construct info list for file: "
<>fileName
<>"\nPlease contact the authors."
<>"\nAborting..."];
Abort[],
outList=Table[nameList[[i]]->infList[[i]],{i,1,long1}];
];
Return[outList];
];(*End makeInfoList...*)

(*fileTypeString: input is the integer from the typer function. output is a file type descriptor for PDS files*)
fileTypeString[typeOut_?IntegerQ]:=
Module[{out},
If[typeOut==6,
out="--CTEQ 6";
];(*cteq 6*)
If[typeOut==10,
out="--CTEQ 10";
];(*cteq 10*)
If[typeOut==12,
out="--CTEQ 12";
];(*cteq 12*)
If[typeOut==99,
out="--CTEQ internal format";
];(*LHA->PDS format*)
Return[out];
];(*End fileTypeString...*)



(* ::Input:: *)
(********************************** MAIN PARSING ROUTINE**********************************)
(*pdfParseCTEQ: input is a .pds file from cteq 6.6, 9, 10, 10nlo, 10nnlo, and 12 .pds files*)
(*output is the data in memory in an array called pdfTableData-->files added sequentialy and identified by nSetCount*)
pdfParseCTEQ[filename_?StringQ,verbose_:True]:=
Module[{stream,order,ipk,nfl,qalpha,alfaQ,lambda,m1,m2,m3,m4,m5,m6,
ipd0,ihdn,iknl,nfmx,nfval,nx,nt,ng,dum,idum,qini,qmax,xmin,xcr,
xlist,nblk,length,biglist,nx1,nt1,list1,list2,list3,xlist1,qlist,
nflav,qlist1,pdstype,tmpstring,qbase,aimass,fswitch,readlist,
alphalist0,nn,end,bigtmp0,bigtmp,biglist0,str,endpoint,nameList0,
infList0,info,duplist,outlist,antivalList,antiupdnList,glueList,
valList,updnList,endList,alphaOut,alphIn,out},
stream=OpenRead[filename];
pdstype=typer[stream];
SetStreamPosition[stream,0];
tmpstring=Read[stream,String];(*name of PDF set*)
Read[stream,String];
If[pdstype==6,
{order,nfl,lambda,m1,m2,m3,m4,m5,m6}=Read[stream,Table[Number,{i,1,9}]];
Read[stream,String];
{ipd0,ihdn,iknl,nfmx,nfval,dum,dum}=Read[stream,Table[Number,{i,1,7}]]
];(*cteq 6*)
If[pdstype==10,
{order,nfl,qbase,m1,m2,m3,m4,m5,m6}=Read[stream,Table[Number,{i,1,9}]];
Read[stream,String];
{ipk,alfaQ,qalpha,nfmx,nfval,dum,dum}=Read[stream,Table[Number,{i,1,7}]];
alphalist0={qalpha,alfaQ}
];(*cteq 10*)
If[pdstype==12,
{ipk,order,qalpha,alfaQ,m1,m2,m3,m4,m5,m6}=Read[stream,Table[Number,{i,1,10}]];
Read[stream,String];
{aimass,fswitch,ipd0,ihdn,iknl,nfmx,nfval}=Read[stream,Table[Number,{i,1,7}]]
];(*cteq 12*)
If[pdstype==99,
{order,alfaQ,m1,m2,m3,m4,m5,m6}=Read[stream,Table[Number,{i,1,8}]];
Read[stream,String];
{nfmx,nfval}=Read[stream,{Number,Number}];
ng=0(*this value is not in the header*)
];(*LHA->PDS format*)
Read[stream,String];
If[pdstype!=99,
{nx,nt,idum,ng,idum}=Read[stream,Table[Number,{i,1,5}]],
{nx,nt}=Read[stream,{Number,Number}]
];
If[ng>0, 
Read[stream,Table[Record,{i,1,ng+1}]]
];
Read[stream,Record];
{qini,qmax}=Read[stream,{Number,Number}];
If[pdstype==12||pdstype==99,(*Post-CT10 or LHA->PDS format *)
readlist=Read[stream,Table[LF[Number,Number,Number],{i,0,nt}]];
qlist=readlist/.LF[a__]:>{{a}[[1]],{a}[[2]]};
alphalist0=readlist/.LF[a__]:>{{a}[[1]],{a}[[3]]},
(*Pre-CT10 format *)
qlist=Read[stream,Table[{Number,Number},{i,0,nt}]]
];
Read[stream,String];
If[pdstype!=99,
{xmin,xcr}=Read[stream,{Number,Number}];
xlist=Read[stream,Table[Number,{i,1,nx}]],
xmin=Read[stream,Number];
xlist=Read[stream,Table[Number,{i,1,nx+1}]]
];
Read[stream,String];
nblk=(nx+1)*(nt+1);
length=nblk*(nfmx+1+nfval);
biglist=Read[stream,Table[Number,{i,1,length}]];(*read in the PDF list*)
endpoint=Read[stream];
If[endpoint!=EndOfFile,
Print["Warning--Parton Distribution does not match header information for ",tmpstring,". Results are not reliable. Please report file to the Developers"]
];(*Check to see that you are at the end of the file*)
nx1=nx+1;(*number of x elements*)
nt1=nt+1;(*number of q elements*)
nflav=(nfmx+1+nfval);(*number of flavors*)
list1=Partition[Partition[biglist,{nx1}],{nt1}];(*partition the PDF file by x then q then flavor is left over*)
If[pdstype!=99,
xlist1=Join[{0.0},xlist],
xlist1=xlist]; (*this adds a lower bound to the xlist*)
(*add factor of x to list*)
Do[ 
list2[i]=Table[xlist1*list1[[i,j]],{j,1,nt1}],
{i,1,nflav}
];
list3=Table[list2[k],{k,1,nflav}];
qlist1=qlist//Transpose//First;
Close[stream];
(*write list to memory*)
If[Length[$MessageList] == 0,
If[(verbose!=False),
Print[tmpstring]
];(*end if...*)
(*structure list to match LHA data file*)
duplist=Take[list3,nfmx-nfval];(*list of sea quarks*)
antivalList=Take[list3,{nfmx-nfval+1,nfmx}];(*list of valance sea quarks*)
antiupdnList=ReplacePart[antivalList,{-2->antivalList[[-1]],-1->antivalList[[-2]]}];(*ubar and dbar*)
glueList=Take[list3,{nfmx+1}];(*gluon*)
valList=Take[list3,-nfval];(*list of valance quarks*)
updnList=ReplacePart[valList,{1->valList[[2]],2->valList[[1]]}];(*down and up*)
endList=Reverse[duplist];(*additional quarks for list*)
outlist=Join[duplist,antiupdnList,updnList,endList,glueList];
If[pdstype==6,
alphIn={}
];
If[pdstype==10,
alphIn={qalpha,alfaQ}
];
If[pdstype==12||pdstype==99,
alphIn=Transpose[alphalist0]
];
If[alphIn == {},
alphaOut={"n/a","n/a"};
{qalpha,alfaQ}=alphaOut,(*These terms are not available in cteq6 files*)
alphaOut=alphIn;
];(*End If...*)
nameList0={
"SetDesc",
"Format",
"Flavors",
"OrderQCD",
"NumFlavors",
"ErrorType",
"ErrorConfLevel",
"XMin",
"XMax",
"QMin",
"QMax",
"MZ",
"MUp",
"MDown",
"MStrange",
"MCharm",
"MBottom",
"MTop",
"AlphaS_MZ",
"AlphaS_Qs",
"AlphaS_Vals"
};
infList0={
tmpstring,
"PDS"<>fileTypeString[pdstype],
makeFlavorList[nfmx],
order,
nfmx,
"hessian",
90,
xmin,
xlist[[nx]],
qini,
qmax,
qalpha,
m1,
m2,
m3,
m4,
m5,
m6,
alfaQ,
alphaOut[[1]],
alphaOut[[2]]
};
info=makeInfoList[nameList0,infList0,tmpstring];
out=propagateLists[filename,nfmx,nfval,info,{xlist1},{qlist1},{outlist},xmin,alphIn,1];
Return[out],
Print[filename," was not initialized: ",Length[$MessageList]," error messages"];
Unprotect[$MessageList];
Do[$MessageList = {},
 {1}
];
Protect[$MessageList];
];(*End If[Length[$MessageList]==0...*)
];(*End pdfParseCTEQ...*)
(********************************** END MAIN PARSING ROUTINE**********************************)



(* ::Input:: *)
(*pdfFamilyParseCTEQ: input is a directory containing PDS files (optional input is filetype-->default is "*.pds")*)
(*                     all PDS files are stored in memory and output is a list containing the setNumbers of the PDS files that were stored *)
pdfFamilyParseCTEQ[path_?StringQ,type_:"*.pds"]:=
Module[{currDir,fileList,output,fLength,startSet,endSet,tmpOut},
(*currDir=Directory[];*)
(*SetDirectory[path];*)
fileList=FileNames[path<>"/"<>type];
fLength=Length[fileList];
(*startSet=pdfCalc`Private`nSetCount+1;*)
Do[tmpOut[i]=pdfParseCTEQ[fileList[[i]],False],
{i,1,fLength}
];
startSet=tmpOut[1];
endSet=tmpOut[fLength];
Print["Included ",endSet-startSet+1," files in the PDF family."];
(*SetDirectory[currDir];*)
output=Range[startSet,endSet];
Return[output];
];(*pdfFamilyParseCTEQ...*)



(* ::Section:: *)
(*End Package*)


(* ::Input:: *)
End[];  (* End Private Context *)

Protect[pdfParseCTEQ,pdfFamilyParseCTEQ];

EndPackage[]; (* End Package Context *)
