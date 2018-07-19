(* ::Package:: *)

(* ::Input:: *)
(**)


(* ::Title:: *)
(*Mathematica package to parse LHA files (Types: CT10, CT10nlo, NNPDF, MSTW,HERA)*)


(* ::Text:: *)
(*Original Version: 29 May 2014*)
(*History : Revised from pdfParseLHA.nb*)
(*DBC 7 Aug 2014--revised to work with new pdfCalc pacakage and added error calculation functionality.*)
(*EJG 6 Feb 2015--updated to new faster data reader*)


(* ::Section:: *)
(*Setup Package*)


(* ::Input:: *)
BeginPackage["pdfParseLHA`","pdfCalc`"];
(*Package requires pdfCalc.m for intrepolation routine. see http://ncteq.hepforge.org/code/pdf.html*)

Print["==============================================================="];
Print[" "];
Print[" - pdfParseLHA - "];
Print["Version:  1.0"];
Print["Authors: E.J. Godat, D.B. Clark & F.I. Olness"];
Print[" "];
Print["Please cite: **************"];
Print["http://ncteq.hepforge.org/code/pdf.html"];
Print[" "];
Print["For a list of available commands, enter: ?pdf*"];
Print[" "];
Print["==============================================================="];



(* ::Input:: *)
(***********************************************************************)
pdfParseLHA::usage="pdfParseLHA[fileNameInfo, fileNameData, [verbose]]: This function reads an individual .info file and .data file specified by \!\(\*StyleBox[\" fileNameInfo \",\nFontSlant->\"Italic\"]\) and \!\(\*StyleBox[\" fileNameData \",\nFontSlant->\"Italic\"]\), respectively, into memory.

The function returns a set number that corresponds to the listing of the .dat file in \!\(\*StyleBox[\" pdfSetList \",\nFontSlant->\"Italic\"]\).

Additionally, the function checks that the number and the order of the flavors are the same in both files.

The optional input allows the user to supress the output of this function by choosing \!\(\*StyleBox[\" verbose \",\nFontSlant->\"Italic\"]\) to be \!\(\*StyleBox[\" False \",\nFontSlant->\"Italic\"]\).
";

(***********************************************************************)
pdfFamilyParseLHA::usage = "pdfFamilyParseLHA[path, [fileType]]: This function reads all the files of type \!\(\*StyleBox[\" fileType \",\nFontSlant->\"Italic\"]\) in the directory \!\(\*StyleBox[\" path \",\nFontSlant->\"Italic\"]\) and stores them in memory.
  
  The function returns a list of set numbers that can be used to define a list. These set numbers correspond to the listing of the .dat files in \!\(\*StyleBox[\" pdfSetList \",\nFontSlant->\"Italic\"]\).
  
  The optional input \!\(\*StyleBox[\" fileType \",\nFontSlant->\"Italic\"]\) has a default value of \"*.dat\".
  
  Example:
    pdfFamilyParseLHA[\"MyGrids\",\"ct10*.dat\"] reads all .dat files in the subdirectory \"MyGrids\" beginning with \"ct10\" into memory.
  ";

(***********************************************************************)

Begin["`Private`"]; 



(* ::Section:: *)
(*Parsing Functions and Subfunctions*)


(* ::Subsection:: *)
(*read info file*)


(* ::Input:: *)
(*The following read function is due to FIO -- added 11 Aug 2014*)
(* This replaces line breaks within a pair of quotes *)
doLineBreaks[string_]:=
Module[{newLines,pairs,repList,string2,quotes,quotesSingle,quotesDouble},
newLines=StringPosition[string,"\n"];
quotesSingle=StringPosition[string,"'"]; 
quotesDouble=StringPosition[string,"\""];
quotes=Join[quotesSingle,quotesDouble];
If[quotes //Length //OddQ,
Write["Error: Odd number of quotes"];
Break[]
];(*End If...*)
pairs=(quotes //Length)/2; 
repList=Table[Select[newLines,((#[[1]]>quotes[[1+2 i]][[1]])&&(#[[1]]< quotes[[2+2i]][[1]]))&],
{i,0,pairs-1}
]//Flatten[#,1]&;
string2=StringReplacePart[string," ",repList];
Return[string2];
];(*End doLineBreaks...*)



(* ::Input:: *)
(* This converts a string into a number *)
doNumber[value_]:=
Module[{tmp,stream},
stream=StringToStream[value];
tmp=Read[stream,Number];
Return[tmp];
];(*End doNumber...*)



(* ::Input:: *)
(* This converts the [...] list of numbers into a Mathematica list of numbers *)
doAlpha[value_]:=
Module[{tmp},
tmp=StringReplace[value,{"["->"{","]"->"}"}];
tmp=StringReplace[tmp,{"e+"->"*10^","e-"->"*10^-","E+"->"*10^","E-"->"*10^-"}];
tmp=ToExpression[tmp];
Return[tmp];
];(*End doAlpha...*)



(* ::Input:: *)
(* This converts the [...] list into a Mathematica list *)
doFlavor[value_]:=
Module[{tmp},
tmp=StringReplace[value,{"["->"{","]"->"}"}];
tmp=ToExpression[tmp];
Return[tmp];
];(*End doFlavor...*)



(* ::Input:: *)
(* This governs the conversions needed to read *)
doConvert[tmp5_]:=
Module[{len,array,data,key,value,tmp6,numerical},
numerical={
"SetIndex",
"DataVersion",
"NumMembers",
"Particle",
"OrderQCD",
"NumFlavors",
"XMin",
"XMax",
"QMin",
"QMax",
"MZ"
};
len=Length[tmp5];
array={};
Do[
data=tmp5[[i]];
key=data[[1]];
value=data[[2]];
If[MemberQ[numerical,key],
value=doNumber[value]
];(*End If...*)
If[MemberQ[{"Flavors"},key],
value=doFlavor[value]
];(*End If...*)
If[MemberQ[{"AlphaS_Vals","AlphaS_Qs"},key],
value=doAlpha[value]
];(*End If...*)
tmp6={key,value};
array=Append[array,tmp6],
{i,1,len}
];(*End Do...*)
Return[array];
];(*End doConvert...*)



(* ::Input:: *)
(* This reads info files *)
readInfo[fileName_,verbose_:True]:=
Module[{stream,tmp,tmp2,tmp3,tmp4,tmp5,rules,infotable0,alphainfo,flavorlist},
stream=OpenRead[fileName];
SetStreamPosition[stream,0];
tmp= Read[stream,Record,RecordSeparators->{}];
tmp2= doLineBreaks[tmp];
tmp3= StringSplit[tmp2,"\n"];
tmp4= StringSplit[#,":"]&/@ tmp3;
tmp5= doConvert[tmp4];
rules=(StringTrim[#[[1]]]->#[[2]])&/@ tmp5;
If[verbose==True,
Print["Successfully read ",fileName,"."];
];(*End If...*)
infotable0=rules;
If[AtomQ["AlphaS_Qs"/.rules ]||AtomQ[ "AlphaS_Vals"/.rules ],
alphainfo={"n/a","n/a"},
(* alphainfo={DeleteDuplicates["AlphaS_Qs"/.rules],DeleteDuplicates["AlphaS_Vals"/.rules]};(*For MSTW grids with duplicate values at the quark masses*)*)
alphainfo={ "AlphaS_Qs"/.rules,"AlphaS_Vals"/.rules};(*DO NOT DELETE DUPS: FIO  28 MARCH 2016 *)

];(*End If...*)
flavorlist="Flavors"/.rules;
Return[{infotable0,alphainfo,flavorlist}];
];(*End readInfo...*)



(* ::Subsection:: *)
(*read data file*)


(* ::Input:: *)
(* This reads data files *)
(*input: .dat file and two lists from the output of readInfo*)
(*output: {flavorlist,iset number}*)
readData[fileName_,infotable0_,alphainfo_, verbose_: True] :=
Module[{record, i, stream, lengthlist, xlen, qlen, flen, product, list, 
listcut, file, tmp, listtmp, xlist, qlist, flavorcheck, bigTable, 
bigListCut1, bigListCut2, bigTableCutDivide, bigTableCutTrans, 
numQpartition, bigX, bigQ, bigList, bigFlav, nummaxflav, numflavval,out,xmin},
If[! StringQ[fileName],
Print["Error, File Name was not in the form of a String"]; 
Return[];
];(*End If...*)
If[! FileExistsQ[fileName],
Print["Error, file does not exist"];
Return[];
];(*End If...*)
If[! StringMatchQ[fileName, "*.dat"],
Print["Error, not .dat"];
Return[];
];(*End If...*)
file = OpenRead[fileName];
SetStreamPosition[file, 0];
tmp = Read[file, Record, RecordSeparators -> {}];
record = StringSplit[tmp, "---"];
numQpartition = Length[record] - 2;
If[StringTake[tmp, -1] != "\n",
(*Print["File missing <newline> at end of file"];*)
numQpartition = Length[record] - 1;
];(*End If...*)
If[numQpartition< 1,
Print["Error: file, ", fileName," not in LHA format"];
Return[];
];(*End If...*)
bigX = {};
bigQ = {};
bigList = {};
bigFlav = {};
list = {};(*Reads in the x, q, and flavor lists as strings to determine length*)
For[i = 1, 
i <= numQpartition, 
i++,
stream = StringToStream[record[[i + 1]]];
lengthlist = ReadList[stream, Record, 3];
xlen = Length[StringSplit[lengthlist[[1]]]];
qlen = Length[StringSplit[lengthlist[[2]]]];
flen = Length[StringSplit[lengthlist[[3]]]];
product = xlen*qlen*flen;
SetStreamPosition[stream, 0];
listtmp = ReadList[stream, Number];
xlist = Take[listtmp, xlen];
qlist = Take[listtmp, {xlen + 1, xlen + qlen}];
flavorcheck = Take[listtmp, {-(flen + product), -(product + 1)}];
bigTable = Take[listtmp, -product];
AppendTo[bigX, xlist];
AppendTo[bigQ, qlist];
AppendTo[bigFlav, flavorcheck];
AppendTo[bigList, bigTable];
bigListCut1 = Partition[bigList[[i]], {flen}];
bigListCut2 = Partition[bigListCut1, {qlen}];
bigTableCutDivide = bigListCut2;
bigTableCutTrans = Transpose[bigTableCutDivide, {3, 2, 1}];(*This reorganizes the indicies into (flavor,q,x)*)
AppendTo[list, bigTableCutTrans];
nummaxflav = ToExpression["NumFlavors" /. infotable0];
numflavval = If[IntegerQ[ToExpression["NumValence" /. infotable0]] == False,
"n/a",
ToExpression["NumValence" /.infotable0]
];(*End If...*)
];(*End For...*)
xmin=Min[bigX];
out=propagateLists[fileName,nummaxflav,numflavval,infotable0,bigX,bigQ,list,xmin,alphainfo,numQpartition];
If[verbose == True,
Print["Successfully read ", fileName,"."];
];(*End If...*)
Return[{bigFlav[[1]],out}];
Unprotect[$MessageList];
Do[$MessageList = {},
 {1}
];
Protect[$MessageList];(*This will reset the $MessageList for the next file*)
];(*End readData...*)



(* ::Section:: *)
(*main parse routine*)


(* ::Input:: *)
(********************************** MAIN PARSING ROUTINE**********************************)
(*pdfParseLHA: input is a single .info file and single .dat files (optional input is verbose-->default is True)*)
(*the .dat file is stored in memory and output is a setNumbers of the .dat file that was stored *)
pdfParseLHA[fileNameInfo_,fileNameData_,verbose_:True]:=
Module[{infolist,flavorlist,flavorcheck,infotable0,alphainfo,iset},
infolist=readInfo[fileNameInfo,verbose];
infotable0=infolist[[1]];
alphainfo=infolist[[2]];
flavorlist=infolist[[3]];
{flavorcheck,iset}=readData[fileNameData,infotable0,alphainfo,verbose];
If[flavorlist!=flavorcheck,
Print["Error, Flavors from info file do not match flavors in data file"];
];(*End If...*)
Return[iset];
];(*End pdfParseLHA...*)



(* ::Input:: *)
(*pdfFamilyParseLHA: input is a directory containing a single .info file and multiple .dat files
(optional input is filetype-->default is "*.dat")*)
(*all .dat files are stored in memory and output is a list containing the setNumbers of the .dat files that were stored *)
pdfFamilyParseLHA[path_?StringQ,type_:"*.dat"]:=
Module[{currDir,datfileList,output,fLength,startSet,endSet,
infoFile,infolist,flavorlist,infotable0,alphainfo,tmpout},
(*currDir=Directory[];*)
(*SetDirectory[path];*)
infoFile=FileNames[path<>"/"<>"*.info"];
If[Length[infoFile]!=1,
Print["Error, Missing or Multiple Info Files"];
Return[],
infoFile=infoFile[[1]];
];(*End If...*)
infolist=readInfo[infoFile,True];
infotable0=infolist[[1]];
alphainfo=infolist[[2]];
flavorlist=infolist[[3]];
datfileList=FileNames[path<>"/"<>type];
fLength=Length[datfileList];
Do[tmpout[i]=readData[datfileList[[i]],infotable0,alphainfo,False],
{i,1,fLength}
];(*End Do...*)
startSet=tmpout[1][[2]];
endSet=tmpout[fLength][[2]];
Print["Included ",endSet-startSet+1," files in the PDF family."];
(*SetDirectory[currDir];*)
output=Range[startSet,endSet];
Return[output];
];(*End pdfFamilyParseLHA...*)
(********************************** END MAIN PARSING ROUTINE**********************************)



(* ::Section:: *)
(*End Package*)


(* ::Input:: *)
End[];  (* End Private Context *)

Protect[pdfParseLHA,pdfFamilyParseLHA];

EndPackage[]; (* End Package Context *)
