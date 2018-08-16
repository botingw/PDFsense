(* ::Package:: *)

(* ::Text:: *)
(*For the simplified program*)
(*Function description:*)
(*1. This executable read  "savedata_config", and then read residual in .dta files and f(x,Q) in .pds files to calculate observables, saving data of observables into /PDFDataDir/PDFDataFile*)
(*2. (x, Q) are points of  samept method (please read the tutorial note of this program)*)
(*3. \[Dash] arguments that users should setup*)
(**)
(*\:2217 PDF set Dir, PDF method, Expt ID List, datalis file*)
(**)
(*\:2217 Correlation Path & Correlation File*)
(**)
(*\[Dash] output: if Path & File are "default", the program make files with all $obs indexes:*)
(**)
(*\[Dash] ./quick_data/{$obs}_samept_data_{$PDFname}.dat, where $PDFname = PDFname, ex: CT14NNLO, $obs = "fxQNset", "residualNset", "dtacentral"*)
(**)
(*formats of data files in ../quick_data/ are List of dimension:*)
(*"fxQ": [[iexpt, iflavour]]*)
(*"residualNset": [[iexpt]]*)
(*"dtacentral": [[iexpt]]*)
(**)


(* ::Section:: *)
(*How to Run*)


(* ::Text:: *)
(*1. setup arguments in "savedata_config.txt" *)
(*2. *)
(*a. if this file's extension is .nb, run "Implement" title*)
(*b. if this file's extension is .m (script version), type math -script "filename of this file" in the terminal  *)


(* ::Input::Initialization:: *)
SetDirectory[(*NotebookDirectory[]*)DirectoryName[$InputFileName] ];
Print["present directory: ",Directory[]];
lib1="corr_proj_funcs.m";
If[FileExistsQ[lib1],Print["loading ",lib1];Get[lib1],Print["library file ",lib1," doesn't exist"] ];
(*Get["corr_proj_funcs.m"]*)
Print["present directory: ",Directory[]];


(* ::Input:: *)
(*DtaExptList[dtaDirin_]:=*)
(*Module[{dtaDir=dtaDirin,dtafile,dtadata,exptlist},*)
(*dtafile=FileNames[dtaDir<>"*dta"][[1]];*)
(*(*assume the data in .dta file is "ct2016 format"*)*)
(*dtadata=ReadExptTable[dtafile,"ct2016"];*)
(*exptlist=Table[dtadata[[iexpt,1]],{iexpt,1,Length[dtadata]}];*)
(*exptlist*)
(*]*)


(* ::Section:: *)
(*Implement*)


(* ::Subsection:: *)
(*set input arguments *)


(* ::Input::Initialization:: *)
(*set input arguments *)

configDir=Directory[]<>"/";(*NotebookDirectory[];*)(*DirectoryName[$InputFileName];*)
(*configfilename="config_pdf_resolution_test.txt";*)
configfilename="cteq_to_input_config.txt";


(* ::Input::Initialization:: *)
(*read arguments from config file*)
(*20180815 change savedata to CTEQtoUserInput config file*)
{PDFset,dtaDir,PDFsetmethod,ExptIDList,datalistFile(*,(*FxQGridDir*)dummy2,(*FxQGridFile*)dummy3,FxQSameptDir,FxQSameptFile,CorrDataDir,CorrDataFile,(*GridNx*)dummy6,(*GridNQ*)dummy7*)}=
readCTEQtoUserInputconfigfile[configDir,configfilename]



(* ::Input:: *)
(*readCTEQtoUserInputconfigfile[configDir,configfilename]*)


(* ::Input::Initialization:: *)
(*20170620*)
Print["configure file directory: ",configDir];
Print["configure filename : ",configfilename];
Print["arguments read:\n","{PDFset,dtaDir,PDFsetmethod,ExptIDList,datalistFile}=\n",readCTEQtoUserInputconfigfile[configDir,configfilename] ];


(* ::Input::Initialization:: *)
(*20180815 hange savedata to CTEQtoUserInput config file*)
(*
myPDFsetDir=PDFsetDir

myPDFsetdtafile=FileNames[myPDFsetDir<>"*dta"][[1]];
(*set PDFset*)
PDFname=StringSplit[myPDFsetDir,"/"][[-1]]
Analyzename=StringSplit[myPDFsetDir,"/"][[-1]]

(*set dta Dir you want to read data, it's the PDFset Dir you choose*)
DtaDir=myPDFsetDir;
*)


(* ::Input::Initialization:: *)
(*set PDFset*)
PDFname=PDFset
Analyzename=StringSplit[dtaDir,"/"][[-1]]



(* ::Input::Initialization:: *)
(*if path setting is default, set it as in quick_data directory*)
quickdataDir="../quick_data/";
If[datalistFile=="default",datalistFile="./dat16lisformathematica"]
(*
If[FxQGridDir\[Equal]"default",FxQGridDir=quickdataDir]
If[FxQGridFile\[Equal]"default",FxQGridFile="fxQ_grid_"<>PDFname<>"_x"<>ToString[GridNx]<>"_Q"<>ToString[GridNQ]<>".dat"]
*)

(*
If[FxQSameptDir\[Equal]"default",FxQSameptDir=quickdataDir]
If[FxQSameptFile\[Equal]"default",FxQSameptFile="fxQ_samept_"<>PDFname<>".dat"]
*)
(*
If[CorrDataDir\[Equal]"default",CorrDataDir=quickdataDir]
If[CorrDataFile\[Equal]"default",CorrDataFile="samept_data_"<>PDFname<>".dat"]
*)


(* ::Input::Initialization:: *)
(*20170620*)

Print["PDFname: ",PDFname];
Print["datalistFile: ",datalistFile];
(*
Print["FxQGridDir: ",FxQGridDir];
Print["FxQGridFile: ",FxQGridFile];
*)

(*
Print["FxQSameptDir: ",FxQSameptDir];
Print["FxQSameptFile: ",FxQSameptFile];
*)
(*
Print["CorrDataDir: ",CorrDataDir];
Print["CorrDataFile: ",CorrDataFile];
*)


(* ::Input:: *)
(*Directory[]*)


(* ::Input::Initialization:: *)
(*initialize datalis file to extract the expt name of each Expt ID*)
ReadLisFile[datalistFile]


(* ::Input::Initialization:: *)

exptlist=ExptIDList;
Print["read data expt id: ",exptlist];
Print[""];


(* ::Input:: *)
(*(*simple test, short expt, should delete later*)*)
(*(*exptlist={204,504}*)*)


(* ::Subsection:: *)
(*read .dta files*)


(* ::Input::Initialization:: *)
(*read expt data from .dta files*)
Print["read .dta files to extract analyzed data"];
Print[""];

exptdata=Readdtafile[["readdta"]][dtaDir,exptlist]


(* ::Input:: *)
(*Dimensions[exptdata]*)


(* ::Subsection:: *)
(*transform .dta data into dtadataclass*)


(* ::Input::Initialization:: *)
(*for all data read from .dta files, transf them to dtadata class*)
mydtadata=
Table[
Readdtafile[["toclass"]][exptdata[[iexpt,iset]],PDFname,PDFsetmethod],
(*todtadataclass[exptdata[[i,j]],PDFname,PDFsetmethod],*)
{iexpt,1,Dimensions[exptdata][[1]]},{iset,1,Dimensions[exptdata][[2]]}
]


(* ::Input:: *)
(*Dimensions[mydtadata]*)
(*mydtadata[[3,1]];*)


(* ::Input:: *)
(*(#[[57]][["data"]]//Length)&/@mydtadata*)


(* ::Subsection:: *)
(*add point index for each data*)


(* ::Input:: *)
(*(*20180322*)*)
(*(*after adding (x,Q) values for each data point, some data will specify multiple (x,Q) values and some failed (x,Q) points will be deleted, to track the sources of specified points, we give ipt index by the ordering of the raw data in .dta files*)*)


(* ::Input:: *)
(*mydtadata//Dimensions*)


(* ::Input:: *)
(*mydtadata[[8,1]][["label"]]*)


(* ::Input:: *)
(*Append[LF[1,2,3],2 ]*)


(* ::Input::Initialization:: *)
Table[
Npt=mydtadata[[iexpt,iset]][["data"]]//Length;
mydtadata[[iexpt,iset]][["label"]]=mydtadata[[iexpt,iset]][["label"]]~Join~{"ipt"};
Table[mydtadata[[iexpt,iset]][["data"]][[ipt]]=Append[mydtadata[[iexpt,iset]][["data"]][[ipt]],ipt];"dummy",{ipt,Npt}],
{iexpt,1,Dimensions[mydtadata][[1]]},{iset,1,Dimensions[mydtadata][[2]]}
];


(* ::Input:: *)
(*mydtadata[[1,1]][["label"]]*)
(*mydtadata[[2,5]][["label"]]*)
(*mydtadata[[1,1]][["data"]][[1;;3]]*)
(*mydtadata[[2,5]][["data"]][[1;;3]]*)


(* ::Subsection:: *)
(*add {x,Q} to data label*)


(* ::Input::Initialization:: *)
(*print Npt before calculate the (x,Q) by formulas*)
Print["Npt of each expt in .dta files (by {exptid, Npt})"];
Print[({#[[1]][["exptinfo","exptid"]],#[[1]][["data"]]//Length})&/@mydtadata];
Print[""];


(* ::Input::Initialization:: *)
(*if samept, run it, otherwise only transf data structure into LF[...]*)
(*add transformed {x,Q} in dtadata*)
(*x, Q will be the column 14, 15 in a data*)


(*decide calculate the grid correlation or samept correlation*)
fxQmode="samept";
If[
fxQmode=="samept",
Table[
(*add (x,Q) specifying PDFs for kinematic quantities of data by the corresponding formulas in selectExptxQv2*)
mydtadata[[iexpt,iset]][["data"]]=selectExptxQv2[mydtadata[[iexpt,iset]][["exptinfo","exptid"]],mydtadata[[iexpt,iset]][["data"]],"dummy"];
(*add label of {x,Q} to -2&-1 -th column*)
mydtadata[[iexpt,iset]][["label"]]=Join[mydtadata[[iexpt,iset]][["label"]],{"x","Q"}];
(*LF[...] become global variable*)
mydtadata[[iexpt,iset]]=Datamethods[["LFglobal"]][mydtadata[[iexpt,iset]] ],
{iexpt,1,Dimensions[mydtadata][[1]]},{iset,1,Dimensions[mydtadata][[2]]}
]
];

If[
fxQmode=="grid",
Table[
mydtadata[[iexpt,iset]]=Datamethods[["LFglobal"]][mydtadata[[iexpt,iset]] ],
{iexpt,1,Dimensions[mydtadata][[1]]},{iset,1,Dimensions[mydtadata][[2]]}
]
];



(* ::Input::Initialization:: *)
(*print Npt after calculate the (x,Q) by formulas*)
Print["Npt of each expt after using formulas to get (x,Q) specifying the kinematical quantities of data points (by {exptid, Npt})"];
Print[({#[[1]][["exptinfo","exptid"]],#[[1]][["data"]]//Length})&/@mydtadata];
Print[""];


(* ::Input:: *)
(*mydtadata[[1,1]][["label"]]*)
(*Datamethods[["getNcolumn"]][mydtadata[[1,1]] ]*)
(*Take[mydtadata[[1,1]][["data"]],3]*)


(* ::Input:: *)
(*(*potential problem of read .dta file: since some exptid will not in data file, mydata will lose these data and exptid of exptlist is different  with mydata*)*)
(*Dimensions[mydtadata]*)
(**)


(* ::Input:: *)
(*mydtadata[[1,1]];*)


(* ::Subsection:: *)
(*set a class with data struture LF[x,Q,residual]*)


(* ::Input:: *)
(*mydtadata[[1,1]][["label"]]*)
(**)


(* ::Input::Initialization:: *)
(*check whether the elements used to calculate residual are correct*)
(*residual === (Theory-ShifteData)/UnCorrError*)

ThIndex=5;ShiftDatIndex=11;UnCorrErrIndex=12;
Print["begin to calculate residuals from data, labels of indexes used for each experiment:"];

Table[
Print[
mydtadata[[iexpt,1]][["exptinfo","exptid"]],": (",mydtadata[[iexpt,1]][["label"]][[ThIndex]]," - ", mydtadata[[iexpt,1]][["label"]][[ShiftDatIndex]],")/",mydtadata[[iexpt,1]][["label"]][[UnCorrErrIndex]]
],
{iexpt,1,Dimensions[mydtadata][[1]]} 
];



(* ::Input::Initialization:: *)
(*calculate residuals (Theory-ShifteData)/UnCorrError*)

Table[
mydtadata[[iexpt,iset]][["data"]]=mydtadata[[iexpt,iset]][["data"]]/.LF[a__]:>LF[Sequence@@{a},({a}[[ThIndex]]-{a}[[ShiftDatIndex]])/{a}[[UnCorrErrIndex]] ];
mydtadata[[iexpt,iset]][["label"]]=Join[mydtadata[[iexpt,iset]][["label"]],{"residual"}];
"dummy",
{iexpt,1,Dimensions[mydtadata][[1]]},{iset,1,Dimensions[mydtadata][[2]]}
];



(* ::Input:: *)
(*mydtadata[[2,1]][["label"]]*)
(*mydtadata[[2,1]][["data"]];*)
(*mydtadata[[2,1]][["rawdata"]];*)


(* ::Input:: *)
(*(*extract Nset of data [[iexpt,iset,{residual},Npt]]*)*)
(*(**)
(*dataNset=*)
(*Table[*)
(*(*grab residual*)*)
(*Datamethods[["picktolist"]][mydtadata[[iexpt,iset]],{-1} ],*)
(*{iexpt,1,Dimensions[mydtadata][[1]]},{iset,1,Dimensions[mydtadata][[2]]}*)
(*];*)
(**)*)


(* ::Input::Initialization:: *)
(*input the dataclass[[Nset]] and the columnN, extract {LF[columnN(1),...,columnN(Nset)]} *)
getNsetLF[dataclassin_,columnin_]:=
Module[{dataclass=dataclassin,column=columnin,dataNset,Nset},
Nset=Length[dataclass];
(*extract Nset of data [[iset,{residual},Npt]]*)
dataNset=
Table[
(*grab residual*)
Datamethods[["picktolist"]][dataclass[[iset]],{column} ],
{iset,1,Nset}
];

(*transf to [[Npt,{residual},Nset]] format*)
dataNset=Transpose[dataNset,{3,2,1}];


(*transf to [[Npt,Nset]] format*)
(*CT14NNLO: [[Npt]]=LF[obs0,...obs56]*)
dataNset=
Table[
(*1 represent residual index*)
dataNset[[ix,1]]/.List->LF,
{ix,1,Length[dataNset]}
];

dataNset
]


(* ::Input:: *)
(**)


(* ::Subsection:: *)
(*build residualNset class*)


(* ::Input:: *)
(*mydtadata[[1,1]][["label"]]*)


(* ::Input::Initialization:: *)

residualNsetclass=
Table[
XIndex=Position[mydtadata[[iexpt,1]][["label"]],"x"][[1,1]];
QIndex=Position[mydtadata[[iexpt,1]][["label"]],"Q"][[1,1]];
(*make a class of {LF[x,Q],LF[x,Q],...}, extract (x, Q) *)
(*
Dtadatatmp=Datamethods[["take"]][mydtadata[[iexpt,1]],{XIndex,QIndex}];
*)
Dtadatatmp=mydtadata[[iexpt,1]];
Dtadatatmp[["data"]]=Dtadatatmp[["data"]][[All,{XIndex,QIndex}]];
Dtadatatmp[["label"]]=Dtadatatmp[["label"]][[{XIndex,QIndex}]];
(*extract residual data {LF[residual1,...Nset],...} for every experiments*)
residualdatatmp=getNsetLF[mydtadata[[iexpt]],-1];
Nset=Length[mydtadata[[iexpt]] ];
datalabel=Table[ToString[iset],{iset,1,Nset}];
Datamethods[["add"]][Dtadatatmp,residualdatatmp,datalabel],
{iexpt,1,Dimensions[mydtadata][[1]]}
];



(* ::Input:: *)
(*residualNsetclass[[1]][["label"]]*)
(*Dimensions[residualNsetclass]*)
(*Datamethods[["getNpt"]][residualNsetclass[[1]] ]*)
(*Datamethods[["getNcolumn"]][residualNsetclass[[1]] ]*)
(*residualNsetclass[[1]][["data"]][[1;;5]]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*mydtadata[[1,1]][["label"]]*)
(*mydtadata[[1,1]][["data"]][[1;;3]]*)
(*mydtadata[[1,10]][["data"]][[1;;3]]*)


(* ::Input:: *)
(**)


(* ::Subsection:: *)
(*build cteq .dta to general format class*)


(* ::Input:: *)
(*mydtadata[[1,1]][["label"]]*)
(*Position[mydtadata[[1,1]][["label"]],"x"][[1,1]]*)
(*Position[mydtadata[[1,1]][["label"]],"Q"][[1,1]]*)
(*mydtadata[[1,1]][["data"]][[All,{iptpos,XIndex,QIndex}]];*)


(* ::Input:: *)
(*(**)
(*{ThIndex,ShiftDatIndex,UnCorrErrIndex}*)
(**)*)


(* ::Input::Initialization:: *)
generalformatclass=
Table[
(*ThIndex=5;ShiftDatIndex=11;UnCorrErrIndex=12;*)
(*20180814 x, Q should be the last one in a array because selectExptxQv2 add them at the tail *)
XIndex=Position[mydtadata[[iexpt,1]][["label"]],"x"][[-1,1]];
QIndex=Position[mydtadata[[iexpt,1]][["label"]],"Q"][[-1,1]];
ThIndex=Position[mydtadata[[iexpt,1]][["label"]],"Th./Norm"][[1,1]];
ExpIndex=Position[mydtadata[[iexpt,1]][["label"]],"Exp"][[1,1]];
TotErrIndex=Position[mydtadata[[iexpt,1]][["label"]],"TotErr"][[1,1]];
ShiftDatIndex=Position[mydtadata[[iexpt,1]][["label"]],"ShiftedData"][[1,1]];
UnCorrErrIndex=Position[mydtadata[[iexpt,1]][["label"]],"UnCorErr"][[1,1]];
iptpos=Position[mydtadata[[iexpt,1]][["label"]],"ipt"][[1,1]];
(*x,Q class*)
(*
tmpdata=Datamethods[["take"]][residualNsetclass[[iexpt]],{1,2}];
tmpdata=Datamethods[["take"]][mydtadata[[iexpt,1]],{iptpos,XIndex,QIndex}];
*)
tmpdata=mydtadata[[iexpt,1]];
tmpdata[["data"]]=tmpdata[["data"]][[All,{iptpos,XIndex,QIndex}]];
tmpdata[["label"]]=tmpdata[["label"]][[{iptpos,XIndex,QIndex}]];
(*Di,\[Sigma]i,shifted Di, uncorrelated \[Sigma]i*)
Disigmaidatatmp=mydtadata[[iexpt,1]][["data"]][[All,{ExpIndex,TotErrIndex,ShiftDatIndex,UnCorrErrIndex}]];
datalabelDisigmai=mydtadata[[iexpt,1]][["label"]][[{ExpIndex,TotErrIndex,ShiftDatIndex,UnCorrErrIndex}]];
(*r0,r1,...r(Nset)*)

ridatatmp=residualNsetclass[[iexpt]][["data"]][[All,3;;-1]];
Nset=Length[mydtadata[[iexpt]] ];
datalabelri=Table["r_"<>ToString[iset],{iset,1,Nset}];

(*T0,T1,...T(Nset)*)
(*
Tidatatmp=getNsetLF[mydtadata[[iexpt]],ThIndex];
Nset=Length[mydtadata[[iexpt]] ];
datalabelTi=Table["Th_"<>ToString[iset],{iset,1,Nset}];
*)
(*add Di, \[Sigma]i to residuals*)
(*
tmpdata=Datamethods[["add"]][tmpdata,Disigmaidatatmp,datalabelDisigmai];
Datamethods[["add"]][tmpdata,Tidatatmp,datalabelTi]
*)
tmpdata=Datamethods[["add"]][tmpdata,Disigmaidatatmp,datalabelDisigmai];
Datamethods[["add"]][tmpdata,ridatatmp,datalabelri],
{iexpt,1,Dimensions[mydtadata][[1]]}
];


(* ::Input:: *)
(*Dimensions[mydtadata][[1]]*)
(*generalformatclass//Dimensions*)


(* ::Input:: *)
(*generalformatclass[[1]][["label"]]*)
(*generalformatclass[[2]][["label"]]*)
(*(generalformatclass[[1]][["data"]][[1;;2,6;;-1]]/.LF->List)-(residualNsetclass[[1]][["data"]][[1;;2,3;;-1]]/.LF->List)*)


(* ::Input:: *)
(*({LF[1.2,1.4,6.44]}/.LF->List)-({LF[1.2,1.4,6.445]}/.LF->List)*)


(* ::Input:: *)
(*generalformatclass[[1]][["data"]][[1]]*)
(*residualNsetclass[[1]][["data"]][[1]]*)


(* ::Subsection:: *)
(*check (x, \[Mu]) of outputs are correct (10^-10<x<1, 1<\[Mu]<10^6)*)


(* ::Input:: *)
(*generalformatclass[[1]][["label"]]*)


(* ::Input::Initialization:: *)
Print["check the (x,\[Mu]) value are not abnormal (too large or too small)"];
Print[""];

Table[
XIndex=Position[generalformatclass[[iexpt]][["label"]],"x"][[1,1]];
QIndex=Position[generalformatclass[[iexpt]][["label"]],"Q"][[1,1]];

tmpxQ=generalformatclass[[iexpt]][["data"]]/.LF[a__]:>{{a}[[XIndex]],{a}[[QIndex]]};
tmpxQerror=Select[tmpxQ,(#[[1]]<10^-10 || #[[1]]>1 || #[[2]]<1 || #[[2]]>10^6)&  ];
If[
Length[tmpxQerror]>0,
Print["Warning: expt ID = ",generalformatclass[[iexpt]][["exptinfo","exptid"]] ];
Print["some (x,\[Mu]) values are abnormal: x>1 or x<10^-10 or \[Mu]>10^6 (GeV) or \[Mu]<1 (GeV)"];
Print[tmpxQerror]
];
"dummy",
{iexpt,1,Length[generalformatclass]}
];


(* ::Input:: *)
(*fxQsamept2class[[3,0+6]][["exptinfo","exptid"]]*)
(*fxQsamept2class[[3,0+6]][["data"]]//Length*)
(*fxQsamept2class[[1,0+6]][["exptinfo","exptid"]]*)
(*fxQsamept2class[[1,0+6]][["data"]]//Length*)
(*fxQsamept2class[[2,0+6]][["exptinfo","exptid"]]*)
(*fxQsamept2class[[2,0+6]][["data"]]//Length*)


(* ::Subsection:: *)
(*save the general input analysis file for making PDFsense database *)


(* ::Input:: *)
(*PDFsetDir*)


(* ::Input:: *)
(*generalformatclass[[1]][["rawdata"]]//ToString//InputForm*)


(* ::Input:: *)
(*generalformatclass[[1]][["exptinfo"]]*)
(*generalformatclass[[1]][["PDFinfo"]]*)
(*generalformatclass[[1]][["rawdata"]][[5]]*)


(* ::Input:: *)
(*generalformatclass[[1]][["rawdata"]][[5]]//Head*)


(* ::Input::Initialization:: *)
(*check Npt info are correct*)
Table[
tmpID=generalformatclass[[iexpt]][["exptinfo","exptid"]];
tmpNpt=generalformatclass[[iexpt]][["rawdata"]][[5]];
If[IntegerQ[tmpID]==False,Print["error, the ",iexpt,"-th data, ID = ",tmpID," is not integer"];Abort[] ];
If[IntegerQ[tmpNpt]==False,Print["error, the Npt of ID = ",tmpID," is not integer"];Abort[] ];
"dummy",
{iexpt,1,Dimensions[mydtadata][[1]]}
]


(* ::Input::Initialization:: *)
(*20180815 change to cteq to inputdata config file*)
OutputDir=dtaDir;
OutputFile=Analyzename<>".txt";
(*make the output string*)
outputstr=""
outputstr=outputstr<>"PDFset: "<>PDFname<>"\n";

Table[
tmpID=generalformatclass[[iexpt]][["exptinfo","exptid"]];
tmpexptname=generalformatclass[[iexpt]][["exptinfo","exptname"]];
(*tmpAnalyzeName=generalformatclass[[iexpt]][["PDFinfo","PDFname"]];*)
tmpNpt=generalformatclass[[iexpt]][["rawdata"]][[5]];
(*ID, name, analyze file*)
outputstr=outputstr<>" DATA SET: "<>ToString[tmpID]<>";";
outputstr=outputstr<>" Expt Name: "<>tmpexptname<>";";
(*outputstr=outputstr<>" Analyze File: "<>tmpAnalyzeName<>";";*)
outputstr=outputstr<>" Expt Npt: "<>ToString[tmpNpt]<>";";
outputstr=outputstr<>"\n";
(*metadata*)
outputstr=outputstr<>"!METADATA"<>"\n";
outputstr=outputstr<>(generalformatclass[[iexpt]][["rawdata"]]//ToString);
outputstr=outputstr<>"\n";
(*data*)
outputstr=outputstr<>"!DATA"<>"\n";
(*make a 2dim data list, transform numbers too large or small to scientific form *)
datastr={generalformatclass[[iexpt]][["label"]]}~Join~Map[If[(Abs[#]<10^5 && Abs[#]>10^-5)||StringQ[#] || IntegerQ[#],#,ToSciForm[#,4]]&,(generalformatclass[[iexpt]][["data"]]/.LF->List),{2}];
(*align data and transform to string format*)
datastr=datastr//TableForm//ToString;
(*remove space line*)
datastr=StringReplace[datastr,"\n\n"->"\n"];
outputstr=outputstr<>datastr<>"\n\n",
{iexpt,1,Dimensions[mydtadata][[1]]}
];



(* ::Input::Initialization:: *)
Print["will save the PDFsense input analysis file: ",OutputDir<>OutputFile," in the .dta directory\n Users can use it to generate the PDFsense plotted data files in the database (,./quick_data/)"];
If[FileExistsQ[OutputDir<>OutputFile]==True,Print["error, the output file ",OutputDir<>OutputFile," has been exist. To avoid covering the existing file, we would not save the output file"];Abort[] ];


(* ::Input::Initialization:: *)



Export[
OutputDir<>OutputFile,
outputstr
(*
StringReplace[({generalformatclass[[1]][["label"]]}~Join~Map[If[(Abs[#]<10^5 && Abs[#]>10^-5)||StringQ[#] || IntegerQ[#],#,ToSciForm[#,4]]&,(generalformatclass[[1]][["data"]]/.LF\[Rule]List),{2}])//TableForm//ToString,"\n\n"\[Rule]"\n"]
*)
]


(* ::Section:: *)
(*others*)


(* ::Input:: *)
(*fxQsamept2class[[1,1]][["label"]]*)
(*dtacentralclass[[1]][["label"]]*)


(* ::Input:: *)
(*corrfxQdtaobsclass=Import[CorrDataDir<>"corr_"<>CorrDataFile,"ExpressionML"];*)
(*dRcorrfxQdtaobsclass=Import[CorrDataDir<>"dRcorr_"<>CorrDataFile,"ExpressionML"];*)
(**)
(*expterrorclass=Import[CorrDataDir<>"expterror_"<>CorrDataFile,"ExpressionML"];*)
(*residualclass=Import[CorrDataDir<>"residual_"<>CorrDataFile,"ExpressionML"];*)
(*deltaRclass=Import[CorrDataDir<>"dR_"<>CorrDataFile,"ExpressionML"];*)
(**)
(**)


(* ::Input:: *)
(*Dimensions[corrfxQdtaobsclass]*)
(*Table[corrfxQdtaobsclass[[iexpt,1]][["exptinfo","exptid"]],{iexpt,1,Length[corrfxQdtaobsclass]}]*)
(*Table[dRcorrfxQdtaobsclass[[iexpt,1]][["exptinfo","exptid"]],{iexpt,1,Length[dRcorrfxQdtaobsclass]}]*)
(*Table[expterrorclass[[iexpt]][["exptinfo","exptid"]],{iexpt,1,Length[expterrorclass]}]*)
(*Table[residualclass[[iexpt]][["exptinfo","exptid"]],{iexpt,1,Length[residualclass]}]*)
(*Table[deltaRclass[[iexpt]][["exptinfo","exptid"]],{iexpt,1,Length[deltaRclass]}]*)
(*Table[residualNsetclass[[iexpt]][["exptinfo","exptid"]],{iexpt,1,Length[residualNsetclass]}]*)
(**)
(*Table[corrfxQdtaobsclass[[iexpt,1]][["data"]]/.LF[a__]:>LF[{a}[[1]],{a}[[2]] ],{iexpt,1,Length[corrfxQdtaobsclass]}]*)
(**)


(* ::Input:: *)
(*corrfxQdtaobsclass[[1,5]][["data"]];*)


(* ::Input:: *)
(*Table[corrfxQdtaobsclass[[iexpt,1]][["data"]],{iexpt,1,Length[corrfxQdtaobsclass]}];*)


(* ::Input:: *)
(*fxQsamept2class[[1,0+6]];*)


(* ::Subsubsection:: *)
(*show data structures*)


(* ::Input:: *)
(*{corrfxQdtaobsclass,dRcorrfxQdtaobsclass,expterrorclass,residualclass,deltaRclass};*)


(* ::Input:: *)
(*fxQgrid2class;residualNsetclass;*)


(* ::Subsubsection:: *)
(*show the data structure of the PDF data*)


(* ::Input:: *)
(*Dimensions[fxQgrid2class]*)


(* ::Input:: *)
(*Keys[fxQgrid2class[[1,1]] ]*)


(* ::Input:: *)
(*fxQgrid2class[[1,1]][["label"]]*)


(* ::Input:: *)
(*Keys[fxQgrid2class[[1,1]][["PDFinfo"]] ]*)
(*Values[fxQgrid2class[[1,1]][["PDFinfo"]] ]*)
(*Keys[fxQgrid2class[[1,1]][["exptinfo"]] ]*)
(*Values[fxQgrid2class[[1,1]][["exptinfo"]] ]*)


(* ::Subsubsection:: *)
(*show the data structure of the observables data*)


(* ::Input:: *)
(*Dimensions[corrfxQdtaobsclass]*)


(* ::Input:: *)
(*Keys[corrfxQdtaobsclass[[1,1]] ]*)


(* ::Input:: *)
(*corrfxQdtaobsclass[[1,1]][["label"]]*)


(* ::Input:: *)
(*Keys[corrfxQdtaobsclass[[1,1]][["PDFinfo"]] ]*)
(*Values[corrfxQdtaobsclass[[1,1]][["PDFinfo"]] ]*)
(*Keys[corrfxQdtaobsclass[[1,1]][["exptinfo"]] ]*)
(*Values[corrfxQdtaobsclass[[1,1]][["exptinfo"]] ]*)


(* ::Input:: *)
(*residualclass[[1]][["data"]]*)


(* ::Input:: *)
(*CorrDataDir*)
(*CorrDataFile*)


(* ::Input:: *)
(*testnewindex=Import[CorrDataDir<>"expterror_"<>CorrDataFile,"ExpressionML"];*)


(* ::Input:: *)
(*testnewindex[[1]][["data"]]*)


(* ::Input:: *)
(*Export[CorrDataDir<>"expterror_"<>CorrDataFile,testnewindex,"ExpressionML"];*)


(* ::Input:: *)
(*Export[CorrDataDir<>"expterror_"<>CorrDataFile,expterrorclass,"ExpressionML"];*)


(* ::Input:: *)
(*expterrorclass[[1]][["data"]][[1,3]]*)
(*Head[expterrorclass[[1]][["data"]][[1,3]] ]*)
(*Head[testnewindex[[1]][["data"]][[1,1]] ]*)
(*testnewindex[[1]][["data"]];*)


(* ::Input:: *)
(*expterrorclass[[1]][["data"]][[1,3]]//Expression*)


(* ::Input:: *)
(*testnewindex[[1]][["data"]];*)


(* ::Input:: *)
(*TreeForm[testnewindex[[1]][["data"]][[1]]/.LF[a__]:>LF[{a}[[3]],{a}[[2]],N[{a}[[ExpErrIndex-4]]/{a}[[ExpIndex-3]] ] ] ];*)


(* ::Input:: *)
(*expterrorclass>>(CorrDataDir<>"expterror_"<>CorrDataFile)*)
(*expterrorclass[[1]];*)
(*residualclass[[1]];*)
(*corrfxQdtaobsclass[[1,1]];*)
(*deltaRclass[[1]];*)
(*residualNsetclass[[1]];*)
(**)


(* ::Input:: *)
(*CorrDataDir<>"expterror_"<>CorrDataFile*)


(* ::Input:: *)
(*mydtadata[[1,1]];*)


(* ::Input:: *)
(*dtacentralclass//Dimensions*)
(*residualNsetclass//Dimensions*)
(*fxQsamept2class//Dimensions*)


(* ::Input:: *)
(*{72,110,90,133}/(Length[dtacentralclass[[#]][["data"]] ]&/@Range[5,8])//N*)
(*{72,110,90,133}/(Length[residualNsetclass[[#]][["data"]] ]&/@Range[5,8])//N*)
(*Table[Length[fxQsamept2class[[#,iflavour+6]][["data"]] ],{iflavour,-5,5}]&/@Range[5,8]*)


(* ::Input:: *)
(*dtacentralclass[[1]][["label"]]*)
(*dtacentralclass[[5]][["data"]]/.LF[a__]:>{a}[[14]]*)


(* ::Input:: *)
(*dtacentralclass[[5]][["rawdata"]][[5]]*)


(* ::Input:: *)
(*Table[GatherBy[dtacentralclass[[iexpt]][["data"]],#[[14]]&]//Length,{iexpt,8}]*)


(* ::Input:: *)
(*(*for the specified (x,Q) points, gather them by the same raw data point, then for the data from the same raw data point, evenly distribute the weight as 1/(Npt raw)*)*)
(*(*specifieddata: specified data list, ptindexlist: the list with the raw data index for each specified data*)*)
(*(*e.g. for specified data {case1, case2,...}, the list is {pt1, pt2,...} representing the pt index of the raw data point specifying the specified data*)*)
(*WeightByNptSpecified[specifieddatain_,ptindexlistin_]:=*)
(*Module[{specifieddata=specifieddatain,ptindexlist=ptindexlistin,Npt,output},*)
(**)
(*(*gather the specified data by the pt index *)*)
(*output=GatherBy[{ptindexlist,specifieddata}//Transpose,#[[1]]&];*)
(*(*the dimension of the output is {groups,data in the same group, {pt index, data}}*)*)
(*(*we only take the data part*)*)
(*output[[All,All,2]]*)
(*]*)


(* ::Input:: *)
(*WeightByNptSpecified[dtacentralclass[[5]][["data"]],dtacentralclass[[5]][["data"]][[All,14]] ][[All,All,5]]*)


(* ::Input:: *)
(*{{1,2},{3,7}}//Mean*)


(* ::Input:: *)
(*(*get the |S| for each raw data point as the average of |S| of all point specified by this point *)*)
(*(Mean[#]&/@(WeightByNptSpecified[dRcorrfxQdtaobsclass[[8,7]][["data"]],dtacentralclass[[8]][["data"]][[All,14]] ]/.LF[a__]:>Abs[{a}[[3]] ]) )//Length*)
(*(Mean[#]&/@(WeightByNptSpecified[dRcorrfxQdtaobsclass[[8,7]][["data"]],dtacentralclass[[8]][["data"]][[All,14]] ]/.LF[a__]:>Abs[{a}[[3]] ]) )*)


(* ::Input:: *)
(*Table[dRcorrfxQdtaobsclass[[iexpt,iflavour]][["data"]]//Length,{iexpt,8},{iflavour,Length[dRcorrfxQdtaobsclass[[1]] ]}]*)


(* ::Input:: *)
(*Table[Count[( ( (dRcorrfxQdtaobsclass[[iexpt,iflavour]][["data"]])/.LF->List)//Flatten),0.0],{iexpt,8},{iflavour,11}]*)
