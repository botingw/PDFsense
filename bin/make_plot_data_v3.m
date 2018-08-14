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


(* ::Input:: *)
(*(*set input arguments *)*)
(**)
(*configDir=Directory[]<>"/";(*NotebookDirectory[];*)(*DirectoryName[$InputFileName];*)*)
(*(*configfilename="config_pdf_resolution_test.txt";*)*)
(*configfilename="savedata_config.txt";*)


(* ::Input:: *)
(*(*read arguments from config file*)*)
(*{PDFsetDir,PDFsetmethod,ExptIDList,datalistFile,(*FxQGridDir*)dummy2,(*FxQGridFile*)dummy3,FxQSameptDir,FxQSameptFile,CorrDataDir,CorrDataFile,(*GridNx*)dummy6,(*GridNQ*)dummy7}=*)
(*readsavedataconfigfile[configDir,configfilename]*)
(**)


(* ::Input:: *)
(*(*20170620*)*)
(*Print["configure file directory: ",configDir];*)
(*Print["configure filename : ",configfilename];*)
(*Print["arguments read:\n","{PDFsetDir,PDFsetmethod,ExptIDList,datalistFile,(*FxQGridDir*)dummy2,(*FxQGridFile*)dummy3,FxQSameptDir,FxQSameptFile,CorrDataDir,CorrDataFile,(*GridNx*)dummy6,(*GridNQ*)dummy7}=\n",readsavedataconfigfile[configDir,configfilename] ];*)


(* ::Input:: *)
(**)
(*myPDFsetDir=PDFsetDir*)
(**)
(*myPDFsetdtafile=FileNames[myPDFsetDir<>"*dta"][[1]];*)
(*(*set PDFset*)*)
(*PDFname=StringSplit[myPDFsetDir,"/"][[-1]]*)
(*Analyzename=StringSplit[myPDFsetDir,"/"][[-1]]*)
(*(*set dta Dir you want to read data, it's the PDFset Dir you choose*)*)
(*DtaDir=myPDFsetDir;*)


(* ::Input:: *)
(*Directory[]*)


(* ::Input:: *)
(*(*simple test, short expt, should delete later*)*)
(*(*exptlist={204,504}*)*)


(* ::Subsection:: *)
(*set again*)


(* ::Input:: *)
(*AnalyzeFile=(*"~/code/pdf_correlation/dta_file/5xx_CT14H2/5xx_CT14H2.txt"*)"/home/botingw/Dropbox/pavel_project_myself/PDF_resolution/20180630/ct14nlo_pLHC.txt"*)
(*(*"PDS" or "LHA"*)*)
(*PDFtableFormat=(*"PDS"*)"LHA"*)
(*PDFsetDir=(*"~/code/pdf_correlation/dta_file/"*)"~/code/pdf_correlation/LHAPDF/"*)
(*(**)
(*PDFname="CT14HERA2NNLO"*)
(**)*)
(*PDFsetmethod="Hessian"*)
(*(*error, no select id yet*)*)
(*ExptIDList=(*{542,545}*){601}*)
(*datalistFile="dat170928lisformathematica"*)
(**)


(* ::Input:: *)
(*Analyzename=FileBaseName[StringSplit[AnalyzeFile,"/"][[-1]] ]*)
(*myPDFsetDir=PDFsetDir*)
(*CorrDataDir="default"*)
(*CorrDataFile="default"*)


(* ::Input::Initialization:: *)
(*set input arguments *)

configDir=Directory[]<>"/";(*NotebookDirectory[];*)(*DirectoryName[$InputFileName];*)
(*configfilename="config_pdf_resolution_test.txt";*)
configfilename="analyzeddata_config.txt";

(*read input arguments from the configure file*)
{AnalyzeFile,PDFsetsDir,(*PDFname,*)PDFFormat,PDFsetmethod,ExptIDList,DatabaseDir,DatabaseFile}=ReadAnalyzedDataConfigFile[configDir,configfilename]


(* ::Input::Initialization:: *)
Analyzename=FileBaseName[StringSplit[AnalyzeFile,"/"][[-1]] ]


(* ::Input::Initialization:: *)
(*if path setting is default, set it as in quick_data directory*)

quickdataDir="../quick_data/";
(*
If[datalistFile\[Equal]"default",datalistFile="./dat16lisformathematica"]
*)

(*
If[FxQGridDir\[Equal]"default",FxQGridDir=quickdataDir]
If[FxQGridFile\[Equal]"default",FxQGridFile="fxQ_grid_"<>PDFname<>"_x"<>ToString[GridNx]<>"_Q"<>ToString[GridNQ]<>".dat"]
*)

(*
If[FxQSameptDir\[Equal]"default",FxQSameptDir=quickdataDir]
If[FxQSameptFile\[Equal]"default",FxQSameptFile="fxQ_samept_"<>PDFname<>".dat"]
*)

If[DatabaseDir=="default",DatabaseDir=quickdataDir]
If[DatabaseFile=="default",DatabaseFile="samept_data_"<>Analyzename<>".dat"]



(* ::Input::Initialization:: *)
(*20170620*)
(*
Print["PDFname: ",PDFname];
Print["datalistFile: ",datalistFile];
*)

(*
Print["FxQGridDir: ",FxQGridDir];
Print["FxQGridFile: ",FxQGridFile];
*)

(*
Print["FxQSameptDir: ",FxQSameptDir];
Print["FxQSameptFile: ",FxQSameptFile];
*)
Print["DatabaseDir: ",DatabaseDir];
Print["DatabaseFile: ",DatabaseFile];



(* ::Input::Initialization:: *)
(*initialize datalis file to extract the expt name of each Expt ID*)
(*
ReadLisFile[datalistFile]
*)


(* ::Input::Initialization:: *)

exptlist=ExptIDList;
Print["read data expt id: ",exptlist];
Print[""];


(* ::Subsection:: *)
(*check input directories exist*)


(* ::Input::Initialization:: *)
If[FileExistsQ[AnalyzeFile]==False,Print["error, the analyzed file ",AnalyzeFile," does not exist"];Abort[] ];
If[DirectoryQ[PDFsetsDir]==False,Print["error, the PDFsets directory ",PDFsetsDir," does not exist"];Abort[] ];
If[DirectoryQ[DatabaseDir]==False,Print["error, the database directory ",DatabaseDir," does not exist"];Abort[] ];


(* ::Subsection:: *)
(*read input file*)


(* ::Input:: *)
(*AnalyzeFile*)


(* ::Input::Initialization:: *)
DtaFile=OpenRead[AnalyzeFile];
If[DtaFile==$Failed,Abort[]];
(* The first line is the name of the file; dump it *)
(*
Read[DtaFile,Record,RecordSeparators->"DATA SET"];
*)
(*
ExperimentRecords=ReadList[DtaFile,Record,RecordSeparators\[Rule]"DATA SET:"];
*)

(*divide the file by experimental datasets*)
ExperimentRecords=ReadList[DtaFile,Record,RecordSeparators->" DATA SET:"];
(*read the PDFname at title*)
PDFname=Read[StringSplit[ExperimentRecords[[1]],":"][[2]]//StringToStream,Word];
(*remove PDFname part in the list*)
ExperimentRecords=Drop[ExperimentRecords,1];
Nexpt=ExperimentRecords//Length;

exptdata=
Table[
ExperimentRecords[[iexpt]]=StringSplit[ExperimentRecords[[iexpt]],{"!METADATA","!DATA"}];
If[Length[ExperimentRecords[[iexpt]] ]!=3,Print["error"] ];

(*read ID, IDname, analyze file*)
MyStream=ExperimentRecords[[iexpt,1]]//StringToStream;

titletmp=ReadLine[MyStream];
titletmp=StringSplit[titletmp,";"];
IDtmp=Read[titletmp[[1]]//StringToStream,Number];
IDnametmp=Read[StringSplit[titletmp[[2]],":"][[2]]//StringToStream,Word];
(*
AnalyzeFiletmp=Read[StringSplit[titletmp[[3]],":"][[2]]//StringToStream,Word];
*)
Npttmp=Read[StringSplit[titletmp[[3]],":"][[2]]//StringToStream,Number];
(*read metadata*)
metadatatmp=ExperimentRecords[[iexpt,2]];
(*read data label*)
MyStream=ExperimentRecords[[iexpt,3]]//StringToStream;

datatmp=ReadList[MyStream,Record];
datalabel=datatmp[[1]];
datalabel=ReadList[datalabel//StringToStream,Word];
(*read data with no label*)
datatmp=Drop[datatmp,1];
Npt=datatmp//Length;
datatmp=Table[ReadList[datatmp[[ipt]]//StringToStream,Number]/.List->LF,{ipt,Npt}];
Ncol1=datalabel//Length;
Ncol2=datatmp[[1]]//Length;
Print["{ID,Npt,Ncol1,Ncol2} = ",{IDtmp,Npt,Ncol1,Ncol2}];
If[Ncol1!=Ncol2,Print["error"] ];
{{IDtmp,IDnametmp,(*AnalyzeFiletmp,*)Npttmp},metadatatmp,datalabel,datatmp},
{iexpt,Nexpt}
];

Print[Dimensions[ExperimentRecords][[1]]," experiment record(s) read from ",AnalyzeFile];
Close[DtaFile];


(* ::Input:: *)
(*exptdata[[2]]*)


(* ::Subsection:: *)
(*data to class*)


(* ::Input::Initialization:: *)
(*test transf output of ReadExptTable into dtadata class form*)
toinputdataclass[datain_,PDFnamein_,PDFsetmethodin_]:=
Module[{data=datain,PDFname=PDFnamein,PDFsetmethod=PDFsetmethodin,Dtadatatmp,Ndatacolumn,
IDtmp,IDnametmp,(*AnalyzeFiletmp*)Npttmp,metadatatmp,datalabeltmp,datatmp},

{{IDtmp,IDnametmp,(*AnalyzeFiletmp*)Npttmp},metadatatmp,datalabeltmp,datatmp};
{IDtmp,IDnametmp,(*AnalyzeFiletmp*)Npttmp}=data[[1]];
metadatatmp=data[[2]];
datalabeltmp=data[[3]];
datatmp=data[[4]];

Dtadatatmp=Dtadata;
Dtadatatmp[["data"]]=datatmp;
Dtadatatmp[["exptinfo","exptid"]]=IDtmp;
Dtadatatmp[["exptinfo","exptname"]]=IDnametmp;
Dtadatatmp[["exptinfo","Npt"]]=Npttmp;
Dtadatatmp[["PDFinfo","PDFname"]]=PDFname;
Dtadatatmp[["PDFinfo","PDFsetmethod"]]=PDFsetmethod;

Dtadatatmp[["label"]]=datalabeltmp;

(*20180712 add the analyze file info to exptinfo, because all expts come from AnalyzeFile, so assign Analyzename to all of them*)
Dtadatatmp[["exptinfo"]]=Join[Dtadatatmp[["exptinfo"]],<|"Analyzename"->Analyzename|>];
(*2017.01.19: some labels of expt are not at data[[3]], give them 13 dummy labels, need modify in the future*)
(*20170828 remove the "wrongformat" reminder*)

(*
If[
Length[Dtadatatmp[["label"]] ]!=13, 
Ndatacolumn=Length[Apply[List,Dtadatatmp[["data"]][[1]] ] ];(*2017.01.22*)
Dtadatatmp[["label"]]=Table["wrongformat",{i,1,Ndatacolumn}]
];
*)

(*2017.01.16 add dta raw data == utput of ReadExptTable except for it's data *)
Dtadatatmp[["rawdata"]]=metadatatmp;

Dtadatatmp
]


(* ::Input:: *)
(*Dtadata*)


(* ::Input:: *)
(*(**)
(*mydtadata=*)
(*Table[*)
(*Dtadatatmp=Dtadata;*)
(**)
(*{IDtmp,IDnametmp,(*AnalyzeFiletmp*)Npttmp}=exptdata[[iexpt,1]];*)
(*Dtadatatmp[["exptinfo","exptid"]]=IDtmp;*)
(*Dtadatatmp[["exptinfo","exptname"]]=IDnametmp;*)
(*Dtadatatmp[["PDFinfo","PDFname"]]=PDFname;*)
(*Dtadatatmp[["PDFinfo","PDFsetmethod"]]=PDFsetmethod;*)
(**)
(*Dtadatatmp[["rawdata"]]=exptdata[[iexpt,2]];*)
(*Dtadatatmp[["label"]]=exptdata[[iexpt,3]];*)
(*Dtadatatmp[["data"]]=exptdata[[iexpt,4]];*)
(*Dtadatatmp,*)
(*{iexpt,exptdata//Length}*)
(*];*)
(**)*)


(* ::Input::Initialization:: *)
mydtadata=
Table[
toinputdataclass[exptdata[[iexpt]],PDFname,PDFsetmethod],
{iexpt,exptdata//Length}
];


(* ::Input:: *)
(**)
(*mydtadata[[1]][["rawdata"]]*)
(*mydtadata[[1]][["PDFinfo"]]*)
(*mydtadata[[1]][["exptinfo"]]*)
(*mydtadata[[1]][["label"]]*)


(* ::Subsection:: *)
(*interpolate PDF values for (x,Q) of data points for flavours from bbar to b*)


(* ::Subsubsection:: *)
(* activate the pdf  library*)


(* ::Input::Initialization:: *)
pdfReset[]


(* ::Input::Initialization:: *)
 (*IniDir="~/cteq/ini/ct10/jpcb/"*)
(*IniDir="/users/nadolsky/cteq/ini/ct10/cx22a/"*)

(*activate the PDFsset*)
(*if CTEQ format*)
IniDir=PDFsetsDir<>PDFname

Switch[
PDFFormat,
"PDS",
If[
Length[FileNames["*pds",{IniDir}] ]>0,
Print[Length[FileNames["*pds",{IniDir}] ] ];pdfFamilyParseCTEQ[IniDir,"*pds"]
],
"LHA",
(*if LHA format*)
If[
Length[FileNames["*dat",{IniDir}] ]>0,
Print[Length[FileNames["*dat",{IniDir}] ] ];pdfFamilyParseLHA[IniDir,"*.dat"]
],
_,
Print[]
];


(* ::Input:: *)
(*(*pdfSetActiveFamily[1]*)*)
(**)
(*pdfFunction[15, 1, 0.1, 1.3]*)


(* ::Input:: *)
(*(*check Nset of the file is equal to Nset of PDFset*)*)
(*If[Nset!=pdfSetList//Length,Print["error, the #replicas in the input data != #replica of ",PDFname];Abort[] ];*)
(**)


(* ::Subsubsection:: *)
(*check the # of theoretical replicas = Nset of the PDFname*)


(* ::Input:: *)
(*Nset*)


(* ::Input::Initialization:: *)
Nset=pdfSetList//Length;
(*???*)

Table[
(*Th case*)
(*
ThiIndex=Position[mydtadata[[iexpt]][["label"]],"Th_1"]//Flatten;
labelThf="Th_"<>ToString[Nset];
labelThfplus1="Th_"<>ToString[Nset+1];
tmpID=mydtadata[[iexpt]][["exptinfo","exptid"]];
(*if the Nset(Th) < Nset(PDFset) or Nset(Th) > Nset(PDFset), return the error message*)
NseteqQ=True;
If[Length[ThiIndex]\[NotEqual]1,Print["error, for Expt ID = ",ToString[tmpID],", the number of column label = Th_1 (which represent the central replica of theoretical predictions) should be 1"];Abort[] ];
ThiIndex=ThiIndex[[1]];

If[
(*check Nset(Th) !< Nset(PDFset)*)
(*length of label < index of Th_final, false*)
Length[mydtadata[[iexpt]][["label"]] ]<(ThiIndex+Nset-1),
NseteqQ=False,
If[
(*length of label >= index of Th_final but the label(Th_final) is not Th_Nset, false*)
mydtadata[[iexpt]][["label"]][[ThiIndex+Nset-1]]\[NotEqual]labelThf,
NseteqQ=False,
(*check Nset(Th) !> Nset(PDFset)*)
(* the label(Th_final) = Th_Nset and length of label >= index of Th_final+1 and label(Th_final +1) =  Th_(Nset+1),
it means Nset(Th)>Nset(PDFset), false*)
If[Length[mydtadata[[iexpt]][["label"]] ]>=(ThiIndex+Nset-0),
If[
mydtadata[[iexpt]][["label"]][[ThiIndex+Nset-0]]==labelThfplus1,
NseteqQ=False
]
]
]
];
*)

(*residual case*)
ThiIndex=Position[mydtadata[[iexpt]][["label"]],"r_1"]//Flatten;
labelThf="r_"<>ToString[Nset];
labelThfplus1="r_"<>ToString[Nset+1];
tmpID=mydtadata[[iexpt]][["exptinfo","exptid"]];
(*if the Nset(Th) < Nset(PDFset) or Nset(Th) > Nset(PDFset), return the error message*)
NseteqQ=True;
If[Length[ThiIndex]!=1,Print["error, for Expt ID = ",ToString[tmpID],", the number of column label = r_1 (which represent the central replica of residuals) should be 1"];Abort[] ];
ThiIndex=ThiIndex[[1]];

If[
(*check Nset(Th) !< Nset(PDFset)*)
(*length of label < index of Th_final, false*)
Length[mydtadata[[iexpt]][["label"]] ]<(ThiIndex+Nset-1),
NseteqQ=False,
If[
(*length of label >= index of Th_final but the label(Th_final) is not Th_Nset, false*)
mydtadata[[iexpt]][["label"]][[ThiIndex+Nset-1]]!=labelThf,
NseteqQ=False,
(*check Nset(Th) !> Nset(PDFset)*)
(* the label(Th_final) = Th_Nset and length of label >= index of Th_final+1 and label(Th_final +1) =  Th_(Nset+1),
it means Nset(Th)>Nset(PDFset), false*)
If[Length[mydtadata[[iexpt]][["label"]] ]>=(ThiIndex+Nset-0),
If[
mydtadata[[iexpt]][["label"]][[ThiIndex+Nset-0]]==labelThfplus1,
NseteqQ=False
]
]
]
];


(*
If[mydtadata[[iexpt]][["label"]][[ThiIndex+Nset-1]]\[NotEqual]labelThf,Print["error, for Expt ID = ",ToString[tmpID],", the #replicas in the input data != #replica of ",PDFname," which should be",Nset];Abort[] ];

If[
If[mydtadata[[iexpt]][["label"]][[ThiIndex+Nset-1]]\[NotEqual]labelThf || mydtadata[[iexpt]][["label"]][[ThiIndex+Nset-0]]==labelThfplus1,Print["error, for Expt ID = ",ToString[tmpID],", the #replicas in the input data != #replica of ",PDFname," which should be",Nset];Abort[] ];
*)
If[NseteqQ==False,Print["error, for Expt ID = ",ToString[tmpID],", the #replicas in the input data != #replica of ",PDFname," which should be",Nset];Abort[] ],
{iexpt,mydtadata//Length}
];


(* ::Subsubsection:: *)
(*make fxQsamept2class*)


(* ::Input:: *)
(*Nset*)


(* ::Input:: *)
(*(*make f(x,Q) list for bbar~b*)*)
(*Table[pdfFlavor[iflv],{iflv,-5,5}]*)


(* ::Input::Initialization:: *)
(*find the flavour index in LHA convention*)
{ibbar,icbar,isbar,idbar,iubar,ig,iu,id,is,ic,ib}=(Position[Table[pdfFlavor[iflv],{iflv,-5,5}],#][[1,1]]&/@{"bbar","cbar","sbar","dbar","ubar","gluon","up","down","strange","charm","bottom"})-6


(* ::Input::Initialization:: *)
ti=AbsoluteTime[];
(*inteprate f(x,Q) by {x,Q} of exptdata*)
Print["read .pds/.dat files to intepolate PDF values corresponding to data points"];
Print[""];


(* ::Input::Initialization:: *)
fxQsamept2class=
Table[
(*set index of variables*)
XIndex=Position[mydtadata[[iexpt]][["label"]],"x"][[1,1]];
QIndex=Position[mydtadata[[iexpt]][["label"]],"Q"][[1,1]];
(*Th case*)
(*
ThiIndex=Position[mydtadata[[iexpt]][["label"]],"Th_1"][[1,1]];
(*need modify later*)ThfIndex=Position[mydtadata[[iexpt]][["label"]],"Th_57"][[1,1]];
*)
(*residual case*)
ThiIndex=Position[mydtadata[[iexpt]][["label"]],"r_1"][[1,1]];
(*20180814 modify*)
(*need modify later*)ThfIndex=Position[mydtadata[[iexpt]][["label"]],"r_"<>ToString[Nset] ][[1,1]];

ExpIndex=Position[mydtadata[[iexpt]][["label"]],"Exp"][[1,1]];
TotErrIndex=Position[mydtadata[[iexpt]][["label"]],"TotErr"][[1,1]];
ShiftDatIndex=Position[mydtadata[[iexpt]][["label"]],"ShiftedData"][[1,1]];
UnCorrErrIndex=Position[mydtadata[[iexpt]][["label"]],"UnCorErr"][[1,1]];
iptpos=Position[mydtadata[[iexpt]][["label"]],"ipt"][[1,1]];

(*interpolate PDF*)
tmpclass=mydtadata[[iexpt]];
tmpclass[["data"]]=tmpclass[["data"]]/.LF[a__]:>LF[{a}[[XIndex]],{a}[[QIndex]],Sequence@@Table[pdfFunction[iset,iflavour,{a}[[XIndex]],{a}[[QIndex]] ],{iset,Nset}] ];
(*set the label*)
labelPDF=Table["fxQ_"<>ToString[iset],{iset,Nset}];
tmpclass[["label"]]=Join[tmpclass[["label"]][[{XIndex,QIndex}]],labelPDF];
tmpclass,
{iexpt,mydtadata//Length},{iflavour,{ibbar,icbar,isbar,idbar,iubar,ig,iu,id,is,ic,ib}}
];


(* ::Input:: *)
(*mydtadata[[28]]*)


(* ::Input:: *)
(*fxQsamept2class[[1,6]][["data"]];*)


(* ::Input::Initialization:: *)
tf=AbsoluteTime[];
GetPDFvaluesTime=tf-ti;
NPDFval=
Sum[
(Datamethods[["getNcolumn"]][fxQsamept2class[[iexpt,flavour+6]] ]-2)*Datamethods[["getNpt"]][fxQsamept2class[[iexpt,flavour+6]] ],{iexpt,1,Dimensions[fxQsamept2class][[1]]},{flavour,-5,-5+Dimensions[fxQsamept2class][[2]]-1}
];
Print["total number of PDF value calculations is ",NPDFval];
Print["average time of one PDF value calculation is ",GetPDFvaluesTime/NPDFval];
Print["time of calculating PDF values is ",GetPDFvaluesTime];


(* ::Input:: *)
(*fxQsamept2class//Dimensions*)
(*fxQsamept2class[[1,6]][["data"]][[1]]*)


(* ::Subsection:: *)
(*make residual and dtacentral class*)


(* ::Input::Initialization:: *)
residualNsetclass=
Table[
XIndex=Position[mydtadata[[iexpt]][["label"]],"x"][[1,1]];
QIndex=Position[mydtadata[[iexpt]][["label"]],"Q"][[1,1]];
(*Th case*)
(*
ThiIndex=Position[mydtadata[[iexpt]][["label"]],"Th_1"][[1,1]];
(*need modify later*)ThfIndex=Position[mydtadata[[iexpt]][["label"]],"Th_57"][[1,1]];
*)
(*residual case*)
ThiIndex=Position[mydtadata[[iexpt]][["label"]],"r_1"][[1,1]];
(*20180814 modify*)
(*need modify later*)ThfIndex=Position[mydtadata[[iexpt]][["label"]],"r_"<>ToString[Nset] ][[1,1]];

ExpIndex=Position[mydtadata[[iexpt]][["label"]],"Exp"][[1,1]];
TotErrIndex=Position[mydtadata[[iexpt]][["label"]],"TotErr"][[1,1]];
ShiftDatIndex=Position[mydtadata[[iexpt]][["label"]],"ShiftedData"][[1,1]];
UnCorrErrIndex=Position[mydtadata[[iexpt]][["label"]],"UnCorErr"][[1,1]];
iptpos=Position[mydtadata[[iexpt]][["label"]],"ipt"][[1,1]];

tmpclass=mydtadata[[iexpt]];
(*Th case, error: difted data are different for replicas, so the residuals are not correct*)
(*
tmpclass[["data"]]=tmpclass[["data"]]/.LF[a__]\[RuleDelayed]LF[{a}[[XIndex]],{a}[[QIndex]],Sequence@@( ({a}[[ThiIndex;;ThfIndex]]-{a}[[ShiftDatIndex]])/{a}[[UnCorrErrIndex]]) ];
*)
(*residual case*)
tmpclass[["data"]]=tmpclass[["data"]]/.LF[a__]:>LF[{a}[[XIndex]],{a}[[QIndex]],Sequence@@({a}[[ThiIndex;;ThfIndex]]) ];
(*
Nset=(ThfIndex-ThiIndex+1);
*)
labelresidual=Table["r_"<>ToString[ilabel],{ilabel,Nset}];
tmpclass[["label"]]=Join[tmpclass[["label"]][[{XIndex,QIndex}]],labelresidual];
tmpclass,
{iexpt,mydtadata//Length}
];


(* ::Input:: *)
(*residualNsetclass//Dimensions*)
(*residualNsetclass[[1]][["label"]]*)
(*residualNsetclass[[1]][["data"]][[1;;3]]*)
(*residualNsetclass[[1]][["label"]]//Length*)
(*residualNsetclass[[1]][["data"]][[1]]//Length*)


(* ::Input::Initialization:: *)
dtacentralclass=
Table[
XIndex=Position[mydtadata[[iexpt]][["label"]],"x"][[1,1]];
QIndex=Position[mydtadata[[iexpt]][["label"]],"Q"][[1,1]];
(*Th case*)
(*
ThiIndex=Position[mydtadata[[iexpt]][["label"]],"Th_1"][[1,1]];
(*need modify later*)ThfIndex=Position[mydtadata[[iexpt]][["label"]],"Th_57"][[1,1]];
*)
(*residual case*)
ThiIndex=Position[mydtadata[[iexpt]][["label"]],"r_1"][[1,1]];
(*20180814 modify*)
(*need modify later*)ThfIndex=Position[mydtadata[[iexpt]][["label"]],"r_"<>ToString[Nset] ][[1,1]];

ExpIndex=Position[mydtadata[[iexpt]][["label"]],"Exp"][[1,1]];
TotErrIndex=Position[mydtadata[[iexpt]][["label"]],"TotErr"][[1,1]];
ShiftDatIndex=Position[mydtadata[[iexpt]][["label"]],"ShiftedData"][[1,1]];
UnCorrErrIndex=Position[mydtadata[[iexpt]][["label"]],"UnCorErr"][[1,1]];
iptpos=Position[mydtadata[[iexpt]][["label"]],"ipt"][[1,1]];

tmpclass=mydtadata[[iexpt]];
tmpclass[["data"]]=tmpclass[["data"]]/.LF[a__]:>LF[Sequence@@Drop[{a},{ThiIndex+1,ThfIndex}] ];
tmpclass[["label"]]=Drop[tmpclass[["label"]],{ThiIndex+1,ThfIndex}];
tmpclass,
{iexpt,mydtadata//Length}
];


(* ::Input:: *)
(*dtacentralclass//Dimensions*)
(*dtacentralclass[[1]][["label"]]*)
(*dtacentralclass[[1]][["data"]][[1]]*)
(*dtacentralclass[[1]][["label"]]//Length*)
(*dtacentralclass[[1]][["data"]][[1]]//Length*)


(* ::Subsection:: *)
(*check (x, \[Mu]) of outputs are correct (10^-10<x<1, 1<\[Mu]<10^6)*)


(* ::Input::Initialization:: *)
Print["check the (x,\[Mu]) value are not abnormal (too large or too small)"];
Print[""];

Table[
tmpxQ=fxQsamept2class[[iexpt,flavour+6]][["data"]]/.LF[a__]:>{{a}[[1]],{a}[[2]]};
tmpxQerror=Select[tmpxQ,(#[[1]]<10^-10 || #[[1]]>1 || #[[2]]<1 || #[[2]]>10^6)&  ];
If[
Length[tmpxQerror]>0,
Print["Warning: expt ID = ",fxQsamept2class[[iexpt,flavour+6]][["exptinfo","exptid"]],", flavour = ",flavour];
Print["some (x,\[Mu]) values are abnormal: x>1 or x<10^-10 or \[Mu]>10^6 (GeV) or \[Mu]<1 (GeV)"];
Print[tmpxQerror]
];
"dummy",
{iexpt,1,Length[fxQsamept2class]},{flavour,-5,-5+Length[fxQsamept2class[[1]] ]-1}
];


(* ::Subsection:: *)
(*save into file*)


(* ::Input::Initialization:: *)
ti=AbsoluteTime[];


(* ::Input::Initialization:: *)
(*20171125: lower the precision of output files to 4 digits: save the space and reading time*)
(*20171126 new way to extract digits of numbers*)
SetNumberDigit[numbersin_,Ndigitin_]:=
Module[{numbers=numbersin,Ndigit=Ndigitin,keepdigits,Norder,Nzero,numbersign},
If[numbers>=0,numbersign=1];
If[numbers<0,numbersign=-1];
(*check numbers are Real*)
{keepdigits,Norder}=RealDigits[numbers];

(*calculate values with the digit number, if digit number > #elements of Digit List, take #elements of Digit List*)
(*we can not use 10.0 to evaluate because by that the number become the numarical number with large digits*)
(*we only use A/B to store this number*)
numbersign*Sum[keepdigits[[i]]*10^(Norder-i),{i,Min[Ndigit,Length[keepdigits]]}]
];

Ndigits=4;

cpfxQ=fxQsamept2class;
cpresidual=residualNsetclass;
Print["begin to transform numbers of f(x,Q) and residuals to only ",Ndigits," digits"];
Print[""];

Table[
residualNsetclass[[iexpt]][["data"]]=residualNsetclass[[iexpt]][["data"]]/.LF[a__]:>LF@@(SetNumberDigit[#,Ndigits]&/@{a}),
{iexpt,Length[residualNsetclass]}
];

Table[
fxQsamept2class[[iexpt,flavour+6]][["data"]]=fxQsamept2class[[iexpt,flavour+6]][["data"]]/.LF[a__]:>LF@@(SetNumberDigit[#,Ndigits]&/@{a}),
{iexpt,1,Length[fxQsamept2class]},{flavour,-5,-5+Length[fxQsamept2class[[1]] ]-1}
];
(*
Table[residualNsetclass[[iexpt]][["data"]]=SetPrecision[residualNsetclass[[iexpt]][["data"]],Ndigits],{iexpt,Length[residualNsetclass]}];

Table[
fxQsamept2class[[iexpt,flavour+6]][["data"]]=SetPrecision[fxQsamept2class[[iexpt,flavour+6]][["data"]],Ndigits],
{iexpt,1,Length[fxQsamept2class]},{flavour,-5,-5+Length[fxQsamept2class[[1]] ]-1}
];
*)


(* ::Input:: *)
(*residualNsetclass[[2]][["data"]];*)


(* ::Input::Initialization:: *)
(*check the transformed numbers/untransformed numbers close to 1*)
(*give ratio a/b, when b = 0, deal with it independently*)
(*20171130: deal with a/b for b = 0 in ratio calculation*)
Ratio[a_,b_]:=If[b!=0.0 && NumberQ[b]==True,a/b,If[a!=0.0,Infinity,1.0] ];(*20171201: if 0/0, define ratio = 1*)

Print["error ratio of numbers after/before the transform: only shows values with Absolute ratio >0.001"];
Print[""];

cpRratio=
Table[
{residualNsetclass[[iexpt]][["exptinfo","exptid"]],Select[(Ratio[( (residualNsetclass[[iexpt]][["data"]]/.LF->List)//Flatten)[[#]],( (cpresidual[[iexpt]][["data"]]/.LF->List)//Flatten)[[#]]]-1.0)&/@Range[(residualNsetclass[[iexpt]][["data"]]/.LF->List)//Flatten//Length],(#>0.001 || #<-0.001)&]},
{iexpt,Length[residualNsetclass]}
];


Print[cpRratio];




(* ::Input:: *)
(*Ratio[0.0,1.0]*)


(* ::Input:: *)
(*(**)
(*( ( (residualNsetclass[[-2]][["data"]]/.LF\[Rule]List)//Flatten)/( (cpresidual[[-2]][["data"]]/.LF\[Rule]List)//Flatten) )*)
(**)*)
(*Select[(residualNsetclass[[1]][["data"]]/.LF->List)//Flatten,#==0&]*)
(*Select[(cpresidual[[1]][["data"]]/.LF->List)//Flatten,#==0&]*)
(*residualNsetclass[[1]][["data"]]/.LF->List*)
(*cpresidual[[1]][["data"]]/.LF->List*)


(* ::Input:: *)
(*Ratio[( (residualNsetclass[[-2]][["data"]]/.LF->List)[[1]]),( (cpresidual[[-2]][["data"]]/.LF->List)[[1]]) ]*)


(* ::Input:: *)
(*(*SetPrecision[fxQsamept2class[[1,1]],5]//OutputForm*)*)


(* ::Input:: *)
(*residualNsetclass//Dimensions*)


(* ::Input:: *)
(*DatabaseDir*)


(* ::Input::Initialization:: *)
Print["begin to save data..."];
Print[""];
(*save Nset of residual*)(*>> doesn't work in script version, use Put[expression, filename]*)
(*
Export[DatabaseDir<>"residualNset_"<>DatabaseFile,residualNsetclass,"ExpressionML"];
*)
Put[residualNsetclass,(DatabaseDir<>"residualNset_"<>DatabaseFile)];
(*save Nset of fxQ*)
Put[fxQsamept2class,(DatabaseDir<>"fxQNset_"<>DatabaseFile)];
(*save original data of.dta files (central value file)*)
(*perhaps we only want to save the central set information so that we don't waste storage and time?*)
(*here only extract the central set for each Expt ID*)

Put[dtacentralclass,(DatabaseDir<>"dtacentral_"<>DatabaseFile)];


(* ::Input::Initialization:: *)
(*save meta data file for the quick_data*)
MakeMetaDataFileV2[fxQsamept2class,(DatabaseDir<>"metadata_"<>DatabaseFile)];


(* ::Input::Initialization:: *)
tf=AbsoluteTime[];
WriteDatavalueTime=tf-ti;
NDataval=
Sum[
(3*6)*Datamethods[["getNpt"]][fxQsamept2class[[iexpt,flavour+6]] ],
{iexpt,1,Dimensions[fxQsamept2class][[1]]},{flavour,-5,-5+Dimensions[fxQsamept2class][[2]]-1}
];
Print["total number of data value writing is ",NDataval];
Print["average time of one data value writing is ",WriteDatavalueTime/NDataval];
Print["time of writing data values is ",WriteDatavalueTime];


(* ::Input::Initialization:: *)
(*20170620*)
(*
Print["data dimension {Nexpt,Nflavour} = ",Dimensions[corrfxQdtaobsclass] ];
Print["data filename = ",DatabaseDir<>"corr_"<>DatabaseFile];
Print["data filename = ",DatabaseDir<>"dRcorr_"<>DatabaseFile];
Print["data filename = ",DatabaseDir<>"expterror_"<>DatabaseFile];
Print["data filename = ",DatabaseDir<>"residual_"<>DatabaseFile];
Print["data filename = ",DatabaseDir<>"dR_"<>DatabaseFile];
Print["data filename = ",DatabaseDir<>"residualNset_"<>DatabaseFile];
Print["data label format example: ",corrfxQdtaobsclass[[1,1]][["label"]] ];
Print["data format example: ",corrfxQdtaobsclass[[1,1]][["data"]][[1]] ];
*)
Print["data dimension {Nexpt,Nflavour} = ",Dimensions[fxQsamept2class] ];
Print["data filename = ",DatabaseDir<>"residualNset_"<>DatabaseFile];
Print["data filename = ",DatabaseDir<>"fxQNset_"<>DatabaseFile];
Print["data filename = ",DatabaseDir<>"dtacentral_"<>DatabaseFile];
Print["data filename = ",DatabaseDir<>"metadata_"<>DatabaseFile];


(* ::Input:: *)
(*(**)
(*dRcorrdataclass=*)
(*Table[*)
(*tmpdataclass=Corrsameptdata;*)
(*(*set info of the correlation data class*)*)
(*tmpdataclass[["PDFinfo"]]=fxQgrid2class[[flavour+6]][["PDFinfo"]];*)
(*tmpdataclass[["exptinfo"]]=mydtadata[[iexpt,1]][["exptinfo"]];*)
(*(*correlation data of the same expt, flavour are in the same List: {LF[x,Q,corr],...} *)*)
(*tmpdataclass[["data"]]=corrgrid[[iexpt,flavour+6]]//Flatten;*)
(*tmpdataclass[["label"]]={"x","Q","residual"};*)
(*(*Print[tmpdataclass[["exptinfo","exptid"]] ];*)*)
(*tmpdataclass,*)
(*{iexpt,1,Length[residualNset]},{flavour,-5,5}*)
(*]*)
(**)*)


(* ::Input:: *)
(*(**)
(*Datamethods[["getNcolumn"]][corrdataclass[[1,6]] ]*)
(*Datamethods[["getNpt"]][corrdataclass[[1,6]] ]*)
(*Datamethods[["getNpt"]][mydtadata[[1,1]] ]*)
(*fxQgrid2class[[0+6]][["data"]]/.LF[a__]\[RuleDelayed]LF[{a}[[1]],{a}[[2]] ];*)
(*corrdataclass[[1,5]][["data"]]*)
(*corrdataclass[[1,14]][["data"]]*)
(**)*)


(* ::Subsubsection:: *)
(*save f (x, Q) grid into quickdata file*)


(* ::Input::Initialization:: *)
Print["The CPU time used is ",TimeUsed[]];
Print["The time used is ",SessionTime[]];


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
