(* ::Package:: *)

(* ::Section:: *)
(*Introduction of making samept PDF*)


(* ::Text:: *)
(*Function description:*)
(*1. This executable read  "savedata_config" and save data of f(x, Q) of all flavours (bbar~b) into /PDFDataDir/PDFDataFile*)
(*2. (x, Q) are points of  samept method (please read the tutorial note of this program)*)
(*3. \[Dash] arguments that users should setup*)
(**)
(*\:2217 PDF set Dir, PDF method, Expt ID List*)
(**)
(*\:2217 F(x,Q) Samept Path & F(x,Q) Samept File for samept f(x,Q) data*)
(**)
(**)
(**)
(*\[Dash] output: if Path & File are "default", the program make a file: *)
(**)
(*\[Dash] ./quick_data/fxQ_{$methods}_{$PDFname}.m for samept data executables, where $PDFname = PDFname, where $methods = "samept", "grid", "xgrid", $PDFname = PDFname, ex: CT14NNLO*)
(**)
(*data in the file is a List of dimension [[iexpt, iflavour]], iflavour = bbar, cbar, sbar, dbar, ubar, g, u, d, s, c, b*)


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


(* ::Section:: *)
(*implement*)


(* ::Subsection:: *)
(*set input arguments *)


(* ::Input::Initialization:: *)
(*set input arguments *)

configDir=Directory[]<>"/";(*NotebookDirectory[];*)(*DirectoryName[$InputFileName];*)
(*configfilename="config_pdf_resolution_test.txt";*)
configfilename="savedata_config.txt";


(* ::Input::Initialization:: *)
(*read arguments from config file*)
{PDFsetDir,PDFsetmethod,ExptIDList,datalistFile,(*FxQGridDir*)dummy2,(*FxQGridFile*)dummy3,FxQSameptDir,FxQSameptFile,(*CorrDataDir*)dummy4,(*CorrDataFile*)dummy5,(*GridNx*)dummy6,(*GridNQ*)dummy7}=
readsavedataconfigfile[configDir,configfilename]



(* ::Input::Initialization:: *)
(*20170620*)
Print["configure file directory: ",configDir];
Print["configure filename : ",configfilename];
Print["arguments read:\n","{PDFsetDir,PDFsetmethod,ExptIDList,datalistFile,(*FxQGridDir*)dummy2,(*FxQGridFile*)dummy3,FxQSameptDir,FxQSameptFile,(*CorrDataDir*)dummy4,(*CorrDataFile*)dummy5,(*GridNx*)dummy6,(*GridNQ*)dummy7}=\n",readsavedataconfigfile[configDir,configfilename] ];


(* ::Input::Initialization:: *)

myPDFsetDir=PDFsetDir

myPDFsetdtafile=FileNames[myPDFsetDir<>"*dta"][[1]];
(*set PDFset*)
PDFname=StringSplit[myPDFsetDir,"/"][[-1]]

(*set dta Dir you want to read data, it's the PDFset Dir you choose*)
DtaDir=myPDFsetDir;


(* ::Input::Initialization:: *)
(*if path setting is default, set it as in quick_data directory*)
quickdataDir="../quick_data/";
If[datalistFile=="default",datalistFile="./dat16lisformathematica"]
(*
If[FxQGridDir\[Equal]"default",FxQGridDir=quickdataDir]
If[FxQGridFile\[Equal]"default",FxQGridFile="fxQ_grid_"<>PDFname<>"_x"<>ToString[GridNx]<>"_Q"<>ToString[GridNQ]<>".m"]
*)

If[FxQSameptDir=="default",FxQSameptDir=quickdataDir]
If[FxQSameptFile=="default",FxQSameptFile="fxQ_samept_"<>PDFname<>".m"]
(*
If[CorrDataDir\[Equal]"default",CorrDataDir=quickdataDir]
If[CorrDataFile\[Equal]"default",CorrDataFile=]
*)


(* ::Input::Initialization:: *)
(*20170620*)

Print["PDFname: ",PDFname];
Print["datalistFile: ",datalistFile];
(*
Print["FxQGridDir: ",FxQGridDir];
Print["FxQGridFile: ",FxQGridFile];
*)
Print["FxQSameptDir: ",FxQSameptDir];
Print["FxQSameptFile: ",FxQSameptFile];
(*
Print["CorrDataDir: ",FxQGridDir];
Print["CorrDataFile: ",FxQGridFile];
*)


(* ::Input:: *)
(*Directory[]*)


(* ::Input::Initialization:: *)
ReadLisFile[datalistFile]


(* ::Input::Initialization:: *)

exptlist=ExptIDList;
Print["read data expt id: ",exptlist];



(* ::Input:: *)
(*(*simple test, short expt, should delete later*)*)
(*(*exptlist={204,504}*)*)


(* ::Subsection:: *)
(*run one time of PDF ifamily, it seems for calculation of deltaR, we need to run one time PDF*)


(* ::Input::Initialization:: *)
(*setup pdf function*)
pdfResetCTEQ;
(*generate a pdf space*)
pdfFamilyParseCTEQ["Dummy"];
ifamily=1; 
(* IniDir="//users//nadolsky//share//lhapdf//6.1.5//share/LHAPDF//CT14nnlo//pds//"; *)
PdsDir=
pdfFamilyParseCTEQ[myPDFsetDir<>"*pds",ifamily];


(* ::Subsection:: *)
(*read .dta files*)


(* ::Input::Initialization:: *)
(*read expt data from .dta files*)
exptdata=Readdtafile[["readdta"]][DtaDir,exptlist]


(* ::Subsection:: *)
(*transform .dta data into dtadataclass*)


(* ::Input::Initialization:: *)
(*for all data read from .dta files, transf them to dtadata class*)
(*
PDFname=StringSplit[DtaDir,"/"][[-1]]
PDFsetmethod="Hessian"
*)
mydtadata=
Table[
Readdtafile[["toclass"]][exptdata[[iexpt,iset]],PDFname,PDFsetmethod],
(*todtadataclass[exptdata[[i,j]],PDFname,PDFsetmethod],*)
{iexpt,1,Dimensions[exptdata][[1]]},{iset,1,Dimensions[exptdata][[2]]}
]


(* ::Input:: *)
(*mydtadata[[4,1]][["data"]]*)


(* ::Subsection:: *)
(*add {x,Q} to data label*)


(* ::Input::Initialization:: *)
(*add transformed {x,Q} in dtadata*)
(*x, Q will be the column 14, 15 in a data*)
Table[
(*add data by formula*)
mydtadata[[iexpt,iset]][["data"]]=selectExptxQv2[mydtadata[[iexpt,iset]][["exptinfo","exptid"]],mydtadata[[iexpt,iset]][["data"]],"dummy"];
(*add label of {x,Q} to -2&-1 -th column*)
mydtadata[[iexpt,iset]][["label"]]=Join[mydtadata[[iexpt,iset]][["label"]],{"x","Q"}];
(*LF[...] become global*)
mydtadata[[iexpt,iset]]=Datamethods[["LFglobal"]][mydtadata[[iexpt,iset]] ],
{iexpt,1,Dimensions[mydtadata][[1]]},{iset,1,Dimensions[mydtadata][[2]]}
]


(* ::Input:: *)
(*mydtadata[[1,1]][["label"]]*)
(*Datamethods[["getNcolumn"]][mydtadata[[1,1]] ]*)
(*Take[mydtadata[[1,1]][["data"]],3]*)
(*Take[mydtadata[[2,1]][["data"]],3]*)
(**)


(* ::Input:: *)
(*(*potential problem of read .dta file: since some exptid will not in data file, mydata will lose these data and exptid of exptlist is different  with mydata*)*)
(*Dimensions[mydtadata]*)
(**)


(* ::Subsection:: *)
(*extract {x,Q} value and get f(x,Q,flavour) for all flavours (-5~5 && 6, 7, 8)*)
(*{6,7,8} = {dbar/ubar,  d/u,  s+sbar/ubar+dbar}*)


(* ::Input:: *)
(*mydtadata[[1,1]][["label"]]*)
(*mydtadata[[2,1]][["label"]]*)


(* ::Input::Initialization:: *)
(*inteprate f(x,Q) by {x,Q} of exptdata*)
fxQsamept2class=
Table[
(*since every iset of expt data has the same {x,Q}, we only take iset=1*)
(*take x,Q value of data*)
Dtadatatmp=Datamethods[["take"]][mydtadata[[iexpt,1]],{-2,-1}];
(*make class*)
Pdsread[["fxQsamept"]][Dtadatatmp,myPDFsetDir,PDFsetmethod,flavour],
{iexpt,1,Dimensions[mydtadata][[1]]},{flavour,-5,5(*Dimensions[mydtadata][[2]]*)}
];


(* ::Input:: *)
(*Dimensions[fxQsamept2class]*)
(*(*print gluon f(x,Q) for 57 sets*)*)
(*fxQsamept2class[[1,6]][["label"]]*)
(*fxQsamept2class[[1,6]][["data"]][[1]]*)


(* ::Subsection:: *)
(*save f(x, Q) grid into quickdata file*)


(* ::Input:: *)
(*Directory[]*)
(*FxQSameptDir*)


(* ::Input:: *)
(*(*set the save path*)*)
(*(**)
(*quickdataDir="../quick_data/";*)
(*fxQfile="fxQ_samept_"<>PDFname<>".m";*)
(**)*)


(* ::Input::Initialization:: *)
(*save f(x, Q) grid into .m file*)
Export[FxQSameptDir<>FxQSameptFile,fxQsamept2class,"ExpressionML"];


(* ::Input:: *)
(*(**)
(*fxQsamept2class=Import[FxQSameptDir<>fxQfile,"ExpressionML"];*)
(**)*)


(* ::Input:: *)
(*Dimensions[fxQsamept2class]*)
(*Dimensions[fxQsamept2class]*)
(*fxQsamept2class[[1,1]][["data"]][[100]]*)


(* ::Input:: *)
(*$ProcessorCount*)


(* ::Input:: *)
(*fxQsamept2class[[2,6]];*)


(* ::Section:: *)
(*test the result*)


(* ::Input:: *)
(*fxQsamept2class=Import[FxQSameptDir<>fxQfile,"ExpressionML"];*)


(* ::Input:: *)
(*Dimensions[fxQsamept2class]*)
(*Table[fxQsamept2class[[iexpt]][[1]][["exptinfo","exptid"]],{iexpt,1,Length[fxQsamept2class]}]*)
(*Table[fxQsamept2class[[iexpt]][[1]][["data"]][[1]],{iexpt,1,Length[fxQsamept2class]}]*)


(* ::Input:: *)
(*Dimensions[fxQsamept2class]*)


(* ::Input:: *)
(*fxQsamept2class[[1,1]]*)


(* ::Input:: *)
(*mydtadata[[1,1]]*)


(* ::Input:: *)
(*selectExptxQv2[mydtadata[[1,1]][["exptinfo","exptid"]],mydtadata[[1,1]][["data"]],"dummy"]*)


(* ::Input:: *)
(*mydtadata[[1,1]][["exptinfo","exptid"]]*)


(* ::Input:: *)
(*mydtadata[[1,1]][["data"]]*)


(* ::Input:: *)
(*fxQsamept2class[[8,6]][["data"]];*)
