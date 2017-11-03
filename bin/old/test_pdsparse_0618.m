(* ::Package:: *)

(* ::Section:: *)
(*step 1*)


(* ::Input::Initialization:: *)
SetDirectory[(*NotebookDirectory[]*)DirectoryName[$InputFileName] ];
Get["corr_proj_funcs.m"];


(* ::Input:: *)
(*(*set input arguments *)*)
(**)
(*configDir=Directory[]<>"/"(*NotebookDirectory[];*)(*DirectoryName[$InputFileName]*);*)
(*(*configfilename="config_pdf_resolution_test.txt";*)*)
(*configfilename="savedata_config.txt";*)


(* ::Input:: *)
(*(*read arguments from config file*)*)
(*{PDFsetDir,PDFsetmethod,ExptIDList,datalistFile,(*FxQGridDir*)dummy2,(*FxQGridFile*)dummy3,FxQSameptDir,FxQSameptFile,(*CorrDataDir*)dummy4,(*CorrDataFile*)dummy5,(*GridNx*)dummy6,(*GridNQ*)dummy7}=*)
(*readsavedataconfigfile[configDir,configfilename];*)
(**)
(*Print["arguments of config file\n",readsavedataconfigfile[configDir,configfilename]];*)


(* ::Input:: *)
(*myPDFsetDir=PDFsetDir;*)
(**)
(*myPDFsetdtafile=FileNames[myPDFsetDir<>"*dta"][[1]];*)
(*(*set PDFset*)*)
(*PDFname=StringSplit[myPDFsetDir,"/"][[-1]];*)
(**)
(*(*set dta Dir you want to read data, it's the PDFset Dir you choose*)*)
(*DtaDir=myPDFsetDir;*)
(**)
(*Print[myPDFsetDir];*)
(*Print[PDFname];*)
(*Print[DtaDir];*)
(**)


(* ::Section:: *)
(*step 2*)


Print["calculate fxQ\n"];

xQLF={LF[0.01,100],LF[0.1,100],LF[0.5,100]};
flavour=0;
Print[xQLF];
Print[myPDFsetDir];
Print[PDFsetmethod];
Print[flavour];

fxQcalculate[xQLF,"/home/botingw/code/pdf_correlation/dta_file/CT14NNLO/","Hessian",flavour];

Abort[];


(* ::Input:: *)
(*(*setup pdf function*)*)
(*pdfResetCTEQ;*)
(*(*generate a pdf space*)*)
(*pdfFamilyParseCTEQ["Dummy"];*)
(*ifamily=1; *)
(*(* IniDir="//users//nadolsky//share//lhapdf//6.1.5//share/LHAPDF//CT14nnlo//pds//"; *)*)
(*PdsDir=*)
(*pdfFamilyParseCTEQ[myPDFsetDir<>"*pds",ifamily];*)
(**)
(*Print[PdsDir];*)
(*Print[myPDFsetDir];*)


(* ::Input:: *)
(*pdfFamilyParseCTEQ[myPDFsetDir<>"*pds",ifamily]*)


(* ::Input:: *)
(*Print[PdsDir];*)
(*Print[myPDFsetDir];*)


(* ::Input::Initialization:: *)
(*setup pdf function*)
pdfResetCTEQ;
(*generate a pdf space*)
pdfFamilyParseCTEQ["Dummy"];
ifamily=1; 
(* IniDir="//users//nadolsky//share//lhapdf//6.1.5//share/LHAPDF//CT14nnlo//pds//"; *)

pdfFamilyParseCTEQ["/home/botingw/code/pdf_correlation/dta_file/2017.0604.1856.-0500_CT14-1_mod/"<>"*pds",ifamily];


Print[myPDFsetDir];


(* ::Input::Initialization:: *)
x=0.1;Q=100.0;
flavour=0;
Nset=Length[pdfSetList[[ifamily]]];
pdfSetActiveFamily[ifamily]; 
Print[
LF[x,Q,Sequence@@Table[pdfCTEQ[x,Q,flavour,iset],{iset,Nset}] ]
];



