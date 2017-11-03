(* ::Package:: *)

(* ::Section:: *)
(*Introduction of making samept PDF*)


(* ::Text:: *)
(*Function description:*)
(*1. This executable read  "savedata_config" and save data of f(x, Q) of all flavours (bbar~b) into /PDFDataDir/PDFDataFile*)
(*2. (x, Q) are points of  xgrid method (please read the tutorial note of this program)*)
(*3. \[Dash] arguments that users should setup*)
(**)
(*\:2217 PDF set Dir, PDF method, Expt ID List*)
(**)
(**)
(**)
(**)
(**)
(*\:2217 F(x,Q) Grid Path & F(x,Q) Grid File, Nx for xgrid f(x,Q) data (Q values are temporary set as Z mass and W mass, this part should be modified)*)
(**)
(*\[Dash] output: if Path & File are "default", the program make a file: *)
(**)
(*\[Dash] ./quick_data/fxQ_{$methods}_{$PDFname}_x{$Nx}.m for xgrid data executables, where $PDFname = PDFname, where $methods = "samept", "grid", "xgrid", $PDFname = PDFname, ex: CT14NNLO*)
(**)
(*data in the file is a List of dimension [[iflavour]], iflavour = bbar, cbar, sbar, dbar, ubar, g, u, d, s, c, b*)


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
{PDFsetDir,PDFsetmethod,(*ExptIDList*)dummy1,datalistFile,FxQGridDir,FxQGridFile,(*FxQSameptDir*)dummy2,(*FxQSameptFile*)dummy3,(*CorrDataDir*)dummy4,(*CorrDataFile*)dummy5,GridNx,(*GridNQ*)dummy6}=
readsavedataconfigfile[configDir,configfilename]



(* ::Input::Initialization:: *)
(*20170620*)
Print["configure file directory: ",configDir];
Print["configure filename : ",configfilename];
Print["arguments read:\n","{PDFsetDir,PDFsetmethod,(*ExptIDList*)dummy1,datalistFile,FxQGridDir,FxQGridFile,(*FxQSameptDir*)dummy2,(*FxQSameptFile*)dummy3,(*CorrDataDir*)dummy4,(*CorrDataFile*)dummy5,GridNx,(*GridNQ*)dummy6}=\n",readsavedataconfigfile[configDir,configfilename] ];


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
If[FxQGridDir=="default",FxQGridDir=quickdataDir]
If[FxQGridFile=="default",FxQGridFile="fxQ_xgrid_"<>PDFname<>"_x"<>ToString[GridNx]<>".m"]
(*
If[FxQSameptDir\[Equal]"default",FxQSameptDir=quickdataDir]
If[FxQSameptFile\[Equal]"default",FxQSameptFile=]
If[CorrDataDir\[Equal]"default",CorrDataDir=quickdataDir]
If[CorrDataFile\[Equal]"default",CorrDataFile=]
*)


(* ::Input::Initialization:: *)
(*20170620*)

Print["PDFname: ",PDFname];
Print["datalistFile: ",datalistFile];

Print["FxQGridDir: ",FxQGridDir];
Print["FxQGridFile: ",FxQGridFile];
(*
Print["FxQSameptDir: ",FxQSameptDir];
Print["FxQSameptFile: ",FxQSameptFile];
*)
(*
Print["CorrDataDir: ",FxQGridDir];
Print["CorrDataFile: ",FxQGridFile];
*)


(* ::Input:: *)
(*Directory[]*)


(* ::Input::Initialization:: *)
ReadLisFile[datalistFile]


(* ::Input:: *)
(*(**)
(*exptlist=ExptIDList;*)
(*Print["read data expt id: ",exptlist];*)
(**)*)


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
(* set up the x, Q of f(x, Q)*)


(* ::Input::Initialization:: *)
(*f(x,Q) versus x figure setting*)
(*set the Q values for plot, Q (GeV)*)

Qs={91.19,80.39}
(*set Nx points on (xmin, xmax), use \[Alpha]x to decide the distribution of points*)
xmin=10^-5;xmax=0.99; Nx=GridNx; \[Alpha]x=1./4.;


(* ::Input:: *)
(*(*Q (GeV)*)*)
(*(*Qs={2.0, 90.0, 125.0,175.0};*)*)
(*(*Qs={2.0,175.0};*)*)


(* ::Input::Initialization:: *)
(*xmin, xmax, Npt, rescaling function*)
(*xmin=10^-5;xmax=0.99; Nx=100; \[Alpha]x=1./4.;*)

{zmin,zmax}={xmin^\[Alpha]x,xmax^\[Alpha]x};
zs=Table[zmin+ix*(zmax-zmin)/Nx,{ix,0,Nx}];
xs=zs^(1/\[Alpha]x)


(* ::Subsection:: *)
(*make f(x,Q) class*)


(* ::Input::Initialization:: *)
xQLF=Table[LF[xs[[ix+1]],Qs[[iQ]] ],{iQ,1,Length[Qs]},{ix,0,Nx}]//Flatten;


(* ::Input:: *)
(*xs;*)


(* ::Input::Initialization:: *)
Npt=Length[xQLF//Flatten];
time=AbsoluteTiming[
fxQgridclass=
Table[
Print["begin to calculate flavour ",flavour];
fxQcalculate[xQLF,myPDFsetDir,PDFsetmethod,flavour],
{flavour,-5,5}
];
];
(*print time and efficency*)
Print["total time to calculate the f(x, Q) for all flavours is ",time," sec, for #points = ",Npt];


(* ::Subsection:: *)
(*save f(x, Q) grid into quickdata file*)


(* ::Input:: *)
(*Directory[]*)
(*FxQGridDir*)


(* ::Input::Initialization:: *)
(*save f(x, Q) grid into .m file*)
Export[FxQGridDir<>FxQGridFile,fxQgridclass,"ExpressionML"];


(* ::Input::Initialization:: *)
(*20170620*)
Print["data dimension {Nexpt,Nflavour} = ",Dimensions[fxQgridclass] ];
Print["data filename = ",FxQGridDir<>FxQGridFile];
Print["data label format example: ",fxQgridclass[[1]][["label"]] ];
Print["data format example: ",fxQgridclass[[1]][["data"]][[1]] ];



(* ::Input:: *)
(*(**)
(*fxQgrid2class=Import[FxQGridDir<>fxQfile,"ExpressionML"];*)
(**)*)


(* ::Input:: *)
(*Dimensions[fxQgridclass]*)
(*Dimensions[fxQgridclass]*)
(*fxQgridclass[[2]][["data"]][[1]]*)


(* ::Section:: *)
(*test the result*)


(* ::Input:: *)
(*fxQgrid2class=Import[FxQGridDir<>FxQGridFile,"ExpressionML"];*)


(* ::Input:: *)
(*Dimensions[fxQgridclass]*)
(*Union[Table[fxQgrid2class[[flavour]][["data"]]/.LF[a__]:>Take[{a},2],{flavour,1,Length[fxQgrid2class]}]//Flatten]*)
(*Table[fxQgrid2class[[flavour]][["data"]][[1]],{flavour,1,Length[fxQgrid2class]}]*)



