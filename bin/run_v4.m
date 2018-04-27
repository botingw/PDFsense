(* ::Package:: *)

(* ::Section:: *)
(*Introduction of run samept*)


(* ::Text:: *)
(*Function description:*)
(*1. This program read  "config1.txt", "plotdata_config.txt", and read data in /CorrDataDir/CorrDataFile to plot  samept figures*)
(*2. (x, Q) are points of  samept method (please read the tutorial note of this program)*)
(*3. plot figures for selected experiments , flavours, and observables*)
(*(observable: correlation, sensitivity (dR*correlation) of PDF and residual of data, residual central value, residual uncertainty, (expt error)/(expt central) )*)
(*figures: 2D-xQ & histograms of observable values*)
(*2D-xQ: Draw (x, Q) of an observable data on x-Q plane. colors and sizes of points are determined by the value of the observable at that point)*)
(*  *)
(*\:2217 #PDF set, # Figures to plot, # Experiments to include, #Functions to use in correlations, #User function parameters are for setup of the input data*)
(**)
(*\:2217 #x-Q figure parameters, #Histogram figure parameters, #in plots, #highlight mode, #data point size are for setup of how figures look like *)
(**)
(*\:2217 datalis file, Correlation Path, Correlation File  in "plotdata_config.txt" are setup of filenames that are read in this executable. If the executable need to load PDF data f(x, Q), filenames are determined by F(x,Q) Samept Path, F(x,Q) Samept File. *)
(*If Correlation File, F(x,Q) Samept File = "default", data filenames are determined by #PDF set.*)
(**)
(*\[Dash] output: 2D-xQ, histograms*)
(**)
(*formats of data files in ../quick_data/ are List of dimension:*)
(*"fxQ": [[iexpt, iflavour]]*)
(*"residualNset": [[iexpt]]*)
(*"dtacentral": [[iexpt]]*)
(**)
(*formats of data (variables) for generating figures are List of dimension: *)
(*"corr": [[iexpt, iflavour]]*)
(*"dRcorr": [[iexpt, iflavour]]*)
(*" dR": [[iexpt]]*)
(*"residual": [[iexpt]]*)
(*"expterror": [[iexpt]]*)


(* ::Section:: *)
(*How to Run*)


(* ::Text:: *)
(*1. setup arguments in "config1.txt" & "plotdata_config.txt" *)
(*2. *)
(*a. if this file's extension is .nb, run "control runfunc you want to run" title at bottom of your code*)
(*b. if this file's extension is .m (script version), type math -script "filename of this file" in the terminal  *)


(* ::Section:: *)
(*Set Working path and call .m packages*)


(* ::Input::Initialization:: *)
(*this switch is for controling of .nb& .m versions, the option of the switch depends this file is saved as .nb/.m (package) extension  *)
(*.m file could be implemented in the terminal*)
(*option: "nb" or "m"*)
ThisFileExtension="m";


(* ::Input::Initialization:: *)
(*this switch is for controling of two versions of the executables, v1:False, v2: True
v1: read Expt IDs from config1.txt and plot all data in one x-\[Mu] figure
v2: read Expt IDs from config1.txt and plot experiments seperately, storing each Expt plots into the directory with it's name \[Equal] Expt ID  
*)
LoopExptBool=False;


(* ::Input::Initialization:: *)
Switch[
ThisFileExtension,
"nb",
SetDirectory[NotebookDirectory[](*DirectoryName[$InputFileName]*) ],
"m",
SetDirectory[(*NotebookDirectory[]*)DirectoryName[$InputFileName] ],
_,
Print["file extension should be \"nb\" or \"m\""];Abort[]
];

(*20171114: 
SetDirectory[NotebookDirectory[](*DirectoryName[$InputFileName]*) ]
*)
Get["corr_proj_funcs.m"]


(* ::Section:: *)
(*Global variables*)


(* ::Input::Initialization:: *)
(*this project includes three branches: Mathematica script, python script, and webpage*)
(*
1. Mathematica script: by setting config files, users can read CTEQ analyzed data (.dta & .pds files) to extract observables by (x,\[Mu]), then generate figures by run_v4.nb. outputs are in a subdirectory of ./plots 
Advandage are that 1. users can generate many figures of many flavours, observables. 2. users can customize user define functions
 
2. python script: users can use python script to run a GUI, them set the expected outputs in the GUI. After submit, outputs will show by html format
Advantages are 1. easy to use  2. users can customize user define functions
3. webpage: users can use the python GUI on a website. outputs are in another webpage.
Advantages: 1. easy to use 2. users don't need to install Mathematica

*)
(*
modes:
1. Mathematica script
2. python script
3. webpage
*)
BranchMode=2;

(*ClassifyMode*)
(*
classify data by various groups, e.g. DIS \[Equal] 100<ID<199, 200<VBP<299, etc
then use different shapes to represent data points in their respective groups in output figures
modes:
"all": all data use one shape
"single": each Expt ID one shape
*)
ClassifyMode="single";


(* ::Section:: *)
(*read correlation (and other data of FigureType in config1.txt) from the data in files*)


(* ::Input::Initialization:: *)

Quicksaveplot[]:=
Module[{(*runfunc,figureDir,myPDFsetDir,PDFsetmethod,PDFname,PDFDataDir,datalistFile,expttype,exptid*)
Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,
ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2,
UserArgFunction(*20171116*),
JobDescription,ColorPaletterange
},
Print["begin function"];
(*set input arguments *)

(*set method of (x,Q) points, options: "samept", "grid", "sameptgrid"*)
(*sameptgrid is figures of comparison of samept data and grid data*)
PDFxQSelectMethod="samept";
Print["method to search (x,Q) points that dominate the process: ",PDFxQSelectMethod];
(*set input arguments *)


(*set config file path*)
configDir=Directory[]<>"/";(*NotebookDirectory[];*)(*DirectoryName[$InputFileName];*)
configfilename="config1.txt";
Print["reading arguments from ",configfilename];
(*20170301: new config file
{runfunc,figureDir,dummy1,dummy2,PDFname,dummy3,datalistFile,expttype,exptid}=
readcorrconfigfile[configDir,configfilename];
*)
(*new config file*)
(*20171109 use readcorrconfigfile5 for new highlight range convention*)
{Jobid,JobDescription(*20171128*),PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,(*UserArgName,UserArgValue,*)
XQfigureXrange,XQfigureYrange,ColorPaletterange(*20171128*),Hist1figureNbin,(*Hist1figureXrange,(*Hist1figureYrange*)dummy12,*)
(*ColorSeperator,*)
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}=
(*readcorrconfigfile4*)readcorrconfigfile6[configDir,configfilename];
Print["input arguments: ",(*readcorrconfigfile4*)readcorrconfigfile6[configDir,configfilename] ];
Print[""];(*space*)


(*check format of arguments*)
(*xyrange*)
If[XQfigureXrange[[1]]=="auto",XQfigureXrange[[1]]=10^-6];
If[XQfigureXrange[[2]]=="auto",XQfigureXrange[[2]]=1];
If[XQfigureYrange[[1]]=="auto",XQfigureYrange[[1]]=1.3];
If[XQfigureYrange[[2]]=="auto",XQfigureYrange[[2]]=1200];

(*If[Hist1figureNbin\[Equal]"auto",Hist1figureNbin=5];*)
(*
If[Hist1figureXrange[[1]]\[Equal]"auto",Hist1figureXrange[[1]]=10^-6];
If[Hist1figureXrange[[2]]\[Equal]"auto",Hist1figureXrange[[2]]=1];
If[Hist1figureYrange[[1]]\[Equal]"auto",Hist1figureYrange[[1]]=1.3];
If[Hist1figureYrange[[2]]\[Equal]"auto",Hist1figureYrange[[2]]=1200];
*)

(*
(*bar seperator input has only 3 elements*)
If[Length[ColorSeperator]\[NotEqual]3,Print["color seperator percentage should be three numbers"];Abort[]];
(*should be small to large, ex: 30, 50, 70*)
If[Sort[ColorSeperator]\[NotEqual]ColorSeperator,Print["color seperator percentage should from small to large"];Abort[]];
*)

(*should in 0% to 100%, ex: 35,55,77.5; 35,76,140.5 is illegal*)

(*size: if highlight mode, set size as small, can't set here, need to set when reading highlight mode of a figure type*)

(*for PDFname, test whether it is in quickdata*)
(*
If[
PDFname\[Equal]"CT14NNLO",
(*printprocessbyPDFsetquicksavemode[Table[quickdatacorr[[iexpt,1]][["exptinfo","exptid"]],{iexpt,1,Length[quickdatacorr]}],datalistFile]*)"dummy",
(*Print["error, the PDFset is not CT14NNLO"];Exit[];*)"dummy"(*20170426: begin to input other PDFsets*)
];
*)

(*data information (files, directory, etc) of figures that will be plotted*)
(*read plotdata_config.txt to get arguments of plot data setting*)
plotdataconfigfilename="plotdata_config.txt";
{datalistFile,(*FxQGridDir*)dummy1,(*FxQGridFile*)dummy2,(*FxQSameptDir*)dummy3,(*FxQSameptFile*)dummy4,CorrDataDir,CorrDataFile,(*GridNx*)dummy5,(*GridNQ*)dummy6}=
readplotdataconfigfile[configDir,plotdataconfigfilename];
Print["input arguments for read plot data: ",readplotdataconfigfile[configDir,plotdataconfigfilename] ];
(*if path setting is default, set it as in quick_data directory*)
quickdataDir="../quick_data/";
If[datalistFile=="default",datalistFile="./dat16lisformathematica"];
(*
If[FxQGridDir\[Equal]"default",FxQGridDir=quickdataDir];
If[FxQGridFile\[Equal]"default",FxQGridFile="fxQ_grid_"<>PDFname<>"_x"<>ToString[GridNx]<>"_Q"<>ToString[GridNQ]<>".dat"];

If[FxQSameptDir\[Equal]"default",FxQSameptDir=quickdataDir];
If[FxQSameptFile\[Equal]"default",FxQSameptFile="fxQ_samept_"<>PDFname<>".dat"];
*)
If[CorrDataDir=="default",CorrDataDir=quickdataDir];
If[CorrDataFile=="default",
If[PDFxQSelectMethod=="samept",CorrDataFile="samept_data_"<>PDFname<>".dat"];
If[PDFxQSelectMethod=="grid",CorrDataFile="grid_data_"<>PDFname<>"_x"<>ToString[GridNx]<>"_Q"<>ToString[GridNQ]<>".dat"];
If[PDFxQSelectMethod=="xgrid",CorrDataFile="xgrid_data_"<>PDFname<>"_x"<>ToString[GridNx]<>".dat"];

If[PDFxQSelectMethod=="sameptgrid",
CorrDataFileSamept="samept_data_"<>PDFname<>".dat";CorrDataFileGrid="xgrid_data_"<>PDFname<>"_x"<>ToString[GridNx]<>".dat"];
"dummy"
];

(*for arguments which we don't want not show on web version, we setup them is web version mathematica as internal variable*)
(*
datalistFile="./dat16lisformathematica";
expttype="multi";
*)
exptid;(*it will equal to exptlist*)
figureDir="../plots/";

(*read data from data package*)
(*decide which data file to read bassed on PDFname*)
(*
quickdataDir="../quick_data/";
CorrDataDir=quickdataDir;
*)
(*correlationdatapackage=quickdataDir<>PDFname<>"_correlation_data.dat";*)
(*Nx=80;NQ=25;*)
(*20171101
Print["file status: ",FileExistsQ[CorrDataDir<>"corr_samept_data_"<>PDFname<>".dat"] ];
*)
Print["present directory:\n",Directory[]];
Print["quick correlation data:\n",FileNames[CorrDataDir<>"*samept_data_"<>PDFname<>"*"] ];
Print[""];(*space*)
(*
If[
FileExistsQ[correlationdatapackage]\[Equal]True,
(*<<correlationdatapackage*)Get[correlationdatapackage];Print["loading quick correlation data..."],
Print["error, the correlation data of PDFname ",PDFname,"does not exist in quick database"];Abort[]
];
*)


(*20170226: for quick save mode, correlation data are loaded from a .m package, so reading experimental id from .dta files are replaced by reading package*)
(*myPDFsetdtafile=FileNames[myPDFsetDir<>"*dta"][[1]];*)


(*set dta Dir you want to read data, it's the PDFset Dir you choose*)
(*20170226: don't need to load dta file for expt information and data*)
(*DtaDir=myPDFsetDir;*)
(*experiments you choose*)
(*
processes={PDISNC,NDISCC,PDISNCCC,PVBPZ,PVBPW,PJP,hDISNC,hDISCC,hVBPZ};
exptlistAll=processes//Flatten;
exptlistProtonNeutron={PDISNC,NDISCC,PDISNCCC,PVBPZ,PVBPW,PJP}//Flatten;
*)

(*20170301: exptid read by new config*)
(*exptlist=exptlistAll;*)
Print["read experimental id"];
exptlist={};
(*check argument inputs are correct*)
Nexpt1=Length[ExptidType];
Nexpt2=Length[ExptidFlag];
If[Nexpt1!=Nexpt2,Print["configure file error, # of exptid is different with # of exptidflag"];Abort[] ];

Table[
If[ExptidFlag[[iexpt]]!=0 &&ExptidFlag[[iexpt]]!=1,Print["configure file error, exptidflag is not 1 or 0"];Abort[] ]
,{iexpt,1,Length[ExptidType]}
];
(*it flag is on, add that exptid into exptlist*)(*20170410: exptlist doesn't mean the expts of final made figure because quick_data does not must have all expts*)
Table[
If[ExptidFlag[[iexpt]]==1,exptlist=Append[exptlist,ExptidType[[iexpt]] ] ],
{iexpt,1,Length[ExptidType]}
];
(*if version 2 (LoopExptBool==True;), select only n-th expt in the loop*)
If[LoopExptBool==True,exptlist={exptlist[[irun]]}];
(*20170301: exptid = exptlist*)
exptid = exptlist;

Print["selected Expt IDs = ",exptid];
Print[""];(*space*)




(*******************RUN*************************)
(*read experiments info (expt names)*)
(*lisTable=*)ReadLisFile[datalistFile];
(*correlation, dr*correlation, deltaR calculation *)

(*
(*calculate correlation*)
corrfxQdtaobsclass=quickdatacorr;
(*get some data from package*)
residualclass="unset";
theoryerrorclass="unset";
expterrorclass"unset";
If[FigureFlag[[2]]\[Equal]1,expterrorclass=quickdataexpterror];
If[FigureFlag[[3]]\[Equal]1 || CorrelationArgFlag[[-1]]\[Equal]1,
residualclass=quickdataresidual;
(*check user define mode has Nset # of value*)
If[
Datamethods[["getNcolumn"]][residualclass[[1]] ]\[Equal]Length[UserArgValue]+2,
Print["#user value is correct, Nset = ",Length[UserArgValue]],
Print["error, #user value is different, it should be ",Datamethods[["getNcolumn"]][residualclass[[1]] ]-2]
];
"dummy"
];
*)

(*read data from quick_data directory*)
(*export into files*)
(*save f(x, Q) grid into .dat file*)
corrdataclass;
{expterrordataclass,residualdataclass,dRdataclass};
dRcorrdataclass;

(*
CorrDataDir="default";CorrDataFile="default";
(*(x,Q) points sets as position at max of corr*)
If[CorrDataDir\[Equal]"default",CorrDataDir=quickdataDir];
If[CorrDataFile\[Equal]"default",CorrDataFile="samept_data_"<>PDFname<>".dat"];
*)
Print["filenames of plotted data:"];
Print["Directory: ",CorrDataDir];
(*20171101 
Print["corrdataclass: ","corr_"<>CorrDataFile,"\nexpterrordataclass: ","expterror_"<>CorrDataFile,"\nresidualdataclass: ","residual_"<>CorrDataFile,"\ndRdataclass: ","dR_"<>CorrDataFile,"\ndRcorrdataclass: ","dRcorr_"<>CorrDataFile,"\nresidualNsetdataclass: ","residualNset_"<>CorrDataFile,"\n"];
*)
(*20171101 change  data format*)
Print["residualNsetclass: ","residualNset_"<>CorrDataFile,"\nfxQsamept2class: ","fxQNset_"<>CorrDataFile,"\ndtacentralclass: ","dtacentral_"<>CorrDataFile,"\n"];
(*20171124 check whether plotted data files exist*)
If[FileExistsQ[(CorrDataDir<>"residualNset_"<>CorrDataFile)]==False,Print["error, file ",(CorrDataDir<>"residualNset_"<>CorrDataFile)," doesn't exist"];Abort[] ];
If[FileExistsQ[(CorrDataDir<>"fxQNset_"<>CorrDataFile)]==False,Print["error, file ",(CorrDataDir<>"fxQNset_"<>CorrDataFile)," doesn't exist"];Abort[] ];
If[FileExistsQ[(CorrDataDir<>"dtacentral_"<>CorrDataFile)]==False,Print["error, file ",(CorrDataDir<>"dtacentral_"<>CorrDataFile)," doesn't exist"];Abort[] ];

(*read correlation data of grid*)
(*20171101: change data file format: fxQ Nset: [[iexpt,iflavour]], [["data"]]=LF[x,Q,fSubscript[(x,Q), 1],...,fSubscript[(x,Q), N]], residual Nset: [[iexpt]], LF[x,Q,Subscript[r, 1],...,Subscript[r, N]], dtacentralclass: [[iexpt]], LF[Subscript[val, 1],Subscript[val, 2],...Subscript[val, l],x,Q]
corrdataclass=Import[CorrDataDir<>"corr_"<>CorrDataFile,"ExpressionML"];
expterrordataclass=Import[CorrDataDir<>"expterror_"<>CorrDataFile,"ExpressionML"];
residualdataclass=Import[CorrDataDir<>"residual_"<>CorrDataFile,"ExpressionML"];
dRdataclass=Import[CorrDataDir<>"dR_"<>CorrDataFile,"ExpressionML"];
*)
(*read dR*correlation data of grid*)
(*
dRcorrdataclass=Import[CorrDataDir<>"dRcorr_"<>CorrDataFile,"ExpressionML"];
residualNsetdataclass=Import[CorrDataDir<>"residualNset_"<>CorrDataFile,"ExpressionML"];

corrfxQdtaobsclass>>(CorrDataDir<>"corr_"<>CorrDataFile);
dRcorrfxQdtaobsclass>>(CorrDataDir<>"dRcorr_"<>CorrDataFile);
expterrorclass>>(CorrDataDir<>"expterror_"<>CorrDataFile);
residualclass>>(CorrDataDir<>"residual_"<>CorrDataFile);
deltaRclass>>(CorrDataDir<>"dR_"<>CorrDataFile);
*)
{residualNsetclass,fxQsamept2class,dtacentralclass};
residualNsetclass=Get[(CorrDataDir<>"residualNset_"<>CorrDataFile)];
(*save Nset of fxQ*)
fxQsamept2class=Get[(CorrDataDir<>"fxQNset_"<>CorrDataFile)];
dtacentralclass=Get[(CorrDataDir<>"dtacentral_"<>CorrDataFile)];


(*set fmax*)
(*
fmax=Length[corrdataclass[[1]] ];
*)
fmax=Length[fxQsamept2class[[1]] ];

Print["Dimensions of plotted data"];
Print[
(*20171101 change  data format
"corrdataclass: ",
Dimensions[corrdataclass],
"dRcorrdataclass: ",
Dimensions[dRcorrdataclass],
"{expterrordataclass,residualdataclass,dRdataclass}: ",
Dimensions[#]&/@{expterrordataclass,residualdataclass,dRdataclass},
*)
"{residual Nset class}: ",
Dimensions[#]&/@{residualNsetclass},
"{fxQ samept class}: ",
Dimensions[#]&/@{fxQsamept2class},
"{dta central class}: ",
Dimensions[#]&/@{dtacentralclass}
];
Print[""];(*space*)

(*20171127 for fraction number input, transfer them to numerical number*)
ToNumericTime=
AbsoluteTiming[
Table[
fxQsamept2class[[iexpt,iflavour]][["data"]]=fxQsamept2class[[iexpt,iflavour]][["data"]]/.LF[a__]:>LF@@(N[{a}]),
{iexpt,Length[fxQsamept2class]},{iflavour,Length[fxQsamept2class[[1]] ]}
];
Table[
residualNsetclass[[iexpt]][["data"]]=residualNsetclass[[iexpt]][["data"]]/.LF[a__]:>LF@@(N[{a}]),
{iexpt,Length[fxQsamept2class]}
];
"dummy"
];
Print["fraction numbers in data transfer to numerical numbers, time = ",ToNumericTime," seconds"];
Print[""];

(*set expeiments by selecting expt data whose ID are in the config1.txt into corrdataclassfinal, ...*)
residualNsetclassfinal={};
fxQsamept2classfinal={};
dtacentralclassfinal={};


expttakeindex={};
exptlistfinal={};
logfile="error_massage.log";

(*make index of data with the same expt id as exptlist*)
Table[
If[
residualNsetclass[[iexpt]][["exptinfo","exptid"]]==exptlist[[iexptlist]],
expttakeindex=Append[expttakeindex,iexpt];
(*make exptlistfinal*)
exptlistfinal=Append[exptlistfinal,exptlist[[iexptlist]] ];
];
"dummy"
,{iexptlist,1,Length[exptlist]},{iexpt,1,Length[residualNsetclass]}
];
expttakeindex;
(*print error message for exptids exptlist don't appear in database*)
exptlistnotfound=Complement[exptlist,exptlistfinal];
If[Length[exptlistnotfound]!=0,Print["the exptid = ",exptlistnotfound," are not found in database"]];

(*pick the data of experiments for plot *)
residualNsetclassfinal=
Table[
residualNsetclass[[expttakeindex[[iexpttakeindex]] ]],
{iexpttakeindex,1,Length[expttakeindex]}
];

fxQsamept2classfinal=
Table[
fxQsamept2class[[expttakeindex[[iexpttakeindex]] ]],
{iexpttakeindex,1,Length[expttakeindex]}
];

dtacentralclassfinal=
Table[
dtacentralclass[[expttakeindex[[iexpttakeindex]] ]],
{iexpttakeindex,1,Length[expttakeindex]}
];

Print["check whether the selected expts are also in the plotted data files"];
Print["expt list: ",exptlist];
Print["expt list in the ./quick_data: ",exptlistfinal];
(*for run_loopexpts_v4.nb: users want to generate figures of all Expt ID in the config1.txt, so we don't want the program breaks down when some Expt IDs in config.txt are not in data files*)
(*when we find exptlistfinal contains no expt ID, we skip this loop*)
If[Length[exptlistfinal]==0,Print["All Expt IDs in ",configfilename," are not in data files"];Return["end Quicksaveplot"] ];
Print[""];(*space*)


(*test corrfxQdtaobsclassfinal*)
Print["Dimensions of data of selected and available expts"];
Print[
(*
"corrdataclass: ",
Dimensions[corrdataclass],
"dRcorrdataclass: ",
Dimensions[dRcorrdataclass],
"{expterrordataclass,residualdataclass,dRdataclass}: ",
Dimensions[#]&/@{expterrordataclass,residualdataclass,dRdataclass},
*)
"{residualNsetclassfinal}: ",
residualNsetclassfinal//Dimensions,
"{fxQsamept2classfinal}: ",
fxQsamept2classfinal//Dimensions,
"{dtacentralclassfinal}: ",
dtacentralclassfinal//Dimensions
];
Print[""];(*space*)


(*20191119 need all expt fxQ info*)
fxQDatabaselist=Table[fxQsamept2class[[iexpt,iflavour]][["data"]],{iexpt,Dimensions[fxQsamept2class][[1]]},{iflavour,Dimensions[fxQsamept2class][[2]]}];
fxQDatabaseExptlist=Table[fxQsamept2class[[iexpt,1]][["exptinfo","exptid"]],{iexpt,Dimensions[fxQsamept2class][[1]]}];

(*clear residualNsetclass, fxQsamept2class, dtacentralclass*)
Clear[residualNsetclass];Clear[fxQsamept2class];Clear[dtacentralclass];

(*20171217 define global variable for username*)
UserNameGlobal={};
(*append user defined values into f(x,Q) as new flavour index*)
If[
CorrelationArgFlag[[-1]]==1,
(*setup pdf function, so that hessian method function could work*)
(*
pdfResetCTEQ;
(*generate a pdf space*)
pdfFamilyParseCTEQ["Dummy"];
ifamily=1; 
(* IniDir="//users//nadolsky//share//lhapdf//6.1.5//share/LHAPDF//CT14nnlo//pds//"; *)
PdsDir=
pdfFamilyParseCTEQ["../fakePDFset/"<>PDFname<>"/"<>"*pds",ifamily];
*)

(*20171116: for new convention of user define function: read it's library first, then define global variable for List data of f(x,Q)*)
(*Get["user_define_function.m"];*)
fxQlist=Table[fxQsamept2classfinal[[iexpt,iflavour]][["data"]],{iexpt,Dimensions[fxQsamept2classfinal][[1]]},{iflavour,Dimensions[fxQsamept2classfinal][[2]]}];
(*20171109: seperate user difine function/data IO and configure file*)
userfuncfilename="user_func.txt";
Print["reading user functions from ",userfuncfilename];
(*20171119 new user function format: {{user name 1, user function 1}, {user name 2, user function 2}...}*)
(*{UserArgName,UserArgFunction}*)
UserArgFunction=(*ReadUserFunctionV2*)ReadUserFunctionV3[configDir,userfuncfilename];
UserArgName=(#[[1]]&/@UserArgFunction);
UserArgFunction=(#[[2]]&/@UserArgFunction);
(*20171217 define global variable for username*)
UserNameGlobal=UserArgName;

Print["function names: ",UserArgName];
(*20171116: new convention of user define function and new way to add it as new flavour*)
(*20171119 new user function format: {{user name 1, user function 1}, {user name 2, user function 2}...}*)
(*fxQdataNewFlavour format: [[ifunc,iexpt]]*)
(*new flavour*)
fxQdataNewFlavour=Table[UDFToClass[UserArgFunction[[ifunc]],fxQsamept2classfinal],{ifunc,Length[UserArgFunction]}];
(*add new flavour to fxQ class for each expt ID*)
Table[
fxQsamept2classfinal[[iexpt]]=Append[fxQsamept2classfinal[[iexpt]],fxQdataNewFlavour[[ifunc,iexpt]] ];
"dummy"
,{iexpt,Length[fxQsamept2classfinal]},{ifunc,Length[UserArgFunction]}
];

(*20171119 bebause CorrelationArgFlag is flavour index, there should be # of user function indece for all user functions, 
# of CorrelationArgFlag should be Nflavour + # of user functions *)
CorrelationArgFlag=Join[CorrelationArgFlag,Table[CorrelationArgFlag[[-1]],{ifunc,Length[UserArgFunction]-1}] ];
"dummy"
(*
fxQsamept2classfinal=Append[fxQsamept2classfinal[[#]],fxQdataNewFlavour[[#]] ]&/@Range[fxQsamept2classfinal//Length];
*)
(*
(*check the # of user define values is the same as # of PDF replicas*)
If[
Length[UserArgValue]\[NotEqual](Datamethods[["getNcolumn"]][fxQsamept2classfinal[[1,1]] ]-2),
Print["error, the # of user-defined values should be the same as # of PDF relicas (Nset)"];
Print["Nset = ",(Datamethods[["getNcolumn"]][fxQsamept2classfinal[[1,1]] ]-2) ];
Print["#user-defined values = ", Length[UserArgValue] ];
Abort[] 
];
(*append data of uservalue to fxQsamept2classfinal for each expt ID*)
Table[
tmpclass=fxQsamept2classfinal[[iexpt,1]];
tmpclass[["data"]]=tmpclass[["data"]]/.LF[a__]\[RuleDelayed]LF[{a}[[1]],{a}[[2]],Sequence@@UserArgValue];
fxQsamept2classfinal[[iexpt]]=Append[fxQsamept2classfinal[[iexpt]],tmpclass];
tmpclass,
{iexpt,1,Length[fxQsamept2classfinal]}
];
*)
];


fmax=Length[fxQsamept2classfinal[[1]] ];
Print["total #flavours: ",fmax];
Print[""];(*space*)

(*20171217*)
(*if any values of points for each flavour are 0, Indeterminate, or ComplexInfinity, give values in all replicas of that point 0*)
(*this step should be prior to the calculation of observables*)
Print["search broken points (0, ComplexInfinity, Indeterminate in any replica) for all points of f(x,Q), then set the values of broken points as 0"];
Print[""];
Table[
fxQsamept2classfinal[[iexpt,flavour+6]][["data"]]=
fxQsamept2classfinal[[iexpt,flavour+6]][["data"]]/.LF[a__]:>
If[
IntersectingQ[{a},{0,0.0,ComplexInfinity,Indeterminate}],
LF[{a}[[1]],{a}[[2]],Sequence@@Table[0.0,{iset,Length[{a}]-2}] ],
LF[a] 
];
"dummy"
,{iexpt,Length[fxQsamept2classfinal]},{flavour,-5,-5+fmax-1}
];


(*test *)(*Print["flavours switch: ",CorrelationArgFlag];Abort[];*)

(*calculate observables for plots from PDF data and residual data of all replicas*)
(*observables: correlation: corr(Subscript[r, i],f(x,\[Mu])), sensitivity: Subscript[\[Delta]r, i]*corr(Subscript[r, i],f(x,\[Mu])), central values of residuals: Subscript[r, i], uncertainties of residuals: Subscript[\[Delta]r, i], expt error ratio: Subscript[\[Sigma], i]/Subscript[D, i]*)
corrdataclassfinal;
dRcorrdataclassfinal;
{expterrordataclassfinal,residualdataclassfinal,dRdataclassfinal};
(*calculate correlation*)
ti=AbsoluteTime[];
corrdataclassfinal=
Table[
corrfxQdtaobs[residualNsetclassfinal[[iexpt]],fxQsamept2classfinal[[iexpt,flavour+6]] ],
{iexpt,1,Length[residualNsetclassfinal]},{flavour,-5,-5+fmax-1}
];

(*calculate dr*correlation*)
dRcorrdataclassfinal=
Table[
dRcorrfxQdtaobs[residualNsetclassfinal[[iexpt]],fxQsamept2classfinal[[iexpt,flavour+6]] ],
{iexpt,1,Length[residualNsetclassfinal]},{flavour,-5,-5+fmax-1}
];

(*calculate dr*)
dRdataclassfinal=
Table[
getdeltaRclass[residualNsetclassfinal[[iexpt]] ],
{iexpt,1,Length[residualNsetclassfinal]}
];

(*20171213 redefine sensitivity as (\[Delta]r/r)*Corr(Subscript[r, i],fSubscript[(x,Q), i])*)
Table[
Npt=dRcorrdataclassfinal[[iexpt,flavour+6]][["data"]]//Length;
(*
(*calculate the (Subscript[(\[Chi]^2), ID]/Subscript[Npt, ID])^0.5*)
iRawdataResidual=Position[dtacentralclassfinal[[iexpt]][["label"]],"ReducedChi2"][[1,1]];
RawNpt=dtacentralclassfinal[[iexpt]][["data"]]//Length;
TotalRawChi2=Sum[Abs[dtacentralclassfinal[[iexpt]][["data"]][[irawpt,iRawdataResidual]] ],{irawpt,RawNpt}];
SensNormalize=(TotalRawChi2/RawNpt)^0.5;
*)
(*20180420: replace the calculation of the rms of residual by the function*)
SensNormalize=GetResidualRMS[residualNsetclassfinal[[iexpt]],dtacentralclassfinal[[iexpt]] ];

Table[
dRcorrdataclassfinal[[iexpt,flavour+6]][["data"]][[ipt,3]]=(dRcorrdataclassfinal[[iexpt,flavour+6]][["data"]][[ipt,3]]/
(*prevent the dr/r blow up when r = 0*)
(*If[residualdataclassfinal[[iexpt]][["data"]][[ipt,3]]\[NotEqual]0.0,residualdataclassfinal[[iexpt]][["data"]][[ipt,3]],10.0^-8])*)
(*total chi^2 for the experiment*)
(SensNormalize)
);
"dummy",
{ipt,Npt}
] ;
"dummy",
{iexpt,1,Length[residualNsetclassfinal]},{flavour,-5,-5+fmax-1}
];

(*calculate central value of residual*)
(*select x, Q, central value of residual (iset==1)*)
residualdataclassfinal=
Table[
Datamethods[["take"]][residualNsetclassfinal[[iexpt]],{1,3}],
{iexpt,1,Length[residualNsetclassfinal]}
];

(*test print normalization for residuals*)
(*
NormalizeIDs=
Table[
(*calculate the (Subscript[(\[Chi]^2), ID]/Subscript[Npt, ID])^0.5*)
iRawdataResidual=Position[dtacentralclassfinal[[iexpt]][["label"]],"ReducedChi2"][[1,1]];
RawNpt=dtacentralclassfinal[[iexpt]][["data"]]//Length;
TotalRawChi2=Sum[Abs[dtacentralclassfinal[[iexpt]][["data"]][[irawpt,iRawdataResidual]] ],{irawpt,RawNpt}];
SensNormalize=(TotalRawChi2/RawNpt)^0.5;
SensNormalize,
{iexpt,1,Length[residualNsetclassfinal]}
];
Print[
Table[
{residualNsetclassfinal[[iexpt]][["exptinfo","exptid"]],NormalizeIDs[[iexpt]]},
{iexpt,1,Length[residualNsetclassfinal]}
]
];
*)


(*test the time of the correlation value calculation*)
tf=AbsoluteTime[];
GetCorrvalueTime=tf-ti;
NCorrval=
Sum[
Datamethods[["getNpt"]][corrdataclassfinal[[iexpt,flavour+6]] ],
{iexpt,1,Length[residualNsetclassfinal]},{flavour,-5,-5+fmax-1}
];
Print["The time for calculating statistical quantities used in plotting"];
Print["total number of correlation & sensitivity value calculations is ",NCorrval];
Print["average time of one correlation + sensitivity value calculations is roughly ",GetCorrvalueTime/NCorrval];
Print["time of calculating all observable data (correlation + sensitivity + ...)  values is ",GetCorrvalueTime];
Print[""];(*space*)

(*calculate experimental error ratio*)
(*check columns used to extract Subscript[\[Sigma], i]/Subscript[D, i] is correct (check the labels are (x,Q,TotErr/Exp) ) *)
(*check whether the elements used to calculate Expt error/expt are correct*)
ExpIndex=4;ExpErrIndex=6;XIndex=-3;QIndex=-2;
Print["begin to calculate \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)/\!\(\*SubscriptBox[\(D\), \(i\)]\) from data, labels of indexes used for each experiment:"];
Print[""];(*space*)

Table[
Print[
dtacentralclassfinal[[iexpt]][["exptinfo","exptid"]],": (",dtacentralclassfinal[[iexpt]][["label"]][[XIndex]],", ",dtacentralclassfinal[[iexpt]][["label"]][[QIndex]],", ",dtacentralclassfinal[[iexpt]][["label"]][[ExpErrIndex]],"/", dtacentralclassfinal[[iexpt]][["label"]][[ExpIndex]]," )"
],
{iexpt,1,Length[residualNsetclassfinal]} 
];

(*extract (x, Q, ExpErrIndex/ExpIndex)*)

expterrordataclassfinal=
Table[
tmpdataclass=dtacentralclassfinal[[iexpt]];
tmpdataclass[["data"]]=tmpdataclass[["data"]]/.LF[a__]:>LF[{a}[[-3]],{a}[[-2]],{a}[[ExpErrIndex]]/{a}[[ExpIndex]] ];
tmpdataclass[["label"]]={"x","Q","expterror/exptcentral"};
tmpdataclass,
{iexpt,1,Length[residualNsetclassfinal]}
];

(*print dimensions of calculated observables*)
Print["data of final expts"];
Print[
"corrdataclass: ",
Dimensions[corrdataclassfinal],
"dRcorrdataclass: ",
Dimensions[dRcorrdataclassfinal],
"{expterrordataclass,residualdataclass,dRdataclass}: ",
Dimensions[#]&/@{expterrordataclassfinal,residualdataclassfinal,dRdataclassfinal}
];
Print[""];(*space*)
(**)
(*test abort*)
(*
Abort[];
*)


(*test plot2: bug comes from plotcorrelation7, there is a global various p2 cover the used variable*)
(*
plot2=processdataplotsmultiexp5percentage[(*{expterrorclassfinal[[1]],expterrorclassfinal[[2]],expterrorclassfinal[[3]],expterrorclassfinal[[4]],expterrorclassfinal[[5]]}*)expterrorclassfinal,readcorrconfigfile4[configDir,configfilename],2,-5 ];
Print[plot2];
*)

(*save to eps, png and pdf file*)
(**********************)
(*save figure files into saveparentpath<>pdfnameexpttypeDir<>exptidDir or saveparentpath<>jobpath*)
(*set dir for saved figures*)
saveparentpath=figureDir;(*"/home/botingw/code/pdf_correlation/code/mathematica/"*)
(*make name of subdir(s)*)
(*pdfnameexpttypeDir=PDFname<>"_"<>expttype<>"/";*)
(*20170313: use job dir*)
jobpath="Jobs/"<>ToString[Jobid]<>"/";
(*if version 2 (LoopExptBool==True;), put the produced figures under the folder with it's name=Expt ID*)
exptidDir=Table[ToString[exptid[[iexpt]] ]<>"_",{iexpt,1,Length[exptid]}];
exptidDir=StringJoin[exptidDir];
exptidDir=StringDrop[exptidDir,-1]<>"/";
If[LoopExptBool==True,jobpath=jobpath<>exptidDir];

(*if expttype = single,multi, set exptidDir as exptid: id1_id2_...*)
(*
exptidDir=Table[ToString[exptid[[iexpt]] ]<>"_",{iexpt,1,Length[exptid]}];
exptidDir=StringJoin[exptidDir];
exptidDir=StringDrop[exptidDir,-1]<>"/";
*)

(*set observables, different kinds of plot, available extension of output files *)
obsname={"xQbyexpt","expt_error_ratio","residual","dr","corrdr","corr"};
representationname={"xQ-1","xQ+1","hist-1","hist+1","legend","xQ","info-1","info+1"};(*20171201 change filename format*)
extensionname={".eps",".png",".pdf",".jpg"};

(*if directory does not exist, create it*)
If[
DirectoryQ[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath]==False,
CreateDirectory[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath];
"dummy"
];

(*20171119: select export figure format by Branch Mode*)
iext=Switch[BranchMode,1,{1},2,{2,3},3,{4},_,Print["error, BranchMode should be 1, 2, 3"];Abort[] ];
(*iext=2;*)(*export eps*)(*20171105: use .png for python script interface version*)
imgresol=144;(*image resolution*)

(*make exptname table jpg*)
(*input a List of string (exptnames) and #row for every column, output a Grid of string with #row*#column *)
(*
makeGrid[strin_,rowsin_]:=
Module[{str=strin,rows=rowsin,columns,
lastcolstr,strout},
columns=Quotient[Length[str],rows];
strout=Table[str[[ic*rows+ir]],{ic,0,columns-1},{ir,1,rows}];
lastcolstr=Table[str[[i]],{i,columns*rows+1,Length[str]}];
strout=Append[strout,lastcolstr];
strout=Insert[strout,{"Data Expts"},1];
Grid[strout,ItemStyle\[Rule]Directive[FontSize\[Rule]18,Black],ItemSize\[Rule]12,Alignment\[Rule]Left]
];
*)


(*test*)
(*
makeGrid2[strin_,rowsin_,titlein_]:=
Module[{str=strin,rows=rowsin,title=titlein,columns,
lastcolstr,strout},
columns=Quotient[Length[str],rows];
strout=Table[str[[ic*rows+ir]],{ic,0,columns-1},{ir,1,rows}];
lastcolstr=Table[str[[i]],{i,columns*rows+1,Length[str]}];
strout=Append[strout,lastcolstr];
strout=Insert[strout,{Text[Style[title,FontSize\[Rule]24] ],SpanFromLeft},1];
strout=Insert[strout,{"Data Expts"},2];
Grid[strout,ItemStyle\[Rule]Directive[FontSize\[Rule]18,Black],ItemSize\[Rule]14,Alignment\[Rule]Left]
];
*)

(*
rows=3;
filename="exptname_table2"<>extensionname[[iext]];
exptnames=Table[ExptIDtoName[exptlist[[iexpt]] ]<>"("<>ToString[exptlist[[iexpt]] ]<>")",{iexpt,1,Length[exptlist]}];
Print["making table of experiments included in plots"];
exptnamestable=makeGrid2[exptnames,rows,"PDF error dr for residual, CT14NNLO\n\n"];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,exptnamestable];
*)
(*
flavour=0;
iext=2;
imgresol=72;
If[
(*correlation plots*)
FigureFlag[[6]]\[Equal]1,
Print["making plot of figure type ",FigureType[[6]],", flavour = ",flavour];
p6=processdataplotsmultiexp5percentage[corrfxQdtaobsclassfinal,readcorrconfigfile4[configDir,configfilename],6,flavour ];
(*add exptname table into output figure*)
(*p6[[1]]=Append[p6[[1]],exptnamestable];*)
(*
Print[p6];
Print[Grid[p6] ];
Print[GraphicsGrid[p6] ];
*)
p6=GraphicsGrid[p6,Spacings\[Rule]Scaled[.15] ];
filename=obsname[[6]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>extensionname[[1]];
t1=AbsoluteTiming[
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6,ImageResolution\[Rule]imgresol(*,ImageSize\[Rule]{1200,1200}*)];
];
filename=obsname[[6]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>extensionname[[2]];
t2=AbsoluteTiming[
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6,ImageResolution\[Rule]imgresol(*,ImageSize\[Rule]{1200,1200}*)];
];
filename=obsname[[6]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>extensionname[[3]];
t3=AbsoluteTiming[
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6,ImageResolution\[Rule]imgresol(*,ImageSize\[Rule]{1200,1200}*)];
];
filename=obsname[[6]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>extensionname[[4]];
t4=AbsoluteTiming[
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6,ImageResolution\[Rule]imgresol(*,ImageSize\[Rule]{1200,1200}*)];
];

Print["t1 t2 t3 t4 = ",{t1,t2,t3,t4}];
];
Abort[];
*)

(*test*)
(*
flavour=0;
p6=processdataplotsmultiexp5percentage[corrdataclassfinal,readcorrconfigfile4[configDir,configfilename],6,flavour ];
Print[p6];
*)

corrdataclassfinal;
dRcorrdataclassfinal;
{expterrordataclassfinal,residualdataclassfinal,dRdataclassfinal};
residualNsetdataclassfinal;


(*20171202 for plot type = 5 or 6, write data info into files, if flavour flag = 1, record the info*)
If[
FigureFlag[[6]]==1 || FigureFlag[[6]]==-1,
(*filename*)
filename=
Switch[
FigureFlag[[6]],
-1,
obsname[[6]]<>"_"<>representationname[[7]]<>"_"<>"_samept_info"<>".txt",
1,
obsname[[6]]<>"_"<>representationname[[8]]<>"_"<>"_samept_info"<>".txt"
];
(*info string*)
datainfostr=
Table[
If[
CorrelationArgFlag[[flavour+6]]==1,
datainfototext[{corrdataclassfinal},{dtacentralclassfinal},readcorrconfigfile6[configDir,configfilename],6,flavour],
""
],
{flavour,-5,-5+fmax-1}
];
(*export the info file*)
(*20171202 add important data info*)
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,datainfostr//StringJoin  ];

"dummy"
];

If[
FigureFlag[[5]]==1 || FigureFlag[[5]]==-1,
(*filename*)
filename=
Switch[
FigureFlag[[5]],
-1,
obsname[[5]]<>"_"<>representationname[[7]]<>"_"<>"_samept_info"<>".txt",
1,
obsname[[5]]<>"_"<>representationname[[8]]<>"_"<>"_samept_info"<>".txt"
];
(*info string*)
datainfostr=
Table[
If[
CorrelationArgFlag[[flavour+6]]==1,
datainfototext[{dRcorrdataclassfinal},{dtacentralclassfinal},readcorrconfigfile6[configDir,configfilename],5,flavour],
""
],
{flavour,-5,-5+fmax-1}
];
(*export the info file*)
(*20171202 add important data info*)
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,datainfostr//StringJoin  ];

"dummy"
];

(*make all figuretype plots 
input {dataclass}, figuretype(in config.txt) of that data, config file arguments, flavour into processdataplotsmultiexp6percentage
output plots (histogram, data on(x,mu) plane, data info )
figure types: 1:data plots,2:expt error,3:residual,4:"residual error" deltaR_i,5:"sensitivity factor" deltaR_i*Corr(r_i,F),6:"correlation" Corr(r_i,F)
the input format of dataclasses are the same format of the dataclasses in data files:
"corr":[[iexpt,iflavour]]
"dRcorr":[[iexpt,iflavour]]
" dR":[[iflavour]]
"residual":[[iflavour]]
"residualNset":[[iflavour]]
"expterror":[[iflavour]]
*)
(*20171202 save all figures with the same plot type into one .m file*)
p5expression={};p6expression={};

Print["generating figures..."];
jpgtime=
AbsoluteTiming[
(*20171109 use processdataplotsmultiexp7percentage to replace version 6 and readcorrconfigfile5 to replace version4: for new highlight range convention*)
(*20171201: CorrelationArgFlag[[iplottype]]\[Equal]1, draw |data| (absolute value), CorrelationArgFlag[[iplottype]]\[Equal]-1, draw data (sign data)*)
Table[
Print["now flavour = ",flavour];
If[
CorrelationArgFlag[[flavour+6]]==1,
If[
(*correlation plots*)
FigureFlag[[6]]==1 || FigureFlag[[6]]==-1,
Print["making plot of figure type ",FigureType[[6]],", flavour = ",flavour];
p6=processdataplotsmultiexp8percentage[{corrdataclassfinal},readcorrconfigfile6[configDir,configfilename],6,flavour ];
(*add exptname table into output figure*)

(*p6=GraphicsGrid[p6,Spacings\[Rule]Scaled[0.15] ];*)
Table[
(*
filename=obsname[[6]]<>"_"<>representationname[[5]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[1,2]],ImageResolution\[Rule]imgresol ];
*)
(*20171201 for +1: absolute values of data, for -1: sign data*)
If[
FigureFlag[[6]]==-1,
filename=obsname[[6]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[1,1]],ImageResolution->imgresol ];
filename=obsname[[6]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[2,1]],ImageResolution->imgresol ];
"dummy"
];
If[
FigureFlag[[6]]==1,
filename=obsname[[6]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[1,1]],ImageResolution->imgresol ];
filename=obsname[[6]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[2,2]],ImageResolution->imgresol ];
"dummy"
];

"dummy",
{i,Length[iext]}
];

(*20171103: add files storing mathematica expressions so that users can change details of every figure*)
(*
filename=obsname[[6]]<>"_"<>representationname[[5]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[1,2]]];
*)
(*20171201 for +1: absolute values of data, for -1: sign data*)
If[
FigureFlag[[6]]==-1,
p6expression=Append[p6expression,{p6[[1,1]],p6[[1,2]],p6[[2,1]]}];
(*
filename=obsname[[6]]<>"_"<>"plotlist"<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,{p6[[1,1]],p6[[1,2]],p6[[2,1]]}  ];
*)
(*
filename=obsname[[6]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[1,1]] ];
filename=obsname[[6]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[2,1]] ];
*)
"dummy"
];
If[
FigureFlag[[6]]==1,
p6expression=Append[p6expression,{p6[[1,1]],p6[[1,2]],p6[[2,2]]}];
(*
filename=obsname[[6]]<>"_"<>"plotlist"<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,{p6[[1,1]],p6[[1,2]],p6[[2,1]]}  ];
*)
(*
filename=obsname[[6]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[1,1]] ];
filename=obsname[[6]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[2,2]] ];
*)
"dummy"
];

];

(*dr*corr plots*)
If[
FigureFlag[[5]]==1 || FigureFlag[[5]]==-1,
Print["making plot of figure type ",FigureType[[5]],", flavour = ",flavour];
p5=processdataplotsmultiexp8percentage[{dRcorrdataclassfinal},readcorrconfigfile6[configDir,configfilename],5,flavour];

(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
Table[
(*
filename=obsname[[5]]<>"_"<>representationname[[5]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[1,2]],ImageResolution\[Rule]imgresol  ];
*)
(*20171201 for +1: absolute values of data, for -1: sign data*)
If[
FigureFlag[[5]]==-1,
filename=obsname[[5]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[1,1]],ImageResolution->imgresol  ];
filename=obsname[[5]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[2,1]],ImageResolution->imgresol ];
"dummy"
];
If[
FigureFlag[[5]]==1,
filename=obsname[[5]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[1,1]],ImageResolution->imgresol  ];
filename=obsname[[5]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[2,2]],ImageResolution->imgresol ];
"dummy"
];

"dummy",
{i,Length[iext]}
];

(*20171103: add files storing mathematica expressions so that users can change details of every figure*)
(*
filename=obsname[[5]]<>"_"<>representationname[[5]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[1,2]] ];
*)
(*20171201 for +1: absolute values of data, for -1: sign data*)
If[
FigureFlag[[5]]==-1,
p5expression=Append[p5expression,{p5[[1,1]],p5[[1,2]],p5[[2,1]]}];
(*
filename=obsname[[5]]<>"_"<>"plotlist"<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,{p5[[1,1]],p5[[1,2]],p5[[2,1]]}  ];
*)
(*
filename=obsname[[5]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[1,1]]  ];
filename=obsname[[5]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[2,1]] ];
*)
"dummy"
];
If[
FigureFlag[[5]]==1,
p5expression=Append[p5expression,{p5[[1,1]],p5[[1,2]],p5[[2,2]]}];
(*
filename=obsname[[5]]<>"_"<>"plotlist"<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,{p5[[1,1]],p5[[1,2]],p5[[2,2]]}  ];
*)
(*
filename=obsname[[5]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[1,1]]  ];
filename=obsname[[5]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[2,2]] ];
*)
"dummy"
];

];


"dummy"
];
"dummy"
,{flavour,-5,-5+fmax-1}
];

(*20171202 save .m file containing all figures of flavours *)
filename=obsname[[5]]<>"_"<>"plotlist"<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5expression  ];
filename=obsname[[6]]<>"_"<>"plotlist"<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6expression  ];

(*2, 3, 4*)
(*dr*corr plots*)
If[
FigureFlag[[2]]==1 || FigureFlag[[2]]==-1,
Print["making plot of figure type ",FigureType[[2]],", flavour = ",flavour];
p234=processdataplotsmultiexp8percentage[{expterrordataclassfinal},readcorrconfigfile6[configDir,configfilename],2,flavour];
(*20171202 add important data info*)
datainfostr=datainfototext[{expterrordataclassfinal},{dtacentralclassfinal},readcorrconfigfile6[configDir,configfilename],2,flavour];
(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)

Table[
filename=obsname[[2]]<>"_"<>representationname[[2]]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]],ImageResolution->imgresol  ];
(*
filename=obsname[[2]]<>"_"<>representationname[[5]]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,2]],ImageResolution\[Rule]imgresol  ];
*)
(*20171108: \[Sigma]/D has no negative data, so delete histogram of range = (-x, x)*)
(*
filename=obsname[[2]]<>"_"<>representationname[[3]]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,1]],ImageResolution\[Rule]imgresol  ];
*)
filename=obsname[[2]]<>"_"<>representationname[[4]]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,2]],ImageResolution->imgresol  ];
"dummy",
{i,Length[iext]}
];

(*20171103: add files storing mathematica expressions so that users can change details of every figure*)
(*20171202 only save in one file*)
filename=obsname[[2]]<>"_"<>"plotlist"<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,{p234[[1,1]],p234[[1,2]],p234[[2,2]]}  ];
(*
filename=obsname[[2]]<>"_"<>representationname[[2]]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]]  ];
filename=obsname[[2]]<>"_"<>representationname[[5]]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,2]]  ];
(*20171108: \[Sigma]/D has no negative data, so delete histogram of range = (-x, x)*)
(*
filename=obsname[[2]]<>"_"<>representationname[[3]]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,1]]  ];
*)
filename=obsname[[2]]<>"_"<>representationname[[4]]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,2]]  ];
*)

(*20171202 add important data info*)
filename=obsname[[2]]<>"_"<>representationname[[8]]<>"_samept"<>".txt";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,datainfostr  ];


];

If[
FigureFlag[[3]]==1 || FigureFlag[[3]]==-1,
Print["making plot of figure type ",FigureType[[3]],", flavour = ",flavour];
p234=processdataplotsmultiexp8percentage[{residualdataclassfinal},readcorrconfigfile6[configDir,configfilename],3,flavour];
(*20171202 add important data info*)
datainfostr=datainfototext[{residualdataclassfinal},{dtacentralclassfinal},readcorrconfigfile6[configDir,configfilename],3,flavour];
(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
Table[
(*
filename=obsname[[3]]<>"_"<>representationname[[5]]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,2]],ImageResolution\[Rule]imgresol  ];
*)
(*20171201 for +1: absolute values of data, for -1: sign data*)
If[
FigureFlag[[3]]==-1,
filename=obsname[[3]]<>"_"<>representationname[[1]]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]],ImageResolution->imgresol  ];
filename=obsname[[3]]<>"_"<>representationname[[3]]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,1]],ImageResolution->imgresol ];
"dummy"
];
If[
FigureFlag[[3]]==1,
filename=obsname[[3]]<>"_"<>representationname[[2]]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]],ImageResolution->imgresol  ];
filename=obsname[[3]]<>"_"<>representationname[[4]]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,2]],ImageResolution->imgresol ];
"dummy"
];

"dummy",
{i,Length[iext]}
];

(*20171103: add files storing mathematica expressions so that users can change details of every figure*)
(*
filename=obsname[[3]]<>"_"<>representationname[[5]]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,2]]  ];
*)
(*20171201 for +1: absolute values of data, for -1: sign data*)
If[
FigureFlag[[3]]==-1,
(*20171202 only save in one file*)
filename=obsname[[3]]<>"_"<>"plotlist"<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,{p234[[1,1]],p234[[1,2]],p234[[2,1]]}  ];
(*
filename=obsname[[3]]<>"_"<>representationname[[1]]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]]  ];
filename=obsname[[3]]<>"_"<>representationname[[3]]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,1]] ];
*)
(*20171202 add important data info*)
filename=obsname[[3]]<>"_"<>representationname[[7]]<>"_samept"<>".txt";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,datainfostr  ];
"dummy"
];
If[
FigureFlag[[3]]==1,
(*20171202 only save in one file*)
filename=obsname[[3]]<>"_"<>"plotlist"<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,{p234[[1,1]],p234[[1,2]],p234[[2,2]]}  ];
(*
filename=obsname[[3]]<>"_"<>representationname[[2]]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]]  ];
filename=obsname[[3]]<>"_"<>representationname[[4]]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,2]] ];
*)
(*20171202 add important data info*)
filename=obsname[[3]]<>"_"<>representationname[[8]]<>"_samept"<>".txt";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,datainfostr  ];
"dummy"
];


];

If[
FigureFlag[[4]]==1 || FigureFlag[[4]]==-1,
Print["making plot of figure type ",FigureType[[4]],", flavour = ",flavour];
p234=processdataplotsmultiexp8percentage[{dRdataclassfinal},readcorrconfigfile6[configDir,configfilename],4,flavour];
(*20171202 add important data info*)
datainfostr=datainfototext[{dRdataclassfinal},{dtacentralclassfinal},readcorrconfigfile6[configDir,configfilename],4,flavour];
(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
Table[
filename=obsname[[4]]<>"_"<>representationname[[2]]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]],ImageResolution->imgresol  ];
(*
filename=obsname[[4]]<>"_"<>representationname[[5]]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,2]],ImageResolution\[Rule]imgresol  ];
*)
(*20171108: \[Delta]r has no negative data, so delete histogram of range = (-x, x)*)
(*
filename=obsname[[4]]<>"_"<>representationname[[3]]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,1]],ImageResolution\[Rule]imgresol  ];
*)
filename=obsname[[4]]<>"_"<>representationname[[4]]<>"_samept"<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,2]],ImageResolution->imgresol  ];
"dummy",
{i,Length[iext]}
];

(*20171202 only save in one file*)
filename=obsname[[4]]<>"_"<>"plotlist"<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,{p234[[1,1]],p234[[1,2]],p234[[2,2]]}  ];
(*
(*20171103: add files storing mathematica expressions so that users can change details of every figure*)
filename=obsname[[4]]<>"_"<>representationname[[2]]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]]  ];
filename=obsname[[4]]<>"_"<>representationname[[5]]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,2]]  ];
(*20171108: \[Delta]r has no negative data, so delete histogram of range = (-x, x)*)
(*
filename=obsname[[4]]<>"_"<>representationname[[3]]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,1]]  ];
*)
filename=obsname[[4]]<>"_"<>representationname[[4]]<>"_samept"<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,2]]  ];
*)
(*20171202 add important data info*)
filename=obsname[[4]]<>"_"<>representationname[[8]]<>"_samept"<>".txt";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,datainfostr  ];


];

If[
FigureFlag[[1]]==1 || FigureFlag[[1]]==-1,
Print["making plot of figure type ",FigureType[[1]] ];
p1=processdataplotsmultiexp8percentage[{corrdataclassfinal},readcorrconfigfile6[configDir,configfilename],1,0 ];

Table[
filename=obsname[[1]]<>"_"<>representationname[[6]]<>extensionname[[iext[[i]] ]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p1[[1]],ImageResolution->imgresol ];
"dummy",
{i,Length[iext]}
];

filename=obsname[[1]]<>"_"<>representationname[[1]]<>".m";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p1[[1]] ];
"dummy"
];


(*
If[
FigureFlag[[2]]\[Equal]1,
Print["making plot of figure type ",FigureType[[2]] ];
p2=processdataplotsmultiexp5percentage[expterrordatacorrmaxclassfinal,readcorrconfigfile4[configDir,configfilename],2,0];

(*p2=GraphicsGrid[p2,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[2]]<>"_"<>representationname[[1]]<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p2[[1,1]],ImageResolution\[Rule]imgresol ];
filename=obsname[[2]]<>"_"<>representationname[[2]]<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p2[[1,2]],ImageResolution\[Rule]imgresol ];
filename=obsname[[2]]<>"_"<>representationname[[3]]<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p2[[2,1]],ImageResolution\[Rule]imgresol ];
filename=obsname[[2]]<>"_"<>representationname[[4]]<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p2[[2,2]],ImageResolution\[Rule]imgresol ];
];

If[
FigureFlag[[3]]\[Equal]1,
Print["making plot of figure type ",FigureType[[3]] ];
p3=processdataplotsmultiexp5percentage[residualdatacorrmaxclassfinal,readcorrconfigfile4[configDir,configfilename],3,0];

(*p3=GraphicsGrid[p3,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[3]]<>"_"<>representationname[[1]]<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p3[[1,1]],ImageResolution\[Rule]imgresol ];
filename=obsname[[3]]<>"_"<>representationname[[2]]<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p3[[1,2]],ImageResolution\[Rule]imgresol ];
filename=obsname[[3]]<>"_"<>representationname[[3]]<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p3[[2,1]],ImageResolution\[Rule]imgresol ];
filename=obsname[[3]]<>"_"<>representationname[[4]]<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p3[[2,2]],ImageResolution\[Rule]imgresol ];
];

(*dr plots*)
If[
FigureFlag[[4]]\[Equal]1,
Print["making plot of figure type ",FigureType[[4]] ];
p4=processdataplotsmultiexp5percentage[dRdatacorrmaxclassfinal,readcorrconfigfile4[configDir,configfilename],4,0];

(*p4=GraphicsGrid[p4,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[4]]<>"_"<>representationname[[1]]<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p4[[1,1]],ImageResolution\[Rule]imgresol ];
filename=obsname[[4]]<>"_"<>representationname[[2]]<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p4[[1,2]],ImageResolution\[Rule]imgresol ];
filename=obsname[[4]]<>"_"<>representationname[[3]]<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p4[[2,1]],ImageResolution\[Rule]imgresol ];
filename=obsname[[4]]<>"_"<>representationname[[4]]<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p4[[2,2]],ImageResolution\[Rule]imgresol ];
];
*)
];
Print[""];(*space*)
Print["time to make plots is ",jpgtime," seconds"];
Print[""];(*space*)


(*copy configure file to job dir*)
(*20170508 if config file in plot path exist, remove it*)
If[FileExistsQ[saveparentpath<>jobpath<>configfilename]==True,DeleteFile[saveparentpath<>jobpath<>configfilename] ];
CopyFile[configDir<>configfilename,saveparentpath<>jobpath<>configfilename];
(*20171119 copy user_func.txt to the output directory*)(*20171127: only copy it when user function mode is on*)
If[
CorrelationArgFlag[[-1]]==1,
If[FileExistsQ[saveparentpath<>jobpath<>userfuncfilename]==True,DeleteFile[saveparentpath<>jobpath<>userfuncfilename] ];
CopyFile[configDir<>userfuncfilename,saveparentpath<>jobpath<>userfuncfilename];
"dummy"
];

(*make exptname table, 20170410: Sean asks to move this process to the final step*)
Table[
rows=3;
filename="exptname_table"<>extensionname[[iext[[i]] ]];
(*20171114: need to show the jobid and time so that python script users know where to find the figures*)
title="jobid: "<>ToString[Jobid](*<>"\n"<>DateString[{"Month","/","Day","/","Year"," ","Hour",":","Minute",":","Second"}]*);
datemode=True;
exptnames=Table[ExptIDtoName[exptlistfinal[[iexpt]] ]<>"("<>ToString[exptlistfinal[[iexpt]] ]<>")",{iexpt,1,Length[exptlistfinal]}];
Print["making table of experiments included in plots"];
(*20171128: add job description in exptname_table file*)
JobDescription="Job description: "<>JobDescription;
exptnamestable=makeGrid2[exptnames,rows,title,JobDescription,datemode];(*20171114: add date mode to write date*)
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,exptnamestable];

(*20171220: add the legend for all figures*)
filename="exptname_legend"<>extensionname[[iext[[i]] ]];
title="";
(*20171126: set labels depend on different classifymode*)
If[
ClassifyMode=="single",
(*20171130: if shapes are used out, repeat the shapes*)
shapeslist=(PlotMarkerList[][[1]]&/@Range[10])//Flatten;
exptnames=Table[ToString[shapeslist[[iexpt]] ]<>ExptIDtoName[exptlistfinal[[iexpt]] ]<>"("<>ToString[exptlistfinal[[iexpt]] ]<>")",{iexpt,1,Length[exptlistfinal]}];
"dummy"
];
If[
ClassifyMode=="all",
exptnames=Table[ExptIDtoName[exptlistfinal[[iexpt]] ]<>"("<>ToString[exptlistfinal[[iexpt]] ]<>")",{iexpt,1,Length[exptlistfinal]}];
"dummy"
];
(*Print["making table of experiments included in plots"];*)
datemode=False;
(*20171128: for legend, don't show job description*)
NoJobDescription="";

(*exptnamestitle=Switch[FigureFlag[[plottype]],-1,title,1,abstitle,_,Print["error, plot type flag should be 1 or -1"];Abort[] ];*)
exptnamestable=makeGrid2[exptnames,rows,(*exptnamestitle<>"\n\n"*)title,NoJobDescription,datemode];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,exptnamestable];
"dummy",
{i,Length[iext]}
];

(*merge .eps files into a pdf file*)
(*20171124*)
(*20171209 add error message when fail to generating merge file, and input the extension depend on the BranchMode *)
Print["merge files to allfigs.pdf..."];
If[
BranchMode==1,
(*
implementeps[saveparentpath<>jobpath,PDFxQSelectMethod]
*)
FigsMergeToPDF[saveparentpath<>jobpath,PDFxQSelectMethod,".eps"];
If[
FileExistsQ[saveparentpath<>jobpath<>"allfigs.pdf"]==True,
Print["generate merge file: ",saveparentpath<>jobpath<>"allfigs.pdf"],
Print["fail to generate merge file ",saveparentpath<>jobpath<>"allfigs.pdf"]
]
];

If[
BranchMode==2,
FigsMergeToPDF[saveparentpath<>jobpath,PDFxQSelectMethod,".pdf"];
If[
FileExistsQ[saveparentpath<>jobpath<>"allfigs.pdf"]==True,
Print["generate merge file: ",saveparentpath<>jobpath<>"allfigs.pdf"],
Print["fail to generate merge file ",saveparentpath<>jobpath<>"allfigs.pdf"]
]
];

(*copy directory to job directory*)
(*make jobid directory*)
(*
jobpath="Jobs/"<>ToString[Jobid]<>"/";
If[
DirectoryQ[saveparentpath<>jobpath]\[Equal]False,
CreateDirectory[saveparentpath<>jobpath];
"dummy"
];
(*copy files to it*)
filescopyfrom=FileNames["*",{saveparentpath<>pdfnameexpttypeDir<>exptidDir}];
Print[filescopyfrom];
filescopyto=Table[tmpfile=StringSplit[filescopyfrom[[i]],"/"][[-1]];saveparentpath<>jobpath<>tmpfile,{i,1,Length[filescopyfrom]}];
Print[filescopyto];
Table[CopyFile[filescopyfrom[[i]],filescopyto[[i]]],{i,1,Length[filescopyfrom]}];
CopyFile["config1",saveparentpath<>jobpath<>"config1"];
*)

(*20171124 print final info*)
Print["The job is finished.All figures are in",saveparentpath<>jobpath];
(*
Print["search figures under",saveparentpath<>jobpath];
Print["extension = ",extensionname[[#]]&/@iext ];
Print["total #flavour for check = ",CorrelationArgFlag];
Print["epspdfcat: combine several eps files into a PDF file using LaTeX"];
*)

"complete the function"
];



(* ::Section:: *)
(*control runfunc you want to run*)


(* ::Input::Initialization:: *)
(*configDir=NotebookDirectory[];*)(*DirectoryName[$InputFileName]*)
(*configfilename="config_pdf_resolution_test.txt";*)
(*configfilename="config1.txt";*)

(*web version: only run the quick mode*)
(*run the function to draw all experimental data togather*)
If[
LoopExptBool==False,
timefunc=
AbsoluteTiming[
Quicksaveplot[];
]
];

Print["total time to run is ",timefunc," seconds"];


(*if version 2 (LoopExptBool==True;), run a loop to draw every experimental data seperately*)

If[
LoopExptBool==True,
(*configDir=NotebookDirectory[];*)(*DirectoryName[$InputFileName]*)
(*configfilename="config_pdf_resolution_test.txt";*)
configDir=Directory[]<>"/";(*NotebookDirectory[];*)(*DirectoryName[$InputFileName];*)
configfilename="config1.txt";

(*web version: only run the quick mode*)
(*read # of expts in config file*)
(*20171109: for readcorrconfigfile4
{dummy,dummy,dummy,dummy,ExptidType,ExptidFlag,dummy,dummy,dummy,dummy,
dummy,dummy,dummy,dummy,(*Hist1figureYrange*)dummy,
dummy,
dummy,dummy,dummy,dummy,dummy}=
readcorrconfigfile4[configDir,configfilename];
*)
(*20171109: for readcorrconfigfile5*)
{dummy,dummy(*20171128*),dummy,dummy,dummy,ExptidType,ExptidFlag,dummy,dummy,(*dummy,dummy,*)
dummy,dummy,(*ColorPaletterange*)(*20171128*)dummy,dummy,(*dummy,(*Hist1figureYrange*)dummy,*)
(*dummy,*)
dummy,dummy,dummy,dummy,dummy}=
readcorrconfigfile6[configDir,configfilename];

Lexpt={};
Table[
If[ExptidFlag[[iexpt]]==1,Lexpt=Append[Lexpt,ExptidType[[iexpt]] ] ],
{iexpt,1,Length[ExptidType]}
];

(*web version: only run the quick mode*)
(*loop for all expts in config file, plot figures of expts one by one*)
Table[
timefunc=
AbsoluteTiming[
Quicksaveplot[];
];

Print["total running time is ",timefunc," seconds"];
If[irun==Length[Lexpt],Print["all processes are done"];Abort[]];
"dummy",
{irun,1,Length[Lexpt]}
]

];


(* ::Section:: *)
(*Others*)


(* ::Input:: *)
(*residualNsetclass//Dimensions*)


(* ::Input:: *)
(*residualNsetclass//Dimensions*)
(*FileExistsQ[(CorrDataDir<>"residualNset_"<>CorrDataFile)]*)
(*residualNsetclass=Get[(CorrDataDir<>"residualNset_"<>CorrDataFile)]*)
(*residualNsetclass//Dimensions*)
(*residualNsetclass[[1]][["label"]]*)
(*filetmp=CorrDataDir<>"residualNset_"<>CorrDataFile*)
(*Directory[]*)
(*AbsoluteTiming[tmpvariable <<filetmp]*)
(*tmpvariable//Dimensions*)
(*tmpvariable=Get[filetmp]*)
(**)
(*fxQsamept2class=Get[(CorrDataDir<>"fxQNset_"<>CorrDataFile)]*)
(**)


(* ::Input:: *)
(*expterrordataclass[[1]]*)


(* ::Input:: *)
(*Dimensions[residualdataclassfinal]*)


(* ::Input:: *)
(*residualdataclassfinal[[2]][["exptinfo","exptid"]]*)


(* ::Input:: *)
(*Table[residualdataclass[[iexpt]][["exptinfo","exptid"]],{iexpt,1,Length[residualdataclass]}]*)


(* ::Input:: *)
(*Length[{159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159,159}]*)


(* ::Input:: *)
(*Table[expterrordataclass[[iexpt]][["exptinfo","exptid"]],{iexpt,1,Length[expterrordataclass]}]*)
(**)


(* ::Input:: *)
(*Dimensions[Flatten[{Table[Datamethods[["take"]][corrfxQdtaobsclassin[[iexpt,flavourin+6]],2][["data"]]/.LF->LF1,{iexpt,1,Length[corrfxQdtaobsclassin]}]},1] ]*)


(* ::Input:: *)
(*Table[Datamethods[["take"]][corrdataclassfinal[[iexpt,0+6]],2][["data"]]/.LF->LF1,{iexpt,1,Length[corrdataclassfinal]}]*)


(* ::Input:: *)
(*Table[corrdataclassfinal[[iexpt,0+6]],{iexpt,1,Length[corrdataclassfinal]}]*)
(*Flatten[{Table[corrdataclassfinal[[iexpt,0+6]],{iexpt,1,Length[corrdataclassfinal]}]},1]*)


(* ::Input:: *)
(*ExptIDtoName[{124,240}]*)


(* ::Input:: *)
(*processdataplotsmultiexp6percentage[{corrdataclassfinal},readcorrconfigfile4[configDir,configfilename],6,0]*)


(* ::Input:: *)
(*processdataplotsmultiexp6percentage[{dRcorrdataclassfinal},readcorrconfigfile4[configDir,configfilename],5,3]*)


(* ::Input:: *)
(*residualdataclassfinal*)


(* ::Input:: *)
(*{{<|"label"->{"x","Q","residual"},"data"->{},"exptinfo"-><|"exptid"->225,"exptname"->"cdfLasy","feyndiagram"->"unset"|>,"PDFinfo"-><|"PDFname"->"CT14HERA2NNLO","PDFsetmethod"->"Hessian","Nset"->57,"iset"->"unset","flavour"->-5|>|>,<|"label"->{"x","Q","residual"},"data"->{},"exptinfo"-><|"exptid"->225,"exptname"->"cdfLasy","feyndiagram"->"unset"|>,"PDFinfo"-><|"PDFname"->"CT14HERA2NNLO","PDFsetmethod"->"Hessian","Nset"->57,"iset"->"unset","flavour"->-4|>|>,<|"label"->{"x","Q","residual"},"data"->{},"exptinfo"-><|"exptid"->225,"exptname"->"cdfLasy","feyndiagram"->"unset"|>,"PDFinfo"-><|"PDFname"->"CT14HERA2NNLO","PDFsetmethod"->"Hessian","Nset"->57,"iset"->"unset","flavour"->-3|>|>,<|"label"->{"x","Q","residual"},"data"->{},"exptinfo"-><|"exptid"->225,"exptname"->"cdfLasy","feyndiagram"->"unset"|>,"PDFinfo"-><|"PDFname"->"CT14HERA2NNLO","PDFsetmethod"->"Hessian","Nset"->57,"iset"->"unset","flavour"->-2|>|>,<|"label"->{"x","Q","residual"},"data"->{},"exptinfo"-><|"exptid"->225,"exptname"->"cdfLasy","feyndiagram"->"unset"|>,"PDFinfo"-><|"PDFname"->"CT14HERA2NNLO","PDFsetmethod"->"Hessian","Nset"->57,"iset"->"unset","flavour"->-1|>|>,<|"label"->{"x","Q","residual"},"data"->{},"exptinfo"-><|"exptid"->225,"exptname"->"cdfLasy","feyndiagram"->"unset"|>,"PDFinfo"-><|"PDFname"->"CT14HERA2NNLO","PDFsetmethod"->"Hessian","Nset"->57,"iset"->"unset","flavour"->0|>|>,<|"label"->{"x","Q","residual"},"data"->{},"exptinfo"-><|"exptid"->225,"exptname"->"cdfLasy","feyndiagram"->"unset"|>,"PDFinfo"-><|"PDFname"->"CT14HERA2NNLO","PDFsetmethod"->"Hessian","Nset"->57,"iset"->"unset","flavour"->1|>|>,<|"label"->{"x","Q","residual"},"data"->{LF[0.17256153384559803`,80.39`,1.971732954545455`],LF[0.1855112310794266`,80.39`,1.971732954545455`],LF[0.19917645454180155`,80.39`,1.971732954545455`],LF[0.21358308834796036`,80.39`,1.971732954545455`],LF[0.22875747653402578`,80.39`,1.971732954545455`],LF[0.2447264230570053`,80.39`,1.971732954545455`],LF[0.261517191794792`,80.39`,1.971732954545455`],LF[0.16030193864596357`,80.39`,-0.13771569433032135`],LF[0.17256153384559803`,80.39`,-0.13771569433032135`],LF[0.1855112310794266`,80.39`,-0.13771569433032135`],LF[0.19917645454180155`,80.39`,-0.13771569433032135`],LF[0.21358308834796036`,80.39`,-0.13771569433032135`],LF[0.22875747653402578`,80.39`,-0.13771569433032135`],LF[0.2447264230570053`,80.39`,-0.13771569433032135`],LF[0.261517191794792`,80.39`,-0.13771569433032135`],LF[0.2791575065461639`,80.39`,-0.13771569433032135`],LF[0.29767555103078436`,80.39`,-0.13771569433032135`],LF[0.3170999688892017`,80.39`,-0.13771569433032135`],LF[0.33745986368284964`,80.39`,-0.13771569433032135`],LF[0.17256153384559803`,80.39`,-0.433294117647059`],LF[0.1855112310794266`,80.39`,-0.433294117647059`],LF[0.19917645454180155`,80.39`,-0.433294117647059`],LF[0.21358308834796036`,80.39`,-0.433294117647059`],LF[0.22875747653402578`,80.39`,-0.433294117647059`],LF[0.2447264230570053`,80.39`,-0.433294117647059`],LF[0.261517191794792`,80.39`,-0.433294117647059`],LF[0.2791575065461639`,80.39`,-0.433294117647059`],LF[0.29767555103078436`,80.39`,-0.433294117647059`],LF[0.3170999688892017`,80.39`,-0.433294117647059`],LF[0.33745986368284964`,80.39`,-0.433294117647059`],LF[0.35878479889404674`,80.39`,-0.433294117647059`],LF[0.38110479792599694`,80.39`,-0.433294117647059`],LF[0.17256153384559803`,80.39`,-0.8482809123649461`],LF[0.1855112310794266`,80.39`,-0.8482809123649461`],LF[0.19917645454180155`,80.39`,-0.8482809123649461`],LF[0.21358308834796036`,80.39`,-0.8482809123649461`],LF[0.22875747653402578`,80.39`,-0.8482809123649461`],LF[0.2447264230570053`,80.39`,-0.8482809123649461`],LF[0.261517191794792`,80.39`,-0.8482809123649461`],LF[0.2791575065461639`,80.39`,-0.8482809123649461`],LF[0.29767555103078436`,80.39`,-0.8482809123649461`],LF[0.3170999688892017`,80.39`,-0.8482809123649461`],LF[0.33745986368284964`,80.39`,-0.8482809123649461`],LF[0.35878479889404674`,80.39`,-0.8482809123649461`],LF[0.38110479792599694`,80.39`,-0.8482809123649461`],LF[0.4044503441027893`,80.39`,-0.8482809123649461`],LF[0.1855112310794266`,80.39`,0.0921636917718761`],LF[0.19917645454180155`,80.39`,0.0921636917718761`],LF[0.21358308834796036`,80.39`,0.0921636917718761`],LF[0.22875747653402578`,80.39`,0.0921636917718761`],LF[0.2447264230570053`,80.39`,0.0921636917718761`],LF[0.261517191794792`,80.39`,0.0921636917718761`],LF[0.2791575065461639`,80.39`,0.0921636917718761`],LF[0.29767555103078436`,80.39`,0.0921636917718761`],LF[0.3170999688892017`,80.39`,0.0921636917718761`],LF[0.33745986368284964`,80.39`,0.0921636917718761`],LF[0.35878479889404674`,80.39`,0.0921636917718761`],LF[0.38110479792599694`,80.39`,0.0921636917718761`],LF[0.4044503441027893`,80.39`,0.0921636917718761`],LF[0.42885238066939807`,80.39`,0.0921636917718761`]},"exptinfo"-><|"exptid"->225,"exptname"->"cdfLasy","feyndiagram"->"unset"|>,"PDFinfo"-><|"PDFname"->"CT14HERA2NNLO","PDFsetmethod"->"Hessian","Nset"->57,"iset"->"unset","flavour"->2|>|>,<|"label"->{"x","Q","residual"},"data"->{},"exptinfo"-><|"exptid"->225,"exptname"->"cdfLasy","feyndiagram"->"unset"|>,"PDFinfo"-><|"PDFname"->"CT14HERA2NNLO","PDFsetmethod"->"Hessian","Nset"->57,"iset"->"unset","flavour"->3|>|>,<|"label"->{"x","Q","residual"},"data"->{},"exptinfo"-><|"exptid"->225,"exptname"->"cdfLasy","feyndiagram"->"unset"|>,"PDFinfo"-><|"PDFname"->"CT14HERA2NNLO","PDFsetmethod"->"Hessian","Nset"->57,"iset"->"unset","flavour"->4|>|>,<|"label"->{"x","Q","residual"},"data"->{},"exptinfo"-><|"exptid"->225,"exptname"->"cdfLasy","feyndiagram"->"unset"|>,"PDFinfo"-><|"PDFname"->"CT14HERA2NNLO","PDFsetmethod"->"Hessian","Nset"->57,"iset"->"unset","flavour"->5|>|>,<|"label"->{"x","Q","residual"},"data"->{},"exptinfo"-><|"exptid"->225,"exptname"->"cdfLasy","feyndiagram"->"unset"|>,"PDFinfo"-><|"PDFname"->"CT14HERA2NNLO","PDFsetmethod"->"Hessian","Nset"->57,"iset"->"unset","flavour"->-5|>|>,<|"label"->{"x","Q","residual"},"data"->{LF[0.19917645454180155`,80.39`,1.971732954545455`],LF[0.21358308834796036`,80.39`,1.971732954545455`],LF[0.22875747653402578`,80.39`,1.971732954545455`],LF[0.2447264230570053`,80.39`,1.971732954545455`],LF[0.261517191794792`,80.39`,1.971732954545455`],LF[0.1855112310794266`,80.39`,-0.13771569433032135`],LF[0.19917645454180155`,80.39`,-0.13771569433032135`],LF[0.21358308834796036`,80.39`,-0.13771569433032135`],LF[0.22875747653402578`,80.39`,-0.13771569433032135`],LF[0.2447264230570053`,80.39`,-0.13771569433032135`],LF[0.261517191794792`,80.39`,-0.13771569433032135`],LF[0.2791575065461639`,80.39`,-0.13771569433032135`],LF[0.29767555103078436`,80.39`,-0.13771569433032135`],LF[0.3170999688892017`,80.39`,-0.13771569433032135`],LF[0.33745986368284964`,80.39`,-0.13771569433032135`],LF[0.1855112310794266`,80.39`,-0.433294117647059`],LF[0.19917645454180155`,80.39`,-0.433294117647059`],LF[0.21358308834796036`,80.39`,-0.433294117647059`],LF[0.22875747653402578`,80.39`,-0.433294117647059`],LF[0.2447264230570053`,80.39`,-0.433294117647059`],LF[0.261517191794792`,80.39`,-0.433294117647059`],LF[0.2791575065461639`,80.39`,-0.433294117647059`],LF[0.29767555103078436`,80.39`,-0.433294117647059`],LF[0.3170999688892017`,80.39`,-0.433294117647059`],LF[0.33745986368284964`,80.39`,-0.433294117647059`],LF[0.35878479889404674`,80.39`,-0.433294117647059`],LF[0.38110479792599694`,80.39`,-0.433294117647059`],LF[0.1855112310794266`,80.39`,-0.8482809123649461`],LF[0.19917645454180155`,80.39`,-0.8482809123649461`],LF[0.21358308834796036`,80.39`,-0.8482809123649461`],LF[0.22875747653402578`,80.39`,-0.8482809123649461`],LF[0.2447264230570053`,80.39`,-0.8482809123649461`],LF[0.261517191794792`,80.39`,-0.8482809123649461`],LF[0.2791575065461639`,80.39`,-0.8482809123649461`],LF[0.29767555103078436`,80.39`,-0.8482809123649461`],LF[0.3170999688892017`,80.39`,-0.8482809123649461`],LF[0.33745986368284964`,80.39`,-0.8482809123649461`],LF[0.35878479889404674`,80.39`,-0.8482809123649461`],LF[0.38110479792599694`,80.39`,-0.8482809123649461`],LF[0.4044503441027893`,80.39`,-0.8482809123649461`],LF[0.19917645454180155`,80.39`,0.0921636917718761`],LF[0.21358308834796036`,80.39`,0.0921636917718761`],LF[0.22875747653402578`,80.39`,0.0921636917718761`],LF[0.2447264230570053`,80.39`,0.0921636917718761`],LF[0.261517191794792`,80.39`,0.0921636917718761`],LF[0.2791575065461639`,80.39`,0.0921636917718761`],LF[0.29767555103078436`,80.39`,0.0921636917718761`],LF[0.3170999688892017`,80.39`,0.0921636917718761`],LF[0.33745986368284964`,80.39`,0.0921636917718761`],LF[0.35878479889404674`,80.39`,0.0921636917718761`],LF[0.38110479792599694`,80.39`,0.0921636917718761`],LF[0.4044503441027893`,80.39`,0.0921636917718761`],LF[0.42885238066939807`,80.39`,0.0921636917718761`]},"exptinfo"-><|"exptid"->225,"exptname"->"cdfLasy","feyndiagram"->"unset"|>,"PDFinfo"-><|"PDFname"->"CT14HERA2NNLO","PDFsetmethod"->"Hessian","Nset"->57,"iset"->"unset","flavour"->-5|>|>,<|"label"->{"x","Q","residual"},"data"->{},"exptinfo"-><|"exptid"->225,"exptname"->"cdfLasy","feyndiagram"->"unset"|>,"PDFinfo"-><|"PDFname"->"CT14HERA2NNLO","PDFsetmethod"->"Hessian","Nset"->57,"iset"->"unset","flavour"->-5|>|>}}*)


(* ::Input:: *)
(*processdataplotsmultiexp6percentage[{residualdataclassfinal},readcorrconfigfile4[configDir,configfilename],3,1]*)


(* ::Input:: *)
(*Show[p234[[1,1]] ]*)


(* ::Input:: *)
(*processdataplotsmultiexp6percentage[{dRdataclassfinal},readcorrconfigfile4[configDir,configfilename],4,1]*)


(* ::Input:: *)
(*processdataplotsmultiexp6percentage[{expterrordataclassfinal},readcorrconfigfile4[configDir,configfilename],2,0]*)


(* ::Input:: *)
(*Export["testreso.eps",processdataplotsmultiexp6percentage[{expterrordataclassfinal},readcorrconfigfile4[configDir,configfilename],2,0][[1,1]],ImageResolution->imgresol]*)


(* ::Input:: *)
(*imgresol=144*)


(* ::Input:: *)
(*NotebookDirectory[]*)


(* ::Input:: *)
(*Directory[]*)


(* ::Input:: *)
(*readcorrconfigfile4[configDir,configfilename]*)


(* ::Input:: *)
(*readcorrconfigfile4[NotebookDirectory[],configfilename]*)


(* ::Input:: *)
(*readcorrconfigfile4[Directory[]<>"/",configfilename]*)


(* ::Input:: *)
(*FileNames[Directory[]]*)
(*FileNames[Directory[]<>"/"]*)


(* ::Input:: *)
(*testnewindex=Import[CorrDataDir<>"expterror_"<>CorrDataFile,"ExpressionML"];*)


(* ::Input:: *)
(*testnewindex[[1]]*)


(* ::Input:: *)
(*CorrDataDir*)


(* ::Input:: *)
(*Get["corr_proj_funcs.m"]*)


(* ::Input:: *)
(*processdataplotsmultiexp7percentage[{expterrordataclassfinal},readcorrconfigfile6[configDir,configfilename],2,flavour]*)


(* ::Input:: *)
(*processdataplotsmultiexp7percentage[{residualdataclassfinal},readcorrconfigfile6[configDir,configfilename],3,flavour]*)


(* ::Input:: *)
(*processdataplotsmultiexp7percentage[{dRdataclassfinal},readcorrconfigfile6[configDir,configfilename],4,flavour]*)


(* ::Input:: *)
(*processdataplotsmultiexp7percentage[{dRcorrdataclassfinal},readcorrconfigfile6[configDir,configfilename],5,0]*)


(* ::Input:: *)
(*processdataplotsmultiexp7percentage[{corrdataclassfinal},readcorrconfigfile6[configDir,configfilename],6,0 ]*)


(* ::Input:: *)
(*Get["corr_proj_funcs.m"]*)
(*ReadLisFile[datalistFile];*)


(* ::Input:: *)
(*ReadUserFunction[Directory[]<>"/","user_define_func.txt"]*)


(* ::Input:: *)
(*(*{Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,*)
(*XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,(*Hist1figureYrange*)dummy12,*)
(*ColorSeperator,*)
(*Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}=*)*)
(*readcorrconfigfile4[configDir,configfilename]*)
(*readcorrconfigfile5[configDir,configfilename][[-2]]*)


(* ::Input:: *)
(*(*20171109: seperate user difine function/data IO and configure file*)*)
(*userfuncfilename="user_define_func.txt";*)
(*{UserArgName,UserArgValue}=ReadUserFunction["./",userfuncfilename]*)


(* ::Input:: *)
(*ReadUserFunctionV3["./",userfuncfilename]*)


(* ::Input:: *)
(*fxQsamept2classfinal[[3,17]]*)


(* ::Input:: *)
(*exptidDir*)


(* ::Input:: *)
(*exptid*)


(* ::Input:: *)
(*fxQDatabaseExptlist*)


(* ::Input:: *)
(*((fxQDatabaselist[[1,iu+6,3]]/.LF->List)-(fxQDatabaselist[[1,iubar+6,3]]/.LF->List))/((fxQDatabaselist[[1,iu+6,3]]/.LF->List)+(fxQDatabaselist[[1,iubar+6,3]]/.LF->List))*)
(*fxQsamept2classfinal[[2,-1]][["data"]][[1]]*)


(* ::Input:: *)
(*((fxQDatabaselist[[10,iu+6,1]]/.LF->List)-(fxQDatabaselist[[10,iubar+6,1]]/.LF->List))/((fxQDatabaselist[[10,iu+6,1]]/.LF->List)+(fxQDatabaselist[[10,iubar+6,1]]/.LF->List))*)
(*fxQsamept2classfinal[[1,-2]][["data"]][[1]]*)
(*fxQsamept2classfinal[[1,1]][["exptinfo","exptid"]]*)


(* ::Input:: *)
(*processdataplotsmultiexp7percentage[{corrdataclassfinal},readcorrconfigfile6[configDir,configfilename],1,0 ]*)


(* ::Input:: *)
(*Get["corr_proj_funcs.m"]*)


(* ::Input:: *)
(*ReadLisFile[datalistFile];*)


(* ::Input:: *)
(*lisTable*)


(* ::Input:: *)
(*readcorrconfigfile6[configDir,configfilename]//TableForm*)


(* ::Input:: *)
(*readcorrconfigfile6[configDir,configfilename]//TableForm*)


(* ::Input:: *)
(*readcorrconfigfile6[configDir,configfilename]//TableForm*)


(* ::Subsection:: *)
(*test run the info of data*)


(* ::Input:: *)
(*(*20171126*)*)
(*(**)
(*when call processdataplotsmultiexp7percentage,*)
(*if plot type = 1, call this function to draw data sets on (x,Q) plane*)
(**)*)
(*PlotDataTypeOneTest[corrfxQdtaobsclassin_,pdfcorrin_,exptlistin_,xyrangein_,classifymodein_,configargumentsin_]:=*)
(*Module[{corrfxQdtaobsclass=corrfxQdtaobsclassin,pdfcorr=pdfcorrin,exptlist=exptlistin,classifymode=classifymodein,configarguments=configargumentsin,*)
(*plottype,expttype,*)
(*Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,(*UserArgName,UserArgValue,*)*)
(*XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,*)
(*ColorSeperator,*)
(*Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2,*)
(*lgdpos,xyrangeplot1,plotrange,*)
(*myplotsetting,imgsize,title,xtitle,ytitle,lgdlabel,xrange,yrange,epilog,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,myplotstyle,myMarker,*)
(*plot1,*)
(*xyrange=xyrangein,*)
(*groupnames,groupExptIDs,*)
(*ColorPaletterange,JobDescription,*)
(*Ncolor},*)
(**)
(*(*read arguments in config file*)*)
(*(*==============================*)*)
(*{Jobid,JobDescription(*20171128*),PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,(*UserArgName,UserArgValue,*)*)
(*XQfigureXrange,XQfigureYrange,ColorPaletterange(*20171128*),Hist1figureNbin,(*Hist1figureXrange,Hist1figureYrange,*)*)
(*(*ColorSeperator,*)*)
(*Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}=configarguments;*)
(*(*20171128: set hist xrange determined by the range of color palette, yrange alway auto*)*)
(*Hist1figureXrange=ColorPaletterange;*)
(*Hist1figureYrange={"auto","auto"};*)
(*(*=============================================================================================================================*)*)
(*(*plot type 1: generate plots============================================================================================================*)*)
(*(*=============================================================================================================================*)*)
(*{myplotsetting,imgsize,title,xtitle,ytitle,lgdlabel,xrange,yrange,epilog,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,myplotstyle,myMarker};*)
(**)
(*(*plot1*)*)
(**)
(*expttype="multi";*)
(*(*20170515 groups of data, legend is exptids in all groups, using Flatten, PDFname should be took cared*)*)
(*myplotsetting=setplotsetting[Flatten[corrfxQdtaobsclassin,1],exptlist//Flatten,expttype,1,"test","test"];*)
(*imgsize=myplotsetting[["imgsize"]];*)
(*title=myplotsetting[["title"]];*)
(*xtitle=myplotsetting[["xtitle"]];*)
(*ytitle=myplotsetting[["ytitle"]];*)
(*lgdlabel=myplotsetting[["lgdlabel"]];*)
(*xrange=myplotsetting[["xrange"]];*)
(*yrange=myplotsetting[["yrange"]];*)
(*epilog=myplotsetting[["epilog"]];*)
(*titlesize=myplotsetting[["titlesize"]];*)
(*xtitlesize=myplotsetting[["xtitlesize"]];*)
(*ytitlesize=myplotsetting[["ytitlesize"]];*)
(*lgdlabelsize=myplotsetting[["lgdlabelsize"]];*)
(*ticklablesize=myplotsetting[["ticklablesize"]];*)
(**)
(*myplotstyle=myplotsetting[["plotstyle"]];*)
(*myMarker=myplotsetting[["marker"]];*)
(**)
(*title="Experimental data in "<>PDFname<>" analysis";*)
(*lgdpos={0.25,0.725};*)
(*(*xyrangeplot1=plotrange;*)(*20170307 change it's name, avoid duplicate*)*)
(*(*20170515: consider expts in all groups*)*)
(*(*20170515: consider expts of all groups, so use Flatten[data,1] *)*)
(**)
(*(**)
(*Print["dim of p1: ",Dimensions[pdfcorr] ];*)
(*Print["dim of p1: ",Dimensions[Flatten[pdfcorr,1] ]];*)
(*Print["data of p1: ",Flatten[pdfcorr,1][[1]] ];*)
(*Print["data of p1: ",Flatten[pdfcorr,1][[2]] ];*)
(*Print["data of p1: ",Flatten[pdfcorr,1][[3]] ];*)
(*Print["data of p1: ",Flatten[pdfcorr,1][[4]] ];*)
(*PDFloglogplot[Flatten[pdfcorr,1]/.LF1\[Rule]List,myMarker,myplotstyle,title,xtitle,ytitle,xyrangeplot1,lgdlabel,lgdpos,imgsize];*)
(*Abort[];*)
(**)*)
(*(*20171126: use marker list*)*)
(*myMarker=PlotMarkerList[];*)
(*myMarker[[2]]=0.0075*myMarker[[2]];*)
(*myMarker=Transpose[myMarker,{2,1}];*)
(*(*20171130 set color for points, everytime the shape return to the first shape, change to another color*)*)
(*Ncolor={RGBColor[0.7, 0.2, 0.2], RGBColor[0.2, 0.2, 0.7], RGBColor[0.2, 0.7, 0.2], RGBColor[0.6, 0.4, 0.2], RGBColor[0.8, 0.4, 0.], *)
(* RGBColor[0.7, 0., 0.7], (*RGBColor[0.6, 0.6, 0.6],*) RGBColor[0.7, 0.7, 0.]};*)
(**)
(*(*myplotstyle=Table[Table[Ncolor[[icolor]],{imarker,Length[myMarker]}],{icolor,Length[Ncolor]}]//Flatten;*)*)
(*myplotstyle=Flatten[(Ncolor&/@Range[Length[myMarker] ])];(*20171201: color also rotation for every Expt ID*)*)
(*(*when all shapes are used out, return to the first one*)*)
(*myMarker=Flatten[(myMarker&/@Range[Length[Ncolor] ]),1];*)
(**)
(*Print["dim of data: ",Dimensions[pdfcorr] ];*)
(*Print["exptlist ",exptlist];*)
(*(*always don't show only one shape for all expt ID points*)*)
(*If[classifymode=="all",classifymode="single"];*)
(*{groupnames,groupExptIDs,(*groupdata*)pdfcorr}=ClassifyPlottedData[pdfcorr,exptlist,classifymode];*)
(*pdfcorr=Table[Flatten[pdfcorr[[igroup]],1],{igroup,Length[pdfcorr]}];*)
(*lgdlabel=*)
(*Switch[classifymode,"single",lgdlabel,"all",groupnames];*)
(**)
(*Print["dim of data: ",Dimensions[pdfcorr] ];*)
(**)
(*plot1=PDFloglogplot[(*Flatten[pdfcorr,1]*)pdfcorr,myMarker,myplotstyle,title,xtitle,ytitle,xyrange(*plot1*),lgdlabel,lgdpos,imgsize];*)
(**)
(*(*return*)*)
(**)
(*{plot1}*)
(**)
(*(**)
(*{myMarker,myplotstyle,title,xtitle,ytitle,xyrange,lgdlabel,lgdpos,imgsize}*)
(**)*)
(*]*)


(* ::Input:: *)
(*(*modify of 3: when plottype = 5, 6, extract data of that flavour*)*)
(*(*modify of 4: don't set local variable of corrfxQdtaobsclassin, avoiding time to copy large data to local variable, for mode 5,6 only deal with *)
(*corresponding flavour data (by flavourin)*)*)
(**)
(*(*20170515 this version corrfxQdtaobsclassin structure is {class1, class2,...}*)
(*it is for plotting different group of data in different point shapes*)
(**)*)
(**)
(*(*20171108: reorganize the function*)*)
(*(*use the new highlight range convention*)*)
(*getcorrinfo[corrfxQdtaobsclassin_,configargumentsin_,plottypein_,flavourin_]:=*)
(*Module[{(*corrfxQdtaobsclass=corrfxQdtaobsclassin,*)configarguments=configargumentsin,*)
(*plottype=plottypein,(*flavour=flavourin,*)flavour,*)
(*Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,*)
(*XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,*)
(*ColorSeperator,*)
(*Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2,*)
(*processes,ExptList1,pdfsetexps,processestitle,PDISNCtitle,NDISCCtitle,PDISNCCCtitle,PVBPZtitle,PVBPWtitle,PJPtitle,hDISNCtitle,hDISCCtitle,hVBPZtitle,pdfnamelable,PDFsetlabel,pdffile,corrfile,pdfcorr,pdfcorrdr,deltaR,textsize,Npttext,maxtext,maxmarker,mintext,minmarker,cuttext,epilogxQ,epilogxQcorr,corrtitle1,corrdrtitle1,deltaRtitle1,title2,obsname,title3,title4,title,xtitle,ytitle,xhisttitle,xhistabstitle,yhisttitle,plotrange,stretchx,stretchy,legendlabel,barseperator,binset,lineelement,plotrangex,SM,SM1,SM2,SM3,xQplotcorr ,histplotcorr1,histplotcorr2,xQplotcorrdr,histplotcorrdr1,histplotcorrdr2,xQplotdr,histplotdr2,myexpids,fmax,fmax2,*)
(*imgsize,(*title,xtitle,ytitle,*)lgdlabel,xrange,yrange,epilog,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,*)
(*myplotstyle,myMarker,*)
(*lgdpos,xyrangeplot1,*)
(*myplotsetting,plot1data,plot1,processesplot1order,*)
(*dummy1,dummy2,percentagetext,hist1plotrangex,histautoxrange,hist2autoxrange,hist2plotrangex,xQautorange,unhighlightsize,highlightrange,highlighttext,*)
(*exptlist,expttype,*)
(*rows,exptnames,exptnamestable,*)
(*lineelement2,maxdata,*)
(*barlegend,*)
(*histtitle,histabstitle,yhistabstitle,HistAutoMode,userdifinefuncfilename,*)
(*hist1plotrangey,hist2plotrangey,BinWidth,hist1plotrange,hist2plotrange,highlightlines,*)
(*xmintmp,xmaxtmp,ymintmp,hist1epilogtext,hist2epilogtext,hist1standardlines,hist2standardlines,LineWidth,HistAutoFixXrangeBool,*)
(*datemode,HistDataList,HistAbsDataList,DataMax,DataMin,AbsDataMax,AbsDataMin,DataMean,AbsDataMean,DataMedian,AbsDataMedian,DataSD,AbsDataSD,*)
(*ColorPaletteMode,PaletteMax,PaletteMin,*)
(*groupnames,groupExptIDs,classifymode,*)
(*ColorPaletterange,JobDescription,*)
(*shapeslist,abstitle,absPaletteMax,absbarseperator,absbarlegend,xQplotcorr2,exptnamestitle,*)
(*safewidth,datatype,output},*)
(**)
(*(*read arguments in config file*)*)
(*(*==============================*)*)
(*{Jobid,JobDescription(*20171128*),PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,(*UserArgName,UserArgValue,*)*)
(*XQfigureXrange,XQfigureYrange,ColorPaletterange(*20171128*),Hist1figureNbin,(*Hist1figureXrange,Hist1figureYrange,*)*)
(*(*ColorSeperator,*)*)
(*Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}=configarguments;*)
(*(*20171128: set hist xrange determined by the range of color palette, yrange alway auto*)*)
(*Hist1figureXrange=ColorPaletterange;*)
(*Hist1figureYrange={"auto","auto"};*)
(*(*should be three numbers, representing percentage, and from small to large, ex: 30, 50, 70*)*)
(*ColorSeperator={50,70,85};*)
(**)
(*(*20171109: seperate user difine function/data IO and configure file*)(*20171116: ->ReadUserFunctionV2, which read Expression from the file*)*)
(*(*20171119 new user function format: {{user name 1, user function 1}, {user name 2, user function 2}...}*)*)
(*userdifinefuncfilename="user_func.txt";*)
(*(**)
(*{UserArgName,UserArgValue}=ReadUserFunctionV2["./",userdifinefuncfilename];*)
(**)*)
(*(*20171127*)*)
(*If[*)
(*CorrelationArgFlag[[-1]]==1,*)
(*UserArgName=ReadUserFunctionV3["./",userdifinefuncfilename];*)
(*UserArgName=#[[1]]&/@UserArgName;*)
(*"dummy"*)
(*];*)
(**)
(*(*20171109: shorten the tiles of figures*)*)
(*If[PDFname=="2017.1008.0954.-0500_CT14HERA2-jet.ev",PDFname="CT14HERA2-jet.ev"];*)
(*(*=============================================================================================================================*)*)
(*(*Data organization============================================================================================================*)*)
(*(*=============================================================================================================================*)*)
(*(*read exptlist*)*)
(*exptlist={};*)
(*If[plottype==1  || plottype==5  || plottype==6,exptlist=Table[#[[iexpt,6]][["exptinfo","exptid"]],{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin ];*)
(*If[plottype==2  || plottype==3  || plottype==4,*)
(*exptlist=Table[#[[iexpt]][["exptinfo","exptid"]],{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin ];*)
(*(*test*)(*Print["expts: ",exptlist];*)*)
(**)
(*(*20171126 classify mode for data \[Rule] different shape for each group*)*)
(*classifymode=ClassifyMode;*)
(**)
(*(*********************************)*)
(*(*prepare for data input to processdataplotsmultiexp*)*)
(*(*********************************)*)
(*(*transf format from LF to LF1, since plot functions use LF1*)*)
(**)
(*(*if dr*corr or corr, data is [[iexpt,flavour]]*)*)
(*(*20170515: pdfcorr = {group1data, group2data, ...}, groupNdata = {LF1[x,Q,obs],...}*)*)
(*If[*)
(*plottype==5  || plottype==6,*)
(*fmax=Length[corrfxQdtaobsclassin[[1,1]] ];*)
(**)
(*(*data format \[Equal] {LF[x,Q,obs],...,...}, to LF1*)*)
(*pdfcorr=Table[#[[iexpt,flavourin+6]][["data"]]/.LF->LF1,{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin;*)
(*Print[Length[exptlist[[1]] ],"  ",Length[pdfcorr[[1]] ] ];(*test length data*)*)
(**)
(*(*20171126: classify exptid by defined groups with classifymode*)*)
(*{groupnames,groupExptIDs,(*groupdata*)pdfcorr}=ClassifyPlottedData[pdfcorr[[1]],exptlist[[1]],classifymode];*)
(*Print[Length[exptlist[[1]] ],"  ",Length[pdfcorr] ];(*test length data*)*)
(*(*pdfcorr=Table[Flatten[pdfcorr[[igroup]],1],{igroup,Length[pdfcorr]}];*)*)
(**)
(*(*merge all experimental data into one, for all flavours*)*)
(*(*ex: corrdataforplot[[iexpt,flavour,Npt]] \[Rule] orrdataforplot[[flavour,Npt]]*)*)
(*(*{pdfcorr ,dummy1,dummy2}=mergeplotdata[{pdfcorr ,pdfcorr,pdfcorr}];*)*)
(*pdfcorr=Flatten[#,1]&/@pdfcorr;*)
(*Print[Length[exptlist[[1]] ],"  ",Length[pdfcorr]," ",Length[groupnames]," ",Length[groupExptIDs] ];(*test length data*)*)
(**)
(*(* deletezerovalue: delete data with 0 value (0 value usually from mb, mc cut)*)*)
(*(*{pdfcorr ,dummy1,dummy2}=deletezerovalue[{pdfcorr ,pdfcorr,pdfcorr}];*)*)
(*pdfcorr=Table[Select[pdfcorr[[igroup]],#[[3]]!=0&],{igroup,1,Length[pdfcorr]}];*)
(*"dummy"*)
(*];*)
(**)
(*(*data of [[iexpt]]*)*)
(*If[*)
(*plottype==2 || plottype==3 || plottype==4,*)
(*(*take data, and format from LF to LF1 (LF1 is format to input to plot functions)*)*)
(*pdfcorr=Table[#[[iexpt]][["data"]]/.LF->LF1,{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin;*)
(*(*20171108 expt error ratio values should be absolute value*)*)
(*If[plottype==2,pdfcorr=pdfcorr/.LF1[a__]:>LF1[{a}[[1]],{a}[[2]],Abs[{a}[[3]] ] ] ];*)
(**)
(*(*20171126: classify exptid by defined groups with classifymode*)*)
(*{groupnames,groupExptIDs,(*groupdata*)pdfcorr}=ClassifyPlottedData[pdfcorr[[1]],exptlist[[1]],classifymode];*)
(**)
(*(*merge all data into one*)*)
(*pdfcorr=Flatten[#,1]&/@pdfcorr;*)
(*"dummy"*)
(*];*)
(*(*test print*)(*Print[pdfcorr ];Print[pdfcorr/.LF1->LF2 ];Print[pdfcorr/.LF1[a__]:>{a}[[2]] ];*)*)
(**)
(*If[*)
(*plottype==1,*)
(*fmax=Length[corrfxQdtaobsclassin[[1,1]] ];*)
(**)
(*(*set {corr[[flavour]],drcorr[[flavour]],dr[[flavour]]}*)*)
(*(*they are used to  input into processdataplotsmultiexp*)*)
(*(*data format \[Equal] {LF[x,Q,obs],...,...}*)*)
(*pdfcorr=Table[Datamethods[["take"]][#[[iexpt,flavourin+6]],2][["data"]]/.LF->LF1,{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin;*)
(*];*)
(**)
(*(*20171202: if the "other" type has no expt ID, delete it*)*)
(*If[Length[groupExptIDs[[-1]] ]==0,{groupnames,groupExptIDs,(*groupdata*)pdfcorr}=Drop[#,-1]&/@{groupnames,groupExptIDs,(*groupdata*)pdfcorr}];*)
(**)
(*Print[Length[exptlist[[1]] ],"  ",Length[pdfcorr] ];(*test length data*)*)
(*(*=============================================================================================================================*)*)
(*(*Statistical quantities of data============================================================================================================*)*)
(*(*=============================================================================================================================*)*)
(*(*20171115*)*)
(*{HistDataList,HistAbsDataList,DataMax,DataMin,AbsDataMax,AbsDataMin,DataMean,AbsDataMean,DataMedian,AbsDataMedian,DataSD,AbsDataSD};*)
(*If[*)
(*plottype==2 || plottype==3 || plottype==4 || plottype==5 || plottype==6,*)
(*HistDataList=Flatten[pdfcorr]/.LF1[a__]:>{a}[[3]];*)
(*HistAbsDataList=Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ];*)
(*DataMax=Max[HistDataList];*)
(*DataMin=Min[HistDataList];*)
(*AbsDataMax=Max[HistAbsDataList];*)
(*AbsDataMin=Min[HistAbsDataList];*)
(*DataMean=Mean[HistDataList];*)
(*AbsDataMean=Mean[HistAbsDataList];*)
(*DataMedian=Median[HistDataList];*)
(*AbsDataMedian=Median[HistAbsDataList];*)
(*DataSD=StandardDeviation[HistDataList];*)
(*AbsDataSD=StandardDeviation[HistAbsDataList];*)
(*"dummy"*)
(*];*)
(*(*=============================================================================================================================*)*)
(*(*Highlight range setting============================================================================================================*)*)
(*(*=============================================================================================================================*)*)
(**)
(*(*for no highlight mode, choose size of data point in plot by Size*)
(*for highlight mode, set size of unhighlighted data as Size, size of highlighted data is larger than Size*)*)
(*(*==============================*)*)
(**)
(*highlightrange=*)
(*Switch[*)
(*HighlightMode[[plottype]],*)
(*0,{{0.0,0.0}},*)
(*(*20171109: use new highlight range convention*)*)
(*1,(*{HighlightMode1[[2*plottype-1]],HighlightMode1[[2*plottype]]}*)HighlightMode1[[plottype]],*)
(*(*20171111: percentage highlight: don't take absolute values for data*)*)
(*(*20171201: percentage highlight depends on drawing data, if draw |data|, use percentage of |data|*)*)
(*2,Switch[*)
(*FigureFlag[[plottype]],*)
(*-1,*)
(*GetDataPercentage[(*Flatten[pdfcorr]/.LF1[a__]\[RuleDelayed]{a}[[3]]*)HistDataList,(*{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}*)#]&/@HighlightMode2[[plottype]],*)
(*1,*)
(*GetDataPercentage[(*Flatten[pdfcorr]/.LF1[a__]\[RuleDelayed]{a}[[3]]*)HistAbsDataList,(*{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}*)#]&/@HighlightMode2[[plottype]],*)
(*_,*)
(*Print["error, figure flag should be 1 or -1"];Abort[]*)
(*]*)
(*(**)
(*Which[*)
(*plottype\[Equal]5  || plottype\[Equal]6,*)
(*GetDataPercentage[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ,{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}],*)
(* plottype\[Equal]2 || plottype\[Equal]3 || plottype\[Equal]4,*)
(*GetDataPercentage[pdfcorr/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ,{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}],*)
(*True,Print["presently plottype is only 2~6 "];Abort[]*)
(*]*),*)
(*_,Print["error, highlight mode should be 0, 1, 2"];Abort[]*)
(*];*)
(*(*20171201 for percentage highlight the largest one should have a range of width to include the highest percentage point*)*)
(*If[*)
(*HighlightMode[[plottype]]==2,*)
(*Table[*)
(*safewidth=0.000001*(highlightrange[[iHL,2]]-highlightrange[[iHL,1]]);*)
(*highlightrange[[iHL,1]]=highlightrange[[iHL,1]]-safewidth;*)
(*highlightrange[[iHL,2]]=highlightrange[[iHL,2]]+safewidth,*)
(*{iHL,Length[highlightrange]}*)
(*];*)
(*"dummy" *)
(*];*)
(*(*******************************************************************************)*)
(*pdfnamelable={"bbar(x,Q)","cbar(x,Q)","sbar(x,Q)","dbar(x,Q)","ubar(x,Q)","g(x,\[Mu])","u(x,\[Mu])","d(x,\[Mu])","s(x,\[Mu])","c(x,\[Mu])","b(x,\[Mu])"};*)
(*(*20171127: only add new string when user function is mode on*)*)
(*If[*)
(*CorrelationArgFlag[[-1]]==1,*)
(*pdfnamelable=pdfnamelable~Join~{Sequence@@UserArgName(*20171119 change to multi-user functions*)};*)
(*"dummy"*)
(*];*)
(*(*20171202 select highlighted points*)*)
(*(*20171202 if plot type flag = -1, show statistics of |data|*)*)
(*datatype={"xQ plot","|sigma/D|","r (residuals)","deltaR (uncertainties of residuals)","Sens(r, f(x,Q))","Corr(r, f(x,Q))"};*)
(*datatype=datatype[[plottype]];*)
(**)
(*If[*)
(*FigureFlag[[plottype]]==1,*)
(*pdfcorr=pdfcorr/.LF1[a__]:>LF1[{a}[[1]],{a}[[2]],Abs[{a}[[3]] ] ];*)
(*If[plottype==3 || plottype==5 || plottype==6,datatype="|"<>datatype<>"|"];*)
(*"dummy"*)
(*];*)
(**)
(*Table[*)
(*pdfcorr[[iexpt]]=*)
(*Select[*)
(*pdfcorr[[iexpt]],*)
(*Length[Intersection[Table[If[(#[[3]]>=highlightrange[[i,1]] && #[[3]]<highlightrange[[i,2]]),True,False],{i,Length[highlightrange]}],{True}] ]>0&*)
(*],*)
(*{iexpt,Length[pdfcorr]}*)
(*];*)
(**)
(*Print[Length[exptlist[[1]] ]," ",Length[pdfcorr] ];*)
(*(*output*)*)
(**)
(*(*if |data|, define get statistical quantities of |data|*)*)
(*If[FigureFlag[[plottype]]==1,DataMax=AbsDataMax;DataMin=AbsDataMin;DataMean=AbsDataMean;DataMedian=AbsDataMedian;DataSD=AbsDataSD];*)
(*(*output {{labels},{values or descriptions}}*)*)
(**)
(*If[*)
(*(plottype==5 || plottype==6),*)
(*output=*)
(*{*)
(*{"datatype","flavour","Expts","DataMax","DataMin","DataMean","DataMedian","DataSD","highlightrange","highlighted data"},*)
(*{datatype,pdfnamelable[[flavourin+6]],{#[[1]],ExptIDtoName[#[[1]] ]}&/@groupExptIDs,DataMax,DataMin,DataMean,DataMedian,DataSD,highlightrange,Transpose[{{#[[1]],ExptIDtoName[#[[1]] ]}&/@groupExptIDs,pdfcorr},{2,1}]}*)
(*}*)
(*];*)
(*If[*)
(*(plottype==2 || plottype==3 || plottype==4),*)
(*output=*)
(*{*)
(*{"datatype",(*"flavour",*)"Expts","DataMax","DataMin","DataMean","DataMedian","DataSD","highlightrange","highlighted data"},*)
(*{datatype,(*pdfnamelable[[flavourin+6]],*){#[[1]],ExptIDtoName[#[[1]] ]}&/@groupExptIDs,DataMax,DataMin,DataMean,DataMedian,DataSD,highlightrange,Transpose[{{#[[1]],ExptIDtoName[#[[1]] ]}&/@groupExptIDs,pdfcorr},{2,1}]}*)
(*}*)
(**)
(*];*)
(**)
(*output*)
(*(**)
(*groupExptIDs*)
(**)*)
(*(**)
(*{exptlist,highlightrange,pdfcorr}*)
(**)*)
(*]*)


(* ::Input:: *)
(*getcorrinfo[{dRdataclassfinal},readcorrconfigfile6[configDir,configfilename],4,flavour];*)
(*getcorrinfo[{corrdataclassfinal},readcorrconfigfile6[configDir,configfilename],6,0 ];*)


(* ::Input:: *)
(*Select[getcorrinfo[{corrdataclassfinal},readcorrconfigfile6[configDir,configfilename],6,0 ][[3,25]],(#[[3]]>0.7 || #[[3]]<-0.7)&] *)


(* ::Input:: *)
(*Export["../plots/"<>jobpath<>"test_data_info.txt",(getcorrinfo[{corrdataclassfinal},readcorrconfigfile6[configDir,configfilename],6,0 ]//TableForm)//OutputForm]*)


(* ::Input:: *)
(*DirectoryQ["../plots/"<>jobpath]*)


(* ::Input:: *)
(*jobpath*)


(* ::Input:: *)
(*Directory[]*)


(* ::Input:: *)
(*getdatainfotext[corrfxQdtaobsclassin_,configargumentsin_,plottypein_]:=*)
(*Module[{configarguments=configargumentsin,*)
(*plottype=plottypein,*)
(*Jobid,JobDescription(*20171128*),PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,(*UserArgName,UserArgValue,*)*)
(*XQfigureXrange,XQfigureYrange,ColorPaletterange(*20171128*),Hist1figureNbin,(*Hist1figureXrange,Hist1figureYrange,*)*)
(*(*ColorSeperator,*)*)
(*Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2,*)
(*alldatainfo,fmax,userdifinefuncfilename,UserArgName},*)
(**)
(*(*read arguments in config file*)*)
(*(*==============================*)*)
(*{Jobid,JobDescription(*20171128*),PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,(*UserArgName,UserArgValue,*)*)
(*XQfigureXrange,XQfigureYrange,ColorPaletterange(*20171128*),Hist1figureNbin,(*Hist1figureXrange,Hist1figureYrange,*)*)
(*(*ColorSeperator,*)*)
(*Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}=configarguments;*)
(*(*==============================*)*)
(*userdifinefuncfilename="user_func.txt";*)
(*(*20171127*)*)
(*If[*)
(*CorrelationArgFlag[[-1]]==1,*)
(*UserArgName=ReadUserFunctionV3["./",userdifinefuncfilename];*)
(*UserArgName=#[[1]]&/@UserArgName;*)
(*"dummy"*)
(*];*)
(**)
(*fmax=Length[corrfxQdtaobsclassin[[1,1]] ];*)
(**)
(*alldatainfo={};*)
(**)
(*Table[*)
(*If[*)
(*CorrelationArgFlag[[iflavour+6]]==1,*)
(*alldatainfo=Append[alldatainfo,getcorrinfo[corrfxQdtaobsclassin,configarguments,plottype,iflavour] ]*)
(*];*)
(*"dummy"*)
(*,{iflavour,-5,-5+fmax-1}*)
(*];*)
(**)
(*alldatainfo*)
(*]*)
(**)
(*datainfototext[corrfxQdtaobsclassin_,configargumentsin_,plottypein_,flavourin_]:=*)
(*Module[{(*corrfxQdtaobsclass=corrfxQdtaobsclassin,*)configarguments=configargumentsin,*)
(*plottype=plottypein,(*flavour=flavourin,*)flavour,*)
(*input,Nlabel,infostring,Nexpt,exptID,exptdata,xqvaluestr*)
(*},*)
(**)
(*input=getcorrinfo[corrfxQdtaobsclassin,configarguments,plottype,flavourin];*)
(*Nlabel=Length[input[[1]] ];*)
(**)
(*infostring="";*)
(*(*only show the meta data part*)*)
(*Table[*)
(*infostring=infostring<>ToString[input[[1,ilabel]] ]<>": "<>ToString[input[[2,ilabel]] ]<>"\n",*)
(*{ilabel,Nlabel-1}*)
(*];*)
(*(*show the data part, format: {{exptname, data with in the highlight range},...}*)*)
(*infostring=infostring<>ToString[input[[1,-1]] ]<>"\n";*)
(**)
(*Nexpt=Length[input[[2,-1]] ];*)
(*Table[*)
(*exptID=input[[2,-1,iexpt,1]]//ToString;*)
(*xqvaluestr="{x, Q, value}";*)
(*exptdata=(ToString[#]<>"\n")&/@(input[[2,-1,iexpt,2]]/.LF1->List);*)
(*infostring=infostring<>exptID<>"\n"<>xqvaluestr<>"\n"<>exptdata<>"\n";*)
(*"dummy",*)
(*{iexpt,Nexpt}*)
(*];*)
(**)
(*infostring*)
(*]*)


(* ::Input:: *)
(*getcorrinfo[{corrdataclassfinal},readcorrconfigfile6[configDir,configfilename],6,0 ][[2,-1,1,1]]*)


(* ::Input:: *)
(*ReadLisFile[datalistFile];*)


(* ::Input:: *)
(*ExptIDtoName[101]*)


(* ::Input:: *)
(*getcorrinfo[{corrdataclassfinal},readcorrconfigfile6[configDir,configfilename],6,-5][[2,2,1]]*)


(* ::Input:: *)
(*Export[*)
(*"../plots/"<>jobpath<>"test_data_info.txt",*)
(*datainfototext[{corrdataclassfinal},readcorrconfigfile6[configDir,configfilename],6,-5]*)
(*]*)


(* ::Input:: *)
(*Export[*)
(*"../plots/"<>jobpath<>"test_data_info.txt",*)
(*Table[datainfototext[{corrdataclassfinal},readcorrconfigfile6[configDir,configfilename],6,iflavour],{iflavour,-5,5}]//StringJoin*)
(*]*)


(* ::Input:: *)
(*getdatainfotext[{corrdataclassfinal},readcorrconfigfile6[configDir,configfilename],6]//Length*)


(* ::Input:: *)
(*corrdataclassfinal//Dimensions*)


(* ::Input:: *)
(*corrdataclassfinal[[2,8]]*)


(* ::Input:: *)
(**)
(*{Jobid,JobDescription(*20171128*),PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,(*UserArgName,UserArgValue,*)*)
(*XQfigureXrange,XQfigureYrange,ColorPaletterange(*20171128*),Hist1figureNbin,(*Hist1figureXrange,(*Hist1figureYrange*)dummy12,*)*)
(*(*ColorSeperator,*)*)
(*Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}=*)
(*(*readcorrconfigfile4*)readcorrconfigfile6[configDir,configfilename];*)
(*If[*)
(*FigureFlag[[3]]==1 || FigureFlag[[3]]==-1,*)
(*Print["making plot of figure type ",FigureType[[3]],", flavour = ",flavour];*)
(*p234=processdataplotsmultiexp7percentage[{residualdataclassfinal},readcorrconfigfile6[configDir,configfilename],3,flavour];*)
(*(*20171202 add important data info*)*)
(*datainfostr=datainfototext[{residualdataclassfinal},readcorrconfigfile6[configDir,configfilename],3,flavour];*)
(*(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)*)
(*Table[*)
(**)
(*filename=obsname[[3]]<>"_"<>representationname[[5]]<>"_samept"<>extensionname[[iext[[i]] ]];*)
(*Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,2]],ImageResolution->imgresol  ];*)
(*(*20171201 for +1: absolute values of data, for -1: sign data*)*)
(*If[*)
(*FigureFlag[[3]]==-1,*)
(*filename=obsname[[3]]<>"_"<>representationname[[1]]<>"_samept"<>extensionname[[iext[[i]] ]];*)
(*Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]],ImageResolution->imgresol  ];*)
(*filename=obsname[[3]]<>"_"<>representationname[[3]]<>"_samept"<>extensionname[[iext[[i]] ]];*)
(*Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,1]],ImageResolution->imgresol ];*)
(*"dummy"*)
(*];*)
(*If[*)
(*FigureFlag[[3]]==1,*)
(*filename=obsname[[3]]<>"_"<>representationname[[2]]<>"_samept"<>extensionname[[iext[[i]] ]];*)
(*Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]],ImageResolution->imgresol  ];*)
(*filename=obsname[[3]]<>"_"<>representationname[[4]]<>"_samept"<>extensionname[[iext[[i]] ]];*)
(*Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,2]],ImageResolution->imgresol ];*)
(*"dummy"*)
(*];*)
(**)
(*"dummy",*)
(*{i,Length[iext]}*)
(*];*)
(**)
(*(*20171103: add files storing mathematica expressions so that users can change details of every figure*)*)
(**)
(*filename=obsname[[3]]<>"_"<>representationname[[5]]<>"_samept"<>".m";*)
(*Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,2]]  ];*)
(*(*20171201 for +1: absolute values of data, for -1: sign data*)*)
(*If[*)
(*FigureFlag[[3]]==-1,*)
(*filename=obsname[[3]]<>"_"<>representationname[[1]]<>"_samept"<>".m";*)
(*Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]]  ];*)
(*filename=obsname[[3]]<>"_"<>representationname[[3]]<>"_samept"<>".m";*)
(*Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,1]] ];*)
(*(*20171202 add important data info*)*)
(*filename=obsname[[3]]<>"_"<>representationname[[7]]<>"_samept"<>".txt";*)
(*Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,datainfostr  ];*)
(*"dummy"*)
(*];*)
(*If[*)
(*FigureFlag[[3]]==1,*)
(*filename=obsname[[3]]<>"_"<>representationname[[2]]<>"_samept"<>".m";*)
(*Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]]  ];*)
(*filename=obsname[[3]]<>"_"<>representationname[[4]]<>"_samept"<>".m";*)
(*Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,2]] ];*)
(*(*20171202 add important data info*)*)
(*filename=obsname[[3]]<>"_"<>representationname[[8]]<>"_samept"<>".txt";*)
(*Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,datainfostr  ];*)
(*"dummy"*)
(*];*)
(**)
(**)
(*];*)


(* ::Input:: *)
(*representationname*)


(* ::Input:: *)
(*representationname={"xQ-1","xQ+1","hist-1","hist+1","legend","xQ","info-1","info+1"};*)


(* ::Input:: *)
(*(*20171202 for plot type = 5 or 6, write data info into files, if flavour flag = 1, record the info*)*)
(*If[*)
(*FigureFlag[[6]]==1 || FigureFlag[[6]]==-1,*)
(*(*filename*)*)
(*filename=*)
(*Switch[*)
(*FigureFlag[[6]],*)
(*-1,*)
(*obsname[[6]]<>"_"<>representationname[[7]]<>"_"<>"_samept_info"<>".txt",*)
(*1,*)
(*obsname[[6]]<>"_"<>representationname[[8]]<>"_"<>"_samept_info"<>".txt"*)
(*];*)
(*(*info string*)*)
(*datainfostr=*)
(*Table[*)
(*If[*)
(*CorrelationArgFlag[[flavour+6]]==1,*)
(*datainfototext[{corrdataclassfinal},readcorrconfigfile6[configDir,configfilename],6,flavour],*)
(*""*)
(*],*)
(*{flavour,-5,-5+fmax-1}*)
(*];*)
(*(*export the info file*)*)
(*(*20171202 add important data info*)*)
(*Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,datainfostr//StringJoin  ];*)
(**)
(*"dummy"*)
(*];*)
(**)


(* ::Input:: *)
(*saveparentpath<>jobpath*)
(*PDFxQSelectMethod*)
(*".pdf"*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*BranchMode*)


(* ::Input:: *)
(*dRcorrdataclassfinal[[1,-2+6]][["data"]]*)


(* ::Input:: *)
(*(*20171213 redefine sensitivity as (\[Delta]r/r)*Corr(Subscript[r, i],fSubscript[(x,Q), i])*)*)
(*testupdatesens=*)
(*Table[*)
(*Npt=dRcorrdataclassfinal[[iexpt,flavour+6]][["data"]]//Length;*)
(*tmpdata=dRcorrdataclassfinal[[iexpt,flavour+6]];*)
(*Table[*)
(**)
(*tmpdata[["data"]][[ipt,3]]=(dRcorrdataclassfinal[[iexpt,flavour+6]][["data"]][[ipt,3]]/*)
(*(*prevent the dr/r blow up when r = 0*)*)
(*If[residualdataclassfinal[[iexpt]][["data"]][[ipt,3]]!=0.0,residualdataclassfinal[[iexpt]][["data"]][[ipt,3]],10.0^-8]);*)
(*"dummy",*)
(*{ipt,Npt}*)
(*];*)
(*tmpdata ,*)
(*{iexpt,1,Length[residualNsetclassfinal]},{flavour,-5,-5+fmax-1}*)
(*]*)


(* ::Input:: *)
(*dRcorrdataclassfinal[[1,0+6]][["data"]][[5,3]]*)


(* ::Input:: *)
(*Table[*)
(*testupdatesens[[iexpt,6]],*)
(*{iexpt,1,(*Length[residualNsetclassfinal]*)1}*)
(*]*)
(*dRcorrdataclassfinal[[1,6]][["data"]]/.LF[a__]:>{a}[[3]]*)


(* ::Input:: *)
(*testupdatesens[[10,5]]*)


(* ::Input:: *)
(*dRcorrdataclassfinal[[2,1+6]]*)


(* ::Input:: *)
(*dtacentralclassfinal[[3]][[1]]*)
(*dtacentralclassfinal[[3]][[2]];*)
(*dtacentralclassfinal[[3]][[3]]*)
(*dtacentralclassfinal[[3]][[4]]*)
(*dtacentralclassfinal[[3]][[5]]*)
(*dtacentralclassfinal[[#]][[5,5]]&/@Range[1,49]*)


(* ::Input:: *)
(*dtacentralclassfinal[[3]][[2]]//Length*)


(* ::Input:: *)
(*Position[dtacentralclassfinal[[5]],"ReducedChi2"][[]]*)


(* ::Input:: *)
(*dtacentralclassfinal[[1]][["label"]]*)


(* ::Input:: *)
(*Position[dtacentralclassfinal[[1]][["label"]],"ReducedChi2"][[1,1]]//Head*)


(* ::Input:: *)
(*dtacentralclassfinal[[1]][["data"]]*)


(* ::Input:: *)
(*NormalizeIDs*)


(* ::Input:: *)
(**)
(*Table[*)
(*{residualNsetclassfinal[[iexpt]][["exptinfo","exptid"]],NormalizeIDs[[iexpt]]},*)
(*{iexpt,1,Length[residualNsetclassfinal]}*)
(*]*)


(* ::Input:: *)
(*fxQsamept2classfinal[[#,1]][["exptinfo","exptid"]]&/@Range[49]*)


(* ::Input:: *)
(*fxQsamept2classfinal[[21,5]][["data"]]*)


(* ::Input:: *)
(*corrdataclassfinal;*)
(*dRcorrdataclassfinal;*)
(*{expterrordataclassfinal,residualdataclassfinal,dRdataclassfinal};*)


(* ::Input:: *)
(*Table[corrdataclassfinal[[iexpt,flavour]][["data"]]//Length,{iexpt,49},{flavour,17}]*)


(* ::Input:: *)
(*corrdataclassfinal[[-14,14]][["data"]]*)


(* ::Input:: *)
(*(PlotMarkerList[][[1]]&/@Range[10])//Flatten*)


(* ::Input:: *)
(*Table[dRcorrdataclassfinal[[iexpt,iflavour]][["data"]]//Length,{iexpt,8},{iflavour,Length[dRcorrdataclassfinal[[1]] ]}]*)


(* ::Input:: *)
(*Table[Count[( ( (dRcorrdataclassfinal[[iexpt,iflavour]][["data"]])/.LF->List)//Flatten),0.0],{iexpt,8},{iflavour,Length[dRcorrdataclassfinal[[1]] ]}]*)


(* ::Input:: *)
(*dRcorrdataclassfinal[[4,3]][["data"]][[43]]*)


(* ::Input:: *)
(*residualNsetclassfinal[[4]][["data"]][[43]]*)
(*fxQsamept2classfinal[[4,3]][["data"]][[43]]*)
(*fxQsamept2classfinal[[4,13]][["data"]][[43]]*)
(*fxQsamept2classfinal[[4,13]][["data"]][[43]]*)


(* ::Input:: *)
(*N[((dtacentralclassfinal[[#]][["rawdata"]][[5]])/(residualNsetclassfinal[[#]][["data"]]//Length))]&/@Range[8]*)


(* ::Input:: *)
(*Position[dtacentralclassfinal[[1]][["label"]],"ipt"][[1,1]]*)


(* ::Input:: *)
(*corrdataclassfinal[[1,6]]*)


(* ::Input:: *)
(*datainfototext[{corrdataclassfinal},{dtacentralclassfinal},readcorrconfigfile6[configDir,configfilename],6,0]*)


(* ::Input:: *)
(*datainfototext[{corrdataclassfinal},{dtacentralclassfinal},readcorrconfigfile6[configDir,configfilename],6,0]*)


(* ::Input:: *)
(*datainfototext[{expterrordataclassfinal},{dtacentralclassfinal},readcorrconfigfile6[configDir,configfilename],2,flavour]*)


(* ::Input:: *)
(*datainfostr=datainfototext[{dRdataclassfinal},{dtacentralclassfinal},readcorrconfigfile6[configDir,configfilename],2,flavour]*)


(* ::Input:: *)
(*dRdataclassfinal//Dimensions*)
(*expterrordataclassfinal//Dimensions*)
(*dRdataclassfinal[[1]]*)
(*expterrordataclassfinal[[1]]*)


(* ::Input:: *)
(*datainfototext[{residualdataclassfinal},{dtacentralclassfinal},readcorrconfigfile6[configDir,configfilename],3,flavour]*)


(* ::Input:: *)
(*Get["corr_proj_funcs.m"]*)


(* ::Input:: *)
(*processdataplotsmultiexp7percentage[{dRcorrdataclassfinal},readcorrconfigfile6[configDir,configfilename],5,-1]*)


(* ::Input:: *)
(*Get["corr_proj_funcs.m"]*)


(* ::Input:: *)
(*processdataplotsmultiexp8percentage[{dRcorrdataclassfinal},readcorrconfigfile6[configDir,configfilename],5,-1]*)
(*processdataplotsmultiexp8percentage[{dRcorrdataclassfinal},readcorrconfigfile6[configDir,configfilename],5,5]*)
(*processdataplotsmultiexp8percentage[{dRdataclassfinal},readcorrconfigfile6[configDir,configfilename],4,-1]*)
(*processdataplotsmultiexp8percentage[{expterrordataclassfinal},readcorrconfigfile6[configDir,configfilename],2,-1]*)
