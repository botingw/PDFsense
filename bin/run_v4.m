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
SetDirectory[NotebookDirectory[](*DirectoryName[$InputFileName]*) ]
Get["corr_proj_funcs.m"]


(* ::Section:: *)
(*read correlation (and other data of FigureType in config1.txt) from the data in files*)


(* ::Input::Initialization:: *)

Quicksaveplot[]:=
Module[{(*runfunc,figureDir,myPDFsetDir,PDFsetmethod,PDFname,PDFDataDir,datalistFile,expttype,exptid*)
Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,
ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2
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

(*20170301: new config file
{runfunc,figureDir,dummy1,dummy2,PDFname,dummy3,datalistFile,expttype,exptid}=
readcorrconfigfile[configDir,configfilename];
*)
(*new config file*)
{Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,(*Hist1figureYrange*)dummy12,
ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}=
readcorrconfigfile4[configDir,configfilename];
Print["input arguments: ",readcorrconfigfile4[configDir,configfilename] ];

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

(*bar seperator input has only 3 elements*)
If[Length[ColorSeperator]!=3,Print["color seperator percentage should be three numbers"];Abort[]];
(*should be small to large, ex: 30, 50, 70*)
If[Sort[ColorSeperator]!=ColorSeperator,Print["color seperator percentage should from small to large"];Abort[]];
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
Print["filenames of data"];
Print["Directory: ",CorrDataDir];
(*20171101 
Print["corrdataclass: ","corr_"<>CorrDataFile,"\nexpterrordataclass: ","expterror_"<>CorrDataFile,"\nresidualdataclass: ","residual_"<>CorrDataFile,"\ndRdataclass: ","dR_"<>CorrDataFile,"\ndRcorrdataclass: ","dRcorr_"<>CorrDataFile,"\nresidualNsetdataclass: ","residualNset_"<>CorrDataFile,"\n"];
*)
(*20171101 change  data format*)
Print["residualNsetclass: ","residualNset_"<>CorrDataFile,"\nfxQsamept2class: ","fxQNset_"<>CorrDataFile,"\ndtacentralclass: ","dtacentral_"<>CorrDataFile,"\n"];

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
If[Length[exptlistnotfound]!=0,Print["the exptid = ",exptlistnotfound,"are not found in database"]];

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


Print["expt list: ",exptlist];
Print["expt list in the ./quick_data: ",exptlistfinal];
(*for run_loopexpts_v4.nb: users want to generate figures of all Expt ID in the config1.txt, so we don't want the program breaks down when some Expt IDs in config.txt are not in data files*)
(*when we find exptlistfinal contains no expt ID, we skip this loop*)
If[Length[exptlistfinal]==0,Print["Expt IDs in config.txt are not in data files"];Return["end Quicksaveplot"] ];


(*test corrfxQdtaobsclassfinal*)
Print["data of final expts"];
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
(*clear residualNsetclass, fxQsamept2class, dtacentralclass*)
Clear[residualNsetclass];Clear[fxQsamept2class];Clear[dtacentralclass];

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

(*check the # of user define values is the same as # of PDF replicas*)
If[
Length[UserArgValue]!=(Datamethods[["getNcolumn"]][fxQsamept2classfinal[[1,1]] ]-2),
Print["error, the # of user-defined values should be the same as # of PDF relicas (Nset)"];
Print["Nset = ",(Datamethods[["getNcolumn"]][fxQsamept2classfinal[[1,1]] ]-2) ];
Print["#user-defined values = ", Length[UserArgValue] ];
Abort[] 
];
(*append data of uservalue to fxQsamept2classfinal for each expt ID*)
Table[
tmpclass=fxQsamept2classfinal[[iexpt,1]];
tmpclass[["data"]]=tmpclass[["data"]]/.LF[a__]:>LF[{a}[[1]],{a}[[2]],Sequence@@UserArgValue];
fxQsamept2classfinal[[iexpt]]=Append[fxQsamept2classfinal[[iexpt]],tmpclass];
tmpclass,
{iexpt,1,Length[fxQsamept2classfinal]}
];
];

fmax=Length[fxQsamept2classfinal[[1]] ];
Print["total #flavours: ",fmax];

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

(*calculate central value of residual*)
(*select x, Q, central value of residual (iset==1)*)
residualdataclassfinal=
Table[
Datamethods[["take"]][residualNsetclassfinal[[iexpt]],{1,3}],
{iexpt,1,Length[residualNsetclassfinal]}
];

(*test the time of the correlation value calculation*)
tf=AbsoluteTime[];
GetCorrvalueTime=tf-ti;
NCorrval=
Sum[
Datamethods[["getNpt"]][corrdataclassfinal[[iexpt,flavour+6]] ],
{iexpt,1,Length[residualNsetclassfinal]},{flavour,-5,-5+fmax-1}
];
Print["total number of correlation & sensitivity value calculations is ",NCorrval];
Print["average time of one correlation + sensitivity value calculations is roughly ",GetCorrvalueTime/NCorrval];
Print["time of calculating all observable data (correlation + sensitivity + ...)  values is ",GetCorrvalueTime];

(*calculate experimental error ratio*)
(*check columns used to extract Subscript[\[Sigma], i]/Subscript[D, i] is correct (check the labels are (x,Q,TotErr/Exp) ) *)
(*check whether the elements used to calculate Expt error/expt are correct*)
ExpIndex=4;ExpErrIndex=6;XIndex=-3;QIndex=-2;
Print["begin to calculate \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)/\!\(\*SubscriptBox[\(D\), \(i\)]\) from data, labels of indexes used for each experiment:"];

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
pdfnameexpttypeDir=PDFname<>"_"<>expttype<>"/";
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
representationname={"xQ","legend","hist1","hist2"};
extensionname={".eps",".png",".pdf",".jpg"};

(*if directory does not exist, create it*)
If[
DirectoryQ[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath]==False,
CreateDirectory[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath];
"dummy"
];

iext=2;(*export eps*)(*20171105: use .png for python script interface version*)
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
jpgtime=
AbsoluteTiming[

Table[
Print["now flavour = ",flavour];
If[
CorrelationArgFlag[[flavour+6]]==1,
If[
(*correlation plots*)
FigureFlag[[6]]==1,
Print["making plot of figure type ",FigureType[[6]],", flavour = ",flavour];
p6=processdataplotsmultiexp6percentage[{corrdataclassfinal},readcorrconfigfile4[configDir,configfilename],6,flavour ];
(*add exptname table into output figure*)

(*p6=GraphicsGrid[p6,Spacings\[Rule]Scaled[0.15] ];*)

filename=obsname[[6]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[1,1]],ImageResolution->imgresol ];
filename=obsname[[6]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[1,2]],ImageResolution->imgresol ];
filename=obsname[[6]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[2,1]],ImageResolution->imgresol ];
filename=obsname[[6]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[2,2]],ImageResolution->imgresol ];

(*20171103: add files storing mathematica expressions so that users can change details of every figure*)
filename=obsname[[6]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[1,1]] ];
filename=obsname[[6]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[1,2]]];
filename=obsname[[6]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[2,1]] ];
filename=obsname[[6]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p6[[2,2]] ];

];

(*dr*corr plots*)
If[
FigureFlag[[5]]==1,
Print["making plot of figure type ",FigureType[[5]],", flavour = ",flavour];
p5=processdataplotsmultiexp6percentage[{dRcorrdataclassfinal},readcorrconfigfile4[configDir,configfilename],5,flavour];

(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[5]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[1,1]],ImageResolution->imgresol  ];
filename=obsname[[5]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[1,2]],ImageResolution->imgresol  ];
filename=obsname[[5]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[2,1]],ImageResolution->imgresol  ];
filename=obsname[[5]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[2,2]],ImageResolution->imgresol  ];

(*20171103: add files storing mathematica expressions so that users can change details of every figure*)
filename=obsname[[5]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[1,1]]  ];
filename=obsname[[5]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[1,2]] ];
filename=obsname[[5]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[2,1]]  ];
filename=obsname[[5]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p5[[2,2]]  ];

];


"dummy"
];
"dummy"
,{flavour,-5,-5+fmax-1}
];

(*2, 3, 4*)
(*dr*corr plots*)
If[
FigureFlag[[2]]==1,
Print["making plot of figure type ",FigureType[[2]],", flavour = ",flavour];
p234=processdataplotsmultiexp6percentage[{expterrordataclassfinal},readcorrconfigfile4[configDir,configfilename],2,flavour];
(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[2]]<>"_"<>representationname[[1]]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]],ImageResolution->imgresol  ];
filename=obsname[[2]]<>"_"<>representationname[[2]]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,2]],ImageResolution->imgresol  ];
filename=obsname[[2]]<>"_"<>representationname[[3]]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,1]],ImageResolution->imgresol  ];
filename=obsname[[2]]<>"_"<>representationname[[4]]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,2]],ImageResolution->imgresol  ];

(*20171103: add files storing mathematica expressions so that users can change details of every figure*)
filename=obsname[[2]]<>"_"<>representationname[[1]]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]]  ];
filename=obsname[[2]]<>"_"<>representationname[[2]]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,2]]  ];
filename=obsname[[2]]<>"_"<>representationname[[3]]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,1]]  ];
filename=obsname[[2]]<>"_"<>representationname[[4]]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,2]]  ];

];

If[
FigureFlag[[3]]==1,
Print["making plot of figure type ",FigureType[[3]],", flavour = ",flavour];
p234=processdataplotsmultiexp6percentage[{residualdataclassfinal},readcorrconfigfile4[configDir,configfilename],3,flavour];
(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[3]]<>"_"<>representationname[[1]]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]],ImageResolution->imgresol  ];
filename=obsname[[3]]<>"_"<>representationname[[2]]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,2]],ImageResolution->imgresol  ];
filename=obsname[[3]]<>"_"<>representationname[[3]]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,1]],ImageResolution->imgresol  ];
filename=obsname[[3]]<>"_"<>representationname[[4]]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,2]],ImageResolution->imgresol  ];

(*20171103: add files storing mathematica expressions so that users can change details of every figure*)
filename=obsname[[3]]<>"_"<>representationname[[1]]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]]  ];
filename=obsname[[3]]<>"_"<>representationname[[2]]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,2]]  ];
filename=obsname[[3]]<>"_"<>representationname[[3]]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,1]]  ];
filename=obsname[[3]]<>"_"<>representationname[[4]]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,2]]  ];


];

If[
FigureFlag[[4]]==1,
Print["making plot of figure type ",FigureType[[4]],", flavour = ",flavour];
p234=processdataplotsmultiexp6percentage[{dRdataclassfinal},readcorrconfigfile4[configDir,configfilename],4,flavour];
(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[4]]<>"_"<>representationname[[1]]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]],ImageResolution->imgresol  ];
filename=obsname[[4]]<>"_"<>representationname[[2]]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,2]],ImageResolution->imgresol  ];
filename=obsname[[4]]<>"_"<>representationname[[3]]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,1]],ImageResolution->imgresol  ];
filename=obsname[[4]]<>"_"<>representationname[[4]]<>"_samept"<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,2]],ImageResolution->imgresol  ];

(*20171103: add files storing mathematica expressions so that users can change details of every figure*)
filename=obsname[[4]]<>"_"<>representationname[[1]]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,1]]  ];
filename=obsname[[4]]<>"_"<>representationname[[2]]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[1,2]]  ];
filename=obsname[[4]]<>"_"<>representationname[[3]]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,1]]  ];
filename=obsname[[4]]<>"_"<>representationname[[4]]<>"_samept"<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p234[[2,2]]  ];


];

If[
FigureFlag[[1]]==1,
Print["making plot of figure type ",FigureType[[1]] ];
p1=processdataplotsmultiexp6percentage[{corrdataclassfinal},readcorrconfigfile4[configDir,configfilename],1,0 ];
filename=obsname[[1]]<>"_"<>representationname[[1]]<>extensionname[[iext]];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p1[[1]],ImageResolution->imgresol ];
filename=obsname[[1]]<>"_"<>representationname[[1]]<>".dat";
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,p1[[1]] ];
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
Print["time to make plots is ",jpgtime," seconds"];


(*copy configure file to job dir*)
(*20170508 if config file in plot path exist, remove it*)
If[FileExistsQ[saveparentpath<>jobpath<>configfilename]==True,DeleteFile[saveparentpath<>jobpath<>configfilename] ];
CopyFile[configDir<>configfilename,saveparentpath<>jobpath<>configfilename];

(*make exptname table, 20170410: Sean asks to move this process to the final step*)
rows=3;
filename="exptname_table"<>extensionname[[iext]];
exptnames=Table[ExptIDtoName[exptlistfinal[[iexpt]] ]<>"("<>ToString[exptlistfinal[[iexpt]] ]<>")",{iexpt,1,Length[exptlistfinal]}];
Print["making table of experiments included in plots"];
exptnamestable=makeGrid2[exptnames,rows,""];
Export[saveparentpath<>(*pdfnameexpttypeDir<>exptidDir*)jobpath<>filename,exptnamestable];

(*merge .eps files into a pdf file*)
implementeps[saveparentpath<>jobpath,PDFxQSelectMethod];

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
{dummy,dummy,dummy,dummy,ExptidType,ExptidFlag,dummy,dummy,dummy,dummy,
dummy,dummy,dummy,dummy,(*Hist1figureYrange*)dummy,
dummy,
dummy,dummy,dummy,dummy,dummy}=
readcorrconfigfile4[configDir,configfilename];
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

Print["total time to run is ",timefunc," seconds"];
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
