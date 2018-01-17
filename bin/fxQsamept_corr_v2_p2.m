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
configfilename="savedata_config.txt";


(* ::Input::Initialization:: *)
(*read arguments from config file*)
{PDFsetDir,PDFsetmethod,ExptIDList,datalistFile,(*FxQGridDir*)dummy2,(*FxQGridFile*)dummy3,FxQSameptDir,FxQSameptFile,CorrDataDir,CorrDataFile,(*GridNx*)dummy6,(*GridNQ*)dummy7}=
readsavedataconfigfile[configDir,configfilename]



(* ::Input::Initialization:: *)
(*20170620*)
Print["configure file directory: ",configDir];
Print["configure filename : ",configfilename];
Print["arguments read:\n","{PDFsetDir,PDFsetmethod,ExptIDList,datalistFile,(*FxQGridDir*)dummy2,(*FxQGridFile*)dummy3,FxQSameptDir,FxQSameptFile,CorrDataDir,CorrDataFile,(*GridNx*)dummy6,(*GridNQ*)dummy7}=\n",readsavedataconfigfile[configDir,configfilename] ];


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
If[FxQGridFile\[Equal]"default",FxQGridFile="fxQ_grid_"<>PDFname<>"_x"<>ToString[GridNx]<>"_Q"<>ToString[GridNQ]<>".dat"]
*)

(*
If[FxQSameptDir\[Equal]"default",FxQSameptDir=quickdataDir]
If[FxQSameptFile\[Equal]"default",FxQSameptFile="fxQ_samept_"<>PDFname<>".dat"]
*)

If[CorrDataDir=="default",CorrDataDir=quickdataDir]
If[CorrDataFile=="default",CorrDataFile="samept_data_"<>PDFname<>".dat"]



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
Print["CorrDataDir: ",CorrDataDir];
Print["CorrDataFile: ",CorrDataFile];



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

exptdata=Readdtafile[["readdta"]][DtaDir,exptlist]


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


(* ::Subsection:: *)
(*add {x,Q} to data label*)


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
(*extract {x,Q} value and get f(x,Q,flavour) for all flavours (-5~5 && 6, 7, 8)*)
(*{6,7,8} = {dbar/ubar,  d/u,  s+sbar/ubar+dbar}*)


(* ::Input:: *)
(*mydtadata[[1,1]][["label"]]*)
(*mydtadata[[1,1]][["data"]]*)
(*mydtadata[[2,1]][["label"]]*)


(* ::Input::Initialization:: *)
ti=AbsoluteTime[];


(* ::Input::Initialization:: *)
(*inteprate f(x,Q) by {x,Q} of exptdata*)
Print["read .dta files to intepolate PDF values corresponding to data points"];
Print[""];

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
(*fxQsamept2class[[1,6]][["data"]][[1]]//Length*)


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


(* ::Input::Initialization:: *)
(*test the time difference between Pdsread[["fxQsamept"]] and pdfCTEQ,
where Pdsread[["fxQsamept"]] is the function using pdfCTEQ to calculate PDF values of data by samept method*)
GetPDFTimeTest=AbsoluteTiming[
Table[
x=0.0001;Q=3.0;pdfCTEQ[x,Q,5,iset],
{i,1,100},{iset,1,(Datamethods[["getNcolumn"]][fxQsamept2class[[1,0+6]] ]-2)}];
][[1]];
Print["average time of one test of pdfCTEQ is ",GetPDFTimeTest/(100*(Datamethods[["getNcolumn"]][fxQsamept2class[[1,0+6]] ]-2))];


(* ::Input:: *)
(*Dimensions[fxQsamept2class]*)
(*(*print gluon f(x,Q) for 57 sets*)*)
(*fxQsamept2class[[1,6]][["label"]]*)
(*fxQsamept2class[[1,6]][["data"]][[1]]*)


(* ::Input::Initialization:: *)
(*add customized flavour for parton density function (flavour = 6,7,8)*)
(*definition of 6, 7, 8 are in setextrafxQ: ubar/dbar, u/d,  s+sbar/ubar+dbar*)

(*20171126 delete q6~q8*)
(*
fxQsamept2class=Table[Join[fxQsamept2class[[iexpt]],setextrafxQ[fxQsamept2class[[iexpt]] ]  ],{iexpt,1,Length[fxQsamept2class]}];
*)


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
(*make a class of {LF[x,Q],LF[x,Q],...}, extract (x, Q) *)
Dtadatatmp=Datamethods[["take"]][mydtadata[[iexpt,1]],{-3,-2}];
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


(* ::Input:: *)
(**)


(* ::Subsection:: *)
(*build deltaR class*)


(* ::Input:: *)
(*Datamethods[["take"]][residualNsetclass[[1]],{3,-1}][["data"]];*)


(* ::Input:: *)
(*(*calculate dR*)*)
(*(**)
(*dR=Table[*)
(*(*take Nset residual data with no x, Q*)*)
(*residualNset=Datamethods[["take"]][residualNsetclass[[iexpt]],{3,-1}][["data"]];*)
(**)
(*pdfHessianSymErrorfake[residualNset/.LF\[Rule]List],*)
(*{iexpt,1,Length[residualNset]},{ix,1,Length[residualNset[[iexpt]] ]}*)
(*];*)
(**)*)


(* ::Input::Initialization:: *)
deltaRclass=
Table[
getdeltaRclass[residualNsetclass[[iexpt]] ],
{iexpt,1,Dimensions[mydtadata][[1]]}
];


(* ::Input:: *)
(**)
(*deltaRclass[[1]][["label"]]*)
(*Dimensions[deltaRclass]*)
(*Datamethods[["getNpt"]][deltaRclass[[1]] ]*)
(*Datamethods[["getNcolumn"]][deltaRclass[[1]] ]*)


(* ::Subsection:: *)
(*build residual(central value of residual) class*)


(* ::Input::Initialization:: *)
(*select x, Q, central value of residual (iset==1)*)
residualclass=
Table[
Datamethods[["take"]][residualNsetclass[[iexpt]],{1,3}],
{iexpt,1,Dimensions[mydtadata][[1]]}
];


(* ::Input:: *)
(*residualclass[[1]][["label"]]*)
(*Dimensions[residualclass]*)
(*Datamethods[["getNpt"]][residualclass[[1]] ]*)
(*Datamethods[["getNcolumn"]][residualclass[[1]] ]*)


(* ::Subsection:: *)
(*build expterror ratio class*)


(* ::Input:: *)
(*mydtadata[[1,1]][["label"]]*)


(* ::Input::Initialization:: *)
(*check whether the elements used to calculate Expt error/expt are correct*)
ExpIndex=4;ExpErrIndex=6;
Print["begin to calculate \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)/\!\(\*SubscriptBox[\(D\), \(i\)]\) from data, labels of indexes used for each experiment:"];

Table[
Print[
mydtadata[[iexpt,1]][["exptinfo","exptid"]],": ",mydtadata[[iexpt,1]][["label"]][[ExpErrIndex]],"/", mydtadata[[iexpt,1]][["label"]][[ExpIndex]]
],
{iexpt,1,Dimensions[mydtadata][[1]]} 
];



(* ::Input::Initialization:: *)
(*extract (x, Q, ExpErrIndex/ExpIndex)*)
expterrorclass=
Table[
tmpdataclass=mydtadata[[iexpt,1]];
tmpdataclass[["data"]]=tmpdataclass[["data"]]/.LF[a__]:>LF[{a}[[-3]],{a}[[-2]],{a}[[ExpErrIndex]]/{a}[[ExpIndex]] ];
tmpdataclass[["label"]]={"x","Q","expterror/exptcentral"};
tmpdataclass,
{iexpt,1,Dimensions[mydtadata][[1]]}
];


(* ::Input:: *)
(*expterrorclass[[1]][["label"]]*)
(*expterrorclass[[1]][["data"]];*)
(*Dimensions[expterrorclass]*)
(*Datamethods[["getNpt"]][expterrorclass[[1]] ]*)
(*Datamethods[["getNcolumn"]][expterrorclass[[1]] ]*)


(* ::Subsection:: *)
(*build corr[residual, f(x,Q)] class*)


(* ::Subsection:: *)
(*correlation, dr*correlation, deltaR calculation *)


(* ::Input::Initialization:: *)
Print[""];

(*copy residual class to a variable, for keeping code simple*)
xQresidualclass=residualNsetclass;
(*
fxQsamept2class=fxQgrid2class;
*)
Dimensions[xQresidualclass]
Dimensions[fxQsamept2class]


(* ::Input:: *)
(*(*check Npt and data elements of f(x,Q) & residual are the same*)*)
(*Datamethods[["getNpt"]][fxQsamept2class[[1,1]] ]*)
(*Datamethods[["getNcolumn"]][fxQsamept2class[[1,1]] ]*)
(**)
(*Datamethods[["getNpt"]][xQresidualclass[[1]] ]*)
(*Datamethods[["getNcolumn"]][xQresidualclass[[1]] ]*)


(* ::Input::Initialization:: *)
fmax=Length[fxQsamept2class[[1]] ]
Length[fxQsamept2class[[1]] ]


(* ::Input::Initialization:: *)

(*calculate correlation*)
ti=AbsoluteTime[];
corrfxQdtaobsclass=
Table[
corrfxQdtaobs[xQresidualclass[[i]],fxQsamept2class[[i,flavour+6]] ],
{i,1,Length[xQresidualclass]},{flavour,-5,-5+fmax-1}
];
(*test the time of the correlation value calculation*)
tf=AbsoluteTime[];
GetCorrvalueTime=tf-ti;
NCorrval=
Sum[
Datamethods[["getNpt"]][corrfxQdtaobsclass[[iexpt,flavour+6]] ],
{iexpt,1,Dimensions[corrfxQdtaobsclass][[1]]},{flavour,-5,-5+Dimensions[corrfxQdtaobsclass][[2]]-1}
];
Print["total number of Corr value calculations is ",NCorrval];
Print["average time of one Corr value calculations is ",GetCorrvalueTime/NCorrval];
Print["time of calculating Corr values is ",GetCorrvalueTime];
Print[];

(*calculate dr*correlation*)
dRcorrfxQdtaobsclass=
Table[
dRcorrfxQdtaobs[xQresidualclass[[i]],fxQsamept2class[[i,flavour+6]] ],
{i,1,Length[xQresidualclass]},{flavour,-5,-5+fmax-1}
];

(*calculate dr*)
deltaRclass=
Table[
getdeltaRclass[xQresidualclass[[i]] ],
{i,1,Length[xQresidualclass]}
];




(* ::Input:: *)
(*Dimensions[corrfxQdtaobsclass]*)
(*Dimensions[dRcorrfxQdtaobsclass]*)
(*Dimensions[deltaRclass]*)


(* ::Input:: *)
(*(*check  dR*correlation*)*)
(*(*check one point*)*)
(*corrfxQdtaobsclass[[1,6]][["data"]][[1]]*)
(*dRcorrfxQdtaobsclass[[1,6]][["data"]][[1]]*)
(*deltaRclass[[1]][["data"]][[1]]*)
(*(*the result of dR*corr/(dR*corr) = 1 *)*)
(*corrfxQdtaobsclass[[1,6]][["data"]][[1,3]]*deltaRclass[[1]][["data"]][[1,3]]/dRcorrfxQdtaobsclass[[1,6]][["data"]][[1,3]]*)
(**)


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
(*export into files*)
(*save f(x, Q) grid into .dat file*)
{corrfxQdtaobsclass,dRcorrfxQdtaobsclass,expterrorclass,residualclass,deltaRclass};
(*
{expterrordatacorrmaxclass,residualdatacorrmaxclass,dRdatacorrmaxclass};
dRcorrdataclass;
{expterrordatadRcorrmaxclass,residualdatadRcorrmaxclass,dRdatadRcorrmaxclass};
*)
(*(x,Q) points sets as position at max of corr*)

(*20171101: only save only the all error sets of residual and f(x,\[Mu]) values 
Export[CorrDataDir<>"corr_"<>CorrDataFile,corrfxQdtaobsclass,"ExpressionML"];
Export[CorrDataDir<>"dRcorr_"<>CorrDataFile,dRcorrfxQdtaobsclass,"ExpressionML"];

Export[CorrDataDir<>"expterror_"<>CorrDataFile,expterrorclass,"ExpressionML"];
Export[CorrDataDir<>"residual_"<>CorrDataFile,residualclass,"ExpressionML"];
Export[CorrDataDir<>"dR_"<>CorrDataFile,deltaRclass,"ExpressionML"];
corrfxQdtaobsclass>>(CorrDataDir<>"corr_"<>CorrDataFile);
dRcorrfxQdtaobsclass>>(CorrDataDir<>"dRcorr_"<>CorrDataFile);
expterrorclass>>(CorrDataDir<>"expterror_"<>CorrDataFile);
residualclass>>(CorrDataDir<>"residual_"<>CorrDataFile);
deltaRclass>>(CorrDataDir<>"dR_"<>CorrDataFile);
*)

(*
Export[CorrDataDir<>"corr_samept_data_"<>PDFname<>".m",corrfxQdtaobsclass,"ExpressionML"];
Export[CorrDataDir<>"dRcorr_samept_data_"<>PDFname<>".m",dRcorrfxQdtaobsclass,"ExpressionML"];
Export[CorrDataDir<>"expterror_samept_data_"<>PDFname<>".m",expterrorclass,"ExpressionML"];
Export[CorrDataDir<>"residual_samept_data_"<>PDFname<>".m",residualclass,"ExpressionML"];
Export[CorrDataDir<>"dR_samept_data_"<>PDFname<>".m",deltaRclass,"ExpressionML"];
*)



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
Print["ratio of numbers after/before the transform: only shows values with Absolute ratio >0.001"];
Print[""];

cpRratio=
Table[
{residualNsetclass[[iexpt]][["exptinfo","exptid"]],Select[(((residualNsetclass[[iexpt]][["data"]]/.LF->List)/(cpresidual[[iexpt]][["data"]]/.LF->List))-1)//Flatten,(#>0.001 || #<-0.001)&]},
{iexpt,Length[residualNsetclass]}
];


Print[cpRratio];




(* ::Input:: *)
(*SetPrecision[fxQsamept2class[[1,1]],5]//OutputForm*)


(* ::Input::Initialization:: *)
Print["begin to save data..."];
Print[""];
(*save Nset of residual*)(*>> doesn't work in script version, use Put[expression, filename]*)
(*
Export[CorrDataDir<>"residualNset_"<>CorrDataFile,residualNsetclass,"ExpressionML"];
*)
Put[residualNsetclass,(CorrDataDir<>"residualNset_"<>CorrDataFile)];
(*save Nset of fxQ*)
Put[fxQsamept2class,(CorrDataDir<>"fxQNset_"<>CorrDataFile)];
(*save original data of.dta files (central value file)*)
(*perhaps we only want to save the central set information so that we don't waste storage and time?*)
(*here only extract the central set for each Expt ID*)
dtacentralclass=mydtadata[[#,1]]&/@Range[Length[mydtadata] ];
Put[dtacentralclass,(CorrDataDir<>"dtacentral_"<>CorrDataFile)];


(* ::Input::Initialization:: *)
tf=AbsoluteTime[];
WriteDatavalueTime=tf-ti;
NDataval=
Sum[
(3*6)*Datamethods[["getNpt"]][corrfxQdtaobsclass[[iexpt,flavour+6]] ],
{iexpt,1,Dimensions[corrfxQdtaobsclass][[1]]},{flavour,-5,-5+Dimensions[corrfxQdtaobsclass][[2]]-1}
];
Print["total number of data value writing is ",NDataval];
Print["average time of one data value writing is ",WriteDatavalueTime/NDataval];
Print["time of writing data values is ",WriteDatavalueTime];


(* ::Input::Initialization:: *)
(*20170620*)
(*
Print["data dimension {Nexpt,Nflavour} = ",Dimensions[corrfxQdtaobsclass] ];
Print["data filename = ",CorrDataDir<>"corr_"<>CorrDataFile];
Print["data filename = ",CorrDataDir<>"dRcorr_"<>CorrDataFile];
Print["data filename = ",CorrDataDir<>"expterror_"<>CorrDataFile];
Print["data filename = ",CorrDataDir<>"residual_"<>CorrDataFile];
Print["data filename = ",CorrDataDir<>"dR_"<>CorrDataFile];
Print["data filename = ",CorrDataDir<>"residualNset_"<>CorrDataFile];
Print["data label format example: ",corrfxQdtaobsclass[[1,1]][["label"]] ];
Print["data format example: ",corrfxQdtaobsclass[[1,1]][["data"]][[1]] ];
*)
Print["data dimension {Nexpt,Nflavour} = ",Dimensions[fxQsamept2class] ];
Print["data filename = ",CorrDataDir<>"residualNset_"<>CorrDataFile];
Print["data filename = ",CorrDataDir<>"fxQNset_"<>CorrDataFile];
Print["data filename = ",CorrDataDir<>"dtacentral_"<>CorrDataFile];


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
