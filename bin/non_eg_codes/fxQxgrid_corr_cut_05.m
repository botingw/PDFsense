(* ::Package:: *)

(* ::Text:: *)
(*Function description:*)
(*1. This executable read  "savedata_config", and then read residue in .dta files and a f(x,Q) data file to calculate observables, saving data of observables into /PDFDataDir/PDFDataFile*)
(*2. (x, Q) are points of  xgrid method (please read the tutorial note of this program)*)
(*3. \[Dash] arguments that users should setup*)
(**)
(*\:2217 PDF set Dir, PDF method, Expt ID List, datalis file*)
(**)
(*\:2217 Correlation Path & Correlation File*)
(**)
(**)
(**)
(*\:2217 F(x,Q) Grid Path & F(x,Q) Grid File, Nx for grid f(x,Q) data*)
(**)
(*\[Dash] output: if Path & File are "default", the program make files with all $obs indexes:*)
(**)
(*\[Dash]./quick_data/{$obs}_samept_data_{$PDFname}_{$Nx}.m, where $PDFname = PDFname, ex: CT14NNLO, $obs = "corr", "dRcorr", "dR", "residue", "residueNset", "expterror"*)
(**)
(*data in the file is a List of dimension: *)
(*"corr": [[iexpt, iflavour]]*)
(*"dRcorr": [[iexpt, iflavour]]*)
(*" dR": [[iexpt, iflavour]]*)
(*"residue": [[iexpt, iflavour]]*)
(*"residueNset": [[iexpt, iflavour]]*)
(*"expterror": [[iexpt, iflavour]]*)


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
{PDFsetDir,PDFsetmethod,ExptIDList,datalistFile,FxQGridDir,FxQGridFile,(*FxQSameptDir*)dummy2,(*FxQSameptFile*)dummy3,CorrDataDir,CorrDataFile,GridNx,(*GridNQ*)dummy4}=
readsavedataconfigfile[configDir,configfilename]



(* ::Input::Initialization:: *)
(*20170620*)
Print["configure file directory: ",configDir];
Print["configure filename : ",configfilename];
Print["arguments read:\n","{PDFsetDir,PDFsetmethod,ExptIDList,datalistFile,FxQGridDir,FxQGridFile,(*FxQSameptDir*)dummy2,(*FxQSameptFile*)dummy3,CorrDataDir,CorrDataFile,GridNx,(*GridNQ*)dummy4}=\n",readsavedataconfigfile[configDir,configfilename] ];


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
*)
If[CorrDataDir=="default",CorrDataDir=quickdataDir]
If[CorrDataFile=="default",CorrDataFile="xgrid_data_"<>PDFname<>"_x"<>ToString[GridNx]<>".m"]



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

Print["CorrDataDir: ",FxQGridDir];
Print["CorrDataFile: ",FxQGridFile];



(* ::Input:: *)
(*Directory[]*)


(* ::Input::Initialization:: *)
ReadLisFile[datalistFile]


(* ::Input::Initialization:: *)

exptlist=ExptIDList;
Print["read data expt id: ",exptlist];



(* ::Subsection:: *)
(*read f(x, Q) grid from FxQGridDir file*)


(* ::Input:: *)
(*Directory[]*)
(*FxQGridDir*)


(* ::Input:: *)
(*(*set the save path*)*)
(*(*quickdataDir="../quick_data/";*)*)
(*(**)
(*(*NQ=GridNQ;*)Nx=GridNx;*)
(*fxQfile="fxQ_xgrid_"<>PDFname<>"_x"<>ToString[Nx]<>"_WZmass"<>".m";*)
(**)*)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
fxQgrid2class=Import[FxQGridDir<>FxQGridFile,"ExpressionML"];


(* ::Input:: *)
(**)
(*Dimensions[fxQgrid2class]*)
(*fxQgrid2class[[1]][["data"]][[1]]*)


(* ::Input:: *)
(*Dimensions[fxQgrid2class][[1]]*)
(*fxQgrid2class[[1]][["data"]][[1]]*)


(* ::Input::Initialization:: *)
(*add customized flavour for parton density function (flavour = 6,7,8)*)

fxQgrid2class=Join[fxQgrid2class,setextrafxQ[fxQgrid2class]  ]




(* ::Input:: *)
(*Dimensions[fxQgrid2class][[1]]*)
(*fxQgrid2class[[1]][["data"]][[1]]*)


(* ::Subsection:: *)
(*read .dta files*)


(* ::Input::Initialization:: *)
(*read expt data from .dta files*)
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
fxQmode="grid";
If[
fxQmode=="samept",
Table[
(*add data by formula*)
mydtadata[[iexpt,iset]][["data"]]=selectExptxQv2[mydtadata[[iexpt,iset]][["exptinfo","exptid"]],mydtadata[[iexpt,iset]][["data"]],"dummy"];
(*add label of {x,Q} to -2&-1 -th column*)
mydtadata[[iexpt,iset]][["label"]]=Join[mydtadata[[iexpt,iset]][["label"]],{"x","Q"}];
(*LF[...] become global*)
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
(*set a class with data struture LF[x,Q,residue]*)


(* ::Input:: *)
(*mydtadata[[1,1]][["label"]]*)
(*mydtadata[[2,1]][["label"]]*)
(*mydtadata[[3,1]][["label"]]*)


(* ::Input::Initialization:: *)
Table[
mydtadata[[iexpt,iset]][["data"]]=mydtadata[[iexpt,iset]][["data"]]/.LF[a__]:>LF[Sequence@@{a},({a}[[5]]-{a}[[11]])/{a}[[12]] ];
mydtadata[[iexpt,iset]][["label"]]=Join[mydtadata[[iexpt,iset]][["label"]],{"residue"}];
"dummy",
{iexpt,1,Dimensions[mydtadata][[1]]},{iset,1,Dimensions[mydtadata][[2]]}
];


(* ::Input:: *)
(*mydtadata[[2,1]][["label"]]*)
(*mydtadata[[2,1]][["data"]];*)
(*mydtadata[[2,1]][["rawdata"]];*)


(* ::Input:: *)
(*(*extract Nset of data [[iexpt,iset,{residue},Npt]]*)*)
(*(**)
(*dataNset=*)
(*Table[*)
(*(*grab residue*)*)
(*Datamethods[["picktolist"]][mydtadata[[iexpt,iset]],{-1} ],*)
(*{iexpt,1,Dimensions[mydtadata][[1]]},{iset,1,Dimensions[mydtadata][[2]]}*)
(*];*)
(**)*)


(* ::Input::Initialization:: *)
(*input the dataclass[[Nset]] and the columnN, extract {LF[columnN(1),...,columnN(Nset)]} *)
getNsetLF[dataclassin_,columnin_]:=
Module[{dataclass=dataclassin,column=columnin,dataNset,Nset},
Nset=Length[dataclass];
(*extract Nset of data [[iset,{residue},Npt]]*)
dataNset=
Table[
(*grab residue*)
Datamethods[["picktolist"]][dataclass[[iset]],{column} ],
{iset,1,Nset}
];

(*transf to [[Npt,{residue},Nset]] format*)
dataNset=Transpose[dataNset,{3,2,1}];


(*transf to [[Npt,Nset]] format*)
(*CT14NNLO: [[Npt]]=LF[obs0,...obs56]*)
dataNset=
Table[
(*1 represent residue index*)
dataNset[[ix,1]]/.List->LF,
{ix,1,Length[dataNset]}
];

dataNset
]


(* ::Input:: *)
(**)


(* ::Subsection:: *)
(*for every grids, calculate LF[x,Q, Corr(residue, f(x, Q) )], then pick up only correlation close to maximum*)


(* ::Input:: *)
(*Dimensions[fxQgrid2class]*)


(* ::Input::Initialization:: *)
(*residueNset[[iexpt,ix]] = LF[obs1,obs2,...,obs Nset]*)
residueNset=Table[getNsetLF[mydtadata[[iexpt]],-1],{iexpt,1,Length[mydtadata]}];



(* ::Input::Initialization:: *)
(*calculate dR*)
dR=Table[
pdfHessianSymErrorfake[residueNset[[iexpt,ix]]/.LF->List],
{iexpt,1,Length[residueNset]},{ix,1,Length[residueNset[[iexpt]] ]}
];


(* ::Input:: *)
(*dR;*)


(* ::Input::Initialization:: *)
fmax=Length[fxQgrid2class]


(* ::Input::Initialization:: *)



(*calculate correlation of fxQgrid2class[[flavour]], residue[[iexpt,Npt]] for all grid points, then select the maximum corr data*)
(*output format [[iexpt,flavour,ix of data set]]*)
(*data of corr*)
obsgridLFdata=
Table[
(*calculate correlation of all (x, Q) grids*)
(*time1=AbsoluteTiming[*)
Qexpt=mydtadata[[iexpt,1]][["data"]][[ix,2]];

corrgridLF={};
Table[
x=fxQgrid2class[[flavour+6]][["data"]][[igrid]][[1]];Q=fxQgrid2class[[flavour+6]][["data"]][[igrid]][[2]];
If[
(*only take the grid point with the same Q(ratio ~ 1%) as data point*)
Abs[(Qexpt-Q)/(Qexpt+Q)]<0.005,
corrxQ=pdfHessianCorrelationfake[fxQgrid2class[[flavour+6]][["data"]][[igrid]]/.LF[a__]:>Drop[{a},2],residueNset[[iexpt,ix]]/.LF->List];
dRcorrxQ=dR[[iexpt,ix]]*corrxQ;
dRxQ=dR[[iexpt,ix]];
 expterrorxQ=mydtadata[[iexpt,1]][["data"]][[ix]]/.LF[a__]:>{a}[[6]]/{a}[[4]];
residuexQ=residueNset[[iexpt,ix]][[1]];
(*if Qexpt=Qgrid, add this data*)
corrgridLF=Append[corrgridLF,LF[x,Q,expterrorxQ,residuexQ,dRxQ,dRcorrxQ,corrxQ] ]
];
"dummy",
{igrid,1,Datamethods[["getNpt"]][fxQgrid2class[[flavour+6]] ]}
];

(*];*)
(*
time2=AbsoluteTiming[
(*max, min or corr*)
maxcorr=Max[corrgridLF/.LF[a__]\[RuleDelayed]{a}[[3]] ];
mincorr=Min[corrgridLF/.LF[a__]\[RuleDelayed]{a}[[3]] ];
(*pick points with correlation close to the maximum value*)
output=
If[
maxcorr>Abs[mincorr],
Select[corrgridLF,#[[3]]\[GreaterEqual] 1.0*maxcorr&],
Select[corrgridLF,#[[3]]\[LessEqual] 1.0*mincorr&]
];
];
*)
(*Print["flavour, iexpt, ix, t1, t2: ",{flavour,iexpt,ix,time1}];*)
corrgridLF
(*search only maximum corr of (x,Q,corr)*),
{iexpt,1,Length[residueNset]},{flavour,-5,fmax-6},{ix,1,Length[residueNset[[iexpt]] ]}
];

(*output format [[iexpt,flavour,ix of data set]]*)
(*data of dR*corr*)
(*
dRcorrgrid=
Table[
(*calculate correlation of all (x, Q) grids*)
time1=AbsoluteTiming[

dRcorrgridLF=
Table[
x=fxQgrid2class[[flavour+6]][["data"]][[igrid]][[1]];Q=fxQgrid2class[[flavour+6]][["data"]][[igrid]][[2]];
LF[x,Q,dR[[iexpt,ix]]*pdfHessianCorrelationfake[fxQgrid2class[[flavour+6]][["data"]][[igrid]]/.LF[a__]\[RuleDelayed]Drop[{a},2],residueNset[[iexpt,ix]]/.LF\[Rule]List] ],
{igrid,1,Datamethods[["getNpt"]][fxQgrid2class[[flavour+6]] ]}
];
];

time2=AbsoluteTiming[
maxdRcorr=Max[dRcorrgridLF/.LF[a__]\[RuleDelayed]{a}[[3]] ];
mindRcorr=Min[dRcorrgridLF/.LF[a__]\[RuleDelayed]{a}[[3]] ];
(*pick points with correlation close to the maximum value*)
output=
If[
maxdRcorr>Abs[mindRcorr],
Select[dRcorrgridLF,#[[3]]\[GreaterEqual]1.0*maxcorr&],
Select[dRcorrgridLF,#[[3]]\[LessEqual] 1.0*mincorr&]
];
];
output
(*search only maximum corr of (x,Q,corr)*),
{iexpt,1,Length[residueNset]},{flavour,-5,5},{ix,1,Length[residueNset[[iexpt]] ]}
];
*)




(* ::Input:: *)
(**)
(*obsgridLFdata[[2,1,1]];*)


(* ::Input:: *)
(*Take[obsgridLFdata[[1,6,1]],10]*)
(**)


(* ::Input:: *)
(*Table[*)
(*(*max, min or corr*)*)
(*maxcorr=Max[obsgridLFdata[[iexpt,flavour+6,ix]]/.LF[a__]:>{a}[[6]] ];*)
(*mincorr=Min[obsgridLFdata[[iexpt,flavour+6,ix]]/.LF[a__]:>{a}[[6]] ];*)
(*"dummy"*)
(*(*search only maximum corr of (x,Q,corr)*),*)
(*{iexpt,1,Length[residueNset]},{flavour,-5,fmax-6},{ix,1,Length[residueNset[[iexpt]] ]}*)
(*];*)
(**)


(* ::Input::Initialization:: *)
maxrate=0.9;


(* ::Input::Initialization:: *)
corrmaxgridLF=
Table[

time2=AbsoluteTiming[
(*max, min or corr*)
maxcorr=Max[obsgridLFdata[[iexpt,flavour+6,ix]]/.LF[a__]:>{a}[[7]] ];
mincorr=Min[obsgridLFdata[[iexpt,flavour+6,ix]]/.LF[a__]:>{a}[[7]] ];
(*pick points with correlation close to the maximum value*)
(*for cut: |corr|>0.5 version*)
output=Select[obsgridLFdata[[iexpt,flavour+6,ix]],Abs[#[[7]] ]>=0.5&]
(*
If[
maxcorr>Abs[mincorr],
Select[obsgridLFdata[[iexpt,flavour+6,ix]],Abs[#[[7]] ]\[GreaterEqual]maxrate*Abs[maxcorr]&],
Select[obsgridLFdata[[iexpt,flavour+6,ix]],Abs[#[[7]] ]\[GreaterEqual] maxrate*Abs[mincorr]&]
];
*)
];
(*
Print["flavour, iexpt, ix, t1, t2: ",{flavour,iexpt,ix,time2}];
*)
output
(*search only maximum corr of (x,Q,corr)*),
{iexpt,1,Length[residueNset]},{flavour,-5,fmax-6},{ix,1,Length[residueNset[[iexpt]] ]}
];

(*
dRcorrmaxgridLF=
Table[

time2=AbsoluteTiming[
(*max, min or corr*)
maxcorr=Max[obsgridLFdata[[iexpt,flavour+6,ix]]/.LF[a__]\[RuleDelayed]{a}[[6]] ];
mincorr=Min[obsgridLFdata[[iexpt,flavour+6,ix]]/.LF[a__]\[RuleDelayed]{a}[[6]] ];
(*pick points with correlation close to the maximum value*)
output=
If[
maxcorr>Abs[mincorr],
Select[obsgridLFdata[[iexpt,flavour+6,ix]],#[[6]]\[GreaterEqual] 1.0*maxcorr&],
Select[obsgridLFdata[[iexpt,flavour+6,ix]],#[[6]]\[LessEqual] 1.0*mincorr&]
];
];
(*
Print["flavour, iexpt, ix, t1, t2: ",{flavour,iexpt,ix,time2}];
*)
output
(*search only maximum corr of (x,Q,corr)*),
{iexpt,1,Length[residueNset]},{flavour,-5,fmax-6},{ix,1,Length[residueNset[[iexpt]] ]}
];
*)



(* ::Input:: *)
(*corrmaxgridLF[[1,11,1]]*)


(* ::Input::Initialization:: *)
(*correlation class and dR*correlation class*)
corrdataclass=
Table[
tmpdataclass=Corrsameptdata;
(*set info of the correlation data class*)
tmpdataclass[["PDFinfo"]]=fxQgrid2class[[flavour+6]][["PDFinfo"]];
tmpdataclass[["exptinfo"]]=mydtadata[[iexpt,1]][["exptinfo"]];
(*correlation data of the same expt, flavour are in the same List: {LF[x,Q,corr],...} *)
tmpdataclass[["data"]]=Flatten[corrmaxgridLF[[iexpt,flavour+6]]/.LF[a__]:>LF[{a}[[1]],{a}[[2]],{a}[[7]] ] ];
tmpdataclass[["label"]]={"x","Q","residue"};
(*Print[tmpdataclass[["exptinfo","exptid"]] ];*)
tmpdataclass,
{iexpt,1,Length[residueNset]},{flavour,-5,fmax-6}
];

{expterrordatacorrmaxclass,residuedatacorrmaxclass,dRdatacorrmaxclass}=
Table[
Table[
tmpdataclass=Corrsameptdata;
(*set info of the correlation data class*)
tmpdataclass[["PDFinfo"]]=fxQgrid2class[[flavour+6]][["PDFinfo"]];
tmpdataclass[["exptinfo"]]=mydtadata[[iexpt,1]][["exptinfo"]];
(*correlation data of the same expt, flavour are in the same List: {LF[x,Q,corr],...} *)
tmpdataclass[["data"]]=Flatten[corrmaxgridLF[[iexpt,flavour+6]]/.LF[a__]:>LF[{a}[[1]],{a}[[2]],{a}[[icolumn]] ] ];
tmpdataclass[["label"]]={"x","Q","residue"};
(*Print[tmpdataclass[["exptinfo","exptid"]] ];*)
tmpdataclass,
{iexpt,1,Length[residueNset]},{flavour,-5,fmax-6}
],
{icolumn,3,5}
];

dRcorrdataclass=
Table[
tmpdataclass=Corrsameptdata;
(*set info of the correlation data class*)
tmpdataclass[["PDFinfo"]]=fxQgrid2class[[flavour+6]][["PDFinfo"]];
tmpdataclass[["exptinfo"]]=mydtadata[[iexpt,1]][["exptinfo"]];
(*correlation data of the same expt, flavour are in the same List: {LF[x,Q,corr],...} *)
tmpdataclass[["data"]]=Flatten[(*dR*)corrmaxgridLF[[iexpt,flavour+6]]/.LF[a__]:>LF[{a}[[1]],{a}[[2]],{a}[[6]] ] ];
tmpdataclass[["label"]]={"x","Q","residue"};
(*Print[tmpdataclass[["exptinfo","exptid"]] ];*)
tmpdataclass,
{iexpt,1,Length[residueNset]},{flavour,-5,fmax-6}
];
(*
{expterrordatadRcorrmaxclass,residuedatadRcorrmaxclass,dRdatadRcorrmaxclass}=
Table[
Table[
tmpdataclass=Corrsameptdata;
(*set info of the correlation data class*)
tmpdataclass[["PDFinfo"]]=fxQgrid2class[[flavour+6]][["PDFinfo"]];
tmpdataclass[["exptinfo"]]=mydtadata[[iexpt,1]][["exptinfo"]];
(*correlation data of the same expt, flavour are in the same List: {LF[x,Q,corr],...} *)
tmpdataclass[["data"]]=Flatten[dRcorrmaxgridLF[[iexpt,flavour+6]]/.LF[a__]\[RuleDelayed]LF[{a}[[1]],{a}[[2]],{a}[[icolumn]] ] ];
tmpdataclass[["label"]]={"x","Q","residue"};
(*Print[tmpdataclass[["exptinfo","exptid"]] ];*)
tmpdataclass,
{iexpt,1,Length[residueNset]},{flavour,-5,fmax-6}
],
{icolumn,3,5}
];
*)


(* ::Input:: *)
(*Table[Select[corrdataclass[[iexpt,flavour+6]][["data"]],Abs[#[[3]] ]>1.0&],{iexpt,1,Length[residueNset]},{flavour,-5,fmax-6}]*)
(*Table[Select[dRcorrdataclass[[iexpt,flavour+6]][["data"]],Abs[#[[3]] ]>1.0&],{iexpt,1,Length[residueNset]},{flavour,-5,fmax-6}]*)


(* ::Subsection:: *)
(*save into file*)


(* ::Input::Initialization:: *)
(*export into files*)
(*save f(x, Q) grid into .m file*)
corrdataclass;
{expterrordatacorrmaxclass,residuedatacorrmaxclass,dRdatacorrmaxclass};
dRcorrdataclass;
(*{expterrordatadRcorrmaxclass,residuedatadRcorrmaxclass,dRdatadRcorrmaxclass};*)
(*(x,Q) points sets as position at max of corr*)
Export[CorrDataDir<>"corr_"<>CorrDataFile,corrdataclass,"ExpressionML"];
Export[CorrDataDir<>"dRcorr_"<>CorrDataFile,dRcorrdataclass,"ExpressionML"];

Export[CorrDataDir<>"expterror_"<>CorrDataFile,expterrordatacorrmaxclass,"ExpressionML"];
Export[CorrDataDir<>"residue_"<>CorrDataFile,residuedatacorrmaxclass,"ExpressionML"];
Export[CorrDataDir<>"dR_"<>CorrDataFile,dRdatacorrmaxclass,"ExpressionML"];

(*
Export[CorrDataDir<>"corr_grid_data_"<>PDFname<>"_x"<>ToString[Nx]<>"_WZmass"<>".m",corrdataclass,"ExpressionML"];
Export[CorrDataDir<>"expterror_grid_data_"<>PDFname<>"_x"<>ToString[Nx]<>"_WZmass"<>".m",expterrordatacorrmaxclass,"ExpressionML"];
Export[CorrDataDir<>"residue_grid_data_"<>PDFname<>"_x"<>ToString[Nx]<>"_WZmass"<>".m",residuedatacorrmaxclass,"ExpressionML"];
Export[CorrDataDir<>"dR_grid_data_"<>PDFname<>"_x"<>ToString[Nx]<>"_WZmass"<>".m",dRdatacorrmaxclass,"ExpressionML"];
(*(x,Q) points sets as position at max of dR*corr*)
Export[CorrDataDir<>"dRcorr_grid_data_"<>PDFname<>"_x"<>ToString[Nx]<>"_WZmass"<>".m",dRcorrdataclass,"ExpressionML"];
*)
(*
Export[CorrDataDir<>"expterror_dRcorrmax_grid_data_"<>PDFname<>"_x"<>ToString[Nx]<>"_Q"<>ToString[NQ]<>".m",expterrordatadRcorrmaxclass,"ExpressionML"];
Export[CorrDataDir<>"residue_dRcorrmax_grid_data_"<>PDFname<>"_x"<>ToString[Nx]<>"_Q"<>ToString[NQ]<>".m",residuedatadRcorrmaxclass,"ExpressionML"];
Export[CorrDataDir<>"dR_dRcorrmax_grid_data_"<>PDFname<>"_x"<>ToString[Nx]<>"_Q"<>ToString[NQ]<>".m",dRdatadRcorrmaxclass,"ExpressionML"];
*)


(* ::Input::Initialization:: *)
(*20170620*)
Print["data dimension {Nexpt,Nflavour} = ",Dimensions[corrdataclass] ];
Print["data filename = ",CorrDataDir<>"corr_"<>CorrDataFile];
Print["data filename = ",CorrDataDir<>"dRcorr_"<>CorrDataFile];
Print["data filename = ",CorrDataDir<>"expterror_"<>CorrDataFile];
Print["data filename = ",CorrDataDir<>"residue_"<>CorrDataFile];
Print["data filename = ",CorrDataDir<>"dR_"<>CorrDataFile];
(*
Print["data filename = ",CorrDataDir<>"residueNset_"<>CorrDataFile];
*)
Print["data label format example: ",corrdataclass[[1,1]][["label"]] ];
Print["data format example: ",corrdataclass[[1,1]][["data"]][[1]] ];



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
(*tmpdataclass[["label"]]={"x","Q","residue"};*)
(*(*Print[tmpdataclass[["exptinfo","exptid"]] ];*)*)
(*tmpdataclass,*)
(*{iexpt,1,Length[residueNset]},{flavour,-5,5}*)
(*]*)
(**)*)


(* ::Input:: *)
(*Datamethods[["getNcolumn"]][corrdataclass[[1,6]] ]*)
(*Datamethods[["getNpt"]][corrdataclass[[1,6]] ]*)
(*Datamethods[["getNpt"]][mydtadata[[1,1]] ]*)
(*fxQgrid2class[[0+6]][["data"]]/.LF[a__]:>LF[{a}[[1]],{a}[[2]] ];*)
(*corrdataclass[[1,5]][["data"]]*)
(*corrdataclass[[1,14]][["data"]]*)


(* ::Section:: *)
(*others*)


(* ::Input:: *)
(*Dimensions[corrdataclass]*)
(*Table[corrdataclass[[iexpt,1]][["exptinfo","exptid"]],{iexpt,1,Length[corrdataclass]}]*)
(*Table[dRcorrdataclass[[iexpt,1]][["exptinfo","exptid"]],{iexpt,1,Length[dRcorrdataclass]}]*)
(*Table[expterrordatacorrmaxclass[[iexpt,1]][["exptinfo","exptid"]],{iexpt,1,Length[expterrordatacorrmaxclass]}]*)
(*Table[residuedatacorrmaxclass[[iexpt,1]][["exptinfo","exptid"]],{iexpt,1,Length[residuedatacorrmaxclass]}]*)
(*Table[dRdatacorrmaxclass[[iexpt,1]][["exptinfo","exptid"]],{iexpt,1,Length[dRdatacorrmaxclass]}]*)
(**)
(*Union[Table[corrdataclass[[iexpt,1]][["data"]]/.LF[a__]:>Take[{a},2],{iexpt,1,Length[corrdataclass]}]//Flatten]*)


(* ::Input:: *)
(*corrdataclass[[2,2]]*)


(* ::Input:: *)
(**)
(*(*setup for arguments*)*)
(*(**)
(*title="Corr(g(x,Q), residue)";*)
(*xtitle="x";*)
(*ytitle="mu";*)
(*plotrange={0.00001,1,1,2000};*)
(*stretchx=1;stretchy=1;*)
(*barseperator={-1.0,-0.85,-0.7,-0.5,0.5,0.7,0.85,1.0};*)
(*legendlabel="";*)
(*epilogtext={};*)
(*highlightrange={0.2,100};*)
(*unhighlightsize=0.0075;*)
(**)*)
(**)


(* ::Input:: *)
(*(**)
(**)
(*(*correlation plot*)*)
(*PDFCorrelationplot7[Select[Flatten[corrdataclass[[3,11]][["data"]] ],Abs[#[[3]] ]>0.3&]/.LF\[Rule]LF1,title,xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,epilogtext,highlightrange,unhighlightsize]*)
(**)
(*binset={-1.0,-0.85,-0.7,-0.5,0.0,0.5,0.7,0.85,1.0};*)
(*lineelement={{binset[[2]],"",Blue},{binset[[3]],"",Blue},{binset[[4]],"",Blue},{binset[[6]],"",Blue},{binset[[7]],"",Blue},{binset[[8]],"",Blue}};*)
(**)
(*(*histogram*)*)
(*histplot4[Flatten[corrdataclass[[1,5]][["data"]] ]/.LF[a__]\[RuleDelayed]{a}[[3]],title,xtitle,ytitle,binset,lineelement,{0,1.0},20]*)
(**)
(**)*)


(* ::Input:: *)
(*exptlistAll*)


(* ::Input:: *)
(*testdtaexpts=ReadExptTable[myPDFsetdtafile,"ct2016"]*)


(* ::Input:: *)
(*exptlistdta=Select[Table[testdtaexpts[[iexpt,1]],{iexpt,1,Length[testdtaexpts]}],(#>99&&#<200)||(#>199&&#<300)||(#>499&&#<600)&]*)


(* ::Input:: *)
(*Complement[exptlistdta,exptlistAll]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*DtaExptList[DtaDir]*)


(* ::Input:: *)
(*mydtadata[[1,1]][["exptinfo"]]*)


(* ::Input:: *)
(*Length[mydtadata[[1]] ]*)
(*mydtadata[[3,5]][["data"]]/.LF[a__]:>{a}[[6]]/{a}[[4]]*)



