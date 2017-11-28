(* ::Package:: *)

(* ::Chapter:: *)
(*read package *)


(* ::Input::Initialization:: *)
 (*SetDirectory["/home/botingw/Downloads"];*)(*other files are under the same directory*)
(*20170620: for script version, executables could not run pdfparse correctly, so modify the path setting*)
SetDirectory[(*NotebookDirectory[]*)DirectoryName[$InputFileName] ];(*Print[Directory[] ];SetDirectory[NotebookDirectory[] ];Print[Directory[] ];*)
Print["present directory: ",Directory[]];
libdir="../lib/";
lib1=libdir<>"pdfParsePDS2013.m";
lib2=libdir<>"dtareadbotingw2016.m";
If[FileExistsQ[lib1],Print["loading ",lib1];Get[lib1],Print["library file ",lib1," doesn't exist"] ];
If[FileExistsQ[lib2],Print["loading ",lib2];Get[lib2],Print["library file ",lib2," doesn't exist"] ];
(*
<<"pdfParsePDS2013.m"
<<"dtareadbotingw2016.m"
*)

(*
Get[libdir<>"pdfParsePDS2013.m"]
Get[libdir<>"dtareadbotingw2016.m"]
*)


(* ::Chapter:: *)
(*Global variables*)


(* ::Input::Initialization:: *)
(*global variables*)
(*2017.04.26, for new CT14NNLO, expid = 245,246,247,542,544 added*)
(*proton=======================*)

PDISNCl={101,106,169};
PDISNClccbar={147};
PDISNClbbbar={145};
PDISNClqqbar=Flatten[{PDISNClccbar,PDISNClbbbar}];
PDISNC=Flatten[{PDISNCl,PDISNClqqbar}];

PDISCCl1={};
PDISCCl2={};
PDISCC=Flatten[{PDISCCl1,PDISCCl2}];

PVBPZ={204,260,261}~Join~{246}~Join~{247,249};(*first 1: pp, left 2: ppbar*)
PVBPW={241,266,267,225,227,234,281}~Join~{250};(*first 3: pp, left 4: ppbar*)
PJP={535,538,504,514}~Join~{542,544}~Join~{545};(*first 2: pp, left 1: ppbar*)
PTTBAR={565,566,567,568};
(*neutron=======================*)

NDISNCl={};
NDISNClqqbar={};
NDISNC=Flatten[{NDISNCl,NDISNClqqbar}];

NDISCCl1={};
NDISCCl2={124,125};
NDISCC=Flatten[{NDISCCl1,NDISCCl2}];

NVBPZ={};
NVBPW={};
NJP={};
(*complex nucleus=======================*)

hDISNCl={102,104};
hDISNClqqbar={};
hDISNC=Flatten[{hDISNCl,hDISNClqqbar}];

hDISCCl1={108,109,110,111};
hDISCCl2={126,127};
hDISCC=Flatten[{hDISCCl1,hDISCCl2}];

hVBPZ={201,203};
hVBPW={};
hJP={};

(*combine types=======================*)
PDISNCCC={159,160};
PVBPWZ={240,268}~Join~{245,248,250};


(*set the title of the output plots of processdataplotsmultiexp6percentage*)
(*if False, plots have no title*)
PlotTitleBool=True;


(* ::Title:: *)
(*Correlation plots project*)


(* ::Chapter:: *)
(*data class*)


(* ::Section:: *)
(*atomic data class*)


(* ::Subsection:: *)
(*expinfo class*)


(* ::Input::Initialization:: *)
Exptinfo=
<|
"exptid"->"unset",
"exptname"->"unset",
"feyndiagram"->"unset"
|>


(* ::Subsection:: *)
(*PDFinfo class*)


(* ::Input::Initialization:: *)
PDFinfo=
<|
"PDFname"->"unset",
"PDFsetmethod"->"unset",
"Nset"->"unset",
"iset"->"unset",
"flavour"->"unset"
|>


(* ::Subsection:: *)
(*data class*)


(* ::Input::Initialization:: *)
Data=
<|
"label"->"unset",
"data"->"unset"
|>


(* ::Section:: *)
(*data class specific for .dta data, f(x,Q) data (from .pds), correlation data*)


(* ::Subsection:: *)
(*dtadata class*)


(* ::Input::Initialization:: *)
Dtadata=
Join[
Data,
<|
"exptinfo"->Exptinfo,
"PDFinfo"->PDFinfo
|>,
(*2017.01.16 add raw data*)
<|"rawdata"->"unset"|>
]


(* ::Subsection:: *)
(*fxQdata class*)


(* ::Input::Initialization:: *)
FxQdata=
Join[
Data,
<|
"PDFinfo"->PDFinfo
|>
]


(* ::Subsection:: *)
(*fxQsameptdata class*)


(* ::Input::Initialization:: *)
FxQsameptdata=
Join[
Data,
<|
"exptinfo"->Exptinfo,
"PDFinfo"->PDFinfo
|>
]


(* ::Subsection:: *)
(*corrdata class*)


(* ::Input::Initialization:: *)
Corrdata=
Join[
Data,
<|
"PDFinfo"->PDFinfo
|>
]


(* ::Subsection:: *)
(*corrsameptdata class*)


(* ::Input::Initialization:: *)
Corrsameptdata=
Join[
Data,
<|
"exptinfo"->Exptinfo,
"PDFinfo"->PDFinfo
|>
]


(* ::Subsection:: *)
(*datamethods class*)


(* ::Input::Initialization:: *)
(*datain={LF[a1__],LF[a2__],...}, dataadd={LF[b1__],LF[b2__],...}, length of two data are the same*)
(*output: {LF[a1__,b1__],LF[a2__,b2__],...}*)
LFadd[datain_,dataaddin_]:=
Module[{data=datain,dataadd=dataaddin,Npt,Nptadd},
Npt=Length[datain];
Nptadd=Length[dataaddin];
If[Npt!=Nptadd,Print["error, #points of data & added data are different"];(*Return[0]*)Abort[] ];
Join[data,dataadd,2]
];

(*pick specific elements in LF*)
(*ex:LFpick[test1,{1,3,5}] output LF[{a}[[1]],{a}[[3]],{a}[[5]] ]*)
LFpick[datain_,picklistin_]:=
Module[{data=datain,picklist=picklistin},
Llist=Length[picklist];
data=data/.LF[a__]:>Apply[LF,Table[{a}[[picklist[[i]] ]],{i,1,Llist}] ];
data
]

(*apply Take rule on all LF elements*)
LFtake[datain_,takelistin_]:=
Module[{data=datain,takelist=takelistin},

data=data/.LF[a__]:>Apply[LF,Take[{a},takelist] ];
data
]

(*apply Delete rule on all LF elements*)
LFdelete[datain_,deletelistin_]:=
Module[{data=datain,deletelist=deletelistin},

data=data/.LF[a__]:>Apply[LF,Delete[{a},deletelist] ];
data
]

(*transf all LF[...] in data to List*)
LFtolist[datain_]:=
Module[{data=datain},

data=data/.LF:>List;
data
]

(*pick columns of data to list*)
(*
ex: LFpicktoList[data,{3,1}] output {x3list,x1list}
x3list = {LF[[3]],LF[[3]],...}, the same for x1list
*)
LFpicktolist[datain_,picklistin_]:=
Module[{data=datain,picklist=picklistin},
Llist=Length[picklist];
data=data/.LF[a__]:>Table[{a}[[picklist[[i]] ]],{i,1,Llist}];
(*transpose from [[Npt,xi]] to [[xi,Npt]] *)
data=Transpose[data,{2,1}];

data
]


(* ::Input::Initialization:: *)



Datamethods=
<|
"getdatainfo"->
Function[{dataclass},
keys=Keys[dataclass];
Table[
If[keys[[i]]!= "data",Print[keys[[i]],":\n",dataclass[[keys[[i]] ]] ] ],
{i,1,Length[keys]}]
],
"getdata"->Function[data,data[["data"]] ],
"setdata"->Function[{dataclass,datain},dataclass[["data"]]=datain ],
"getNpt"->Function[data,Length[data[["data"]] ] ],
"getNcolumn"->Function[data,Length[data[["data"]][[1]] ] ],
(*
"getxQ"->"unset",
*)
"getdatalabel"->Function[{dataclass},dataclass[["label"]] ],
"add"->
Function[{dataclassin,dataaddin,labeladdin},
Module[{dataclass=dataclassin,dataadd=dataaddin,labeladd=labeladdin},
(*
If[Dimensions[dataclass[["data"]] ]!=Dimensions[dataadd],Print["error, dimension of data & added data are different"];Return[0] ];
If[Dimensions[dataclass[["label"]] ]!=Dimensions[labeladd],Print["error, dimension of label & added label are different"]Return[0] ];
*)
dataclass[["label"]]=Join[dataclass[["label"]],labeladd];
dataclass[["data"]]=LFadd[dataclass[["data"]],dataadd];
dataclass
(*
dataclass[["label"]]=Join[dataclass[["label"]],labeladd];
dataclass
*)
]
],
"pick"->
Function[{dataclassin,picklistin},
Module[{dataclass=dataclassin,picklist=picklistin},
(*change data*)
dataclass[["data"]]=LFpick[dataclass[["data"]],picklist];
(*change data label, transf Head of it to LF, do the same thing as what you do on data, then transf back to List*)
dataclass[["label"]]=LFpick[Apply[LF,dataclass[["label"]] ],picklist];
dataclass[["label"]]=Apply[List,dataclass[["label"]] ];

dataclass
]
],
"take"->
Function[{dataclassin,takelistin},
Module[{dataclass=dataclassin,takelist=takelistin},
(*change data*)
dataclass[["data"]]=LFtake[dataclass[["data"]],takelist];
(*change data label, transf Head of data label to LF, do the same thing as what you do on data, then transf back to List*)
dataclass[["label"]]=LFtake[Apply[LF,dataclass[["label"]] ],takelist];
dataclass[["label"]]=Apply[List,dataclass[["label"]] ];

dataclass
]
],
"delete"->
Function[{dataclassin,deletelistin},
Module[{dataclass=dataclassin,deletelist=deletelistin},
(*change data*)
dataclass[["data"]]=LFdelete[dataclass[["data"]],deletelist];
(*change data label, transf Head of data label to LF, do the same thing as what you do on data, then transf back to List*)
dataclass[["label"]]=LFdelete[Apply[LF,dataclass[["label"]] ],deletelist];
dataclass[["label"]]=Apply[List,dataclass[["label"]] ];

dataclass
]
],
"tolist"->
Function[{dataclassin},
Module[{dataclass=dataclassin},
(*change data*)
dataclass[["data"]]=dataclass[["data"]]/.LF:>List;

dataclass[["data"]]
]
],(*LFpicktoList[datain_,picklistin_]*)
"picktolist"->
Function[{dataclassin,picklistin},
Module[{dataclass=dataclassin,picklist=picklistin},
(*change data*)
dataclass[["data"]]=LFpicktolist[dataclass[["data"]],picklist];

dataclass[["data"]]
]
],
(*
"reorder"->"unset",
"extract"->"unset",
*)
(*2017.01.16: discard dtaread2016boting`Private` from dtaread2016boting`Private`LF*)
"LFglobal"->
Function[{dataclassin},
Module[{dataclass=dataclassin},
(*change data*)
dataclass[["data"]]=dataclass[["data"]]/.dtaread2016boting`Private`LF ->LF;
dataclass
]
]
|>



(* ::Chapter:: *)
(*.dta data class*)


(* ::Section:: *)
(*read .dta file into format of dtadata class*)


(* ::Subsection:: *)
(*readdtafile class*)


(* ::Input::Initialization:: *)
(*test function: read explist, save exp data with the same exptid into "exptdata"*)
readexptsbydta[dtaDirin_,explistin_]:=
Module[{dtaDir=dtaDirin,explist=explistin,residuetmp,dtafiles,exptdata,Nset,Nexpt},
(*get all .dta files under dtaDir*)
dtafiles=FileNames[dtaDir<>"*dta"];

(*read all data from .dta files*)
residuetmp=
Table[
ReadExptTable[dtafiles[[f]],"ct2016"],
(*
makeobsdataset[ReadExptTable[dtafiles[[f]],"ct2016"],Nexp,"dummy",obs] ,(*//Flatten,*)
*)
{f,1,Length[dtafiles]}
];

Dimensions[residuetmp];
(*
residuetmp[[1,10,7]]
residuetmp[[1,10,7]]/.dtaread2016boting`Private`LF\[RuleDelayed]List
*)

(*variable to save data we want (in explist)*)
exptdata={};
(*assume #files is the same as Nset*)
Nset=Dimensions[residuetmp][[1]];
Nexpt=Dimensions[residuetmp][[2]];
(*if expt you want in the .dta file, read all iset .dta files into variable "exptdata"*)
Table[
If[
explist[[iexplist]]==residuetmp[[1,iexpt,1]],
Print["find exptid = ",explist[[iexplist]],"." ];
exptdata=Append[exptdata,Table[residuetmp[[iset,iexpt]],{iset,1,Nset}] ] 
];

"dummy"
(*explist[[i]]*)
,{iexplist,1,Length[explist]},{iexpt,1,Nexpt}
];

(*return exp datas with dimensions [[iexpt,iset]]*)
exptdata
]

(*test transf output of ReadExptTable into dtadata class form*)
todtadataclass[datain_,PDFnamein_,PDFsetmethodin_]:=
Module[{data=datain,PDFname=PDFnamein,PDFsetmethod=PDFsetmethodin,Dtadatatmp,Ndatacolumn},
Dtadatatmp=Dtadata;
Dtadatatmp[["data"]]=data[[7]];
Dtadatatmp[["exptinfo","exptid"]]=data[[1]];
Dtadatatmp[["exptinfo","exptname"]]=data[[2]];
Dtadatatmp[["PDFinfo","PDFname"]]=PDFname;
Dtadatatmp[["PDFinfo","PDFsetmethod"]]=PDFsetmethod;

Dtadatatmp[["label"]]=StringSplit[data[[3]] ];
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
Dtadatatmp[["rawdata"]]=Delete[data,7];

Dtadatatmp
]


(* ::Input::Initialization:: *)
Readdtafile=
<|
"readdta"->readexptsbydta,
"toclass"->todtadataclass
|>


(* ::Section:: *)
(*class for operate (edit) dtadata class*)


(* ::Subsection:: *)
(*dtaobs class*)


(* ::Input::Initialization:: *)
(*function for calculating residual of dta data*)
addresidue[dataclassin_]:=
Module[{dataclass=dataclassin,ExptID,expItype,residue,NormFac},
ExptID=dataclass[["exptid"]];
expItype=ExptIDinfo[ExptID];

(*get residual method 1*)
(*
residue=data[[7]]/.LF[a__]\[RuleDelayed]{a}[[13]];
residue=Map[If[#>0,Sqrt[#],-Sqrt[-#]]&,residue,{1}];
*)

(*get residual method 2*)
(*result of method1&2 mostly diff in 5%, method 2 should be accurate*)
(*2017.01.18: NormFac should be in formula or not? with NormFac=1, result of residual is close to "ReducedChi2" *)
NormFac=dataclass[["rawdata"]][[4]];
(*v1*)
(* v1 is wrong
residue=dataclass[["data"]]/.LF[a__]\[RuleDelayed]LF[({a}[[5]]*NormFac-{a}[[11]])/{a}[[12]] ];
*)
(*v2*)

residue=dataclass[["data"]]/.LF[a__]:>LF[({a}[[5]]-{a}[[11]])/{a}[[12]] ];

dataclass=Datamethods[["add"]][dataclass,residue,{"residual"}];
dataclass
]



(* ::Chapter:: *)
(*.pds data class*)


(* ::Section:: *)
(*functions in class*)


(* ::Subsection:: *)
(*calculate f(x,Q,flavour)*)


(* ::Input::Initialization:: *)
pdflist[xQlistin_,flavourin_,ifamilyin_]:=
Module[{xQlist=xQlistin,flavour=flavourin,ifamily=ifamilyin,x,Q,Nset,output},
pdfSetActiveFamily[ifamily]; (* choose PDF family *)
Nset=Length[pdfSetList[[ifamily]]] ;(* number of PDF sets *)
output=Table[
x=xQlist[[ix,1]];
Q=xQlist[[ix,2]];
{x,Q,Table[pdfCTEQ[x,Q,flavour,iset],{iset,Nset}]},
{ix,1,Length[xQlist]}
];
output
];

pdfLF[xQLFin_,flavourin_,ifamilyin_]:=
Module[{xQLF=xQLFin,flavour=flavourin,ifamily=ifamilyin,x,Q,Nset,output},
pdfSetActiveFamily[ifamily]; (* choose PDF family *)
Nset=Length[pdfSetList[[ifamily]]] ;(* number of PDF sets *)
output=Table[
x=xQLF[[ix,1]];
Q=xQLF[[ix,2]];
(*for flavour = {-5,5}, direct use the f(x,Q,flavour)
if it is > 5, we define it's pdf as following:
*)

LF[x,Q,Sequence@@Table[pdfCTEQ[x,Q,flavour,iset],{iset,Nset}] ],
{ix,1,Length[xQLF]}
];
output
];




(* ::Input::Initialization:: *)
(*input PDFDir, xQLF,PDFsetmethod, and flavour, output FxQdata class*)
fxQcalculate[xQLFin_,PdsDirin_,PDFsetmethodin_,flavourin_]:=
Module[{xQLF=xQLFin,PdsDir=PdsDirin,PDFsetmethod=PDFsetmethodin,flavour=flavourin,fxQclasstmp,ifamily,Nset,datalabel,xQlabel,PDFname},
fxQclasstmp=FxQdata;

(*setup pdf function*)
pdfResetCTEQ;
(*
For[i=1,i\[LessEqual]20,i++,pdfFamilyParseCTEQ["Dummy"]];
*)
(*generate a pdf space*)
pdfFamilyParseCTEQ["Dummy"];
ifamily=1; 
(* IniDir="//users//nadolsky//share//lhapdf//6.1.5//share/LHAPDF//CT14nnlo//pds//"; *)
pdfFamilyParseCTEQ[PdsDir<>"*pds",ifamily];

Nset=Length[pdfSetList[[ifamily]] ];(* number of PDF sets *)
(*set data label*)
datalabel=Table[ToString[i-1],{i,1,Nset}];
xQlabel={"x","Q"};
datalabel=Join[xQlabel,datalabel];
(*set PDFname*)
PDFname=StringSplit[PdsDir,"/"][[-1]];
(*set info of class*)
fxQclasstmp[["label"]]=datalabel;
fxQclasstmp[["PDFinfo","PDFname"]]=PDFname;
fxQclasstmp[["PDFinfo","PDFsetmethod"]]=PDFsetmethod;
fxQclasstmp[["PDFinfo","Nset"]]=Nset;
fxQclasstmp[["PDFinfo","flavour"]]=flavour;
(*calculate data by xQ*)
fxQclasstmp[["data"]]=pdfLF[xQLF,flavour,ifamily];
(*output*)
fxQclasstmp

];


(* ::Subsection:: *)
(*calculate f(x,Q,flavour) by same point of experiments (from dtadata class)*)


(* ::Input::Initialization:: *)
(*read xQLF from Dtadata, pds file from PdsDir and set flavour
output FxQsameptdata class, 
"exptinfo" is copy from Dtadata[["exptinfo"]], "PDFinfo" is set by input parameter *)
fxQsameptcalculate[dtadataclassin_,PdsDirin_,PDFsetmethodin_,flavourin_]:=
Module[{dtadataclass=dtadataclassin,PdsDir=PdsDirin,PDFsetmethod=PDFsetmethodin,flavour=flavourin,fxQclasstmp},
fxQclasstmp=FxQsameptdata;

(*2017,01,12*)
(*here we do not deal with part: extract {x,Q} from dtadata, this part need modified later.  *)
fxQclasstmp=fxQcalculate[dtadataclass[["data"]],PdsDir,PDFsetmethod,flavour];
fxQclasstmp[["exptinfo"]]=dtadataclass[["exptinfo"]];
fxQclasstmp
]


(* ::Input::Initialization:: *)
(*input fxQdataclass[[flavour]], flavour from -5 ~ 5, output the customized f(x,Q), as following:
dbar/ubar, d/u, s+sbar/ubar+dbar
*)
(*the way to calculate the combination of pdf could be modify: if f(x,Q,flavour)=0, then don't consider the point*)


setextrafxQ[fxQdataclasslistin_]:=
Module[{fxQdataclasslist=fxQdataclasslistin,bbar,cbar,sbar,dbar,ubar,g,u,d,s,c,b,Npt,Nset,newfxQdata,LFa,LFb,LFc,LFd,x,Q,Nsetdata,fxQmincut,dbarubar,du,ssbarubardbar},
(*set quark label*)
{bbar,cbar,sbar,dbar,ubar,g,u,d,s,c,b}={-5+6,-4+6,-3+6,-2+6,-1+6,0+6,1+6,2+6,3+6,4+6,5+6};
(*set variables, assume all of fxQdataclasslist have the same Npt, label*)
Npt=Datamethods[["getNpt"]][fxQdataclasslist[[1]] ];
Nset=Datamethods[["getNcolumn"]][fxQdataclasslist[[1]] ]-2;
(*pdf minimum cut for denominator*)
fxQmincut=10.0^-10;
(*set classes for newly defined flavours*)
dbarubar=fxQdataclasslist[[1]];
du=fxQdataclasslist[[1]];
ssbarubardbar=fxQdataclasslist[[1]];

(*dbar/ubar*)
LFa=fxQdataclasslist[[dbar]][["data"]];
LFb=fxQdataclasslist[[ubar]][["data"]];

dbarubar[["data"]]=
Table[
x=LFa[[ix,1]];Q=LFa[[ix,2]];
Nsetdata=
Table[
If[
LFa[[ix,iset+2]]!=0 && LFb[[ix,iset+2]]!=0, 
LFa[[ix,iset+2]]/Max[LFb[[ix,iset+2]],fxQmincut],
0.0
],
{iset,1,Nset}
];
LF[x,Q,Sequence@@Nsetdata],
{ix,1,Npt}
];

(*d/u*)
LFa=fxQdataclasslist[[d]][["data"]];
LFb=fxQdataclasslist[[u]][["data"]];

du[["data"]]=
Table[
x=LFa[[ix,1]];Q=LFa[[ix,2]];
Nsetdata=
Table[
If[
LFa[[ix,iset+2]]!=0 && LFb[[ix,iset+2]]!=0, 
LFa[[ix,iset+2]]/Max[LFb[[ix,iset+2]],fxQmincut],
0.0
],
{iset,1,Nset}
];
LF[x,Q,Sequence@@Nsetdata],
{ix,1,Npt}
];

(*s+sbar/ubar+dbar*)
LFa=fxQdataclasslist[[s]][["data"]];
LFb=fxQdataclasslist[[sbar]][["data"]];
LFc=fxQdataclasslist[[ubar]][["data"]];
LFd=fxQdataclasslist[[dbar]][["data"]];

ssbarubardbar[["data"]]=
Table[
x=LFa[[ix,1]];Q=LFa[[ix,2]];
Nsetdata=
Table[
If[
LFa[[ix,iset+2]]!=0 && LFb[[ix,iset+2]]!=0&& LFc[[ix,iset+2]]!=0&& LFd[[ix,iset+2]]!=0, 
(LFa[[ix,iset+2]]+LFb[[ix,iset+2]])/Max[(LFc[[ix,iset+2]]+LFd[[ix,iset+2]]),fxQmincut],
0.0
],
{iset,1,Nset}
];
LF[x,Q,Sequence@@Nsetdata],
{ix,1,Npt}
];

{dbarubar,du,ssbarubardbar}
]




(* ::Section:: *)
(*read .pds file into format of fxQdata(fxQsameptdata) class*)


(* ::Subsection:: *)
(*pdsread class*)


(* ::Input::Initialization:: *)
Pdsread=
<|
"fxQ"->fxQcalculate,
"fxQsamept"->fxQsameptcalculate
|>




(* ::Chapter:: *)
(*correlation data class*)


(* ::Section:: *)
(*calculate correlation function (general version)*)
(*input any two observables, get correlation*)


(* ::Subsection:: *)
(*corrcalculate class*)


(* ::Input::Initialization:: *)
(*20170315: for web version, we don't need PDF library and initilize PDFsets, but we still need correlation function. so make a fake function work here
all functions use these two functions replaced by fake version*)

pdfHessianSymErrorfake[f_]:=Module[{Neigen},
If[ListQ[f],Neigen=Length[f],Print["pdfHessianSymErrorfake input should be list "];Abort[] ];
If[OddQ[Neigen],
1/2 Sqrt[Sum[
If[ListQ[f],(f[[2*iset+1]]-f[[2*iset]])^2,(f[2*iset+1]-f[2*iset])^2],
{iset,1,(Neigen-1)/2}]],
Print["Error: pdfHessianSymErrorfake requires an odd number of entries, not ",Neigen]]
]; 

pdfHessianCorrelationfake[list1_, list2_] :=Module[{Neigen,Neigen1, Neigen2, PDFerror1, PDFerror2},
   If[ ! ListQ[list1] || ! ListQ[list2], 
    Print["pdfHessianCorrelationfake: both arguments must be lists"]; Return];
   If[Length[list1] != Length[list2],
    Print["Problem: length of the lists do not match, ", 
     Length[list1] , "!=", Length[list2]; Return]
    ];
   Neigen = Length[list1];
   If[EvenQ[Neigen], Print["Stop, an even number of eigenvectors: ", Neigen]; Return];
   {PDFerror1, PDFerror2} = Max[pdfHessianSymErrorfake[#], 10^-8] & /@ {list1, list2};
   1/(PDFerror1 PDFerror2) 1/4 Sum[(list1[[2*iset + 1]] - list1[[2*iset]])*(list2[[2*iset + 1]] - list2[[2*iset]]), {iset, 1, (Neigen - 1)/2}]
   ];




(* ::Input::Initialization:: *)
(*input: obsALF={LF[N elements],...}, obsBLF: same as obsALF*)
(*output list of correlation: {corr1,corr2,corr3,......}*)
(*2017.01.12 not yet consider MC correlation part*)
corrAB[obsALFin_,obsBLFin_]:=
Module[{obsALF=obsALFin,obsBLF=obsBLFin,NA,NB,NptA,NptB,Npt,correlation},
NA=Length[obsALF[[1]] ];
NB=Length[obsBLF[[1]] ];
If[NA!=NB,Print["error, two observables has different length"];Return[0] ];
(*# of points*)
NptA=Length[obsALF];
NptB=Length[obsALF];
If[NptA!=NptB,Print["error, two observables has different #points"];Return[0] ];
Npt=NptA;

(*calculate correlation for all points*)
correlation=Table[
pdfHessianCorrelationfake[obsALF[[ix]]/.LF->List,obsBLF[[ix]]/.LF->List],
{ix,1,Npt}];
(*output the list of correlation for all points*)
correlation
];



(* ::Section:: *)
(*calculate correlation function (specific for corr(f(x,Q,flavour), obs(x,Q)), obs==residual, deltaR*residual)*)
(*and LF[x,Q,deltaR] data,*)
(*they are for plots of default*)


(* ::Subsection:: *)
(*corrfxQresiduesamept class*)


(* ::Input::Initialization:: *)
(*input a dtadata class with data\[Equal]{LF[x,Q,obs1,obs2,...obsNset],...}
and a fxQsameptdata class with data\[Equal]{LF[x,Q,f(x,Q,flavour,iset1),...,f(x,Q,flavour,Nset)],...},
the function assume {x,Q} of dtadataclass & fxQsameptdataclass are the same
output the corrsameptdata class*)
corrfxQdtaobs[dtadataclassin_,fxQsameptdataclassin_]:=
Module[{dtadataclass=dtadataclassin,fxQsameptdataclass=fxQsameptdataclassin,corrsameptdatatmp,obsALF,obsBLF,corrdatatmp,Npt,x,Q},
(*set info part of output class*)
corrsameptdatatmp=Corrsameptdata;
corrsameptdatatmp[["exptinfo"]]=dtadataclass[["exptinfo"]];
corrsameptdatatmp[["PDFinfo"]]=dtadataclass[["PDFinfo"]];
(*set f(x,Q) data (delete x,Q part of data)*)
obsALF=LFdelete[fxQsameptdataclass[["data"]],{{1},{2}}];
(*set obs data*)
obsBLF=LFdelete[dtadataclass[["data"]],{{1},{2}}];

(*calculate correlation*)
corrdatatmp=corrAB[obsALF,obsBLF];
(*{corr(x1,Q1),...}\[Rule]{LF[x,Q,corr(x1,Q1)]}*)
Npt=Length[obsALF];
corrdatatmp=
Table[
(*assume {x,Q} of dtadataclass & fxQsameptdataclass are the same*)
x=dtadataclass[["data"]][[ix,1]];
Q=dtadataclass[["data"]][[ix,2]];
LF[x,Q,corrdatatmp[[ix]] ]
,
{ix,1,Npt}
];

corrsameptdatatmp[["data"]]=corrdatatmp;
corrsameptdatatmp[["label"]]={"x","Q","corr"};
(*output*)
corrsameptdatatmp
(*
{obsALF,obsBLF,corrdatatmp}
*)
];


(* ::Input::Initialization:: *)
(*input a dtadata class with data\[Equal]{LF[x,Q,obs1,obs2,...obsNset],...}
and a fxQsameptdata class with data\[Equal]{LF[x,Q,f(x,Q,flavour,iset1),...,f(x,Q,flavour,Nset)],...},
the function assume {x,Q} of dtadataclass & fxQsameptdataclass are the same
output the corrsameptdata class with dr*)
dRcorrfxQdtaobs[dtadataclassin_,fxQsameptdataclassin_]:=
Module[{dtadataclass=dtadataclassin,fxQsameptdataclass=fxQsameptdataclassin,deltaR,Npt,Nset,output,drcorrtmp},

deltaR=getdeltaR[dtadataclass];
Npt=Datamethods[["getNpt"]][dtadataclass];
Nset=Datamethods[["getNcolumn"]][dtadataclass]-2;(*Ncolumn = Nset + 2 (x&Q)*)

drcorrtmp=corrfxQdtaobs[dtadataclass,fxQsameptdataclass];
(*calculate dr*corr for all points*)
Table[
drcorrtmp[["data"]][[ix,3]]=drcorrtmp[["data"]][[ix,3]]*deltaR[[ix]],
{ix,1,Npt}
];

drcorrtmp
]


(* ::Input::Initialization:: *)
corrfxQdtaresidue[dtadataclassin_,fxQsameptdataclassin_]:=
Module[{},
"testoutput"
]




(* ::Input::Initialization:: *)
(*input a class with LF[x,Q,iset1,...,Nset]*)
(*output deltaR as List: {dR1,dR2,...dR Npt}*)

getdeltaR[dtadataclassin_]:=
Module[{dtadataclass=dtadataclassin,Npt,observable,deltaR,PDFerror1},
Npt=Datamethods[["getNpt"]][dtadataclass];
(*take Nset of observables (delete x,Q)*)
(*{LF[iset,...,Nset],...}*)
observable=Datamethods[["take"]][dtadataclass,{3,-1}];
observable=Datamethods[["tolist"]][observable];

deltaR=
Table[
PDFerror1= Max[pdfHessianSymErrorfake[observable[[ix]] ], 10^-8],
{ix,1,Npt}];
deltaR
]


getdeltaRclass[dtadataclassin_]:=
Module[{dtadataclass=dtadataclassin,Npt,observable,deltaR,PDFerror1},
Npt=Datamethods[["getNpt"]][dtadataclass];
(*take Nset of observables (delete x,Q)*)
(*{LF[iset,...,Nset],...}*)
observable=Datamethods[["take"]][dtadataclass,{1,2}];
deltaR=getdeltaR[dtadataclass];
(*transf to {LF[],LF[],...}*)
deltaR=
Table[LF[deltaR[[ix]] ],{ix,1,Npt}];

(*{x,Q} + {deltaR}*)
observable=Datamethods[["add"]][observable,deltaR,{"deltaR"}];

observable
]


(* ::Input::Initialization:: *)
corrfxQresiduesamept=
<|
"corrsamept"->corrfxQdtaobs,
"dRcorrsamept"->dRcorrfxQdtaobs,
"deltaR"->getdeltaR,
"residue"->corrfxQdtaresidue
|>


(* ::Chapter:: *)
(*read/write class*)


(* ::Section:: *)
(*IO of a general data class*)


(* ::Subsection:: *)
(*IO number transform: always transform a number into scientific notation*)


(* ::Input::Initialization:: *)
(*show a number to scientific form: ex: 312.5 \[Rule] 3.125E2*)
ToSciForm[N_,decimal_]:=
Module[{output},
output=ScientificForm[N,decimal,NumberFormat->(Row[{#1,"E",#3}]&)];
(*case: if becom N1E, it means N1E0, ex: 3.55E means 3.55*10^0 = 3.55E0*)
If[Length[StringSplit[ToString[output],"E"] ] ==1,output=ScientificForm[N,decimal,NumberFormat->(Row[{#1,"E","0"}]&)] ];
(*case of 0, it will transf to 0E, so we need to modify to 0E0*)
(*
If[N\[Equal]0 || (Abs[N]\[GreaterEqual]1 && Abs[N]<10),output=ScientificForm[N,decimal,NumberFormat\[Rule](Row[{#1,"E","0"}]&)] ];
*)
output
]

(*transf a string of scientific notation to number, ex: "3.125E2" \[Rule] 312.5*)
(*mathematica could not transf it, *)
SciStrToNumber[str_]:=
Module[{strtmp},
strtmp=StringSplit[str,"E"];
If[Length[strtmp]!=2,Print["error, input is not a number with scientific notation, ",str] ];
ToExpression[strtmp[[1]]<>"*"<>"10^"<>strtmp[[2]] ]
]


(* ::Subsection:: *)
(*write data class*)


(* ::Subsection:: *)
(*read data class*)


(* ::Section:: *)
(*IO of fxQdata class*)


(* ::Input:: *)
(*(*fxQ format for IO:*)
(*pdfflavours[[#flavour]][[Npt]][[ x, Q, {#iset of pdf(x,Q)}]]*)
(**)*)


(* ::Subsection:: *)
(*write fxQdata class*)


(* ::Input:: *)
(*(*input data format: {LF[x,Q,iset,...,Nset],LF[],...}, transform it's data to format of IO (used by function pdfwritesameptnew)*)*)
(*LFtoIOformat[datain_]:=*)
(*Module[{data=datain,output},*)
(*output=data/.LF[a__]:>{{a}[[1]],{a}[[2]],Take[{a},{3,-1}]};*)
(*output*)
(*]*)
(**)
(*IOtoLFformat[datain_]:=*)
(*Module[{data=datain,Npt,Nset,fmax,x,Q,output},*)
(**)
(*Npt=Length[data];*)
(*output=*)
(*Table[*)
(*(*x and Q*)*)
(*x=data[[ix,1]];Q=data[[ix,2]];*)
(*LF[x,Q,Sequence@@data[[ix,3]] ],*)
(*{ix,1,Npt}*)
(*];*)
(**)
(*output*)
(*]*)
(*(*input a IO (used by function pdfwritesameptnew), transform it's data to format of fxQclass*)*)
(*(**)
(*IOtofxQclassformat[pdfflavoursin_,exptidin_,exptnamein_,PDFnamein_,PDFsetmethodin_,flavourin_]:=*)
(*Module[{pdfflavours=pdfflavoursin,Npt,Nset,fmax,x,Q,output},*)
(*(*set variables*)*)
(*Npt=Length[pdfflavours[[1]] ];(*assume all points have the same points, it could be modified to Npt(flavour)*)*)
(*fmax=Length[pdfflavours];(*how many f(x,Q,flavour) input*)*)
(*Nset=Length[pdfflavours[[1,1,3]] ];(*Nset*)*)
(*(*test*)Print["{Npt,fmax,Nset} = ",{Npt,fmax,Nset}];*)
(**)
(*output=FxQsameptdata;*)
(*(*set info*)*)
(*output[["label"]]=Join[{"x","Q"},Table[ToString[i],{i,1,Nset}] ];*)
(*output[["exptinfo","exptid"]]=exptid;*)
(*output[["exptinfo","exptname"]]=exptname;*)
(*output[["PDFinfo","PDFname"]]=PDFname;*)
(*output[["PDFinfo","PDFsetmethod"]]=PDFsetmethod;*)
(*output[["PDFinfo","Nset"]]=Nset;*)
(*output[["PDFinfo","flavour"]]=flavour;*)
(**)
(*output[["data"]]=*)
(*Table[*)
(*Table[*)
(*(*x and Q*)*)
(*x=pdfflavours[[f+6,ix,1]];Q=pdfflavours[[f+6,ix,2]];*)
(*LF[x,Q,Sequence@@pdfflavours[[f+6,ix,3]] ],*)
(*{ix,1,Npt}*)
(*],*)
(*{f,-5,-5+fmax-1}*)
(*];*)
(**)
(**)
(**)
(*output*)
(*]*)
(**)*)


(* ::Input::Initialization:: *)
FxQsameptdata


(* ::Input:: *)
(*(*PDFCorrelationplot[MyCorrelations,"corr g-chi","x","Q",{0.00001,1,1,120},0.2,0.2]*)*)
(*(*observablein: {{ {x,Q}, {obs1,obs2,...obs(iset)}}, { {x,Q}, {obs1,obs2,...obs(iset)}},...}*)*)
(*(*datafilein: filename used to store data*)*)
(*(*IO test, scientific in/output format not supported by mathematica, we can use NumberForm to control output precision *)*)
(*pdfwritesamept[observablein_,ifamilyin_,datafilein_]:=*)
(*Module[{observable=observablein,ifamily=ifamilyin,datafile=datafilein,xQlist,pdfflavours,x,Q,MyCorrelations,str,Nset,Nset2,xQstr,pdfisetstr,fmax,outputprecision},*)
(**)
(*(*extract (x,Q) value from observable*)*)
(*xQlist=Flatten[Take[observable,All,1],1];*)
(**)
(*(*set #iset*)*)
(*pdfSetActiveFamily[ifamily]; (* choose PDF family *)*)
(*Nset=Length[pdfSetList[[ifamily]] ] ;(* number of PDF sets *)*)
(*Nset2=Length[observable[[1,2]] ];*)
(*If[Nset!=Nset2,Print["error, info of #iset in observable and ifamily are different"]];*)
(*(*set fmax*)*)
(*fmax=11;*)
(**)
(**)
(*(*generate pdf value for all flavour, (x,Q) points*)*)
(**)
(*pdfflavours=*)
(*Table[Print["calculate pdf ",flavour];pdflist[xQlist,flavour,1],{flavour,-5,5}];(*test mode: run fast*)*)
(**)
(*(*open write*)*)
(*str=OpenWrite[datafile];*)
(**)
(*WriteString[str,"#flavour: "<>ToString[fmax]<>"  Npt: "<>ToString[Length[xQlist] ]<>" #iset: "<>ToString[Nset]<> "\n"];*)
(**)
(**)
(*Do[*)
(*WriteLine[str,"flavour= "<>ToString[f] ];*)
(*Do[*)
(*(*x and Q*)*)
(*x=xQlist[[ix,1]];Q=xQlist[[ix,2]];*)
(*xQstr=ToString[x]<>" "<>ToString[Q];*)
(*(*f(x,Q) for all iset*)*)
(*pdfisetstr="";(*ToString > StringJoin*)*)
(**)
(*outputprecision=10;*)
(*(**)
(*pdfisetstr=ToString[#]&/@NumberForm[pdfflavours[[f+6,ix,3]],outputprecision ];*)
(*pdfisetstr=ToString[NumberForm[#,outputprecision]]&/@pdfflavours[[f+6,ix,3]];*)
(**)*)
(*pdfisetstr=ToString[ToSciForm[#,outputprecision]]&/@pdfflavours[[f+6,ix,3]];*)
(*pdfisetstr=StringJoin[#," "]&/@pdfisetstr;*)
(*pdfisetstr=StringJoin[pdfisetstr];*)
(**)
(*WriteLine[str,xQstr];*)
(*WriteLine[str,pdfisetstr];*)
(**)
(*,*)
(*{ix,1,Length[xQlist]}*)
(*];,*)
(*{f,-5,5}*)
(*];*)
(**)
(**)
(*Close[str];*)
(**)
(*(**)
(*pdfflavours*)
(**)*)
(*{1,1,1}*)
(*];*)
(**)
(*pdfwritesameptnew[pdfflavoursin_,datafilein_]:=*)
(*Module[{pdfflavours=pdfflavoursin,datafile=datafilein,x,Q,str,Npt,Nset,Nset2,xQstr,pdfisetstr,fmax,outputprecision},*)
(**)
(*(*set variables*)*)
(*Npt=Length[pdfflavours[[1]] ];(*assume all points have the same points, it could be modified to Npt(flavour)*)*)
(*fmax=Length[pdfflavours];(*how many f(x,Q,flavour) input*)*)
(*Nset=Length[pdfflavours[[1,1,3]] ];(*Nset*)*)
(*(*test*)Print["{Npt,fmax,Nset} = ",{Npt,fmax,Nset}];*)
(**)
(**)
(*(*open write*)*)
(*str=OpenWrite[datafile];*)
(**)
(*WriteString[str,"#flavour: "<>ToString[fmax]<>"  Npt: "<>ToString[Npt]<>" #iset: "<>ToString[Nset]<> "\n"];*)
(**)
(**)
(*Do[*)
(*WriteLine[str,"flavour= "<>ToString[f] ];*)
(*Do[*)
(*(*x and Q*)*)
(*x=pdfflavours[[f+6,ix,1]];Q=pdfflavours[[f+6,ix,2]];*)
(*xQstr=ToString[x]<>" "<>ToString[Q];*)
(*(*f(x,Q) for all iset*)*)
(*pdfisetstr="";(*ToString > StringJoin*)*)
(**)
(*outputprecision=10;*)
(*(**)
(*pdfisetstr=ToString[#]&/@NumberForm[pdfflavours[[f+6,ix,3]],outputprecision ];*)
(*pdfisetstr=ToString[NumberForm[#,outputprecision]]&/@pdfflavours[[f+6,ix,3]];*)
(**)*)
(*pdfisetstr=ToString[ToSciForm[#,outputprecision]]&/@pdfflavours[[f+6,ix,3]];*)
(*pdfisetstr=StringJoin[#," "]&/@pdfisetstr;*)
(*pdfisetstr=StringJoin[pdfisetstr];*)
(**)
(*WriteLine[str,xQstr];*)
(*WriteLine[str,pdfisetstr];*)
(**)
(*,*)
(*{ix,1,Npt}*)
(*];,*)
(*{f,-5,-5+fmax-1}*)
(*];*)
(**)
(**)
(*Close[str];*)
(**)
(*(**)
(*pdfflavours*)
(**)*)
(*{1,1,1}*)
(*];*)
(**)
(*(*input: dataclasslist: FxQsameptdata[[flavour]]*)*)
(*(*save f(x,Q,flavour) into file*)*)
(*pdfwritesamept2new[dataclasslistin_,datafilein_]:=*)
(*Module[{dataclasslist=dataclasslistin,datafile=datafilein,pdfflavours,x,Q,str,Npt,Npt2,Nset,Nset2,xQstr,pdfisetstr,fmax,fmax2,outputprecision,exptid,exptname,PDFname,PDFsetmethod},*)
(**)
(*(*transform data format*)*)
(*Npt=Datamethods[["getNpt"]][dataclasslist[[1]] ];*)
(*fmax=Length[dataclasslist];(*how many f(x,Q,flavour) input*)*)
(*Nset=Datamethods[["getNcolumn"]][dataclasslist[[1]] ]-2;*)
(**)
(*pdfflavours=*)
(*Table[*)
(*LFtoIOformat[dataclasslist[[f+6]][["data"]] ],*)
(*{f,-5,-5+fmax-1}*)
(*];*)
(**)
(*(*set variables*)*)
(*Npt2=Length[pdfflavours[[1]] ];(*assume all points have the same points, it could be modified to Npt(flavour)*)*)
(*fmax2=Length[pdfflavours];(*how many f(x,Q,flavour) input*)*)
(*Nset2=Length[pdfflavours[[1,1,3]] ];(*Nset*)*)
(*(*test*)Print["{Npt,fmax,Nset} = ",{Npt2,fmax2,Nset2}];*)
(**)
(*(*check if diff from the info on class*)*)
(*If[Npt2!=Npt,Print["error, #Npt "] ];*)
(*If[fmax2!=fmax,Print["error, #flavour"] ];*)
(*If[Nset2!=Nset,Print["error, Nset"] ];*)
(**)
(*(*set data info*)*)
(*exptid=dataclasslist[[1]][["exptinfo","exptid"]];*)
(*exptname=dataclasslist[[1]][["exptinfo","exptname"]];*)
(*PDFname=dataclasslist[[1]][["PDFinfo","PDFname"]];*)
(*PDFsetmethod=dataclasslist[[1]][["PDFinfo","PDFsetmethod"]];*)
(**)
(*(*open write*)*)
(*str=OpenWrite[datafile];*)
(**)
(*WriteString[*)
(*str,*)
(*"#flavour: "<>ToString[fmax]<>*)
(*"  Npt: "<>ToString[Npt]<>*)
(*" #iset: "<>ToString[Nset]<> *)
(*" #exptid: "<>ToString[exptid]<>*)
(*" exptname: "<>exptname<>*)
(*" PDFname: "<>PDFname<>*)
(*" PDFsetmethod: "<>PDFsetmethod<>*)
(*"\n"*)
(*];*)
(*(**)
(*output[["exptinfo","exptid"]]=exptid;*)
(*output[["exptinfo","exptname"]]=exptname;*)
(*output[["PDFinfo","PDFname"]]=PDFname;*)
(*output[["PDFinfo","PDFsetmethod"]]=PDFsetmethod;*)
(*output[["PDFinfo","Nset"]]=Nset;*)
(*output[["PDFinfo","flavour"]]=flavour;*)
(**)*)
(**)
(**)
(*Do[*)
(*WriteLine[str,"flavour= "<>ToString[f] ];*)
(*Do[*)
(*(*x and Q*)*)
(*x=pdfflavours[[f+6,ix,1]];Q=pdfflavours[[f+6,ix,2]];*)
(*xQstr=ToString[x]<>" "<>ToString[Q];*)
(*(*f(x,Q) for all iset*)*)
(*pdfisetstr="";(*ToString > StringJoin*)*)
(**)
(*outputprecision=10;*)
(*(**)
(*pdfisetstr=ToString[#]&/@NumberForm[pdfflavours[[f+6,ix,3]],outputprecision ];*)
(*pdfisetstr=ToString[NumberForm[#,outputprecision]]&/@pdfflavours[[f+6,ix,3]];*)
(**)*)
(*pdfisetstr=ToString[ToSciForm[#,outputprecision]]&/@pdfflavours[[f+6,ix,3]];*)
(*pdfisetstr=StringJoin[#," "]&/@pdfisetstr;*)
(*pdfisetstr=StringJoin[pdfisetstr];*)
(**)
(*WriteLine[str,xQstr];*)
(*WriteLine[str,pdfisetstr];*)
(**)
(*,*)
(*{ix,1,Npt}*)
(*];,*)
(*{f,-5,-5+fmax-1}*)
(*];*)
(**)
(**)
(*Close[str];*)
(**)
(*(**)
(*pdfflavours*)
(**)*)
(*{1,1,1}*)
(*];*)


(* ::Subsection:: *)
(*read fxQdata class*)


(* ::Input:: *)
(*(*read data stored by function pdfwritesamept*)*)
(*(*read info of {fmax,Npt,Nset}, then check whether data is the same as {fmax,Npt,Nset}, if yes \[Rule] output the data by*)
(*observablein: {{ {x,Q}, {obs1,obs2,...obs(iset)}}, { {x,Q}, {obs1,obs2,...obs(iset)}},...}*)
(*where {fmax,Npt,Nset} means #flavour in data, #of points of data, #iset of data*)*)
(*(*output format:*)
(*pdfflavours[[#flavour]][[Npt]][[ x, Q, {#iset of pdf(x,Q)}]]*)
(**)*)
(*pdfreadsamept2[datafilein_]:=*)
(*Module[{datafile=datafilein,xQlist,pdfflavours,pdfisetstr,x,Q,MyCorrelations,str,fmax,fmax2,Npt,dummy,i,Nset,mystr,tmpdata,exptid,exptname,PDFname,PDFsetmethod,outputtmp,output},*)
(**)
(*(*open file*)*)
(*str=OpenRead[datafile];*)
(*(*read info {fmax,Npt,Nset} from first line of file*)*)
(*{dummy,fmax,dummy,Npt,dummy,Nset,dummy,exptid,dummy,exptname,dummy,PDFname,dummy,PDFsetmethod}=*)
(*Read[str,{Word,Number,Word,Number,Word,Number,Word,Number,Word,Word,Word,Word,Word,Word}];*)
(*Print["{#flavour, Npt, #set} = ",{fmax,Npt,Nset,exptid,exptname,PDFname,PDFsetmethod}];*)
(*(*dummy=ReadLine[str];Print[dummy];*)(*read left of the line*)*)
(**)
(*(*seperate flavour data to a list*)*)
(*tmpdata=ReadList[str,Record,RecordSeparators->"flavour="];*)
(*tmpdata=Drop[tmpdata,1];(*first element is dummy*)*)
(*fmax2=Length[tmpdata];*)
(*If[fmax!=fmax2,Print["error, head info of total #flavour inconsistent with data, fmax1: ",fmax," fmax2: ",fmax2 ];Return["error"] ];*)
(**)
(*mystr=Table[StringToStream[tmpdata[[f]] ],{f,1,fmax2}];*)
(**)
(*Close[str];*)
(**)
(*(*declare variable*)*)
(*pdfflavours={};*)
(*dummy={};*)
(*x={};*)
(*Q={};*)
(*pdfisetstr={};*)
(**)
(*(*read data from file to pdfflavours*)*)
(*Do[*)
(*(**)
(*dummy=ReadLine[mystr[[f+6]] ];*)
(**)*)
(**)
(*dummy=Read[mystr[[f+6]],Number];(*read flavour index*)*)
(*pdfflavours=Append[pdfflavours,{}];*)
(**)
(*Do[*)
(*(*x and Q*)*)
(*x=Read[mystr[[f+6]],Number];*)
(*Q=Read[mystr[[f+6]],Number];*)
(**)
(*(*Nset=57;*)*)
(*(**)
(*pdfisetstr=Read[mystr[[f+6]],Table[Number,{i,1,Nset}] ];*)
(**)*)
(*pdfisetstr=Read[mystr[[f+6]],Table[Word,{i,1,Nset}] ];(*scientific notation\[Rule] word\[Rule] number*)*)
(*pdfisetstr=SciStrToNumber[#]&/@pdfisetstr;(*word\[Rule] number array*)*)
(**)
(*(*read left part of last line*)*)
(*dummy=ReadLine[mystr[[f+6]] ];*)
(**)
(*(*set {x,Q,{#iset of pdf}} of ix *)*)
(*pdfflavours[[f+6]]=Append[pdfflavours[[f+6]],{}];*)
(*pdfflavours[[f+6,ix]]=Append[pdfflavours[[f+6,ix]],x];*)
(*pdfflavours[[f+6,ix]]=Append[pdfflavours[[f+6,ix]],Q];*)
(**)
(*pdfflavours[[f+6,ix]]=Append[pdfflavours[[f+6,ix]],pdfisetstr];*)
(**)
(*(**)
(*x=xQlist[[ix,1]];Q=xQlist[[ix,2]];*)
(*xQstr=ToString[x]<>" "<>ToString[Q];*)
(**)*)
(*(*f(x,Q) for all iset*)*)
(*(**)
(*pdfisetstr="";(*ToString > StringJoin*)*)
(**)
(*pdfisetstr=ToString[#]&/@pdfflavours[[f+6,ix,3]];*)
(*pdfisetstr=StringJoin[#," "]&/@pdfisetstr;*)
(*pdfisetstr=StringJoin[pdfisetstr];*)
(**)
(*WriteLine[str,xQstr];*)
(*WriteLine[str,pdfisetstr];*)
(**)*)
(*,*)
(*{ix,1,Npt}*)
(*];,*)
(*{f,-5,-5+fmax-1}(*test f*)*)
(*];*)
(**)
(*(*set data*)*)
(*output=*)
(*Table[*)
(*(*set fxQdataclass*)*)
(*outputtmp=FxQsameptdata;*)
(*(*set info*)*)
(*outputtmp[["label"]]=Join[{"x","Q"},Table[ToString[i],{i,1,Nset}] ];*)
(*outputtmp[["exptinfo","exptid"]]=exptid;*)
(*outputtmp[["exptinfo","exptname"]]=exptname;*)
(*outputtmp[["PDFinfo","PDFname"]]=PDFname;*)
(*outputtmp[["PDFinfo","PDFsetmethod"]]=PDFsetmethod;*)
(*outputtmp[["PDFinfo","Nset"]]=Nset;*)
(*(*output[["PDFinfo","flavour"]]=flavour;*)*)
(**)
(*(*set data*)*)
(*outputtmp[["data"]]=IOtoLFformat[pdfflavours[[f+6]] ];*)
(*(*output*)*)
(*outputtmp,*)
(*{f,-5,-5+fmax-1}*)
(*];*)
(**)
(*output*)
(*]*)


(* ::Section:: *)
(*IO of corrdata class*)


(* ::Subsection:: *)
(*write corrdata class*)


(* ::Subsection:: *)
(*read corrdata class*)


(* ::Section:: *)
(*IO of  config file*)


(* ::Input:: *)
(*(*write config file*)*)
(*(*20170514: for read data and make plots*)*)
(*makecorrconfigfile[configDirin_,configfilenamein_,figureDirin_,myPDFsetDirin_,PDFsetmethodin_,PDFnamein_,PDFDataDirin_,datalistFilein_,expttypein_,exptidin_]:=*)
(*Module[{configDir=configDirin,configfilename=configfilenamein,figureDir=figureDirin,myPDFsetDir=myPDFsetDirin,PDFsetmethod=PDFsetmethodin,PDFname=PDFnamein,PDFDataDir=PDFDataDirin,datalistFile=datalistFilein,expttype=expttypein,exptid=exptidin,figureDirtag,PDFsetDirtag,PDFsetmethodtag,PDFDataDirtag,CorrDataDirtag,PDFnametag,datalistFiletag,expttypetag,exptidtag,*)
(*figureDirtext,PDFsetDirtext,PDFsetmethodtext,PDFDataDirtext,CorrDataDirtext,PDFnametext,datalistFiletext,expttypetext,exptidtext,*)
(*s,enterstr,exptidtmp},*)
(*(*set tag of arguments*)*)
(*figureDirtag=" = figureDirtag !";*)
(**)
(*PDFsetDirtag=" = PDFsetDir !";*)
(*PDFsetmethodtag=" = PDFsetmethod !";*)
(**)
(*PDFDataDirtag=" = PDFDataDir !";*)
(*CorrDataDirtag=" = CorrDataDir !";*)
(*PDFnametag=" = PDFname !";*)
(**)
(*datalistFiletag=" = datalist !";*)
(**)
(*expttypetag=" = expttype !";*)
(*exptidtag=" = exptid !";*)
(*(*set text for explanation of arguments*)*)
(*figureDirtext=" the directory used to save figure files";*)
(**)
(*PDFsetDirtext="  PDFset directory ";*)
(*PDFsetmethodtext="  method used to generate PDFset ";*)
(**)
(*PDFDataDirtext="  directory storing f(x,Q) data ";*)
(*CorrDataDirtext="  directory storing correlation data ";*)
(*PDFnametext="  PDFname of file storing f(x,Q) value ";*)
(**)
(*datalistFiletext="  datalist with experimental information ";*)
(**)
(*expttypetext="  expt type mode: single, multi, All, ProtonNeutron are available options ";*)
(*exptidtext="  experimental ID ";*)
(**)
(*enterstr="\n";*)
(**)
(*s=OpenWrite[configDir<>configfilename];*)
(*WriteString[s,"#this file is config file of correlation_plot_project_v3.nb\n"];*)
(*WriteString[s,"#figureDir is the directory of figure files \n"];*)
(*WriteString[s,"#PDFsetDir is the directory of PDFset, PDFsetmethod = Hessian or MC \n"];*)
(*WriteString[s,"#PDFDataDir is directory store f(x,Q) data, PDFname is name of PDFset, ex: CT14NNLO \n"];*)
(*WriteString[s,"#datalistFile is the file with experimental information \n"];*)
(*WriteString[s,"#expttype & exptid are used to decide which experiments you want to show  \n"];*)
(*WriteString[s,"\n"];*)
(**)
(*WriteString[s,figureDir,figureDirtag,figureDirtext,enterstr];*)
(*WriteString[s,"\n"];*)
(**)
(*WriteString[s,myPDFsetDir,PDFsetDirtag,PDFsetDirtext,enterstr];*)
(*WriteString[s,PDFsetmethod,PDFsetmethodtag,PDFsetmethodtext,enterstr];*)
(*WriteString[s,"\n"];*)
(**)
(*WriteString[s,PDFDataDir,PDFDataDirtag,PDFDataDirtext,enterstr];*)
(*(*WriteString[s,CorrDataDir,CorrDataDirtag,CorrDataDirtext,enterstr];*)*)
(*WriteString[s,PDFname,PDFnametag,PDFnametext,enterstr];*)
(*WriteString[s,"\n"];*)
(**)
(*WriteString[s,datalistFile,datalistFiletag,datalistFiletext,enterstr];*)
(*WriteString[s,"\n"];*)
(**)
(*WriteString[s,expttype,expttypetag,expttypetext,enterstr];*)
(*WriteString[s,"\n"];*)
(*(*if single or multi type, save format = #1 #2 #3 ... = xxx! *)*)
(*exptidtmp="";(*initialize exptidtmp, exptidtmp is a string with format of expid in the config file*)*)
(*If[*)
(*expttype=="All" || expttype=="ProtonNeutron",*)
(*exptidtmp=exptid*)
(*];*)
(*If[*)
(*expttype=="single" || expttype=="multi",*)
(*exptidtmp=Table[ToString[exptid[[iexptid]] ]<>" ",{iexptid,1,Length[exptid]}];*)
(*exptidtmp=StringJoin[exptidtmp];*)
(*exptidtmp=Drop[exptidtmp,-1];(*drop the last " "*)*)
(*];*)
(*WriteString[s,exptidtmp,exptidtag,exptidtext,enterstr];*)
(*WriteString[s,"\n"];*)
(*(**)
(*If[*)
(*StringContainsQ[output[[i]],exptidtag] && (expttype\[Equal]"single" || expttype\[Equal]"multi"),*)
(*exptidtmp=ReadList[StringToStream[output[[i]] ],Record,RecordSeparators\[Rule]exptidtag];*)
(*If[Length[exptidtmp]\[Equal]2,exptid=ReadList[StringToStream[exptidtmp[[1]] ],Number] ](*structure is like exptid... exptidtag explanation, so exptidtmp = 2 *)*)
(*];*)
(**)*)
(**)
(*Close[s];*)
(**)
(*"make correlation config file done"*)
(*]*)


(* ::Input::Initialization:: *)
(*20170514: for save data*)
readsavedataconfigfile[configDirin_,configfilenamein_]:=
Module[{configDir=configDirin,configfilename=configfilenamein,
PDFsetDirTag,PDFsetmethodTag,ExptIDListTag,datalistFileTag,FxQGridDirTag,FxQGridFileTag,FxQSameptDirTag,FxQSameptFileTag,CorrDataDirTag,CorrDataFileTag,GridNxTag,GridNQTag,
PDFsetDir,PDFsetmethod,ExptIDList,datalistFile,FxQGridDir,FxQGridFile,FxQSameptDir,FxQSameptFile,CorrDataDir,CorrDataFile,GridNx,GridNQ,
itag,s,output,output2,output3},

PDFsetDirTag="PDF set Dir";
PDFsetmethodTag="PDF method";
ExptIDListTag="Expt ID List";
datalistFileTag="datalis file";
FxQGridDirTag="F(x,Q) Grid Path";
FxQGridFileTag="F(x,Q) Grid File";
FxQSameptDirTag="F(x,Q) Samept Path";
FxQSameptFileTag="F(x,Q) Samept File";
CorrDataDirTag="Correlation Path";
CorrDataFileTag="Correlation File";

GridNxTag="Nx";
GridNQTag="NQ";
{PDFsetDirTag,PDFsetmethodTag,ExptIDListTag,datalistFileTag,FxQGridDirTag,FxQGridFileTag,FxQSameptDirTag,FxQSameptFileTag,CorrDataDirTag,CorrDataFileTag,GridNxTag,GridNQTag};

PDFsetDir="unset";
PDFsetmethod="unset";
ExptIDList="unset";
datalistFile="unset";
FxQGridDir="unset";
FxQGridFile="unset";
FxQSameptDir="unset";
FxQSameptFile="unset";
CorrDataDir="unset";
CorrDataFile="unset";

GridNx="unset";
GridNQ="unset";
{PDFsetDir,PDFsetmethod,ExptIDList,datalistFile,FxQGridDir,FxQGridFile,FxQSameptDir,FxQSameptFile,CorrDataDir,CorrDataFile,GridNx,GridNQ};


(*read config file line by line into list*)
s=OpenRead[configDir<>configfilename];
output=ReadList[s,String];
Close[s];

(*delete comments: with "#" at begining of line*)
output2={};
Table[
If[StringTake[output[[i]],1]!="#",output2=Append[output2,output[[i]] ] ];
"dummy"
,{i,1,Length[output]}
];
(*seperate the tag of configure file and arguments by ":"*)
output3=Table[StringSplit[output2[[i]],":"],{i,1,Length[output2]}];

{PDFsetDirTag,PDFsetmethodTag,ExptIDListTag,datalistFileTag,FxQGridDirTag,FxQSameptDirTag,CorrDataDirTag};
(*check tag exist, if a tag exist, read arguments corresponding to that tag*)
(*read PDFset Dir*)
itag=1;
If[output3[[itag,1]]==PDFsetDirTag,PDFsetDir=output3[[itag,2]];PDFsetDir=Read[StringToStream[PDFsetDir],Word] ];

(*read PDF method *)
itag=itag+1;
If[output3[[itag,1]]==PDFsetmethodTag,PDFsetmethod=output3[[itag,2]];PDFsetmethod=Read[StringToStream[PDFsetmethod],Word] ];

(*read ExptID List *)
itag=itag+1;
If[output3[[itag,1]]==ExptIDListTag,ExptIDList=output3[[itag,2]];ExptIDList=ReadList[StringToStream[ExptIDList],Number] ];

(*read datalist File *)
itag=itag+1;
If[output3[[itag,1]]==datalistFileTag,datalistFile=output3[[itag,2]];datalistFile=Read[StringToStream[datalistFile],Word] ];

(*read FxQGrid Dir *)
itag=itag+1;
If[output3[[itag,1]]==FxQGridDirTag,FxQGridDir=output3[[itag,2]];FxQGridDir=Read[StringToStream[FxQGridDir],Word] ];

(*read FxQGrid File *)
itag=itag+1;
If[output3[[itag,1]]==FxQGridFileTag,FxQGridFile=output3[[itag,2]];FxQGridFile=Read[StringToStream[FxQGridFile],Word] ];

(*read FxQSamept Dir *)
itag=itag+1;
If[output3[[itag,1]]==FxQSameptDirTag,FxQSameptDir=output3[[itag,2]];FxQSameptDir=Read[StringToStream[FxQSameptDir],Word] ];

(*read FxQSamept File *)
itag=itag+1;
If[output3[[itag,1]]==FxQSameptFileTag,FxQSameptFile=output3[[itag,2]];FxQSameptFile=Read[StringToStream[FxQSameptFile],Word] ];

(*read Correlation Data Dir *)
itag=itag+1;
If[output3[[itag,1]]==CorrDataDirTag,CorrDataDir=output3[[itag,2]];CorrDataDir=Read[StringToStream[CorrDataDir],Word] ];

(*read Correlation Data File *)
itag=itag+1;
If[output3[[itag,1]]==CorrDataFileTag,CorrDataFile=output3[[itag,2]];CorrDataFile=Read[StringToStream[CorrDataFile],Word] ];

(*read Grid Nx *)
itag=itag+1;
If[output3[[itag,1]]==GridNxTag,GridNx=output3[[itag,2]];GridNx=Read[StringToStream[GridNx],Number] ];

(*read Grid NQ *)
itag=itag+1;
If[output3[[itag,1]]==GridNQTag,GridNQ=output3[[itag,2]];GridNQ=Read[StringToStream[GridNQ],Number] ];


{PDFsetDir,PDFsetmethod,ExptIDList,datalistFile,FxQGridDir,FxQGridFile,FxQSameptDir,FxQSameptFile,CorrDataDir,CorrDataFile,GridNx,GridNQ}
]


(* ::Input::Initialization:: *)
(*20170514: for save data*)
readplotdataconfigfile[configDirin_,configfilenamein_]:=
Module[{configDir=configDirin,configfilename=configfilenamein,
PDFsetDirTag,PDFsetmethodTag,ExptIDListTag,datalistFileTag,FxQGridDirTag,FxQGridFileTag,FxQSameptDirTag,FxQSameptFileTag,CorrDataDirTag,CorrDataFileTag,GridNxTag,GridNQTag,
PDFsetDir,PDFsetmethod,ExptIDList,datalistFile,FxQGridDir,FxQGridFile,FxQSameptDir,FxQSameptFile,CorrDataDir,CorrDataFile,GridNx,GridNQ,
itag,s,output,output2,output3},


datalistFileTag="datalis file";
FxQGridDirTag="F(x,Q) Grid Path";
FxQGridFileTag="F(x,Q) Grid File";
FxQSameptDirTag="F(x,Q) Samept Path";
FxQSameptFileTag="F(x,Q) Samept File";
CorrDataDirTag="Correlation Path";
CorrDataFileTag="Correlation File";

GridNxTag="Nx";
GridNQTag="NQ";
{datalistFileTag,FxQGridDirTag,FxQGridFileTag,FxQSameptDirTag,FxQSameptFileTag,CorrDataDirTag,CorrDataFileTag,GridNxTag,GridNQTag};


datalistFile="unset";
FxQGridDir="unset";
FxQGridFile="unset";
FxQSameptDir="unset";
FxQSameptFile="unset";
CorrDataDir="unset";
CorrDataFile="unset";

GridNx="unset";
GridNQ="unset";
{datalistFile,FxQGridDir,FxQGridFile,FxQSameptDir,FxQSameptFile,CorrDataDir,CorrDataFile,GridNx,GridNQ};


(*read config file line by line into list*)
s=OpenRead[configDir<>configfilename];
output=ReadList[s,String];
Close[s];

(*delete comments: with "#" at begining of line*)
output2={};
Table[
If[StringTake[output[[i]],1]!="#",output2=Append[output2,output[[i]] ] ];
"dummy"
,{i,1,Length[output]}
];
(*seperate the tag of configure file and arguments by ":"*)
output3=Table[StringSplit[output2[[i]],":"],{i,1,Length[output2]}];

{datalistFileTag,FxQGridDirTag,FxQSameptDirTag,CorrDataDirTag};
(*check tag exist, if a tag exist, read arguments corresponding to that tag*)

(*read datalist File *)
itag=1;
If[output3[[itag,1]]==datalistFileTag,datalistFile=output3[[itag,2]];datalistFile=Read[StringToStream[datalistFile],Word] ];

(*read FxQGrid Dir *)
itag=itag+1;
If[output3[[itag,1]]==FxQGridDirTag,FxQGridDir=output3[[itag,2]];FxQGridDir=Read[StringToStream[FxQGridDir],Word] ];

(*read FxQGrid File *)
itag=itag+1;
If[output3[[itag,1]]==FxQGridFileTag,FxQGridFile=output3[[itag,2]];FxQGridFile=Read[StringToStream[FxQGridFile],Word] ];

(*read FxQSamept Dir *)
itag=itag+1;
If[output3[[itag,1]]==FxQSameptDirTag,FxQSameptDir=output3[[itag,2]];FxQSameptDir=Read[StringToStream[FxQSameptDir],Word] ];

(*read FxQSamept File *)
itag=itag+1;
If[output3[[itag,1]]==FxQSameptFileTag,FxQSameptFile=output3[[itag,2]];FxQSameptFile=Read[StringToStream[FxQSameptFile],Word] ];

(*read Correlation Data Dir *)
itag=itag+1;
If[output3[[itag,1]]==CorrDataDirTag,CorrDataDir=output3[[itag,2]];CorrDataDir=Read[StringToStream[CorrDataDir],Word] ];

(*read Correlation Data File *)
itag=itag+1;
If[output3[[itag,1]]==CorrDataFileTag,CorrDataFile=output3[[itag,2]];CorrDataFile=Read[StringToStream[CorrDataFile],Word] ];

(*read Grid Nx *)
itag=itag+1;
If[output3[[itag,1]]==GridNxTag,GridNx=output3[[itag,2]];GridNx=Read[StringToStream[GridNx],Number] ];

(*read Grid NQ *)
itag=itag+1;
If[output3[[itag,1]]==GridNQTag,GridNQ=output3[[itag,2]];GridNQ=Read[StringToStream[GridNQ],Number] ];


{datalistFile,FxQGridDir,FxQGridFile,FxQSameptDir,FxQSameptFile,CorrDataDir,CorrDataFile,GridNx,GridNQ}
]


(*
makecorrconfigfile[configDirin_,configfilenamein_,myPDFsetDirin_,PDFsetmethodin_,PDFnamein_,PDFDataDirin_,datalistFilein_]:=
Module[{configDir=configDirin,configfilename=configfilenamein,myPDFsetDir=myPDFsetDirin,PDFsetmethod=PDFsetmethodin,PDFname=PDFnamein,PDFDataDir=PDFDataDirin,datalistFile=datalistFilein,PDFsetDirtag,PDFsetmethodtag,PDFDataDirtag,CorrDataDirtag,PDFnametag,datalistFiletag,s.enterstr},

PDFsetDirtag=" = PDFset directory !";
PDFsetmethodtag=" = method used to generate PDFset !";

PDFDataDirtag=" = directory storing f(x,Q) data !";
CorrDataDirtag=" = directory storing correlation data !";
PDFnametag=" = PDFname of file storing f(x,Q) value !";

datalistFiletag=" = datalist with experimental information !";

s=OpenWrite[configDir<>configfilename];
WriteString[s,"#this file is config file of correlation_plot_project_v3.nb\n"];
WriteString[s,"#PDFsetDir is the directory of PDFset, PDFsetmethod = Hessian or MC \n"];
WriteString[s,"#PDFDataDir is directory store f(x,Q) data, PDFname is name of PDFset, ex: CT14NNLO \n"];
WriteString[s,"#datalistFile is the file with experimental information \n"];
WriteString[s,"\n"];

enterstr="\n";
WriteString[s,myPDFsetDir,PDFsetDirtag,enterstr];
WriteString[s,PDFsetmethod,PDFsetmethodtag,enterstr];
WriteString[s,"\n"];

WriteString[s,PDFDataDir,PDFDataDirtag,enterstr];
WriteString[s,CorrDataDir,CorrDataDirtag,enterstr];
WriteString[s,PDFname,PDFnametag,enterstr];
WriteString[s,"\n"];

WriteString[s,datalistFile,datalistFiletag,enterstr];

Close[s];

"make correlation config file done"
]
*)



(* ::Input:: *)
(*(*read config file*)*)
(*(*script version, add runfunc mode into configure file, so that user can use commands to decide which function he want to run*)*)
(*readcorrconfigfile[configDirin_,configfilenamein_]:=*)
(*Module[{configDir=configDirin,configfilename=configfilenamein,*)
(*runfunc,figureDir,myPDFsetDir,PDFsetmethod,PDFname,PDFDataDir,CorrDataDir,datalistFile,expttype,exptid,*)
(*runfunctag,figureDirtag,PDFsetDirtag,PDFsetmethodtag,PDFDataDirtag,CorrDataDirtag,PDFnametag,datalistFiletag,expttypetag,exptidtag,*)
(*runfunctext,figureDirtext,PDFsetDirtext,PDFsetmethodtext,PDFDataDirtext,CorrDataDirtext,PDFnametext,datalistFiletext,expttypetext,exptidtext,*)
(*s,output,exptidtmp},*)
(*(*set tag of arguments*)*)
(*runfunctag=" = runfunc !";*)
(*figureDirtag=" = figureDirtag !";*)
(**)
(*PDFsetDirtag=" = PDFsetDir !";*)
(*PDFsetmethodtag=" = PDFsetmethod !";*)
(**)
(*PDFDataDirtag=" = PDFDataDir !";*)
(*CorrDataDirtag=" = CorrDataDir !";*)
(*PDFnametag=" = PDFname !";*)
(**)
(*datalistFiletag=" = datalist !";*)
(**)
(*expttypetag=" = expttype !";*)
(*exptidtag=" = exptid !";*)
(**)
(*(*set text for explanation of arguments*)*)
(*runfunctext=" the action you want to do, what you can do: save_fx, save_corr_plots";*)
(*figureDirtext=" the directory used to save figure files";*)
(**)
(*PDFsetDirtext="  PDFset directory ";*)
(*PDFsetmethodtext="  method used to generate PDFset ";*)
(**)
(*PDFDataDirtext="  directory storing f(x,Q) data ";*)
(*CorrDataDirtext="  directory storing correlation data ";*)
(*PDFnametext="  PDFname of file storing f(x,Q) value ";*)
(**)
(*datalistFiletext="  datalist with experimental information ";*)
(**)
(*expttypetext="  expt type mode: single, multi, All, ProtonNeutron are available options ";*)
(*exptidtext="  experimental ID ";*)
(**)
(*(*tag of arguments*)*)
(*(**)
(*PDFsetDirtag=" = PDFset directory !";*)
(*PDFsetmethodtag=" = method used to generate PDFset !";*)
(**)
(*PDFDataDirtag=" = directory storing f(x,Q) data !";*)
(*CorrDataDirtag=" = directory storing correlation data !";*)
(*PDFnametag=" = PDFname of file storing f(x,Q) value !";*)
(**)
(*datalistFiletag=" = datalist with experimental information !";*)
(**)*)
(*(*initialize arguments*)*)
(*runfunc="unset";*)
(*figureDir="unset";*)
(*myPDFsetDir="unset";*)
(*PDFsetmethod="unset";*)
(*PDFname="unset";*)
(*PDFDataDir="unset";*)
(*datalistFile="unset";*)
(*expttype="unset";*)
(*exptid="unset";*)
(**)
(*(*read file*)*)
(*s=OpenRead[configDir<>configfilename];*)
(*output=ReadList[s,String];*)
(*Close[s];*)
(**)
(*(*if read tag, assign the first word of that line to arguments*)*)
(*(*20170228: for mathematica 10.2, it sseems stringcontainsQ does not work*)*)
(**)
(*Table[*)
(**)
(*If[*)
(*(**)
(*StringContainsQ[output[[i]],runfunctag]*)
(**)StringMatchQ[output[[i]],"*"<>runfunctag<>"*"],*)
(*runfunc=Read[StringToStream[output[[i]] ],Word]*)
(*];*)
(*If[*)
(*(**)
(*StringContainsQ[output[[i]],figureDirtag]*)
(**)StringMatchQ[output[[i]],"*"<>figureDirtag<>"*"],*)
(*figureDir=Read[StringToStream[output[[i]] ],Word]*)
(*];*)
(*If[*)
(*(**)
(*StringContainsQ[output[[i]],PDFsetDirtag]*)
(**)StringMatchQ[output[[i]],"*"<>PDFsetDirtag<>"*"],*)
(*myPDFsetDir=Read[StringToStream[output[[i]] ],Word]*)
(*];*)
(*If[*)
(*(**)
(*StringContainsQ[output[[i]],PDFsetmethodtag]*)
(**)StringMatchQ[output[[i]],"*"<>PDFsetmethodtag<>"*"],*)
(*PDFsetmethod=Read[StringToStream[output[[i]] ],Word]*)
(*];*)
(*If[*)
(*(**)
(*StringContainsQ[output[[i]],PDFDataDirtag]*)
(**)StringMatchQ[output[[i]],"*"<>PDFDataDirtag<>"*"],*)
(*PDFDataDir=Read[StringToStream[output[[i]] ],Word]*)
(*];*)
(*If[*)
(*(**)
(*StringContainsQ[output[[i]],CorrDataDirtag]*)
(**)StringMatchQ[output[[i]],"*"<>CorrDataDirtag<>"*"],*)
(*CorrDataDir=Read[StringToStream[output[[i]] ],Word]*)
(*];*)
(*If[*)
(*(**)
(*StringContainsQ[output[[i]],PDFnametag]*)
(**)StringMatchQ[output[[i]],"*"<>PDFnametag<>"*"],*)
(*PDFname=Read[StringToStream[output[[i]] ],Word]*)
(*];*)
(*If[*)
(*(**)
(*StringContainsQ[output[[i]],datalistFiletag]*)
(**)StringMatchQ[output[[i]],"*"<>datalistFiletag<>"*"],*)
(*datalistFile=Read[StringToStream[output[[i]] ],Word]*)
(*];*)
(*If[*)
(*(**)
(*StringContainsQ[output[[i]],expttypetag]*)
(**)StringMatchQ[output[[i]],"*"<>expttypetag<>"*"],*)
(*expttype=Read[StringToStream[output[[i]] ],Word]*)
(*];*)
(*(*read exptid, assuming there are several exptids*)*)
(*(*take string in front of exptidtag, then read all numbers from string*)*)
(*If[*)
(*(*StringContainsQ[output[[i]],exptidtag]*)StringMatchQ[output[[i]],"*"<>exptidtag<>"*"] && (expttype=="single" || expttype=="multi"),*)
(*exptidtmp=ReadList[StringToStream[output[[i]] ],Record,RecordSeparators->exptidtag];*)
(*If[Length[exptidtmp]==2,exptid=ReadList[StringToStream[exptidtmp[[1]] ],Number] ](*structure is like exptid... exptidtag explanation, so exptidtmp = 2 *)*)
(*];*)
(**)
(*(*don't need to set exptid if expttype = All,ProtonNeutron*)*)
(*"dummy"*)
(*,{i,1,Length[output]}*)
(*];*)
(**)
(**)
(*(*if any arguments still unset, return error message*)*)
(*(*output the config setting*)*)
(*{runfunc,figureDir,myPDFsetDir,PDFsetmethod,PDFname,PDFDataDir,datalistFile,expttype,exptid}*)
(*]*)


(* ::Input::Initialization:: *)
readcorrconfigfile4[configDirin_,configfilenamein_]:=
Module[{configDir=configDirin,configfilename=configfilenamein,
JobidTag,PDFnameTag,FigureTypeTag,FigureFlagTag,ExptidTypeTag,ExptidFlagTag,CorrelationArgTypeTag,CorrelationArgFlagTag,UserArgNameTag,UserArgValueTag,
XQfigureXrangeTag,XQfigureYrangeTag,Hist1figureNbinTag,Hist1figureXrangeTag,Hist1figureYrangeTag,ColorSeperatorTag,
SizeTag,HighlightTypeTag,HighlightModeTag,HighlightMode1Tag,HighlightMode2Tag,
Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2,
itag,s,output,output2,output3
},

JobidTag="Job ID (copy from the counter file)";
PDFnameTag="PDF set";
FigureTypeTag="Type";
FigureFlagTag="Flag";
ExptidTypeTag="Expt. ID";
ExptidFlagTag="Expt. Flag";
CorrelationArgTypeTag="Type";
CorrelationArgFlagTag="Flag";
UserArgNameTag="Name";
UserArgValueTag="Values";
XQfigureXrangeTag="xmin,   xmax";
XQfigureYrangeTag="mumin, mumax";
Hist1figureNbinTag="Number of bins";
Hist1figureXrangeTag="xmin, xmax";
Hist1figureYrangeTag="ymin, ymax";
(*
Hist2figureXrangeTag
Hist2figureYrangeTag
*)
ColorSeperatorTag="Color by data percentage";
SizeTag="Size";
HighlightTypeTag="Type";
HighlightModeTag="Mode";
HighlightMode1Tag="Mode 1 range";
HighlightMode2Tag="Mode 2 range";

Jobid="unset";
PDFname="unset";
FigureType="unset";
FigureFlag="unset";
ExptidType="unset";
ExptidFlag="unset";
CorrelationArgType="unset";
CorrelationArgFlag="unset";
UserArgName="unset";
UserArgValue="unset";
XQfigureXrange="unset";
XQfigureYrange="unset";
Hist1figureNbin="unset";
Hist1figureXrange="unset";
Hist1figureYrange="unset";

ColorSeperator="unset";
Size="unset";
HighlightType="unset";
HighlightMode="unset";
HighlightMode1="unset";
HighlightMode2="unset";

(*read config file line by line into list*)
s=OpenRead[configDir<>configfilename];
output=ReadList[s,String];
Close[s];

(*delete comments: with "#" at begining of line*)
output2={};
Table[
If[StringTake[output[[i]],1]!="#",output2=Append[output2,output[[i]] ] ];
"dummy"
,{i,1,Length[output]}
];
(*seperate the tag of configure file and arguments by ":"*)
output3=Table[StringSplit[output2[[i]],":"],{i,1,Length[output2]}];

(*check tag exist, if a tag exist, read arguments corresponding to that tag*)
(*read Job id*)
itag=1;
If[output3[[itag,1]]==JobidTag,Jobid=output3[[itag,2]];Jobid=Read[StringToStream[Jobid],Number] ];
Head[Jobid];
(*read PDFname*)
itag=itag+1;
If[output3[[itag,1]]==PDFnameTag,PDFname=output3[[itag,2]];PDFname=Read[StringToStream[PDFname],Word] ];
Head[PDFname];
(*read FigureType and FigureFlag*)
itag=itag+1;
If[output3[[itag,1]]==FigureTypeTag,FigureType=output3[[itag,2]] ];
Head[FigureType];

itag=itag+1;
If[output3[[itag,1]]==FigureFlagTag,FigureFlag=output3[[itag,2]] ];
Head[FigureFlag];

FigureType=ReadList[StringToStream[FigureType],Word];
FigureFlag=ReadList[StringToStream[FigureFlag],Number];

(*read ExptidType and ExptidFlag*)
itag=itag+1;
If[output3[[itag,1]]==ExptidTypeTag,ExptidType=output3[[itag,2]] ];
Head[ExptidType];

itag=itag+1;
If[output3[[itag,1]]==ExptidFlagTag,ExptidFlag=output3[[itag,2]] ];
Head[ExptidFlag];

ExptidType=ReadList[StringToStream[ExptidType],Number];
ExptidFlag=ReadList[StringToStream[ExptidFlag],Number];

(*read CorrelationArgType and CorrelationArgFlag*)
itag=itag+1;
If[output3[[itag,1]]==CorrelationArgTypeTag,CorrelationArgType=output3[[itag,2]] ];
Head[CorrelationArgType];

itag=itag+1;
If[output3[[itag,1]]==CorrelationArgFlagTag,CorrelationArgFlag=output3[[itag,2]] ];
Head[CorrelationArgFlag];

CorrelationArgType=ReadList[StringToStream[CorrelationArgType],Word];
CorrelationArgFlag=ReadList[StringToStream[CorrelationArgFlag],Number];

(*read UserArgName*)
itag=itag+1;
If[output3[[itag,1]]==UserArgNameTag,UserArgName=output3[[itag,2]] ];
Head[UserArgName];
(*read UserArgValue*)
itag=itag+1;
If[output3[[itag,1]]==UserArgValueTag,UserArgValue=output3[[itag,2]] ];
Head[UserArgValue];

UserArgValue=ReadList[StringToStream[UserArgValue],Number];
(*read XQfigureXrange and XQfigureYrange*)
itag=itag+1;
If[output3[[itag,1]]==XQfigureXrangeTag,XQfigureXrange=output3[[itag,2]] ];
Head[XQfigureXrange];

itag=itag+1;
If[output3[[itag,1]]==XQfigureYrangeTag,XQfigureYrange=output3[[itag,2]] ];
Head[XQfigureYrange];

XQfigureXrange=ReadList[StringToStream[XQfigureXrange],Word];
XQfigureYrange=ReadList[StringToStream[XQfigureYrange],Word];
If[XQfigureXrange[[1]]!="auto",XQfigureXrange[[1]]=Read[StringToStream[XQfigureXrange[[1]] ],Number] ];
If[XQfigureXrange[[2]]!="auto",XQfigureXrange[[2]]=Read[StringToStream[XQfigureXrange[[2]] ],Number] ];
If[XQfigureYrange[[1]]!="auto",XQfigureYrange[[1]]=Read[StringToStream[XQfigureYrange[[1]] ],Number] ];
If[XQfigureYrange[[2]]!="auto",XQfigureYrange[[2]]=Read[StringToStream[XQfigureYrange[[2]] ],Number] ];
(*read Hist1figureNbin, Hist1figureXrange, Hist1figureYrange*)
itag=itag+1;
If[output3[[itag,1]]==Hist1figureNbinTag,Hist1figureNbin=output3[[itag,2]];Hist1figureNbin=Read[StringToStream[Hist1figureNbin],Word] ];
Head[Hist1figureNbin];
If[Hist1figureNbin!="auto",Hist1figureNbin=Read[StringToStream[Hist1figureNbin],Number] ];

itag=itag+1;
If[output3[[itag,1]]==Hist1figureXrangeTag,Hist1figureXrange=output3[[itag,2]] ];
Head[Hist1figureXrange];

itag=itag+1;
If[output3[[itag,1]]==Hist1figureYrangeTag,Hist1figureYrange=output3[[itag,2]] ];
Head[Hist1figureYrange];

Hist1figureXrange=ReadList[StringToStream[Hist1figureXrange],Word];
Hist1figureYrange=ReadList[StringToStream[Hist1figureYrange],Word];
If[Hist1figureXrange[[1]]!="auto",Hist1figureXrange[[1]]=Read[StringToStream[Hist1figureXrange[[1]] ],Number] ];
If[Hist1figureXrange[[2]]!="auto",Hist1figureXrange[[2]]=Read[StringToStream[Hist1figureXrange[[2]] ],Number] ];
If[Hist1figureYrange[[1]]!="auto",Hist1figureYrange[[1]]=Read[StringToStream[Hist1figureYrange[[1]] ],Number] ];
If[Hist1figureYrange[[2]]!="auto",Hist1figureYrange[[2]]=Read[StringToStream[Hist1figureYrange[[2]] ],Number] ];

(*20170306: add color seperator*)
itag=itag+1;
If[output3[[itag,1]]==ColorSeperatorTag,ColorSeperator=output3[[itag,2]] ];
Head[ColorSeperator];

ColorSeperator=ReadList[StringToStream[ColorSeperator],Word];
If[ColorSeperator[[1]]!="auto",
Table[ColorSeperator[[i]]=Read[StringToStream[ColorSeperator[[i]] ],Number];"dummy",{i,1,Length[ColorSeperator]}] 
];

(*read HighlightType(FigureType) and HighlightMode*)
itag=itag+1;
If[output3[[itag,1]]==HighlightTypeTag,HighlightType=output3[[itag,2]] ];
Head[HighlightType];

itag=itag+1;
If[output3[[itag,1]]==HighlightModeTag,HighlightMode=output3[[itag,2]] ];
Head[HighlightMode];
(*Print[output3[[itag,1]],"   ",HighlightModeTag];*)

itag=itag+1;
If[output3[[itag,1]]==HighlightMode1Tag,HighlightMode1=output3[[itag,2]] ];
Head[HighlightMode1];
(*Print[output3[[itag,1]],"   ",HighlightMode1Tag];*)

itag=itag+1;
If[output3[[itag,1]]==HighlightMode2Tag,HighlightMode2=output3[[itag,2]] ];
Head[HighlightMode2];
(*Print[output3[[itag,1]],"   ",HighlightMode2Tag];*)

HighlightType=ReadList[StringToStream[HighlightType],Word];
HighlightMode=ReadList[StringToStream[HighlightMode],Number];
HighlightMode1=ReadList[StringToStream[HighlightMode1],Number];
HighlightMode2=ReadList[StringToStream[HighlightMode2],Number];

(*20170307*)
(*read Size*)
(*20170315: size replace to the next of highlight mode*)
itag=itag+1;
If[output3[[itag,1]]==SizeTag,Size=output3[[itag,2]];Size=Read[StringToStream[Size],Word] ];
Head[Size];
(*Print[output3];*)


"dummy";

{Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,
ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}
]


(* ::Input::Initialization:: *)
(*20171109: version 5: delete the user define function value part, ReadUserFunction will be in charge of it*)
readcorrconfigfile5[configDirin_,configfilenamein_]:=
Module[{configDir=configDirin,configfilename=configfilenamein,
JobidTag,PDFnameTag,FigureTypeTag,FigureFlagTag,ExptidTypeTag,ExptidFlagTag,CorrelationArgTypeTag,CorrelationArgFlagTag,UserArgNameTag,UserArgValueTag,
XQfigureXrangeTag,XQfigureYrangeTag,Hist1figureNbinTag,Hist1figureXrangeTag,Hist1figureYrangeTag,ColorSeperatorTag,
SizeTag,HighlightTypeTag,HighlightModeTag,HighlightMode1Tag,HighlightMode2Tag,
Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2,
itag,s,output,output2,output3
},

JobidTag="Job ID (copy from the counter file)";
PDFnameTag="PDF set";
FigureTypeTag="Type";
FigureFlagTag="Flag";
ExptidTypeTag="Expt. ID";
ExptidFlagTag="Expt. Flag";
CorrelationArgTypeTag="Type";
CorrelationArgFlagTag="Flag";
(*20171109: delete the user define function value part, ReadUserFunction will be in charge of it*)
(*
UserArgNameTag="Name";
UserArgValueTag="Values";
*)
XQfigureXrangeTag="xmin,   xmax";
XQfigureYrangeTag="mumin, mumax";
Hist1figureNbinTag="Number of bins";
Hist1figureXrangeTag="xmin, xmax";
Hist1figureYrangeTag="ymin, ymax";
(*
Hist2figureXrangeTag
Hist2figureYrangeTag
*)
ColorSeperatorTag="Color by data percentage";
SizeTag="Size";
HighlightTypeTag="Type";
HighlightModeTag="Mode";
HighlightMode1Tag="Mode 1 range";
HighlightMode2Tag="Mode 2 range";

Jobid="unset";
PDFname="unset";
FigureType="unset";
FigureFlag="unset";
ExptidType="unset";
ExptidFlag="unset";
CorrelationArgType="unset";
CorrelationArgFlag="unset";
(*20171109: delete the user define function value part, ReadUserFunction will be in charge of it*)
(*
UserArgName="unset";
UserArgValue="unset";
*)
XQfigureXrange="unset";
XQfigureYrange="unset";
Hist1figureNbin="unset";
Hist1figureXrange="unset";
Hist1figureYrange="unset";

ColorSeperator="unset";
Size="unset";
HighlightType="unset";
HighlightMode="unset";
HighlightMode1="unset";
HighlightMode2="unset";

(*read config file line by line into list*)
s=OpenRead[configDir<>configfilename];
output=ReadList[s,String];
Close[s];

(*delete comments: with "#" at begining of line*)
output2={};
Table[
If[StringTake[output[[i]],1]!="#",output2=Append[output2,output[[i]] ] ];
"dummy"
,{i,1,Length[output]}
];
(*seperate the tag of configure file and arguments by ":"*)
output3=Table[StringSplit[output2[[i]],":"],{i,1,Length[output2]}];

(*check tag exist, if a tag exist, read arguments corresponding to that tag*)
(*read Job id*)
itag=1;
If[output3[[itag,1]]==JobidTag,Jobid=output3[[itag,2]];Jobid=Read[StringToStream[Jobid],Number] ];
Head[Jobid];
(*read PDFname*)
itag=itag+1;
If[output3[[itag,1]]==PDFnameTag,PDFname=output3[[itag,2]];PDFname=Read[StringToStream[PDFname],Word] ];
Head[PDFname];
(*read FigureType and FigureFlag*)
itag=itag+1;
If[output3[[itag,1]]==FigureTypeTag,FigureType=output3[[itag,2]] ];
Head[FigureType];

itag=itag+1;
If[output3[[itag,1]]==FigureFlagTag,FigureFlag=output3[[itag,2]] ];
Head[FigureFlag];

FigureType=ReadList[StringToStream[FigureType],Word];
FigureFlag=ReadList[StringToStream[FigureFlag],Number];

(*read ExptidType and ExptidFlag*)
itag=itag+1;
If[output3[[itag,1]]==ExptidTypeTag,ExptidType=output3[[itag,2]] ];
Head[ExptidType];

itag=itag+1;
If[output3[[itag,1]]==ExptidFlagTag,ExptidFlag=output3[[itag,2]] ];
Head[ExptidFlag];

ExptidType=ReadList[StringToStream[ExptidType],Number];
ExptidFlag=ReadList[StringToStream[ExptidFlag],Number];

(*read CorrelationArgType and CorrelationArgFlag*)
itag=itag+1;
If[output3[[itag,1]]==CorrelationArgTypeTag,CorrelationArgType=output3[[itag,2]] ];
Head[CorrelationArgType];

itag=itag+1;
If[output3[[itag,1]]==CorrelationArgFlagTag,CorrelationArgFlag=output3[[itag,2]] ];
Head[CorrelationArgFlag];

CorrelationArgType=ReadList[StringToStream[CorrelationArgType],Word];
CorrelationArgFlag=ReadList[StringToStream[CorrelationArgFlag],Number];

(*20171109: delete the user define function value part, ReadUserFunction will be in charge of it*)
(*
(*read UserArgName*)
itag=itag+1;
If[output3[[itag,1]]==UserArgNameTag,UserArgName=output3[[itag,2]] ];
Head[UserArgName];
(*read UserArgValue*)
itag=itag+1;
If[output3[[itag,1]]==UserArgValueTag,UserArgValue=output3[[itag,2]] ];
Head[UserArgValue];

UserArgValue=ReadList[StringToStream[UserArgValue],Number];
*)
(*read XQfigureXrange and XQfigureYrange*)
itag=itag+1;
If[output3[[itag,1]]==XQfigureXrangeTag,XQfigureXrange=output3[[itag,2]] ];
Head[XQfigureXrange];

itag=itag+1;
If[output3[[itag,1]]==XQfigureYrangeTag,XQfigureYrange=output3[[itag,2]] ];
Head[XQfigureYrange];

XQfigureXrange=ReadList[StringToStream[XQfigureXrange],Word];
XQfigureYrange=ReadList[StringToStream[XQfigureYrange],Word];
If[XQfigureXrange[[1]]!="auto",XQfigureXrange[[1]]=Read[StringToStream[XQfigureXrange[[1]] ],Number] ];
If[XQfigureXrange[[2]]!="auto",XQfigureXrange[[2]]=Read[StringToStream[XQfigureXrange[[2]] ],Number] ];
If[XQfigureYrange[[1]]!="auto",XQfigureYrange[[1]]=Read[StringToStream[XQfigureYrange[[1]] ],Number] ];
If[XQfigureYrange[[2]]!="auto",XQfigureYrange[[2]]=Read[StringToStream[XQfigureYrange[[2]] ],Number] ];
(*read Hist1figureNbin, Hist1figureXrange, Hist1figureYrange*)
itag=itag+1;
If[output3[[itag,1]]==Hist1figureNbinTag,Hist1figureNbin=output3[[itag,2]];Hist1figureNbin=Read[StringToStream[Hist1figureNbin],Word] ];
Head[Hist1figureNbin];
If[Hist1figureNbin!="auto",Hist1figureNbin=Read[StringToStream[Hist1figureNbin],Number] ];

itag=itag+1;
If[output3[[itag,1]]==Hist1figureXrangeTag,Hist1figureXrange=output3[[itag,2]] ];
Head[Hist1figureXrange];

itag=itag+1;
If[output3[[itag,1]]==Hist1figureYrangeTag,Hist1figureYrange=output3[[itag,2]] ];
Head[Hist1figureYrange];

Hist1figureXrange=ReadList[StringToStream[Hist1figureXrange],Word];
Hist1figureYrange=ReadList[StringToStream[Hist1figureYrange],Word];
If[Hist1figureXrange[[1]]!="auto",Hist1figureXrange[[1]]=Read[StringToStream[Hist1figureXrange[[1]] ],Number] ];
If[Hist1figureXrange[[2]]!="auto",Hist1figureXrange[[2]]=Read[StringToStream[Hist1figureXrange[[2]] ],Number] ];
If[Hist1figureYrange[[1]]!="auto",Hist1figureYrange[[1]]=Read[StringToStream[Hist1figureYrange[[1]] ],Number] ];
If[Hist1figureYrange[[2]]!="auto",Hist1figureYrange[[2]]=Read[StringToStream[Hist1figureYrange[[2]] ],Number] ];

(*20170306: add color seperator*)
itag=itag+1;
If[output3[[itag,1]]==ColorSeperatorTag,ColorSeperator=output3[[itag,2]] ];
Head[ColorSeperator];

ColorSeperator=ReadList[StringToStream[ColorSeperator],Word];
If[ColorSeperator[[1]]!="auto",
Table[ColorSeperator[[i]]=Read[StringToStream[ColorSeperator[[i]] ],Number];"dummy",{i,1,Length[ColorSeperator]}] 
];

(*read HighlightType(FigureType) and HighlightMode*)
itag=itag+1;
If[output3[[itag,1]]==HighlightTypeTag,HighlightType=output3[[itag,2]] ];
Head[HighlightType];

itag=itag+1;
If[output3[[itag,1]]==HighlightModeTag,HighlightMode=output3[[itag,2]] ];
Head[HighlightMode];
(*Print[output3[[itag,1]],"   ",HighlightModeTag];*)

itag=itag+1;
If[output3[[itag,1]]==HighlightMode1Tag,HighlightMode1=output3[[itag,2]] ];
Head[HighlightMode1];
(*Print[output3[[itag,1]],"   ",HighlightMode1Tag];*)

itag=itag+1;
If[output3[[itag,1]]==HighlightMode2Tag,HighlightMode2=output3[[itag,2]] ];
Head[HighlightMode2];
(*Print[output3[[itag,1]],"   ",HighlightMode2Tag];*)

HighlightType=ReadList[StringToStream[HighlightType],Word];
HighlightMode=ReadList[StringToStream[HighlightMode],Number];
(*20171109: new convention of highlight range:  *)
(* Mode:                        1                      2    3   4 ...*)
(* Mode 1 range: {{hmin1,hmax1},{hmin2,hmax2},...};  ...; ...; ...*)
HighlightMode1=StringSplit[Read[StringToStream[HighlightMode1],String],";"];(*ReadList[StringToStream[HighlightMode1],Number];*)
HighlightMode2=StringSplit[Read[StringToStream[HighlightMode2],String],";"];(*ReadList[StringToStream[HighlightMode2],Number];*)
(*read the List expression for every figure type*)
HighlightMode1=Read[StringToStream[#],Expression]&/@HighlightMode1;
HighlightMode2=Read[StringToStream[#],Expression]&/@HighlightMode2;
(*20170307*)
(*read Size*)
(*20170315: size replace to the next of highlight mode*)
itag=itag+1;
If[output3[[itag,1]]==SizeTag,Size=output3[[itag,2]];Size=Read[StringToStream[Size],Word] ];
Head[Size];
(*Print[output3];*)


"dummy";

{Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,(*UserArgName,UserArgValue,*)
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,
ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}
]


(* ::Input::Initialization:: *)
(*20171109: version 5: delete the user define function value part, ReadUserFunction will be in charge of it*)
(*20171128: change hist xmin, xmax as zmin, zmax of color palette, delete color percentage, job id could be string*)
readcorrconfigfile6[configDirin_,configfilenamein_]:=
Module[{configDir=configDirin,configfilename=configfilenamein,
JobidTag,PDFnameTag,FigureTypeTag,FigureFlagTag,ExptidTypeTag,ExptidFlagTag,CorrelationArgTypeTag,CorrelationArgFlagTag,UserArgNameTag,UserArgValueTag,
XQfigureXrangeTag,XQfigureYrangeTag,Hist1figureNbinTag,Hist1figureXrangeTag,Hist1figureYrangeTag,ColorSeperatorTag,
SizeTag,HighlightTypeTag,HighlightModeTag,HighlightMode1Tag,HighlightMode2Tag,
Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2,
itag,s,output,output2,output3,
ColorPaletterange,ColorPaletterangeTag
},

JobidTag="Job ID (copy from the counter file)";
PDFnameTag="PDF set";
FigureTypeTag="Type";
FigureFlagTag="Flag";
ExptidTypeTag="Expt. ID";
ExptidFlagTag="Expt. Flag";
CorrelationArgTypeTag="Type";
CorrelationArgFlagTag="Flag";
(*20171109: delete the user define function value part, ReadUserFunction will be in charge of it*)
(*
UserArgNameTag="Name";
UserArgValueTag="Values";
*)

XQfigureXrangeTag="xmin,   xmax";
XQfigureYrangeTag="mumin, mumax";
ColorPaletterangeTag="zmin, zmax";(*20171128*)
Hist1figureNbinTag="Number of bins";
(*
Hist1figureXrangeTag="xmin, xmax";
Hist1figureYrangeTag="ymin, ymax";
*)
(*
Hist2figureXrangeTag
Hist2figureYrangeTag
*)
ColorSeperatorTag="Color by data percentage";
SizeTag="Size";
HighlightTypeTag="Type";
HighlightModeTag="Mode";
HighlightMode1Tag="Mode 1 range";
HighlightMode2Tag="Mode 2 range";

Jobid="unset";
PDFname="unset";
FigureType="unset";
FigureFlag="unset";
ExptidType="unset";
ExptidFlag="unset";
CorrelationArgType="unset";
CorrelationArgFlag="unset";
(*20171109: delete the user define function value part, ReadUserFunction will be in charge of it*)
(*
UserArgName="unset";
UserArgValue="unset";
*)
XQfigureXrange="unset";
XQfigureYrange="unset";
ColorPaletterange="unset";(*20171128*)
Hist1figureNbin="unset";
(*
Hist1figureXrange="unset";
Hist1figureYrange="unset";
*)
ColorSeperator="unset";
Size="unset";
HighlightType="unset";
HighlightMode="unset";
HighlightMode1="unset";
HighlightMode2="unset";

(*read config file line by line into list*)
s=OpenRead[configDir<>configfilename];
output=ReadList[s,String];
Close[s];

(*delete comments: with "#" at begining of line*)
output2={};
Table[
If[StringTake[output[[i]],1]!="#",output2=Append[output2,output[[i]] ] ];
"dummy"
,{i,1,Length[output]}
];
(*seperate the tag of configure file and arguments by ":"*)
output3=Table[StringSplit[output2[[i]],":"],{i,1,Length[output2]}];

(*check tag exist, if a tag exist, read arguments corresponding to that tag*)
(*read Job id*)
itag=1;
If[output3[[itag,1]]==JobidTag,Jobid=output3[[itag,2]];Jobid=Read[StringToStream[Jobid],(*Number*)(*20171128*)String] ];
Head[Jobid];
(*read PDFname*)
itag=itag+1;
If[output3[[itag,1]]==PDFnameTag,PDFname=output3[[itag,2]];PDFname=Read[StringToStream[PDFname],Word] ];
Head[PDFname];
(*read FigureType and FigureFlag*)
itag=itag+1;
If[output3[[itag,1]]==FigureTypeTag,FigureType=output3[[itag,2]] ];
Head[FigureType];

itag=itag+1;
If[output3[[itag,1]]==FigureFlagTag,FigureFlag=output3[[itag,2]] ];
Head[FigureFlag];

FigureType=ReadList[StringToStream[FigureType],Word];
FigureFlag=ReadList[StringToStream[FigureFlag],Number];

(*read ExptidType and ExptidFlag*)
itag=itag+1;
If[output3[[itag,1]]==ExptidTypeTag,ExptidType=output3[[itag,2]] ];
Head[ExptidType];

itag=itag+1;
If[output3[[itag,1]]==ExptidFlagTag,ExptidFlag=output3[[itag,2]] ];
Head[ExptidFlag];

ExptidType=ReadList[StringToStream[ExptidType],Number];
ExptidFlag=ReadList[StringToStream[ExptidFlag],Number];

(*read CorrelationArgType and CorrelationArgFlag*)
itag=itag+1;
If[output3[[itag,1]]==CorrelationArgTypeTag,CorrelationArgType=output3[[itag,2]] ];
Head[CorrelationArgType];

itag=itag+1;
If[output3[[itag,1]]==CorrelationArgFlagTag,CorrelationArgFlag=output3[[itag,2]] ];
Head[CorrelationArgFlag];

CorrelationArgType=ReadList[StringToStream[CorrelationArgType],Word];
CorrelationArgFlag=ReadList[StringToStream[CorrelationArgFlag],Number];

(*20171109: delete the user define function value part, ReadUserFunction will be in charge of it*)
(*
(*read UserArgName*)
itag=itag+1;
If[output3[[itag,1]]==UserArgNameTag,UserArgName=output3[[itag,2]] ];
Head[UserArgName];
(*read UserArgValue*)
itag=itag+1;
If[output3[[itag,1]]==UserArgValueTag,UserArgValue=output3[[itag,2]] ];
Head[UserArgValue];

UserArgValue=ReadList[StringToStream[UserArgValue],Number];
*)
(*read XQfigureXrange and XQfigureYrange*)
itag=itag+1;
If[output3[[itag,1]]==XQfigureXrangeTag,XQfigureXrange=output3[[itag,2]] ];
Head[XQfigureXrange];

itag=itag+1;
If[output3[[itag,1]]==XQfigureYrangeTag,XQfigureYrange=output3[[itag,2]] ];
Head[XQfigureYrange];

XQfigureXrange=ReadList[StringToStream[XQfigureXrange],Word];
XQfigureYrange=ReadList[StringToStream[XQfigureYrange],Word];
If[XQfigureXrange[[1]]!="auto",XQfigureXrange[[1]]=Read[StringToStream[XQfigureXrange[[1]] ],Number] ];
If[XQfigureXrange[[2]]!="auto",XQfigureXrange[[2]]=Read[StringToStream[XQfigureXrange[[2]] ],Number] ];
If[XQfigureYrange[[1]]!="auto",XQfigureYrange[[1]]=Read[StringToStream[XQfigureYrange[[1]] ],Number] ];
If[XQfigureYrange[[2]]!="auto",XQfigureYrange[[2]]=Read[StringToStream[XQfigureYrange[[2]] ],Number] ];
(*read Hist1figureNbin, Hist1figureXrange, Hist1figureYrange*)(*20171128 delete histogram ranges and add color palette*)
itag=itag+1;
If[output3[[itag,1]]==ColorPaletterangeTag,ColorPaletterange=output3[[itag,2]] ];
Head[ColorPaletterange];
ColorPaletterange=ReadList[StringToStream[ColorPaletterange],Word];
If[ColorPaletterange[[1]]!="auto",ColorPaletterange[[1]]=Read[StringToStream[ColorPaletterange[[1]] ],Number] ];
If[ColorPaletterange[[2]]!="auto",ColorPaletterange[[2]]=Read[StringToStream[ColorPaletterange[[2]] ],Number] ];


itag=itag+1;
If[output3[[itag,1]]==Hist1figureNbinTag,Hist1figureNbin=output3[[itag,2]];Hist1figureNbin=Read[StringToStream[Hist1figureNbin],Word] ];
Head[Hist1figureNbin];
If[Hist1figureNbin!="auto",Hist1figureNbin=Read[StringToStream[Hist1figureNbin],Number] ];
(*20171128
itag=itag+1;
If[output3[[itag,1]]==Hist1figureXrangeTag,Hist1figureXrange=output3[[itag,2]] ];
Head[Hist1figureXrange];

itag=itag+1;
If[output3[[itag,1]]==Hist1figureYrangeTag,Hist1figureYrange=output3[[itag,2]] ];
Head[Hist1figureYrange];

Hist1figureXrange=ReadList[StringToStream[Hist1figureXrange],Word];
Hist1figureYrange=ReadList[StringToStream[Hist1figureYrange],Word];
If[Hist1figureXrange[[1]]!="auto",Hist1figureXrange[[1]]=Read[StringToStream[Hist1figureXrange[[1]] ],Number] ];
If[Hist1figureXrange[[2]]!="auto",Hist1figureXrange[[2]]=Read[StringToStream[Hist1figureXrange[[2]] ],Number] ];
If[Hist1figureYrange[[1]]!="auto",Hist1figureYrange[[1]]=Read[StringToStream[Hist1figureYrange[[1]] ],Number] ];
If[Hist1figureYrange[[2]]!="auto",Hist1figureYrange[[2]]=Read[StringToStream[Hist1figureYrange[[2]] ],Number] ];
*)
(*20170306: add color seperator*)
itag=itag+1;
If[output3[[itag,1]]==ColorSeperatorTag,ColorSeperator=output3[[itag,2]] ];
Head[ColorSeperator];

ColorSeperator=ReadList[StringToStream[ColorSeperator],Word];
If[ColorSeperator[[1]]!="auto",
Table[ColorSeperator[[i]]=Read[StringToStream[ColorSeperator[[i]] ],Number];"dummy",{i,1,Length[ColorSeperator]}] 
];

(*read HighlightType(FigureType) and HighlightMode*)
itag=itag+1;
If[output3[[itag,1]]==HighlightTypeTag,HighlightType=output3[[itag,2]] ];
Head[HighlightType];

itag=itag+1;
If[output3[[itag,1]]==HighlightModeTag,HighlightMode=output3[[itag,2]] ];
Head[HighlightMode];
(*Print[output3[[itag,1]],"   ",HighlightModeTag];*)

itag=itag+1;
If[output3[[itag,1]]==HighlightMode1Tag,HighlightMode1=output3[[itag,2]] ];
Head[HighlightMode1];
(*Print[output3[[itag,1]],"   ",HighlightMode1Tag];*)

itag=itag+1;
If[output3[[itag,1]]==HighlightMode2Tag,HighlightMode2=output3[[itag,2]] ];
Head[HighlightMode2];
(*Print[output3[[itag,1]],"   ",HighlightMode2Tag];*)

HighlightType=ReadList[StringToStream[HighlightType],Word];
HighlightMode=ReadList[StringToStream[HighlightMode],Number];
(*20171109: new convention of highlight range:  *)
(* Mode:                        1                      2    3   4 ...*)
(* Mode 1 range: {{hmin1,hmax1},{hmin2,hmax2},...};  ...; ...; ...*)
HighlightMode1=StringSplit[Read[StringToStream[HighlightMode1],String],";"];(*ReadList[StringToStream[HighlightMode1],Number];*)
HighlightMode2=StringSplit[Read[StringToStream[HighlightMode2],String],";"];(*ReadList[StringToStream[HighlightMode2],Number];*)
(*read the List expression for every figure type*)
HighlightMode1=Read[StringToStream[#],Expression]&/@HighlightMode1;
HighlightMode2=Read[StringToStream[#],Expression]&/@HighlightMode2;
(*20170307*)
(*read Size*)
(*20170315: size replace to the next of highlight mode*)
itag=itag+1;
If[output3[[itag,1]]==SizeTag,Size=output3[[itag,2]];Size=Read[StringToStream[Size],Word] ];
Head[Size];
(*Print[output3];*)


"dummy";

{Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,(*UserArgName,UserArgValue,*)
XQfigureXrange,XQfigureYrange,ColorPaletterange(*20171128*),Hist1figureNbin,(*Hist1figureXrange,Hist1figureYrange,*)
ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}
]


(* ::Input::Initialization:: *)
(*20171109*)
(*read user define function or data, input filename and output user define values*)
ReadUserFunction[UserFuncDirin_,UserFuncfilenamein_]:=
Module[{UserFuncDir=UserFuncDirin,UserFuncfilename=UserFuncfilenamein,
UserArgNameTag,UserArgValueTag,UserArgName,UserArgValue,
itag,s,output,output2,output3
},

UserArgNameTag="Name";
UserArgValueTag="Values";


UserArgName="unset";
UserArgValue="unset";


(*read config file line by line into list*)
s=OpenRead[UserFuncDir<>UserFuncfilename];
output=ReadList[s,String];
Close[s];

(*delete comments: with "#" at begining of line*)
output2={};
Table[
If[StringTake[output[[i]],1]!="#",output2=Append[output2,output[[i]] ] ];
"dummy"
,{i,1,Length[output]}
];
(*seperate the tag of configure file and arguments by ":"*)
output3=Table[StringSplit[output2[[i]],":"],{i,1,Length[output2]}];

(*check tag exist, if a tag exist, read arguments corresponding to that tag*)
(*read UserArgName*)
itag=1;
If[output3[[itag,1]]==UserArgNameTag,UserArgName=output3[[itag,2]] ];
Head[UserArgName];
(*read UserArgValue*)
itag=itag+1;
If[output3[[itag,1]]==UserArgValueTag,UserArgValue=output3[[itag,2]] ];
Head[UserArgValue];
UserArgValue=ReadList[StringToStream[UserArgValue],Number];

{UserArgName,UserArgValue}
]

(*20171116*)
(*read user define function or data, input filename and output user define function, the function coulld be the Mathematica format function*)
ReadUserFunctionV2[UserFuncDirin_,UserFuncfilenamein_]:=
Module[{UserFuncDir=UserFuncDirin,UserFuncfilename=UserFuncfilenamein,
UserArgNameTag,UserArgFunctionTag,UserArgName,UserArgFunction,
itag,s,output,output2,output3
},

UserArgNameTag="Name";
UserArgFunctionTag="Function";


UserArgName="unset";
UserArgFunction="unset";


(*read config file line by line into list*)
s=OpenRead[UserFuncDir<>UserFuncfilename];
output=ReadList[s,String];
Close[s];

(*delete comments: with "#" at begining of line*)
output2={};
Table[
If[StringTake[output[[i]],1]!="#",output2=Append[output2,output[[i]] ] ];
"dummy"
,{i,1,Length[output]}
];
(*seperate the tag of configure file and arguments by ":"*)
output3=Table[StringSplit[output2[[i]],":"],{i,1,Length[output2]}];

(*check tag exist, if a tag exist, read arguments corresponding to that tag*)
(*read UserArgName*)
itag=1;
If[output3[[itag,1]]==UserArgNameTag,UserArgName=output3[[itag,2]] ];
Head[UserArgName];
(*read UserArgFunction*)
itag=itag+1;
If[output3[[itag,1]]==UserArgFunctionTag,UserArgFunction=output3[[itag,2]] ];
Head[UserArgFunction];
UserArgFunction=Read[StringToStream[UserArgFunction],Expression];

{UserArgName,UserArgFunction}
]

(*20171119*)
(*read user define function or data, input filename and output user define function, the function coulld be the Mathematica format function*)
(*the expression is between UserArgFunctionBeginTag and UserArgFunctionEndTag*)
ReadUserFunctionV3[UserFuncDirin_,UserFuncfilenamein_]:=
Module[{UserFuncDir=UserFuncDirin,UserFuncfilename=UserFuncfilenamein,
UserArgNameTag,UserArgFunctionTag,UserArgName,UserArgFunction,
itag,s,output,output2,output3,
UserArgFunctionBeginTag,UserArgFunctionEndTag
},

UserArgNameTag="Name";
UserArgFunctionTag="Function";
UserArgFunctionBeginTag="!Function Begin:";
UserArgFunctionEndTag="!Function End";

(*
UserArgName="unset";
UserArgFunction="unset";

UserArgFunctionBegin="unset";
UserArgFunctionEnd="unset";
*)


(*read config file line by line into list*)
s=OpenRead[UserFuncDir<>UserFuncfilename];
output=ReadList[s,String];
Close[s];

(*delete comments: with "#" at begining of line*)
output2={};
Table[
If[StringTake[output[[i]],1]!="#",output2=Append[output2,output[[i]] ] ];
"dummy"
,{i,1,Length[output]}
];
(*check the first tag is "Name:"*)
If[StringSplit[output2[[1]],":"][[1]]!=UserArgNameTag, Print["error, the first tag should be ",UserArgNameTag,":"];Abort[] ];

(*back to one string*)
output3=StringJoin[output2];
(*seperate string by user name tag*)
output3=StringSplit[output3,UserArgNameTag<>":"];
(*check after user name tag, there are one function begin tag and one function end tag*)
If[StringCount[#,UserArgFunctionBeginTag]!=1 ||StringCount[#,UserArgFunctionEndTag]!=1 ,Print["error, the file format should be ",UserArgNameTag,": xxx ",UserArgFunctionBeginTag," xxx ",UserArgFunctionEndTag];Abort[] ]&/@output3;
(*for each string after user name tag, seperate it by begin function tag*)
output3=StringSplit[#,UserArgFunctionBeginTag]&/@output3;

(*check after split, there is {username, function description} output for each user name tag*)
If[Length[#]!=2,Print["error, the file format should be ",UserArgNameTag,": xxx ",UserArgFunctionBeginTag," xxx ",UserArgFunctionEndTag];Abort[] ]&/@output3;

(*check every string of begin function should has an end function tag at the tail*)
If[
Length[StringSplit[#[[2]],UserArgFunctionEndTag] ]!=1 || StringCount[#[[2]],UserArgFunctionEndTag]!=1,
Print["error, the file format should be ",UserArgNameTag,": xxx ",UserArgFunctionBeginTag," xxx ",UserArgFunctionEndTag];
Abort[] 
]&/@output3;
(*delete the function end tag for each function begin content*)
Table[output3[[i,2]]=StringSplit[output3[[i,2]],UserArgFunctionEndTag][[1]];"dummy",{i,Length[output3]}];

(*for each user name, read function content into expression format*)
Table[output3[[i,2]]=Read[StringToStream[output3[[i,2]] ],Expression];"dummy",{i,Length[output3]}];

Return[output3];
(*seperate the tag of configure file and arguments by ":"*)
output3=Table[StringSplit[output2[[i]],":"],{i,1,Length[output2]}];



(*check tag exist, if a tag exist, read arguments corresponding to that tag*)
(*read UserArgName*)
itag=1;
If[output3[[itag,1]]==UserArgNameTag,UserArgName=output3[[itag,2]] ];
Head[UserArgName];
(*read UserArgFunction*)
itag=itag+1;
If[output3[[itag,1]]==UserArgFunctionTag,UserArgFunction=output3[[itag,2]] ];
Head[UserArgFunction];
UserArgFunction=Read[StringToStream[UserArgFunction],Expression];
(*
{UserArgName,UserArgFunction}
*)
(*output format*)
(*{{user name 1, user function 1}, {user name 2, user function 2}...}*)
output3
]


(* ::Chapter:: *)
(*plot class*)


(* ::Section:: *)
(*print function: print info of experiment in a PDFset*)


(* ::Input::Initialization:: *)
(*20170228: it seems subsetQ does not work at 10.2 version (curie, rubin), before solving it, don't run it*)
(*don't initialize it*)
(*8_2 version: delete them*)




(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Section:: *)
(*class for general setting of various kinds of plots*)


(* ::Subsection:: *)
(*tools for plots class*)


(* ::Input::Initialization:: *)
(*this function return markers and their normalized size by {shapes,sizes}*)
(*shapes = {shape1,shape2,...}, sizes = {size1,size2,...}*)
PlotMarkerList[]:=
Module[{shapes0,shapes1,shapes2,shapes3,sizes0,sizes1,sizes2,sizes3},
shapes1={\[FivePointedStar],\[ClubSuit],\[HeartSuit],\[SpadeSuit],\[EighthNote],\[Sharp],\[Yen],\[Section],\[BeamedSixteenthNote],\[EmptySet],\[Earth],"\[Times]","\[CircleMinus]","\[Superset]",">","<",\[LightBulb],\[Euro],\[Cent],\[Flat]};
sizes1={800,800,1000,800,800,1000,1000,1000,800,900,900,1200,900,800,1000,1000,1000,1000,1000,1200};
shapes2={"\[FilledCircle]","\[FilledSquare]","\[FilledDiamond]","\[FilledUpTriangle]","\[FilledDownTriangle]","\[EmptyCircle]","\[EmptySquare]","\[EmptyDiamond]","\[EmptyUpTriangle]","\[EmptyDownTriangle]"};
sizes2={750.0,1000.0,900.0,850.0,850.0,1050.0,1300.0,1200.0,1150.0,1150.0};
shapes0={\[CloverLeaf],\[Currency],\[DownExclamation],\[Checkmark],"\[DoubleRightArrow]","\[DoubleLeftArrow]","\[DoubleUpArrow]","\[DoubleDownArrow]","\[SquareSubset]","\[SquareSuperset]","\[NotTilde]","\[NotEqual]","\[PartialD]","\[Intersection]","\[Infinity]","\[CapitalDifferentialD]"};
sizes0={1000,1200,1200,1000,1000,1000,900,900,800,800,1400,1200,900,800,1000,800};

(*check size and shape list Length are the same*)
If[Length[shapes0]!=Length[sizes0],Print["error, #shapes0 and #sizes0 are not the same, {#shapes0,#sizes0} = ",{shapes0,sizes0}];Abort[] ];
If[Length[shapes1]!=Length[sizes1],Print["error, #shapes1 and #sizes1 are not the same, {#shapes1,#sizes2} = ",{shapes1,sizes1}];Abort[] ];
If[Length[shapes2]!=Length[sizes2],Print["error, #shapes2 and #sizes2 are not the same, {#shapes2,#sizes2} = ",{shapes2,sizes2}];Abort[] ];
shapes3=Join[shapes2,shapes1,shapes0];
sizes3=Join[sizes2,sizes1,sizes0];

(*return*)
{shapes3,sizes3}
]


(*input: RGB="R" or "G" or "B" or "Brown" or "Gray"*)
(*output: red (red, deep red, deep orange...), green, blue, blown, gray color lists*)
colorset[RGBin_]:=
Module[{RGB=RGBin,colorset,Rcolor,Gcolor,Bcolor,Graycolor,Browncolor,output},
colorset=ColorData["WebSafe","ColorList"];
Rcolor={colorset[[12]],colorset[[14]],colorset[[32]],colorset[[18]],colorset[[23]],colorset[[29]],colorset[[36]],colorset[[22]],Red,colorset[[43*1+29]]};
Gcolor={colorset[[43*4+12]],colorset[[43*4+14]],colorset[[43*4+20]],colorset[[43*4+26]],colorset[[43*4+32]],colorset[[43*2+40]],colorset[[41]],Green};
Bcolor={colorset[[43*4+3]],colorset[[43*4+9]],colorset[[43*4+15]],colorset[[43*4+22]],colorset[[43]],colorset[[43*2+7]],colorset[[43*2+13]],colorset[[43*1+30]],Blue};
Graycolor={colorset[[43*1+1]],colorset[[43*2+1]],colorset[[43*3+1]],colorset[[43*4+1]],Gray};
Browncolor={colorset[[43*2+9]],colorset[[43*1+9]],colorset[[43*3+9]],colorset[[43*2+16]],colorset[[43*2+22]],Brown};

output=
Switch[
RGB,
"R",Rcolor,
"G",Gcolor,
"B",Bcolor,
"Gray",Graycolor,
"Brown",Browncolor,
_,
Print["error, input should be \"R\" or \"G\" or \"B\" or \"Brown\" or \"Gray\""]
];

output

]


(* ::Input::Initialization:: *)
(*classify data by groups of ExptID lists*)
(*
data: {data1,data2,data3,...}
dataExptID={ID1,ID2,...} corresponding to {data1,data2,...}
grouplists: {group1,group2,group2,...}; groupN = {element1,element2,...}
output the subgroups, if the Expt ID for a data belong to groupN, add the data in that subgroup 
e.g.:
if IDN belong to elementM, them put dataN in the M-th subgroup
*)
ClassifyByGroups[datain_,dataExptIDin_,groupnamesin_,grouplistsin_]:=
Module[{data=datain,dataExptID=dataExptIDin,groupnames=groupnamesin,grouplists=grouplistsin,subgroupindexlist,otherindexlist,groupExptIDs,groupdata},
(*check data and dataExptID have the same Length *)
If[
Length[data]!=Length[dataExptID],
Print["error, data and dataExptID should have the same # of elements, {#data,#dataExptID} = ",{Length[data],Length[dataExptID]}];
Abort[] 
];
(*check groupnames and grouplists have the same Length *)
If[
Length[groupnames]!=Length[grouplists],
Print["error, groupnames and grouplists should have the same # of elements, {#groupnames,#grouplists} = ",{Length[groupnames],Length[grouplists]}];
Abort[] 
];
(*check input subgroups do not overlap*)
If[Length[Union[Flatten[grouplists] ] ]!=Length[Flatten[grouplists] ],Print["error, some elements are in multiple subgroups, grouplist: ",grouplists];Abort[] ];

(*classify data by each subgroup*)
subgroupindexlist=
Table[
(*if the dataExptID[[i]] belong to a subgroup, add it to this subgroup*)
Select[Range[Length[dataExptID] ],Length[Intersection[{dataExptID[[#]]},grouplists[[igroup]] ] ]==1& ],
{igroup,Length[grouplists]}
];
otherindexlist=Complement[Range[Length[dataExptID] ],Flatten[subgroupindexlist] ];

(*add the other group (not belong to anyone)*)
subgroupindexlist=Append[subgroupindexlist,otherindexlist];
(*check all elements after being classified are still exist*)
If[
Length[subgroupindexlist//Flatten]!=Length[dataExptID],
Print["error, elements of unclassified data and elements of classified data do not match, {#origin,#classified} = ",{Length[dataExptID],Length[subgroupindexlist//Flatten]}];
Abort[] 
];

(*construct new data by categories*)
groupnames=groupnames~Join~{"Others"};
groupExptIDs=
Table[
dataExptID[[subgroupindexlist[[igroup]] ]],
{igroup,Length[grouplists]+1}
];
groupdata=
Table[
data[[subgroupindexlist[[igroup]] ]],
{igroup,Length[grouplists]+1}
];

(*output*)
{groupnames,groupExptIDs,groupdata}
]


(*input classify modes, output *)
(*
this function input the groupnames & grouplists into ClassifyByGroups according to ClassifyMode,
single: classify data by each expt ID
all: define all data in one group
...
...

output the {groupnames,groupExptIDs,groupdata} for corresponding ClassifyMode
*)
ClassifyPlottedData[datain_,dataExptIDin_,ClassifyModein_]:=
Module[{(*data=datain,dataExptID=dataExptIDin,*)ClassifyMode=ClassifyModein,groupnames,grouplists,output},
output=
Switch[
ClassifyMode,
"single",
groupnames=ToString[#]&/@dataExptIDin;
grouplists={#}&/@dataExptIDin;
 ClassifyByGroups[datain,dataExptIDin,groupnames,grouplists],
"all",
groupnames={"All"};
grouplists={dataExptIDin};
(*
 ClassifyByGroups[datain,dataExptIDin,groupnames,grouplists]
*){groupnames,grouplists,{datain}},
_,
Print["error, Classify Mode should be \"single\" or \"all\" "];Abort[]
];

output
]


(* ::Subsection:: *)
(*plotsetting class*)


(* ::Input::Initialization:: *)
Plotsetting=
<|
"imgsize"-> "none",
"title"-> "none",
"xtitle"->  "none",
"ytitle"->  "none",
"lgdlabel"->  "none",
"xrange"->  "none",
"yrange"->  "none",
"epilog"->  "none",
"titlesize"->  "none",
"xtitlesize"->  "none",
"ytitlesize"->  "none",
"lgdlabelsize"->  "none",
"ticklablesize"->  "none",
(*for plot 1*)
"plotstyle"->  "none",
"marker"->  "none",
(*for plot 2*)
"a"->  "none",
"b"->  "none",
"c"->  "none",
"d"->  "none"
|>


(* ::Input::Initialization:: *)



(* ::Input::Initialization:: *)
(*this function input a class of data, and expttype (option: single, multi, All, ProtonNeutron)
then define setting of xQplot (title, legend,...)*)
setplotsetting[dataclassin_,exptlistin_,expttypein_,plottypein_,obsAtypein_,obsBtypein_]:=
Module[{dataclass=dataclassin,exptlist=exptlistin,expttype=expttypein,plottype=plottypein,myplotsetting,
imgsize,title,xtitle,ytitle,lgdlabel,xrange,yrange,epilog,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,
PDISNCtitle,NDISCCtitle,PDISNCCCtitle,PVBPZtitle,PVBPWtitle,PJPtitle,hDISNCtitle,hDISCCtitle,hVBPZtitle,
Markdft,Psize,Nsize,hsize,myMarker,marknorm,myplotstyle,
PDISNCmarker,hDISNCmarker,NDISCCmarker,hDISCCmarker,PDISNCCCmarker,PVBPZmarker,hVBPZmarker,PVBPWmarker,PJPmarker,
PDISNCcolor,hDISNCcolor,NDISCCcolor,hDISCCcolor,PDISNCCCcolor,PVBPZcolor,hVBPZcolor,PVBPWcolor,PJPcolor},
(*declare a plot setting class*)
myplotsetting=Plotsetting;

(*declare*)
title="none";
lgdlabel="none";
epilog="none";
(*****************)
(*general setting*)
(*****************)
imgsize={{700},{700}};

xtitle="x";
ytitle="\[Mu] [GeV]";
xrange={0.000001,1.0};
yrange={1,1200};

titlesize=24;
xtitlesize=18;
ytitlesize=18;
lgdlabelsize=12;
ticklablesize=18;
(*****************)
(*for plot1 general setting*)
(*****************)
(*legend title set*)
PDISNCtitle="DIS NC (\[ScriptP]\[ScriptL] \[Rule] \[ScriptL]X)";
NDISCCtitle="DIS CC (\[Nu]N \[Rule] \[ScriptL]X)";
PDISNCCCtitle="DIS NC&CC (\[ScriptP]\[ScriptL] \[Rule] \[ScriptL]X/\[Nu]X)";
PVBPZtitle="VBP Z/\!\(\*SuperscriptBox[\(\[Gamma]\), \(*\)]\) (\[ScriptP]\[ScriptP]/\[ScriptP]\!\(\*OverscriptBox[\(\[ScriptP]\), \(_\)]\) \[Rule] \!\(\*SuperscriptBox[\(\[ScriptL]\), \(+\)]\)\!\(\*SuperscriptBox[\(\[ScriptL]\), \(-\)]\)X)";
PVBPWtitle="VBP \[ScriptL] asym (\[ScriptP]\[ScriptP]/\[ScriptP]\!\(\*OverscriptBox[\(\[ScriptP]\), \(_\)]\) \[Rule] \[ScriptL]\[Nu]X)";
PJPtitle="\[ScriptP]\[ScriptP]/\[ScriptP]\!\(\*OverscriptBox[\(\[ScriptP]\), \(_\)]\) \[Rule] \[ScriptJ]X";

hDISNCtitle="DIS NC (\[ScriptP]\[ScriptL]/d\[ScriptL] \[Rule] \[ScriptL]X)";
hDISCCtitle="DIS CC (\[Nu]Fe \[Rule] \[ScriptL]X)";
hVBPZtitle="VBP Z/\!\(\*SuperscriptBox[\(\[Gamma]\), \(*\)]\) (\[ScriptP]Cu \[Rule] \!\(\*SuperscriptBox[\(\[ScriptL]\), \(+\)]\)\!\(\*SuperscriptBox[\(\[ScriptL]\), \(-\)]\)X)";
(*marker set 1*)

Markdft=Graphics`PlotMarkers[];

Psize=8;
Nsize=15;
hsize=12;
PDISNCmarker={Markdft[[4,1]],Psize};
hDISNCmarker={"\[Times]",hsize};
NDISCCmarker={Markdft[[9,1]],Nsize};
hDISCCmarker={"\[Times]",hsize};
PDISNCCCmarker={Markdft[[4,1]],Psize};
(*PVBPWmarker=Markdft[[2]];*)
PVBPZmarker={Markdft[[4,1]],Psize};
hVBPZmarker={"\[Times]",hsize};
PVBPWmarker={Markdft[[4,1]],Psize};
PJPmarker={Markdft[[4,1]],Psize};

(*marker color set 1*)
PDISNCcolor=colorset["Brown"][[4]];
hDISNCcolor=colorset["Gray"][[4]];
NDISCCcolor=colorset["R"][[7]];
hDISCCcolor=colorset["R"][[3]];
PDISNCCCcolor=colorset["R"][[4]];
(*
PVBPWcolor=Blue;
*)
PVBPZcolor=colorset["B"][[4]];
hVBPZcolor=colorset["B"][[3]];
PVBPWcolor=colorset["B"][[1]];
PJPcolor=colorset["G"][[1]];

(*legend set*)
(*lgd={PDISNCtitle,hDISNCtitle,NDISCCtitle,hDISCCtitle,PDISNCCCtitle,PVBPZtitle,hVBPZtitle,PVBPWtitle,PJPtitle};*)
(*marker set*)
myMarker={PDISNCmarker,hDISNCmarker,NDISCCmarker,hDISCCmarker,PDISNCCCmarker,PVBPZmarker,hVBPZmarker,PVBPWmarker,PJPmarker};
marknorm=0.7;
myMarker=Table[{myMarker[[i,1]],marknorm*myMarker[[i,2]]},{i,1,Length[myMarker]}];

(*marker color set*)
myplotstyle={PDISNCcolor,hDISNCcolor,NDISCCcolor,hDISCCcolor,PDISNCCCcolor,PVBPZcolor,hVBPZcolor,PVBPWcolor,PJPcolor};


(*****************)
(*specific setting for various plots*)
(*****************)
(*plot 1*)
If[
plottype==1,
If[
expttype=="single",
title="Experimental data in "<>dataclass[[1,1]][["PDFinfo","PDFname"]]<>" analysis \n "<>" ("<>ExptIDtoName[exptlist[[1]] ]<>")";(*dataclass[[1,1]] is [[expt,flavour]], need modify it later by variable rather than number*)
(*legend set*)
lgdlabel={dataclass[[1,1]][["exptinfo","exptname"]]};
];

If[
expttype=="multi",
title="Experimental data in "<>dataclass[[1,1]][["PDFinfo","PDFname"]]<>" analysis \n expt id: "<>ToString[exptlist];
(*legend set*)
lgdlabel=Table[dataclass[[iexpt,1]][["exptinfo","exptname"]],{iexpt,1,Length[dataclass]}];
];

If[
expttype=="All",
title="Experimental data in "<>dataclass[[1,1]][["PDFinfo","PDFname"]]<>" analysis \n(heavy nucleus collision included)";
(*legend set*)
lgdlabel={PDISNCtitle,hDISNCtitle,NDISCCtitle,hDISCCtitle,PDISNCCCtitle,PVBPZtitle,hVBPZtitle,PVBPWtitle,PJPtitle};
(*marker set*)
myMarker=myMarker;
(*marker color set*)
myplotstyle=myplotstyle;

];
(*
If[
expttype\[Equal]"ByProcess",
myplotsetting[["title"]]="Experimental data in "<>dataclass[["PDFinfo","PDFname"]]<>"analysis \n(heavy nucleus collision included)";
];
*)

If[
expttype=="ProtonNeutron",
title="Experimental data in "<>dataclass[[1,1]][["PDFinfo","PDFname"]]<>"analysis \n (only P/N collision included)";
(*legend set*)
lgdlabel={PDISNCtitle,NDISCCtitle,PDISNCCCtitle,PVBPZtitle,PVBPWtitle,PJPtitle};
(*marker set, delete heavy neucleon collision processes*)
myMarker=Drop[myMarker,{7}];myMarker=Drop[myMarker,{4}];myMarker=Drop[myMarker,{2}];
(*marker color set, delete heavy neucleon collision processes*)
myplotstyle=Drop[myplotstyle,{7}];myplotstyle=Drop[myplotstyle,{4}];myplotstyle=Drop[myplotstyle,{2}];
];
];
(*plot 2*)

(*plot 3*)

(*read setting to class and output the class*)
myplotsetting[["imgsize"]]=imgsize;
myplotsetting[["title"]]=title;
myplotsetting[["xtitle"]]=xtitle;
myplotsetting[["ytitle"]]=ytitle;
myplotsetting[["lgdlabel"]]=lgdlabel;
myplotsetting[["xrange"]]=xrange;
myplotsetting[["yrange"]]=yrange;
myplotsetting[["epilog"]]=epilog;
myplotsetting[["titlesize"]]=titlesize;
myplotsetting[["xtitlesize"]]=xtitlesize;
myplotsetting[["ytitlesize"]]=ytitlesize;
myplotsetting[["lgdlabelsize"]]=lgdlabelsize;
myplotsetting[["ticklablesize"]]=ticklablesize;

myplotsetting[["plotstyle"]]=myplotstyle;
myplotsetting[["marker"]]=myMarker;

myplotsetting

]


(* ::Subsection:: *)
(*plotlabelsize class*)


(* ::Section:: *)
(*class for three kinds of plots: *)
(*1. {x,Q} plot for inputs*)
(*2. {x,Q} plot for size by value*)
(*3. histogram*)


(* ::Subsection:: *)
(*{x,Q} plot for inputs: xQplotsetting class*)


(* ::Input::Initialization:: *)
(*2016.11.04 botingw*)
(*plot pdf function with log log scale*)
(*if plotrangein\[Equal]"None", then plotrange is by default*)
(*example input: *)
(*
myMarker
myplotstyle
title="(x, Q) points of experiments based on CT14LN fitting\n (Heavy Nucleons included)";
xtitle="x";
ytitle="Q [GeV]";
lgd={"pl \[Rule] lX, NC","pl \[Rule] clX, NC","\[Nu]N \[Rule] X, CC","\[Nu]N \[Rule] cX, CC","pOverscript[p, _] \[Rule] l^+l^-X, \[ScriptD]\[Sigma]/\[ScriptD]y","pOverscript[p, _] \[Rule] l\[Nu]X, A(Subscript[y, l])"};
lgdpos={0.15,0.6};
PDFloglogplot[tmpDISdataf1,myMarker,myplotstyle,title,xtitle,ytitle,{0.001,5,1,500},lgd,lgdpos]

myMarker={{"\[FilledCircle]",6.272`},{"\[FilledSquare]",6.272`},{"\[FilledDiamond]",7.616`},{"\[FilledDiamond]",7.616`},{"\[EmptyCircle]",7.167999999999999`},{"\[EmptySquare]",7.167999999999999`}};
myplotstyle={,,,,,};

*)
PDFloglogplot[datain_,plotmarkerin_,plotstylein_,titlein_,xtitlein_,ytitlein_,plotrangein_,lgdin_,lgdposin_,imgsizein_]:=
Module[{data=datain,plotmarker=plotmarkerin,plotstyle=plotstylein,title=titlein,xtitle=xtitlein,ytitle=ytitlein,plotrange=plotrangein,lgd=lgdin,lgdpos=lgdposin,imgsize=imgsizein,plotxQout,minx,maxx,miny,maxy,imgsizedefault,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,plotrangetmp},

(*2017.01.24 data format is {{data1},{data2},...}
data = {LF1[x,Q],LF1[x,Q],...}*)

(*20170517: for some data, there is no data point, which break down the function. solution: add a fake point if there is no data point*)
(*this fake point should not in the figure xyrange, so x=2, Q=any*)
Table[If[Length[data[[idata]] ]==0,data[[idata]]=Append[data[[idata]],LF1[2.0,10.0] ] ];"dummy",{idata,1,Length[data]}];

(*when plot, need to transform to List format for every point*)
data=data/.LF1->List;

(*default*)
minx=0.00001;
maxx=1;
miny=1;
maxy=1100;
imgsizedefault={{600},{600}};
titlesize=24;
xtitlesize=18;
ytitlesize=18;
lgdlabelsize=12;
ticklablesize=18;

(*if want to customize the plot range*)
plotrangetmp=ToString[plotrange];
If[
plotrangetmp!="None",
minx=plotrange[[1]];
maxx=plotrange[[2]];
miny=plotrange[[3]];
maxy=plotrange[[4]];
];
(*set image size, by default is {600,600}, if input a imgsize, them use the input arguments as imgsize*)
If[imgsize!= "None",imgsize=imgsize,imgsize=imgsizedefault];

plotxQout=ListLogLogPlot[
data,

Frame->True,
(*
PlotMarkers\[Rule]{Automatic,Markersize},
*)
PlotMarkers->plotmarker,
PlotStyle->plotstyle,
BaseStyle->{FontSize->18(*,FontFamily\[Rule]"Helvetica"  test close it*)},
ImageSize->imgsize,(*size of image, including title, xy title, it is not just size of image frame *)
FrameLabel->{Style[xtitle,xtitlesize],Style[ytitle,ytitlesize]},
PlotLabel->Style[title,titlesize],
FrameTicksStyle->Directive[Black,ticklablesize],
PlotRange->{{minx,maxx},{miny,maxy}},
PlotLegends->Placed[PointLegend[lgd,LabelStyle->Directive[Black,lgdlabelsize,FontFamily->"Latin Modern Roman Caps"],LegendMarkerSize->15,LegendFunction->Framed ],(*lgdpos*)Right],
GridLines->{{0.00001,0.0001,0.001,0.01,0.1},{10,50,100,500,1000}},
GridLinesStyle->Directive[Dashed],
AspectRatio->1(*size ratio of x, y frame *)


];

plotxQout
 ]


(* ::Subsection:: *)
(*{x,Q} plot for size by value: xQptsizeplotsetting class*)


(* ::Input::Initialization:: *)
(*for PDFCorrelationplot3*)
(*set point size*)
Setpointsize[valuein_,maxvaluein_,pin_]:=
Module[{value=valuein,maxvalue=maxvaluein,p=pin,basesize,normsize,size},
basesize=0.001;
normsize=0.0125;(*20170307: make size small*)
(*set size uplimit*)
If[Abs[value]>maxvalue,value=maxvalue];
(*set size formula*)
size=basesize+(normsize)*(Abs[value]/maxvalue)^p;
(*plot test*)
(*
Graphics[{PointSize[size],Red,Point[{0,0}]}]
*)
size
];

(*20170309: version for highlight range of points, highlighted points larger than unhighlighted points*)
(*input value, highlight range, base size, decide the size of point*)
Setpointsize2[valuein_,highlightrangein_,nohighlightsizein_]:=
Module[{value=valuein,highlightrange=highlightrangein,nohighlightsize=nohighlightsizein,rescalesize,addsize,size},
(*rescalesize=1.5;*)

(*set size uplimit*)
If[
(*20171106: the color seperator criteria is r1\[LessEqual]value<r2, so the size also follow it*)
(value>=highlightrange[[1]]&& value<highlightrange[[2]])||(value>=(-highlightrange[[2]])&& value<(-highlightrange[[1]]) )
(*Abs[value]>=highlightrange[[1]]&& Abs[value]<=highlightrange[[2]]*),
(*for case of highlight max, min at the same point, avoid blow up by give it a value*)
addsize=
If[
highlightrange[[2]]!=highlightrange[[1]],
0.01*(Abs[value]-highlightrange[[1]])/(highlightrange[[2]]-highlightrange[[1]]),
0.01
];
size=nohighlightsize+addsize,
size=nohighlightsize
];
size
];

(*highlight points one size, unhighlighted points another size *)
Setpointsize3[valuein_,highlightrangein_,nohighlightsizein_]:=
Module[{value=valuein,highlightrange=highlightrangein,nohighlightsize=nohighlightsizein,rescalesize,addsize,size},
(*rescalesize=1.5;*)

(*set size uplimit*)
If[
(*20171106: the color seperator criteria is r1\[LessEqual]value<r2, so the size also follow it*)
(value>=highlightrange[[1]]&& value<highlightrange[[2]])||(value>=(-highlightrange[[2]])&& value<(-highlightrange[[1]]) )
(*Abs[value]>=highlightrange[[1]]&& Abs[value]<=highlightrange[[2]]*),
(*for case of highlight max, min at the same point, avoid blow up by give it a value*)
addsize=
If[
highlightrange[[2]]!=highlightrange[[1]],
0.005,
0.005
];
size=nohighlightsize+addsize,
size=nohighlightsize
];
size
];

(*20171109: highlight convention change: {{hmin1,hmax1},{hmin2,hmax2},...}*)
(*highlight points one size, unhighlighted points another size *)
Setpointsize4[valuein_,highlightrangein_,nohighlightsizein_]:=
Module[{value=valuein,highlightrange=highlightrangein,nohighlightsize=nohighlightsizein,rescalesize,addsize,size},
(*check highlight range convention is correct*)
If[Length[Dimensions[highlightrange] ]!=2 ,
Print["highlight range convention incorrect, the correct convention should be {{hmin1,hmax1},{hmin2,hmax2},...}, yours: ",highlightrange];Quit[] ];
If[Dimensions[highlightrange][[2]]!=2,
Print["highlight range convention incorrect, the correct convention should be {{hmin1,hmax1},{hmin2,hmax2},...}, yours: ",highlightrange];Quit[] ];
If[#[[2]]<#[[1]],Print["error, in each {hmin, hmax} the specifying highlight range, hmax should larger than hmin"];Quit[] ]&/@highlightrange;
(*rescalesize=1.5;*)

(*set size uplimit*)
If[
(*20171106: the color seperator criteria is r1\[LessEqual]value<r2, so the size also follow it*)
(*(value>=highlightrange[[1]]&& value<highlightrange[[2]])||(value>=(-highlightrange[[2]])&& value<(-highlightrange[[1]]) )*)
(*20171109: if the value is in any highlight range, add size*)
Intersection[(value>=#[[1]] && value<#[[2]])&/@highlightrange,{True}]=={True}
(*Abs[value]>=highlightrange[[1]]&& Abs[value]<=highlightrange[[2]]*),
(*for case of highlight max, min at the same point, avoid blow up by give it a value*)
addsize=
If[
(*20171109 for new highlight range convention*)(*highlightrange[[2]]!=highlightrange[[1]]*)highlightrange!={{0.0,0.0}},
0.005,
0.0
];
size=nohighlightsize+addsize,
size=nohighlightsize
];
size
];

(*set color scheme*)
getbarseperator[medianin_,Nmedianin_]:=
Module[{median=medianin,Nmedian=Nmedianin,barseperator},
(*set bar seperator as {-N*median,...-median,median,...N*median} *)
barseperator=Table[{i*median,-i*median},{i,1,Nmedian}]//Flatten//Sort;
barseperator
];

getbarcolor[medianin_,Nmedianin_,colorschemein_]:=
Module[{median=medianin,Nmedian=Nmedianin,colorscheme=colorschemein,barseperator,barcolor,barmax,barmin},
barseperator=getbarseperator[median,Nmedian];
(*set max, min of data*)
barmax=Max[barseperator];
barmin=Min[barseperator];
(*set color between every bar seperator, color value is from (seperator[i]+seperator[i+1])/2 *)
barcolor=Table[ColorData[{colorscheme,{barmin,barmax}},(barseperator[[i]]+barseperator[[i+1]])/2.0 ],{i,1,Length[barseperator]-1}];
(*add tail color for outlayer data*)
barcolor=Insert[barcolor,Blue,1];
barcolor=Insert[barcolor,Red,-1];
barcolor
];

getbarcolor2[barseperatorin_,colorschemein_]:=
Module[{barseperator=barseperatorin,colorscheme=colorschemein,barcolor,barmax,barmin},

(*set max, min of data*)
barmax=Max[barseperator];
barmin=Min[barseperator];
(*set color between every bar seperator, color value is from (seperator[i]+seperator[i+1])/2 *)
barcolor=Table[ColorData[{colorscheme,{barmin,barmax}},(barseperator[[i]]+barseperator[[i+1]])/2.0 ],{i,1,Length[barseperator]-1}];
(*add tail color for outlayer data*)
barcolor=Insert[barcolor,Blue,1];
barcolor=Insert[barcolor,Red,-1];
barcolor
];
(*
Setbarlegend[medianin_,Nmedianin_,colorschemein_]:=
Module[{median=medianin,Nmedian=Nmedianin,colorscheme=colorschemein,barseperator,barmax,barmin,barcolor},
barseperator=getbarseperator[median,Nmedian];
barcolor=getbarcolor[median,Nmedian,colorscheme];

BarLegend[{barcolor,{-10000,10000}},(*{2*barmin,barseperator,2*barmax}//Flatten*)barseperator]
];
*)
Setbarlegend[barseperatorin_,barcolorin_]:=
Module[{barseperator=barseperatorin,barcolor=barcolorin},

If[Length[barcolor]!=Length[barseperator]+1,Print["error, Ncolor != Nseperator+1"] ];

BarLegend[{barcolor,{-10000,10000}},(*{2*barmin,barseperator,2*barmax}//Flatten*)barseperator]
];

Setbarcolorfunc[barcolorin_,barseperatorin_,valuein_]:=
Module[{barcolor=barcolorin,barseperator=barseperatorin,value=valuein,output},
output={};
(*c1|s1|c2|s2|...|sN-1|cN*)
Do[
If[value>=barseperator[[i]],output=barcolor[[i+1]] ],
{i,1,Length[barseperator]}
];

If[value<barseperator[[1]],output=barcolor[[1]] ];
output
];

(*for PDFCorrelationplot6*)
Setbarcolorfunc2[barcolorin_,barseperatorin_,valuein_]:=
Module[{barcolor=barcolorin,barseperator=barseperatorin,value=valuein,output},
output={};
(*|s1|c1|s2|...|cN-1|sN|*)
Do[
If[value>=barseperator[[i]],output=barcolor[[i]] ],
{i,1,Length[barseperator]-1}
];
(*value> max || value < min, give max color and min color*)
If[value<barseperator[[1]],output=barcolor[[1]] ];
If[value>=barseperator[[ -1 ]],output=barcolor[[-1]] ];
(*test*)
output
];


(* ::Input::Initialization:: *)
(*make exptname table jpg*)
(*input a List of string (exptnames), #row for every column and title for this table, output a Grid of string with #row*#column *)
(*20171114: add date mode for track, if datemode = "True", write the date, False, don't write the date*)
makeGrid2[strin_,rowsin_,titlein_,datemodein_]:=
Module[{str=strin,rows=rowsin,title=titlein,datemode=datemodein,columns,
lastcolstr,strout,datestring},
columns=Quotient[Length[str],rows];
strout=Table[str[[ic*rows+ir]],{ic,0,columns-1},{ir,1,rows}];
lastcolstr=Table[str[[i]],{i,columns*rows+1,Length[str]}];
strout=Append[strout,lastcolstr];
strout=Insert[strout,{Text[Style[title,FontSize->24] ],SpanFromLeft},1];
strout=Insert[strout,{"Data Sets"},2];
(*20171114: add date for track*)
If[
datemode==True,
datestring=DateString[{"Month","/","Day","/","Year"," ","Hour",":","Minute",":","Second"}];
strout=Insert[strout,{Text[Style[datestring,FontSize->10] ],SpanFromLeft},-1]
];
Grid[strout,ItemStyle->Directive[FontSize->18,Black],ItemSize->14,Alignment->Left]
];


(* ::Input:: *)
(*(*20170313:*)*)
(*PDFCorrelationplot6[datain_,titlein_,xtitlein_,ytitlein_,plotrangein_,stretchxin_,stretchyin_,barseperatorin_,legendlabelin_,epilogtextin_]:=*)
(*Module[{data=datain,title=titlein,xtitle=xtitlein,ytitle=ytitlein,plotrange=plotrangein,\[Alpha]x=stretchxin,\[Alpha]y=stretchyin,barseperator=barseperatorin,legendlabel=legendlabelin,epilogtext=epilogtextin,plotxQout,minx,maxx,miny,maxy,imgsize,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,plotrangetmp,mycolorscheme,psizebase,psizenorm,p,datalist,drange,barcolor,mybarlegend,barmin,barmax,textsize,outlayertext,*)
(*tickslist,tickslog,nolable,Loglable,xTicks,yTicks,p2,AllPlots},*)
(**)
(*(*default*)*)
(*minx=0.00001;*)
(*maxx=1;*)
(*miny=1;*)
(*maxy=1100;*)
(*imgsize={{600},{600}};*)
(*titlesize=24;*)
(*xtitlesize=18;*)
(*ytitlesize=18;*)
(*lgdlabelsize=12;*)
(*ticklablesize=18;*)
(**)
(*(*decide data max range in plot*)*)
(**)
(*datalist=datain/.LF1[a__]:>Abs[{a}[[3]] ];*)
(*drange=0.5+0.5*IntegerPart[Max[datalist]/0.5];*)
(**)
(*(*color scheme*)*)
(*(**)
(*mycolorscheme="AlpineColors";*)
(*mycolorscheme="LightTemperatureMap";*)
(**)*)
(*mycolorscheme="TemperatureMap";*)
(**)
(*barseperator=barseperator;*)
(*barmin=Min[barseperator];barmax=Max[barseperator];*)
(*barcolor=Table[ColorData[{mycolorscheme,{barmin,barmax}}, barmin+(i-0.5)*(barmax-barmin)/(Length[barseperator]-1)],{i,1,Length[barseperator]-1}];*)
(*barcolor=Darker[#,0.1]&/@barcolor;(*make color darker*)*)
(*(*lowlimit color is at the middle of barcolor*)*)
(*barcolor[[(Length[barcolor]+1)/2 ]]=(*ColorData["Atoms","ColorList"][[22]];*)(*ColorData[34,"ColorList"][[4]];*)ColorData[49,"ColorList"][[4]];*)
(*mybarlegend=BarLegend[{barcolor,{barmin,barmax}},barseperator,LegendLabel->legendlabel];*)
(**)
(*(*add text to plot by Epilog*)*)
(*textsize=16;*)
(*(*Npttext=Text[Style["Npt: "<>ToString[Length[data//Flatten] ],textsize,Black],Scaled[{0.1,0.9}] ];*)*)
(*outlayertext={epilogtext};*)
(**)
(*(*point size normalization*)*)
(*psizebase=0.005;*)
(*psizenorm=0.01;*)
(*p=2.0;(*20170307: make size small*)p=1.5;*)
(**)
(*(*if want to customize the plot range*)*)
(*plotrangetmp=ToString[plotrange];*)
(*If[*)
(*plotrangetmp!="None",*)
(*minx=plotrange[[1]];*)
(*maxx=plotrange[[2]];*)
(*miny=plotrange[[3]];*)
(*maxy=plotrange[[4]];*)
(*];*)
(**)
(*tickslist=Table[Table[j*10.0^i,{j,1,9}],{i,-6,6}]//Flatten;*)
(*tickslog=Table[Log[tickslist[[i]]  ],{i,1,11*9}];*)
(*nolable={"","","","","","","",""};*)
(*Loglable={"\!\(\*SuperscriptBox[\(10\), \(-6\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-5\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-4\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-3\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-2\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-1\)]\)",nolable,"1",nolable,"\!\(\*SuperscriptBox[\(10\), \(1\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(2\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(3\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(4\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(5\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(6\)]\)",nolable}//Flatten;*)
(*xTicks=Table[Table[tkl=If[j==1,{0.02,0.0},{0.01,0}];LF[tickslog[[(i-1)*9+j]],Loglable[[(i-1)*9+j]] ,tkl],{j,1,9}],{i,1,11}]//Flatten;*)
(*yTicks=Table[Table[tkl=If[j==1,{0.02,0.0},{0.01,0}];LF[tickslog[[(i-1)*9+j]],Loglable[[(i-1)*9+j]] ,tkl],{j,1,9}],{i,1,11}]//Flatten;*)
(**)
(*(*data structure: *)
(*{ pt1, pt2, pt3, pt4, ...}*)
(*ptn == LF1[ x1n, x2n, corr( obsA(x1n), obsB(x2n))]*)
(**)*)
(*p2=ListPlot[datain/.LF1[a__]:>Style[*)
(*{Log[{a}[[1]] ],Log[{a}[[2]] ]},*)
(*PointSize[(*psizebase+psizenorm*(Abs[{a}[[3]] ]/drange)*)Setpointsize[{a}[[3]],barmax,p] ],(*ColorData[{mycolorscheme,{-drange,drange} }][{a}[[3]]]*)*)
(*If[{a}[[3]]<barmax&&{a}[[3]]>barmin,Setbarcolorfunc2[barcolor,barseperator,{a}[[3]] ],If[{a}[[3]]>=barmax,Red,Blue](*outlayer*) ] *)
(*(*If[ {a}[[3]]<Max[barseperator]&&{a}[[3]]>Min[barseperator],Red,Black]*)*)
(*],*)
(*AspectRatio->1,*)
(*PlotRange->{{Log[minx],Log[maxx]},{Log[miny],Log[maxy]}},*)
(*PlotLegends->(*BarLegend[{mycolorscheme,{-drange,drange}}]*)mybarlegend,*)
(*PlotStyle->White,(*solve bug: system automatically set blue points in plot, \[Rule] set blue to white so that we don't see it*)*)
(*Epilog->{epilogtext,outlayertext}*)
(* ];*)
(**)
(*AllPlots=Show[*)
(*p2,*)
(*Frame->True,*)
(*FrameTicks->{xTicks/. LF->List,xTicks/. LF->List,*)
(*xTicks/. LF[a__]:>{{a}[[1]],""},xTicks/. LF[a__]:>{{a}[[1]],""}},*)
(*Axes->False,*)
(*PlotLabel->Style[title,titlesize],*)
(*FrameLabel->{Style[xtitle,xtitlesize],Style[ytitle,ytitlesize]},*)
(*ImageSize->imgsize,*)
(*AspectRatio->1*)
(*];*)
(**)
(*AllPlots*)
(* ];*)


(*20170309:using size function of highlight*)
(*
make plot of the data on x-Q plane,color of the point of the data depend on value of data at that point,data size,highlighted data will become larger

datain: {LF1[x,Q,value],LF1[x,Q,value],...}
titlein, xtitlein, ytitlein: titles of the figure
plotrangein:
stretchxin, stretchyin: 1 is normal,the two arguments will adjust the scale of x, y axis (stretch or squeeze the scale)
barseperatorin: List, seperate the color by values in it, ex: {0,0.5,0.7,1} seperate the {0,1} into 3 colors  
legendlabelin: user could add a label on the color bar, ex: legendlabelin="color by correlation"
epilogtextin: this argument allow user to setup epilog into the output figure, just input it as content of epilog in ListPlot
highlightrangein, unhighlightsizein: set size of point by unhighlightsizein and range of highlighted points, the function enlarge points in that range
ex: unhighlightsizein=Small;highlightrangein={0.5,1}
*)
(*
example:
PDFCorrelationplot7[pdfcorr[[flavourin+6]],title,xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,(*Append[epilogxQ,cuttext]*)epilogxQ,highlightrange,unhighlightsize ];
*)


PDFCorrelationplot7[datain_,titlein_,xtitlein_,ytitlein_,plotrangein_,stretchxin_,stretchyin_,barseperatorin_,legendlabelin_,epilogtextin_,highlightrangein_,unhighlightsizein_]:=
Module[{data=datain,title=titlein,xtitle=xtitlein,ytitle=ytitlein,plotrange=plotrangein,\[Alpha]x=stretchxin,\[Alpha]y=stretchyin,barseperator=barseperatorin,legendlabel=legendlabelin,epilogtext=epilogtextin,highlightrange=highlightrangein,unhighlightsize=unhighlightsizein,
plotxQout,minx,maxx,miny,maxy,imgsize,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,plotrangetmp,mycolorscheme,psizebase,psizenorm,p,datalist,drange,barcolor,mybarlegend,barmin,barmax,textsize,outlayertext,
tickslist,tickslog,nolable,Loglable,xTicks,yTicks,p2,AllPlots},

(*default*)
minx=0.00001;
maxx=1;
miny=1;
maxy=1100;
imgsize={{600},{600}};
titlesize=24;
xtitlesize=18;
ytitlesize=18;
lgdlabelsize=12;
ticklablesize=18;

(*decide data max range in plot*)

datalist=datain/.LF1[a__]:>Abs[{a}[[3]] ];
drange=0.5+0.5*IntegerPart[Max[datalist]/0.5];

(*color scheme*)
(*
mycolorscheme="AlpineColors";
mycolorscheme="LightTemperatureMap";
*)
mycolorscheme="TemperatureMap";

barseperator=barseperator;
barmin=Min[barseperator];barmax=Max[barseperator];
barcolor=Table[ColorData[{mycolorscheme,{barmin,barmax}}, barmin+(i-0.5)*(barmax-barmin)/(Length[barseperator]-1)],{i,1,Length[barseperator]-1}];
barcolor=Darker[#,0.2]&/@barcolor;(*make color darker*)
(*lowlimit color is at the middle of barcolor*)
barcolor[[(Length[barcolor]+1)/2 ]]=(*ColorData["Atoms","ColorList"][[22]];*)(*ColorData[34,"ColorList"][[4]];*)(*ColorData[49,"ColorList"][[4]];*)ColorData[30,"ColorList"][[5]];
(*make small value data unvisible*)
barcolor[[(Length[barcolor]+1)/2 ]]=Lighter[barcolor[[(Length[barcolor]+1)/2 ]],0.5];

mybarlegend=BarLegend[{barcolor,{barmin,barmax}},barseperator,LegendLabel->legendlabel,LabelStyle->{FontSize->14,Black}];

(*add text to plot by Epilog*)
textsize=16;
(*Npttext=Text[Style["Npt: "<>ToString[Length[data//Flatten] ],textsize,Black],Scaled[{0.1,0.9}] ];*)
outlayertext={epilogtext};

(*point size normalization*)
psizebase=0.005;
psizenorm=0.01;
p=2.0;(*20170307: make size small*)p=1.5;

(*if want to customize the plot range*)
plotrangetmp=ToString[plotrange];
If[
plotrangetmp!="None",
minx=plotrange[[1]];
maxx=plotrange[[2]];
miny=plotrange[[3]];
maxy=plotrange[[4]];
];


tickslist=Table[Table[j*10.0^i,{j,1,9}],{i,-6,6}]//Flatten;
tickslog=Table[Log[tickslist[[i]]  ],{i,1,11*9}];
nolable={"","","","","","","",""};
Loglable={"\!\(\*SuperscriptBox[\(10\), \(-6\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-5\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-4\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-3\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-2\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-1\)]\)",nolable,"1",nolable,"\!\(\*SuperscriptBox[\(10\), \(1\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(2\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(3\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(4\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(5\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(6\)]\)",nolable}//Flatten;
xTicks=Table[Table[tkl=If[j==1,{0.02,0.0},{0.01,0}];LF[tickslog[[(i-1)*9+j]],Loglable[[(i-1)*9+j]] ,tkl],{j,1,9}],{i,1,11}]//Flatten;
yTicks=Table[Table[tkl=If[j==1,{0.02,0.0},{0.01,0}];LF[tickslog[[(i-1)*9+j]],Loglable[[(i-1)*9+j]] ,tkl],{j,1,9}],{i,1,11}]//Flatten;


(*data structure: 
{ pt1, pt2, pt3, pt4, ...}
ptn == LF1[ x1n, x2n, corr( obsA(x1n), obsB(x2n))]
*)
p2=ListPlot[datain/.LF1[a__]:>Style[
{Log[{a}[[1]] ],Log[{a}[[2]] ]},
PointSize[(*psizebase+psizenorm*(Abs[{a}[[3]] ]/drange)*)Setpointsize2[{a}[[3]],highlightrange,unhighlightsize] ],(*ColorData[{mycolorscheme,{-drange,drange} }][{a}[[3]]]*)
If[{a}[[3]]<barmax&&{a}[[3]]>barmin,Setbarcolorfunc2[barcolor,barseperator,{a}[[3]] ],If[{a}[[3]]>=barmax,Red,Blue](*outlayer*) ] 
(*If[ {a}[[3]]<Max[barseperator]&&{a}[[3]]>Min[barseperator],Red,Black]*)
],
AspectRatio->1,
PlotRange->{{Log[minx],Log[maxx]},{Log[miny],Log[maxy]}},
PlotLegends->(*BarLegend[{mycolorscheme,{-drange,drange}}]*)Placed[mybarlegend,{1.0,0.5}],
PlotStyle->White,(*solve bug: system automatically set blue points in plot, \[Rule] set blue to white so that we don't see it*)
Epilog->{epilogtext,outlayertext}
 ];

AllPlots=Show[
p2,
Frame->True,
FrameTicks->{xTicks/. LF->List,xTicks/. LF->List,
xTicks/. LF[a__]:>{{a}[[1]],""},xTicks/. LF[a__]:>{{a}[[1]],""}},
Axes->False,
PlotLabel->Style[title,titlesize],
FrameLabel->{Style[xtitle,xtitlesize],Style[ytitle,ytitlesize]},
ImageSize->imgsize,
AspectRatio->1
];

AllPlots
 ];
 
 
 
 


(*20171105: functionize barlegend generating*)
(*input seperator should be a 8 element List and elements order from small to large, e.g: {-1,-0.85,-0.7,-0.5,0.5,0.7,0.85,1}*)
(*output a 7 colors palette (blue to red)*)
Set2DxqBarLegend[barseperatorin_,legendlabelin_(*,highlightrangein_*)]:=
Module[{barseperator=barseperatorin,legendlabel=legendlabelin,(*highlightrange=highlightrangein,*)
mycolorscheme,barmin,barmax,barcolor,mybarlegend,greycolor},

(*20171108: check #seperator elements is correct*)
If[Length[barseperator]!=8,Print["error, the input color discrete list should include 8 elements, yours: ",barseperator] ];

mycolorscheme="TemperatureMap";
greycolor=Lighter[ColorData[30,"ColorList"][[5]],0.5];

barmin=Min[barseperator];barmax=Max[barseperator];
(*
barcolor=Table[ColorData[{mycolorscheme,{barmin,barmax}}, barmin+(i-0.5)*(barmax-barmin)/(Length[barseperator]-1)],{i,1,Length[barseperator]-1}];
barcolor=Darker[#,0.2]&/@barcolor;(*make color darker*)
(*lowlimit color is at the middle of barcolor*)
barcolor[[(Length[barcolor]+1)/2 ]]=(*ColorData["Atoms","ColorList"][[22]];*)(*ColorData[34,"ColorList"][[4]];*)(*ColorData[49,"ColorList"][[4]];*)ColorData[30,"ColorList"][[5]];
(*make small value data unvisible*)
barcolor[[(Length[barcolor]+1)/2 ]]=Lighter[barcolor[[(Length[barcolor]+1)/2 ]],0.5];
(*20170528: customize my color scheme*)
barcolor={RGBColor[0.,0.,1.0],RGBColor[0.,0.5,1.],RGBColor[0.0,1.,1.],RGBColor[1.,1.,1.],RGBColor[1.,0.8,0.],RGBColor[1.0,0.4,0.],RGBColor[1.0,0.,0.]};
barcolor=Darker[barcolor,0.3];
*)
(*20170606: brighter ogrange and yellow*)(*20171108 in the middle = Green*)
barcolor={RGBColor[0.,0.,0.7],RGBColor[0.,0.5,0.75],RGBColor[0.0,0.75,0.75],RGBColor[0,0.7,0],RGBColor[0.75,0.75,0.],RGBColor[0.75,0.55,0.],RGBColor[0.7,0.,0.]};
(*
barcolor[[(Length[barcolor]+1)/2 ]]=greycolor;
*)

mybarlegend=BarLegend[{barcolor,{barmin,barmax}},barseperator,LegendLabel->legendlabel,LabelStyle->{FontSize->14,Black}];
mybarlegend

];
(*20171108: set an only red spectrum color palette*)
(*input seperator should be a 5 element List and elements order from small to large, e.g: {0,0.5,0.7,0.85,1}*)
(*output a 4 colors palette (green to red)*)
Set2DxqRedBarLegend[barseperatorin_,legendlabelin_(*,highlightrangein_*)]:=
Module[{barseperator=barseperatorin,legendlabel=legendlabelin,(*highlightrange=highlightrangein,*)
mycolorscheme,barmin,barmax,barcolor,mybarlegend,greycolor},

(*20171108: check #seperator elements is correct*)
If[Length[barseperator]!=5,Print["error, the input color discrete list should include 5 elements, yours: ",barseperator] ];

mycolorscheme="TemperatureMap";
greycolor=Lighter[ColorData[30,"ColorList"][[5]],0.5];

barmin=Min[barseperator];barmax=Max[barseperator];
(*
barcolor=Table[ColorData[{mycolorscheme,{barmin,barmax}}, barmin+(i-0.5)*(barmax-barmin)/(Length[barseperator]-1)],{i,1,Length[barseperator]-1}];
barcolor=Darker[#,0.2]&/@barcolor;(*make color darker*)
(*lowlimit color is at the middle of barcolor*)
barcolor[[(Length[barcolor]+1)/2 ]]=(*ColorData["Atoms","ColorList"][[22]];*)(*ColorData[34,"ColorList"][[4]];*)(*ColorData[49,"ColorList"][[4]];*)ColorData[30,"ColorList"][[5]];
(*make small value data unvisible*)
barcolor[[(Length[barcolor]+1)/2 ]]=Lighter[barcolor[[(Length[barcolor]+1)/2 ]],0.5];
(*20170528: customize my color scheme*)
barcolor={RGBColor[0.,0.,1.0],RGBColor[0.,0.5,1.],RGBColor[0.0,1.,1.],RGBColor[1.,1.,1.],RGBColor[1.,0.8,0.],RGBColor[1.0,0.4,0.],RGBColor[1.0,0.,0.]};
barcolor=Darker[barcolor,0.3];
*)
(*20170606: brighter ogrange and yellow*)(*20171108 in the middle = Green*)
barcolor={RGBColor[0,0.7,0],RGBColor[0.75,0.75,0.],RGBColor[0.75,0.55,0.],RGBColor[0.7,0.,0.]};
(*
barcolor[[(Length[barcolor]+1)/2 ]]=greycolor;
*)

mybarlegend=BarLegend[{barcolor,{barmin,barmax}},barseperator,LegendLabel->legendlabel,LabelStyle->{FontSize->14,Black}];
mybarlegend

];

Set2DxqHighlightBarLegend[legendlabelin_,highlightrangein_]:=
Module[{barseperator,legendlabel=legendlabelin,highlightrange=highlightrangein,
mycolorscheme,barmin,barmax,barcolor,mybarlegend,greycolor,
positivedivid,negativedivid},

greycolor=Lighter[ColorData[30,"ColorList"][[5]],0.5];

If[highlightrange[[1]]<0.0 || highlightrange[[2]]<highlightrange[[1]],Print["error, highlight range should be {min, max} and min should >= 0"];Quit[] ];
(*barseperator={highlightrange[[1]],,,highlightrange[[2]]};*)(*20171114: for warning message*)
(*set bar the seperator in highlight range*)
positivedivid=Table[highlightrange[[1]]+(i-1)*(highlightrange[[2]]-highlightrange[[1]])/3,{i,1,4}];
negativedivid=(-positivedivid);
negativedivid=Reverse[negativedivid];
barseperator=Join[{-10^6},negativedivid,positivedivid,{10^6}];
(*set bar color, outside of highlight range should be grey*)
barcolor={greycolor,RGBColor[0.,0.,0.7],RGBColor[0.,0.5,0.75],RGBColor[0.0,0.75,0.75],greycolor,RGBColor[0.75,0.75,0.],RGBColor[0.75,0.55,0.],RGBColor[0.7,0.,0.],greycolor};
(*set barlegend*)
barmin=Min[barseperator];barmax=Max[barseperator];
mybarlegend=BarLegend[{barcolor,{barmin,barmax}},barseperator,LegendLabel->legendlabel,LabelStyle->{FontSize->14,Black}];

mybarlegend
]

(*20170514: test new version with shape of point could be choosed*)
(*20171105: add barcolor*)
 PDFCorrelationplot8[datain_,titlein_,xtitlein_,ytitlein_,plotrangein_,stretchxin_,stretchyin_,barlegendin_,(*barseperatorin_,barcolorin_,legendlabelin_,*)epilogtextin_,highlightrangein_,unhighlightsizein_]:=
Module[{data=datain,title=titlein,xtitle=xtitlein,ytitle=ytitlein,plotrange=plotrangein,\[Alpha]x=stretchxin,\[Alpha]y=stretchyin,barlegend=barlegendin(*barseperator=barseperatorin,legendlabel=legendlabelin*),epilogtext=epilogtextin,highlightrange=highlightrangein,unhighlightsize=unhighlightsizein,
plotxQout,minx,maxx,miny,maxy,imgsize,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,plotrangetmp,mycolorscheme,psizebase,psizenorm,p,datalist,drange,barcolor,mybarlegend,barmin,barmax,textsize,outlayertext,
tickslist,tickslog,nolable,Loglable,xTicks,yTicks,p2,AllPlots,
ptshape,ptshaperescale,
barseperatordefault,barcolordefault,barseperator,
dataordering},

(*default*)
minx=0.00001;
maxx=1;
miny=1;
maxy=1100;
imgsize={{600},{600}};
titlesize=24;
xtitlesize=18;
ytitlesize=18;
lgdlabelsize=12;
ticklablesize=18;
(*20171105 set default barlegend*)
barseperatordefault={-1,-0.85,-0.7,-0.5,0.5,0.7,0.85,1};
barcolordefault={RGBColor[0.,0.,0.7],RGBColor[0.,0.5,0.75],RGBColor[0.0,0.75,0.75],RGBColor[0.7,0.7,0.7],RGBColor[0.75,0.75,0.],RGBColor[0.75,0.55,0.],RGBColor[0.7,0.,0.]};

(*decide data max range in plot*)

datalist=datain/.LF1[a__]:>Abs[{a}[[3]] ];
drange=0.5+0.5*IntegerPart[Max[datalist]/0.5];

(*color scheme*)
(*
mycolorscheme="AlpineColors";
mycolorscheme="LightTemperatureMap";
*)


(*
mycolorscheme="TemperatureMap";

barseperator=barseperator;
barmin=Min[barseperator];barmax=Max[barseperator];
barcolor=Table[ColorData[{mycolorscheme,{barmin,barmax}}, barmin+(i-0.5)*(barmax-barmin)/(Length[barseperator]-1)],{i,1,Length[barseperator]-1}];
barcolor=Darker[#,0.2]&/@barcolor;(*make color darker*)
(*lowlimit color is at the middle of barcolor*)
barcolor[[(Length[barcolor]+1)/2 ]]=(*ColorData["Atoms","ColorList"][[22]];*)(*ColorData[34,"ColorList"][[4]];*)(*ColorData[49,"ColorList"][[4]];*)ColorData[30,"ColorList"][[5]];
(*make small value data unvisible*)
barcolor[[(Length[barcolor]+1)/2 ]]=Lighter[barcolor[[(Length[barcolor]+1)/2 ]],0.5];
(*20170528: customize my color scheme*)
barcolor={RGBColor[0.,0.,1.0],RGBColor[0.,0.5,1.],RGBColor[0.0,1.,1.],RGBColor[1.,1.,1.],RGBColor[1.,0.8,0.],RGBColor[1.0,0.4,0.],RGBColor[1.0,0.,0.]};
barcolor=Darker[barcolor,0.3];
*)
(*20170606: brighter ogrange and yellow*)
(*
barcolor={RGBColor[0.,0.,0.7],RGBColor[0.,0.5,0.75],RGBColor[0.0,0.75,0.75],RGBColor[0.7,0.7,0.7],RGBColor[0.75,0.75,0.],RGBColor[0.75,0.55,0.],RGBColor[0.7,0.,0.]};
barcolor[[(Length[barcolor]+1)/2 ]]=Lighter[ColorData[30,"ColorList"][[5]],0.5];
*)

(*20171105: set bar default color and seperator*)
(*
If[barseperator\[Equal]"default",barseperator=barseperatordefault];
If[barcolor\[Equal]"default",barcolor=barcolordefault];
If[Length[barseperator]\[NotEqual](Length[barcolor]+1),Print["error, # of colors is not # of spaces -1,: {#color, #space}",Length[barcolor]," ",Length[barseperator] ];Quit[] ];
*)
If[
(Head[barlegend]!=BarLegend) && barlegend!="default",
Print["error, barlegend input should be a BarLegend or \"default\""];Quit[]
];
If[
barlegend=="default",
barseperator=barseperatordefault;
barcolor=barcolordefault;
barmin=Min[barseperator];barmax=Max[barseperator];
mybarlegend=BarLegend[{barcolordefault,{barmin,barmax}},barseperatordefault,LegendLabel->"",LabelStyle->{FontSize->14,Black}];
];
If[
(Head[barlegend]==BarLegend),
mybarlegend=barlegend;
(*for determining color, we need to set up barmax, barmin*)
barseperator=barlegend[[2]];
barcolor=barlegend[[1,1]];
barmin=Min[barseperator];barmax=Max[barseperator]
];
(*
Print["output of barlegend:"];
Print[InputForm[barlegend]];
Print["seperator ",barseperator];
Print["color ",barcolor];
*)

(*add text to plot by Epilog*)
textsize=16;
(*Npttext=Text[Style["Npt: "<>ToString[Length[data//Flatten] ],textsize,Black],Scaled[{0.1,0.9}] ];*)
outlayertext={epilogtext};

(*point size normalization*)
psizebase=0.005;
psizenorm=0.01;
p=2.0;(*20170307: make size small*)p=1.5;

(*if want to customize the plot range*)
plotrangetmp=ToString[plotrange];
If[
plotrangetmp!="None",
minx=plotrange[[1]];
maxx=plotrange[[2]];
miny=plotrange[[3]];
maxy=plotrange[[4]];
];


tickslist=Table[Table[j*10.0^i,{j,1,9}],{i,-6,6}]//Flatten;
tickslog=Table[Log[tickslist[[i]]  ],{i,1,11*9}];
nolable={"","","","","","","",""};
Loglable={"\!\(\*SuperscriptBox[\(10\), \(-6\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-5\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-4\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-3\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-2\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(-1\)]\)",nolable,"1",nolable,"\!\(\*SuperscriptBox[\(10\), \(1\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(2\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(3\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(4\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(5\)]\)",nolable,"\!\(\*SuperscriptBox[\(10\), \(6\)]\)",nolable}//Flatten;
xTicks=Table[Table[tkl=If[j==1,{0.02,0.0},{0.01,0}];LF[tickslog[[(i-1)*9+j]],Loglable[[(i-1)*9+j]] ,tkl],{j,1,9}],{i,1,11}]//Flatten;
yTicks=Table[Table[tkl=If[j==1,{0.02,0.0},{0.01,0}];LF[tickslog[[(i-1)*9+j]],Loglable[[(i-1)*9+j]] ,tkl],{j,1,9}],{i,1,11}]//Flatten;

(*test print*)
(*
Print[(datain/.LF1[a__]\[RuleDelayed]ToString[{
Setpointsize2[{a}[[3]],highlightrange,unhighlightsize],
If[{a}[[3]]<barmax&&{a}[[3]]>barmin,Setbarcolorfunc2[barcolor,barseperator,{a}[[3]] ],If[{a}[[3]]>=barmax,Red,Blue](*outlayer*) ],
{a}[[3]],
highlightrange}]<>"\n") ];
*)

(*data structure: 
{ pt1, pt2, pt3, pt4, ...}
ptn == LF1[ x1n, x2n, corr( obsA(x1n), obsB(x2n))]
*)
p2=ListPlot[
{{0.00001,2},{0.99,1200.0}}
(*
datain/.LF1[a__]:>Style[
{Log[{a}[[1]] ],Log[{a}[[2]] ]},
PointSize[(*psizebase+psizenorm*(Abs[{a}[[3]] ]/drange)*)Setpointsize2[{a}[[3]],highlightrange,unhighlightsize] ],(*ColorData[{mycolorscheme,{-drange,drange} }][{a}[[3]]]*)
If[{a}[[3]]<barmax&&{a}[[3]]>barmin,Setbarcolorfunc2[barcolor,barseperator,{a}[[3]] ],If[{a}[[3]]>=barmax,Red,Blue](*outlayer*) ] 
(*If[ {a}[[3]]<Max[barseperator]&&{a}[[3]]>Min[barseperator],Red,Black]*)
]*),
AspectRatio->1,
PlotRange->{{Log[minx],Log[maxx]},{Log[miny],Log[maxy]}},
PlotLegends->(*BarLegend[{mycolorscheme,{-drange,drange}}]*)Placed[mybarlegend,{1.0,0.5}],
PlotStyle->White,(*solve bug: system automatically set blue points in plot, \[Rule] set blue to white so that we don't see it*)
Epilog->{epilogtext,outlayertext}
 ];

ptshape={"\[FilledCircle]",(*"*"*)(*"x"*)"\[EmptySquare]","\[FilledUpTriangle]","\[FilledDownTriangle]","\[EmptyUpTriangle]","\[EmptyDownTriangle]","\[EmptyCircle]"};
ptshaperescale={750.0,2000.0,850.0,800.0,750.0};
{{"\[FilledCircle]",8.96`},{"\[FilledSquare]",8.96`},{"\[FilledDiamond]",10.88`},{"\[FilledUpTriangle]",10.24`},{"\[FilledDownTriangle]",10.24`},{"\[EmptyCircle]",10.24`},{"\[EmptySquare]",10.24`},{"\[EmptyDiamond]",10.24`},{"\[EmptyUpTriangle]",11.136`},{"\[EmptyDownTriangle]",11.136`}};

(*20171126: use marker list*)
{ptshape,ptshaperescale}=PlotMarkerList[];
(*ptshaperescale=0.0075*ptshaperescale;*)

(*20171126: for multi shape plotting, we need to sort data in all groups togather, otherwise the last group members with small values still cover the large values*)
dataordering=Ordering[Flatten[datain]/.LF1[a__]:>Abs[{a}[[3]] ] ];

AllPlots=Show[
{
p2,
Graphics[
(*20171126: sort all group members by ordering of Abs[values] (using dataordering)*)
(
Table[
(*20170530: datain[[igroup]] \[Rule] sort from small value to large value so that deep color points would not be covered by light color points*)
(*Sort[datain[[igroup]],Abs[#2[[3]] ]>Abs[#1[[3]] ]&]*)datain[[igroup]]/.LF1[a__]:>Style[
Text[ptshape[[igroup]],{Log[{a}[[1]] ],Log[{a}[[2]] ]}],
(*20171108: if highlighted, medium, unhighlighted, small
(*PointSize[*)(*psizebase+psizenorm*(Abs[{a}[[3]] ]/drange)*)ptshaperescale[[igroup]]*Setpointsize2[{a}[[3]],highlightrange,unhighlightsize] (*]*),(*ColorData[{mycolorscheme,{-drange,drange} }][{a}[[3]]]*)
*)
(*20171109: use Setpointsize4 to satisfy the format of new highlight range*)
ptshaperescale[[igroup]]*(*Setpointsize3*)Setpointsize4[{a}[[3]],highlightrange,unhighlightsize],
(*20171108 if highllighted, grey out, otherwise use colors in the palette*)
If
[
(*
({a}[[3]]>=highlightrange[[1]] && {a}[[3]]<highlightrange[[2]]) || ({a}[[3]]\[GreaterEqual](-highlightrange[[2]]) && {a}[[3]]<(-highlightrange[[1]]) )
*)
(*20171109: for new convention of highlight range: if a point is in any highlight range, give color on the palette to it, otherwise give gray color*) 
(*20171114: fix the bug: when highlight mode = 0 (no highlight), also give points color by their color palette*)         
Intersection[({a}[[3]]>=#[[1]] && {a}[[3]]<#[[2]])&/@highlightrange,{True}]=={True} || highlightrange=={{0.0,0.0}},
(*If[ {a}[[3]]<Max[barseperator]&&{a}[[3]]>Min[barseperator],Red,Black]*)
If[{a}[[3]]<barmax&&{a}[[3]]>barmin,Setbarcolorfunc2[barcolor,barseperator,{a}[[3]] ],If[{a}[[3]]>=barmax,Red,Blue](*outlayer*) ],
(*Gray*)RGBColor[0.6,0.6,0.6]
]
],
{igroup,1,Length[datain]}]//Flatten
)[[dataordering]]
]
},
Frame->True,
FrameTicks->{xTicks/. LF->List,xTicks/. LF->List,
xTicks/. LF[a__]:>{{a}[[1]],""},xTicks/. LF[a__]:>{{a}[[1]],""}},
Axes->False,
PlotLabel->Style[title,titlesize,Black],
FrameLabel->{Style[xtitle,xtitlesize,Black],Style[ytitle,ytitlesize,Black]},
FrameTicksStyle->Black,
ImageSize->imgsize,
AspectRatio->1.0,
(*20171125: add grid lines*)
GridLines->Log[{{0.0000001,0.000001,0.00001,0.0001,0.001,0.01,0.1},{10,50,100,500,1000,5000,10000}}],
GridLinesStyle->Directive[Dashed]
];

AllPlots
 ];
 


(* ::Subsection:: *)
(*histogram: histsetting class*)


(* ::Input::Initialization:: *)
(*input: lineelement={{xvalue,text,color},...} *)
setxlineinplot2[datain_,lineelementin_,yrangein_]:=
Module[{data=datain,lineelement=lineelementin,yrange=yrangein,x,text,color,textsize,lineout,textout,ymin,ymax,output},
ymin=yrange[[1]];
ymax=yrange[[2]];

output=
Table[
x=lineelementin[[i,1]];
text=lineelementin[[i,2]];
color=lineelementin[[i,3]];
textsize=16;
lineout=Style[Line[{{x,ymin},{x,0.95*ymax} }],color];
textout=Text[Style[text,textsize,color],{x,1.025*ymax-(*20170306, solve overlap of text*)0.04*i*ymax} ];
{textout,lineout}
,{i,1,Length[lineelement]}];
(*
output={Style[Line[{{1,1},{1,4}}],Green],Style["a2",Green]}
*)
output
]


(* ::Input::Initialization:: *)
(*input: lineelement={{xvalue,text,color},...} *)
setxlineinplot3[lineelementin_,yrangein_]:=
Module[{lineelement=lineelementin,yrange=yrangein,x,text,color,textsize,lineout,textout,ymin,ymax,output},
ymin=yrange[[1]];
ymax=yrange[[2]];

output=
Table[
x=lineelementin[[i,1]];
text=lineelementin[[i,2]];
color=lineelementin[[i,3]];
textsize=16;
lineout=Style[Line[{{x,ymin},{x,0.95*ymax} }],color];
textout=Text[Style[text,textsize,color],{x,1.025*ymax-(*20170306, solve overlap of text*)0.04*i*ymax} ];
{textout,lineout}
,{i,1,Length[lineelement]}];
(*
output={Style[Line[{{1,1},{1,4}}],Green],Style["a2",Green]}
*)
output
]


(* ::Input:: *)
(*(*plot statistics of correlation data line*)*)
(*(*input*)
(*histlistin: data, {LF1[x,Q,corr],...,...}*)
(*titlein,xtitlein,ytitlein: title of plot*)
(*binsetin: bin of histogram, {tick1, tick2, tick3,...}*)
(*lineelementin: text with line you want to add in plot: {{xvalue,"text",color},{...},...}*)
(*plotrangexin: {xmin,xmax}*)
(**)*)
(*histplot3[histlistin_,titlein_,xtitlein_,ytitlein_,binsetin_,lineelementin_,plotrangexin_,Nbinin_]:=*)
(*Module[{histlist=histlistin,title=titlein,xtitle=xtitlein,ytitle=ytitlein,binset=binsetin,lineelement=lineelementin,*)
(*plotrangex=plotrangexin,Nbin=Nbinin,*)
(*xmin,xmax,ymin,ymax,binset2,textsize,Npttext,imgsize,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,avgw,p2,AllPlots},*)
(**)
(*(*default*)*)
(*imgsize={{600},{600}};*)
(*titlesize=24;*)
(*xtitlesize=18;*)
(*ytitlesize=18;*)
(*lgdlabelsize=12;*)
(*ticklablesize=18;*)
(**)
(*(*subNbin=5;*)*)
(*binset2={Table[binset[[i-1]]+(j/5)*(binset[[i]]-binset[[i-1]]),{i,2,Length[binset]},{j,0,5-1}]//Flatten};*)
(*binset2[[1]]=Append[binset2[[1]],binset[[-1]] ];*)
(*(*add some bins on the head and tail*)*)
(*avgw=(binset[[-1]]-binset[[1]])/(5*(Length[binset]-1));*)
(*binset2[[1]]=Join[binset2[[1]],Table[binset[[-1]]+i*avgw,{i,1,30}] ];*)
(*binset2[[1]]=Join[Table[binset[[1]]-i*avgw,{i,30,1,-1}],binset2[[1]] ];*)
(*(*Print["bin set: ",binset2];*)*)
(*(*binset2=Append[binset2[[1]],1000];*)(*add a very large number*)*)
(**)
(*(*set xy range by max[data] and the most high bin data*)*)
(*xmin=Min[histlist,0.0];xmax=Max[histlist];*)
(*xmax=xmax+0.1*(xmax-xmin);*)
(*(*test*)(*Print["binset2: ",binset2];*)*)
(*ymin=0.0;ymax=Max[HistogramList[histlist,binset2][[2]] ];*)
(**)
(*(*set line of median and mean, and texts*)*)
(*textsize=16;*)
(**)
(*Npttext=Text[Style["Npt: "<>ToString[Length[histlist//Flatten] ],textsize,Black],Scaled[{0.1,0.9}] ];*)
(*(**)
(*Print[setxlineinplot2[histlist,lineelement,{ymin,ymax}] ];*)
(*testeplog=Style[Line[{{3,3},{3,5}}],Blue];*)
(**)*)
(**)
(*(*output histogram*)*)
(*p2=*)
(*Histogram[histlist,(*{xmin,xmax,w}*)binset2,*)
(*Epilog->{setxlineinplot2[histlist,lineelement,{ymin,ymax}],Npttext},*)
(*PlotRange->{{plotrangex[[1]],plotrangex[[2]]},{ymin,ymax}}*)
(*];*)
(**)
(*(*20170307 set Nbin by argument if not auto mode*)*)
(*If[Nbin!="auto",*)
(*p2=*)
(*Histogram[histlist,(*{xmin,xmax,w}*)Nbin,*)
(*Epilog->{setxlineinplot2[histlist,lineelement,{ymin,ymax}],Npttext},*)
(*PlotRange->{{plotrangex[[1]],plotrangex[[2]]},{ymin,ymax}}*)
(*];*)
(*Print["Nbin is ",Nbin]*)
(*];*)
(**)
(*(**)
(*titlesize=16;*)
(*xtitlesize=16;*)
(*ytitlesize=16;*)
(*imgsize={{450},{450}};*)
(*title="title";xtitle="x";ytitle="y";*)
(**)*)
(*AllPlots=Show[*)
(*p2,*)
(*BaseStyle->{FontSize->16},*)
(*Frame->True,*)
(*(**)
(*FrameTicks\[Rule]{xTicks/.LF\[Rule]List,xTicks/.LF\[Rule]List,*)
(*xTicks/.LF[a__]\[RuleDelayed]{{a}\[LeftDoubleBracket]1\[RightDoubleBracket],""},xTicks/.LF[a__]\[RuleDelayed]{{a}\[LeftDoubleBracket]1\[RightDoubleBracket],""}},*)
(**)*)
(*Axes->False,*)
(*PlotLabel->Style[title,titlesize],*)
(*FrameLabel->{Style[xtitle,xtitlesize],Style[ytitle,ytitlesize]},*)
(*ImageSize->imgsize,*)
(*AspectRatio->1*)
(*];*)
(**)
(*AllPlots*)
(*(**)
(*{median,binset2,ymax,xmax}*)
(**)*)
(*];*)
(**)
(**)


(*plot statistics of correlation data line*)
(*input
histlistin: data, {value1, value2,...,...}
titlein,xtitlein,ytitlein: title of plot
Nbinin: # of bins in figure, when is set as "auto", the bin is defined by binsetin
binsetin: bin of histogram, {tick1, tick2, tick3,...}, used when Nbin="auto"
lineelementin: text with line you want to add in plot: {{xvalue,"text",color},{...},...}
plotrangexin: {xmin,xmax}
*)
(*example*)
(*histplot4[{1,2,3,4,2,3},"title","corr","#counts",{0,0.5,1.0,2,3},{{0.75,"red line",Red},{0,3.0},"auto"];*)
histplot4[histlistin_,titlein_,xtitlein_,ytitlein_,binsetin_,lineelementin_,plotrangexin_,Nbinin_]:=
Module[{histlist=histlistin,title=titlein,xtitle=xtitlein,ytitle=ytitlein,binset=binsetin,lineelement=lineelementin,
plotrangex=plotrangexin,Nbin=Nbinin,
xmin,xmax,ymin,ymax,binset2,textsize,Npttext,imgsize,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,avgw,p2,AllPlots},

(*default*)
imgsize={{600},{600}};
titlesize=24;
xtitlesize=18;
ytitlesize=18;
lgdlabelsize=12;
ticklablesize=18;

(*subNbin=5;*)
(*
binset2={Table[binset[[i-1]]+(j/5)*(binset[[i]]-binset[[i-1]]),{i,2,Length[binset]},{j,0,5-1}]//Flatten};
binset2[[1]]=Append[binset2[[1]],binset[[-1]] ];
(*add some bins on the head and tail*)
avgw=(binset[[-1]]-binset[[1]])/(5*(Length[binset]-1));
binset2[[1]]=Join[binset2[[1]],Table[binset[[-1]]+i*avgw,{i,1,30}] ];
binset2[[1]]=Join[Table[binset[[1]]-i*avgw,{i,30,1,-1}],binset2[[1]] ];
*)

(*if set Nbin, then use Nbin setting*)
If[Nbin=="auto",binset2=binset ];
If[Nbin!="auto",binset2={Table[plotrangex[[1]]+i*((plotrangex[[2]]-plotrangex[[1]])/Nbin),{i,0,Nbin}]} ];
(*Print[plotrangex,binset2];*)
(*Print["bin set: ",binset2];*)
(*binset2=Append[binset2[[1]],1000];*)(*add a very large number*)

(*set xy range by max[data] and the most high bin data*)
xmin=Min[histlist,0.0];xmax=Max[histlist];
xmax=xmax+0.1*(xmax-xmin);

(*test*)(*Print["binset2: ",binset2];*)
ymin=0.0;ymax=Max[HistogramList[histlist,binset2][[2]] ];
(*Print[HistogramList[histlist,binset2,xmin,xmax] ];*)

(*set line of median and mean, and texts*)
textsize=16;

Npttext=Text[Style["Npt: "<>ToString[Length[histlist//Flatten] ],textsize,Black],Scaled[{0.1,0.9}] ];
(*
Print[setxlineinplot2[histlist,lineelement,{ymin,ymax}] ];
testeplog=Style[Line[{{3,3},{3,5}}],Blue];
*)

(*output histogram*)
p2=
Histogram[histlist,(*{xmin,xmax,w}*)binset2,
Epilog->{setxlineinplot2[histlist,lineelement,{ymin,ymax}],Npttext},
PlotRange->{{plotrangex[[1]],plotrangex[[2]]},{ymin,ymax}}
];

(*20170307 set Nbin by argument if not auto mode*)
(*
If[Nbin\[NotEqual]"auto",
p2=
Histogram[histlist,(*{xmin,xmax,w}*)Nbin,
Epilog\[Rule]{setxlineinplot2[histlist,lineelement,{ymin,ymax}],Npttext},
PlotRange\[Rule]{{plotrangex[[1]],plotrangex[[2]]},{ymin,ymax}}
];
Print["Nbin is ",Nbin]
];
*)

(*
titlesize=16;
xtitlesize=16;
ytitlesize=16;
imgsize={{450},{450}};
title="title";xtitle="x";ytitle="y";
*)
AllPlots=Show[
p2,
BaseStyle->{FontSize->16},
Frame->True,
(*
FrameTicks\[Rule]{xTicks/.LF\[Rule]List,xTicks/.LF\[Rule]List,
xTicks/.LF[a__]\[RuleDelayed]{{a}\[LeftDoubleBracket]1\[RightDoubleBracket],""},xTicks/.LF[a__]\[RuleDelayed]{{a}\[LeftDoubleBracket]1\[RightDoubleBracket],""}},
*)
Axes->False,
PlotLabel->Style[title,titlesize,Black],
FrameLabel->{Style[xtitle,xtitlesize,Black],Style[ytitle,ytitlesize,Black]},
ImageSize->imgsize,
AspectRatio->1
];

AllPlots
(*
{median,binset2,ymax,xmax}
*)
];


(* ::Input::Initialization:: *)
(*20171111*)
(*plot statistics of correlation data line*)
(*input
histlistin: data, {value1, value2,...,...}
titlein,xtitlein,ytitlein: title of plot
BinWidth: the bin width of the histogram
epilogtext: the epilog of the histogram
lineelementin: text with line you want to add in plot: {{xvalue,"text",color},{...},...}
plotrangexin: {xmin,xmax}
*)
histplot5[histlistin_,titlein_,xtitlein_,ytitlein_,plotrangein_,BinWidthin_,epilogtextin_]:=
Module[{histlist=histlistin,title=titlein,xtitle=xtitlein,ytitle=ytitlein,plotrange=plotrangein,BinWidth=BinWidthin,epilogtext=epilogtextin,
Nbin,
xmin,xmax,ymin,ymax,binset2,textsize,Npttext,imgsize,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,avgw,p2,AllPlots},


(*default*)
imgsize={{600},{600}};
titlesize=24;
xtitlesize=18;
ytitlesize=18;
lgdlabelsize=12;
ticklablesize=18;

(*set the bins and xy range of the histogram*)
xmin=plotrange[[1]];
xmax=plotrange[[2]];
ymin=plotrange[[3]];
ymax=plotrange[[4]];

If[xmin=="default",xmin=Min[histlist,0.0] ];
If[xmax=="default",xmax=Max[histlist];xmax=xmax+0.05*(xmax-xmin);If[xmin<0,xmin=xmin-0.05*(xmax-xmin)] ];

(*set bins of the histogram*)
If[BinWidth=="default",BinWidth=(xmax-xmin)/20];
(*set default y max by the max value of of the histogram*)
If[ymin=="default",ymin=0.0 ];
If[ymax=="default",ymax=Max[HistogramList[histlist,{BinWidth}][[2]] ] ];



(*if set Nbin, then use Nbin setting*)
(*
If[Nbin=="auto",binset2=binset ];
If[Nbin!="auto",binset2={Table[plotrangex[[1]]+i*((plotrangex[[2]]-plotrangex[[1]])/Nbin),{i,0,Nbin}]} ];
*)
(*set line of median and mean, and texts*)
(*
textsize=16;
Npttext=Text[Style["Npt: "<>ToString[Length[histlist//Flatten] ],textsize,Black],Scaled[{0.1,0.9}] ];
*)

(*output histogram*)
p2=
Histogram[histlist,(*{xmin,xmax,w}*){BinWidth},
Epilog->epilogtext,
PlotRange->{{xmin,xmax},{ymin,ymax}}
];

AllPlots=Show[
p2,
BaseStyle->{FontSize->16},
Frame->True,
(*
FrameTicks\[Rule]{xTicks/.LF\[Rule]List,xTicks/.LF\[Rule]List,
xTicks/.LF[a__]\[RuleDelayed]{{a}\[LeftDoubleBracket]1\[RightDoubleBracket],""},xTicks/.LF[a__]\[RuleDelayed]{{a}\[LeftDoubleBracket]1\[RightDoubleBracket],""}},
*)
Axes->False,
PlotLabel->Style[title,titlesize,Black],
FrameLabel->{Style[xtitle,xtitlesize,Black],Style[ytitle,ytitlesize,Black]},
ImageSize->imgsize,
AspectRatio->1
];

AllPlots
(*
{median,binset2,ymax,xmax}
*)
];


(* ::Section:: *)
(*make plot (general use)*)


(* ::Subsection:: *)
(*make {x,Q} plot*)


(* ::Subsection:: *)
(*make {x,Q} plot for size by value*)


(* ::Subsection:: *)
(*make histogram*)


(* ::Subsection:: *)
(*save plot into file (pdf, eps, jpg)*)


(* ::Section:: *)
(*make plot (specific for correlation and .etc plots in our project)*)


(* ::Subsection:: *)
(*make {x,Q} plot *)


(* ::Subsection:: *)
(*make {x,Q} plot for size by value (corr, dr*corr, dr)*)


(* ::Subsection:: *)
(*make histogram (corr, dr*corr, dr)*)


(* ::Section:: *)
(*make all kinds plots *)


(* ::Subsection:: *)
(*read all kinds data from files*)


(* ::Subsection:: *)
(*do something on the data *)
(*(Mb, Mc cut for PDF data (cut value = 0 data), merge data of experiments into one)*)


(* ::Input:: *)
(*deletezerovalue[pdfdatain_]:=*)
(*Module[{pdfcorr ,pdfcorrdr,deltaR,deletelist,fmax,fmax2,fmax3},*)
(**)
(*{pdfcorr ,pdfcorrdr,deltaR}={pdfdatain[[1]],pdfdatain[[2]],pdfdatain[[3]]};*)
(*(*get #flavour*)*)
(*fmax=Length[pdfcorr];*)
(*fmax2=Length[pdfcorrdr];*)
(*fmax3=Length[deltaR];*)
(*If[fmax!=fmax2,Print["error, fmax of corr& dr*corr are different",fmax," ",fmax2];Return[1] ];*)
(*If[fmax!=fmax3,Print["error, fmax of corr& dr*corr are different",fmax," ",fmax3];Return[1] ];*)
(**)
(*(*delete data with corr or dr*corr \[Equal]0, those points mostly because of Mb or Mc cut*)*)
(*(*for usage of deltaR: set index of f(x,Q)\[Equal]0 as positive, and selete them by criteria positive, then delete them*)*)
(*deletelist=Table[Select[Table[If[pdfcorr[[f+6,ix,3]]==0,{ix},{-ix}],{ix,1,Length[pdfcorr[[f+6]] ]}],#[[1]]>0&],{f,-5,-5+fmax-1}];*)
(*pdfcorr=Table[Select[pdfcorr[[f+6]],#[[3]]!=0&] ,{f,-5,-5+fmax-1}];*)
(*pdfcorrdr=Table[Select[pdfcorrdr[[f+6]],#[[3]]!=0&] ,{f,-5,-5+fmax-1}];*)
(*deltaR=Table[Delete[deltaR[[f+6]],deletelist[[f+6]] ],{f,-5,-5+fmax-1}];(*2017.01.21 deltaR has [[flavour]] elements*)*)
(**)
(*(*if after delete some data, #points of {pdfcorr ,pdfcorrdr,deltaR} are diff, then skip out the function*)*)
(*Table[*)
(*If[Length[pdfcorr[[f+6]] ]!=Length[pdfcorrdr[[f+6]] ],Print["error, Npt of corr& dr*corr are different",",f=",f];Return[1] ];*)
(*If[Length[pdfcorr[[f+6]] ]!=Length[deltaR[[f+6]] ],Print["error, Npt of corr& dr are different",",f=",f];Print["(#pt corr, #pt dr) =",Length[pdfcorr[[f+6]] ]," ",Length[deltaR[[f+6]] ]];Return[1] ];*)
(*,{f,-5,-5+fmax-1}*)
(*];*)
(**)
(*{pdfcorr ,pdfcorrdr,deltaR}*)
(*]*)


(* ::Input:: *)
(**)


(* ::Code:: *)
(*(*input data format: {pdfcorr ,pdfcorrdr,deltaR}[[iexpt,flavour,Npt]]*)
(*merge data of all experiments,*)
(*output: {pdfcorr ,pdfcorrdr,deltaR}[[flavour,Npt]]*)
(**)*)
(*mergeplotdata[pdfdatain_]:=*)
(*Module[{pdfcorr ,pdfcorrdr,deltaR},*)
(**)
(*{pdfcorr ,pdfcorrdr,deltaR}={pdfdatain[[1]],pdfdatain[[2]],pdfdatain[[3]]};*)
(*(*combine data*)*)
(*(*transpose pdfcorr, pdfcorrdr to [[flavour,expids]], then flatten for all flavours*)*)
(*deltaR=Flatten[#]&/@Transpose[deltaR,{2,1} ];*)
(*pdfcorr=Flatten[#]&/@Transpose[pdfcorr,{2,1} ];*)
(*pdfcorrdr=Flatten[#]&/@Transpose[pdfcorrdr ,{2,1}];*)
(**)
(*{pdfcorr,pdfcorrdr,deltaR}*)
(*]*)
(**)


(* ::Subsection:: *)
(*make all kinds plots by data (corr, dr*corr, dr)*)


(* ::Input:: *)
(*(*transf from expid (processes[[proc,expid]]) to {proc,expid}*)*)
(*toprocexptid[exptidin_]:=*)
(*Module[{exptid=exptidin,processes,output},*)
(*processes={PDISNC,NDISCC,PDISNCCC,PVBPZ,PVBPW,PJP,hDISNC,hDISCC,hVBPZ};*)
(*output=False;*)
(*Do[*)
(*If[*)
(*exptid==processes[[iproc,iexpid]],*)
(*output={iproc,iexpid}*)
(*],*)
(*{iproc,1,Length[processes]},{iexpid,1,Length[processes[[iproc]] ]}*)
(*];*)
(*output*)
(*];*)
(*(*input exptlist, output {{procs},{expids}}*)*)
(*toprocexptidlist[exptidlistin_]:=*)
(*Module[{exptidlist=exptidlistin,output},*)
(*(*transf exptid to {{procs1,expids1},{},...}*)*)
(*output=Table[toprocexptid[exptidlist[[i]] ],{i,1,Length[exptidlist]}];*)
(*(*only select match exptid, if function return False, delete it*)*)
(*output=Select[output,SameQ[#,False]==False&];*)
(*(*transf to {{procs},{expids}}*)*)
(*output=Transpose[output,{2,1}];*)
(*output*)
(*]*)
(**)


(* ::Input::Initialization:: *)
(*processdataplotsmultiexp*)
(*for some reason, the function need to be deleted to keep the code*)


(* ::Input::Initialization:: *)
(*processdataplotsmultiexp2percentage*)
(*for some reason, the function need to be deleted to keep the code*)


(* ::Input::Initialization:: *)
(*20170306: use percentage as bar seperator*)
(*function to get value of choose percentage of data*)
(*20171111*)
(*
input:
data: a List of number
percentage: format = {per1,per2,...}, ex: {50, 75} means the value that larger than 50% and 70% of data
order data from small to large, specifying the value that larger than percentage of data
ex: median is the value that larger than 50% of data
*)
(*
output:
get the value that larger than percentage of data
*)
GetDataPercentage[datain_,percentagesin_]:=
Module[{data=datain,percentages=percentagesin,
Ndata,output},
(*20171111: check inputs are correct*)
If[Head[data]!=List,Print["error, input data should be a List of number"];Quit[] ];
If[#<0 || #>100,Print["error, percentage should be numbers in the range of {0,100}"];Quit[] ]&/@percentages;
Ndata=Length[data];

data=Sort[data];

output=
Table[
(*20171111: avoid the data index representing the percentage is 0, which means the data index should be at least 1*)
data[[Max[Round[Ndata*percentages[[i]]/100.0],1] ]],
{i,1,Length[percentages]}
];

output
];


(* ::Input:: *)
(*(*input class and configure arguments then get output plots, data struture: plottype = 5,6 [[iexpt,iflavour]], plottype = 2,3,4 [[iexpt]]*)*)
(*processdataplotsmultiexp3percentage[corrfxQdtaobsclassin_,configargumentsin_,plottypein_,flavourin_]:=*)
(*Module[{corrfxQdtaobsclass=corrfxQdtaobsclassin,configarguments=configargumentsin,*)
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
(*dummy1,dummy2,percentagetext,hist1plotrangex,histautoxrange,hist2plotrangex,unhighlightsize,highlightrange,highlighttext},*)
(*(*read arguments in config file*)*)
(*(*==============================*)*)
(*{Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,*)
(*XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,*)
(*ColorSeperator,*)
(*Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}=configarguments;*)
(**)
(*(*base on FigureFlag, decide the plot type of output plots (which data is used to plot), *)
(*ex: if correlation flag = 1, use correlation data*)*)
(*(*==============================*)*)
(**)
(*(*decide title by PDFname, FigureFlag, CorrelationArgFlag, ex: Corr(f_j(x,Q),r_i(x,Q)).*)
(*if user of CorrelationArgFlag is on, Corr( user_input,r_i(x,Q))*)*)
(*(*==============================*)*)
(*corrtitle1="Corr( ";*)
(*corrdrtitle1="\[Delta]r*Corr( ";*)
(*deltaRtitle1="\[Delta]r ";*)
(*title2=", r(x,\[Mu]))";*)
(*obsname="";(*initialize*)*)
(*pdfnamelable={"\!\(\*OverscriptBox[\(b\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(c\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(s\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(d\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(u\), \(_\)]\)(x,\[Mu])","g(x,\[Mu])","u(x,\[Mu])","d(x,\[Mu])","s(x,\[Mu])","c(x,\[Mu])","b(x,\[Mu])","\!\(\*FractionBox[\(\*OverscriptBox[\(d\), \(_\)] \((x, \[Mu])\)\), \(\*OverscriptBox[\(u\), \(_\)] \((x, \[Mu])\)\)]\)","\!\(\*FractionBox[\(d \((x, \[Mu])\)\), \(u \((x, \[Mu])\)\)]\)","\!\(\*FractionBox[\(s \((x, \[Mu])\) + \*OverscriptBox[\(s\), \(_\)] \((x, \[Mu])\)\), \(\*OverscriptBox[\(u\), \(_\)] \((x, \[Mu])\) + \*OverscriptBox[\(d\), \(_\)] \((x, \[Mu])\)\)]\)",UserArgName};*)
(**)
(*If[*)
(*plottype>=1 && plottype<=6,*)
(*If[*)
(*plottype==1 ,*)
(*obsname="";*)
(*];*)
(*If[*)
(*plottype==2 ,*)
(*obsname="Expt Uncertainty Ratio";*)
(*];*)
(*If[*)
(*plottype==3 ,*)
(*obsname="Residual(central value)";*)
(*];*)
(*If[*)
(*plottype==4 ,*)
(*obsname=deltaRtitle1;*)
(*];*)
(*If[*)
(*plottype==5 ,*)
(*obsname=corrdrtitle1<>pdfnamelable[[flavourin+6]]<>title2;*)
(*];*)
(*If[*)
(*plottype==6 ,*)
(*obsname=corrtitle1<>pdfnamelable[[flavourin+6]]<>title2;*)
(*];*)
(**)
(*];*)
(**)
(*title3=" for dataset of "<>PDFname;*)
(*title=obsname<>title3;*)
(**)
(*xtitle="x";*)
(*ytitle="\[Mu] [GeV]";*)
(*xhisttitle=obsname;*)
(*xhistabstitle="| "<>obsname<>" |";*)
(*yhisttitle="#points";*)
(**)
(*(*decide the data include by ExptidFlag << perhaps could be done before function*)*)
(*(*==============================*)*)
(*(*make text for Npt of data*)*)
(*(*********************************)*)
(*(*prepare for data input to processdataplotsmultiexp*)*)
(*(*********************************)*)
(*(*transf format from LF to LF1, since plot functions use LF1*)*)
(**)
(*(*if dr*corr or corr, data is [[iexpt,flavour]]*)*)
(*If[*)
(*plottype==5  || plottype==6,*)
(*fmax=Length[corrfxQdtaobsclass[[1]] ];*)
(*Table[*)
(*corrfxQdtaobsclass[[i,flavour+6]][["data"]]=corrfxQdtaobsclass[[i,flavour+6]][["data"]]/.LF->LF1;*)
(*(*deltaR[[i,flavour+6]][["data"]]=deltaR[[i,flavour+6]][["data"]]/.LF\[Rule]LF1;*)*)
(*"dummy",*)
(*{i,1,Length[corrfxQdtaobsclass]},{flavour,-5,-5+fmax-1}*)
(*];*)
(**)
(*(*set {corr[[flavour]],drcorr[[flavour]],dr[[flavour]]}*)*)
(*(*they are used to  input into processdataplotsmultiexp*)*)
(*(*data format \[Equal] {LF[x,Q,obs],...,...}*)*)
(*pdfcorr=Table[corrfxQdtaobsclass[[iexpt,flavour+6]][["data"]],{iexpt,1,Length[corrfxQdtaobsclass]},{flavour,-5,-5+fmax-1}];*)
(**)
(*(*merge all experimental data into one, for all flavours*)*)
(*(*ex: corrdataforplot[[iexpt,flavour,Npt]] \[Rule] orrdataforplot[[flavour,Npt]]*)*)
(*{pdfcorr ,dummy1,dummy2}=mergeplotdata[{pdfcorr ,pdfcorr,pdfcorr}];*)
(**)
(*(* deletezerovalue: delete data with 0 value (0 value usually from mb, mc cut)*)*)
(*{pdfcorr ,dummy1,dummy2}=deletezerovalue[{pdfcorr ,pdfcorr,pdfcorr}];*)
(*];*)
(**)
(*(*data of [[iexpt]]*)*)
(*If[*)
(*plottype==2 || plottype==3 || plottype==4,*)
(*Table[*)
(*corrfxQdtaobsclass[[i]][["data"]]=corrfxQdtaobsclass[[i]][["data"]]/.LF->LF1;*)
(*(*deltaR[[i,flavour+6]][["data"]]=deltaR[[i,flavour+6]][["data"]]/.LF\[Rule]LF1;*)*)
(*"dummy",*)
(*{i,1,Length[corrfxQdtaobsclass]}*)
(*];*)
(**)
(*pdfcorr=Table[corrfxQdtaobsclass[[iexpt]][["data"]],{iexpt,1,Length[corrfxQdtaobsclass]}];*)
(**)
(*(*merge all data into one*)*)
(*pdfcorr=Flatten[pdfcorr,1];*)
(*];*)
(**)
(*(*decide xy range of xQ plot, Nbin of histogram, xy range of histogram by*)
(*XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange*)*)
(*(*==============================*)*)
(*plotrange={XQfigureXrange,XQfigureYrange}//Flatten;*)
(**)
(*(*plotrangex=Hist1figureXrange;*)(*for histogram, how to deal with auto?*)*)
(*hist1plotrangex=Hist1figureXrange;*)
(*If[*)
(*plottype==5  || plottype==6,*)
(*histautoxrange=3*Median[pdfcorr[[flavourin+6]]/.LF1[a__]:>Abs[{a}[[3]] ] ];*)
(*];*)
(*If[*)
(*plottype==2 || plottype==3 || plottype==4,*)
(*histautoxrange=3*Median[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ] ];*)
(*];*)
(*If[hist1plotrangex[[1]]=="auto",hist1plotrangex[[1]]=-histautoxrange];*)
(*If[hist1plotrangex[[2]]=="auto",hist1plotrangex[[2]]=histautoxrange];*)
(*Print[histautoxrange,hist1plotrangex];*)
(**)
(*hist2plotrangex=hist1plotrangex;If[hist2plotrangex[[1]]<0.0,hist2plotrangex[[1]]=0.0];*)
(*(*for correlation histogram, set range (-1,1)*)*)
(*If[plottype==6,hist1plotrangex={-1,1};hist2plotrangex={0,1};];*)
(*stretchx=1;stretchy=1;*)
(*Hist1figureNbin=Hist1figureNbin;*)
(**)
(*(*setup texts and lines in plots*)*)
(*(*==============================*)*)
(*(*set outlayer points label in plot*)*)
(*textsize=16;*)
(*Npttext=Text[Style["Npt: "<>ToString[Length[pdfcorr[[flavourin+6]]//Flatten] ],textsize,Black],Scaled[{0.1,0.9}] ];*)
(*maxtext=Text[Style[ToString[ColorSeperator[[3]] ]<>"%",textsize,Black],Scaled[{0.1,0.8}] ];*)
(*maxmarker={Red,PointSize->0.02,Point[Scaled[{0.175,0.8}] ]};*)
(*mintext=Text[Style[ToString[ColorSeperator[[3]] ]<>"%(-)",textsize,Black],Scaled[{0.1,0.7}] ];*)
(*minmarker={Blue,PointSize->0.02,Point[Scaled[{0.175,0.7}] ]};*)
(*cuttext=Text[Style["cut: |data|<0.5",textsize,Black],Scaled[{0.15,0.6}] ];*)
(*percentagetext=Text[Style["percentage of colors:\n"<>ToString[ColorSeperator[[1]] ]<>"%"<>ToString[ColorSeperator[[2]] ]<>"%"<>ToString[ColorSeperator[[3]] ]<>"%",textsize,Black],Scaled[{0.2,0.8}] ];*)
(**)
(*(*for correlation, set color seperator by 0.5, 0.7, 0.85, 1*)*)
(*(*for uncertainty of theory, experiment, also 0.5, 0.7, 0.85, 1*)*)
(*(*for residual central value, deltaR, dr*corr, since value could be > 1 and even very large*)
(*color seperator decided by ColorSeperator*)*)
(*(*==============================*)*)
(*If[*)
(*plottype==5,*)
(*legendlabel="";*)
(*barseperator=GetDataPercentage[pdfcorr[[flavourin+6]]/.LF1[a__]:>Abs[{a}[[3]] ] ,Join[ColorSeperator,{100}] ];*)
(*barseperator={-barseperator[[4]],-barseperator[[3]],-barseperator[[2]],-barseperator[[1]],barseperator[[1]],barseperator[[2]],barseperator[[3]],barseperator[[4]]};*)
(*epilogxQ={Npttext,(*maxtext,maxmarker,mintext,minmarker*)percentagetext};*)
(**)
(*"dummy"*)
(*(*corr plot cut by |data|<0.5*)*)
(*];*)
(*(*same as plottype=5, but data strucure pdfcorr is different*)*)
(*If[*)
(* plottype==2 || plottype==3 || plottype==4,*)
(*legendlabel="";*)
(*barseperator=GetDataPercentage[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ] ,Join[ColorSeperator,{100}] ];*)
(*barseperator={-barseperator[[4]],-barseperator[[3]],-barseperator[[2]],-barseperator[[1]],barseperator[[1]],barseperator[[2]],barseperator[[3]],barseperator[[4]]};*)
(*epilogxQ={Npttext,(*maxtext,maxmarker,mintext,minmarker*)percentagetext};*)
(**)
(*"dummy"*)
(*(*corr plot cut by |data|<0.5*)*)
(*];*)
(**)
(*If[*)
(*plottype==6,*)
(*legendlabel="";*)
(*barseperator={-1,-0.85,-0.7,-0.5,0.5,0.7,0.85,1};*)
(*epilogxQ={Npttext};*)
(**)
(*"dummy"*)
(*(*corr plot cut by |data|<0.5*)*)
(*];*)
(**)
(*(*for no highlight mode, choose size of data point in plot by Size*)
(*for highlight mode, set size of unhighlighted data as Size, size of highlighted data is larger than Size*)*)
(*(*==============================*)*)
(*Print[HighlightMode];*)
(**)
(*unhighlightsize=*)
(*Switch[Size,"tiny",0.005,"small",0.0075,"medium",0.01,"large",0.0125,_,Print["error,size type is not correct"];Abort[] ];*)
(*highlightrange=*)
(*Switch[*)
(*HighlightMode[[plottype]],*)
(*0,{0.0,0.0},*)
(*1,{HighlightMode1[[2*plottype-1]],HighlightMode1[[2*plottype]]},*)
(*2,*)
(*Which[*)
(*plottype==5  || plottype==6,*)
(*GetDataPercentage[pdfcorr[[flavourin+6]]/.LF1[a__]:>Abs[{a}[[3]] ] ,{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}],*)
(* plottype==2 || plottype==3 || plottype==4,*)
(*GetDataPercentage[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ] ,{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}],*)
(*True,Print["presently plottype is only 2~6 "];Abort[]*)
(*],*)
(*_,Print["error, highlight mode should be 0, 1, 2"];Abort[]*)
(*];*)
(**)
(*highlighttext=Text[Style["highlight range:\n"<>ToString[highlightrange],textsize,Black],Scaled[{0.2,0.7}] ];*)
(*If[HighlightMode[[plottype]]!=0,epilogxQ=Append[epilogxQ,highlighttext] ];*)
(**)
(*Print["highlight range: ",highlightrange];*)
(**)
(*(*for histogram, setup highlighted value range by red line and color seperator value by blue line*)*)
(*(*make histogram of data and |data|*)*)
(*(*==============================*)*)
(**)
(*(*set xtitle by observable part of title, ex: |Corr(f_j(x,Q),r_i(x,Q))|*)*)
(*(*==============================*)*)
(**)
(**)
(*(*plot x,Q data by size for all quarks(CorrelationArgFlag), and plot corresponding histogram*)*)
(*(*GridGraphic x,Q data by size and histograms*)*)
(*(*==============================*)*)
(**)
(**)
(*(*correlation plot*)*)
(*If[*)
(*plottype==5  || plottype==6,*)
(*xQplotcorr=PDFCorrelationplot7[pdfcorr[[flavourin+6]],title,xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,(*Append[epilogxQ,cuttext]*)epilogxQ,highlightrange,unhighlightsize ];*)
(*"dummy"*)
(*];*)
(*If[*)
(* plottype==2 || plottype==3 || plottype==4,*)
(*xQplotcorr=PDFCorrelationplot7[pdfcorr,title,xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,(*Append[epilogxQ,cuttext]*)epilogxQ,highlightrange,unhighlightsize ];*)
(*"dummy"*)
(*];*)
(*(*correlation histogram*)*)
(**)
(*(*binset: for Nbin\[Equal]"auto", define auto binset*)*)
(*(*set auto bin as 5 bins in first color bar seperator *)*)
(*binset={Table[i*barseperator[[Length[barseperator]/2+1]]/5.0,{i,-100,100}]};*)
(**)
(*(*lineelement={{barseperator[[2]],"",Blue},{barseperator[[3]],"",Blue},{barseperator[[4]],"",Blue},{barseperator[[5]],"",Blue},{barseperator[[6]],"",Blue},{barseperator[[7]],"",Blue}};*)*)
(*lineelement={{barseperator[[2]],ToString[ColorSeperator[[3]] ]<>"%",Blue},{barseperator[[3]],ToString[ColorSeperator[[2]] ]<>"%",Blue},{barseperator[[4]],ToString[ColorSeperator[[1]] ]<>"%",Blue},{barseperator[[5]],ToString[ColorSeperator[[1]] ]<>"%",Blue},{barseperator[[6]],ToString[ColorSeperator[[2]] ]<>"%",Blue},{barseperator[[7]],ToString[ColorSeperator[[3]] ]<>"%",Blue}};*)
(*(*20170307: bin from argument*)*)
(*(**)
(*If[*)
(*Hist1figureNbin=="auto",*)
(*binset=barseperator;*)
(*binset=Insert[binset,0.0,5]*)
(*];*)
(**)*)
(*If[*)
(*plottype==5  || plottype==6,*)
(*(*hist1: data with no absolute*)*)
(*histplotcorr1=histplot4[pdfcorr[[flavourin+6]]/.LF1[a__]:>{a}[[3]],title,xhisttitle,yhisttitle,binset,lineelement,hist1plotrangex,Hist1figureNbin];*)
(*(*hist1: data with absolute(|data|)*)*)
(*histplotcorr2=histplot4[pdfcorr[[flavourin+6]]/.LF1[a__]:>Abs[{a}[[3]] ],title,xhistabstitle,yhisttitle,binset,Take[lineelement,-3],hist2plotrangex,Hist1figureNbin];*)
(*"dummy"*)
(*];*)
(*If[*)
(* plottype==2 || plottype==3 || plottype==4,*)
(*(*hist1: data with no absolute*)*)
(*histplotcorr1=histplot4[pdfcorr/.LF1[a__]:>{a}[[3]],title,xhisttitle,yhisttitle,binset,lineelement,hist1plotrangex,Hist1figureNbin];*)
(*(*hist1: data with absolute(|data|)*)*)
(*histplotcorr2=histplot4[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ],title,xhistabstitle,yhisttitle,binset,Take[lineelement,-3],hist2plotrangex,Hist1figureNbin];*)
(*"dummy"*)
(*];*)
(**)
(**)
(*(*make expt name & ID table*)*)
(*(*==============================*)*)
(**)
(*(*output*)*)
(*(*==============================*)*)
(*title;*)
(*GraphicsGrid[{{xQplotcorr},{histplotcorr1,histplotcorr2}}]*)
(**)
(**)
(*]*)


(* ::Code:: *)
(*(*modify of 3: when plottype = 5, 6, extract data of that flavour*)*)
(*processdataplotsmultiexp4percentage[corrfxQdtaobsclassin_,configargumentsin_,plottypein_,flavourin_]:=*)
(*Module[{corrfxQdtaobsclass=corrfxQdtaobsclassin,configarguments=configargumentsin,*)
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
(*dummy1,dummy2,percentagetext,hist1plotrangex,histautoxrange,hist2plotrangex,unhighlightsize,highlightrange,highlighttext,*)
(*rows,exptnames,exptnamestable},*)
(*(*read arguments in config file*)*)
(*(*==============================*)*)
(*{Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,*)
(*XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,*)
(*ColorSeperator,*)
(*Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}=configarguments;*)
(**)
(*(*base on FigureFlag, decide the plot type of output plots (which data is used to plot), *)
(*ex: if correlation flag = 1, use correlation data*)*)
(*(*==============================*)*)
(**)
(*(*decide title by PDFname, FigureFlag, CorrelationArgFlag, ex: Corr(f_j(x,Q),r_i(x,Q)).*)
(*if user of CorrelationArgFlag is on, Corr( user_input,r_i(x,Q))*)*)
(*(*==============================*)*)
(*corrtitle1="Corr( ";*)
(*corrdrtitle1="\[Delta]r*Corr( ";*)
(*deltaRtitle1="\[Delta]r ";*)
(*title2=", r(x,\[Mu]))";*)
(*obsname="";(*initialize*)*)
(*pdfnamelable={"\!\(\*OverscriptBox[\(b\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(c\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(s\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(d\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(u\), \(_\)]\)(x,\[Mu])","g(x,\[Mu])","u(x,\[Mu])","d(x,\[Mu])","s(x,\[Mu])","c(x,\[Mu])","b(x,\[Mu])","\!\(\*FractionBox[\(\*OverscriptBox[\(d\), \(_\)] \((x, \[Mu])\)\), \(\*OverscriptBox[\(u\), \(_\)] \((x, \[Mu])\)\)]\)","\!\(\*FractionBox[\(d \((x, \[Mu])\)\), \(u \((x, \[Mu])\)\)]\)","\!\(\*FractionBox[\(s \((x, \[Mu])\) + \*OverscriptBox[\(s\), \(_\)] \((x, \[Mu])\)\), \(\*OverscriptBox[\(u\), \(_\)] \((x, \[Mu])\) + \*OverscriptBox[\(d\), \(_\)] \((x, \[Mu])\)\)]\)",UserArgName};*)
(**)
(*If[*)
(*plottype>=1 && plottype<=6,*)
(*If[*)
(*plottype==1 ,*)
(*obsname="";*)
(*];*)
(*If[*)
(*plottype==2 ,*)
(*obsname="Expt Uncertainty Ratio";*)
(*];*)
(*If[*)
(*plottype==3 ,*)
(*obsname="Residual(central value)";*)
(*];*)
(*If[*)
(*plottype==4 ,*)
(*obsname=deltaRtitle1;*)
(*];*)
(*If[*)
(*plottype==5 ,*)
(*obsname=corrdrtitle1<>pdfnamelable[[flavourin+6]]<>title2;*)
(*];*)
(*If[*)
(*plottype==6 ,*)
(*obsname=corrtitle1<>pdfnamelable[[flavourin+6]]<>title2;*)
(*];*)
(**)
(*];*)
(**)
(*title3=" for dataset of "<>PDFname;*)
(*title=obsname<>title3;*)
(**)
(*xtitle="x";*)
(*ytitle="\[Mu] [GeV]";*)
(*xhisttitle=obsname;*)
(*xhistabstitle="| "<>obsname<>" |";*)
(*yhisttitle="#points";*)
(**)
(*(*btw test*)Print["test: ",plottype];*)
(**)
(*(*decide the data include by ExptidFlag << perhaps could be done before function*)*)
(*(*==============================*)*)
(*(*make text for Npt of data*)*)
(*(*********************************)*)
(*(*prepare for data input to processdataplotsmultiexp*)*)
(*(*********************************)*)
(*(*transf format from LF to LF1, since plot functions use LF1*)*)
(**)
(*(*if dr*corr or corr, data is [[iexpt,flavour]]*)*)
(*If[*)
(*plottype==5  || plottype==6,*)
(*fmax=Length[corrfxQdtaobsclass[[1]] ];*)
(*Table[*)
(*corrfxQdtaobsclass[[i,flavour+6]][["data"]]=corrfxQdtaobsclass[[i,flavour+6]][["data"]]/.LF->LF1;*)
(*(*deltaR[[i,flavour+6]][["data"]]=deltaR[[i,flavour+6]][["data"]]/.LF\[Rule]LF1;*)*)
(*"dummy",*)
(*{i,1,Length[corrfxQdtaobsclass]},{flavour,-5,-5+fmax-1}*)
(*];*)
(**)
(*(*set {corr[[flavour]],drcorr[[flavour]],dr[[flavour]]}*)*)
(*(*they are used to  input into processdataplotsmultiexp*)*)
(*(*data format \[Equal] {LF[x,Q,obs],...,...}*)*)
(*pdfcorr=Table[corrfxQdtaobsclass[[iexpt,flavour+6]][["data"]],{iexpt,1,Length[corrfxQdtaobsclass]},{flavour,-5,-5+fmax-1}];*)
(**)
(*(*merge all experimental data into one, for all flavours*)*)
(*(*ex: corrdataforplot[[iexpt,flavour,Npt]] \[Rule] orrdataforplot[[flavour,Npt]]*)*)
(*{pdfcorr ,dummy1,dummy2}=mergeplotdata[{pdfcorr ,pdfcorr,pdfcorr}];*)
(**)
(*(* deletezerovalue: delete data with 0 value (0 value usually from mb, mc cut)*)*)
(*{pdfcorr ,dummy1,dummy2}=deletezerovalue[{pdfcorr ,pdfcorr,pdfcorr}];*)
(*(*only take input flavour data*)*)
(*pdfcorr=pdfcorr[[flavourin+6]];*)
(*"dummy"*)
(*];*)
(**)
(*(*data of [[iexpt]]*)*)
(*If[*)
(*plottype==2 || plottype==3 || plottype==4,*)
(*Table[*)
(*corrfxQdtaobsclass[[i]][["data"]]=corrfxQdtaobsclass[[i]][["data"]]/.LF->LF1;*)
(*(*deltaR[[i,flavour+6]][["data"]]=deltaR[[i,flavour+6]][["data"]]/.LF\[Rule]LF1;*)*)
(*"dummy",*)
(*{i,1,Length[corrfxQdtaobsclass]}*)
(*];*)
(**)
(*pdfcorr=Table[corrfxQdtaobsclass[[iexpt]][["data"]],{iexpt,1,Length[corrfxQdtaobsclass]}];*)
(**)
(*(*merge all data into one*)*)
(*pdfcorr=Flatten[pdfcorr,1];*)
(*"dummy"*)
(*];*)
(**)
(*If[*)
(*plottype==1,*)
(*fmax=Length[corrfxQdtaobsclass[[1]] ];*)
(*Table[*)
(*corrfxQdtaobsclass[[i,flavour+6]][["data"]]=Datamethods[["take"]][corrfxQdtaobsclass[[i,flavour+6]],2][["data"]]/.LF->LF1;*)
(*(*deltaR[[i,flavour+6]][["data"]]=deltaR[[i,flavour+6]][["data"]]/.LF\[Rule]LF1;*)*)
(*"dummy",*)
(*{i,1,Length[corrfxQdtaobsclass]},{flavour,-5,-5+fmax-1}*)
(*];*)
(*(*set {corr[[flavour]],drcorr[[flavour]],dr[[flavour]]}*)*)
(*(*they are used to  input into processdataplotsmultiexp*)*)
(*(*data format \[Equal] {LF[x,Q,obs],...,...}*)*)
(*pdfcorr=Table[corrfxQdtaobsclass[[iexpt,6]][["data"]],{iexpt,1,Length[corrfxQdtaobsclass]}];*)
(*];*)
(**)
(*(*decide xy range of xQ plot, Nbin of histogram, xy range of histogram by*)
(*XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange*)*)
(*(*==============================*)*)
(*plotrange={XQfigureXrange,XQfigureYrange}//Flatten;*)
(**)
(*(*plotrangex=Hist1figureXrange;*)(*for histogram, how to deal with auto?*)*)
(*hist1plotrangex=Hist1figureXrange;*)
(*(**)
(*If[*)
(*plottype\[Equal]5  || plottype\[Equal]6,*)
(*histautoxrange=3*Median[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ];*)
(*];*)
(**)*)
(*If[*)
(*plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,*)
(*histautoxrange=3*Median[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ] ];*)
(*];*)
(*If[hist1plotrangex[[1]]=="auto",hist1plotrangex[[1]]=-histautoxrange];*)
(*If[hist1plotrangex[[2]]=="auto",hist1plotrangex[[2]]=histautoxrange];*)
(*Print[histautoxrange,hist1plotrangex];*)
(**)
(*hist2plotrangex=hist1plotrangex;If[hist2plotrangex[[1]]<0.0,hist2plotrangex[[1]]=0.0];*)
(*(*for correlation histogram, set range (-1,1)*)*)
(*If[plottype==6,hist1plotrangex={-1,1};hist2plotrangex={0,1};];*)
(*stretchx=1;stretchy=1;*)
(*Hist1figureNbin=Hist1figureNbin;*)
(**)
(*(*setup texts and lines in plots*)*)
(*(*==============================*)*)
(*(*set outlayer points label in plot*)*)
(*textsize=16;*)
(*Npttext=Text[Style["Npt: "<>ToString[Length[pdfcorr//Flatten] ],textsize,Black],Scaled[{0.1,0.9}] ];*)
(*maxtext=Text[Style[ToString[ColorSeperator[[3]] ]<>"%",textsize,Black],Scaled[{0.1,0.8}] ];*)
(*maxmarker={Red,PointSize->0.02,Point[Scaled[{0.175,0.8}] ]};*)
(*mintext=Text[Style[ToString[ColorSeperator[[3]] ]<>"%(-)",textsize,Black],Scaled[{0.1,0.7}] ];*)
(*minmarker={Blue,PointSize->0.02,Point[Scaled[{0.175,0.7}] ]};*)
(*cuttext=Text[Style["cut: |data|<0.5",textsize,Black],Scaled[{0.15,0.6}] ];*)
(*percentagetext=Text[Style["percentage of colors:\n"<>ToString[ColorSeperator[[1]] ]<>"%"<>ToString[ColorSeperator[[2]] ]<>"%"<>ToString[ColorSeperator[[3]] ]<>"%",textsize,Black],Scaled[{0.2,0.8}] ];*)
(**)
(*(*for correlation, set color seperator by 0.5, 0.7, 0.85, 1*)*)
(*(*for uncertainty of theory, experiment, also 0.5, 0.7, 0.85, 1*)*)
(*(*for residual central value, deltaR, dr*corr, since value could be > 1 and even very large*)
(*color seperator decided by ColorSeperator*)*)
(*(*==============================*)*)
(*(**)
(*If[*)
(*plottype\[Equal]5,*)
(*legendlabel="";*)
(*barseperator=GetDataPercentage[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ,Join[ColorSeperator,{100}] ];*)
(*barseperator={-barseperator[[4]],-barseperator[[3]],-barseperator[[2]],-barseperator[[1]],barseperator[[1]],barseperator[[2]],barseperator[[3]],barseperator[[4]]};*)
(*epilogxQ={Npttext,(*maxtext,maxmarker,mintext,minmarker*)percentagetext};*)
(**)
(*"dummy"*)
(*(*corr plot cut by |data|<0.5*)*)
(*];*)
(**)*)
(*(*same as plottype=5, but data strucure pdfcorr is different*)*)
(*If[*)
(* plottype==2 || plottype==3 || plottype==4 || plottype==5,*)
(*legendlabel="";*)
(*barseperator=GetDataPercentage[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ] ,Join[ColorSeperator,{100}] ];*)
(*barseperator={-barseperator[[4]],-barseperator[[3]],-barseperator[[2]],-barseperator[[1]],barseperator[[1]],barseperator[[2]],barseperator[[3]],barseperator[[4]]};*)
(*epilogxQ={Npttext,(*maxtext,maxmarker,mintext,minmarker*)percentagetext};*)
(**)
(*"dummy"*)
(*(*corr plot cut by |data|<0.5*)*)
(*];*)
(**)
(*If[*)
(*plottype==6,*)
(*legendlabel="";*)
(*barseperator={-1,-0.85,-0.7,-0.5,0.5,0.7,0.85,1};*)
(*epilogxQ={Npttext};*)
(**)
(*"dummy"*)
(*(*corr plot cut by |data|<0.5*)*)
(*];*)
(*(*plot type 1: just need barseperator so that function doesn't break*)*)
(*If[*)
(*plottype==1,*)
(*barseperator={-1,-0.85,-0.7,-0.5,0.5,0.7,0.85,1};*)
(*];*)
(**)
(*(*for no highlight mode, choose size of data point in plot by Size*)
(*for highlight mode, set size of unhighlighted data as Size, size of highlighted data is larger than Size*)*)
(*(*==============================*)*)
(*Print[HighlightMode];*)
(**)
(*unhighlightsize=*)
(*Switch[Size,"tiny",0.005,"small",0.0075,"medium",0.01,"large",0.0125,_,Print["error,size type is not correct"];Abort[] ];*)
(*highlightrange=*)
(*Switch[*)
(*HighlightMode[[plottype]],*)
(*0,{0.0,0.0},*)
(*1,{HighlightMode1[[2*plottype-1]],HighlightMode1[[2*plottype]]},*)
(*2,GetDataPercentage[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ] ,{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}]*)
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
(**)
(*highlighttext=Text[Style["highlight range:\n"<>ToString[highlightrange],textsize,Black],Scaled[{0.2,0.7}] ];*)
(*If[HighlightMode[[plottype]]!=0,epilogxQ=Append[epilogxQ,highlighttext] ];*)
(**)
(*Print["highlight range: ",highlightrange];*)
(**)
(*(*for histogram, setup highlighted value range by red line and color seperator value by blue line*)*)
(*(*make histogram of data and |data|*)*)
(*(*==============================*)*)
(**)
(*(*set xtitle by observable part of title, ex: |Corr(f_j(x,Q),r_i(x,Q))|*)*)
(*(*==============================*)*)
(**)
(**)
(*(*plot x,Q data by size for all quarks(CorrelationArgFlag), and plot corresponding histogram*)*)
(*(*GridGraphic x,Q data by size and histograms*)*)
(*(*==============================*)*)
(**)
(**)
(*(*correlation plot*)*)
(*(**)
(*If[*)
(*plottype\[Equal]5  || plottype\[Equal]6,*)
(*xQplotcorr=PDFCorrelationplot7[pdfcorr[[flavourin+6]],title,xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,(*Append[epilogxQ,cuttext]*)epilogxQ,highlightrange,unhighlightsize ];*)
(*"dummy"*)
(*];*)
(**)*)
(*If[*)
(* plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,*)
(*xQplotcorr=PDFCorrelationplot7[pdfcorr,title,xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,(*Append[epilogxQ,cuttext]*)epilogxQ,highlightrange,unhighlightsize ];*)
(*"dummy"*)
(*];*)
(*(*correlation histogram*)*)
(**)
(*(*binset: for Nbin\[Equal]"auto", define auto binset*)*)
(*(*set auto bin as 5 bins in first color bar seperator *)*)
(*binset={Table[i*barseperator[[Length[barseperator]/2+1]]/5.0,{i,-100,100}]};*)
(**)
(*(*lineelement={{barseperator[[2]],"",Blue},{barseperator[[3]],"",Blue},{barseperator[[4]],"",Blue},{barseperator[[5]],"",Blue},{barseperator[[6]],"",Blue},{barseperator[[7]],"",Blue}};*)*)
(*lineelement={{barseperator[[2]],ToString[ColorSeperator[[3]] ]<>"%",Blue},{barseperator[[3]],ToString[ColorSeperator[[2]] ]<>"%",Blue},{barseperator[[4]],ToString[ColorSeperator[[1]] ]<>"%",Blue},{barseperator[[5]],ToString[ColorSeperator[[1]] ]<>"%",Blue},{barseperator[[6]],ToString[ColorSeperator[[2]] ]<>"%",Blue},{barseperator[[7]],ToString[ColorSeperator[[3]] ]<>"%",Blue}};*)
(*(*20170307: bin from argument*)*)
(*(**)
(*If[*)
(*Hist1figureNbin=="auto",*)
(*binset=barseperator;*)
(*binset=Insert[binset,0.0,5]*)
(*];*)
(**)*)
(*(**)
(*If[*)
(*plottype\[Equal]5  || plottype\[Equal]6,*)
(*(*hist1: data with no absolute*)*)
(*histplotcorr1=histplot4[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]{a}[[3]],title,xhisttitle,yhisttitle,binset,lineelement,hist1plotrangex,Hist1figureNbin];*)
(*(*hist1: data with absolute(|data|)*)*)
(*histplotcorr2=histplot4[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ],title,xhistabstitle,yhisttitle,binset,Take[lineelement,-3],hist2plotrangex,Hist1figureNbin];*)
(*"dummy"*)
(*];*)
(**)*)
(*If[*)
(* plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,*)
(*(*hist1: data with no absolute*)*)
(*histplotcorr1=histplot4[pdfcorr/.LF1[a__]:>{a}[[3]],title,xhisttitle,yhisttitle,binset,lineelement,hist1plotrangex,Hist1figureNbin];*)
(*(*hist1: data with absolute(|data|)*)*)
(*histplotcorr2=histplot4[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ],title,xhistabstitle,yhisttitle,binset,Take[lineelement,-3],hist2plotrangex,Hist1figureNbin];*)
(*"dummy"*)
(*];*)
(**)
(*If[*)
(* plottype==1,*)
(*(*plot1*)*)
(*exptlist={1,2,3,4,5};*)
(*expttype="multi";*)
(*myplotsetting=setplotsetting[corrfxQdtaobsclass,exptlist,expttype,1,"test","test"];*)
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
(*lgdpos={0.25,0.725};*)
(*xyrangeplot1=plotrange;(*20170307 change it's name, avoid duplicate*)*)
(*plot1=PDFloglogplot[pdfcorr,myMarker,myplotstyle,title,xtitle,ytitle,xyrangeplot1,lgdlabel,lgdpos,imgsize];*)
(*];*)
(**)
(**)
(*(*make expt name & ID table*)*)
(*(*==============================*)*)
(**)
(*(*output*)*)
(*(*==============================*)*)
(*title;*)
(*(**)
(*GraphicsGrid[{{xQplotcorr},{histplotcorr1,histplotcorr2}}];*)
(*{{xQplotcorr},{histplotcorr1,histplotcorr2}};*)
(**)*)
(*Which[*)
(* plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,*)
(*{{xQplotcorr},{histplotcorr1,histplotcorr2}},*)
(*plottype==1,*)
(*{plot1}*)
(*]*)
(*]*)


(* ::Input:: *)
(*(*modify of 3: when plottype = 5, 6, extract data of that flavour*)*)
(*(*modify of 4: don't set local variable of corrfxQdtaobsclassin, avoiding time to copy large data to local variable, for mode 5,6 only deal with *)
(*corresponding flavour data (by flavourin)*)*)
(**)
(*(**)
(*processdataplotsmultiexp5percentage[corrfxQdtaobsclassin_,configargumentsin_,plottypein_,flavourin_]:=*)
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
(*dummy1,dummy2,percentagetext,hist1plotrangex,histautoxrange,hist2plotrangex,unhighlightsize,highlightrange,highlighttext,*)
(*exptlist,expttype,*)
(*rows,exptnames,exptnamestable},*)
(*(*read arguments in config file*)*)
(*(*==============================*)*)
(*{Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,*)
(*XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,*)
(*ColorSeperator,*)
(*Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}=configarguments;*)
(**)
(*(*read exptlist*)*)
(*exptlist={};*)
(*If[plottype==1  || plottype==5  || plottype==6,exptlist=Table[corrfxQdtaobsclassin[[iexpt,6]][["exptinfo","exptid"]],{iexpt,1,Length[corrfxQdtaobsclassin]}] ];*)
(*If[plottype==2  || plottype==3  || plottype==4,exptlist=Table[corrfxQdtaobsclassin[[iexpt]][["exptinfo","exptid"]],{iexpt,1,Length[corrfxQdtaobsclassin]}] ];*)
(*(*base on FigureFlag, decide the plot type of output plots (which data is used to plot), *)
(*ex: if correlation flag = 1, use correlation data*)*)
(*(*==============================*)*)
(**)
(*(*decide title by PDFname, FigureFlag, CorrelationArgFlag, ex: Corr(f_j(x,Q),r_i(x,Q)).*)
(*if user of CorrelationArgFlag is on, Corr( user_input,r_i(x,Q))*)*)
(*(*==============================*)*)
(*corrtitle1="Corr( ";*)
(*corrdrtitle1=(*"\[Delta]r*Corr( ";*)"Sensitivity to ";*)
(*deltaRtitle1=(*"\[Delta]r ";*)"PDF error \[Delta]r for residuals, ";*)
(*title2=", r(x,\[Mu]))";*)
(*title3=" for dataset of "<>PDFname;*)
(*obsname="";(*initialize*)*)
(*pdfnamelable={"\!\(\*OverscriptBox[\(b\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(c\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(s\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(d\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(u\), \(_\)]\)(x,\[Mu])","g(x,\[Mu])","u(x,\[Mu])","d(x,\[Mu])","s(x,\[Mu])","c(x,\[Mu])","b(x,\[Mu])","\!\(\*FractionBox[\(\*OverscriptBox[\(d\), \(_\)] \((x, \[Mu])\)\), \(\*OverscriptBox[\(u\), \(_\)] \((x, \[Mu])\)\)]\)","\!\(\*FractionBox[\(d \((x, \[Mu])\)\), \(u \((x, \[Mu])\)\)]\)","\!\(\*FractionBox[\(s \((x, \[Mu])\) + \*OverscriptBox[\(s\), \(_\)] \((x, \[Mu])\)\), \(\*OverscriptBox[\(u\), \(_\)] \((x, \[Mu])\) + \*OverscriptBox[\(d\), \(_\)] \((x, \[Mu])\)\)]\)",UserArgName};*)
(**)
(*If[*)
(*plottype>=1 && plottype<=6,*)
(*If[*)
(*plottype==1 ,*)
(*obsname="";*)
(*];*)
(*If[*)
(*plottype==2 ,*)
(*obsname="Expt Uncertainty Ratio";*)
(*title=obsname<>title3;*)
(*];*)
(*If[*)
(*plottype==3 ,*)
(*obsname="Residual(central value)";*)
(*title=obsname<>title3;*)
(*];*)
(*If[*)
(*plottype==4 ,*)
(*obsname=deltaRtitle1<>PDFname;*)
(*title=obsname;*)
(*];*)
(*If[*)
(*plottype==5 ,*)
(*obsname=corrdrtitle1<>pdfnamelable[[flavourin+6]](*<>title2*)<>", "<>PDFname;*)
(*title=obsname;*)
(*];*)
(*If[*)
(*plottype==6 ,*)
(*obsname=corrtitle1<>pdfnamelable[[flavourin+6]]<>title2;*)
(*title=obsname<>title3;*)
(*];*)
(**)
(*];*)
(**)
(**)
(*xtitle="x";*)
(*ytitle="\[Mu] [GeV]";*)
(*xhisttitle=obsname;*)
(*xhistabstitle="| "<>obsname<>" |";*)
(*yhisttitle="#points";*)
(**)
(*(*btw test*)(*Print["test: ",plottype];*)*)
(**)
(*(*decide the data include by ExptidFlag << perhaps could be done before function*)*)
(*(*==============================*)*)
(*(*make text for Npt of data*)*)
(*(*********************************)*)
(*(*prepare for data input to processdataplotsmultiexp*)*)
(*(*********************************)*)
(*(*transf format from LF to LF1, since plot functions use LF1*)*)
(**)
(*(*if dr*corr or corr, data is [[iexpt,flavour]]*)*)
(*If[*)
(*plottype==5  || plottype==6,*)
(*fmax=Length[corrfxQdtaobsclassin[[1]] ];*)
(**)
(*(*data format \[Equal] {LF[x,Q,obs],...,...}, to LF1*)*)
(*pdfcorr=Table[corrfxQdtaobsclassin[[iexpt,flavourin+6]][["data"]]/.LF->LF1,{iexpt,1,Length[corrfxQdtaobsclassin]}];*)
(**)
(*(*merge all experimental data into one, for all flavours*)*)
(*(*ex: corrdataforplot[[iexpt,flavour,Npt]] \[Rule] orrdataforplot[[flavour,Npt]]*)*)
(*(*{pdfcorr ,dummy1,dummy2}=mergeplotdata[{pdfcorr ,pdfcorr,pdfcorr}];*)*)
(*pdfcorr=Flatten[pdfcorr,1];*)
(**)
(*(* deletezerovalue: delete data with 0 value (0 value usually from mb, mc cut)*)*)
(*(*{pdfcorr ,dummy1,dummy2}=deletezerovalue[{pdfcorr ,pdfcorr,pdfcorr}];*)*)
(*pdfcorr=Select[pdfcorr,#[[3]]!=0&];*)
(*"dummy"*)
(*];*)
(**)
(*(*data of [[iexpt]]*)*)
(*If[*)
(*plottype==2 || plottype==3 || plottype==4,*)
(*(*take data, and format from LF to LF1 (LF1 is format to input to plot functions)*)*)
(*pdfcorr=Table[corrfxQdtaobsclassin[[iexpt]][["data"]]/.LF->LF1,{iexpt,1,Length[corrfxQdtaobsclassin]}];*)
(**)
(*(*merge all data into one*)*)
(*pdfcorr=Flatten[pdfcorr,1];*)
(*"dummy"*)
(*];*)
(**)
(*If[*)
(*plottype==1,*)
(*fmax=Length[corrfxQdtaobsclassin[[1]] ];*)
(**)
(*(*set {corr[[flavour]],drcorr[[flavour]],dr[[flavour]]}*)*)
(*(*they are used to  input into processdataplotsmultiexp*)*)
(*(*data format \[Equal] {LF[x,Q,obs],...,...}*)*)
(*pdfcorr=Table[Datamethods[["take"]][corrfxQdtaobsclassin[[iexpt,flavourin+6]],2][["data"]]/.LF->LF1,{iexpt,1,Length[corrfxQdtaobsclassin]}];*)
(*];*)
(**)
(*(*decide xy range of xQ plot, Nbin of histogram, xy range of histogram by*)
(*XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange*)*)
(*(*==============================*)*)
(*plotrange={XQfigureXrange,XQfigureYrange}//Flatten;*)
(**)
(*(*plotrangex=Hist1figureXrange;*)(*for histogram, how to deal with auto?*)*)
(*hist1plotrangex=Hist1figureXrange;*)
(*(**)
(*If[*)
(*plottype\[Equal]5  || plottype\[Equal]6,*)
(*histautoxrange=3*Median[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ];*)
(*];*)
(**)*)
(*If[*)
(*plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,*)
(*histautoxrange=3*Median[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ] ];*)
(*];*)
(*If[hist1plotrangex[[1]]=="auto",hist1plotrangex[[1]]=-histautoxrange];*)
(*If[hist1plotrangex[[2]]=="auto",hist1plotrangex[[2]]=histautoxrange];*)
(*(*Print[histautoxrange,hist1plotrangex];*)*)
(**)
(*hist2plotrangex=hist1plotrangex;If[hist2plotrangex[[1]]<0.0,hist2plotrangex[[1]]=0.0];*)
(*(*for correlation histogram, set range (-1,1)*)*)
(*If[plottype==6,hist1plotrangex={-1,1};hist2plotrangex={0,1};];*)
(*stretchx=1;stretchy=1;*)
(*Hist1figureNbin=Hist1figureNbin;*)
(**)
(*(*setup texts and lines in plots*)*)
(*(*==============================*)*)
(*(*set outlayer points label in plot*)*)
(*textsize=16;*)
(*Npttext=Text[Style["Npt: "<>ToString[Length[pdfcorr//Flatten] ],textsize,Black],Scaled[{0.1,0.9}] ];*)
(*maxtext=Text[Style[ToString[ColorSeperator[[3]] ]<>"%",textsize,Black],Scaled[{0.1,0.8}] ];*)
(*maxmarker={Red,PointSize->0.02,Point[Scaled[{0.175,0.8}] ]};*)
(*mintext=Text[Style[ToString[ColorSeperator[[3]] ]<>"%(-)",textsize,Black],Scaled[{0.1,0.7}] ];*)
(*minmarker={Blue,PointSize->0.02,Point[Scaled[{0.175,0.7}] ]};*)
(*cuttext=Text[Style["cut: |data|<0.5",textsize,Black],Scaled[{0.15,0.6}] ];*)
(*percentagetext=Text[Style["percentage of colors:\n"<>ToString[ColorSeperator[[1]] ]<>"%"<>ToString[ColorSeperator[[2]] ]<>"%"<>ToString[ColorSeperator[[3]] ]<>"%",textsize,Black],Scaled[{0.2,0.8}] ];*)
(**)
(*(*for correlation, set color seperator by 0.5, 0.7, 0.85, 1*)*)
(*(*for uncertainty of theory, experiment, also 0.5, 0.7, 0.85, 1*)*)
(*(*for residual central value, deltaR, dr*corr, since value could be > 1 and even very large*)
(*color seperator decided by ColorSeperator*)*)
(*(*==============================*)*)
(*(**)
(*If[*)
(*plottype\[Equal]5,*)
(*legendlabel="";*)
(*barseperator=GetDataPercentage[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ,Join[ColorSeperator,{100}] ];*)
(*barseperator={-barseperator[[4]],-barseperator[[3]],-barseperator[[2]],-barseperator[[1]],barseperator[[1]],barseperator[[2]],barseperator[[3]],barseperator[[4]]};*)
(*epilogxQ={Npttext,(*maxtext,maxmarker,mintext,minmarker*)percentagetext};*)
(**)
(*"dummy"*)
(*(*corr plot cut by |data|<0.5*)*)
(*];*)
(**)*)
(*(*same as plottype=5, but data strucure pdfcorr is different*)*)
(*If[*)
(* plottype==2 || plottype==3 || plottype==4 || plottype==5,*)
(*legendlabel="";*)
(*barseperator=GetDataPercentage[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ] ,Join[ColorSeperator,{100}] ];*)
(*barseperator={-barseperator[[4]],-barseperator[[3]],-barseperator[[2]],-barseperator[[1]],barseperator[[1]],barseperator[[2]],barseperator[[3]],barseperator[[4]]};*)
(*epilogxQ={Npttext,(*maxtext,maxmarker,mintext,minmarker*)percentagetext};*)
(**)
(*"dummy"*)
(*(*corr plot cut by |data|<0.5*)*)
(*];*)
(**)
(*If[*)
(*plottype==6,*)
(*legendlabel="";*)
(*barseperator={-1,-0.85,-0.7,-0.5,0.5,0.7,0.85,1};*)
(*epilogxQ={Npttext};*)
(**)
(*"dummy"*)
(*(*corr plot cut by |data|<0.5*)*)
(*];*)
(*(*plot type 1: just need barseperator so that function doesn't break*)*)
(*If[*)
(*plottype==1,*)
(*barseperator={-1,-0.85,-0.7,-0.5,0.5,0.7,0.85,1};*)
(*];*)
(**)
(*(*for no highlight mode, choose size of data point in plot by Size*)
(*for highlight mode, set size of unhighlighted data as Size, size of highlighted data is larger than Size*)*)
(*(*==============================*)*)
(*(*Print[HighlightMode];*)*)
(**)
(*highlightrange=*)
(*Switch[*)
(*HighlightMode[[plottype]],*)
(*0,{0.0,0.0},*)
(*1,{HighlightMode1[[2*plottype-1]],HighlightMode1[[2*plottype]]},*)
(*2,GetDataPercentage[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ] ,{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}]*)
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
(*(*for highlight mode, always only have no choice of size*)*)
(*If[HighlightMode[[plottype]]==1 || HighlightMode[[plottype]]==2,Size="small"];*)
(*(*set size*)*)
(*unhighlightsize=*)
(*Switch[Size,"tiny",0.005,"small",0.0075,"medium",0.01,"large",0.0125,_,Print["error,size type is not correct"];Abort[] ];*)
(**)
(*highlighttext=Text[Style["highlight range:\n"<>ToString[highlightrange],textsize,Black],Scaled[{0.2,0.7}] ];*)
(*If[HighlightMode[[plottype]]!=0,epilogxQ=Append[epilogxQ,highlighttext] ];*)
(**)
(*(*Print["highlight range: ",highlightrange];*)*)
(**)
(*(*for histogram, setup highlighted value range by red line and color seperator value by blue line*)*)
(*(*make histogram of data and |data|*)*)
(*(*==============================*)*)
(**)
(*(*set xtitle by observable part of title, ex: |Corr(f_j(x,Q),r_i(x,Q))|*)*)
(*(*==============================*)*)
(**)
(**)
(*(*plot x,Q data by size for all quarks(CorrelationArgFlag), and plot corresponding histogram*)*)
(*(*GridGraphic x,Q data by size and histograms*)*)
(*(*==============================*)*)
(**)
(**)
(*(*correlation plot*)*)
(*(**)
(*If[*)
(*plottype\[Equal]5  || plottype\[Equal]6,*)
(*xQplotcorr=PDFCorrelationplot7[pdfcorr[[flavourin+6]],title,xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,(*Append[epilogxQ,cuttext]*)epilogxQ,highlightrange,unhighlightsize ];*)
(*"dummy"*)
(*];*)
(**)*)
(*(*test print*)*)
(*(**)
(*Print["test print"];*)
(*Print["highlight mode 1"];*)
(*Print[pdfcorr];*)
(*Print[HighlightMode1];*)
(*Print[{xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,epilogxQ,highlightrange,unhighlightsize }];*)
(**)*)
(**)
(*If[*)
(* plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,*)
(*xQplotcorr=PDFCorrelationplot7[pdfcorr,"",xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,(*Append[epilogxQ,cuttext]*)epilogxQ,highlightrange,unhighlightsize ];*)
(*"dummy"*)
(*];*)
(*(*correlation histogram*)*)
(**)
(*(*binset: for Nbin\[Equal]"auto", define auto binset*)*)
(*(*set auto bin as 5 bins in first color bar seperator *)*)
(*binset={Table[i*barseperator[[Length[barseperator]/2+1]]/5.0,{i,-100,100}]};*)
(**)
(*(*lineelement={{barseperator[[2]],"",Blue},{barseperator[[3]],"",Blue},{barseperator[[4]],"",Blue},{barseperator[[5]],"",Blue},{barseperator[[6]],"",Blue},{barseperator[[7]],"",Blue}};*)*)
(*lineelement={{barseperator[[2]],ToString[ColorSeperator[[3]] ]<>"%",Blue},{barseperator[[3]],ToString[ColorSeperator[[2]] ]<>"%",Blue},{barseperator[[4]],ToString[ColorSeperator[[1]] ]<>"%",Blue},{barseperator[[5]],ToString[ColorSeperator[[1]] ]<>"%",Blue},{barseperator[[6]],ToString[ColorSeperator[[2]] ]<>"%",Blue},{barseperator[[7]],ToString[ColorSeperator[[3]] ]<>"%",Blue}};*)
(*(*20170307: bin from argument*)*)
(*(**)
(*If[*)
(*Hist1figureNbin=="auto",*)
(*binset=barseperator;*)
(*binset=Insert[binset,0.0,5]*)
(*];*)
(**)*)
(*(**)
(*If[*)
(*plottype\[Equal]5  || plottype\[Equal]6,*)
(*(*hist1: data with no absolute*)*)
(*histplotcorr1=histplot4[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]{a}[[3]],title,xhisttitle,yhisttitle,binset,lineelement,hist1plotrangex,Hist1figureNbin];*)
(*(*hist1: data with absolute(|data|)*)*)
(*histplotcorr2=histplot4[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ],title,xhistabstitle,yhisttitle,binset,Take[lineelement,-3],hist2plotrangex,Hist1figureNbin];*)
(*"dummy"*)
(*];*)
(**)*)
(*If[*)
(* plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,*)
(*(*hist1: data with no absolute*)*)
(*histplotcorr1=histplot4[pdfcorr/.LF1[a__]:>{a}[[3]],"",xhisttitle,yhisttitle,binset,lineelement,hist1plotrangex,Hist1figureNbin];*)
(*(*hist1: data with absolute(|data|)*)*)
(*histplotcorr2=histplot4[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ],"",xhistabstitle,yhisttitle,binset,Take[lineelement,-3],hist2plotrangex,Hist1figureNbin];*)
(*"dummy"*)
(*];*)
(**)
(*If[*)
(* plottype==1,*)
(*(*plot1*)*)
(**)
(*expttype="multi";*)
(*myplotsetting=setplotsetting[corrfxQdtaobsclassin,exptlist,expttype,1,"test","test"];*)
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
(*xyrangeplot1=plotrange;(*20170307 change it's name, avoid duplicate*)*)
(*plot1=PDFloglogplot[pdfcorr,myMarker,myplotstyle,title,xtitle,ytitle,xyrangeplot1,lgdlabel,lgdpos,imgsize];*)
(*];*)
(**)
(**)
(*(*make expt name & ID table*)*)
(*(*==============================*)*)
(**)
(*(*output*)*)
(*(*==============================*)*)
(*title;*)
(*(**)
(*GraphicsGrid[{{xQplotcorr},{histplotcorr1,histplotcorr2}}];*)
(*{{xQplotcorr},{histplotcorr1,histplotcorr2}};*)
(**)*)
(**)
(*(*make  exptname table*)*)
(*rows=3;*)
(*exptnames=Table[ExptIDtoName[exptlist[[iexpt]] ]<>"("<>ToString[exptlist[[iexpt]] ]<>")",{iexpt,1,Length[exptlist]}];*)
(*Print["making table of experiments included in plots"];*)
(*exptnamestable=makeGrid2[exptnames,rows,title<>"\n\n"];*)
(**)
(*Which[*)
(* plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,*)
(*{{xQplotcorr,exptnamestable},{histplotcorr1,histplotcorr2}},*)
(*plottype==1,*)
(*{plot1}*)
(*]*)
(*]*)
(**)
(**)*)


(* ::Input::Initialization:: *)
(*modify of 3: when plottype = 5, 6, extract data of that flavour*)
(*modify of 4: don't set local variable of corrfxQdtaobsclassin, avoiding time to copy large data to local variable, for mode 5,6 only deal with 
corresponding flavour data (by flavourin)*)

(*20170515 this version corrfxQdtaobsclassin structure is {class1, class2,...}
it is for plotting different group of data in different point shapes
*)
processdataplotsmultiexp6percentage[corrfxQdtaobsclassin_,configargumentsin_,plottypein_,flavourin_]:=
Module[{(*corrfxQdtaobsclass=corrfxQdtaobsclassin,*)configarguments=configargumentsin,
plottype=plottypein,(*flavour=flavourin,*)flavour,
Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,
ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2,
processes,ExptList1,pdfsetexps,processestitle,PDISNCtitle,NDISCCtitle,PDISNCCCtitle,PVBPZtitle,PVBPWtitle,PJPtitle,hDISNCtitle,hDISCCtitle,hVBPZtitle,pdfnamelable,PDFsetlabel,pdffile,corrfile,pdfcorr,pdfcorrdr,deltaR,textsize,Npttext,maxtext,maxmarker,mintext,minmarker,cuttext,epilogxQ,epilogxQcorr,corrtitle1,corrdrtitle1,deltaRtitle1,title2,obsname,title3,title4,title,xtitle,ytitle,xhisttitle,xhistabstitle,yhisttitle,plotrange,stretchx,stretchy,legendlabel,barseperator,binset,lineelement,plotrangex,SM,SM1,SM2,SM3,xQplotcorr ,histplotcorr1,histplotcorr2,xQplotcorrdr,histplotcorrdr1,histplotcorrdr2,xQplotdr,histplotdr2,myexpids,fmax,fmax2,
imgsize,(*title,xtitle,ytitle,*)lgdlabel,xrange,yrange,epilog,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,
myplotstyle,myMarker,
lgdpos,xyrangeplot1,
myplotsetting,plot1data,plot1,processesplot1order,
dummy1,dummy2,percentagetext,hist1plotrangex,histautoxrange,hist2plotrangex,xQautorange,unhighlightsize,highlightrange,highlighttext,
exptlist,expttype,
rows,exptnames,exptnamestable,
lineelement2,maxdata,
barlegend},
(*read arguments in config file*)
(*==============================*)
{Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,
ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}=configarguments;

(*read exptlist*)
exptlist={};
If[plottype==1  || plottype==5  || plottype==6,exptlist=Table[#[[iexpt,6]][["exptinfo","exptid"]],{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin ];
If[plottype==2  || plottype==3  || plottype==4,
exptlist=Table[#[[iexpt]][["exptinfo","exptid"]],{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin ];
(*test*)Print["expts: ",exptlist];
(*base on FigureFlag, decide the plot type of output plots (which data is used to plot), 
ex: if correlation flag = 1, use correlation data*)
(*==============================*)

(*decide title by PDFname, FigureFlag, CorrelationArgFlag, ex: Corr(f_j(x,Q),r_i(x,Q)).
if user of CorrelationArgFlag is on, Corr( user_input,r_i(x,Q))*)
(*==============================*)
corrtitle1="Corr( ";
corrdrtitle1=(*"\[Delta]r*Corr( ";*)"Sensitivity to ";
(*deltaRtitle1=(*"\[Delta]r ";*)(*"PDF error \[Delta]r for residuals, ";*)"\[Delta]r";*)
title2=(*", r(x,\[Mu]))";*)", \!\(\*SubscriptBox[\(r\), \(i\)]\))";
title3=(*" for dataset of "*)", "<>PDFname;
obsname="";(*initialize*)
pdfnamelable={"\!\(\*OverscriptBox[\(b\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(c\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(s\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(d\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(u\), \(_\)]\)(x,\[Mu])","g(x,\[Mu])","u(x,\[Mu])","d(x,\[Mu])","s(x,\[Mu])","c(x,\[Mu])","b(x,\[Mu])","\!\(\*FractionBox[\(\*OverscriptBox[\(d\), \(_\)] \((x, \[Mu])\)\), \(\*OverscriptBox[\(u\), \(_\)] \((x, \[Mu])\)\)]\)","\!\(\*FractionBox[\(d \((x, \[Mu])\)\), \(u \((x, \[Mu])\)\)]\)","\!\(\*FractionBox[\(s \((x, \[Mu])\) + \*OverscriptBox[\(s\), \(_\)] \((x, \[Mu])\)\), \(\*OverscriptBox[\(u\), \(_\)] \((x, \[Mu])\) + \*OverscriptBox[\(d\), \(_\)] \((x, \[Mu])\)\)]\)",UserArgName};

(*20171107: simplify title labels*)
If[
plottype>=1 && plottype<=6,
If[
plottype==1 ,
obsname="";
];
If[
plottype==2 ,
obsname="\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)/\!\(\*SubscriptBox[\(D\), \(i\)]\)";
title=obsname<>title3;
];
If[
plottype==3 ,
obsname="\!\(\*SubscriptBox[\(r\), \(i\)]\) (Residual)";
title=obsname<>title3;
];
If[
plottype==4 ,
obsname="\!\(\*SubscriptBox[\(\[Delta]r\), \(i\)]\)";
(*obsname=deltaRtitle1<>PDFname;*)
title=obsname<>title3;
];
If[
plottype==5 ,
obsname=corrdrtitle1<>pdfnamelable[[flavourin+6]](*<>title2*);
title=obsname<>title3;
];
If[
plottype==6 ,
obsname=corrtitle1<>pdfnamelable[[flavourin+6]]<>title2;
title=obsname<>title3;
];

];


xtitle="x";
ytitle="\[Mu] [GeV]";
xhisttitle=obsname;
xhistabstitle="| "<>obsname<>" |";
yhisttitle="#points";

(*btw test*)(*Print["test: ",plottype];*)

(*decide the data include by ExptidFlag << perhaps could be done before function*)
(*==============================*)
(*make text for Npt of data*)
(*********************************)
(*prepare for data input to processdataplotsmultiexp*)
(*********************************)
(*transf format from LF to LF1, since plot functions use LF1*)

(*if dr*corr or corr, data is [[iexpt,flavour]]*)
(*20170515: pdfcorr = {group1data, group2data, ...}, groupNdata = {LF1[x,Q,obs],...}*)
If[
plottype==5  || plottype==6,
fmax=Length[corrfxQdtaobsclassin[[1,1]] ];

(*data format \[Equal] {LF[x,Q,obs],...,...}, to LF1*)
pdfcorr=Table[#[[iexpt,flavourin+6]][["data"]]/.LF->LF1,{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin;

(*merge all experimental data into one, for all flavours*)
(*ex: corrdataforplot[[iexpt,flavour,Npt]] \[Rule] orrdataforplot[[flavour,Npt]]*)
(*{pdfcorr ,dummy1,dummy2}=mergeplotdata[{pdfcorr ,pdfcorr,pdfcorr}];*)
pdfcorr=Flatten[#,1]&/@pdfcorr;

(* deletezerovalue: delete data with 0 value (0 value usually from mb, mc cut)*)
(*{pdfcorr ,dummy1,dummy2}=deletezerovalue[{pdfcorr ,pdfcorr,pdfcorr}];*)
pdfcorr=Table[Select[pdfcorr[[igroup]],#[[3]]!=0&],{igroup,1,Length[pdfcorr]}];
"dummy"
];

(*data of [[iexpt]]*)
If[
plottype==2 || plottype==3 || plottype==4,
(*take data, and format from LF to LF1 (LF1 is format to input to plot functions)*)
pdfcorr=Table[#[[iexpt]][["data"]]/.LF->LF1,{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin;
(*20171108 expt error ratio values should be absolute value*)
If[plottype==2,pdfcorr=pdfcorr/.LF1[a__]:>LF1[{a}[[1]],{a}[[2]],Abs[{a}[[3]] ] ] ];
(*merge all data into one*)
pdfcorr=Flatten[#,1]&/@pdfcorr;
"dummy"
];
(*test print*)(*Print[pdfcorr ];Print[pdfcorr/.LF1->LF2 ];Print[pdfcorr/.LF1[a__]:>{a}[[2]] ];*)

If[
plottype==1,
fmax=Length[corrfxQdtaobsclassin[[1,1]] ];

(*set {corr[[flavour]],drcorr[[flavour]],dr[[flavour]]}*)
(*they are used to  input into processdataplotsmultiexp*)
(*data format \[Equal] {LF[x,Q,obs],...,...}*)
pdfcorr=Table[Datamethods[["take"]][#[[iexpt,flavourin+6]],2][["data"]]/.LF->LF1,{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin;
];

(*decide xy range of xQ plot, Nbin of histogram, xy range of histogram by
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange*)
(*==============================*)
plotrange={XQfigureXrange,XQfigureYrange}//Flatten;

(*plotrangex=Hist1figureXrange;*)(*for histogram, how to deal with auto?*)
hist1plotrangex=Hist1figureXrange;

(*for no highlight mode, choose size of data point in plot by Size
for highlight mode, set size of unhighlighted data as Size, size of highlighted data is larger than Size*)
(*==============================*)
highlightrange=
Switch[
HighlightMode[[plottype]],
0,{0.0,0.0},
1,{HighlightMode1[[2*plottype-1]],HighlightMode1[[2*plottype]]},
2,GetDataPercentage[Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ] ,{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}]
(*
Which[
plottype\[Equal]5  || plottype\[Equal]6,
GetDataPercentage[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ,{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}],
 plottype\[Equal]2 || plottype\[Equal]3 || plottype\[Equal]4,
GetDataPercentage[pdfcorr/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ,{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}],
True,Print["presently plottype is only 2~6 "];Abort[]
]*),
_,Print["error, highlight mode should be 0, 1, 2"];Abort[]
];
(*
If[
plottype\[Equal]5  || plottype\[Equal]6,
histautoxrange=3*Median[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ];
];
*)
(*20171102 xQ plot auto range*)
If[
 plottype==1 || plottype==2 || plottype==3 || plottype==4 || plottype==5 || plottype==6 ,
xQautorange={{10^-5,1},{1,2000}}
];
If[plotrange[[1]]=="auto",plotrange[[1]]=xQautorange[[1,1]] ];
If[plotrange[[2]]=="auto",plotrange[[2]]=xQautorange[[1,2]] ];
If[plotrange[[3]]=="auto",plotrange[[3]]=xQautorange[[2,1]] ];
If[plotrange[[4]]=="auto",plotrange[[4]]=xQautorange[[2,2]] ];

(*20170515: for statistics of total data, the all data are considered, so here Flatten the data pdfcorr*)
(*20170515: for auto mode, correlation, deltaR, dR*correlation should have histogram of roughly 1*)
If[
plottype==2 (*|| plottype==3 || plottype==4 || plottype==5  || plottype==6*),
histautoxrange=3*Median[Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ] ]
];
If[
plottype==3 || plottype==4 || plottype==5 ,
histautoxrange=Max[3*Median[Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ] ],1.0];
];
(*20171106: if highlight mode on, auto range set as the range of highlight*)(*20171108: ranges of all modes are the same*)
(*
If[
HighlightMode[[plottype]]==1 || HighlightMode[[plottype]]==2,
histautoxrange=1.1*highlightrange[[2]]
];
*)

If[hist1plotrangex[[1]]=="auto",hist1plotrangex[[1]]=-histautoxrange];
If[hist1plotrangex[[2]]=="auto",hist1plotrangex[[2]]=histautoxrange];
(*Print[histautoxrange,hist1plotrangex];*)

hist2plotrangex=hist1plotrangex;If[hist2plotrangex[[1]]<0.0,hist2plotrangex[[1]]=0.0];
(*for correlation histogram, set range (-1,1)*)
If[plottype==6,hist1plotrangex={-1,1};hist2plotrangex={0,1};];
stretchx=1;stretchy=1;
Hist1figureNbin=Hist1figureNbin;

(*setup texts and lines in plots*)
(*==============================*)
(*set outlayer points label in plot*)
textsize=16;
Npttext=Text[Style["Npt: "<>ToString[Length[pdfcorr//Flatten] ],textsize,Black],Scaled[{0.1,0.9}] ];
maxtext=Text[Style[ToString[ColorSeperator[[3]] ]<>"%",textsize,Black],Scaled[{0.1,0.8}] ];
maxmarker={Red,PointSize->0.02,Point[Scaled[{0.175,0.8}] ]};
mintext=Text[Style[ToString[ColorSeperator[[3]] ]<>"%(-)",textsize,Black],Scaled[{0.1,0.7}] ];
minmarker={Blue,PointSize->0.02,Point[Scaled[{0.175,0.7}] ]};
cuttext=Text[Style["cut: |data|<0.5",textsize,Black],Scaled[{0.15,0.6}] ];
percentagetext=Text[Style["percentage of colors:\n"<>ToString[ColorSeperator[[1]] ]<>"%"<>ToString[ColorSeperator[[2]] ]<>"%"<>ToString[ColorSeperator[[3]] ]<>"%",textsize,Black],Scaled[{0.2,0.8}] ];

(*for correlation, set color seperator by 0.5, 0.7, 0.85, 1*)
(*for uncertainty of theory, experiment, also 0.5, 0.7, 0.85, 1*)
(*for residual central value, deltaR, dr*corr, since value could be > 1 and even very large
color seperator decided by ColorSeperator*)
(*==============================*)
(*
If[
plottype\[Equal]5,
legendlabel="";
barseperator=GetDataPercentage[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ,Join[ColorSeperator,{100}] ];
barseperator={-barseperator[[4]],-barseperator[[3]],-barseperator[[2]],-barseperator[[1]],barseperator[[1]],barseperator[[2]],barseperator[[3]],barseperator[[4]]};
epilogxQ={Npttext,(*maxtext,maxmarker,mintext,minmarker*)percentagetext};

"dummy"
(*corr plot cut by |data|<0.5*)
];
*)
(*same as plottype=5, but data strucure pdfcorr is different*)
(*20170515: for statistics of total data, the all data are considered, so here Flatten the data pdfcorr*)
If[
 plottype==2 || plottype==3 || plottype==4 || plottype==5,
legendlabel="";
barseperator=GetDataPercentage[Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ] ,Join[ColorSeperator,{100}] ];(*20170606make sure 100% data included*)barseperator[[4]]=1.001*barseperator[[4]];
barseperator={-barseperator[[4]],-barseperator[[3]],-barseperator[[2]],-barseperator[[1]],barseperator[[1]],barseperator[[2]],barseperator[[3]],barseperator[[4]]};
epilogxQ={Npttext,(*maxtext,maxmarker,mintext,minmarker*)percentagetext};

"dummy"
(*corr plot cut by |data|<0.5*)
];

If[
plottype==6,
legendlabel="";
barseperator={-1,-0.85,-0.7,-0.5,0.5,0.7,0.85,1};
epilogxQ={Npttext};

"dummy"
(*corr plot cut by |data|<0.5*)
];
(*20170517: the most important of dr*corr is to show it's absolute value(how many data larger than 1)*)
If[
(*plottype==3 || plottype==4 || *)plottype==5,
legendlabel="";
(*20171025 set bar seperator as {-100,-5.0,-2.0,-0.5,0.5,2.0,5.0,100}*)
(*
barseperator={-100,-0.85,-0.7,-0.5,0.5,0.7,0.85,100};
*)
barseperator={-100,-5.0,-2.0,-0.5,0.5,2.0,5.0,100};
epilogxQ={Npttext};

"dummy"
];
(*10170606: deltaR & residual central: scale should be very small, close to 1, large, very large*)
If[
plottype==3 (*|| plottype==4*),
legendlabel="";
barseperator={-100,-5.0,-2.0,-0.5,0.5,2.0,5.0,100};
epilogxQ={Npttext};

"dummy"
];
(*20171108: residual error are always positive*)
If[
plottype==4,
legendlabel="";
barseperator={0,0.5,2.0,5.0,100};
epilogxQ={Npttext};

"dummy"
];

(*20171107: set seperator of \[Sigma]/D*)(*20171108: \[Sigma]/D are always positive*)
If[
plottype==2,
legendlabel="";
barseperator={(*-100,-0.3,-0.1,-0.05*)0,0.05,0.1,0.3,100};
epilogxQ={Npttext};
"dummy"
];

(*plot type 1: just need barseperator so that function doesn't break*)
If[
plottype==1,
barseperator={-1,-0.85,-0.7,-0.5,0.5,0.7,0.85,1};
];

(*20170517: if highlight mode = 1 && not correlation data, then if highlight uplimit > max of data, automatically adjust it to the max of the data*)
(*20171106: don't choose max of data as highlightrange[[2]], just use the input number*)
(*
If[
HighlightMode[[plottype]]==1&&plottype!=6&&plottype!=1,
maxdata=Max[Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ] ];

If[
maxdata>highlightrange[[1]],
highlightrange[[2]]=Min[highlightrange[[2]],maxdata];
];
"dummy" ];
*)

(*20171106 set highlight barlegend*)(*20171108: all modes use the same color palette (barlegend)*)
legendlabel="";
If[
(HighlightMode[[plottype]]==0 || HighlightMode[[plottype]]==1 || HighlightMode[[plottype]]==2),
If[
(plottype==3|| plottype==5  || plottype==6),
barlegend=Set2DxqBarLegend[barseperator,legendlabel] 
];
If[
(plottype==2 || plottype==4),
barlegend=Set2DxqRedBarLegend[barseperator,legendlabel] 
];
"dummy"
];
(*
If[HighlightMode[[plottype]]\[Equal]1 || HighlightMode[[plottype]]\[Equal]2,barlegend=(*Set2DxqHighlightBarLegend[legendlabel,highlightrange]*)Set2DxqBarLegend[barseperator,legendlabel] ];
*)

(*Print["max of data and highlight: ",{highlightrange[[2]],maxdata}];*)

(*for highlight mode, always only have no choice of size*)
If[HighlightMode[[plottype]]==1 || HighlightMode[[plottype]]==2,Size="small"];
(*set size*)
unhighlightsize=
Switch[Size,"tiny",0.005,"small",0.0075,"medium",0.01,"large",0.0125,_,Print["error,size type is not correct"];Abort[] ];

If[
HighlightMode[[plottype]]==1,
highlighttext=Text[Style["highlighted range:\n"<>ToString[highlightrange],textsize,Black],Scaled[{0.2,0.7}] ];
"dummy"
];
(*20171108: only show 3 digits *)
If[
HighlightMode[[plottype]]==2,
highlighttext=Text[Style["highlighted range:\n"<>"("<>ToString[HighlightMode2[[2*plottype-1]] ]<>"% ~ "<>ToString[HighlightMode2[[2*plottype]] ]<>"%)\n"<>ToString[NumberForm[highlightrange,{100,2}] ],textsize,Black],Scaled[{0.2,0.6}] ];
"dummy"
];
If[HighlightMode[[plottype]]!=0,epilogxQ=Append[epilogxQ,highlighttext] ];

(*Print["highlight range: ",highlightrange];*)

(*for histogram, setup highlighted value range by red line and color seperator value by blue line*)
(*make histogram of data and |data|*)
(*==============================*)

(*set xtitle by observable part of title, ex: |Corr(f_j(x,Q),r_i(x,Q))|*)
(*==============================*)


(*plot x,Q data by size for all quarks(CorrelationArgFlag), and plot corresponding histogram*)
(*GridGraphic x,Q data by size and histograms*)
(*==============================*)


(*correlation plot*)
(*
If[
plottype\[Equal]5  || plottype\[Equal]6,
xQplotcorr=PDFCorrelationplot8[pdfcorr[[flavourin+6]],title,xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,(*Append[epilogxQ,cuttext]*)epilogxQ,highlightrange,unhighlightsize ];
"dummy"
];
*)
(*test print*)
(*
Print["test print"];
Print["highlight mode 1"];
Print[pdfcorr];
Print[HighlightMode1];
Print[{xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,epilogxQ,highlightrange,unhighlightsize }];
*)
If[PlotTitleBool==False,title=""];
If[
 plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,
xQplotcorr=PDFCorrelationplot8[pdfcorr,title,xtitle,ytitle,plotrange,stretchx,stretchy,barlegend,(*barseperator,legendlabel,*)(*Append[epilogxQ,cuttext]*)epilogxQ,highlightrange,unhighlightsize ];
"dummy"
];
(*correlation histogram*)

(*binset: for Nbin\[Equal]"auto", define auto binset*)
(*set auto bin as 10 bins in first color bar seperator *)
(*20171108: set auto bin as N = 20 in positive side of xrange, bin width = xrange(+)/20 *)
(*
binset={Table[i*barseperator[[Length[barseperator]/2+1]]/10.0,{i,-100,100}]};
*)
binset={Table[i*hist1plotrangex[[2]]/20.0,{i,-100,100}]};

(*lineelement={{barseperator[[2]],"",Blue},{barseperator[[3]],"",Blue},{barseperator[[4]],"",Blue},{barseperator[[5]],"",Blue},{barseperator[[6]],"",Blue},{barseperator[[7]],"",Blue}};*)
If[
plottype==2,
(*20171108: \[Sigma]/D only has posive data*)
(*
lineelement={{barseperator[[2]],ToString[ColorSeperator[[3]] ]<>"%",Blue},{barseperator[[3]],ToString[ColorSeperator[[2]] ]<>"%",Blue},{barseperator[[4]],ToString[ColorSeperator[[1]] ]<>"%",Blue},{barseperator[[5]],ToString[ColorSeperator[[1]] ]<>"%",Blue},{barseperator[[6]],ToString[ColorSeperator[[2]] ]<>"%",Blue},{barseperator[[7]],ToString[ColorSeperator[[3]] ]<>"%",Blue}};
*)
lineelement={{-0.1,"",Red},{0.1,"",Red}};
lineelement2=(*Take[lineelement,-3]*){{0.1,"",Red}};
];
(*if correlation histogram, don't need show lines to represent the % of data *)
If[ plottype==3 || plottype==4 || plottype==5,lineelement={{-1,"",Red},{1,"",Red}};lineelement2={{1,"",Red}};"dummy"];
If[ plottype==6,lineelement={{-0.7,"",Red},{0.7,"",Red}};lineelement2={{0.7,"",Red}};"dummy"];

(*20171106: bin line in histogram for highlight mode*)(*20171108: don't add it presently*)
(*
If[
HighlightMode[[plottype]]\[Equal]1 || HighlightMode[[plottype]]\[Equal]2,
lineelement={{-highlightrange[[1]],"",Red},{-highlightrange[[2]],"",Red},{highlightrange[[1]],"",Red},{highlightrange[[2]],"",Red}};
lineelement2={{highlightrange[[1]],"",Red},{highlightrange[[2]],"",Red}}; 
binset={Table[i*(highlightrange[[2]]-highlightrange[[1]])/20.0,{i,-100,100}]}
(*
binset={Table[i*barseperator[[Length[barseperator]/2+1]]/10.0,{i,-100,100}]};(*test*)
*)
];
*)
(*test*)(*Print[binset];Print[lineelement2];*)

(*20170307: bin from argument*)
(*
If[
Hist1figureNbin=="auto",
binset=barseperator;
binset=Insert[binset,0.0,5]
];
*)
(*
If[
plottype\[Equal]5  || plottype\[Equal]6,
(*hist1: data with no absolute*)
histplotcorr1=histplot4[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]{a}[[3]],title,xhisttitle,yhisttitle,binset,lineelement,hist1plotrangex,Hist1figureNbin];
(*hist1: data with absolute(|data|)*)
histplotcorr2=histplot4[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ],title,xhistabstitle,yhisttitle,binset,Take[lineelement,-3],hist2plotrangex,Hist1figureNbin];
"dummy"
];
*)
If[
 plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,
(*hist1: data with no absolute*)
(*20170515: temporary use data of all groups to draw the histogram in one color*)
histplotcorr1=histplot4[Flatten[pdfcorr]/.LF1[a__]:>{a}[[3]],title,xhisttitle,yhisttitle,binset,lineelement,hist1plotrangex,Hist1figureNbin];
(*hist1: data with absolute(|data|)*)
histplotcorr2=histplot4[Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ],title,xhistabstitle,yhisttitle,binset,(*Take[lineelement,-3]*)lineelement2,hist2plotrangex,Hist1figureNbin];
"dummy"
];

If[
 plottype==1,
(*plot1*)

expttype="multi";
(*20170515 groups of data, legend is exptids in all groups, using Flatten, PDFname should be took cared*)
myplotsetting=setplotsetting[Flatten[corrfxQdtaobsclassin,1],exptlist//Flatten,expttype,1,"test","test"];
imgsize=myplotsetting[["imgsize"]];
title=myplotsetting[["title"]];
xtitle=myplotsetting[["xtitle"]];
ytitle=myplotsetting[["ytitle"]];
lgdlabel=myplotsetting[["lgdlabel"]];
xrange=myplotsetting[["xrange"]];
yrange=myplotsetting[["yrange"]];
epilog=myplotsetting[["epilog"]];
titlesize=myplotsetting[["titlesize"]];
xtitlesize=myplotsetting[["xtitlesize"]];
ytitlesize=myplotsetting[["ytitlesize"]];
lgdlabelsize=myplotsetting[["lgdlabelsize"]];
ticklablesize=myplotsetting[["ticklablesize"]];

myplotstyle=myplotsetting[["plotstyle"]];
myMarker=myplotsetting[["marker"]];

title="Experimental data in "<>PDFname<>" analysis";
lgdpos={0.25,0.725};
xyrangeplot1=plotrange;(*20170307 change it's name, avoid duplicate*)
(*20170515: consider expts in all groups*)
(*20170515: consider expts of all groups, so use Flatten[data,1] *)

(*
Print["dim of p1: ",Dimensions[pdfcorr] ];
Print["dim of p1: ",Dimensions[Flatten[pdfcorr,1] ]];
Print["data of p1: ",Flatten[pdfcorr,1][[1]] ];
Print["data of p1: ",Flatten[pdfcorr,1][[2]] ];
Print["data of p1: ",Flatten[pdfcorr,1][[3]] ];
Print["data of p1: ",Flatten[pdfcorr,1][[4]] ];
PDFloglogplot[Flatten[pdfcorr,1]/.LF1\[Rule]List,myMarker,myplotstyle,title,xtitle,ytitle,xyrangeplot1,lgdlabel,lgdpos,imgsize];
Abort[];
*)

plot1=PDFloglogplot[Flatten[pdfcorr,1],myMarker,myplotstyle,title,xtitle,ytitle,xyrangeplot1,lgdlabel,lgdpos,imgsize];
];


(*make expt name & ID table*)
(*==============================*)

(*output*)
(*==============================*)
title;
(*
GraphicsGrid[{{xQplotcorr},{histplotcorr1,histplotcorr2}}];
{{xQplotcorr},{histplotcorr1,histplotcorr2}};
*)

(*make  exptname table*)
(*20170515: when show all experiments, show expts of all groups*)
rows=3;
exptnames=Table[ExptIDtoName[Flatten[exptlist][[iexpt]] ]<>"("<>ToString[Flatten[exptlist][[iexpt]] ]<>")",{iexpt,1,Length[exptlist//Flatten]}];
Print["making table of experiments included in plots"];
exptnamestable=makeGrid2[exptnames,rows,title<>"\n\n"];

Which[
 plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,
{{xQplotcorr,exptnamestable},{histplotcorr1,histplotcorr2}},
plottype==1,
{plot1}
]
]


(* ::Input::Initialization:: *)
(*20171126*)
(*
when call processdataplotsmultiexp7percentage,
if plot type = 1, call this function to draw data sets on (x,Q) plane
*)
PlotDataTypeOne[corrfxQdtaobsclassin_,pdfcorrin_,exptlistin_,xyrangein_,classifymodein_,configargumentsin_]:=
Module[{corrfxQdtaobsclass=corrfxQdtaobsclassin,pdfcorr=pdfcorrin,exptlist=exptlistin,classifymode=classifymodein,configarguments=configargumentsin,
plottype,expttype,
Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,(*UserArgName,UserArgValue,*)
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,
ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2,
lgdpos,xyrangeplot1,plotrange,
myplotsetting,imgsize,title,xtitle,ytitle,lgdlabel,xrange,yrange,epilog,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,myplotstyle,myMarker,
plot1,
xyrange=xyrangein,
groupnames,groupExptIDs,
ColorPaletterange},

(*read arguments in config file*)
(*==============================*)
{Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,(*UserArgName,UserArgValue,*)
XQfigureXrange,XQfigureYrange,ColorPaletterange(*20171128*),Hist1figureNbin,(*Hist1figureXrange,Hist1figureYrange,*)
ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}=configarguments;
(*20171128: set hist xrange determined by the range of color palette, yrange alway auto*)
Hist1figureXrange=ColorPaletterange;
Hist1figureYrange={"auto","auto"};
(*=============================================================================================================================*)
(*plot type 1: generate plots============================================================================================================*)
(*=============================================================================================================================*)
{myplotsetting,imgsize,title,xtitle,ytitle,lgdlabel,xrange,yrange,epilog,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,myplotstyle,myMarker};

(*plot1*)

expttype="multi";
(*20170515 groups of data, legend is exptids in all groups, using Flatten, PDFname should be took cared*)
myplotsetting=setplotsetting[Flatten[corrfxQdtaobsclassin,1],exptlist//Flatten,expttype,1,"test","test"];
imgsize=myplotsetting[["imgsize"]];
title=myplotsetting[["title"]];
xtitle=myplotsetting[["xtitle"]];
ytitle=myplotsetting[["ytitle"]];
lgdlabel=myplotsetting[["lgdlabel"]];
xrange=myplotsetting[["xrange"]];
yrange=myplotsetting[["yrange"]];
epilog=myplotsetting[["epilog"]];
titlesize=myplotsetting[["titlesize"]];
xtitlesize=myplotsetting[["xtitlesize"]];
ytitlesize=myplotsetting[["ytitlesize"]];
lgdlabelsize=myplotsetting[["lgdlabelsize"]];
ticklablesize=myplotsetting[["ticklablesize"]];

myplotstyle=myplotsetting[["plotstyle"]];
myMarker=myplotsetting[["marker"]];

title="Experimental data in "<>PDFname<>" analysis";
lgdpos={0.25,0.725};
(*xyrangeplot1=plotrange;*)(*20170307 change it's name, avoid duplicate*)
(*20170515: consider expts in all groups*)
(*20170515: consider expts of all groups, so use Flatten[data,1] *)

(*
Print["dim of p1: ",Dimensions[pdfcorr] ];
Print["dim of p1: ",Dimensions[Flatten[pdfcorr,1] ]];
Print["data of p1: ",Flatten[pdfcorr,1][[1]] ];
Print["data of p1: ",Flatten[pdfcorr,1][[2]] ];
Print["data of p1: ",Flatten[pdfcorr,1][[3]] ];
Print["data of p1: ",Flatten[pdfcorr,1][[4]] ];
PDFloglogplot[Flatten[pdfcorr,1]/.LF1\[Rule]List,myMarker,myplotstyle,title,xtitle,ytitle,xyrangeplot1,lgdlabel,lgdpos,imgsize];
Abort[];
*)
(*20171126: use marker list*)
myMarker=PlotMarkerList[];
myMarker[[2]]=0.0075*myMarker[[2]];
myMarker=Transpose[myMarker,{2,1}];
Print["dim of data: ",Dimensions[pdfcorr] ];
Print["exptlist ",exptlist];
(*always don't show only one shape for all expt ID points*)
If[classifymode=="all",classifymode="single"];
{groupnames,groupExptIDs,(*groupdata*)pdfcorr}=ClassifyPlottedData[pdfcorr,exptlist,classifymode];
pdfcorr=Table[Flatten[pdfcorr[[igroup]],1],{igroup,Length[pdfcorr]}];
lgdlabel=
Switch[classifymode,"single",lgdlabel,"all",groupnames];

Print["dim of data: ",Dimensions[pdfcorr] ];

plot1=PDFloglogplot[(*Flatten[pdfcorr,1]*)pdfcorr,myMarker,myplotstyle,title,xtitle,ytitle,xyrange(*plot1*),lgdlabel,lgdpos,imgsize];

(*return*)

{plot1}

(*
{myMarker,myplotstyle,title,xtitle,ytitle,xyrange,lgdlabel,lgdpos,imgsize}
*)
]


(* ::Input::Initialization:: *)
(*modify of 3: when plottype = 5, 6, extract data of that flavour*)
(*modify of 4: don't set local variable of corrfxQdtaobsclassin, avoiding time to copy large data to local variable, for mode 5,6 only deal with 
corresponding flavour data (by flavourin)*)

(*20170515 this version corrfxQdtaobsclassin structure is {class1, class2,...}
it is for plotting different group of data in different point shapes
*)

(*20171108: reorganize the function*)
(*use the new highlight range convention*)
processdataplotsmultiexp7percentage[corrfxQdtaobsclassin_,configargumentsin_,plottypein_,flavourin_]:=
Module[{(*corrfxQdtaobsclass=corrfxQdtaobsclassin,*)configarguments=configargumentsin,
plottype=plottypein,(*flavour=flavourin,*)flavour,
Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,
ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2,
processes,ExptList1,pdfsetexps,processestitle,PDISNCtitle,NDISCCtitle,PDISNCCCtitle,PVBPZtitle,PVBPWtitle,PJPtitle,hDISNCtitle,hDISCCtitle,hVBPZtitle,pdfnamelable,PDFsetlabel,pdffile,corrfile,pdfcorr,pdfcorrdr,deltaR,textsize,Npttext,maxtext,maxmarker,mintext,minmarker,cuttext,epilogxQ,epilogxQcorr,corrtitle1,corrdrtitle1,deltaRtitle1,title2,obsname,title3,title4,title,xtitle,ytitle,xhisttitle,xhistabstitle,yhisttitle,plotrange,stretchx,stretchy,legendlabel,barseperator,binset,lineelement,plotrangex,SM,SM1,SM2,SM3,xQplotcorr ,histplotcorr1,histplotcorr2,xQplotcorrdr,histplotcorrdr1,histplotcorrdr2,xQplotdr,histplotdr2,myexpids,fmax,fmax2,
imgsize,(*title,xtitle,ytitle,*)lgdlabel,xrange,yrange,epilog,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,
myplotstyle,myMarker,
lgdpos,xyrangeplot1,
myplotsetting,plot1data,plot1,processesplot1order,
dummy1,dummy2,percentagetext,hist1plotrangex,histautoxrange,hist2autoxrange,hist2plotrangex,xQautorange,unhighlightsize,highlightrange,highlighttext,
exptlist,expttype,
rows,exptnames,exptnamestable,
lineelement2,maxdata,
barlegend,
histtitle,histabstitle,yhistabstitle,HistAutoMode,userdifinefuncfilename,
hist1plotrangey,hist2plotrangey,BinWidth,hist1plotrange,hist2plotrange,highlightlines,
xmintmp,xmaxtmp,ymintmp,hist1epilogtext,hist2epilogtext,hist1standardlines,hist2standardlines,LineWidth,HistAutoFixXrangeBool,
datemode,HistDataList,HistAbsDataList,DataMax,DataMin,AbsDataMax,AbsDataMin,DataMean,AbsDataMean,DataMedian,AbsDataMedian,DataSD,AbsDataSD,
ColorPaletteMode,PaletteMax,PaletteMin,
groupnames,groupExptIDs,classifymode,
ColorPaletterange},
(*read arguments in config file*)
(*==============================*)
{Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,(*UserArgName,UserArgValue,*)
XQfigureXrange,XQfigureYrange,ColorPaletterange(*20171128*),Hist1figureNbin,(*Hist1figureXrange,Hist1figureYrange,*)
ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}=configarguments;
(*20171128: set hist xrange determined by the range of color palette, yrange alway auto*)
Hist1figureXrange=ColorPaletterange;
Hist1figureYrange={"auto","auto"};

(*20171109: seperate user difine function/data IO and configure file*)(*20171116: ->ReadUserFunctionV2, which read Expression from the file*)
(*20171119 new user function format: {{user name 1, user function 1}, {user name 2, user function 2}...}*)
userdifinefuncfilename="user_func.txt";
(*
{UserArgName,UserArgValue}=ReadUserFunctionV2["./",userdifinefuncfilename];
*)
(*20171127*)
If[
CorrelationArgFlag[[-1]]==1,
UserArgName=ReadUserFunctionV3["./",userdifinefuncfilename];
UserArgName=#[[1]]&/@UserArgName;
"dummy"
];

(*20171109: shorten the tiles of figures*)
If[PDFname=="2017.1008.0954.-0500_CT14HERA2-jet.ev",PDFname="CT14HERA2-jet.ev"];
(*=============================================================================================================================*)
(*Data organization============================================================================================================*)
(*=============================================================================================================================*)
(*read exptlist*)
exptlist={};
If[plottype==1  || plottype==5  || plottype==6,exptlist=Table[#[[iexpt,6]][["exptinfo","exptid"]],{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin ];
If[plottype==2  || plottype==3  || plottype==4,
exptlist=Table[#[[iexpt]][["exptinfo","exptid"]],{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin ];
(*test*)(*Print["expts: ",exptlist];*)

(*20171126 classify mode for data \[Rule] different shape for each group*)
classifymode=ClassifyMode;

(*********************************)
(*prepare for data input to processdataplotsmultiexp*)
(*********************************)
(*transf format from LF to LF1, since plot functions use LF1*)

(*if dr*corr or corr, data is [[iexpt,flavour]]*)
(*20170515: pdfcorr = {group1data, group2data, ...}, groupNdata = {LF1[x,Q,obs],...}*)
If[
plottype==5  || plottype==6,
fmax=Length[corrfxQdtaobsclassin[[1,1]] ];

(*data format \[Equal] {LF[x,Q,obs],...,...}, to LF1*)
pdfcorr=Table[#[[iexpt,flavourin+6]][["data"]]/.LF->LF1,{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin;

(*20171126: classify exptid by defined groups with classifymode*)
{groupnames,groupExptIDs,(*groupdata*)pdfcorr}=ClassifyPlottedData[pdfcorr[[1]],exptlist[[1]],classifymode];
(*pdfcorr=Table[Flatten[pdfcorr[[igroup]],1],{igroup,Length[pdfcorr]}];*)

(*merge all experimental data into one, for all flavours*)
(*ex: corrdataforplot[[iexpt,flavour,Npt]] \[Rule] orrdataforplot[[flavour,Npt]]*)
(*{pdfcorr ,dummy1,dummy2}=mergeplotdata[{pdfcorr ,pdfcorr,pdfcorr}];*)
pdfcorr=Flatten[#,1]&/@pdfcorr;

(* deletezerovalue: delete data with 0 value (0 value usually from mb, mc cut)*)
(*{pdfcorr ,dummy1,dummy2}=deletezerovalue[{pdfcorr ,pdfcorr,pdfcorr}];*)
pdfcorr=Table[Select[pdfcorr[[igroup]],#[[3]]!=0&],{igroup,1,Length[pdfcorr]}];
"dummy"
];

(*data of [[iexpt]]*)
If[
plottype==2 || plottype==3 || plottype==4,
(*take data, and format from LF to LF1 (LF1 is format to input to plot functions)*)
pdfcorr=Table[#[[iexpt]][["data"]]/.LF->LF1,{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin;
(*20171108 expt error ratio values should be absolute value*)
If[plottype==2,pdfcorr=pdfcorr/.LF1[a__]:>LF1[{a}[[1]],{a}[[2]],Abs[{a}[[3]] ] ] ];

(*20171126: classify exptid by defined groups with classifymode*)
{groupnames,groupExptIDs,(*groupdata*)pdfcorr}=ClassifyPlottedData[pdfcorr[[1]],exptlist[[1]],classifymode];

(*merge all data into one*)
pdfcorr=Flatten[#,1]&/@pdfcorr;
"dummy"
];
(*test print*)(*Print[pdfcorr ];Print[pdfcorr/.LF1->LF2 ];Print[pdfcorr/.LF1[a__]:>{a}[[2]] ];*)

If[
plottype==1,
fmax=Length[corrfxQdtaobsclassin[[1,1]] ];

(*set {corr[[flavour]],drcorr[[flavour]],dr[[flavour]]}*)
(*they are used to  input into processdataplotsmultiexp*)
(*data format \[Equal] {LF[x,Q,obs],...,...}*)
pdfcorr=Table[Datamethods[["take"]][#[[iexpt,flavourin+6]],2][["data"]]/.LF->LF1,{iexpt,1,Length[#]}]&/@corrfxQdtaobsclassin;
];
(*=============================================================================================================================*)
(*Statistical quantities of data============================================================================================================*)
(*=============================================================================================================================*)
(*20171115*)
{HistDataList,HistAbsDataList,DataMax,DataMin,AbsDataMax,AbsDataMin,DataMean,AbsDataMean,DataMedian,AbsDataMedian,DataSD,AbsDataSD};
If[
plottype==2 || plottype==3 || plottype==4 || plottype==5 || plottype==6,
HistDataList=Flatten[pdfcorr]/.LF1[a__]:>{a}[[3]];
HistAbsDataList=Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ];
DataMax=Max[HistDataList];
DataMin=Min[HistDataList];
AbsDataMax=Max[HistAbsDataList];
AbsDataMin=Min[HistAbsDataList];
DataMean=Mean[HistDataList];
AbsDataMean=Mean[HistAbsDataList];
DataMedian=Median[HistDataList];
AbsDataMedian=Median[HistAbsDataList];
DataSD=StandardDeviation[HistDataList];
AbsDataSD=StandardDeviation[HistAbsDataList];
"dummy"
];
(*=============================================================================================================================*)
(*Highlight range setting============================================================================================================*)
(*=============================================================================================================================*)

(*for no highlight mode, choose size of data point in plot by Size
for highlight mode, set size of unhighlighted data as Size, size of highlighted data is larger than Size*)
(*==============================*)

highlightrange=
Switch[
HighlightMode[[plottype]],
0,{{0.0,0.0}},
(*20171109: use new highlight range convention*)
1,(*{HighlightMode1[[2*plottype-1]],HighlightMode1[[2*plottype]]}*)HighlightMode1[[plottype]],
(*20171111: percentage highlight: don't take absolute values for data*)
2,GetDataPercentage[(*Flatten[pdfcorr]/.LF1[a__]\[RuleDelayed]{a}[[3]]*)HistDataList,(*{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}*)#]&/@HighlightMode2[[plottype]]
(*
Which[
plottype\[Equal]5  || plottype\[Equal]6,
GetDataPercentage[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ,{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}],
 plottype\[Equal]2 || plottype\[Equal]3 || plottype\[Equal]4,
GetDataPercentage[pdfcorr/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ,{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}],
True,Print["presently plottype is only 2~6 "];Abort[]
]*),
_,Print["error, highlight mode should be 0, 1, 2"];Abort[]
];

(*=============================================================================================================================*)
(*title, xtitle, ytitle============================================================================================================*)
(*=============================================================================================================================*)

(*decide title by PDFname, FigureFlag, CorrelationArgFlag, ex: Corr(f_j(x,Q),r_i(x,Q)).
if user of CorrelationArgFlag is on, Corr( user_input,r_i(x,Q))*)
(*==============================*)
corrtitle1="Corr( ";
corrdrtitle1=(*"\[Delta]r*Corr( ";*)"Sensitivity to ";
(*deltaRtitle1=(*"\[Delta]r ";*)(*"PDF error \[Delta]r for residuals, ";*)"\[Delta]r";*)
title2=(*", r(x,\[Mu]))";*)", \!\(\*SubscriptBox[\(r\), \(i\)]\))";
title3=(*" for dataset of "*)", "<>PDFname;
obsname="";(*initialize*)
pdfnamelable={"\!\(\*OverscriptBox[\(b\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(c\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(s\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(d\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(u\), \(_\)]\)(x,\[Mu])","g(x,\[Mu])","u(x,\[Mu])","d(x,\[Mu])","s(x,\[Mu])","c(x,\[Mu])","b(x,\[Mu])"
(*20171126: delete q6~q8*)
(*
,
"\!\(\*FractionBox[\(\*OverscriptBox[\(d\), \(_\)] \((x, \[Mu])\)\), \(\*OverscriptBox[\(u\), \(_\)] \((x, \[Mu])\)\)]\)","\!\(\*FractionBox[\(d \((x, \[Mu])\)\), \(u \((x, \[Mu])\)\)]\)","\!\(\*FractionBox[\(s \((x, \[Mu])\) + \*OverscriptBox[\(s\), \(_\)] \((x, \[Mu])\)\), \(\*OverscriptBox[\(u\), \(_\)] \((x, \[Mu])\) + \*OverscriptBox[\(d\), \(_\)] \((x, \[Mu])\)\)]\)",
*)(*Sequence@@UserArgName*)(*20171119 change to multi-user functions*)};
(*20171127: only add new string when user function is mode on*)
If[
CorrelationArgFlag[[-1]]==1,
pdfnamelable=pdfnamelable~Join~{Sequence@@UserArgName(*20171119 change to multi-user functions*)};
"dummy"
];

(*20171107: simplify title labels*)
If[
plottype>=1 && plottype<=6,
If[
plottype==1 ,
obsname="";
];
If[
plottype==2 ,
obsname="\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)/\!\(\*SubscriptBox[\(D\), \(i\)]\)";
title=obsname<>title3;
histabstitle="| "<>obsname<>" |"<>title3
];
If[
plottype==3 ,
obsname="\!\(\*SubscriptBox[\(r\), \(i\)]\) (Residual)";
title=obsname<>title3;
histabstitle="| "<>obsname<>" |"<>title3
];
If[
plottype==4 ,
obsname="\!\(\*SubscriptBox[\(\[Delta]r\), \(i\)]\)";
(*obsname=deltaRtitle1<>PDFname;*)
title=obsname<>title3;
histabstitle="| "<>obsname<>" |"<>title3
];
If[
plottype==5 ,
obsname=corrdrtitle1<>pdfnamelable[[flavourin+6]](*<>title2*);
title=obsname<>title3;
histabstitle="| "<>obsname<>" |"<>title3
];
If[
plottype==6 ,
obsname=corrtitle1<>pdfnamelable[[flavourin+6]]<>title2;
title=obsname<>title3;
histabstitle="| "<>obsname<>" |"<>title3
];

];

xtitle="x";
ytitle="\[Mu] [GeV]";
(*20171109: histogram title for data and |data|*)
histtitle=title;
xhisttitle=obsname;
yhisttitle="#points";

histabstitle=histabstitle;
xhistabstitle="| "<>obsname<>" |";
yhistabstitle=yhisttitle;

(*=============================================================================================================================*)
(*Set (x, y)-axis range of figures============================================================================================================*)
(*=============================================================================================================================*)
(*20171109: set different modes for auto x, y range of histogram*)
(*mode = 1, use some statistical quantity to estimate a reasonable x, y range*)
(*mode = 2, fix ranges depend on their figure type (observables of data)*)
(*mode = 3, zoom in the highlighted ranges*)
(*mode = 4, use the max, min of data as the range*)
HistAutoMode=4;
(*20171111: if the xrange of the histogram is fixed by static values, set true, ex: HistAutoMode=2*)
(*
HistAutoFixXrangeBool=False;
*)

(*decide xy range of xQ plot, Nbin of histogram, xy range of histogram by
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange*)
(*==============================*)
plotrange={XQfigureXrange,XQfigureYrange}//Flatten;

(*plotrangex=Hist1figureXrange;*)(*for histogram, how to deal with auto?*)
hist1plotrangex=Hist1figureXrange;

(*20171102 xQ plot auto range*)
If[
 plottype==1 || plottype==2 || plottype==3 || plottype==4 || plottype==5 || plottype==6 ,
xQautorange={{10^-5,1},{1,2000}}
];
If[plotrange[[1]]=="auto",plotrange[[1]]=xQautorange[[1,1]] ];
If[plotrange[[2]]=="auto",plotrange[[2]]=xQautorange[[1,2]] ];
If[plotrange[[3]]=="auto",plotrange[[3]]=xQautorange[[2,1]] ];
If[plotrange[[4]]=="auto",plotrange[[4]]=xQautorange[[2,2]] ];

(*20170515: for statistics of total data, the all data are considered, so here Flatten the data pdfcorr*)
(*20170515: for auto mode, correlation, deltaR, dR*correlation should have histogram of roughly 1*)
(*20171109: set auto the hist xrange depends on HistAutoMode*)
If[
HistAutoMode==1,
If[
plottype==2 (*|| plottype==3 || plottype==4 || plottype==5  || plottype==6*),
histautoxrange=3*AbsDataMedian(*Median[Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ] ]*);
(*20171114: histautoxrange format becomes {min,max}*)
histautoxrange={-histautoxrange,histautoxrange};
hist2autoxrange={0.0,histautoxrange[[2]]}
];
If[
plottype==3 || plottype==4 || plottype==5 ,
(*histautoxrange=Max[3*Median[Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ] ],1.0];*)
histautoxrange=Max[3*AbsDataMedian,1.0];
(*20171111: if the xrange of the histogram is fixed by static values, set true, ex: HistAutoMode=2*)
(*
If[histautoxrange>1.0 && hist1plotrangex[[1]]\[Equal]"auto" && hist1plotrangex[[2]]\[Equal]"auto",Hist1AutoFixXrangeBool=True];
If[histautoxrange>1.0 && hist2plotrangex[[1]]\[Equal]"auto" && hist2plotrangex[[2]]\[Equal]"auto",Hist2AutoFixXrangeBool=True]
*)
(*20171114: histautoxrange format becomes {min,max}*)
histautoxrange={-histautoxrange,histautoxrange};
hist2autoxrange={0.0,histautoxrange[[2]]}
];

"dummy"
];

If[
HistAutoMode==2,
If[
plottype==2 (*|| plottype==3 || plottype==4 || plottype==5  || plottype==6*),
histautoxrange=0.3;
(*20171114: histautoxrange format becomes {min,max}*)
histautoxrange={-histautoxrange,histautoxrange};
hist2autoxrange={0.0,histautoxrange[[2]]}
];
If[
plottype==3 || plottype==4 || plottype==5 ,
histautoxrange=3.0;
(*20171114: histautoxrange format becomes {min,max}*)
histautoxrange={-histautoxrange,histautoxrange};
hist2autoxrange={0.0,histautoxrange[[2]]}
];
(*20171111: if the xrange of the histogram is fixed by static values, set true, ex: HistAutoMode=2*)
(*
If[hist1plotrangex[[1]]\[Equal]"auto" && hist1plotrangex[[2]]\[Equal]"auto",Hist1AutoFixXrangeBool=True];
If[hist2plotrangex[[1]]\[Equal]"auto" && hist2plotrangex[[2]]\[Equal]"auto",Hist2AutoFixXrangeBool=True]
*)
"dummy"
];

If[
HistAutoMode==4,
If[
plottype==2 || plottype==3 || plottype==4 || plottype==5,
(*20171114: histautoxrange format becomes {min,max}*)
histautoxrange={DataMin,DataMax};
hist2autoxrange={AbsDataMin,AbsDataMax};
(*
histautoxrange={Min[Flatten[pdfcorr]/.LF1[a__]:>{a}[[3]] ],Max[Flatten[pdfcorr]/.LF1[a__]:>{a}[[3]] ]};
hist2autoxrange={Min[Flatten[pdfcorr]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ],Max[Flatten[pdfcorr]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ]}
*)
"dummy"
];

"dummy"
];

(*for correlation histogram, always set range (-1,1)*)
(*20171111: if the xrange of the histogram is fixed by static values, set true, ex: HistAutoMode=2*)
(*20171114: histautoxrange format becomes {min,max}*)
(*If[plottype==6,histautoxrange=1.0(*;HistAutoFixXrangeBool=True*)];*)
If[plottype==6,histautoxrange={-1.0,1.0};hist2autoxrange={0.0,1.0}];
(*test print*)(*Print["hist auto xrange: ",histautoxrange,", plottype & HistAutoMode",{plottype,HistAutoMode}];Abort[];*)
(*20171106: if highlight mode on, auto range set as the range of highlight*)(*20171108: ranges of all modes are the same*)
(*
If[
HighlightMode[[plottype]]==1 || HighlightMode[[plottype]]==2,
histautoxrange=1.1*highlightrange[[2]]
];
*)

If[
plottype==2 || plottype==3 || plottype==4 || plottype==5 || plottype==6,
(*20171114: histautoxrange format becomes {min,max}*)
hist2plotrangex=hist1plotrangex;
If[hist1plotrangex[[1]]=="auto",hist1plotrangex[[1]]=histautoxrange[[1]] ];
If[hist1plotrangex[[2]]=="auto",hist1plotrangex[[2]]=histautoxrange[[2]] ];
If[hist2plotrangex[[1]]=="auto",hist2plotrangex[[1]]=hist2autoxrange[[1]] ];
If[hist2plotrangex[[2]]=="auto",hist2plotrangex[[2]]=hist2autoxrange[[2]] ];
(*Print[histautoxrange,hist1plotrangex];*)

If[hist2plotrangex[[1]]<0.0,hist2plotrangex[[1]]=0.0];
"dummy"
];

(*20171126 draw plot type 1 and return the figure, end this function*)
If[
plottype==1,
Return[PlotDataTypeOne[corrfxQdtaobsclassin,pdfcorr[[1]],exptlist[[1]],plotrange,(*classifymode*)classifymode,configarguments] ]
];
(*=============================================================================================================================*)
(*Histograms: Nbin and hist yrange============================================================================================================*)
(*=============================================================================================================================*)
{hist1plotrangey,hist2plotrangey,BinWidth,hist1plotrange,hist2plotrange,highlightlines};
Hist1figureNbin=Hist1figureNbin;
hist1plotrangey=Hist1figureYrange;
hist2plotrangey=Hist1figureYrange;

(*20171111 Nbin auto*)
If[Hist1figureNbin=="auto",Hist1figureNbin=20];
(*20171111*)
(*set default y max by the max value of of the histogram*)
If[
 plottype==2 || plottype==3 || plottype==4 || plottype==5 || plottype==6,

BinWidth=(hist1plotrangex[[2]]-hist1plotrangex[[1]])/Hist1figureNbin;
If[hist1plotrangey[[1]]=="auto",hist1plotrangey[[1]]=0.0 ];
If[hist1plotrangey[[2]]=="auto",hist1plotrangey[[2]]=Max[HistogramList[Flatten[pdfcorr]/.LF1[a__]:>{a}[[3]],{BinWidth}][[2]] ] ];
(*set bin width of two histograms the same*)(*BinWidth=(hist2plotrangex[[2]]-hist2plotrangex[[1]])/Hist1figureNbin;*)
If[hist2plotrangey[[1]]=="auto",hist2plotrangey[[1]]=0.0 ];
If[hist2plotrangey[[2]]=="auto",hist2plotrangey[[2]]=Max[HistogramList[Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ],{BinWidth}][[2]] ] ];

hist1plotrange=Flatten[{hist1plotrangex,hist1plotrangey}];
hist2plotrange=Flatten[{hist2plotrangex,hist2plotrangey}];
"dummy"
];
(*correlation histogram*)

(*binset: for Nbin\[Equal]"auto", define auto binset*)
(*set auto bin as 10 bins in first color bar seperator *)
(*20171108: set auto bin as N = 20 in positive side of xrange, bin width = xrange(+)/20 *)
(*
binset={Table[i*barseperator[[Length[barseperator]/2+1]]/10.0,{i,-100,100}]};
*)
(*20111111: use new bins format in histplot5
binset={Table[i*hist1plotrangex[[2]]/20.0,{i,-100,100}]};
*)
(*=============================================================================================================================*)
(*2DxQ: color palette and color bar============================================================================================================*)
(*=============================================================================================================================*)
(*20171114: different mode for color palette*)
(*mode 1 depends on some statistical quantities of the data*)
(*mode 2: fix palette by plot type*)
(*mode 3: depend on highlight range*)
(*mode 4: depend on max and min of data*)
(*mode 5: depend on the range of histogram*)
(*mode 6: depend on the percentage of |data|*)
ColorPaletteMode=5;
(*20170515: for statistics of total data, the all data are considered, so here Flatten the data pdfcorr*)
If[
ColorPaletteMode==6,
If[
 plottype==2 || plottype==3 || plottype==4 || plottype==5 || plottype==6,
legendlabel="";
barseperator=GetDataPercentage[Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ] ,Join[ColorSeperator,{100}] ];(*20170606make sure 100% data included*)barseperator[[4]]=1.001*barseperator[[4]];
barseperator={-barseperator[[4]],-barseperator[[3]],-barseperator[[2]],-barseperator[[1]],barseperator[[1]],barseperator[[2]],barseperator[[3]],barseperator[[4]]};
epilogxQ={Npttext,(*maxtext,maxmarker,mintext,minmarker*)percentagetext};

"dummy"
(*corr plot cut by |data|<0.5*)
];

"dummy"
];

If[
ColorPaletteMode==2,
If[
plottype==6,
legendlabel="";
barseperator={-1,-0.85,-0.7,-0.5,0.5,0.7,0.85,1};
epilogxQ={Npttext};

"dummy"
(*corr plot cut by |data|<0.5*)
];
(*20170517: the most important of dr*corr is to show it's absolute value(how many data larger than 1)*)
If[
(*plottype==3 || plottype==4 || *)plottype==5,
legendlabel="";
(*20171025 set bar seperator as {-100,-5.0,-2.0,-0.5,0.5,2.0,5.0,100}*)
(*
barseperator={-100,-0.85,-0.7,-0.5,0.5,0.7,0.85,100};
*)
barseperator={-100,-5.0,-2.0,-0.5,0.5,2.0,5.0,100};
epilogxQ={Npttext};

"dummy"
];
(*10170606: deltaR & residual central: scale should be very small, close to 1, large, very large*)
If[
plottype==3 (*|| plottype==4*),
legendlabel="";
barseperator={-100,-5.0,-2.0,-0.5,0.5,2.0,5.0,100};
epilogxQ={Npttext};

"dummy"
];
(*20171108: residual error are always positive*)
If[
plottype==4,
legendlabel="";
barseperator={0,0.5,2.0,5.0,100};
epilogxQ={Npttext};

"dummy"
];

(*20171107: set seperator of \[Sigma]/D*)(*20171108: \[Sigma]/D are always positive*)
If[
plottype==2,
legendlabel="";
barseperator={(*-100,-0.3,-0.1,-0.05*)0,0.05,0.1,0.3,100};
epilogxQ={Npttext};
"dummy"
];

"dummy"
];

(*20171115 color palette by histogram range*)
If[
ColorPaletteMode==4 || ColorPaletteMode==5,
(*20171125: set max, min of the color palette range by mode*)
If[
ColorPaletteMode==4,
PaletteMax=DataMax;
PaletteMin=DataMin
];
If[
ColorPaletteMode==5,
PaletteMax=hist1plotrange[[2]];
PaletteMin=hist1plotrange[[1]]
];

If[
 (*plottype==3 || *)plottype==5,
legendlabel="";
(*20171125: use log scale to seperate the colors min, min/3, min/9, min/27, max/27,max/9,max/3,max*)
barseperator=Table[(DataMin+0.5*(DataMax-DataMin) )+If[isep>0,1.0,-1.0]*(1/3)^(Abs[isep]-1)*(0.5*(DataMax-DataMin) ),{isep,{-1,-2,-3,-4,4,3,2,1}}];
(*
barseperator=Table[DataMin+isep*(DataMax-DataMin)/7.0,{isep,0,7}];
*)
barseperator[[-1]]=barseperator[[-1]]*1.001;(*add a width*)
epilogxQ={Npttext};

"dummy"
(*corr plot cut by |data|<0.5*)
];

If[
 plottype==3(* || plottype==5*),
legendlabel="";
(*20171125: use log scale to seperate the colors min, min/3, min/9, min/27, max/27,max/9,max/3,max*)
barseperator=Table[(DataMin+0.5*(DataMax-DataMin) )+If[isep>0,1.0,-1.0]*(1/1.5)^(Abs[isep]-1)*(0.5*(DataMax-DataMin) ),{isep,{-1,-2,-3,-4,4,3,2,1}}];
(*
barseperator=Table[DataMin+isep*(DataMax-DataMin)/7.0,{isep,0,7}];
*)
barseperator[[-1]]=barseperator[[-1]]*1.001;(*add a width*)
epilogxQ={Npttext};

"dummy"
(*corr plot cut by |data|<0.5*)
];

If[
plottype==4,
legendlabel="";
(*20171125: use log scale to seperate the colors min, min/3, min/9, min/27, max/27,max/9,max/3,max*)
barseperator={0}~Join~Table[(1/1.5)^(Abs[isep]-1)*(DataMax ),{isep,{4,3,2,1}}];
(*
barseperator=Table[DataMin+isep*(DataMax-DataMin)/7.0,{isep,0,7}];
*)
barseperator[[-1]]=barseperator[[-1]]*1.001;(*add a width*)
epilogxQ={Npttext};

"dummy"
(*corr plot cut by |data|<0.5*)
];

If[
 plottype==2(* ||  plottype\[Equal]4*),
legendlabel="";
(*20171125: use log scale to seperate the colors {0, max/27,max/9,max/3,max}*)
barseperator={0}~Join~Table[(1/2)^(Abs[isep]-1)*(DataMax ),{isep,{4,3,2,1}}];
(*
barseperator=Table[isep*(AbsDataMax)/4.0,{isep,0,4}];
*)
barseperator[[-1]]=barseperator[[-1]]*1.001;(*add a width*)
epilogxQ={Npttext};

"dummy"
(*corr plot cut by |data|<0.5*)
];
(*
If[
 plottype\[Equal]4,
legendlabel="";
(*20171125: use log scale to seperate the colors {0, max/27,max/9,max/3,max}*)
barseperator={0}~Join~Table[DataMax-(1/2)^Abs[isep]*(DataMax ),{isep,{1,2,3,4}}];
(*
barseperator=Table[isep*(AbsDataMax)/4.0,{isep,0,4}];
*)
barseperator[[-1]]=barseperator[[-1]]*1.001;(*add a width*)
epilogxQ={Npttext};

"dummy"
(*corr plot cut by |data|<0.5*)
];
*)

If[
plottype==6,
legendlabel="";
barseperator={-1,-0.85,-0.7,-0.5,0.5,0.7,0.85,1};
epilogxQ={Npttext};

"dummy"
(*corr plot cut by |data|<0.5*)
];

"dummy"
];

(*plot type 1: just need barseperator so that function doesn't break*)
If[
plottype==1,
barseperator={-1,-0.85,-0.7,-0.5,0.5,0.7,0.85,1};
];
(*20170517: if highlight mode = 1 && not correlation data, then if highlight uplimit > max of data, automatically adjust it to the max of the data*)
(*20171106: don't choose max of data as highlightrange[[2]], just use the input number*)
(*
If[
HighlightMode[[plottype]]==1&&plottype!=6&&plottype!=1,
maxdata=Max[Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ] ];

If[
maxdata>highlightrange[[1]],
highlightrange[[2]]=Min[highlightrange[[2]],maxdata];
];
"dummy" ];
*)

(*20171106 set highlight barlegend*)(*20171108: all modes use the same color palette (barlegend)*)
legendlabel="";
If[
(HighlightMode[[plottype]]==0 || HighlightMode[[plottype]]==1 || HighlightMode[[plottype]]==2),
If[
(plottype==3|| plottype==5  || plottype==6),
barlegend=Set2DxqBarLegend[barseperator,legendlabel] 
];
If[
(plottype==2 || plottype==4),
barlegend=Set2DxqRedBarLegend[barseperator,legendlabel] 
];
"dummy"
];
(*
If[HighlightMode[[plottype]]\[Equal]1 || HighlightMode[[plottype]]\[Equal]2,barlegend=(*Set2DxqHighlightBarLegend[legendlabel,highlightrange]*)Set2DxqBarLegend[barseperator,legendlabel] ];
*)

(*=============================================================================================================================*)
(*2DxQ: size of points============================================================================================================*)
(*=============================================================================================================================*)

(*for highlight mode, always only have no choice of size*)
If[HighlightMode[[plottype]]==1 || HighlightMode[[plottype]]==2,Size=(*20171126\[Rule]tiny*)(*"small"*)"tiny"];
(*set size*)
unhighlightsize=
Switch[Size,"tiny",0.005,"small",0.0075,"medium",0.01,"large",0.0125,_,Print["error,size type is not correct"];Abort[] ];

(*=============================================================================================================================*)
(*2DxQ: legend============================================================================================================*)
(*=============================================================================================================================*)

(*make expt name & ID table*)
(*==============================*)

(*output*)
(*==============================*)
title;
(*
GraphicsGrid[{{xQplotcorr},{histplotcorr1,histplotcorr2}}];
{{xQplotcorr},{histplotcorr1,histplotcorr2}};
*)

(*make  exptname table*)
(*20170515: when show all experiments, show expts of all groups*)
(*20171109: plottype1 don't generate the legend*)
If[
 plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,
rows=3;
(*20171126: set labels depend on different classifymode*)
If[
classifymode=="single",
exptnames=Table[ToString[PlotMarkerList[][[1,iexpt]] ]<>ExptIDtoName[Flatten[exptlist][[iexpt]] ]<>"("<>ToString[Flatten[exptlist][[iexpt]] ]<>")",{iexpt,1,Length[exptlist//Flatten]}];
"dummy"
];
If[
classifymode=="all",
exptnames=Table[ExptIDtoName[Flatten[exptlist][[iexpt]] ]<>"("<>ToString[Flatten[exptlist][[iexpt]] ]<>")",{iexpt,1,Length[exptlist//Flatten]}];
"dummy"
];
Print["making table of experiments included in plots"];
datemode=False;
exptnamestable=makeGrid2[exptnames,rows,title<>"\n\n",datemode];
"dummy"
];

(*=============================================================================================================================*)
(*2DxQ: epilogs============================================================================================================*)
(*=============================================================================================================================*)

(*setup texts and lines in plots*)
(*==============================*)
(*set outlayer points label in plot*)
textsize=16;
Npttext=Text[Style["Npt: "<>ToString[Length[pdfcorr//Flatten] ],textsize,Black],Scaled[{0.1,0.9}] ];
maxtext=Text[Style[ToString[ColorSeperator[[3]] ]<>"%",textsize,Black],Scaled[{0.1,0.8}] ];
maxmarker={Red,PointSize->0.02,Point[Scaled[{0.175,0.8}] ]};
mintext=Text[Style[ToString[ColorSeperator[[3]] ]<>"%(-)",textsize,Black],Scaled[{0.1,0.7}] ];
minmarker={Blue,PointSize->0.02,Point[Scaled[{0.175,0.7}] ]};
cuttext=Text[Style["cut: |data|<0.5",textsize,Black],Scaled[{0.15,0.6}] ];
percentagetext=Text[Style["percentage of colors:\n"<>ToString[ColorSeperator[[1]] ]<>"%"<>ToString[ColorSeperator[[2]] ]<>"%"<>ToString[ColorSeperator[[3]] ]<>"%",textsize,Black],Scaled[{0.2,0.8}] ];


If[
HighlightMode[[plottype]]==1,
highlighttext=Text[Style["highlighted range:\n"<>ToString[highlightrange],textsize,Black],Scaled[{0.2,0.7}] ];
"dummy"
];
(*20171108: only show 3 digits *)
If[
HighlightMode[[plottype]]==2,
highlighttext=
(*20171109 use new highlight convention
Text[Style["highlighted range:\n"<>"("<>ToString[HighlightMode2[[2*plottype-1]] ]<>"% ~ "<>ToString[HighlightMode2[[2*plottype]] ]<>"%)\n"<>ToString[NumberForm[highlightrange,{100,2}] ],textsize,Black],Scaled[{0.2,0.6}] ];
*)
(*20171111: percentage text is long. move it to right-up to avoid the text cutted by edge*)
Text[Style["highlighted range:\n"<>ToString[Map[(ToString[#]<>"%")&,HighlightMode2[[plottype]],{2}] ]<>"\n"<>ToString[NumberForm[highlightrange,{100,2}] ],textsize,Black],Scaled[{0.25,0.7}] ];
"dummy"
];
If[HighlightMode[[plottype]]!=0,epilogxQ=Append[epilogxQ,highlighttext] ];


(*=============================================================================================================================*)
(*Histograms: epilogs============================================================================================================*)
(*=============================================================================================================================*)
{xmintmp,xmaxtmp,ymintmp,hist1epilogtext,hist2epilogtext};
(*2011111: set lines to represent the highlight ranges *)
If[
 plottype==2 || plottype==3 || plottype==4 || plottype==5 || plottype==6,
LineWidth=Thick;
highlightlines=
Table[
xmintmp=highlightrange[[irange]][[1]];
xmaxtmp=highlightrange[[irange]][[2]];
(*set lines at x axis*)
ymintmp=hist1plotrange[[3]];
Style[Line[{{xmintmp,ymintmp},{xmaxtmp,ymintmp} }],Darker[Blue,0.3] ],
{irange,Length[highlightrange]}
];
highlightlines=Prepend[highlightlines,LineWidth];
"dummy"
];

If[
plottype==2,
(*20171108: \[Sigma]/D only has posive data*)
(*
lineelement={{barseperator[[2]],ToString[ColorSeperator[[3]] ]<>"%",Blue},{barseperator[[3]],ToString[ColorSeperator[[2]] ]<>"%",Blue},{barseperator[[4]],ToString[ColorSeperator[[1]] ]<>"%",Blue},{barseperator[[5]],ToString[ColorSeperator[[1]] ]<>"%",Blue},{barseperator[[6]],ToString[ColorSeperator[[2]] ]<>"%",Blue},{barseperator[[7]],ToString[ColorSeperator[[3]] ]<>"%",Blue}};
*)
lineelement={{-0.1,"",Darker[Red,0.3]},{0.1,"",Darker[Red,0.3]}};
lineelement2=(*Take[lineelement,-3]*){{0.1,"",Darker[Red,0.3]}};
];
(*if correlation histogram, don't need show lines to represent the % of data *)
If[ plottype==3 || plottype==4 || plottype==5,lineelement={{-1,"",Darker[Red,0.3]},{1,"",Darker[Red,0.3]}};lineelement2={{1,"",Darker[Red,0.3]}};"dummy"];
If[ plottype==6,lineelement={{-0.7,"",Darker[Red,0.3]},{0.7,"",Darker[Red,0.3]}};lineelement2={{0.7,"",Darker[Red,0.3]}};"dummy"];

(*20171106: bin line in histogram for highlight mode*)(*20171108: don't add it presently*)
(*
If[
HighlightMode[[plottype]]\[Equal]1 || HighlightMode[[plottype]]\[Equal]2,
lineelement={{-highlightrange[[1]],"",Red},{-highlightrange[[2]],"",Red},{highlightrange[[1]],"",Red},{highlightrange[[2]],"",Red}};
lineelement2={{highlightrange[[1]],"",Red},{highlightrange[[2]],"",Red}}; 
binset={Table[i*(highlightrange[[2]]-highlightrange[[1]])/20.0,{i,-100,100}]}
(*
binset={Table[i*barseperator[[Length[barseperator]/2+1]]/10.0,{i,-100,100}]};(*test*)
*)
];
*)
(*20171111 set lines for y direction*)
If[
 plottype==2 || plottype==3 || plottype==4 || plottype==5 || plottype==6,
hist1standardlines=setxlineinplot2[Flatten[pdfcorr]/.LF1[a__]:>{a}[[3]],lineelement,{hist1plotrange[[3]],hist1plotrange[[4]]}];
hist2standardlines=setxlineinplot2[Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ],lineelement2,{hist2plotrange[[3]],hist2plotrange[[4]]}];
hist1standardlines=Prepend[hist1standardlines,LineWidth];
hist2standardlines=Prepend[hist2standardlines,LineWidth];
"dummy"
];
(*=============================================================================================================================*)
(*2DxQ: generate plots============================================================================================================*)
(*=============================================================================================================================*)

stretchx=1;stretchy=1;

If[PlotTitleBool==False,title=""];
If[
 plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,
xQplotcorr=PDFCorrelationplot8[pdfcorr,title,xtitle,ytitle,plotrange,stretchx,stretchy,barlegend,(*barseperator,legendlabel,*)(*Append[epilogxQ,cuttext]*)epilogxQ,highlightrange,unhighlightsize ];
"dummy"
];

(*=============================================================================================================================*)
(*Histogram: generate plots============================================================================================================*)
(*=============================================================================================================================*)
(*20171111 new histplot function*)
hist1epilogtext={hist1standardlines,highlightlines,Npttext};
hist2epilogtext={hist2standardlines,highlightlines,Npttext};
(*20171111 if xrange of histograms are fixed, then we don't need to show red lines because red lines are one the boarder of the figure*)
(*
If[
(HistAutoFixXrangeBool==True) && ( plottype==2 || plottype==3 || plottype==4 || plottype==5),
hist1epilogtext={highlightlines,Npttext};
hist2epilogtext={highlightlines,Npttext};
"dummy"
];
*)
(*20171111: check whether the red lines are on the boarder of (or in the) figure frames*)
(*if it is on the boarder or larger than the boarder, don't draw it*)
If[
 plottype==2,
If[ hist1plotrange[[2]]<=0.1 &&  hist1plotrange[[1]]>=-0.1,hist1epilogtext={highlightlines,Npttext}];
If[ hist2plotrange[[2]]<=0.1,hist2epilogtext={highlightlines,Npttext}];
"dummy"
];
If[
( plottype==3 || plottype==4 || plottype==5),
If[ hist1plotrange[[2]]<=1.0 &&  hist1plotrange[[1]]>=-1.0,hist1epilogtext={highlightlines,Npttext}];
If[ hist2plotrange[[2]]<=1.0,hist2epilogtext={highlightlines,Npttext}];
"dummy"
];


If[
 plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,
(*hist1: data with no absolute*)
(*20171111: use new histplot5 function*)
(*
(*20170515: temporary use data of all groups to draw the histogram in one color*)
histplotcorr1=histplot4[Flatten[pdfcorr]/.LF1[a__]:>{a}[[3]],histtitle,xhisttitle,yhisttitle,binset,lineelement,hist1plotrangex,Hist1figureNbin];
(*hist1: data with absolute(|data|)*)
histplotcorr2=histplot4[Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ],histabstitle,xhistabstitle,yhistabstitle,binset,(*Take[lineelement,-3]*)lineelement2,hist2plotrangex,Hist1figureNbin];
*)
histplotcorr1=histplot5[Flatten[pdfcorr]/.LF1[a__]:>{a}[[3]],histtitle,xhisttitle,yhisttitle,hist1plotrange,BinWidth,hist1epilogtext];
histplotcorr2=histplot5[Flatten[pdfcorr]/.LF1[a__]:>Abs[{a}[[3]] ],histabstitle,xhistabstitle,yhistabstitle,hist2plotrange,BinWidth,hist2epilogtext];
"dummy"
];

(*=============================================================================================================================*)
(*plot type 1: generate plots============================================================================================================*)
(*=============================================================================================================================*)

If[
 plottype==1,
(*plot1*)

expttype="multi";
(*20170515 groups of data, legend is exptids in all groups, using Flatten, PDFname should be took cared*)
myplotsetting=setplotsetting[Flatten[corrfxQdtaobsclassin,1],exptlist//Flatten,expttype,1,"test","test"];
imgsize=myplotsetting[["imgsize"]];
title=myplotsetting[["title"]];
xtitle=myplotsetting[["xtitle"]];
ytitle=myplotsetting[["ytitle"]];
lgdlabel=myplotsetting[["lgdlabel"]];
xrange=myplotsetting[["xrange"]];
yrange=myplotsetting[["yrange"]];
epilog=myplotsetting[["epilog"]];
titlesize=myplotsetting[["titlesize"]];
xtitlesize=myplotsetting[["xtitlesize"]];
ytitlesize=myplotsetting[["ytitlesize"]];
lgdlabelsize=myplotsetting[["lgdlabelsize"]];
ticklablesize=myplotsetting[["ticklablesize"]];

myplotstyle=myplotsetting[["plotstyle"]];
myMarker=myplotsetting[["marker"]];

title="Experimental data in "<>PDFname<>" analysis";
lgdpos={0.25,0.725};
xyrangeplot1=plotrange;(*20170307 change it's name, avoid duplicate*)
(*20170515: consider expts in all groups*)
(*20170515: consider expts of all groups, so use Flatten[data,1] *)

(*
Print["dim of p1: ",Dimensions[pdfcorr] ];
Print["dim of p1: ",Dimensions[Flatten[pdfcorr,1] ]];
Print["data of p1: ",Flatten[pdfcorr,1][[1]] ];
Print["data of p1: ",Flatten[pdfcorr,1][[2]] ];
Print["data of p1: ",Flatten[pdfcorr,1][[3]] ];
Print["data of p1: ",Flatten[pdfcorr,1][[4]] ];
PDFloglogplot[Flatten[pdfcorr,1]/.LF1\[Rule]List,myMarker,myplotstyle,title,xtitle,ytitle,xyrangeplot1,lgdlabel,lgdpos,imgsize];
Abort[];
*)

plot1=PDFloglogplot[Flatten[pdfcorr,1],myMarker,myplotstyle,title,xtitle,ytitle,xyrangeplot1,lgdlabel,lgdpos,imgsize];
];

(*=============================================================================================================================*)
(*Output============================================================================================================*)
(*=============================================================================================================================*)

Which[
 plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,
{{xQplotcorr,exptnamestable},{histplotcorr1,histplotcorr2}},
plottype==1,
{plot1}
]

(*=============================================================================================================================*)
(*End============================================================================================================*)
(*=============================================================================================================================*)


(*base on FigureFlag, decide the plot type of output plots (which data is used to plot), 
ex: if correlation flag = 1, use correlation data*)
(*==============================*)


(*btw test*)(*Print["test: ",plottype];*)

(*decide the data include by ExptidFlag << perhaps could be done before function*)
(*==============================*)
(*make text for Npt of data*)





(*
If[
plottype\[Equal]5  || plottype\[Equal]6,
histautoxrange=3*Median[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ];
];
*)



(*for correlation, set color seperator by 0.5, 0.7, 0.85, 1*)
(*for uncertainty of theory, experiment, also 0.5, 0.7, 0.85, 1*)
(*for residual central value, deltaR, dr*corr, since value could be > 1 and even very large
color seperator decided by ColorSeperator*)
(*==============================*)
(*
If[
plottype\[Equal]5,
legendlabel="";
barseperator=GetDataPercentage[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ,Join[ColorSeperator,{100}] ];
barseperator={-barseperator[[4]],-barseperator[[3]],-barseperator[[2]],-barseperator[[1]],barseperator[[1]],barseperator[[2]],barseperator[[3]],barseperator[[4]]};
epilogxQ={Npttext,(*maxtext,maxmarker,mintext,minmarker*)percentagetext};

"dummy"
(*corr plot cut by |data|<0.5*)
];
*)
(*same as plottype=5, but data strucure pdfcorr is different*)





(*Print["max of data and highlight: ",{highlightrange[[2]],maxdata}];*)





(*Print["highlight range: ",highlightrange];*)

(*for histogram, setup highlighted value range by red line and color seperator value by blue line*)
(*make histogram of data and |data|*)
(*==============================*)

(*set xtitle by observable part of title, ex: |Corr(f_j(x,Q),r_i(x,Q))|*)
(*==============================*)


(*plot x,Q data by size for all quarks(CorrelationArgFlag), and plot corresponding histogram*)
(*GridGraphic x,Q data by size and histograms*)
(*==============================*)


(*correlation plot*)
(*
If[
plottype\[Equal]5  || plottype\[Equal]6,
xQplotcorr=PDFCorrelationplot8[pdfcorr[[flavourin+6]],title,xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,(*Append[epilogxQ,cuttext]*)epilogxQ,highlightrange,unhighlightsize ];
"dummy"
];
*)
(*test print*)
(*
Print["test print"];
Print["highlight mode 1"];
Print[pdfcorr];
Print[HighlightMode1];
Print[{xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,epilogxQ,highlightrange,unhighlightsize }];
*)



(*lineelement={{barseperator[[2]],"",Blue},{barseperator[[3]],"",Blue},{barseperator[[4]],"",Blue},{barseperator[[5]],"",Blue},{barseperator[[6]],"",Blue},{barseperator[[7]],"",Blue}};*)

(*test*)(*Print[binset];Print[lineelement2];*)

(*20170307: bin from argument*)
(*
If[
Hist1figureNbin=="auto",
binset=barseperator;
binset=Insert[binset,0.0,5]
];
*)
(*
If[
plottype\[Equal]5  || plottype\[Equal]6,
(*hist1: data with no absolute*)
histplotcorr1=histplot4[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]{a}[[3]],title,xhisttitle,yhisttitle,binset,lineelement,hist1plotrangex,Hist1figureNbin];
(*hist1: data with absolute(|data|)*)
histplotcorr2=histplot4[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ],title,xhistabstitle,yhisttitle,binset,Take[lineelement,-3],hist2plotrangex,Hist1figureNbin];
"dummy"
];
*)


]


(* ::Input::Initialization:: *)
(*modify of 3: when plottype = 5, 6, extract data of that flavour*)
(*modify of 4: don't set local variable of corrfxQdtaobsclassin, avoiding time to copy large data to local variable, for mode 5,6 only deal with 
corresponding flavour data (by flavourin)*)
processdataplotsmultiexp5percentageGrid[corrfxQdtaobsclassin_,configargumentsin_,plottypein_,flavourin_]:=
Module[{(*corrfxQdtaobsclass=corrfxQdtaobsclassin,*)configarguments=configargumentsin,
plottype=plottypein,(*flavour=flavourin,*)flavour,
Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,
ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2,
processes,ExptList1,pdfsetexps,processestitle,PDISNCtitle,NDISCCtitle,PDISNCCCtitle,PVBPZtitle,PVBPWtitle,PJPtitle,hDISNCtitle,hDISCCtitle,hVBPZtitle,pdfnamelable,PDFsetlabel,pdffile,corrfile,pdfcorr,pdfcorrdr,deltaR,textsize,Npttext,maxtext,maxmarker,mintext,minmarker,cuttext,epilogxQ,epilogxQcorr,corrtitle1,corrdrtitle1,deltaRtitle1,title2,obsname,title3,title4,title,xtitle,ytitle,xhisttitle,xhistabstitle,yhisttitle,plotrange,stretchx,stretchy,legendlabel,barseperator,binset,lineelement,plotrangex,SM,SM1,SM2,SM3,xQplotcorr ,histplotcorr1,histplotcorr2,xQplotcorrdr,histplotcorrdr1,histplotcorrdr2,xQplotdr,histplotdr2,myexpids,fmax,fmax2,
imgsize,(*title,xtitle,ytitle,*)lgdlabel,xrange,yrange,epilog,titlesize,xtitlesize,ytitlesize,lgdlabelsize,ticklablesize,
myplotstyle,myMarker,
lgdpos,xyrangeplot1,
myplotsetting,plot1data,plot1,processesplot1order,
dummy1,dummy2,percentagetext,hist1plotrangex,histautoxrange,hist2plotrangex,unhighlightsize,highlightrange,highlighttext,
exptlist,expttype,
rows,exptnames,exptnamestable},
(*read arguments in config file*)
(*==============================*)
{Jobid,PDFname,FigureType,FigureFlag,ExptidType,ExptidFlag,CorrelationArgType,CorrelationArgFlag,UserArgName,UserArgValue,
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange,
ColorSeperator,
Size,HighlightType,HighlightMode,HighlightMode1,HighlightMode2}=configarguments;

(*read exptlist*)
exptlist={};
If[plottype==1  || plottype==5  || plottype==6,exptlist=Table[corrfxQdtaobsclassin[[iexpt,6]][["exptinfo","exptid"]],{iexpt,1,Length[corrfxQdtaobsclassin]}] ];
If[plottype==2  || plottype==3  || plottype==4,exptlist=Table[corrfxQdtaobsclassin[[iexpt,6]][["exptinfo","exptid"]],{iexpt,1,Length[corrfxQdtaobsclassin]}] ];
(*base on FigureFlag, decide the plot type of output plots (which data is used to plot), 
ex: if correlation flag = 1, use correlation data*)
(*==============================*)

(*decide title by PDFname, FigureFlag, CorrelationArgFlag, ex: Corr(f_j(x,Q),r_i(x,Q)).
if user of CorrelationArgFlag is on, Corr( user_input,r_i(x,Q))*)
(*==============================*)
corrtitle1="Corr( ";
corrdrtitle1=(*"\[Delta]r*Corr( ";*)"Sensitivity to ";
deltaRtitle1=(*"\[Delta]r ";*)"PDF error \[Delta]r for residuals, ";
title2=", r(x,\[Mu]))";
title3=" for dataset of "<>PDFname;
obsname="";(*initialize*)
pdfnamelable={"\!\(\*OverscriptBox[\(b\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(c\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(s\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(d\), \(_\)]\)(x,\[Mu])","\!\(\*OverscriptBox[\(u\), \(_\)]\)(x,\[Mu])","g(x,\[Mu])","u(x,\[Mu])","d(x,\[Mu])","s(x,\[Mu])","c(x,\[Mu])","b(x,\[Mu])","\!\(\*FractionBox[\(\*OverscriptBox[\(d\), \(_\)] \((x, \[Mu])\)\), \(\*OverscriptBox[\(u\), \(_\)] \((x, \[Mu])\)\)]\)","\!\(\*FractionBox[\(d \((x, \[Mu])\)\), \(u \((x, \[Mu])\)\)]\)","\!\(\*FractionBox[\(s \((x, \[Mu])\) + \*OverscriptBox[\(s\), \(_\)] \((x, \[Mu])\)\), \(\*OverscriptBox[\(u\), \(_\)] \((x, \[Mu])\) + \*OverscriptBox[\(d\), \(_\)] \((x, \[Mu])\)\)]\)",UserArgName};

If[
plottype>=1 && plottype<=6,
If[
plottype==1 ,
obsname="";
];
If[
plottype==2 ,
obsname="Expt Uncertainty Ratio";
title=obsname<>title3;
];
If[
plottype==3 ,
obsname="Residual(central value)";
title=obsname<>title3;
];
If[
plottype==4 ,
obsname=deltaRtitle1<>PDFname;
title=obsname;
];
If[
plottype==5 ,
obsname=corrdrtitle1<>pdfnamelable[[flavourin+6]](*<>title2*)<>", "<>PDFname;
title=obsname;
];
If[
plottype==6 ,
obsname=corrtitle1<>pdfnamelable[[flavourin+6]]<>title2;
title=obsname<>title3;
];

];


xtitle="x";
ytitle="\[Mu] [GeV]";
xhisttitle=obsname;
xhistabstitle="| "<>obsname<>" |";
yhisttitle="#points";

(*btw test*)(*Print["test: ",plottype];*)

(*decide the data include by ExptidFlag << perhaps could be done before function*)
(*==============================*)
(*make text for Npt of data*)
(*********************************)
(*prepare for data input to processdataplotsmultiexp*)
(*********************************)
(*transf format from LF to LF1, since plot functions use LF1*)

(*if dr*corr or corr, data is [[iexpt,flavour]]*)
If[
plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,
fmax=Length[corrfxQdtaobsclassin[[1]] ];

(*data format \[Equal] {LF[x,Q,obs],...,...}, to LF1*)
pdfcorr=Table[corrfxQdtaobsclassin[[iexpt,flavourin+6]][["data"]]/.LF->LF1,{iexpt,1,Length[corrfxQdtaobsclassin]}];

(*merge all experimental data into one, for all flavours*)
(*ex: corrdataforplot[[iexpt,flavour,Npt]] \[Rule] orrdataforplot[[flavour,Npt]]*)
(*{pdfcorr ,dummy1,dummy2}=mergeplotdata[{pdfcorr ,pdfcorr,pdfcorr}];*)
pdfcorr=Flatten[pdfcorr,1];

(* deletezerovalue: delete data with 0 value (0 value usually from mb, mc cut)*)
(*{pdfcorr ,dummy1,dummy2}=deletezerovalue[{pdfcorr ,pdfcorr,pdfcorr}];*)
pdfcorr=Select[pdfcorr,#[[3]]!=0&];
"dummy"
];

If[
plottype==1,
fmax=Length[corrfxQdtaobsclassin[[1]] ];

(*set {corr[[flavour]],drcorr[[flavour]],dr[[flavour]]}*)
(*they are used to  input into processdataplotsmultiexp*)
(*data format \[Equal] {LF[x,Q,obs],...,...}*)
pdfcorr=Table[Datamethods[["take"]][corrfxQdtaobsclassin[[iexpt,flavourin+6]],2][["data"]]/.LF->LF1,{iexpt,1,Length[corrfxQdtaobsclassin]}];
];

(*decide xy range of xQ plot, Nbin of histogram, xy range of histogram by
XQfigureXrange,XQfigureYrange,Hist1figureNbin,Hist1figureXrange,Hist1figureYrange*)
(*==============================*)
plotrange={XQfigureXrange,XQfigureYrange}//Flatten;

(*plotrangex=Hist1figureXrange;*)(*for histogram, how to deal with auto?*)
hist1plotrangex=Hist1figureXrange;
(*
If[
plottype\[Equal]5  || plottype\[Equal]6,
histautoxrange=3*Median[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ];
];
*)
If[
plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,
histautoxrange=3*Median[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ] ];
];
If[hist1plotrangex[[1]]=="auto",hist1plotrangex[[1]]=-histautoxrange];
If[hist1plotrangex[[2]]=="auto",hist1plotrangex[[2]]=histautoxrange];
(*Print[histautoxrange,hist1plotrangex];*)

hist2plotrangex=hist1plotrangex;If[hist2plotrangex[[1]]<0.0,hist2plotrangex[[1]]=0.0];
(*for correlation histogram, set range (-1,1)*)
If[plottype==6,hist1plotrangex={-1,1};hist2plotrangex={0,1};];
stretchx=1;stretchy=1;
Hist1figureNbin=Hist1figureNbin;

(*setup texts and lines in plots*)
(*==============================*)
(*set outlayer points label in plot*)
textsize=16;
Npttext=Text[Style["Npt: "<>ToString[Length[pdfcorr//Flatten] ],textsize,Black],Scaled[{0.1,0.9}] ];
maxtext=Text[Style[ToString[ColorSeperator[[3]] ]<>"%",textsize,Black],Scaled[{0.1,0.8}] ];
maxmarker={Red,PointSize->0.02,Point[Scaled[{0.175,0.8}] ]};
mintext=Text[Style[ToString[ColorSeperator[[3]] ]<>"%(-)",textsize,Black],Scaled[{0.1,0.7}] ];
minmarker={Blue,PointSize->0.02,Point[Scaled[{0.175,0.7}] ]};
cuttext=Text[Style["cut: |data|<0.5",textsize,Black],Scaled[{0.15,0.6}] ];
percentagetext=Text[Style["percentage of colors:\n"<>ToString[ColorSeperator[[1]] ]<>"%"<>ToString[ColorSeperator[[2]] ]<>"%"<>ToString[ColorSeperator[[3]] ]<>"%",textsize,Black],Scaled[{0.2,0.8}] ];

(*for correlation, set color seperator by 0.5, 0.7, 0.85, 1*)
(*for uncertainty of theory, experiment, also 0.5, 0.7, 0.85, 1*)
(*for residual central value, deltaR, dr*corr, since value could be > 1 and even very large
color seperator decided by ColorSeperator*)
(*==============================*)
(*
If[
plottype\[Equal]5,
legendlabel="";
barseperator=GetDataPercentage[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ,Join[ColorSeperator,{100}] ];
barseperator={-barseperator[[4]],-barseperator[[3]],-barseperator[[2]],-barseperator[[1]],barseperator[[1]],barseperator[[2]],barseperator[[3]],barseperator[[4]]};
epilogxQ={Npttext,(*maxtext,maxmarker,mintext,minmarker*)percentagetext};

"dummy"
(*corr plot cut by |data|<0.5*)
];
*)
(*same as plottype=5, but data strucure pdfcorr is different*)
If[
 plottype==2 || plottype==3 || plottype==4 || plottype==5,
legendlabel="";
barseperator=GetDataPercentage[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ] ,Join[ColorSeperator,{100}] ];
barseperator={-barseperator[[4]],-barseperator[[3]],-barseperator[[2]],-barseperator[[1]],barseperator[[1]],barseperator[[2]],barseperator[[3]],barseperator[[4]]};
epilogxQ={Npttext,(*maxtext,maxmarker,mintext,minmarker*)percentagetext};

"dummy"
(*corr plot cut by |data|<0.5*)
];

If[
plottype==6,
legendlabel="";
barseperator={-1,-0.85,-0.7,-0.5,0.5,0.7,0.85,1};
epilogxQ={Npttext};

"dummy"
(*corr plot cut by |data|<0.5*)
];
(*plot type 1: just need barseperator so that function doesn't break*)
If[
plottype==1,
barseperator={-1,-0.85,-0.7,-0.5,0.5,0.7,0.85,1};
];

(*for no highlight mode, choose size of data point in plot by Size
for highlight mode, set size of unhighlighted data as Size, size of highlighted data is larger than Size*)
(*==============================*)
(*Print[HighlightMode];*)

highlightrange=
Switch[
HighlightMode[[plottype]],
0,{0.0,0.0},
1,{HighlightMode1[[2*plottype-1]],HighlightMode1[[2*plottype]]},
2,GetDataPercentage[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ] ,{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}]
(*
Which[
plottype\[Equal]5  || plottype\[Equal]6,
GetDataPercentage[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ,{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}],
 plottype\[Equal]2 || plottype\[Equal]3 || plottype\[Equal]4,
GetDataPercentage[pdfcorr/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ] ,{HighlightMode2[[2*plottype-1]],HighlightMode2[[2*plottype]]}],
True,Print["presently plottype is only 2~6 "];Abort[]
]*),
_,Print["error, highlight mode should be 0, 1, 2"];Abort[]
];
(*20171106: if percentage highlight, add a small width to avoid data problems at boundary *)
If[HighlightMode[[plottype]]==2,highlightrange[[1]]=0.99999*highlightrange[[1]];highlightrange[[2]]=1.00001*highlightrange[[2]] ];

(*for highlight mode, always only have no choice of size*)
If[HighlightMode[[plottype]]==1 || HighlightMode[[plottype]]==2,Size="small"];
(*set size*)
unhighlightsize=
Switch[Size,"tiny",0.005,"small",0.0075,"medium",0.01,"large",0.0125,_,Print["error,size type is not correct"];Abort[] ];

highlighttext=Text[Style["highlight range:\n"<>ToString[highlightrange],textsize,Black],Scaled[{0.2,0.7}] ];
If[HighlightMode[[plottype]]!=0,epilogxQ=Append[epilogxQ,highlighttext] ];

(*Print["highlight range: ",highlightrange];*)

(*for histogram, setup highlighted value range by red line and color seperator value by blue line*)
(*make histogram of data and |data|*)
(*==============================*)

(*set xtitle by observable part of title, ex: |Corr(f_j(x,Q),r_i(x,Q))|*)
(*==============================*)


(*plot x,Q data by size for all quarks(CorrelationArgFlag), and plot corresponding histogram*)
(*GridGraphic x,Q data by size and histograms*)
(*==============================*)


(*correlation plot*)
(*
If[
plottype\[Equal]5  || plottype\[Equal]6,
xQplotcorr=PDFCorrelationplot7[pdfcorr[[flavourin+6]],title,xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,(*Append[epilogxQ,cuttext]*)epilogxQ,highlightrange,unhighlightsize ];
"dummy"
];
*)
(*test print*)
(*
Print["test print"];
Print["highlight mode 1"];
Print[pdfcorr];
Print[HighlightMode1];
Print[{xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,epilogxQ,highlightrange,unhighlightsize }];
*)

If[
 plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,
xQplotcorr=PDFCorrelationplot7[pdfcorr,"",xtitle,ytitle,plotrange,stretchx,stretchy,barseperator,legendlabel,(*Append[epilogxQ,cuttext]*)epilogxQ,highlightrange,unhighlightsize ];
"dummy"
];
(*correlation histogram*)

(*binset: for Nbin\[Equal]"auto", define auto binset*)
(*set auto bin as 5 bins in first color bar seperator *)
binset={Table[i*barseperator[[Length[barseperator]/2+1]]/5.0,{i,-100,100}]};

(*lineelement={{barseperator[[2]],"",Blue},{barseperator[[3]],"",Blue},{barseperator[[4]],"",Blue},{barseperator[[5]],"",Blue},{barseperator[[6]],"",Blue},{barseperator[[7]],"",Blue}};*)
lineelement={{barseperator[[2]],ToString[ColorSeperator[[3]] ]<>"%",Blue},{barseperator[[3]],ToString[ColorSeperator[[2]] ]<>"%",Blue},{barseperator[[4]],ToString[ColorSeperator[[1]] ]<>"%",Blue},{barseperator[[5]],ToString[ColorSeperator[[1]] ]<>"%",Blue},{barseperator[[6]],ToString[ColorSeperator[[2]] ]<>"%",Blue},{barseperator[[7]],ToString[ColorSeperator[[3]] ]<>"%",Blue}};
(*20170307: bin from argument*)
(*
If[
Hist1figureNbin=="auto",
binset=barseperator;
binset=Insert[binset,0.0,5]
];
*)
(*
If[
plottype\[Equal]5  || plottype\[Equal]6,
(*hist1: data with no absolute*)
histplotcorr1=histplot4[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]{a}[[3]],title,xhisttitle,yhisttitle,binset,lineelement,hist1plotrangex,Hist1figureNbin];
(*hist1: data with absolute(|data|)*)
histplotcorr2=histplot4[pdfcorr[[flavourin+6]]/.LF1[a__]\[RuleDelayed]Abs[{a}[[3]] ],title,xhistabstitle,yhisttitle,binset,Take[lineelement,-3],hist2plotrangex,Hist1figureNbin];
"dummy"
];
*)
If[
 plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,
(*hist1: data with no absolute*)
histplotcorr1=histplot4[pdfcorr/.LF1[a__]:>{a}[[3]],"",xhisttitle,yhisttitle,binset,lineelement,hist1plotrangex,Hist1figureNbin];
(*hist1: data with absolute(|data|)*)
histplotcorr2=histplot4[pdfcorr/.LF1[a__]:>Abs[{a}[[3]] ],"",xhistabstitle,yhisttitle,binset,Take[lineelement,-3],hist2plotrangex,Hist1figureNbin];
"dummy"
];

If[
 plottype==1,
(*plot1*)

expttype="multi";
myplotsetting=setplotsetting[corrfxQdtaobsclassin,exptlist,expttype,1,"test","test"];
imgsize=myplotsetting[["imgsize"]];
title=myplotsetting[["title"]];
xtitle=myplotsetting[["xtitle"]];
ytitle=myplotsetting[["ytitle"]];
lgdlabel=myplotsetting[["lgdlabel"]];
xrange=myplotsetting[["xrange"]];
yrange=myplotsetting[["yrange"]];
epilog=myplotsetting[["epilog"]];
titlesize=myplotsetting[["titlesize"]];
xtitlesize=myplotsetting[["xtitlesize"]];
ytitlesize=myplotsetting[["ytitlesize"]];
lgdlabelsize=myplotsetting[["lgdlabelsize"]];
ticklablesize=myplotsetting[["ticklablesize"]];

myplotstyle=myplotsetting[["plotstyle"]];
myMarker=myplotsetting[["marker"]];

title="Experimental data in "<>PDFname<>" analysis";
lgdpos={0.25,0.725};
xyrangeplot1=plotrange;(*20170307 change it's name, avoid duplicate*)
plot1=PDFloglogplot[pdfcorr,myMarker,myplotstyle,title,xtitle,ytitle,xyrangeplot1,lgdlabel,lgdpos,imgsize];
];


(*make expt name & ID table*)
(*==============================*)

(*output*)
(*==============================*)
title;
(*
GraphicsGrid[{{xQplotcorr},{histplotcorr1,histplotcorr2}}];
{{xQplotcorr},{histplotcorr1,histplotcorr2}};
*)

(*make  exptname table*)
rows=3;
exptnames=Table[ExptIDtoName[exptlist[[iexpt]] ]<>"("<>ToString[exptlist[[iexpt]] ]<>")",{iexpt,1,Length[exptlist]}];
Print["making table of experiments included in plots"];
exptnamestable=makeGrid2[exptnames,rows,title<>"\n\n"];

Which[
 plottype==2 || plottype==3 || plottype==4 || plottype==5  || plottype==6,
{{xQplotcorr,exptnamestable},{histplotcorr1,histplotcorr2}},
plottype==1,
{plot1}
]
]


(* ::Subsection:: *)
(*make all kinds plots by all kinds data (corr, dr*corr, dr), (plot1, plot2, plot3, ...) *)


(* ::Subsection:: *)
(*set manipulation interface of plots*)


(* ::Input:: *)
(*processplotsmodes2[plotin_,procin_,expidin_,modein_,flavourmodein_]:=*)
(*Module[{p=plotin,proc=procin,expid=expidin,mode=modein,flavourmode=flavourmodein,flavour,output},*)
(*output=0;*)
(*flavour=flavourmode;*)
(**)
(*If[mode==1,*)
(*output=(*Table[processplots[pdfsetfile,PDFDataDir,CorrDataDir,proc,expid,flavour][[1]],{flavour,-5,5}]*)(*Table[p[[flavour+6,1]],{flavour,-5,5}]*)p[[flavour+6,1]]*)
(*];*)
(*If[mode==2,*)
(*output=(*Table[processplots[pdfsetfile,PDFDataDir,CorrDataDir,proc,expid,flavour][[2]],{flavour,-5,5}]*)p[[flavour+6,2]]*)
(*];*)
(*If[mode==3,*)
(*output=(*Table[processplots[pdfsetfile,PDFDataDir,CorrDataDir,proc,expid,flavour][[3]],{flavour,-5,5}]*)(*Table[p[[flavour+6,3]],{flavour,-5,5}]*)p[[flavour+6,3]]*)
(*];*)
(*If[mode==4,*)
(*output=(*Append[{processplots[pdfsetfile,PDFDataDir,CorrDataDir,proc,expid,0][[2,2]]},Table[processplots[pdfsetfile,proc,expid,flavour][[3,3]],{flavour,-5,5}] ]*)*)
(*(*Append[{p[[1,2,2]]},Table[p[[flavour+6,3,3]],{flavour,-5,5}] ]*)*)
(*{p[[flavour+6,2,2]],p[[flavour+6,3,3]]}(*20170214 btw: p[[1,2,2]]>p[[flavour+6,2,2]]*)*)
(*];*)
(*If[mode==5,*)
(*output=(*Table[{p[[flavour+6,1,3]],p[[flavour+6,3,3]]},{flavour,-5,5}]*){p[[flavour+6,1,3]],p[[flavour+6,3,3]]}*)
(*(**)
(*Table[*)
(*p=processplots[pdfsetfile,PDFDataDir,CorrDataDir,proc,expid,flavour];*)
(*{p[[1,3]],p[[3,3]]},{flavour,-5,5}] *)
(**)*)
(*];*)
(**)
(*(*2017.01.24*)*)
(*If[mode==6,*)
(*output=(*Table[processplots[pdfsetfile,PDFDataDir,CorrDataDir,proc,expid,flavour][[1]],{flavour,-5,5}]*)(*Table[p[[flavour+6,1]],{flavour,-5,5}]*)p[[flavour+6,4]]*)
(*];*)
(**)
(*If[mode!=1 && mode!=2 && mode!=3&& mode!=4&& mode!=5&& mode!=6,*)
(*Print["error, mode should be 1, 2, 3, 4"];output=0*)
(*];*)
(*output*)
(*];*)


(* ::Input:: *)
(*(*input dataclass list of correlation, dr*correlation, deltaR with dimensions [[iexpt,flavour]]*)*)
(*(*output plots for manipulate*)*)
(*makeplotsformanipulate[corrfxQdtaobsclassin_,dRcorrfxQdtaobsclassin_,deltaRclassin_,pdfsetfilein_,datalistFilein_]:=*)
(*Module[{corrfxQdtaobsclass=corrfxQdtaobsclassin,dRcorrfxQdtaobsclass=dRcorrfxQdtaobsclassin,deltaRclass=deltaRclassin,pdfsetfile=pdfsetfilein,datalistFile=datalistFilein,fmax,corrdataforplot,drcorrdataforplot,drforplot,procs,expids,p},*)
(*(*********************************)*)
(*(*prepare for data input to processdataplotsmultiexp*)*)
(*(*********************************)*)
(*(*transf format from LF to LF1, since plot functions use LF1*)*)
(*fmax=Length[corrfxQdtaobsclass[[1]] ];*)
(**)
(*Table[*)
(*corrfxQdtaobsclass[[i,flavour+6]][["data"]]=corrfxQdtaobsclass[[i,flavour+6]][["data"]]/.LF->LF1;*)
(*dRcorrfxQdtaobsclass[[i,flavour+6]][["data"]]=dRcorrfxQdtaobsclass[[i,flavour+6]][["data"]]/.LF->LF1;*)
(*deltaRclass[[i,flavour+6]][["data"]]=deltaRclass[[i,flavour+6]][["data"]]/.LF->LF1;*)
(*(*deltaR[[i,flavour+6]][["data"]]=deltaR[[i,flavour+6]][["data"]]/.LF\[Rule]LF1;*)*)
(*"dummy",*)
(*{i,1,Length[corrfxQdtaobsclass]},{flavour,-5,-5+fmax-1}*)
(*];*)
(**)
(*(*set {corr[[flavour]],drcorr[[flavour]],dr[[flavour]]}*)*)
(*(*they are used to  input into processdataplotsmultiexp*)*)
(*(*data format \[Equal] {LF[x,Q,obs],...,...}*)*)
(*corrdataforplot=Table[corrfxQdtaobsclass[[iexpt,flavour+6]][["data"]],{iexpt,1,Length[corrfxQdtaobsclass]},{flavour,-5,-5+fmax-1}];*)
(*drcorrdataforplot=Table[dRcorrfxQdtaobsclass[[iexpt,flavour+6]][["data"]],{iexpt,1,Length[corrfxQdtaobsclass]},{flavour,-5,-5+fmax-1}];*)
(*drforplot=Table[deltaRclass[[iexpt,flavour+6]][["data"]],{iexpt,1,Length[corrfxQdtaobsclass]},{flavour,-5,-5+fmax-1}];*)
(**)
(*(*merge all experimental data into one, for all flavours*)*)
(*(*ex: corrdataforplot[[iexpt,flavour,Npt]] \[Rule] orrdataforplot[[flavour,Npt]]*)*)
(*{corrdataforplot,drcorrdataforplot,drforplot}=mergeplotdata[{corrdataforplot,drcorrdataforplot,drforplot}];*)
(**)
(*(* deletezerovalue: delete data with 0 value (0 value usually from mb, mc cut)*)*)
(*{corrdataforplot,drcorrdataforplot,drforplot}=deletezerovalue[{corrdataforplot,drcorrdataforplot,drforplot}];*)
(**)
(*(*********************************)*)
(*(*use processdataplotsmultiexp to make plots*)*)
(*(*********************************)*)
(**)
(*(*prepare arguments: expt index of processes={PDISNC,NDISCC,PDISNCCC,PVBPZ,PVBPW,PJP,hDISNC,hDISCC,hVBPZ},*)
(*it is for processdataplotsmultiexp*)*)
(*{procs,expids}=toprocexptidlist[exptlist];*)
(*(*make all plots for manipulate *)*)
(**)
(*p=*)
(*Table[*)
(*processdataplotsmultiexp[{corrdataforplot,drcorrdataforplot,drforplot},pdfsetfile,(*PDFDataDirin_,CorrDataDirin_,*)datalistFile,procs,expids,flavour,SMmode],*)
(*{SMmode,1,3},{flavour,-5,-5+fmax-1}];*)
(**)
(*p*)
(*]*)


(* ::Subsection:: *)
(*save plot into file (pdf, eps, jpg)*)


(* ::Chapter:: *)
(*Other functions*)


(* ::Section:: *)
(*convert figure files into eps, png, pdf, jpg format*)


(* ::Input::Initialization:: *)
ConvertPlotExtension[filenamelistin_,extIndexin_]:=
Module[{filenamelist=filenamelistin,extIndex=extIndexin,
convertstr,extensionname,iext,filenamelistnew},
convertstr="convert ";
extensionname={".eps",".png",".pdf",".jpg"};
iext=extIndex;


(*cut extension part and add the new extension we want to convert*)
filenamelistnew=Table[StringDrop[filenamelist[[ifile]],-1-(FileExtension[filenamelist[[ifile]] ]//StringLength) ]<>extensionname[[iext]],{ifile,1,Length[filenamelist]}];

(*convert files to new extension*)
Table[Run[convertstr<>filenamelist[[ifile]]<>" "<>filenamelistnew[[ifile]] ];"dummy",{ifile,1,Length[filenamelist]}];

"dummy"
]


(* ::Section:: *)
(*merge files into a pdf file*)


(* ::Subsection:: *)
(*get filenames which are output figures of figure making executables*)


(* ::Input::Initialization:: *)



GetxQplotListSamept[PlotDirin_,extIndexin_]:=
Module[{PlotDir=PlotDirin,extIndex=extIndexin,
filenamelist,filename,iext,obsname,representationname,extensionname,fmax},

(*set observables, different kinds of plot, available extension of output files *)
obsname={"xQbyexpt","expt_error_ratio","residual","dr","corrdr","corr"};
representationname={"xQ","legend","hist1","hist2"};
extensionname={".eps",".png",".pdf",".jpg"};

(*make all figuretype plots *)
filenamelist={};
iext=extIndex;
fmax=15;

Print["search figures under",PlotDir,", extension = ",extensionname[[extIndex]],", total #flavour for check = ",fmax];
Table[
(*Print["now flavour = ",flavour];*)
(*add exptname table into output figure*)

(*p6=GraphicsGrid[p6,Spacings\[Rule]Scaled[0.15] ];*)

filename=obsname[[6]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[6]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[6]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[6]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];


(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[5]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[5]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[5]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[5]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];

"dummy"
,{flavour,-5,-5+fmax-1}
];

(*2, 3, 4*)
(*dr*corr plots*)
(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[2]]<>"_"<>representationname[[1]]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[2]]<>"_"<>representationname[[2]]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[2]]<>"_"<>representationname[[3]]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[2]]<>"_"<>representationname[[4]]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];


(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[3]]<>"_"<>representationname[[1]]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[3]]<>"_"<>representationname[[2]]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[3]]<>"_"<>representationname[[3]]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[3]]<>"_"<>representationname[[4]]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];


(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[4]]<>"_"<>representationname[[1]]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[4]]<>"_"<>representationname[[2]]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[4]]<>"_"<>representationname[[3]]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
filename=obsname[[4]]<>"_"<>representationname[[4]]<>"_samept";
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];


filename=obsname[[1]]<>"_"<>representationname[[1]];
If[FileExistsQ[PlotDir<>filename<>extensionname[[iext]]]==True,AppendTo[filenamelist,PlotDir<>filename<>extensionname[[iext]] ] ];
(*output*)
filenamelist

]


(* ::Input::Initialization:: *)
GetxQplotListGrid[PlotDirin_,extIndexin_]:=
Module[{PlotDir=PlotDirin,extIndex=extIndexin,
filenamelist,filename,iext,obsname,representationname,extensionname,fmax},

(*set observables, different kinds of plot, available extension of output files *)
obsname={"xQbyexpt","expt_error_ratio","residual","dr","corrdr","corr"};
representationname={"xQ","legend","hist1","hist2"};
extensionname={".eps",".png",".pdf",".jpg"};

(*make all figuretype plots *)
filenamelist={};
iext=extIndex;
fmax=15;

Print["search figures under",PlotDir,", extension = ",extensionname[[extIndex]],", total #flavour for check = ",fmax];
Table[
(*Print["now flavour = ",flavour];*)
(*add exptname table into output figure*)

(*p6=GraphicsGrid[p6,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[6]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[6]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[6]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[6]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];

(*dr*corr plots*)
(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[5]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[5]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[5]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[5]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];

(*2, 3, 4*)
(*dr*corr plots*)

(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[2]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[2]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[2]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[2]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];


(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[3]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[3]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[3]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[3]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];

(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[4]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[4]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[4]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[4]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_grid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
"dummy"
,{flavour,-5,-5+fmax-1}
];

filename=obsname[[1]]<>"_"<>representationname[[1]]<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];

(*output*)
filenamelist
]



(* ::Input::Initialization:: *)
GetxQplotListSameptGrid[PlotDirin_,extIndexin_]:=
Module[{PlotDir=PlotDirin,extIndex=extIndexin,
filenamelist,filename,iext,obsname,representationname,extensionname,fmax},

(*set observables, different kinds of plot, available extension of output files *)
obsname={"xQbyexpt","expt_error_ratio","residual","dr","corrdr","corr"};
representationname={"xQ","legend","hist1","hist2"};
extensionname={".eps",".png",".pdf",".jpg"};

(*make all figuretype plots *)
filenamelist={};
iext=extIndex;
fmax=15;

Print["search figures under",PlotDir,", extension = ",extensionname[[extIndex]],", total #flavour for check = ",fmax];
Table[
(*Print["now flavour = ",flavour];*)
(*add exptname table into output figure*)

(*p6=GraphicsGrid[p6,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[6]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[6]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[6]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[6]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];

(*dr*corr plots*)
(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[5]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[5]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[5]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[5]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];

(*2, 3, 4*)
(*dr*corr plots*)

(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[2]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[2]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[2]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[2]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];


(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[3]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[3]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[3]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[3]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];

(*p5=GraphicsGrid[p5,Spacings\[Rule]Scaled[0.15] ];*)
filename=obsname[[4]]<>"_"<>representationname[[1]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[4]]<>"_"<>representationname[[2]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[4]]<>"_"<>representationname[[3]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
filename=obsname[[4]]<>"_"<>representationname[[4]]<>"_"<>"f"<>ToString[flavour]<>"_sameptgrid"<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];
"dummy"
,{flavour,-5,-5+fmax-1}
];

filename=obsname[[1]]<>"_"<>representationname[[1]]<>extensionname[[iext]];
If[FileExistsQ[PlotDir<>filename]==True,AppendTo[filenamelist,PlotDir<>filename] ];

(*output*)
filenamelist
]



(* ::Subsection:: *)
(*make a pdf file to merge filenames *)


(* ::Input::Initialization:: *)
MergePDFfiles[filenamelistin_]:=
Module[{filenamelist=filenamelistin,
pdfmergestr,pdfmergefiles,nofileflag},
(*pdfmergestr="convert -density 300 ";*)
pdfmergestr="bash epspdfcat ";
(*pdffinalfile="allfig.pdf";*)


(*if any files are not exist, print it*)
nofileflag=False;
Table[
If[
FileExistsQ[filenamelist[[ifile]] ]==False,
Print["the file doesn't exist: ",filenamelist[[ifile]] ];
nofileflag=True;
"dummy"
];
"dummy",
{ifile,1,Length[filenamelist]}
];
(*if any files are not exist, stop the function*)
If[nofileflag==True,Abort[]];

pdfmergefiles="";
Table[pdfmergefiles=pdfmergefiles<>filenamelist[[ifile]]<>" ";"dummy",{ifile,1,Length[filenamelist]}];

Run[pdfmergestr<>pdfmergefiles];
"dummy"
]


(* ::Subsection:: *)
(*make a pdf file to merge filenames which are output of figure making executables (3 kinds of output figures: samept, grid, sameptgrid)*)


(* ::Input::Initialization:: *)
(*this function transf png files into eps files, then convert eps files into one pdf file*)
(*input: Dir putting figures, DirType\[Equal]"samept", "grid", "sameptgrid"*)
implement[PlotDirin_,DirTypein_]:=
Module[{PlotDir=PlotDirin,DirType=DirTypein,filelistold},

(*original file extension \[Equal] .png*)
iext=2;
(*run*)
(*make the file list of original file (extension)*)
filelistold=
Which[
DirType=="samept",
GetxQplotListSamept[PlotDir,iext],
DirType=="grid",
GetxQplotListGrid[PlotDir,iext],
DirType=="sameptgrid",
GetxQplotListSameptGrid[PlotDir,iext]
];
(*convert to the new extension, ex: eps*)
ConvertPlotExtension[filelistold,1];
(*set new file list (.eps)*)
filelistnew=
Which[
DirType=="samept",
GetxQplotListSamept[PlotDir,1],
DirType=="grid" || DirType=="xgrid",
GetxQplotListGrid[PlotDir,1],
DirType=="sameptgrid",
GetxQplotListSameptGrid[PlotDir,1]
];
(*merge eps files into pdf file*)
MergePDFfiles[filelistnew];
(*move the merged pdf file to the directory*)
Run["mv allfigs.pdf "<>PlotDir];
"dummy"
]


(* ::Input::Initialization:: *)
(*this function convert eps files into one pdf file*)
(*input: Dir putting figures, DirType\[Equal]"samept", "grid", "sameptgrid",
output: a pdf file that merges all .eps files which are output of samept, grid, sameptgrid figure making executables (ex: run.nb is samept figure maker)  *)
implementeps[PlotDirin_,DirTypein_]:=
Module[{PlotDir=PlotDirin,DirType=DirTypein},

(*set new file list (.eps)*)
filelistnew=
Which[
DirType=="samept",
GetxQplotListSamept[PlotDir,1],
DirType=="grid" || DirType=="xgrid",
GetxQplotListGrid[PlotDir,1],
DirType=="sameptgrid",
GetxQplotListSameptGrid[PlotDir,1]
];
(*merge eps files into pdf file*)
MergePDFfiles[filelistnew];Print[filelistnew];
(*move the merged pdf file to the directory*)
Run["mv allfigs.pdf "<>PlotDir];
"dummy"
]


(* ::Section:: *)
(*for user define function *)


(* ::Subsection:: *)
(*Read f(x,Q) for specific Expt ID, flavour, point, replica index*)


(* ::Input::Initialization:: *)
{ibbar,icbar,isbar,idbar,iubar,ig,iu,id,is,ic,ib}=Range[-5,5,1]


(* ::Input::Initialization:: *)
(*this function extract the specific part of PDF data in fxQlist, where fxQlist is [[iexpt,iflavour,ipt]] List, each element is LLF[x,Q,fSubscript[(x,Q), 1],,,fSubscript[(x,Q), Nset]]*)
(*global variable: fxQlist, fxQDatabaselist, fxQDatabaseExptlist*)
(*for input data format [[iExpt,iFlavour,iPt]] = LF[x,Q,fSubscript[(x,Q), 1],,,fSubscript[(x,Q), Nset]], input ExptID,iFlavour,iPt, iSet, output the corresponding f(x,Q)*)
(*
ExptID,iFlavour,iPt, iSet could be index numbers or "All", if "All", the function output all data for that index  
e.g. ExptID="All": select all experiments (in the config1.txt), iFlavour="All": select data of all flavours, iPt="All": select all points, iSet="All": select all replicas
ExptID=159, the experiment in fxQlist with Expt ID = 159 (fxQlist[[iexpt,iflavour]][["exptinfo","exptid"]] = 159 )
iFlavour=-5~5, bbar ~ b
iPt=1, the first data point
iSet=1, the central set f(x,Q)

*)
(*
output: FxQTest[ExptID,"All",iPt,"All"] return a List of PDF values with dimensions = [[iflavour,iset]] for (iExpt,iPt),
where iExpt is the iexpt index in the input f(x,Q) data fxQDatabaselist that it's expt ID is the same as the input ExptID 
e.g. 
*)

FxQTest[ExptIDin_,iFlavourin_,iPtin_,iSetin_]:=
Module[{ExptID=ExptIDin,iFlavour=iFlavourin,iPt=iPtin,iSet=iSetin,NExpt,NFlavour,NPt,NSet,output,Nlayer,iExpt},

(*20171119: when input Expt ID ="All", this function only extract data sets selected by config1.txt*)
(*when input = specific expt ID, this function extract the data set with the same input expt ID from the loaded database *)

(*20171119: iexpt \[Rule] exptID, them the function find the corresponding iexpt*)
If[ExptID=="All",output=fxQlist;iExpt="All"];

If[
ExptID!="All",
output=(*fxQlist;*)fxQDatabaselist;
iExpt=Position[fxQDatabaseExptlist,ExptID]; 
If[
Length[iExpt]!=1,
Print["error, the should only have 1 data set of in your database of which the input expt ID is the same as the Expt ID"];
Print["your input expt ID: ",ExptID,", expt IDs loaded: ",fxQDatabaseExptlist];
Quit[] 
];
iExpt=iExpt[[1,1]];
"dummy"
];

(*check input*)
NExpt=Dimensions[output][[1]];
NFlavour=Dimensions[output][[2]];
(*include one expts: Npt \[Equal] *)
If[iExpt!="All",NPt=Length[output[[iExpt,1]] ],NPt=Min[Length[#[[1]] ]&/@output ] ];
NSet=Length[output[[1,1,1]] ]-2;
(*y default, #flavour = 11 (bbar to b)*)
NFlavour=11;
If[iExpt>NExpt,Print["error, the input iExpt > # of expts ",NExpt];Quit[] ];
If[(iFlavour+6)>NFlavour,Print["error, the input iExpt > # of flavours ",NFlavour];Quit[] ];
If[iPt>NPt,Print["error, the input iExpt > # of points ",NPt];Quit[] ];
If[iSet>NSet,Print["error, the input iExpt > # of replicas ",NSet];Quit[] ];
(*output*)
Nlayer=0;
If[iExpt!="All",output=output[[iExpt]],Nlayer=Nlayer+1 ];(*if iExpt \[Equal] "All", layer = [[iExpt,...]], if iExpt\[NotEqual]"All", layer = [[iflavour,...]]*)
If[iFlavour!="All",output=Map[#[[iFlavour+6]]&,output,{Nlayer}],Nlayer=Nlayer+1 ];
If[iPt!="All",output=Map[#[[iPt]]&,output,{Nlayer}],Nlayer=Nlayer+1 ];
If[iSet!="All",output=output/.LF[x_,Q_,a__]:>LF[x,Q,{a}[[iSet]] ],Nlayer=Nlayer+1 ];
(*
If[iPt\[NotEqual]"All",output=Map[#[[iPt]]&,output,{2}] ];

If[iFlavour\[NotEqual]"All",output=Map[#[[iFlavour+6]]&,output,{1}] ];
If[iExpt\[NotEqual]"All",output=output[[iExpt]] ];
*)
(*output*)
output/.LF[x_,Q_,a__]:>{a}
]

(*define top level f(x,Q) function for users*)

FxQ[iFlavourin_]:=FxQTest["All",iFlavourin,"All","All"];
FxQ[iExptin_,iFlavourin_,iPtin_,iSetin_]:=FxQTest[iExptin,iFlavourin,iPtin,iSetin];
FxQ[iExptin_,iFlavourin_,iPtin_]:=FxQTest[iExptin,iFlavourin,iPtin,"All"];



(* ::Subsection:: *)
(*transform the format of user define function to fxQ class List [[iexpt]]*)


(* ::Input::Initialization:: *)
(*this function input the user define function then define it as a new flavour index data*)
(*input 
UserDefFuncData, the data read from user define function
fxQdataclasslist: format = [[iexpt,iflavour]], [["data"]]={LF[x,Q,fSubscript[(x,Q), 1],...fSubscript[(x,Q), Nset]],LF[x,Q,fSubscript[(x,Q), 1],...fSubscript[(x,Q), Nset]],...}
*)
(*
output:
if user define function is {Nset values}, the new flavour data is {Nset values} for every point ipt
if user define function is [[iexpt,ipt]], each element = {Nset values}, the Nset data of points of the new flavour is 
the {Nset values} for the corresponding [[iexpt,ipt]] 
output format: [[iexpt]] of fxQ class
*)
(*
example of the case 1 is like the error sets of an physical observable
example of the case 1 is like the data of combination of flavours such as u/d for each point and experiment
*)

UDFToClass[UserDefFuncDatain_,fxQdataclasslistin_]:=
Module[{UserDefFuncData=UserDefFuncDatain,fxQdataclasslist=fxQdataclasslistin,DataFormatMode,tmpclass,Nset,Nexpt,x,Q,output},
(*format of data*)
(*1 for case 1, 2 for case 2*)
DataFormatMode="unset";

Nset=Length[fxQdataclasslist[[1,1]][["label"]] ]-2;
Nexpt=Length[fxQdataclasslist];
(*check the user define function is {Nset values}*)
If[Head[UserDefFuncData]==List && Dimensions[UserDefFuncData]=={Nset},DataFormatMode=1];

(*check the user define function is [[iexpt,ipt]], each element = {Nset}*)
If[Head[UserDefFuncData]==List && Dimensions[UserDefFuncData][[1]]==Nexpt && Length[UserDefFuncData[[1,1]] ]==Nset ,DataFormatMode=2];
(*
Print["mode: ",DataFormatMode];
Print[Head[UserDefFuncData]," ",Dimensions[UserDefFuncData][[1]]," ",Length[UserDefFuncData[[1,1]] ]];
Print["List, ","Next: ",Nexpt,", Nset: ",Nset];
*)

If[
DataFormatMode!=1 && DataFormatMode!=2,
Print["error, the input user define function data is not correct"];
Print["Head of the input: ",Head[UserDefFuncData] ];
Print["Dimensions of the input: ",Dimensions[UserDefFuncData] ];
Print["expected Dimensions: {Nset} = ",{Nset}," or {Nexpt,Npt,Nset} = ",Dimensions[fxQdataclasslist] ];
Abort[] 
];

(*for input is {Nset of values}*)
(*append data of user define function to fxQ class for each expt ID*)
If[
DataFormatMode==1,
output=
Table[
tmpclass=fxQdataclasslist[[iexpt,1]];
tmpclass[["data"]]=tmpclass[["data"]]/.LF[a__]:>LF[{a}[[1]],{a}[[2]],Sequence@@UserDefFuncData];
(*fxQdataclasslist[[iexpt]]=Append[fxQdataclasslist[[iexpt]],tmpclass];*)
tmpclass,
{iexpt,1,Nexpt}
];
"dummy"
];

(*for input is [[iexpt,ipt]]{Nset of values}*)
(*append the corresponding {Nset of values} to each fxQ class [[iexpt,ipt]]*)
If[
DataFormatMode==2,
output=
Table[
tmpclass=fxQdataclasslist[[iexpt,1]];
Table[
x=tmpclass[["data"]][[ipt]][[1]];Q=tmpclass[["data"]][[ipt]][[2]];
tmpclass[["data"]][[ipt]]=LF[x,Q,Sequence@@UserDefFuncData[[iexpt,ipt]] ],
{ipt,1,Length[fxQdataclasslist[[iexpt,1]][["data"]] ]}
];
(*fxQdataclasslist[[iexpt]]=Append[fxQdataclasslist[[iexpt]],tmpclass]*)
tmpclass,
{iexpt,Nexpt}
];
"dummy"
];

(*put data *)
output
]


(* ::Subsection:: *)
(*Example*)


(* ::Input:: *)
(*fxQfile="/home/botingw/Downloads/xqplotter-20171105_color_palette_fix/quick_data/fxQNset_samept_data_CT14NNLO.dat"*)
(*fxQdata=Get[fxQfile];*)
(*fxQlist=Table[fxQdata[[iexpt,iflavour]][["data"]],{iexpt,Dimensions[fxQdata][[1]]},{iflavour,Dimensions[fxQdata][[2]]}];*)


(* ::Input:: *)
(*iExpt=10;iFlavour=iu;iPt=3;iSet=5;*)
(*iFlavour*)


(* ::Input:: *)
(*FxQTest[iExpt,iFlavour,iPt,"All"]*)
(*fxQlist[[iExpt,iFlavour+6,iPt]]*)


(* ::Input:: *)
(*FxQTest[iExpt,iFlavour,"All",iSet][[iPt]]*)
(*fxQlist[[iExpt,iFlavour+6,iPt]][[iSet+2]]*)


(* ::Input:: *)
(*FxQTest["All","All","All","All"][[iExpt,iFlavour+6,iPt]]*)
(*fxQlist[[iExpt,iFlavour+6,iPt]]/.LF[a__]:>Drop[{a},2]*)


(* ::Input:: *)
(*pdfHessianCorrelationfake[{1},{35.1}]*)
(*pdfHessianCorrelationfake[FxQTest[iExpt,iu,iPt,"All"],FxQTest[iExpt,iu,iPt,"All"] ]*)
(*pdfHessianCorrelationfake[FxQTest[iExpt,iu,iPt,"All"],FxQTest[iExpt,id,iPt,"All"] ]*)



