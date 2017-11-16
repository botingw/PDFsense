(* ::Package:: *)

(* ::Section:: *)
(*F(x,Q functions)*)


(* ::Input::Initialization:: *)
{ibbar,icbar,isbar,idbar,iubar,ig,iu,id,is,ic,ib}=Range[-5,5,1]


(* ::Input::Initialization:: *)
(*for input format [[iExpt,iFlavour,iPt]] = LF[x,Q,fSubscript[(x,Q), 1],,,fSubscript[(x,Q), Nset]], input iExpt,iFlavour,iPt, iSet, output the corresponding f(x,Q)*)
(*global variable: fxQlist*)
FxQTest[iExptin_,iFlavourin_,iPtin_,iSetin_]:=
Module[{iExpt=iExptin,iFlavour=iFlavourin,iPt=iPtin,iSet=iSetin,NExpt,NFlavour,NPt,NSet,output,Nlayer},
output=fxQlist;
(*check input*)
NExpt=Dimensions[output][[1]];
NFlavour=Dimensions[output][[2]];
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
If[iPt!="All",output=Map[#[[iPt]]&,output,{Nlayer}],,Nlayer=Nlayer+1 ];
If[iSet!="All",output=output/.LF[x_,Q_,a__]:>LF[x,Q,{a}[[iSet]] ],Nlayer=Nlayer+1 ];
(*
If[iPt\[NotEqual]"All",output=Map[#[[iPt]]&,output,{2}] ];

If[iFlavour\[NotEqual]"All",output=Map[#[[iFlavour+6]]&,output,{1}] ];
If[iExpt\[NotEqual]"All",output=output[[iExpt]] ];
*)
(*output*)
output/.LF[x_,Q_,a__]:>{a}
]


(* ::Input:: *)
(*(*20170315: for web version, we don't need PDF library and initilize PDFsets, but we still need correlation function. so make a fake function work here*)
(*all functions use these two functions replaced by fake version*)*)
(**)
(*(**)
(*pdfHessianSymErrorfake[f_]:=Module[{Neigen},*)
(*If[ListQ[f],Neigen=Length[f],Print["pdfHessianSymErrorfake input should be list "];Abort[] ];*)
(*If[OddQ[Neigen],*)
(*1/2 Sqrt[Sum[*)
(*If[ListQ[f],(f[[2*iset+1]]-f[[2*iset]])^2,(f[2*iset+1]-f[2*iset])^2],*)
(*{iset,1,(Neigen-1)/2}]],*)
(*Print["Error: pdfHessianSymErrorfake requires an odd number of entries, not ",Neigen]]*)
(*]; *)
(**)
(*pdfHessianCorrelationfake[list1_, list2_] :=Module[{Neigen,Neigen1, Neigen2, PDFerror1, PDFerror2},*)
(*   If[ ! ListQ[list1] || ! ListQ[list2], *)
(*    Print["pdfHessianCorrelationfake: both arguments must be lists"]; Return];*)
(*   If[Length[list1] != Length[list2],*)
(*    Print["Problem: length of the lists do not match, ", *)
(*     Length[list1] , "!=", Length[list2]; Return]*)
(*    ];*)
(*   Neigen = Length[list1];*)
(*   If[EvenQ[Neigen], Print["Stop, an even number of eigenvectors: ", Neigen]; Return];*)
(*   {PDFerror1, PDFerror2} = Max[pdfHessianSymErrorfake[#], 10^-8] & /@ {list1, list2};*)
(*   1/(PDFerror1 PDFerror2) 1/4 Sum[(list1[[2*iset + 1]] - list1[[2*iset]])*(list2[[2*iset + 1]] - list2[[2*iset]]), {iset, 1, (Neigen - 1)/2}]*)
(*   ];*)
(**)*)
(**)
(**)


(* ::Section:: *)
(*Get the class for the input user define function*)


(* ::Input::Initialization:: *)
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


(* ::Input:: *)
(*ReadUserFunction["./Downloads/","tst2.m"]*)
(**)


(* ::Input::Initialization:: *)
(*
ReadUDF[UserDefFuncDirin_,UserDefFuncFilein_]:=
Module[{UserDefFuncDir=UserDefFuncDirin,UserDefFuncFile=UserDefFuncFilein},
(*read user define function from file*)

(*for that function, input pdf data and get the output of that function*)
]
*)

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
Print["mode: ",DataFormatMode];
Print[Head[UserDefFuncData]," ",Dimensions[UserDefFuncData][[1]]," ",Length[UserDefFuncData[[1,1]] ]];
Print["List, ","Next: ",Nexpt,", Nset: ",Nset];

If[DataFormatMode!=1 && DataFormatMode!=2,Print["error, the input user define function data is not correct"];Abort[] ];

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


(* ::Section:: *)
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


(* ::Section:: *)
(*Data to class test*)


(* ::Subsection:: *)
(*read data from user define function file*)


(* ::Input:: *)
(*fxQfile="/home/botingw/Downloads/xqplotter-20171105_color_palette_fix/quick_data/fxQNset_samept_data_CT14NNLO.dat"*)
(*fxQdata=Get[fxQfile];*)
(*fxQlist=Table[fxQdata[[iexpt,iflavour]][["data"]],{iexpt,Dimensions[fxQdata][[1]]},{iflavour,Dimensions[fxQdata][[2]]}];*)


(* ::Input:: *)
(*Dimensions[fxQlist]*)
(*Dimensions[fxQlist[[1,1]] ]*)
(*Dimensions[fxQlist[[1,1,1]] ]*)
(*fxQlist[[1,1]]*)


(* ::Input:: *)
(*(*iExpt=28;*)*)
(*{UserArgName,UserArgFunction}=ReadUserFunction["./Downloads/","tst2.m"]*)


(* ::Input:: *)
(*Length[UserArgFunction]*)
(*Length[UserArgFunction[[1]] ]*)


(* ::Subsection:: *)
(*transfer user define function to class format*)


(* ::Input:: *)
(*(*transf user define function data to fxQ class format*)*)
(*UDFToClass[UserArgFunction,fxQdata][[3]][["data"]][[5]]*)
(*UserArgFunction[[3,5]]*)


(* ::Input:: *)
(*(*new flavour*)*)
(*fxQdataNewFlavour=UDFToClass[UserArgFunction,fxQdata]*)


(* ::Input:: *)
(*fxQdata//Dimensions*)


(* ::Input:: *)
(*(*add new flavour for each expt ID*)*)
(*Nexpt=Length[fxQdata]*)
(*fxQdata=Append[fxQdata[[#]],fxQdataNewFlavour[[#]] ]&/@Range[Nexpt]*)


(* ::Input:: *)
(*fxQdata//Dimensions*)
