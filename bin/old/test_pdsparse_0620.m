(* ::Package:: *)

(* ::Section:: *)
(*step 1*)


(* ::Input::Initialization:: *)
SetDirectory[(*NotebookDirectory[]*)DirectoryName[$InputFileName] ];
Get["corr_proj_funcs.m"];
(*
libdir="../lib/";
(*
<<"pdfParsePDS2013.m"
<<"dtareadbotingw2016.m"
*)
Get[libdir<>"pdfParsePDS2013.m"]
Get[libdir<>"dtareadbotingw2016.m"]
*)


(* ::Section:: *)
(*step 2*)


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
myPDFsetDir="/home/botingw/code/pdf_correlation/dta_file/2017.0604.1856.-0500_CT14-1_mod/";
pdfFamilyParseCTEQ[myPDFsetDir<>"*pds",ifamily];


Print[myPDFsetDir];


(* ::Input::Initialization:: *)
x=0.01;Q=100.0;
flavour=0;
Nset=Length[pdfSetList[[ifamily]]];
pdfSetActiveFamily[ifamily]; 
Print[
LF[x,Q,Sequence@@Table[pdfCTEQ[x,Q,flavour,iset],{iset,Nset}] ]
];
