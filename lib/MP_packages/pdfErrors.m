(* ::Package:: *)

(* ::Title:: *)
(*Mathematica package to calculate errors and correlations of funcitons of CTEQXX.pds files*)


(* ::Text:: *)
(*Original Version:*)
(*History : 15 Nov 2012-- v .0 .1, Ben Clark, Fred Olness*)
(*09 Jan 2013-- v .0 .2, Fred Olness, Olek Kusina Pass Iset, not whole list.Add miscList so we can deal with cases other than NF = 5.*)
(*15 Apr 2013-- v .0 .3, Pavel Nadolsky, implemented the new CT10 NNLO format, additional error control features, a flag for verbose printing in pdfParseCTEQ*)
(*20 Apr 2013-- v1 .0, Ben Clark, code reworked for consistancy and versions 0.1 and 0.2 merged, addtional error correction and alpha_s information included*)
(**)
(*New set of packages:*)
(*History: 30 October 2013- v1.1 Ben Clark, reworked to produce 3 packages. One to parse PDS files,  One to compute errors (This one) , and One to inport LHAgrid files for CTEQ*)


(* ::Section:: *)
(*Setup Package*)


(* ::Input:: *)
(**)


BeginPackage["pdfErrors`","pdfCalc`"];
(*Package requires pdfCalc.m for pdfFunction routine. see http://ncteq.hepforge.org/code/pdf.html*)

Print["==============================================================="];
Print[" "];
Print[" - pdfErrors - "];
Print["Version:  1.0"];
Print["Authors: D.B. Clark, E.J. Godat & F.I. Olness"];
Print[" "];
Print["Please cite: **************"];
Print["http://ncteq.hepforge.org/code/pdf.html"];
Print[" "];
Print["For a list of available commands, enter: ?pdf*"];
Print[" "];
Print["==============================================================="];



(* ::Input:: *)
(***********************************************************************)


pdfHessianCorrelation::usage=
"pdfHessianCorrelation[list1,list2]: This function returns the correlation between \!\(\*StyleBox[\" list1 \",\nFontSlant->\"Italic\"]\) and \!\(\*StyleBox[\" list2 \",\nFontSlant->\"Italic\"]\) of observables calculated with Hessian PDF error sets.

\!\(\*StyleBox[\" Note \",\nFontSlant->\"Italic\"]\): The lists \!\(\*StyleBox[\" list1 \",\nFontSlant->\"Italic\"]\) and \!\(\*StyleBox[\" list2 \",\nFontSlant->\"Italic\"]\) and should be the same length and correspond to the same PDF sets in memory.
";

(***********************************************************************)
pdfHessianError::usage=
"pdfHessianError[family,flavor,x,Q,[method]]: This function returns the PDF uncertainty for Hessian PDF error sets in \!\(\*StyleBox[\" family \",\nFontSlant->\"Italic\"]\), at given momentum fraction \!\(\*StyleBox[\" x \",\nFontSlant->\"Italic\"]\) and scale \!\(\*StyleBox[\" Q \",\nFontSlant->\"Italic\"]\).

The optional input \!\(\*StyleBox[\" method \",\nFontSlant->\"Italic\"]\) defaults to \"sym\" for the symmetric error. You may also set this input to \"plus\" or \"minus\" for the positive and negative asymmetric errors.

\!\(\*StyleBox[\" Warning \",\nFontSlant->\"Italic\"]\): The function assumes that the first member of family is the central value PDF set followed by an even number of PDF eigenvector sets. 

The eigenvector sets should alternate between the plus and minus errors for each of the parameters.

pdfHessianError[f[setNumber],[method]]: Will accept a list or function \!\(\*StyleBox[\" f \",\nFontSlant->\"Italic\"]\) of sets \!\(\*StyleBox[\" setNumber \",\nFontSlant->\"Italic\"]\) obtained over a Hessian PDF family.
";

(***********************************************************************)
pdfMCCentral::usage=
"pdfMCCentral[f[setNumber],[verbose]]: Will accept a list or function \!\(\*StyleBox[\" f \",\nFontSlant->\"Italic\"]\) of sets \!\(\*StyleBox[\" setNumber \",\nFontSlant->\"Italic\"]\) obtained over a Monte Carlo PDF replica family.

\!\(\*StyleBox[\" Warning \",\nFontSlant->\"Italic\"]\): The function throws an error message if the central value calculated does not match the central value PDF. This error does not interrupt calculation and can be turned off by setting  \!\(\*StyleBox[\" verbose \",\nFontSlant->\"Italic\"]\) to \!\(\*StyleBox[\" False \",\nFontSlant->\"Italic\"]\).
"; (*pdfMCCentral[family,flavor,x,Q,[verbose]]: This function returns the central value for a Monte Carlo PDF replica set in the list \!\(\*StyleBox[\" family \",\nFontSlant->\"Italic\"]\).*)

(***********************************************************************)
pdfMCCorrelation::usage=
"pdfMCCorrelation[list1,list2]: This function returns the correlation between \!\(\*StyleBox[\" list1 \",\nFontSlant->\"Italic\"]\) and \!\(\*StyleBox[\" list2 \",\nFontSlant->\"Italic\"]\) of observables calculated with Monte Carlo PDF error sets.

\!\(\*StyleBox[\" Note \",\nFontSlant->\"Italic\"]\): The lists \!\(\*StyleBox[\" list1 \",\nFontSlant->\"Italic\"]\) and \!\(\*StyleBox[\" list2 \",\nFontSlant->\"Italic\"]\) and should be the same length and correspond to the same PDF sets in memory.
";

(***********************************************************************)
pdfMCError::usage=
"pdfMCError[family,flavor,x,Q]: This function returns the symmetric PDF uncertainty for Monte Carlo PDF error sets in \!\(\*StyleBox[\" family \",\nFontSlant->\"Italic\"]\).

pdfMCError[f[setNumber],[method]]: Will accept a list or function \!\(\*StyleBox[\" f \",\nFontSlant->\"Italic\"]\) of sets \!\(\*StyleBox[\" setNumber \",\nFontSlant->\"Italic\"]\) obtained over a Monte Carlo PDF replica family.

The optional input \!\(\*StyleBox[\" method \",\nFontSlant->\"Italic\"]\) defaults to \"sym\" for the symmetric error. You may also set this input to \"plus\" or \"minus\" for the positive and negative asymmetric errors.

"; 

(*(***********************************************************************)
pdfMCStdDeviation::usage="pdfMCStdDeviation[x,q,ipart]: returns the standard deviation for Monte Carlo PDF error sets.  
pdfMCStdDeviation[f]: returns the standard deviation for the function f[iset] or list f[[iset]] obtained with Monte Carlo PDF replica sets.";
*)

(***********************************************************************)
pdfMCCentralInterval::usage="pdfMCCentralInterval[family,ipart,x,q,[p]]: This function returns {fc, flow, fup}, where fc is the central value of the PDF f(x,q,ipart) on a sample of Monte Carlo PDF replica sets, corresponding to the cumulative probability of 1/2; flow and fup are the lower and upper limits of the central 68.2% (the default value of \!\(\*StyleBox[\" p \",\nFontSlant->\"Italic\"]\); the user can change this with the optional input) probability interval, corresponding to the cumulative probabilities of (1-0.682)/2 and (1+0.682)/2. No assumptions the statistical distribution of the MC replicas are made.

pdfMCCentralInterval[family,x,q,ipart,{p1, p2,...}]: returns {fc, flow1, fup1, flow2, fup2, ...}, where fc is the central value, as above; flow1, fup1, flow2, fup2, ... are the upper and lower limits of the central intervals containing probabilities p1, p2, ... ( 0 <= p <= 1). 

pdfMCCentralInterval[f] and pdfMCCentralInterval[f, {p1, p2,...}]: Are described as above, where either a function f[iset] or a list f[[iset]] provides the values of the PDFs for each Monte Carlo replica."; 

(***********************************************************************)
pdfFamilyFunction::usage=
"pdfFamilyFunction[family,inputFunction]: This function evaluates the \!\(\*StyleBox[\" inputFunction \",\nFontSlant->\"Italic\"]\)  for each member of the PDF \!\(\*StyleBox[\" family \",\nFontSlant->\"Italic\"]\) and returns the output as a list.
";

(***********************************************************************)

pdfError::usage=
"pdfError[family_,flavor_,x_,Q_,[method]]:  This function returns the PDF uncertainty for either Monte Carlo or Hessian PDF error sets in \!\(\*StyleBox[\" family \",\nFontSlant->\"Italic\"]\), at given momentum fraction \!\(\*StyleBox[\" x \",\nFontSlant->\"Italic\"]\) and scale \!\(\*StyleBox[\" Q \",\nFontSlant->\"Italic\"]\).

The optional input \!\(\*StyleBox[\" method \",\nFontSlant->\"Italic\"]\) defaults to \"sym\" for the symmetric error. You may also set this input to \"plus\" or \"minus\" for the positive and negative asymmetric errors.

\!\(\*StyleBox[\" Warning \",\nFontSlant->\"Italic\"]\): This function will return an error if the Info file associated with the PDF set does not include the ErrorType of the set. If this occurs, choose manually between pdfMCError and pdfHessianError. These functions also provide more versatility when working with observables.
";

(***********************************************************************)

Begin["`Private`"]; 



(* ::Section:: *)
(*Support fuctions*)


(* ::Input:: *)
(*pdfFamilyFunction: accepts a family and a function of sets in the family and returns a list of func evaluated for each member of family*)


pdfFamilyFunction[family_?ListQ,inputFunc_]:=
Module[{length,output},
length=Length[family];
If[
Head[inputFunc]==Function,
output=Table[inputFunc[family[[i]]],{i,1,length}],
Print["Error: input should be a funciton over set number."];
Return[Null],
Print["Error: input should be a function over set number."];
Return[Null];
];Return[output];
];(*End generateList...*)



(* ::Section:: *)
(*Internal error functions*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*hessian functions--based on master functions arxiv/1401.0013 and order of eigevector sets in *)
(*cteq6 .1 paper PhysRevD .65.014013*)
(*outputSym: input a list over members of a family and output is symmetric error for the list*)


(*NOTE: THIS WILL NOT WORK FOR ABM AS THEY USE A MODIFIED VERSION. SEE CITATION*)
outputSym[famList_]:=
Module[{len,out,i},
len=Length[famList];
out=(1/2)*Sqrt[
Sum[
(famList[[2*i]]-famList[[2*i+1]])^2,
{i,1,(len-1)/2}](*End Sum...*)
];(*End Sqrt...*)
Return[out];
];(*End outputSym...*)

(*outputPlus: input a list over members of a family and output is positive asymmetric error for the list*)
outputPlus[famList_]:=
Module[{len,out,central,i},
len=Length[famList];
central=famList[[1]];
out=Sqrt[
Sum[
Max[famList[[2*i]]-central,famList[[2*i+1]]-central,0]^2,
{i,1,(len-1)/2}
](*End Sum...*)
      ];(*End Sqrt...*)
Return[out];
];(*End outputPlus...*)

(*outputMinus: input a list over members of a family and output is negative asymmetric error for the list*)
outputMinus[famList_]:=
Module[{len,central,out,i},
len=Length[famList];
central=First[famList];
out=Sqrt[
Sum[
Min[famList[[2*i]]-central,famList[[2*i+1]]-central,0]^2,
{i,1,(len-1)/2}
](*End Sum...*)
      ];(*End Sqrt...*)
      Return[out];
];(*End outputMinus...*)


(*MC functions--based on master functions arxiv/1401.0013*)

(*outputCen: input a list over members of a family and output is {central value, mean} for the list*)
outputCen[famList_]:=
Module[{len,cent0,mean,i},
len=Length[famList];
cent0=famList[[1]];
(*mean=(1/(len-1))*Sum[famList[[i]],{i,2,len}];*)
mean=Mean[Drop[famList,1]];
Return[{cent0,mean}];
];(*End outputCen...*)

(*outputMCerr: input a list over members of a family and output is Monte Carlo 68% c.l. error for the list*)
(*This corresponds to the unbiased standard deviation.*)
outputMCsym[famList_]:=
Module[{nreps,mean,i,output},
nreps=Length[famList]-1;(*number of replica sets*)
mean=outputCen[famList][[2]];
If[nreps>1,(*Prevents divide by 0*)
output=Sqrt[(1/(nreps-0))*Sum[(famList[[i]]- mean)^2,{i,2,(nreps+1)}]];
,output=Null;
];
Return[output];
];(*End outputMCerr...*)

(*outputMCplus: input is list over sets in a family and output is the MC plus errror for the family*)
outputMCplus[famList_]:=
Module[{mean,nreps,out},
mean=outputCen[famList][[2]];
nreps=Sort[Tally[Table[HeavisideTheta[famList[[i]]-mean],{i,2,Length[famList]}]]][[2,2]];
(*number of positive reps*)(*Sort required to control order of Tally output*)
If[nreps>1,(*Prevents divide by 0*)
out=Sqrt[(1/(nreps-0))*Sum[(famList[[i]]- mean)^2*HeavisideTheta[famList[[i]]-mean],{i,2,(nreps+1)}]];
,out=Null;
];
Return[out];
];(*End outputMCplus...*)

(*outputMCminus: input is list over sets in a family and output is the MC minus errror for the family*)
outputMCminus[famList_]:=
Module[{mean,nreps,out},
mean=outputCen[famList][[2]];
nreps=Sort[Tally[Table[HeavisideTheta[mean-famList[[i]]],{i,2,Length[famList]}]]][[2,2]];
(*number of negative reps*)(*Sort required to control order of Tally output*)
If[nreps>1,(*Prevents divide by 0*)
out=Sqrt[(1/(nreps-0))*Sum[(famList[[i]]-mean)^2*HeavisideTheta[mean-famList[[i]]],{i,2,(nreps+1)}]];
,out=Null;
];
Return[out];
];(*End outputMCplus...*)

(*outputCentInt: input over members of a family and list of intervals, output is meadian value and central interval*)
outputCentInt[famList_,p_]:=
Module[{len,values,y,median,intervals,ip,out,xlow,xup(*,ip*)},
len=Length[famList];
values=Sort[famList];
y=len/2;
median={
values[[Floor[y]]] + (values[[Ceiling[y]]] - values[[Floor[y]]])*FractionalPart[y]
};
intervals=Table[
       {xlow, xup} = {len*(1.0 - p[[ip]])/2,len*(1.0 + p[[ip]])/2};
values[[Floor[#]]] + (values[[Ceiling[#]]] - values[[Floor[#]]])*FractionalPart[#] & /@ {xlow, xup},
       {ip, 1, Length[p]}
];
out=Join[median,Flatten[intervals]];
Return[out];
];(*End outputCentInt...*)




(* ::Section:: *)
(*User error functions*)


(* ::Subsection:: *)
(*Hessian PDF families*)


(* ::Input:: *)
(*pdfHessianError: input is a list of observables over members of a complete family.*)
(*optional input can be "sym", "plus" or "minus" for type of error.*)
(*output is the Hessian error of type method for the function or list ff*)


pdfHessianError[ff_?ListQ,method_:"sym"]:=
Module[{length,meth,output},
length=Length[ff];
meth=False;
output=Null;(*output intitialized to Null*)
If[
EvenQ[length],
Print["Error: pdfHessianError requires an odd number of entries, not ",length];
Return[Null];
];(*End If...*)
If[
method=="sym",
meth=True;
output=outputSym[ff];
];(*End If...*)
If[
method=="plus",
meth=True;
output=outputPlus[ff];
];(*End If...*)
If[
method=="minus",
meth=True;
output=outputMinus[ff]
];(*End If...*)
If[meth==False,
Print["Error: Hessian error method is unrecognized."];
];
Return[output];
];(*End pdfHessianError...*)
    
pdfHessianError[family_?ListQ,ipart_,x_,q_,method_:"sym"]:=
Module[{func,famList,output},
func=pdfFunction[#,ipart,x,q]&;
famList=pdfFamilyFunction[family,func];
output=pdfHessianError[famList,method];
Return[output];
];(*End pdfHessianError...*)

(*pdfHessianCorrelation: input is 2 lists of observables caculated for members of a complete family*)
(*output is the Hessian correlation between the values in the list*)
pdfHessianCorrelation[list1_, list2_]:=
Module[{length,PDFerror1,PDFerror2,output,i},
If[
! ListQ[list1] || ! ListQ[list2],
Print["Error: both arguments must be lists"];
Return[Null];
];(*End If...*)
If[
Length[list1] != Length[list2],
Print["Error: length of the lists do not match, ", Length[list1] , "!=", Length[list2]];
Return[Null];
];(*End If...*)
length = Length[list1];
If[
EvenQ[length],
Print["Error: pdfHessianCorrelation requires an odd number of entries, not ",length];
Return[Null];
];(*End If...*)
{PDFerror1, PDFerror2} = Max[pdfHessianError[#,"sym"], 10^-8] & /@ {list1, list2};
output=1/(PDFerror1 PDFerror2)*(1/4)*Sum[
(list1[[2*i+1]]-list1[[2*i]])*(list2[[2*i+1]]-list2[[2*i]]), 
{i,1,(length-1)/2}
];(*End Sum...*)
Return[output];
];(*End pdfHessianCorrelation...*)



(* ::Subsection:: *)
(*Monte Carlo PDF families*)


(* ::Input:: *)
(*pdfMCCentral: input is a family, a function of members of that family or a list over members of that family*)
(*                output is the central value for the function or list ff*)
(*pdfMCCentral: throws an error message if the central value calculated does not match the central value pdf -->does not interrupt calculation. This can be turned off with the optional verbose flag.*)


pdfMCCentral[ff_?ListQ,verbose_:True]:=
Module[{length,pdf0,mean},
length=Length[ff];
mean=Null;
(*If[
EvenQ[length],
Print["Error: pdfMCCentral requires an odd number of entries, not ",length];
Return[Null];
];(*End If...*)*)
If[verbose==True||verbose==False,
{pdf0,mean}=outputCen[ff];
];
If[verbose==True&&Chop[(pdf0-mean),10^-5] != 0.,
Print["Warning: Central value of the replicas ",mean," does not match the value for the central set ",pdf0,"."];
];
If[mean==Null,
Print["Error: verbose option must be True or False."];
];
Return[mean];
];(*End pdfMCCentral...*)

(*pdfMCCentral[family_?ListQ,ipart_,x_,q_,verbose_:True]:=
Module[{func,famList,output},
func=pdfFunction[#,ipart,x,q]&;
famList=pdfFamilyFunction[family,func];
output=pdfMCCentral[famList,verbose];
Return[output];
];(*End pdfMCCentral...*)
*)
(*pdfMCError: input is a family and function of members of that family or a list over members of that family*)
(*output is the symmetric Monte Carlo error for the function or list ff*)
pdfMCError[ff_?ListQ,method_:"sym"]:=
Module[{length,output,meth},
length=Length[ff];
output=Null;
meth=False;
If[
length<=2,(*Since Heaviside[0] is undefined and the Mean is calculated from the replicas, if there is only one replica, this will prevent an error*)
Print["Error: pdfMCError requires a minimum of 3 sets (central value, 2 error sets), not ",length];
Return[Null];
];
If[
method=="sym",
meth=True;
output=outputMCsym[ff];
];(*End If...*)
If[
method=="plus",
meth=True;
output=outputMCplus[ff];
];(*End If...*)
If[
method=="minus",
meth=True;
output=outputMCminus[ff]
];(*End If...*)
If[meth==False,
Print["Error: Monte Carlo error method is unrecognized."];
];
Return[output];
];(*End pdfMCError...*)

pdfMCError[family_?ListQ,ipart_,x_,q_,method_:"sym"]:=
Module[{func,famList,output},
func=pdfFunction[#,ipart,x,q]&;
famList=pdfFamilyFunction[family,func];
output=pdfMCError[famList,method];
Return[output];
];(*End pdfMCError...*)

(*pdfMCCorrelation: input is a family and 2 lists of valus caculated for members of that family*)
(*                output is the MonteCarlo correlation between the values in the list*)
pdfMCCorrelation[list1_?ListQ, list2_?ListQ]:=
Module[{nreps,pdfError1,pdfError2,central1,central2,list3,central3,output},
If[
Length[list1] != Length[list2],
Print["Error: length of the lists do not match, ",
Length[list1] , "!=", Length[list2]];
Return[Null];
];(*End If...*)
nreps = Length[list1] - 1;
(*If[
OddQ[nreps],
Print["Error: pdfMCCorrelation requires an odd number of entries, not ",nreps+1];
Return[Null]
];(*End If..*)*)
pdfError1 = Max[pdfMCError[list1], 10^-8];(*btw 20170424 add lowlimit of the value*)
pdfError2 = Max[pdfMCError[list2], 10^-8];(*btw 20170424 add lowlimit of the value*)
central1 = pdfMCCentral[list1,False];
central2 = pdfMCCentral[list2,False];
list3 = list1*list2;
central3 = pdfMCCentral[list3,False];
output = nreps*(central3 - central1*central2)/((nreps - 1)*pdfError1*pdfError2);
Return[output];
];(*End pdfMCCorrelation...*)

(*pdfMCCentralInterval: input is a family, fuction or list over family, and tolerence level.*)
(*output is a list {median, upper bound, lower bound}*)
pdfMCCentralInterval[ff_?ListQ, p_: {0.682}]:= 
  Module[{length,output}, 
length=Length[ff];
(*If[
EvenQ[length],
Print["Error: pdfMCCentralInterval requires an odd number of entries, not ",length];
Return[Null]
];(*End If...*)*)
If[
length < 2,
Print["Error: pdfMCCentralInterval requires more than 1 MC replica set."];
Return[Null]
];(*End If...*)
output=outputCentInt[ff,p];
Return[output];
];

pdfMCCentralInterval[family_?ListQ,ipart_,x_,q_, p_: {0.682}]:=
Module[{func,famList,output},
func=pdfFunction[#,ipart,x,q]&;
famList=pdfFamilyFunction[family,func];
output=pdfMCCentralInterval[famList,p];
Return[output];
];




(* ::Subsection:: *)
(*Unified PDF Error Function*)


(* ::Input:: *)
(**)


pdfError[family_,flavor_,x_,Q_,method_:"sym"]:=Module[{errortype,f,verbose},
verbose=False;
errortype=pdfGetInfo[family[[1]],"ErrorType"];

If[StringCases[errortype,"hessian"]=={"hessian"},
If[verbose==True,Print["Used pdfHessianError function"]];
Return[pdfHessianError[family,flavor,x,Q,method]];
];
If[StringCases[errortype,"replicas"]=={"replicas"},
If[verbose==True,Print["Used pdfMCError function"]];
Return[pdfMCError[family,flavor,x,Q,method]];
];
(*If[verbose\[Equal]True,*)Print["Unknown error type, please manually choose from pdfHessianError or pdfMCError"];
(*];*)
Return[Null];
]






(* ::Section:: *)
(*End Package*)


(* ::Input:: *)
(**)


End[];  (* End Private Context *)

Protect[pdfHessianCorrelation,pdfHessianError,pdfMCCentral,pdfMCCorrelation,pdfMCError, pdfFamilyFunction, pdfMCCentralInterval
];

EndPackage[]; (* End Package Context *)
