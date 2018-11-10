#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*NetModelDownload*)


names=NetModel[];


Print["Length@Models",names//Length]


NotebookDirectory[]


names2Download=names;


idx=0;
Do[flag=0;c=0;
Print@i;
Check[NetModel[i],flag=True];
While[flag&&c<10,
Print[c,"'s Trying to download"];c++;
ResourceRemove[ResourceObject@i];Check[NetModel[i],flag=True]
];
Print[idx+=1]
,
{i,names2Download}]
