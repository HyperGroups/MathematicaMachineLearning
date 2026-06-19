#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Text:: *)
(*\[WarningSign] \:5df2\:5f03\:7528 DEPRECATED\:ff08\:9488\:5bf9 V11.3\:ff09\:3002\:8bf7\:6539\:7528 ../Tools/NetModelDownloader.wls\:ff08\:9002\:7528 13/14/15\:ff09\:3002\:6b64\:6587\:4ef6\:4ec5\:4f5c\:5386\:53f2\:53c2\:8003\:3002*)

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
