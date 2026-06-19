#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*NetModelDownloader*)


(* ::Text:: *)
(*\:6279\:91cf\:9884\:4e0b\:8f7d Wolfram \:795e\:7ecf\:7f51\:7edc\:4ed3\:5e93\:4e2d\:7684\:6a21\:578b\:ff0c\:7f13\:5b58\:5230\:672c\:5730\:5bf9\:8c61\:5e93 $LocalBase\:3002*)
(*\:9002\:7528\:ff1aWolfram 13 / 14 / 15\:3002\:65e7\:7248 NetModelDownloader@V11.3.wls \:5df2\:5f03\:7528\:ff0c\:4ec5\:4f5c\:5386\:53f2\:53c2\:8003\:3002*)

(* ----------------------------------------------------------------------------
   \:7528\:6cd5 Usage:
     wolframscript -file NetModelDownloader.wls
     wolframscript -file NetModelDownloader.wls "Inception V1 Trained on ImageNet"

   \:8bf4\:660e:
   - NetModel[name] \:4f1a\:6309\:9700\:8054\:7f51\:4e0b\:8f7d\:5e76\:7f13\:5b58\:5230 $LocalBase\:3002
   - \:7528 LocalObjects[] \:67e5\:770b\:5df2\:7f13\:5b58\:8d44\:6e90\:ff0cResourceRemove \:6e05\:9664\:3002
   - \:9700\:8981 Wolfram \:8d26\:6237\:767b\:5f55\:ff08\:90e8\:5206\:6a21\:578b\: ff09\:3002
   \:6ce8: \:8054\:7f51\:884c\:4e3a\:672a\:5728\:9759\:6001\:6761\:4ef6\:4e0b\:5b9e\:673a\:9a8c\:8bc1\:ff0c\:8bf7\:5728 15.0 \:5b9e\:8dd1\:786e\:8ba4\:3002
   ---------------------------------------------------------------------------- *)


(* ::Section:: *)
(*\:914d\:7f6e Configuration*)


(* \:91cd\:8bd5\:6b21\:6570\:4e0e\:5355\:6a21\:578b\:8d85\:65f6\:ff08\:79d2\:ff09 *)
$maxRetries = 5;
$timeLimit = 600;


(* ::Section:: *)
(*\:4e3b\:903b\:8f91 Main*)


(* \:5f85\:4e0b\:8f7d\:6a21\:578b\:5217\:8868\:ff1a\:547d\:4ee4\:884c\:53c2\:6570\:6307\:5b9a\:5219\:53ea\:4e0b\:8f7d\:6307\:5b9a\:8005\:ff0c\:5426\:5219\:5168\:90e8\:3002 *)
namesAll = NetModel[];
Print["Length@Models = ", Length @ namesAll];

names2Download =
  If[Length @ $ScriptCommandLine > 1,
    Rest @ $ScriptCommandLine,
    namesAll
  ];


(* \:5355\:4e2a\:6a21\:578b\:4e0b\:8f7d\:ff0c\:5e26\:91cd\:8bd5\:4e0e\:8d85\:65f6\:4fdd\:62a4\:ff0c\:8fd4\:56de\:6210\:5426\:6210\:529f *)
downloadOne[name_String] := Module[{ok = False, c = 0},
  While[!ok && c < $maxRetries,
    c++;
    ok = TimeConstrained[
      Quiet @ Check[NetModel[name, "EvaluationNet"]; True, False],
      $timeLimit,
      False
    ];
    If[!ok,
      Print["  retry ", c, "/", $maxRetries, " : ", name];
      Quiet @ Check[ResourceRemove @ ResourceObject @ name, Null];
    ];
  ];
  ok
]


(* \:6267\:884c\:4e0b\:8f7d\:ff0c\:8bb0\:5f55\:6210\:8d25 *)
results = Association @ MapIndexed[
  (Print[First @ #2, "/", Length @ names2Download, " : ", #1];
   #1 -> downloadOne[#1]) &,
  names2Download
];

failed = Keys @ Select[results, # === False &];
Print["\nDownloaded OK : ", Count[Values @ results, True]];
Print["Failed        : ", Length @ failed];
If[failed =!= {}, Print["Failed models:\n", StringRiffle[failed, "\n"]]];


(* ::Section:: *)
(*\:7f13\:5b58\:67e5\:770b Inspect cache*)


(* \:5217\:51fa\:672c\:5730\:5bf9\:8c61\:5e93\:4f4d\:7f6e\:4e0e\:5df2\:7f13\:5b58\:6570\:91cf *)
Print["\n$LocalBase = ", $LocalBase];
Print["LocalObjects count = ", Length @ LocalObjects[]];
