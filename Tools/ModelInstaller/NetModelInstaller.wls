#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*NetModelInstaller*)


(* ::Text:: *)
(*\:79bb\:7ebf / \:5185\:7f51\:5206\:53d1 Wolfram \:795e\:7ecf\:7f51\:7edc\:6a21\:578b\:7684\:73b0\:4ee3\:5316\:5de5\:5177\:ff08\:9002\:7528 Wolfram 13 / 14 / 15\:ff09\:3002*)


(* ----------------------------------------------------------------------------
   \:4e3a\:4ec0\:4e48\:91cd\:5199\:ff1a
     \:65e7\:7248 NetModelInstaller_12.0.wls \:901a\:8fc7\:624b\:5de5\:6539\:5199\:8d44\:6e90\:7f13\:5b58\:6587\:4ef6\:5b9e\:73b0\:79bb\:7ebf\:5b89\:88c5\:ff0c
     \:4f9d\:8d56\:672a\:516c\:5f00\:7684\:79c1\:6709\:7b26\:53f7\:ff08ResourceSystemClient`Private`importRaw\:3001
     LocalObjects`LocalObject`Dump`format \:7b49\:ff09\:548c\:5199\:6b7b\:7684 LanguageVersion->11.3\:ff0c
     \:8de8\:7248\:672c\:5df2\:57fa\:672c\:5931\:6548\:3002\:81ea 12.1+ \:8d77\:ff0cNetModel \:81ea\:52a8\:7f13\:5b58\:5230\:672c\:5730\:5bf9\:8c61\:5e93\:ff0c
     \:4e0d\:9700\:8981\:8fd9\:79cd\:201c\:624b\:672f\:201d\:3002

   \:73b0\:4ee3\:5316\:601d\:8def\:ff08\:5168\:90e8\:57fa\:4e8e\:6587\:6863\:5316\:673a\:5236\:ff09\:ff1a
     A. \:5728\:8054\:7f51\:673a\:5668\:4e0a\:7528 NetModelDownloader.wls \:9884\:4e0b\:8f7d\:ff0c\:6a21\:578b\:7f13\:5b58\:5728 $LocalBase\:3002
     B. \:5c06\:6574\:4e2a\:672c\:5730\:5bf9\:8c61\:5e93\:76ee\:5f55\:590d\:5236\:5230\:79bb\:7ebf\:673a\:5668\:7684\:540c\:540d $LocalBase\:3002
     C. ONNX \:683c\:5f0f\:6a21\:578b\:53ef\:7528 NetExternalObject\:ff0813.2+\:ff09\:76f4\:63a5\:5728 WL \:5185\:8fd0\:884c\:3002

   \:6ce8\:ff1a\:672c\:811a\:672c\:6d89\:53ca\:8054\:7f51 / \:6587\:4ef6\:7cfb\:7edf / \:8de8\:673a\:5668\:64cd\:4f5c\:ff0c
        \:5728\:9759\:6001\:6761\:4ef6\:4e0b\:672a\:5b9e\:673a\:9a8c\:8bc1\:ff0c\:8bf7\:5728 15.0 \:5b9e\:8dd1\:786e\:8ba4\:540e\:518d\:7528\:4e8e\:751f\:4ea7\:3002
   ---------------------------------------------------------------------------- *)


(* ::Section:: *)
(*\:67e5\:770b\:672c\:5730\:7f13\:5b58 Inspect local cache*)


(* \:5217\:51fa\:672c\:5730\:5bf9\:8c61\:5e93\:4f4d\:7f6e\:4e0e\:5df2\:7f13\:5b58\:7684\:8d44\:6e90 *)
showLocalCache[] := Module[{},
  Print["$LocalBase = ", $LocalBase];
  Print["LocalObjects count = ", Length @ LocalObjects[]];
]


(* ::Section:: *)
(*\:65b9\:6848 A+B\:ff1a\:9884\:4e0b\:8f7d\:540e\:590d\:5236\:7f13\:5b58\:76ee\:5f55*)


(* ::Text:: *)
(*\:5728\:8054\:7f51\:673a\:5668\:ff1a*)
(*  1. wolframscript -file ../NetModelDownloader.wls "<model name>"*)
(*  2. \:8bb0\:4e0b $LocalBase \:8def\:5f84\:ff08\:8fd0\:884c showLocalCache[]\:ff09\:3002*)
(*\:5728\:79bb\:7ebf\:673a\:5668\:ff1a\:5c06\:8be5\:76ee\:5f55\:6574\:4f53\:62f7\:5230\:672c\:673a\:540c\:540d $LocalBase\:ff0c\:5373\:53ef\:79bb\:7ebf\: NetModel[name]\:3002*)


(* \:5c06\:6307\:5b9a\:6a21\:578b\:7684\:672c\:5730\:7f13\:5b58\:5bfc\:51fa\:4e3a\:4e00\:4e2a .wlnet \:6587\:4ef6\:ff08\:53ef\:79fb\:690d\:5206\:53d1\:ff09 *)
exportModelFile[name_String, dirExport_String] := Module[{net, file},
  If[!DirectoryQ[dirExport], CreateDirectory[dirExport, CreateIntermediateDirectories -> True]];
  file = FileNameJoin[{dirExport, StringReplace[name, {" " -> "_", "/" -> "-"}] <> ".wlnet"}];
  net = NetModel[name, "EvaluationNet"];        (* \:9700\:5df2\:7f13\:5b58\:6216\:53ef\:8054\:7f51 *)
  Export[file, net, "WLNet"];
  Print["Exported: ", file];
  file
]


(* \:4ece .wlnet \:6587\:4ef6\:52a0\:8f7d\:7f51\:7edc\:ff08\:79bb\:7ebf\:53ef\:7528\:ff0c\:65e0\:9700\:8d44\:6e90\:7cfb\:7edf\:ff09 *)
importModelFile[file_String] := Import[file, "WLNet"]


(* ::Section:: *)
(*\:65b9\:6848 C\:ff1aONNX \:5916\:90e8\:6a21\:578b NetExternalObject\:ff0813.2+\:ff09*)


(* ::Text:: *)
(*\:4efb\:4f55 ONNX \:6a21\:578b\:53ef\:76f4\:63a5\:4ee5 NetExternalObject \:5728 WL \:5185\:8fd0\:884c\:ff0c\:9002\:5408\:79bb\:7ebf\:573a\:666f\:ff1a*)
(*  obj = NetExternalObject["model.onnx"];*)
(*  obj[input]*)


importONNX[file_String] := NetExternalObject[file]


(* ::Section:: *)
(*\:5165\:53e3 Entry*)


(* \:65e0\:53c2\:6570\:65f6\:6253\:5370\:7f13\:5b58\:4fe1\:606f\:4e0e\:7528\:6cd5\:63d0\:793a *)
If[Length @ $ScriptCommandLine <= 1,
  showLocalCache[];
  Print["\nUsage:"];
  Print["  exportModelFile[name, dir]  - \:5bfc\:51fa\:6a21\:578b\:4e3a .wlnet \:6587\:4ef6"];
  Print["  importModelFile[file]       - \:4ece .wlnet \:52a0\:8f7d\:ff08\:79bb\:7ebf\:ff09"];
  Print["  importONNX[file]            - \:52a0\:8f7d ONNX \:6a21\:578b\:ff08NetExternalObject\:ff09"];
];
