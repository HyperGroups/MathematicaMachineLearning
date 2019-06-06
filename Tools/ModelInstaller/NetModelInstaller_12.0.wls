#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*NetModel-Installer*)


(* ::Chapter:: *)
(*V12.0*)


(* ::Section:: *)
(*Prepare*)


names=NetModel[];


names//Length


(* ::Subsection:: *)
(*\:672c\:5730\:6a21\:578b*)


(* ::Text:: *)
(*\:5982\:679c\:6709\:62a5\:9519\:4fe1\:606f\:ff0c\:8bf4\:660e\:6a21\:578b\:8d44\:6e90\:6570\:636e\:6709\:4e9b\:4e0d\:5e72\:51c0\:6574\:9f50\:3002*)


localModels=Association@Select[fullModels=Table[PrintTemporary@i;i->NetModel[i,"ContentElementLocations"]@"EvaluationNet",{i,names}],Head@#[[2]]==LocalObject&];


lenLocal=Length@localModels;
Print["length@Local Models=", lenLocal]


modelNamesLocal=Keys@localModels;


(* ::Subsection:: *)
(*Cloud\:6a21\:578b*)


cloudModels=Association@Select[fullModels,Head@#[[2]]==CloudObject&];


lenCloud=Length@cloudModels;
Print["length@Cloud Models=", lenCloud]


modelNamesCloud=Keys@cloudModels;
Print["Cloud Model===\n",StringRiffle[modelNamesCloud,"\n"]]


(* ::Subsection:: *)
(*Other*)


namesOther=Complement[names,modelNamesLocal~Join~modelNamesCloud]


(* ::Section:: *)
(*Functions*)


filePathProcess[filefile_]:=Block[{dir},
Which[
StringMatchQ[$SystemID,"MacOSX"~~__],
dir=StringTrim[filefile,{"file://","file:\\\\","file:\\"}]
,
StringMatchQ[$SystemID,"Windows"~~__],
dir=StringTrim[filefile,{"file:///","file:\\\\","file:\\"}]
,
StringMatchQ[$SystemID,"Linux"~~__],
dir=StringTrim[filefile,{"file://","file:\\\\","file:\\"}]
,
True,
Print@filefile;
Print@"Problem@fun@filePathProcess@";Abort[];

]
]


(* ::Section:: *)
(*\:6a21\:578b\:5bfc\:51fa*)


Options[exportModel]={force->False};
exportModel[modelName_,dirExport_,OptionsPattern[]]:=Block[{fileResource,dir,fileDest,dirDest,uuid,file1,file2,file,files},

dir=filePathProcess[fileResource=ResourceObject[modelName]["ResourceLocations"][[1,1]]];
Print@dir;
filesRaw=FileNames["data.WLNet",dir,\[Infinity]];
files=Select[filesRaw,FileExistsQ@#&&StringContainsQ[#,"EvaluationNet"]&];
If[Length@files>0,
file=files[[1]];
If[FileExistsQ[file],
uuid=NetModel[modelName,"UUID"];
fileDest=FileNameJoin[{dirExport,uuid}];
If[DirectoryQ@dirExport,Nothing,CreateDirectory[dirExport]];
If[FileExistsQ@fileDest,Print["File Exist@",fileDest];
If[OptionValue[force],Print["Force Mode"];
DeleteFile@fileDest;
CopyFile[file,fileDest]]
,
CopyFile[file,fileDest]
]
]
,
Print["Problem@",modelName,"@dir=",dir]
](*End If*)
]


(* ::Input:: *)
(*(*exportModel[#,"/data/wolfram"]&/@{name};*)*)


(* ::Section:: *)
(*\:6a21\:578b\:5b89\:88c5*)


(* ::Subsection:: *)
(*Single Model Install from File with Wolfram Cloud Connect*)


asso["object.wl"]=<|"Get" -> ResourceSystemClient`Private`importRaw, "Format" -> "WLNet", 
 "Type" -> "Export", "FileFormat" -> LocalObjects`LocalObject`Dump`format, 
 "FileByteCount" -> LocalObjects`LocalObject`Dump`filebytecount, 
 "Version" -> 1, "ExternalData" -> "data.WLNet", "LanguageVersion" -> 11.3|>;


(* ::Text:: *)
(*\:6ce8\:610f\:4e8b\:9879\:ff1a\:6b64\:51fd\:6570\:4f1a\:8054\:7f51\:4e0b\:8f7d\:6bd4\:598220\:79d2\:949f\:ff0c\:9ed8\:8ba4\:4f1a\:4e0b\:8f7d\:5230\:4e00\:4e2a*)
(*Wolfram/Objects/Resources/d59/d5998df5-78c4-4160-88df-ebdc5672a403/metadata/put.wl*)
(*Wolfram/Objects/Resources/d59/d5998df5-78c4-4160-88df-ebdc5672a403/metadata/object.wl*)
(*Wolfram/Objects/Resources/d59/d5998df5-78c4-4160-88df-ebdc5672a403/download/EvaluationNet/41f18e3b11f833a6/data/object.wl*)
(*\:4e09\:4e2a\:6587\:4ef6\:ff0c\:5982\:679c\:6700\:540e\:4e00\:4e2a\:6ca1\:6709\:4e0b\:8f7d\:ff0c\:4f1a\:81ea\:5df1\:751f\:6210\:ff0c\:524d\:4e24\:4e2a\:4e5f\:53ef\:4ee5\:81ea\:5df1\:751f\:6210\:ff0c\:53ef\:4ee5\:5199\:4e00\:4e2a\:65b0\:7684\:65b9\:6cd5\:3002*)


(*NetModel Will Download some metadata from Wolfram Cloud*)
Options[modelInstallFile]={force->False,TimeLimit->20,dirRelative->"download/EvaluationNet/41f18e3b11f833a6/data"};
modelInstallFile[modelName_,fileIn_String,OptionsPattern[]]:=Block[{dir,fileDest,dirDest,assoConfig,file,fileConfig,obj},
dir=filePathProcess[ResourceObject[modelName]["ResourceLocations"][[1,1]]];
If[DirectoryQ@ToString@dir,Nothing,Print["Not Directory@",dir];Abort[]];
fileDest=FileNameJoin[{dir,OptionValue[dirRelative],"data.WLNet"}];
dirDest=DirectoryName@fileDest;
ResourceRemove[obj=ResourceObject@modelName];
Print["Download config files for a while, TimeLimit->",OptionValue[TimeLimit]];
TimeConstrained[ResourceData[obj],OptionValue[TimeLimit]];
If[DirectoryQ@dirDest,Nothing,CreateDirectory[dirDest]];
If[OptionValue[force],If[FileExistsQ@fileDest,DeleteFile@fileDest]];
file=fileIn;
If[FileExistsQ[file],CopyFile[file,fileDest];
fileConfig=FileNameJoin[{dir,"metadata","put.wl"}];
fileConfigObject=FileNameJoin[{dir,OptionValue[dirRelative],"object.wl"}];
If[FileExistsQ@fileConfigObject,Nothing,
Export[fileConfigObject,asso["object.wl"]];
];

If[FileExistsQ@fileConfig,
assoConfig=Import[fileConfig];
assoConfig["ContentElementLocations"]["EvaluationNet"]=LocalObject[dirDest];
Export[fileConfig,assoConfig]
,Print[" model config file not exist@Please Re-Run the Code or Create a config File"]]
,Print[" model file not exist"]
];
Print["Please Quit Kernel to make the model usable: Evaluate@Quit[]"]
]


(* ::Input:: *)
(*(*modelInstallFile["Wolfram AudioIdentify V1 Trained on AudioSet Data","/Users/hypergroups/Wolfram AudioIdentify V1 Trained on AudioSet Data.WLNet"]*)*)


(* ::Subsection:: *)
(*BatchInstall*)


assoModelName2UUID=Association[#->NetModel[#,"UUID"]&/@names];


assoUUID2ModelName=Association[NetModel[#,"UUID"]->#&/@names];


Keys[assoUUID2ModelName]==Values[assoModelName2UUID]


files=FileNames["*.WLNet","D:\\wolfram\\NetModels"];


assoNamesFiles=KeyMap[assoUUID2ModelName,GroupBy[files,FileBaseName,First]];


assoNames2Install=KeyTake[assoNamesFiles,modelNamesCloud]


asso2Install=KeyValueMap[modelInstallFile[#1,#2]&,assoNames2Install]


(*exportModels[#,"D:\\wolfram\\NetModels"]&/@modelNamesCloud*)


(*dirResource="D:\\wolfram\\NetModels";*)


(*uuid2Install=Values@KeyTake[assoModelName2UUID,modelNamesCloud]*)


(* ::Input:: *)
(*(*modelInstallUUID[#,dirResource,TimeLimit\[Rule]10]&/@{"d5547754-96cb-4386-8c50-913d84f24da3"}*)*)
