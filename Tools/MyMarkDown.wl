(* ::Package:: *)

(* ::Title:: *)
(*MyMarkDown*)


BeginPackage["MyMarkDown`"]

MyMarkDown::usage =
        "SE\:7684\:8f85\:52a9\:7a0b\:5e8f\:ff0c\:4e3b\:8981\:529f\:80fd\:662f\:8f6c\:6362\:7b14\:8bb0\:672c\:5230SE\:7684MarkDown\:7b49\:7efc\:5408\:683c\:5f0f";


stringSplit::usage="\:5b57\:7b26\:4e32\:6309\:56fa\:5b9a\:7684\:5bbd\:5ea6\:5206\:5272\:ff0c\:6700\:597d\:662f\:5c3d\:53ef\:80fd\:5728\:4e0d\:6539\:53d8\:8bed\:4e49\:7684\:60c5\:51b5\:4e0b\:ff0c\:683c\:5f0f\:5316\:826f\:597d\:3002
SE\:7684\:5bbd\:5728Mathematica\:91cc\:8bbe\:7f6e\:4e3a90\:6bd4\:8f83\:597d\:3002
ExportTypesetOptions -> {\"PageWidth\" -> 90}";

TextReplaceRules::usage="\:53d8\:91cf\:540d\:7684\:66ff\:6362\:89c4\:5219\:ff0c
\:6587\:672c\:5355\:5143\:65f6\:6bd4\:5982\:4e00\:4e9b\:7cfb\:7edf\:51fd\:6570\:540d\:ff0c\:4e0d\:5237\:683c\:5f0f\:ff0c\:4e5f\:81ea\:52a8\:66ff\:6362\:6210\:884c\:5185\:4ee3\:7801\:683c\:5f0f\:3002\:ff08SE\:7684\:793e\:533a\:98ce\:683c\:ff09
\:4e9b\:662f\:4e0d\:5fc5\:8981\:7684\:81ea\:52a8\:66ff\:6362\:7684\:ff0c\:5982I\:5728\:4e0d\:8868\:793a\:590d\:6570\:65f6\:ff0c\:6240\:4ee5\:56f0\:96be\:662f\:5177\:6709\:591a\:4e49\:6027\:7684\:51fd\:6570\:540d\:ff0c
\:4e5f\:53ef\:4ee5\:5168\:90e8\:5168\:7528Plot[]\:6765\:533a\:522b\:662f\:5426\:662f\:51fd\:6570\:540d\:3002
\:53e6\:4e00\:4e9b\:662f\:7528\:6237\:81ea\:5b9a\:4e49\:7684\:ff0c\:6bd4\:5982Mathematica\:ff0cPython\:659c\:4f53\:663e\:793a\:7b49\:ff0c
\:53c8\:5982InlineFormula\:7b49\:5e76\:4e0d\:662f\:4e00\:4e2a\:51fd\:6570\:540d\:ff0c\:800c\:53ea\:662f\:4e00\:4e2a\:6837\:5f0f\:540d\:ff0c\:4e5f\:53ef\:4ee5\:7528\:884c\:5185\:4ee3\:7801\:663e\:793a\:ff0c\:6216\:8005\:659c\:4f53\:3002
\:76ee\:524d\:8fd8\:6709\:5404\:79cd\:95ee\:9898";


Cell2Markdown::usage ="\:5355\:5143\:8f6cMarkdown\:ff0c\:7ed9\:5355\:5143\:589e\:52a0\:5404\:79cdTag\:53ef\:4ee5\:4fee\:6539\:884c\:4e3a\:3002";
Notebook2Markdown::usage ="\:7b14\:8bb0\:672c\:8f6cMarkdown\:ff1a\:5728\:5f53\:524d\:7b14\:8bb0\:672c\:5185\:4f7f\:7528\:6b64\:51fd\:6570
\:5982Notebook2Markdown[EvaluationNotebook[]]";

Notebook2MarkdownwithImage::usage="\:6709\:56fe\:7247\:751f\:6210\:7684\:7248\:672c\:3002";

CellExpression2PlainText::usage="\:8f6c\:6362\:5355\:5143\:5185\:5bb9\:5230\:6734\:7d20\:6587\:672c\:ff0cPlainText\:6a21\:5f0f\:4e0b\:ff0c\:7a7a\:683c\:4e0d\:4fdd\:7559\:3002";
CellExpression2InputText::usage="\:8f6c\:6362\:5355\:5143\:5185\:5bb9\:5230\:8f93\:5165\:6587\:672c\:ff0cInputText\:ff0c\:7a7a\:683c\:90e8\:5206\:4fdd\:7559\:3002";

OutputCell2SE::usage="Output\:5355\:5143\:8f6cSE\:ff0c\:4e3b\:8981\:6709\:8f93\:51fa\:662f\:56fe\:7247\:ff0c\:8fd8\:662f\:5b57\:7b26\:4e32\:ff0c\:8fd8\:662f\:5355\:5143\:8868\:8fbe\:5f0f\:7684\:533a\:522b\:ff0c
\:6539\:8fdb\:96be\:70b9\:662f\:81ea\:52a8\:5224\:65ad\:3002";

InputCell2SE::usage="Input\:5355\:5143\:8f6cSE\:ff0c\:4e3b\:8981\:56f0\:96be\:662f\:4ee3\:7801\:7684\:683c\:5f0f\:5316\:3002
\:56e0\:4e3a\:5404\:79cd\:8f93\:5165\:4ee3\:7801\:7684\:5343\:53d8\:4e07\:5316\:ff0c\:76ee\:524d\:8fd8\:6709\:5404\:79cd\:95ee\:9898\:3002";

TextCell2SE::usage="Text\:5355\:5143\:8f6cSE\:ff0c\:4e3b\:8981\:662f\:56f0\:96be\:5404\:79cd\:6837\:5f0f\:53ca\:5185\:8054\:5355\:5143\:7684\:5e94\:7528\:7684\:8f6c\:5316\:3002
\:6587\:672c\:7ea2\:8272\:7b49MarkDown\:4e0d\:652f\:6301\:7684\:53ef\:4ee5\:8f6cTeX\:6548\:679c\:ff0c\:5f53\:7136\:4e0d\:8981\:6ee5\:7528\:989c\:8272\:7b49\:3002";

SectionCell2SE::usage="\:81ea\:52a8\:6dfb\:52a0\:5206\:9694\:7ebf\:ff0c\:52a0\:7c97";

InlineFormulaCell2SE::usage="InlineFormula\:6837\:5f0f\:7684\:5355\:5143\:8f6cSE\:ff0c\:53ef\:4ee5\:5e94\:7528\:5728\:6bd4\:5982Text\:6837\:5f0f\:5355\:5143\:7684\:5185\:8054\:5355\:5143\:4e2d\:3002
\:5373\:5728TextCell2SE\:4e2d\:8c03\:7528\:6b64\:6b64\:51fd\:6570\:3002DisplayFormula\:7b49\:6837\:5f0f\:4e5f\:53ef\:4ee5\:6682\:65f6\:4f7f\:7528\:6b64\:51fd\:6570\:3002";


counter::usage="\:8ba1\:6570\:5668\:ff0c\:4e00\:4e9b\:8ba1\:6570\:7684\:4e1c\:897f\:ff0c\:6bd4\:5982\:516c\:5f0f\:7f16\:53f7\:ff0c\:6240\:6709\:8f93\:5165\:8f93\:51fa\:5355\:5143\:7684\:7f16\:53f7\:7b49\:ff0c
\:4e0d\:8fc7\:7ecf\:6211\:5b9e\:8df5\:ff0c\:6548\:679c\:53ef\:80fd\:5e76\:4e0d\:53d7\:6b22\:8fce\:3002\:3002\:3002";

DisplayFormulaCount::usage="\:516c\:5f0f\:7f16\:53f7\:ff0c\:4ee5\:540e\:8981\:52a0\:4e00\:4e2a\:5224\:65ad\:ff0c\:82e5\:53ea\:6709\:4e00\:4e2a\:516c\:5f0f\:ff0c\:5219\:4e0d\:52a0\:7f16\:53f7\:3002";

styleBoxesGet::usage="\:6837\:5f0fBox\:83b7\:53d6";
inlineCellsGet::usage="\:5185\:8054\:5355\:5143\:83b7\:53d6\:ff0c\:6682\:65f6\:5185\:8054\:5355\:5143\:53ea\:652f\:6301\:5185\:8054InlineFormula\:5355\:5143\:3002
\:4e00\:4e2a\:539f\:56e0\:662f\:6d89\:53ca\:5230\:7528\:521b\:5efa\:5185\:8054\:5355\:5143\:65f6\:ff0c\:6709\:65f6\:6709InlineFormula\:6837\:5f0f\:ff0c\:6709\:65f6\:53c8\:6ca1\:6709\:6837\:5f0f\:540d\:ff0c\:6545\:4e3a\:6682\:65f6\:65b9\:4fbf\:8d77\:89c1\:7ed9\:6240\:6709\:5185\:8054\:5355\:5143\:90fd\:589e\:52a0InlineFormula\:7684\:5355\:5143\:3002";
StyleBox2SE::usage="\:4e3b\:8981\:662f";

varDebug::usage="Debug Data"


(*Begin["`Private`"];*)


(* ::Section:: *)
(*Packages and Common Functions*)


TextReplaceRules=Block[{systemNames,list2,userNames0,userNames},(systemNames=DeleteCases[Select[(StringCases[list2=Names["System`*"],RegularExpression["[a-z]+"],IgnoreCase->True]//Flatten),MemberQ[list2,#]&],"I"];

SystemNamesReplaceRules=((" "<>#<>" "&/@systemNames)/.x_String:>(x->" `"<>x<>"` "))~Join~((" "<>#<>"."&/@systemNames)/.x_String:>(x->" `"<>x<>"`."))~Join~((" "<>#<>"[]"&/@systemNames)/.x_String:>(x->" `"<>x<>"`"));

userNames0={" Python "," Mathematica "," Python."," Mathematica."};
userNames=(userNames0~Join~(ToLowerCase/@userNames0));
namesReplaceRules=Thread[Rule[usrNames,StringReplacePart[#,{" *"<>ToUpperCase@StringTake[#,1],StringTake[#,-1]<>"* "},{{1,1},{-1,-1}}]&/@usrNames]];
UserNamesReplaceRules~Join~SystemNamesReplaceRules)
];


stringReplaceRepeated[string0_,rules0_]:=Module[{string=string0,rules=rules0},Fold[StringReplace,string,rules]]


stringSplit[str_,width0_:90,indent0_:4]:=Module[{tempStr,strNew=StringReplace[str,{"\r\n"->"","\t"->""}],
indentSpaces,indent=indent0,width=width0},
indentSpaces=StringJoin[ConstantArray[" ",{indent}]];
indentSpaces<>StringJoin[Riffle[tempStr=StringTake[strNew,{First[#],Last[#]}&/@Partition[Range[StringLength@strNew],width,width,1,{}]],"\n"<>indentSpaces]]]


CellExpression2PlainText[cellExpression_]:=First[FrontEndExecute[FrontEnd`ExportPacket[cellExpression,"PlainText"]]];


CellExpression2InputText[cellExpression_]:=First[FrontEndExecute[FrontEnd`ExportPacket[cellExpression,"InputText"]]];


(* ::Section:: *)
(*Mathematica.SE*)


(* ::Subsection:: *)
(*StyleBox\:683c\:5f0f\:8f6c\:6362*)


styleBoxesGet[cellExpression_]:=Cases[cellExpression, StyleBox[x___,___], Infinity];


inlineCellsGet[cellExpression_]:=Cases[cellExpression, Cell[x___,___], Infinity];


(*Subsubtitle\:6837\:5f0f\:7684\:81ea\:52a8\:8f6c\:6362\:6210SE\:4e0a\:7684\:884c\:5185\:6837\:5f0f,Which\:51fd\:6570\:591a\:5224\:65ad\:8981\:653e\:5728\:4e4b\:524d*)
StyleBox2SE=Module[{content=StringTrim@#[[1]]},
If[Or@@{#[[2]]==="Subsubtitle",#[[2]]==="Subtitle",#[[2]]==="Title",#[[2]]==="Code"},
Which[(FontWeight/.Options[#])==="Bold"&&(FontSlant/.Options[content])==="Italic",Cell[" ***"<>content<>"*** "],(FontSlant/.Options[#])==="Italic",Cell[" *`"<>content<>"`* "],(FontWeight/.Options[#])==="Bold",Cell[" **`"<>content<>"`** "],True,Cell["`"<>content<>"`"]],Which[(FontWeight/.Options[#])==="Bold"&&(FontSlant/.Options[#])==="Italic",Cell[" ***"<>content<>"*** "],(FontSlant/.Options[#])==="Italic",Cell[" *"<>content<>"* "],(FontWeight/.Options[#])==="Bold",Cell[" **"<>content<>"** "],True,Cell[content]]]]&;


counter=(inputCellCount=1;DisplayFormulaCount=1;)


TextCell2SE[cellExpression0_Cell]:= Module[{debug,cellExpression,cellExpressionFinal,str,styleBoxes},
(*Subtitle\:6837\:5f0f\:7684\:8f6c\:6309\:952e*)
(styleBoxes = styleBoxesGet[cellNew=cellExpression=Replace[cellExpression0//.HoldPattern[StyleBox[x__,"Subtitle",___]]:>"<kbd>"<>x<>"</kbd>"//.(*\:6240\:6709\:5185\:8054\:5355\:5143\:90fd\:662fInlineFormula\:6837\:5f0f*)HoldPattern[Cell[x__,"Subtitle",___]]:>"<kbd>"<>x<>"</kbd>",Cell[x__,y___]->Insert[Cell[x,y],"InlineFormula",2],{1,-1}]];

styleBoxesNumbered0 = MapThread[#1[[1]]<>" "<>#2<>"StyleBoxes"&, {styleBoxes, IntegerString[#,10,4]& /@ Range[Length@styleBoxes]}];
styleBoxesNumbered =Cell[#(*,"Title"*)]&/@styleBoxesNumbered0;
styleBoxesPos = Position[cellExpression, StyleBox];
rulesStyleBoxes = Thread[Rule[styleBoxes,styleBoxesNumbered0]];
rulesStyleBoxesReversed = Thread[Rule[CellExpression2PlainText/@styleBoxesNumbered, 
(*\:5982\:679c\:662fTitle\:6837\:5f0f\:ff0c\:5219\:8f6cSE\:7684\:884c\:5185\:4ee3\:7801code\:ff0c\:6b64\:65f6\:5b57\:4f53\:52a0\:7c97\:ff0c\:503e\:659c\:7b49\:65e0\:6548*)Which[#[[2]]==="Title",
(CellExpression2PlainText[StyleBox2SE@#]),
#[[2]]==="Subtitle",("_"<>CellExpression2PlainText[#]<>"_"),
#[[2]]==="Subsubtitle",("**"<>CellExpression2PlainText[#]<>"**"),
True,CellExpression2PlainText@StyleBox2SE@#]&/@(styleBoxes/.StyleBox->Cell)]]);

(inlineCells = inlineCellsGet[cellExpression];
inlineCellsNumbered = ("InlineFormula "<>IntegerString[#,10,4])&/@Range[Length@inlineCells];
rulesInlineCells = Thread[Rule[inlineCells,inlineCellsNumbered]];
rulesInlineCellsReversed = Thread[Rule[inlineCellsNumbered, Which[#[[2]]==="InlineFormula",(InlineFormulaCell2SE[#]),True,CellExpression2PlainText@#]&/@inlineCells]]);

cellExpressionFinal = cellExpression//.HoldPattern[ButtonBox[s_String,BaseStyle->"Hyperlink",ButtonData->{URL[u_String],___},___]]:>StringJoin["[",s,"](",u,")"]/.rulesStyleBoxes~Join~rulesInlineCells;
(*
\:6709\:4ec0\:4e48\:8981\:66ff\:6362\:7684\:53ef\:4ee5\:5728\:8fd9\:91cc\:589e\:52a0\:5355\:5143\:7684\:66ff\:6362\:89c4\:5219\:ff0c\:6bd4\:5982\:628a\:67d0\:4e00StyleBox\:66ff\:6362\:6210\:67d0\:4e00\:683c\:5f0f\:5316\:7684\:5b57\:7b26\:4e32\:3002
*)

debug[cE]=cellExpressionFinal;

str=CellExpression2PlainText[cellExpressionFinal];
StringReplace[str,rulesStyleBoxesReversed~Join~rulesInlineCellsReversed]]


SectionCell2SE[cellExpression_Cell]:=Module[{styleName=cellExpression[[2]]},
Which[Or@@(styleName===#&/@{"Title","Section"}),"#"<>TextCell2SE[cellExpression]<>"\r\n---\r\n",
Or@@(styleName===#&/@{"Subtitle","Subsection"}),"###"<>TextCell2SE[cellExpression]<>"\r\n",
Or@@(styleName===#&/@{"subsubtitle","Subsubsection"}),"######"<>TextCell2SE[cellExpression]<>"\r\n"]]


(*\:5b57\:7b26\:4e32\:683c\:5f0f\:5316\:51fd\:6570---\:5904\:7406\:4e0d\:5408\:9002\:7684\:6362\:884c\:7b26*)
strFormatNewLine1[str0_,step_:70,wild_:{"\r\n","\r"}]:=Module[{
str=str0,wildList,length,posList,str1,str2,strFinal,list,i},
wildList=Flatten[StringSplit[#,""]&/@wild]//Union;
posList=Flatten[StringPosition[str,#][[All,1]]&/@wildList]//Union;
length=Length@posList;
list={0};For[i=0,i<length,i++,If[posList[[i]]-Last[list]>step,AppendTo[list,posList[[i]]]]];
str1=StringReplacePart[str,"Keep",{#,#+1}&/@Rest[list]];
str2=StringReplace[str1,Thread[wildList->""]];
strFinal=StringReplace[str2,"Keep"->"\r\n"]
];


InputCell2SE[cellExpression_, width_: 80, length_: 2000] := Module[{str1, str2, str3, str, strFinal}, 
str = First[FrontEndExecute[FrontEnd`ExportPacket[cellExpression, "InputText"]]];
str1= "    "<>StringDelete[str,{"\r","\n", Repeated[" ",{2,\[Infinity]}]}];
str2= StringReplace[str1, {";;\n"->"[@@@]",";;"->"[@@@]"}];
str3= StringReplace[str2, {";"->";\n    ", "[@@@]"->";;"}]
]


InlineFormulaCell2SE[cellExpression0_]:=Module[{styleBoxes, debug1,debug2, cellExpression=cellExpression0,cellExpressionFinal,str},
(styleBoxes (*(\:6682\:65f6\:662f\:5b57\:4f53\:989c\:8272)*)=(*Select[*)styleBoxesGet@cellExpression(*,MemberQ[Options[#],FontColor\[Rule]_]&]*);

styleBoxesRule[styleBoxes_]:=Module[{posAll,pos},(pos=(*{First[#],Last[#]}&@*)(posAll=Position[styleBoxes[[1]],_String,Infinity]);
If[posAll==={},"SingleColorLeft"<>styleBoxes[[1]]<>"SingleColorRight",RowBox[{"BlockColorLeft{",styleBoxes[[1]],"}BlockColorRight"}](*ReplacePart[styleBoxes[[1]],Thread[Rule[pos,{"ColorLeft---"<>#1,#2<>"---ColorRight"}&@@Extract[styleBoxes[[1]],pos]]]]*)])];
styleBoxesColored=styleBoxesRule/@styleBoxes;
rulesStyleBoxes=Thread[Rule[styleBoxes,styleBoxesColored]]
(*styleBoxesNumbered=("styleBoxes "<>IntegerString[#,10,4])&/@Range[Length@styleBoxes];
rulesStyleBoxes=Thread[ Rule[styleBoxes,styleBoxesNumbered]];

teXForm[x_]:=ExportString[x,"TeX",BoxRule];

rulesStyleBoxesReversed = Thread[Rule[styleBoxesNumbered, Which[#[[2]]==="InlineFormula",(teXForm[#]),True,#]&/@styleBoxes]]*));

cellExpressionFinal = cellExpression /. rulesStyleBoxes/."\[IndentingNewLine]"->"IndentNewLine";

str=stringReplaceRepeated[StringCases[ExportString[cellExpressionFinal, "Tex", "BoxRules"->{SubscriptBox[x,y]:>x<>"_"<>y}, 
"ConversionRules" ->{ "DisplayFormulaNumbered" -> {"$$","\\tag{"<>ToString[DisplayFormulaCount]<>"}$$"},"
" -> {"$$","$$"(*"~~~("<>ToString[DisplayFormulaCount]<>")$$"*)}, "InlineFormula" -> {"", ""}}] , "\\begin{document}" ~~ xxx___ ~~ "\\end{document}" :> StringTrim@xxx][[1]], {Shortest["\\(\\(" ~~ yyy___ ~~ "\\)\\)"] :> "$" ~~ yyy ~~ "$", Shortest["\\(" ~~ yyy___ ~~ "\\)"] :> "$" ~~ yyy ~~ "$", " { }" -> " ", (*"\\\\" -> "\n\n",*) Shortest["\\begin{doublespace}" ~~ yyy___ ~~ "\\end{doublespace}"] -> StringTrim@yyy,Shortest["\\begin{equation}" ~~ yyy___ ~~ "\\end{equation}"] :> StringTrim@yyy, "\\noindent" -> "", "\\noindent$\\pmb" -> "$", "\\section*" -> "", Shortest["\\title{" ~~ x___ ~~ "}"] ~~ y___ ~~ EndOfString -> x(*, "\\\\\r\n" -> "\r\n"*)}];
debug2=str;
(*StringReplace[str,rulesStyleBoxesReversed]*)
StringReplace[StringReplace[str,{Shortest["\\text{BlockColorLeft$\\{$}"~~x___~~"\\text{$\\}$BlockColorRight}"]:>"\\color{blue}{"<>x<>"}",
Shortest["\\text{SingleColorLeft$"~~x__~~"$SingleColorRight}"]:>"\\color{red}{"<>x<>"}",
Shortest["\\text{SingleColorLeft"~~x__~~"SingleColorRight}"]:>"\\color{red}{"<>x<>"}",
Shortest["\\text{BlockColorLeft}\\{"~~x__~~"\\}\\text{BlockColorRight}"]:>"\\color{Green}{"<>x<>"}",
"_{\\text{}}"->"","{}"->""
}],{"\n\n"->"","\r\n"->"","\\text{IndentNewLine}"->"\\\\&"(*,"{}"->*),"\\text{...}"->"\\cdots ","\\textit{$"~~x__~~"$}":>""<>x<>"",Shortest["\\$"~~x__~~"$]"]->x,"$$\\begin{align*}"->"\\begin{align*}","\\end{align*}$$"->"\\end{align*}",Shortest["\\text{"~~x__~~"}"]:>"\\text{ "<>x<>"}"}]

]


(*DisplayFormulaCell2SE[cellExpression_]:=stringReplaceRepeated[StringCases[ExportString[cellExpression, "Tex", "ConversionRules" ->{ "DisplayFormula" -> {"$$","~~~"<>ToString[DisplayFormulaCount]<>"$$"}, "InlineFormula" -> {"", ""}}] , "\\begin{document}" ~~ xxx___ ~~ "\\end{document}" -> StringTrim@xxx][[1]], {Shortest["\\(\\(" ~~ yyy___ ~~ "\\)\\)"] -> "$" ~~ yyy ~~ "$", Shortest["\\(" ~~ yyy___ ~~ "\\)"] -> "$" ~~ yyy ~~ "$", " { }" -> " ", "\\\\" -> "\n\n", Shortest["\\begin{doublespace}" ~~ yyy___ ~~ "\\end{doublespace}"] -> StringTrim@yyy, "\\noindent" -> "", "\\noindent$\\pmb" -> "$", "\\section*" -> "", Shortest["\\title{" ~~ x___ ~~ "}"] ~~ y___ ~~ EndOfString -> x, "\\\\\r\n" -> "\r\n"}]*)


(* ::Text:: *)
(*\:6362\:884c\:7684\:4e00\:4e2a\:95ee\:9898\:ff0c\:4ee3\:7801\:4e2d\:6709\:624b\:52a8\:6253\:7684\:6362\:884c\:7b26\:662f\:8981\:4fdd\:7559\:7684\:ff0c\:56e0\:4e3a\:5404\:79cd\:539f\:56e0\:ff0c\:6682\:65f6\:5148\:4e0d\:589e\:52a0\:8fd9\:4e2a\:7279\:6027*)


(* ::Text:: *)
(*Output\:5355\:5143\:ff0c\:5b57\:7b26\:4e32\:6bd4\:8f83\:5c11\:7684\:ff0cCode\:6ce8\:91ca\:ff0c\:5b57\:7b26\:4e32\:6bd4\:8f83\:591a\:7684\:5355\:5143\:8868\:8fbe\:5f0f\:ff0cGraphicsBox\:7ed3\:6784\:7684\:56fe\:7247\:3002*)


Options[OutputCell2SE]={"outputType"->"String","dirOutput"->Directory[],"title"->"temp",Debug->False,urlMode->"Relative"};


OutputCell2SE[cellExpression0_,cellID_,options: OptionsPattern[]]:=Module[{
cellExpression=cellExpression0, str, var, debug1, debug2, fileExport, dirExport, fileExportRelative},
str= First[FrontEndExecute[FrontEnd`ExportPacket[cellExpression,"PlainText"]]];
var["outputType"]=OptionValue["outputType"];
var["dirOutput"]=OptionValue["dirOutput"];
var[Debug]=OptionValue[Debug];
var["title"]=OptionValue["title"];

If[var[Debug],Print["fun@OutputCell2SE@OutputCell@cellID=",cellID,"@dirOutput=",var["dirOutput"]]];

If[var[Debug], Print["CellType@",cellExpression[[2]]]];
Which[MemberQ[cellExpression,GraphicsBox,\[Infinity],Heads->True]||
MemberQ[cellExpression,Graphics3DBox,\[Infinity],Heads->True]||
var["outputType"]=="Image"||MemberQ[cellExpression,PanelBox,\[Infinity],Heads->True]||
StringContainsQ[str,"ImageTag"],
If[var[Debug],Print@"Graphics"];(
fileExportRelative= FileNameJoin[{var["title"],"/resource/",var["title"]<>"_"<>ToString[cellID]<>".jpg"}];
fileExport= FileNameJoin[{var["dirOutput"], fileExportRelative}];
dirExport= DirectoryName@fileExport;
If[Not@DirectoryQ@dirExport,CreateDirectory[dirExport]];
If[var[Debug],Print@{"fileExport=",fileExport,"fileExportRelative=",fileExportRelative}];
Check[Export[fileExport, cellExpression,ImageResolution->100,ImageSize->Scaled[2]],
Print@"ExportFailed"];
If[OptionValue[urlMode]==="Relative",
"![OutputCell]("<>fileExportRelative<>")","![OutputCell]("<>fileExport<>")"]
)
,
True,Print@"RawString";str,
(*StringLength@str<3000, str; Print@"Here",*)
(*">     (*Output cellExpression, You can copy to Notebook or just Skip*)\n     "<>StringReplace[str, "\r\n"->""]*)
True, Print@"CheckOutputCell"

]
]


Options[Cell2Markdown]={"dirOutput"->Directory[],"title"->"temp",StringReplace->False,Debug->False};
Cell2Markdown[cellExpression0_Cell,cellID_Integer,options:OptionsPattern[]]:=Module[{cellExpression=cellExpression0, var, strInput, styleName, x1},
var["dirOutput"]=OptionValue["dirOutput"];
var["title"]=OptionValue["title"];
var[Debug]=OptionValue[Debug];
styleName=cellExpression[[2]];
If[var[Debug],Print["fun@Cell2Markdown=",cellID]];
Which[Options[cellExpression,CellTags][[1,2]]==="PlainText",First[FrontEndExecute[FrontEnd`ExportPacket[cellExpression,"PlainText"]]],
Options[cellExpression,CellTags][[1,2]]==="cellExpression", 
Nothing,
Options[cellExpression,CellTags][[1,2]]==="Image"||Head[cellExpression[[1,1]]]===GraphicsBox,
OutputCell2SE[cellExpression,cellID,"title"->var["title"],"outputType"->"Image","dirOutput" -> var["dirOutput"]],
Or@@(styleName==#&/@{(*"InlineFormula","DisplayFormula",*)(*"Title","Section","Subsection","Subsubsection",*)"Text"}),
If[var[Debug],Print@"TextCell"];If[var[StringReplace]===True,StringReplace[TextCell2SE[cellExpression],TextReplaceRules],TextCell2SE[cellExpression]],(*\:975eText\:8f6c\:4e3aText---*)
Or@@(styleName==#&/@{"Title","Subtitle","Subsubtitle","Section","Subsection","Subsubsection"}),SectionCell2SE[cellExpression],Or@@(styleName==#&/@{"InlineFormula"}),InlineFormulaCell2SE[cellExpression],
Or@@(styleName==#&/@{"DisplayFormula","DisplayFormulaNumbered"}),DisplayFormulaCount++;(*(\:5bf9\:9f50)*)StringReplace[strFormula=InlineFormulaCell2SE[cellExpression],{StartOfString~~"\\[":>"$\\begin{align*}","\\]"~~EndOfString:>"\\end{align*}$"}],
Or@@(styleName==#&/@{"Input","Code"}),
(strInput=InputCell2SE[cellExpression])<>"",
Or@@(styleName==#&/@{"Output"}),
Print["Here@OutputCell2SE"];OutputCell2SE[cellExpression,cellID,"title"->var["title"],"dirOutput" -> var["dirOutput"]],
Or@@(styleName==#&/@{"Program","Quote"}),">"<>First@FrontEndExecute[FrontEnd`ExportPacket[cellExpression,"PlainText"]],
True,
First@FrontEndExecute[FrontEnd`ExportPacket[cellExpression,"PlainText"]]]]//Quiet


Options[Notebook2Markdown]={"dirOutput"->Directory[], "title"->"temp",Debug->False,StringReplace->False};
Notebook2Markdown[nb0_: (nb := EvaluationNotebook[]), options:OptionsPattern[]] := Module[
{nb = nb0,cellsExpressionRaw,var,cellsString,cellsExpression,inputCells,inputCellCount,htmlString}, 
var["dirOutput"]=OptionValue["dirOutput"];
var["title"]=OptionValue["title"];

Which[Head@nb===NotebookObject,
cellsExpressionRaw = NotebookRead[Cells[nb]],
ListQ@nb&&Head[nb[[1]]]===Cell,cellsExpressionRaw=nb0,
True,
Print["Problem@Notebook@Notebook2Markdown"];
Abort[]];

cellsExpression = Select[DeleteCases[cellsExpressionRaw, Cell[___, CellTags -> "Ignore"]],StringFreeQ[ToString@#,"Notebook2Mark"]&]; 
inputCells = Cells[nb, CellStyle -> "Input"]; inputCellCount = 1; 
DisplayFormulaCount = 0;(*inputCellCount=Length@inputCells;*)
cellsString= MapIndexed[Cell2Markdown[#1, First@#2,"title"->var["title"],"dirOutput"->var["dirOutput"],Debug->OptionValue[Debug],StringReplace->OptionValue[StringReplace]] & , 
cellsExpression
];
htmlString = StringRiffle[ToString /@cellsString, "\r\n"]; 
htmlString // CopyToClipboard]


(* ::Section:: *)
(*End*)


(*End[]*)


EndPackage[ ]
