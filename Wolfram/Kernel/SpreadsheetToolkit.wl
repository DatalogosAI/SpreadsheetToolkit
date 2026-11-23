(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["DanieleGregori`SpreadsheetToolkit`"];


SpreadsheetImport::usage="SpreadsheetImport[file] imports the sheets, data, formulas and defined names of a spreadsheet file.";
ImportSpreadsheetSheets::usage="ImportSpreadsheetSheets[file] imports the sheet names of a spreadsheet file.";
ImportSpreadsheetData::usage="ImportSpreadsheetData[file] imports the cell values of each sheet of a spreadsheet file.";
ImportSpreadsheetFormulas::usage="ImportSpreadsheetFormulas[file] imports the cell formulas of each sheet of a spreadsheet file.";
ImportSpreadsheetDefinedNames::usage="ImportSpreadsheetDefinedNames[file] imports the defined names of a spreadsheet file, each as {name, scope, reference}.";


SpreadsheetConvert::usage="SpreadsheetConvert[cell] converts a cell into a grid position {row, column}.\nSpreadsheetConvert[{row, column}] converts a grid position into a cell.\nSpreadsheetConvert[col] gives the letters of column col.";
PositionToSpreadsheetIndex::usage="PositionToSpreadsheetIndex[{row, column}] gives the cell at a grid position.\nPositionToSpreadsheetIndex[n] gives the letters of column n.";
SpreadsheetIndexToPosition::usage="SpreadsheetIndexToPosition[cell] gives the grid position {row, column} of a cell.";


SpreadsheetContract::usage="SpreadsheetContract[cells] contracts a list of cells into ranges, aggregating rows, columns and ranges.";
ContractSpreadsheetRows::usage="ContractSpreadsheetRows[cells] aggregates adjacent cells in each row into ranges.";
ContractSpreadsheetColumns::usage="ContractSpreadsheetColumns[cells] aggregates adjacent cells in each column into ranges.";
ContractSpreadsheetRowRanges::usage="ContractSpreadsheetRowRanges[ranges] aggregates ranges with the same columns in consecutive rows.";
ContractSpreadsheetColumnRanges::usage="ContractSpreadsheetColumnRanges[ranges] aggregates ranges with the same rows in adjacent columns.";


SpreadsheetExpand::usage="SpreadsheetExpand[range] expands a range into its cells, leaving other labels unchanged.";


SpreadsheetParse::usage="SpreadsheetParse[formula] finds all the cells involved in a spreadsheet formula.";


SpreadsheetTrace::usage="SpreadsheetTrace[file, cell] traces the dependencies of a cell in a spreadsheet file.\nSpreadsheetTrace[{sheets, data, formulas, names}, cell] traces them given the sheets, data, formulas and defined names.";


Begin["`Private`"];


(* ::Section::Closed:: *)
(*Import*)


(* ::Subsection::Closed:: *)
(*ImportOnce*)


importOnce=ResourceFunction["ImportOnce","Function"];


(* ::Subsection::Closed:: *)
(*Particular*)


ImportSpreadsheetSheets[file_]:=importOnce[ExpandFileName@file,"Sheets"]


ImportSpreadsheetData[file_]:=importOnce[ExpandFileName@file,"Data"]


ImportSpreadsheetFormulas[file_]:=importOnce[ExpandFileName@file,"Formulas"]


ImportSpreadsheetDefinedNames[file_]:=
	With[{wb=Quiet@
				importOnce[ExpandFileName@file,
							{"ZIP","xl/workbook.xml",(*maybe redundant*)"XMLObject"}]},
	
		If[MatchQ[wb,XMLObject[_][___]],
		
			With[{shs=Cases[wb,
							XMLElement["sheet",a_,_]:>Lookup[a,"name"],
							Infinity]},
							
				Cases[
					wb,
					XMLElement["definedName",a_,{ref_String}]:>
						With[{n=Lookup[a,"name"],id=Lookup[a,"localSheetId",""]},
						
							Which[
								id==="",
									{n,"",ref},
								StringMatchQ[id,DigitCharacter..]&&FromDigits[id]<Length[shs],
									{n,shs[[FromDigits[id]+1]],ref},
								True,
									Nothing]],
					Infinity]],
				
			(*failover path*)			
			With[{names=Quiet@
							importOnce[ExpandFileName@file,
										"NamedRanges"]},
					If[AssociationQ[names],names,<||>]]]]


(* ::Subsection::Closed:: *)
(*General*)


Clear[SpreadsheetImport]
SpreadsheetImport[file_]:=
	Join[
		importOnce[ExpandFileName@file,{{"Sheets","Data","Formulas"}}],
		{ImportSpreadsheetDefinedNames[file]}]


(* ::Section::Closed:: *)
(*Conversion*)


(* ::Subsection::Closed:: *)
(*IndexToPosition*)


SpreadsheetIndexToPosition[cellpatt_]:=
	Block[{row,collett},
		row=First@StringCases[cellpatt,NumberString];
		collett=StringDelete[cellpatt,{NumberString,"$"}];
		{FromDigits@row,FromDigits[ToCharacterCode@collett-64,26]}]
IndexToPosition=SpreadsheetIndexToPosition;


(* ::Subsection::Closed:: *)
(*PositionToIndex*)


PositionToSpreadsheetIndex[{r_Integer?Positive,c_Integer?Positive}]:=
	PositionToSpreadsheetIndex[c]<>ToString[r]
PositionToSpreadsheetIndex[n_Integer?Positive]:=
	If[n<=26,
		FromCharacterCode[64+n],
		PositionToSpreadsheetIndex[Quotient[n-1,26]]<>FromCharacterCode[65+Mod[n-1,26]]]
PositionToIndex=PositionToSpreadsheetIndex;


(* ::Subsection::Closed:: *)
(*General*)


SpreadsheetConvert[cell_String]:=SpreadsheetIndexToPosition[cell]
SpreadsheetConvert[arg_]:=PositionToSpreadsheetIndex[arg]


(* ::Section:: *)
(*Tracing*)


(*equivalent to
SpreadsheetTrace=ResourceFunction["SpreadsheetTrace",ResourceVersion->"2.0.0"];*)


(* ::Subsection:: *)
(*Import*)


(* ::Subsubsection:: *)
(*ImportOnce*)


importOnce=ResourceFunction["ImportOnce","Function"];


(* ::Subsubsection::Closed:: *)
(*Import defined names*)


(*defined names as stored in xl/workbook.xml (xlsx, xlsm): {{name, scope, reference}, ...}, scope "" for a workbook
name or the sheet of a sheet name. Import "NamedRanges" drops the scope and keeps only one of the names with the same
text on a sheet, so it is used only for files without xl/workbook.xml: <|sheet -> <|name -> {rows, columns}|>|>, all
taken as workbook names; <||> if there are none or the format has no names*)
importNames[file_]:=
	With[{wb=Quiet@
				importOnce[ExpandFileName@file,
							{"ZIP","xl/workbook.xml",(*maybe redundant*)"XMLObject"}]},
	
		If[MatchQ[wb,XMLObject[_][___]],
		
			With[{shs=Cases[wb,
							XMLElement["sheet",a_,_]:>Lookup[a,"name"],
							Infinity]},
							
				Cases[
					wb,
					XMLElement["definedName",a_,{ref_String}]:>
						With[{n=Lookup[a,"name"],id=Lookup[a,"localSheetId",""]},
						
							Which[
								id==="",
									{n,"",ref},
								StringMatchQ[id,DigitCharacter..]&&FromDigits[id]<Length[shs],
									{n,shs[[FromDigits[id]+1]],ref},
								True,
									Nothing]],
					Infinity]],
				
			(*failover path*)			
			With[{names=Quiet@
							importOnce[ExpandFileName@file,
										"NamedRanges"]},
					If[AssociationQ[names],names,<||>]]]]


(* ::Subsection::Closed:: *)
(*Resolution*)


(* ::Subsubsection::Closed:: *)
(*Cell resolution*)


(* ::Input::Initialization:: *)
(*ssInd2Pos=ResourceFunction["SpreadsheetIndexToPosition"];
pos2SsInd=ResourceFunction["PositionToSpreadsheetIndex"];*)


(*cell address -> {row, column}: B78 -> {78, 2}, letters A-Z then digits, as the resource function
SpreadsheetIndexToPosition (which costs about 70 microseconds per call); absolute references are read too, $B$78,
B$78, $B78 -> {78, 2} (the starting cell may be written so).
Upper-case letters only: formulas store references in upper case, and the main function puts the starting cell in
upper case, so there is no ToUpperCase here (a lower-case b5 would give {5, 34}).
The trace gives only such addresses, so the check of the condition is left commented: uncommented, other strings
stay unevaluated. The pattern is a constant, as written in the condition it would be rebuilt at every call;
CharacterRange["0","9"], as DigitCharacter also matches other scripts' digits. The same check is faster (about 1.7
instead of 2.7 microseconds) as /;StringMatchQ[cellpatt,RegularExpression["\\$?[A-Z]+\\$?[0-9]+"]]*)

(*addrPatt=Repeated["$",{0,1}]~~CharacterRange["A","Z"]..~~Repeated["$",{0,1}]~~CharacterRange["0","9"]..;*)
(*or*)
(*addrPatt=RegularExpression["\\$?[A-Z]+\\$?[0-9]+"];*)

ssInd2Pos[cellpatt_String](*/;StringMatchQ[cellpatt,addrPatt]*):=
	Block[{row,collett},
		row=First@StringCases[cellpatt,NumberString];
		collett=StringDelete[cellpatt,{NumberString,"$"}];
		{FromDigits@row,FromDigits[ToCharacterCode@collett-64,26]}]

(*column number -> letters, 28 -> AB, and {row, column} -> cell address, {78, 2} -> B78, as the resource function
PositionToSpreadsheetIndex*)
pos2SsInd[n_Integer?Positive]:=
	If[n<=26,
		FromCharacterCode[64+n],
		pos2SsInd[Quotient[n-1,26]]<>FromCharacterCode[65+Mod[n-1,26]]]

pos2SsInd[{r_Integer?Positive,c_Integer?Positive}]:=
	pos2SsInd[c]<>ToString[r]


(* ::Input::Initialization:: *)
row2Ind=ToExpression@First@
			StringCases[#,NumberString]&;


(* ::Subsubsection::Closed:: *)
(*Sheet resolution*)


(*sheet name without quotes: 'O''Brien' -> O'Brien; formulas quote a name only when needed, so quoted and plain forms are compared this way*)
sheetName[s_String]:=
	If[StringMatchQ[s,"'"~~___~~"'"],
		StringReplace[StringTake[s,{2,-2}],"''"->"'"],
		s]
(*sheet name quoted as in formulas: O'Brien -> 'O''Brien'*)
quoteSheet[s_String]:=
	"'"<>StringReplace[sheetName[s],"'"->"''"]<>"'"


(* ::Subsubsection::Closed:: *)
(*Defined names*)


(*names from xl/workbook.xml: {{name, scope, reference}, ...}; label n for a workbook name, Sheet!n for a sheet name
(skipped if its sheet is not in the workbook)*)
namedCells[names_List]:=
	Association@Cases[names,
		{n_String,sc_String,ref_String}:>
			With[{lbl=If[sc==="",n,Replace[First@FirstPosition[ssSheetNames,sheetName[sc],{None},{1}],k_Integer:>ssSheets[[k]]<>"!"<>n]],
					rng=nameRange[ref]},
				If[lbl=!=None&&rng=!={},
					lbl->rng,
					Nothing]]]

(*names from Import "NamedRanges": <|sheet -> <|name -> {rows, columns}|>|>, taken as workbook names;
sheet index None for a sheet not in the workbook: the name stays unresolved*)
namedCells[names_Association]:=
	Association@Flatten@KeyValueMap[
		Function[{sh,rngs},
			With[{k=First@FirstPosition[ssSheetNames,sheetName[sh],{None},{1}]},
				KeyValueMap[
					Function[{name,rng},
						If[MatchQ[rng,{Span[_Integer,_Integer],Span[_Integer,_Integer]}],
							name->{k,{rng[[1,1]],rng[[2,1]]},{rng[[1,2]],rng[[2,2]]}},
							Nothing]],
					rngs]]],
		names]

(*reference of a name, Sheet!A1 or Sheet!A1:B2 -> {sheet index, first cell position, last cell position}, sheet index
None for a sheet not in the workbook (the name stays unresolved); {} for anything else: constants, formulas, several
ranges, whole rows or columns, #REF!*)
nameRange[ref_String]:=
	Replace[
		StringCases[ref,{
			StartOfString~~p:sheetPatt~~"!"~~a:cellPatt~~":"~~b:cellPatt~~EndOfString:>{p,a,b},
			StartOfString~~p:sheetPatt~~"!"~~a:cellPatt~~EndOfString:>{p,a,a}}],
		{
			{{p_,a_,b_}}:>
				With[{k=First@FirstPosition[ssSheetNames,sheetName[p],{None},{1}],
						pa=ssInd2Pos@StringDelete[a,"$"],pb=ssInd2Pos@StringDelete[b,"$"]},
					{k,MapThread[Min,{pa,pb}],MapThread[Max,{pa,pb}]}],
			_->{}}]
			
definedNames=<||>;


(* ::Subsubsection::Closed:: *)
(*Cross-sheet resolution*)


(* ::Input::Initialization:: *)
cleanCellRef[cell_String]:=
	If[StringContainsQ[cell,"!"],

		Last@StringSplit[cell,"!"],

		cell]


cellSheetIndex[cell_String,default_Integer:1]:=
	Which[
	
		KeyExistsQ[definedNames,cell],
			definedNames[cell][[1]],
			
		StringContainsQ[cell,"!"],
			FirstPosition[
				ssSheetNames,
				sheetName@First@StringSplit[cell,"!"],
				{None},{1}]
					//First,
		
		True,
			default]


(* ::Subsubsection::Closed:: *)
(*Explicit sheets*)


(*label of a shared definition: the cell on its sheet k, with the sheet as in ssSheets whether or not the reference
wrote one (cleanCellRef drops it); defined names keep their name*)
qualifiedLabel[cell_String,k_Integer]:=
	If[KeyExistsQ[definedNames,cell],
		cell,
		ssSheets[[k]]<>"!"<>cleanCellRef[cell]]

(*label of a cell in the trace: as written, or with "ExplicitSheets" a cell written without its sheet gets the sheet k
it resolves to, in the form used for shared definitions; defined names keep their name*)
cellLabel[cell_String,k_Integer]:=
	If[And[explsheets,!StringContainsQ[cell,"!"]],
			qualifiedLabel[cell,k],
			cell]


(* ::Subsection::Closed:: *)
(*Parsing*)


(* ::Subsubsection::Closed:: *)
(*Main patterns*)


(*a column: optional $ and at most 3 letters (last column XFD), so Tasso1 or ATAN2 are not cells*)
colPatt=Repeated["$",{0,1}]~~Repeated[LetterCharacter,{1,3}];

(*a cell: column, optional $, row*)
cellPatt=colPatt~~Repeated["$",{0,1}]~~DigitCharacter..;


(* ::Input::Initialization:: *)
(*argument of SpreadsheetTrace: a cell, optionally with its sheet*)
cellGenPatt=Repeated[__~~"!",{0,1}]~~cellPatt;

(*word: function or defined name, plain sheet name, number, TRUE, ...*)
wordPatt=(WordCharacter|"_"|".")..;

(*quoted sheet name, '' escapes an apostrophe*)
quotedPatt="'"~~(Except["'"]|"''")..~~"'";

(*sheet as written before "!": 'any name', plain name, or a 3D span Jan:Mar*)
sheetPatt=quotedPatt|(wordPatt~~Repeated[":"~~wordPatt,{0,1}]);


(* ::Subsubsection::Closed:: *)
(*Range expansion*)


(*cells of the range a:b, row by row: B2:C3 -> B2, C2, B3, C3*)
rangeCells[a_String,b_String]:=
	With[{pa=ssInd2Pos@StringDelete[a,"$"],pb=ssInd2Pos@StringDelete[b,"$"]},
	
		Outer[
			#2<>#1&,
			ToString/@Range@@MinMax@{pa[[1]],pb[[1]]}(*MinMax since Excel is not sensitive to reverse order*),
			pos2SsInd/@Range@@MinMax@{pa[[2]],pb[[2]]}]
				//Flatten]


(*cells of the columns a:b down to row nr, column by column: A:B -> A1, ..., Anr, B1, ..., Bnr*)
columnCells[a_String,b_String,nr_Integer]:=
	With[{ca=ssInd2Pos[StringDelete[a,"$"]<>"1"][[2]],cb=ssInd2Pos[StringDelete[b,"$"]<>"1"][[2]]},
		
		Outer[
			#1<>#2&,
			pos2SsInd/@Range@@MinMax@{ca,cb},
			ToString/@Range[nr]]
				//Flatten]


(* ::Subsubsection::Closed:: *)
(*Sheet references*)


(*sheet as written -> {{label prefix, sheet index}, ...}: one sheet, or each sheet of a 3D span; "" is the formula's sheet si.
A sheet not in the workbook, or a 3D span with such an end, gives index None: the reference is kept as written, unresolved*)
sheetRefs["",si_Integer]:={{"",si}}

sheetRefs[p_String,si_Integer]:=
	With[{ks=First@FirstPosition[ssSheetNames,#,{None},{1}]&/@
				StringSplit[sheetName[p],":"](*possible 3D span of sheets*)},
				
		Which[
		
			Length[ks]==1,
				{{
					p<>"!",
					First[ks]}},
				
			MemberQ[ks,None],
				{{
					p<>"!",
					None}},
				
			True,
				Table[
					{
					If[StringStartsQ[p,"'"],
						quoteSheet[ssSheetNames[[k]]],
						ssSheetNames[[k]]]<>"!",
						k},
					{k,Min[ks],Max[ks]}]]]


(* ::Subsubsection::Closed:: *)
(*Labels of references*)


(*labels of a reference found by the parser, complete of sheet references*)

refLabels[{type_,p_(*sheet*),a_(*initial cell/column*),b_(*final cell/column*)},si_]:=
	(*prefix as written <> cell, on each sheet of the reference; on a sheet not in the workbook the rows of
	whole columns are unknown, so A:C stays as written*)
	
	Function[
		{pre,k},
		
		pre<>#&/@
			Which[
			
				type==="columns"&&k===None(*no specified sheet*),
					{StringDelete[a,"$"]<>":"<>StringDelete[b,"$"]},
					
				type==="columns",
					columnCells[a,b,If[1<=k<=Length[ssRows],ssRows[[k]](*rows of sheet k*),0(*no specified sheet*)]],
					
				a===b,
					{StringDelete[a,"$"]},
					
				True,
					rangeCells[a,b]]
					
				]@@@sheetRefs[p,si]//Flatten


(*label of the name n, written p!n or n (p ""), in a formula on sheet si: as in Excel, the name of sheet p, or of the
formula's sheet, if there is one, else the workbook name; Missing if neither exists (refLabels then keeps the
reference unresolved). With a sheet p not in the workbook the label is p!n as written, unresolved*)
nameLabel[p_String,n_String,si_Integer]:=
	With[{k=If[p==="",
				si,
				First@FirstPosition[
					ssSheetNames,
					sheetName[p],
					{None},{1}]]},
					
		If[k===None,
		
			p<>"!"<>n,
			
			SelectFirst[
				{
					If[1<=k<=Length[ssSheets],ssSheets[[k]]<>"!"<>n,Nothing],
					n},
					
				KeyExistsQ[definedNames,#]&]](*always returns a single string*)]
				
refLabels[{"name",p_,n_},si_]:=
	With[{lbl=nameLabel[p,n,si]},
	
		Which[
			(*no name of that sheet nor workbook name (Excel shows #NAME?): kept as written, unresolved*)
			MissingQ[lbl],
				{unresolved[If[p==="",n,p<>"!"<>n],"UnknownName"]},
			
			(*written with a sheet not in the workbook, or a name of cells on such a sheet: one label, unresolved*)
			!KeyExistsQ[definedNames,lbl]||definedNames[lbl][[1]]===None,
				{lbl},
			
			(*a name of one cell is labelled with its name, Sheet!name for a sheet name; a name of a range gives its cells*)
			definedNames[lbl][[2]]===definedNames[lbl][[3]],
				{lbl},
			
			True,
				ssSheets[[definedNames[lbl][[1]]]]<>"!"<>#&/@
					rangeCells[pos2SsInd[definedNames[lbl][[2]]],pos2SsInd[definedNames[lbl][[3]]]]]]


(* ::Subsubsection::Closed:: *)
(*Formula parser*)


(*references, tried in this order at each position of the formula; the rule for defined names is inserted at 4*)
(*only for ranges*)
refRules={

	(*Sheet!A1:B2, Sheet!A:C, Sheet!A1*)
	p:sheetPatt~~"!"~~a:cellPatt~~":"~~Repeated[sheetPatt~~"!",{0,1}]~~b:cellPatt~~WordBoundary:>{"cells",p,a,b},
	p:sheetPatt~~"!"~~a:colPatt~~":"~~Repeated[sheetPatt~~"!",{0,1}]~~b:colPatt~~WordBoundary:>{"columns",p,a,b},
	p:sheetPatt~~"!"~~c:cellPatt~~WordBoundary:>{"cells",p,c,c},
	
	(*A1:B2, A:C, A1 on the formula's sheet*)
	a:cellPatt~~":"~~b:cellPatt~~WordBoundary:>{"cells","",a,b},
	a:colPatt~~":"~~b:colPatt~~WordBoundary:>{"columns","",a,b},
	c:cellPatt~~WordBoundary:>{"cells","",c,c},
	
	(*anything else is skipped whole: numbers, TRUE, sheet-prefixed items that are not cells, ...*)
	wordPatt|quotedPatt:>Nothing};


(*refRules with the defined names inserted at 4, as Sheet!name or name (the labels of definedNames end with the name);
built once for the names of a trace and kept until the names change*)
lastNames=None;
lastRules={};

parseRules[]:=
	If[definedNames===lastNames,
	
		lastRules,
		
		lastNames=definedNames;
		lastRules=With[{nm=Alternatives@@
								DeleteDuplicates[
									Last@StringSplit[#,"!"]&/@
										Keys[definedNames]]},
					Join[
						refRules[[;;3]],
						{p:sheetPatt~~"!"~~n:nm~~WordBoundary:>{"name",p,n},
						n:nm~~WordBoundary:>{"name","",n}},
						refRules[[4;;]]]]]


(* ::Input::Initialization:: *)
ClearAll[spreadsheetFormulaParse]

spreadsheetFormulaParse["",___]:={}

spreadsheetFormulaParse[_?NumericQ,___]:={}

(*si: sheet of the formula, for whole-column ranges without a sheet*)
spreadsheetFormulaParse[form_String,si_Integer:0]:=

	refLabels[#,si]&/@
			StringCases[

				(*drop text in double quotes and function names, keeping quoted sheet names whole*)
				StringReplace[
				form,
				{
						q:quotedPatt:>q,
						"\""~~Except["\""]...~~"\""->"",
						wordPatt~~"("->"("}],

				If[definedNames===<||>,
				refRules,
				parseRules[]]

		]//Flatten


(* ::Subsection::Closed:: *)
(*Tracing*)


(* ::Subsubsection::Closed:: *)
(*Operations*)


(* ::Input::Initialization:: *)
formulaQ[pos_,si_Integer]:=
	Part[ssFormulas[[si]],Sequence@@pos]=!=""


(* ::Input::Initialization:: *)
getFormula[fq_?BooleanQ,pos_,si_Integer]:=
	If[fq,
		Part[ssFormulas[[si]],Sequence@@pos],
		Part[ssData[[si]],Sequence@@pos]]


(* ::Input::Initialization:: *)
getParsing[fq_?BooleanQ,form_,si_Integer:0]:=
	If[!fq,
		{},
		If[tracedup,
			Identity,
			DeleteDuplicates]@Flatten@
				spreadsheetFormulaParse[form,si]]


getPosition[cell_String]:=
	If[KeyExistsQ[definedNames,cell],
		definedNames[cell][[2]],
		ssInd2Pos@cleanCellRef[cell]]


(* ::Subsubsection::Closed:: *)
(*Cell pipeline*)


(*{form, parse, sheet index} of a cell: its formula, or its value if it has none ("" beyond the used range, $Aborted
after "MaxSteps" steps); the references the formula parses to; the sheet the cell resolves to*)
cellPipeline[cell_String,si_]:=
	Block[{pos,dim,test,nsi,fq,form,parse},

		pos=getPosition[cell];
		nsi=cellSheetIndex[cell,si];
		dim=ssDims[[nsi]];
		test=Not[pos[[1]]<=dim[[1]]&&pos[[2]]<=dim[[2]]];

		(*cells beyond the used range are empty*)
		fq=!test&&formulaQ[pos,nsi];
		form=If[test,"",getFormula[fq,pos,nsi]];
		parse=getParsing[fq,form,nsi];
		
		Increment[steps];

		If[steps<=maxsteps,
			{form,parse,nsi},
			{$Aborted,{},nsi}]]


(* ::Subsubsection:: *)
(*Core recursion*)


ClearAll[iSpreadsheetTrace]

(*traces a cell, psi being the sheet of the formula that refers to it (1 for the starting cell); each cell is traced
once. The output is not the trace tree but one of:
	{label, value}: a cell without dependencies (a value, an empty cell, a formula without cell references);
	ref[k, label]: a cell with dependencies, k numbering the cells with dependencies in the order they are first
		traced (the starting cell, if it has dependencies, is 1);
	{label, Missing[reason]}: a reference that cannot be resolved (sheet not in the workbook, unknown name).
The label is the cell as written at that place (cellLabel). The tree is kept in the Block globals of the main
function: nodes[k] = {form, child outputs...}, labels[k] = the sheet-qualified label of the cell k, refs[k] = how many
times the cell k is referred to, cyclic[k] = True when k refers back to itself (circular reference, found through
active, the cells still being traced); traced and leaves map each resolved cell {sheet, position} to its k or value.
buildTrace writes the tree from the output of the starting cell, expanding every ref[k, label] to
{label, form, child traces...} or to a symbol defined in a ConditionalExpression*)

iSpreadsheetTrace[cell_String,psi_Integer]:=
	Block[{form,parse,si,key,lbl,k},

		si=cellSheetIndex[cell,psi];

		If[si===None,

			(*a sheet that is not in the workbook: the reference stays as written, unresolved*)
			{cell,Missing["UnknownSheet"]},

			(*regular sheet*)
			key={si,getPosition[cell]};
			lbl=cellLabel[cell,si](*possibly join sheet name to cell*);

			Which[

				(*a cell with dependencies traced before, or being traced (circular reference)*)
				KeyExistsQ[traced,key],
					k=traced[key];
					refs[k]+=1;
					If[KeyExistsQ[active,k],cyclic[k]=True];
					ref[k,lbl],

				(*a value read before*)
				KeyExistsQ[leaves,key],
					{lbl,leaves[key]},

				True,
					{form,parse,si}=cellPipeline[cell,psi];

					If[parse==={},

						(*value, empty cell, or formula without cell references*)
						leaves[key]=form;
						{lbl,form},

						k=Length[traced]+1;
						traced[key]=k;
						refs[k]=1(*first appearance of a cell*);
						(*label of a shared definition: sheet-qualified cell, or the defined name*)
						labels[k]=qualifiedLabel[cell,si];
						active[k]=True;
						(*recursion: the outputs of the referenced cells, on the sheet of this formula*)
						nodes[k]=Prepend[iSpreadsheetTrace[#,si]&/@parse,form];
						KeyDropFrom[active,k];
						ref[k,lbl]]]]]

(*a reference the parser could not resolve, unresolved[label, reason]: kept as written*)
iSpreadsheetTrace[unresolved[lbl_String,why_String],___]:=
	{lbl,Missing[why]}


(* ::Subsubsection:: *)
(*Trace building*)


(*symq: cells referenced more than once become \[FormalS][1], \[FormalS][2], ... in order of first appearance,
defined with their sheet-qualified label in the condition of a ConditionalExpression; the others stay in place.
not symq: every reference is written in full, as in a plain trace, except references closing a circular dependency*)
buildTrace[root_,symq_]:=
	Block[{keep,sym,expand,expandKids,tree},
	
		keep=If[symq,
					Keys@Select[refs,#>1&],
					Sort@Keys@cyclic];
		sym=AssociationThread[keep,Array[\[FormalS],Length[keep]]];
		
		expand[ref[k_,lbl_]]:=If[KeyExistsQ[sym,k],
									sym[k],
									Join[{lbl},expandKids[k]]];
		expand[x_]:=x;
		expandKids[k_]:=expandKids[k]=expand/@nodes[k];
		tree=expand[root];
		
		If[keep==={},
		
			tree,
			
			ConditionalExpression[
				tree,
				Table[
					sym[k]==Join[{labels[k]},expandKids[k]],
					{k,keep}]//Apply[And]]]]


(*number of nodes of the trace written without shared sub-traces, that is the recursion steps of a plain trace;
Infinity for circular references*)
traceSize[root_]:=
	Module[{size},
		size[ref[k_,_]]:=size[k];
		size[k_Integer]:=size[k]=If[KeyExistsQ[cyclic,k],
										Infinity,
										1+Total[size/@Rest[nodes[k]]]];
		size[_List]=1;
		
		size[root]]


(* ::Subsection::Closed:: *)
(*Main*)


Clear[SpreadsheetTrace]

Options[SpreadsheetTrace]={"TraceDuplicates"->True,MaxSteps->Infinity,"SymbolicSubtraces"->Automatic,"ExplicitSheets"->Automatic};

SpreadsheetTrace[{sheets_List,data_List,formulas_List,names:(_Association|_List):<||>},cell_String,opts:OptionsPattern[SpreadsheetTrace]]/;
	Depth@sheets==2&&Depth@data>=4&&Depth@formulas>=4&&StringMatchQ[cell,cellGenPatt]:=


	(*the Block sets the globals of the trace, read by the parser and the tracing functions instead of arguments:
	ssSheets: the sheets as written in formulas, all quoted if any name has whitespace;
	ssSheetNames: the sheet names without quotes, compared with the sheets written in formulas (sheetName);
	ssData, ssFormulas, ssRows, ssDims: values, formulas, number of rows and dimensions of each sheet;
	definedNames: label -> {sheet index, first cell position, last cell position}, for names of one rectangular range;
	the label of a workbook name is the name, of a sheet name Sheet!name with the sheet as in ssSheets (Excel allows
	one workbook name and one name per sheet with the same text);
	explsheets, tracedup: the options "ExplicitSheets" and "TraceDuplicates" (False drops repeated references within
	a formula);
	traced, leaves, refs, labels, nodes, active, cyclic, steps, maxsteps: the state of the recursion*)
	Block[{
			cl(*cell*),root(*trace of the starting cell*),steps=0,
			ssSheets=sheets,ssSheetNames,ssData=data,ssFormulas=formulas,
			definedNames,ssRows=Length/@data,ssDims=Dimensions/@data,(*data*)
			tracedup,maxsteps,symbsubtr,explsheets,(*options*)
			traced=<||>,leaves=<||>,refs=<||>,labels=<||>,nodes=<||>,active=<||>,cyclic=<||>(*global symbols*)},
			
		{tracedup,maxsteps,symbsubtr,explsheets}=OptionValue[{"TraceDuplicates",MaxSteps,"SymbolicSubtraces","ExplicitSheets"}]
													//MapAt[If[#===True||#===Automatic&&Length[ssSheets]=!=1,
																True,
																False]&,
															#,4]&;
			
		(*the cell in upper case, b5 -> B5, as ssInd2Pos reads upper-case letters only; the sheet stays as written*)
		cl=StringReplace[cell,ce:cellPatt~~EndOfString:>ToUpperCase[ce]];
		If[AnyTrue[ssSheets,StringContainsQ[#,Whitespace]&],
			(*quoted as in formulas, apostrophes doubled: O'Brien -> 'O''Brien'; quoteSheet leaves a quoted name as it is*)
			ssSheets=quoteSheet/@ssSheets;
			cl=StringReplace[cl,StartOfString~~sh__~~"!"~~ce:Except["!"]..~~EndOfString:>quoteSheet[sh]<>"!"<>ce]];
		ssSheetNames=sheetName/@ssSheets;
		(*labels of sheet names use the sheets as written in the trace*)
		definedNames=namedCells[names];
		
		(*1: the sheet of a starting cell written without one; a starting cell on a sheet not in the workbook is
		returned unresolved by iSpreadsheetTrace*)
		root=iSpreadsheetTrace[cl,1];

		(*without "ExplicitSheets" the starting cell drops its sheet when the trace stays on that sheet, as the cells on
		it, and is labelled as with "ExplicitSheets" when the trace reaches other sheets (the sheets of the cells in traced
		and leaves): ref[k, label] for a cell with dependencies, {label, value} otherwise; a sheet not in the workbook
		stays as written*)
		If[!explsheets,
			With[{lbl=If[CountDistinct[Join[Keys[traced][[All,1]],Keys[leaves][[All,1]]]]>1,
								If[StringContainsQ[cl,"!"],
										cl,
										qualifiedLabel[cl,1]],
								cleanCellRef[cl]]},
				root=Replace[root,{
						ref[k_,_]:>ref[k,lbl],
						{_String,v:Except[_Missing]}:>{lbl,v}}]]];

		(*Automatic: symbolic sub-traces when the plain trace would take more than 1000 recursion steps*)
		buildTrace[root,
					If[symbsubtr===Automatic,
							traceSize[root]>10^3,
							symbsubtr=!=False]]]

SpreadsheetTrace[file:_File|_String,cell_String,opts:OptionsPattern[SpreadsheetTrace]]/;
	FileExistsQ[file]&&StringMatchQ[cell,cellGenPatt]:=
	
	Block[{sheets,data,formulas,names},
	
		{sheets,data,formulas}=importOnce[ExpandFileName@file,{{"Sheets","Data","Formulas"}}];
		names=importNames[file];
		
		SpreadsheetTrace[{sheets,data,formulas,names},cell,opts]]


(* ::Section::Closed:: *)
(*Parsing*)


Clear[SpreadsheetParse]

SpreadsheetParse["",___]:={}

SpreadsheetParse[_?NumericQ,___]:={}

(*si: sheet of the formula, for whole-column ranges without a sheet*)
SpreadsheetParse[form_String,si_Integer:0]:=
Block[{definedNames},

	If[!ValueQ[definedNames],definedNames=<||>];
	refLabels[#,si]&/@
			StringCases[

				(*drop text in double quotes and function names, keeping quoted sheet names whole*)
				StringReplace[
				form,
				{
						q:quotedPatt:>q,
						"\""~~Except["\""]...~~"\""->"",
						wordPatt~~"("->"("}],

				If[definedNames===<||>,
				refRules,
				parseRules[]]

		]//Flatten]


(* ::Section::Closed:: *)
(*Contraction*)


(* ::Subsection::Closed:: *)
(*Resources*)


partitionWhile=ResourceFunction["PartitionWhile","Function"];


selectDiscard=ResourceFunction["SelectDiscard","Function"];


(* ::Subsection::Closed:: *)
(*Helpers*)


(* ::Input::Initialization:: *)
(*sheet of a cell label with its "!", "" for a label without sheet: the text up to the last "!", since a sheet name may contain "!" and an address cannot*)
sheetPrefix[label_String]:=StringDrop[label,-StringLength@Last@StringSplit[label,"!"]]


(* ::Subsection::Closed:: *)
(*Particular*)


(* ::Input::Initialization:: *)
ContractSpreadsheetColumns[cells_]:=
With[{q=StringMatchQ[cells,cellGenPatt](*True for cells,False for labels that are not cells:defined names*)},Join[Module[{range},Flatten@KeyValueMap[With[{sh=#1},Map[sh<>#&,#2]]&,Map[Flatten@KeyValueMap[With[{col=PositionToIndex[#1]},Map[If[Head[#]===range,col<>ToString[First[#]]<>":"<>col<>ToString[Last[#]],col<>ToString[#]]&,#2]]&,Map[With[{part=partitionWhile[#,MatchQ[BlockMap[#[[2]]-#[[1]]&,#,2,1],{1..}]&]},
Flatten@{Map[range[First[#],Last[#]]&,part],Complement[#,Flatten[part]]}]&,Union/@GroupBy[#,Last->First]]]&,GroupBy[Pick[cells,q],(sheetPrefix[#]&)->(IndexToPosition@Last@StringSplit[#,"!"]&)]]]],
DeleteDuplicates@Pick[cells,q,False]]]

ContractSpreadsheetRows[cells_]:=
With[{q=StringMatchQ[cells,cellGenPatt](*True for cells,False for labels that are not cells:defined names*)},
Join[
Module[{range},Flatten@KeyValueMap[With[{sh=#1},Map[sh<>#&,#2]]&,Map[Flatten@KeyValueMap[With[{row=ToString@#1},{cols=Map[If[Head[#]===range,StringRiffle[Map[PositionToIndex[#]<>row&,List@@#],":"],PositionToIndex[#]<>row]&,#2]},cols]&,Map[With[{part=partitionWhile[#,MatchQ[BlockMap[#[[2]]-#[[1]]&,#,2,1],{1..}]&]},
Flatten@{Map[range[First[#],Last[#]]&,part],Complement[#,Flatten[part]]}]&,Union/@GroupBy[#,First->Last]]]&,GroupBy[Pick[cells,q],(sheetPrefix[#]&)->(IndexToPosition@Last@StringSplit[#,"!"]&)]]]],
DeleteDuplicates@Pick[cells,q,False]]]


(* ::Input::Initialization:: *)
ContractSpreadsheetColumnRanges[cells_]:=
Flatten@KeyValueMap[With[{sh=#1},
Map[sh<>#&,#2]]&,Map[

Flatten@Map[If[AssociationQ[#],With[{asc=Map[SortBy[#,Last]&,#]},
With[{part=Map[partitionWhile[#,MatchQ[BlockMap[#[[2,1,2]]-#[[1,2,2]]&,#,2,1],{1..}]&]&,asc]},
Values@Merge[{part,asc},Join[If[#[[1]]=!={},Map[StringRiffle[{PositionToIndex@#[[1,1]],PositionToIndex@#[[-1,-1]]},":"]&,#[[1]]],#[[1]]],Map[StringRiffle[#,":"]&,Map[PositionToIndex,Complement[#[[2]],Flatten[#[[1]],1]],{2}]]]&]]],#]&,
KeyValueMap[If[#1==2,GroupBy[Map[IndexToPosition,#2,{2}],({#[[1,1]],#[[2,1]]}&)],#2]&,GroupBy[Map[StringSplit[#,":"]&,#],Length->(If[Length[#]==2,#,#[[1]]]&)]]]&,

GroupBy[cells,(sheetPrefix[#]&)->(Last@StringSplit[#,"!"]&)]]]

ContractSpreadsheetRowRanges[cells_]:=
Flatten@KeyValueMap[With[{sh=#1},
Map[sh<>#&,#2]]&,Map[

Flatten@Map[If[AssociationQ[#],With[{asc=Map[SortBy[#,Last]&,#]},
With[{part=Map[partitionWhile[#,MatchQ[BlockMap[#[[2,1,1]]-#[[1,2,1]]&,#,2,1],{1..}]&]&,asc]},
Values@Merge[{part,asc},Join[If[#[[1]]=!={},Map[StringRiffle[{PositionToIndex@#[[1,1]],PositionToIndex@#[[-1,-1]]},":"]&,#[[1]]],#[[1]]],Map[StringRiffle[#,":"]&,Map[PositionToIndex,Complement[#[[2]],Flatten[#[[1]],1]],{2}]]]&]]],#]&,
KeyValueMap[If[#1==2,GroupBy[Map[IndexToPosition,#2,{2}],({#[[1,-1]],#[[2,-1]]}&)],#2]&,GroupBy[Map[StringSplit[#,":"]&,#],Length->(If[Length[#]==2,#,#[[1]]]&)]]]&,

GroupBy[cells,(sheetPrefix[#]&)->(Last@StringSplit[#,"!"]&)]]]


(* ::Subsection::Closed:: *)
(*General*)


SpreadsheetContract[cells_]:=
First[TakeSmallestBy[
{ContractSpreadsheetRowRanges@Flatten@MapAt[ContractSpreadsheetColumns,
selectDiscard[StringContainsQ[#,":"]&]@ContractSpreadsheetRows[cells],
2],
ContractSpreadsheetColumnRanges@Flatten@MapAt[ContractSpreadsheetRows,
selectDiscard[StringContainsQ[#,":"]&]@ContractSpreadsheetColumns[cells],
2]},Length,1],{}]


(* ::Section::Closed:: *)
(*Expansion*)


Clear[SpreadsheetExpand]

SpreadsheetExpand["",___]:={}

SpreadsheetExpand[_?NumericQ,___]:={}

(*a range label: the sheet as written up to the last "!" (plain, quoted, or a 3D span Jan:Mar), if any, then cell:cell*)
cellRangePatt=Repeated[__~~"!",{0,1}]~~cellPatt~~":"~~cellPatt;

SpreadsheetExpand[label_String]:=
	If[StringMatchQ[label,cellRangePatt],
		(*the cells of the range row by row, each with the sheet as written*)
		sheetPrefix[label]<>#&/@rangeCells@@StringSplit[Last@StringSplit[label,"!"],":"],
		label]


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
