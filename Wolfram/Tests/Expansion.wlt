(* ::Package:: *)

(* ::Title:: *)
(*SpreadsheetToolkit: tests of SpreadsheetExpand*)


(* ::Text:: *)
(*SpreadsheetExpand of Kernel/SpreadsheetToolkit.wl: a range label gives its cells, with the sheet as written (plain,*)
(*quoted, or a 3D span); any other label (a cell, a defined name, whole columns) is returned as it is, so*)
(*SpreadsheetExpand undoes SpreadsheetContract (see also the ex11 tests of Contraction.wlt).*)
(*Run TestReport on this file, best in a fresh kernel. The package symbols are cleared and the package file is read*)
(*again with Get, so the tests see its current code. To test another version of the file, see the Setup.*)
(*No files and no internet connection are needed.*)


(* ::Section:: *)
(*Setup*)


(* The package file under test: Kernel/SpreadsheetToolkit.wl, or another version set just before TestReport, e.g.
SpreadsheetToolkitTests`packageFile = "/path/to/SpreadsheetToolkit-new.wl";
the setting is cleared at the end of the tests. *)
If[! StringQ[SpreadsheetToolkitTests`packageFile],
	SpreadsheetToolkitTests`packageFile = FileNameJoin[{ParentDirectory[DirectoryName[If[StringQ[$TestFileName] && $TestFileName =!= "", $TestFileName, $InputFileName]]],
		"Kernel", "SpreadsheetToolkit.wl"}]];
ClearAll["DanieleGregori`SpreadsheetToolkit`*", "DanieleGregori`SpreadsheetToolkit`Private`*"];
Get[SpreadsheetToolkitTests`packageFile];


Begin["SpreadsheetToolkitTests`"];


(* ::Subsection:: *)
(*Helpers, independent of the package code*)


columnNames = Join[CharacterRange["A", "Z"], "A" <> # & /@ CharacterRange["A", "Z"]];

(* n random cells on sheets written plain, quoted (one with "!"), unquoted with an apostrophe, as a 3D span, or
without sheet, some of them repeated, and up to three defined names *)
randomLabels[n_Integer, rows_Integer, columns_Integer] :=
	Join[
		Table[RandomChoice[{"Jan!", "'Q 3'!", "'Q1!'!", "O'Brien!", "Jan:Mar!", ""}] <> columnNames[[RandomInteger[{1, columns}]]] <>
			IntegerString[RandomInteger[{1, rows}]], n],
		RandomSample[{"start", "end", "Rate1"}, RandomInteger[{0, 3}]]]

(* a workbook in list form: no sheet name has whitespace, so the trace writes the sheet O'Brien without quotes in
the labels of the cells referred to on that sheet *)
apostropheWorkbook = {{"Main", "O'Brien"}, {{{1.}}, {{1.}, {2.}, {3.}}}, {{{"'O''Brien'!A3"}}, {{""}, {""}, {"A1+A2"}}}};


(* ::Section:: *)
(*Ranges*)


VerificationTest[
	{SpreadsheetExpand["Data!B2:B4"], SpreadsheetExpand["Data!B2:D2"]},
	{{"Data!B2", "Data!B3", "Data!B4"}, {"Data!B2", "Data!C2", "Data!D2"}},
	TestID -> "SpreadsheetExpand-column-and-row-ranges"]

VerificationTest[
	SpreadsheetExpand["'Technical Data'!B14:C25"],
	Flatten@Table["'Technical Data'!" <> c <> ToString[r], {r, 14, 25}, {c, {"B", "C"}}],
	TestID -> "SpreadsheetExpand-2D-range-row-by-row"]

VerificationTest[
	SpreadsheetExpand["Data!D3:B2"],
	{"Data!B2", "Data!C2", "Data!D2", "Data!B3", "Data!C3", "Data!D3"},
	TestID -> "SpreadsheetExpand-corners-in-reverse-order"]

VerificationTest[
	SpreadsheetExpand["Data!$B$2:$B$4"],
	{"Data!B2", "Data!B3", "Data!B4"},
	TestID -> "SpreadsheetExpand-absolute-references"]

VerificationTest[
	{SpreadsheetExpand["Data!Z5:AB5"], SpreadsheetExpand["Data!XFC1:XFD2"]},
	{{"Data!Z5", "Data!AA5", "Data!AB5"}, {"Data!XFC1", "Data!XFD1", "Data!XFC2", "Data!XFD2"}},
	TestID -> "SpreadsheetExpand-columns-after-Z"]

(* ranges of one-sheet workbooks, as SpreadsheetContract gives them for cells without sheet *)
VerificationTest[
	SpreadsheetExpand /@ {"B2:B4", "B2:D2", "AA2:AB3", "$B$2:$B$4"},
	{{"B2", "B3", "B4"}, {"B2", "C2", "D2"}, {"AA2", "AB2", "AA3", "AB3"}, {"B2", "B3", "B4"}},
	TestID -> "SpreadsheetExpand-ranges-without-sheet"]


(* ::Section:: *)
(*Sheets*)


VerificationTest[
	SpreadsheetExpand /@ {"'O''Brien'!B2:B3", "'Q1!'!B2:C2", "'Monthly Costs (1st year,\[Euro])'!F9:H9", "'2024'!B2:B3"},
	{{"'O''Brien'!B2", "'O''Brien'!B3"}, {"'Q1!'!B2", "'Q1!'!C2"},
		{"'Monthly Costs (1st year,\[Euro])'!F9", "'Monthly Costs (1st year,\[Euro])'!G9", "'Monthly Costs (1st year,\[Euro])'!H9"},
		{"'2024'!B2", "'2024'!B3"}},
	TestID -> "SpreadsheetExpand-quoted-sheet-names"]

(* as trace labels write a sheet when no sheet name has whitespace: without quotes, whatever its characters *)
VerificationTest[
	SpreadsheetExpand /@ {"O'Brien!B2:B3", "Q1-2024!B2:B3"},
	{{"O'Brien!B2", "O'Brien!B3"}, {"Q1-2024!B2", "Q1-2024!B3"}},
	TestID -> "SpreadsheetExpand-sheet-names-without-quotes"]

(* without a workbook the rows of a sheet are unknown: whole columns are kept as written, with or without sheet *)
VerificationTest[
	Flatten@{SpreadsheetExpand["Data!A:C"], SpreadsheetExpand["A:C"]},
	{"Data!A:C", "A:C"},
	TestID -> "SpreadsheetExpand-whole-columns-kept-as-written"]


(* ::Section:: *)
(*Three-dimensional references*)


(* without a workbook the sheets between Jan and Mar are unknown: the span is kept as written on each cell *)
VerificationTest[
	SpreadsheetExpand /@ {"Jan:Mar!B2:B3", "'Jan 2024:Mar 2024'!B2:C2", "Jan:Mar!$B$2:$B$3"},
	{{"Jan:Mar!B2", "Jan:Mar!B3"}, {"'Jan 2024:Mar 2024'!B2", "'Jan 2024:Mar 2024'!C2"}, {"Jan:Mar!B2", "Jan:Mar!B3"}},
	TestID -> "SpreadsheetExpand-3D-ranges"]

VerificationTest[
	Flatten@{SpreadsheetExpand["Jan:Mar!B2"], SpreadsheetExpand["'Jan 2024:Mar 2024'!B2"]},
	{"Jan:Mar!B2", "'Jan 2024:Mar 2024'!B2"},
	TestID -> "SpreadsheetExpand-3D-cells-kept-as-written"]

VerificationTest[
	With[{labels = {"Jan:Mar!B2", "Jan:Mar!B3", "Jan:Mar!C2", "Jan:Mar!C3", "'Jan 2024:Mar 2024'!D5", "'Jan 2024:Mar 2024'!D6"}},
		{Sort@SpreadsheetContract[labels], Sort@Flatten[SpreadsheetExpand /@ SpreadsheetContract[labels]] === Sort[labels]}],
	{Sort@{"Jan:Mar!B2:C3", "'Jan 2024:Mar 2024'!D5:D6"}, True},
	TestID -> "SpreadsheetExpand-3D-inverse-of-SpreadsheetContract"]


(* ::Section:: *)
(*Labels that are not ranges*)


VerificationTest[
	SpreadsheetExpand /@ {"Data!B2", "B5", "start", "'E - Bank Loan Schedule'!Loan_amount"},
	{"Data!B2", "B5", "start", "'E - Bank Loan Schedule'!Loan_amount"},
	TestID -> "SpreadsheetExpand-cells-and-defined-names-as-they-are"]

VerificationTest[
	SpreadsheetExpand /@ {"", 3.5},
	{{}, {}},
	TestID -> "SpreadsheetExpand-no-label"]


(* ::Section:: *)
(*Inverse of SpreadsheetContract*)


(* 30 random sets of labels: the cells and names come back, each once; the list gives the sets that do not *)
VerificationTest[
	BlockRandom[
		SeedRandom[20261123];
		Select[
			Table[randomLabels[RandomInteger[{1, 80}], RandomInteger[{2, 12}], RandomInteger[{2, 40}]], 30],
			Sort@Flatten[SpreadsheetExpand /@ SpreadsheetContract[#]] =!= Union[#] &]],
	{},
	TestID -> "SpreadsheetExpand-inverse-of-SpreadsheetContract-random-labels"]

(* the final cells of a trace on a workbook whose sheets are written without quotes come back after contraction *)
VerificationTest[
	With[{cells = DeleteDuplicates@Cases[SpreadsheetTrace[apostropheWorkbook, "Main!A1"], {s_String, _?NumericQ} :> s, Infinity]},
		{cells, SpreadsheetContract[cells], Flatten[SpreadsheetExpand /@ SpreadsheetContract[cells]]}],
	{{"O'Brien!A1", "O'Brien!A2"}, {"O'Brien!A1:A2"}, {"O'Brien!A1", "O'Brien!A2"}},
	TestID -> "SpreadsheetExpand-inverse-of-SpreadsheetContract-trace-labels"]


Clear[SpreadsheetToolkitTests`packageFile];
End[];
