(* ::Package:: *)

(* ::Title:: *)
(*SpreadsheetToolkit: tests of SpreadsheetParse*)


(* ::Text:: *)
(*SpreadsheetParse of Kernel/SpreadsheetToolkit.wl: the cells a formula refers to, without a workbook, so sheets are*)
(*kept as written and defined names are not resolved.*)
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


(* ::Section:: *)
(*Examples of ResourceDefinition.nb*)


(* the formulas of example 1 *)
VerificationTest[
	AssociationMap[SpreadsheetParse, {"A1+B1", "C1*2", "D1+A1", "A3^2", "B3-A3", "LEN(A5)", "B5+1"}],
	<|"A1+B1" -> {"A1", "B1"}, "C1*2" -> {"C1"}, "D1+A1" -> {"D1", "A1"}, "A3^2" -> {"A3"}, "B3-A3" -> {"B3", "A3"},
		"LEN(A5)" -> {"A5"}, "B5+1" -> {"B5"}|>,
	TestID -> "SpreadsheetParse-examples-of-ResourceDefinition"]

(* an empty cell or a value is not a formula *)
VerificationTest[
	SpreadsheetParse /@ {"", 3.5, 7},
	{{}, {}, {}},
	TestID -> "SpreadsheetParse-no-formula"]


(* ::Section:: *)
(*Cells and ranges*)


VerificationTest[
	{SpreadsheetParse["SUM(B2:B4)"], SpreadsheetParse["SUM(B2:D2)"]},
	{{"B2", "B3", "B4"}, {"B2", "C2", "D2"}},
	TestID -> "SpreadsheetParse-column-and-row-ranges"]

VerificationTest[
	SpreadsheetParse["SUM(B2:C3)"],
	{"B2", "C2", "B3", "C3"},
	TestID -> "SpreadsheetParse-2D-range-row-by-row"]

VerificationTest[
	SpreadsheetParse["SUM(C3:B2)"],
	{"B2", "C2", "B3", "C3"},
	TestID -> "SpreadsheetParse-corners-in-reverse-order"]

VerificationTest[
	{SpreadsheetParse["$B$2+B$3+$C4"], SpreadsheetParse["SUM($B$2:$B$4)"]},
	{{"B2", "B3", "C4"}, {"B2", "B3", "B4"}},
	TestID -> "SpreadsheetParse-absolute-references"]

VerificationTest[
	SpreadsheetParse["XFD1048576+AAA1"],
	{"XFD1048576", "AAA1"},
	TestID -> "SpreadsheetParse-columns-after-Z"]

VerificationTest[
	{SpreadsheetParse["SUM(B2:B3*C2:C3)"], SpreadsheetParse["B2 + C2"]},
	{{"B2", "B3", "C2", "C3"}, {"B2", "C2"}},
	TestID -> "SpreadsheetParse-ranges-inside-expressions"]

VerificationTest[
	SpreadsheetParse["A1+B1+A1"],
	{"A1", "B1", "A1"},
	TestID -> "SpreadsheetParse-order-of-appearance-with-repeats"]


(* ::Section:: *)
(*Sheets*)


VerificationTest[
	SpreadsheetParse["Input!B2*2"],
	{"Input!B2"},
	TestID -> "SpreadsheetParse-sheet-reference"]

VerificationTest[
	SpreadsheetParse /@ {"'Monthly Costs (1st year,\[Euro])'!F9+1", "'O''Brien'!B3", "SUM('2024'!B2:B3)", "'Q1!'!B2",
		"'Sheet 2'!A1:A2"},
	{{"'Monthly Costs (1st year,\[Euro])'!F9"}, {"'O''Brien'!B3"}, {"'2024'!B2", "'2024'!B3"}, {"'Q1!'!B2"},
		{"'Sheet 2'!A1", "'Sheet 2'!A2"}},
	TestID -> "SpreadsheetParse-quoted-sheet-names"]

VerificationTest[
	SpreadsheetParse["SUM(Input!B2:Input!B4)"],
	{"Input!B2", "Input!B3", "Input!B4"},
	TestID -> "SpreadsheetParse-sheet-on-both-corners"]

(* without a workbook the sheets of a 3D reference are unknown: kept as written *)
VerificationTest[
	{SpreadsheetParse["SUM(Jan:Mar!B2)"], SpreadsheetParse["SUM('Jan:Mar'!B2)"]},
	{{"Jan:Mar!B2"}, {"'Jan:Mar'!B2"}},
	TestID -> "SpreadsheetParse-3D-reference-kept-as-written"]

(* without a workbook the rows of a sheet are unknown: whole columns of a sheet are kept as written *)
VerificationTest[
	{SpreadsheetParse["SUM(Data!A:B)"], SpreadsheetParse["VLOOKUP(A2,Data!$A:$C,3,FALSE)"]},
	{{"Data!A:B"}, {"A2", "Data!A:C"}},
	TestID -> "SpreadsheetParse-whole-columns-of-a-sheet-kept-as-written"]


(* ::Section:: *)
(*What is not a reference*)


VerificationTest[
	{SpreadsheetParse["IF(A1>0,\"B2\",C3)"], SpreadsheetParse["\"Total: A1\"&B2"]},
	{{"A1", "C3"}, {"B2"}},
	TestID -> "SpreadsheetParse-text-in-double-quotes"]

VerificationTest[
	SpreadsheetParse["LOG10(A1)+ATAN2(B1,C1)"],
	{"A1", "B1", "C1"},
	TestID -> "SpreadsheetParse-function-names-with-digits"]

(* a column has at most 3 letters, and a cell is a whole word *)
VerificationTest[
	{SpreadsheetParse["Tasso1*B2"], SpreadsheetParse["ABCD1+B2"]},
	{{"B2"}, {"B2"}},
	TestID -> "SpreadsheetParse-words-that-look-like-cells"]

VerificationTest[
	{SpreadsheetParse["IF(TRUE,B2,C2)"], SpreadsheetParse["B2*1.5E3+2"]},
	{{"B2", "C2"}, {"B2"}},
	TestID -> "SpreadsheetParse-booleans-and-numbers"]

VerificationTest[
	SpreadsheetParse["Rate1*B2+start"],
	{"B2"},
	TestID -> "SpreadsheetParse-defined-names-are-not-cells"]

VerificationTest[
	SpreadsheetParse["#REF!+B2"],
	{"B2"},
	TestID -> "SpreadsheetParse-error-values"]


(* ::Section:: *)
(*Ranges written alone*)


(* a range given by the contraction functions expands back into its cells *)
VerificationTest[
	SpreadsheetParse["'Technical Data'!B14:C25"],
	Flatten@Table["'Technical Data'!" <> c <> ToString[r], {r, 14, 25}, {c, {"B", "C"}}],
	TestID -> "SpreadsheetParse-range-written-alone"]


Clear[SpreadsheetToolkitTests`packageFile];
End[];
