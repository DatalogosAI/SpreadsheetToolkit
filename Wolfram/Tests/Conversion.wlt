(* ::Package:: *)

(* ::Title:: *)
(*SpreadsheetToolkit: tests of the conversion functions*)


(* ::Text:: *)
(*SpreadsheetConvert, SpreadsheetIndexToPosition and PositionToSpreadsheetIndex of Kernel/SpreadsheetToolkit.wl.*)
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


(* the 16384 column letters of a worksheet in their order, A to XFD: the letters, then the pairs, then the triples *)
columnLetters = With[{az = CharacterRange["A", "Z"]},
	Take[Join[az, StringJoin /@ Tuples[az, 2], StringJoin /@ Tuples[az, 3]], 16384]];


(* ::Section:: *)
(*SpreadsheetConvert*)


VerificationTest[
	{SpreadsheetConvert["J2"], SpreadsheetConvert[{2, 10}]},
	{{2, 10}, "J2"},
	TestID -> "SpreadsheetConvert-examples-of-ResourceDefinition"]

VerificationTest[
	SpreadsheetConvert[28],
	"AB",
	TestID -> "SpreadsheetConvert-column-number"]

VerificationTest[
	{SpreadsheetConvert[SpreadsheetConvert["XFD1048576"]], SpreadsheetConvert[SpreadsheetConvert[{5, 30}]]},
	{"XFD1048576", {5, 30}},
	TestID -> "SpreadsheetConvert-is-its-own-inverse"]


(* ::Section:: *)
(*SpreadsheetIndexToPosition*)


VerificationTest[
	SpreadsheetIndexToPosition /@ {"A1", "Z9", "AA10", "AZ3", "BA3", "ZZ1", "AAA1", "XFD1048576"},
	{{1, 1}, {9, 26}, {10, 27}, {3, 52}, {3, 53}, {1, 702}, {1, 703}, {1048576, 16384}},
	TestID -> "SpreadsheetIndexToPosition-cells"]

VerificationTest[
	SpreadsheetIndexToPosition /@ {"$B$7", "B$7", "$B7"},
	{{7, 2}, {7, 2}, {7, 2}},
	TestID -> "SpreadsheetIndexToPosition-absolute-references"]

VerificationTest[
	Table[SpreadsheetIndexToPosition[columnLetters[[c]] <> "7"], {c, 16384}],
	Table[{7, c}, {c, 16384}],
	TestID -> "SpreadsheetIndexToPosition-all-columns"]


(* ::Section:: *)
(*PositionToSpreadsheetIndex*)


VerificationTest[
	PositionToSpreadsheetIndex /@ {{1, 1}, {9, 26}, {10, 27}, {1, 702}, {1, 703}, {1048576, 16384}},
	{"A1", "Z9", "AA10", "ZZ1", "AAA1", "XFD1048576"},
	TestID -> "PositionToSpreadsheetIndex-positions"]

VerificationTest[
	PositionToSpreadsheetIndex /@ {1, 26, 27, 52, 53, 702, 703, 16384},
	{"A", "Z", "AA", "AZ", "BA", "ZZ", "AAA", "XFD"},
	TestID -> "PositionToSpreadsheetIndex-column-numbers"]

VerificationTest[
	Array[PositionToSpreadsheetIndex, 16384],
	columnLetters,
	TestID -> "PositionToSpreadsheetIndex-all-columns"]

(* positions with zero, negative or non-integer parts stay unevaluated *)
VerificationTest[
	Head /@ {PositionToSpreadsheetIndex[{0, 1}], PositionToSpreadsheetIndex[{1, 0}], PositionToSpreadsheetIndex[0],
		PositionToSpreadsheetIndex[-3], PositionToSpreadsheetIndex[{1.5, 2}], PositionToSpreadsheetIndex[{1, 2, 3}]},
	ConstantArray[PositionToSpreadsheetIndex, 6],
	TestID -> "PositionToSpreadsheetIndex-invalid-positions-unevaluated"]


(* ::Section:: *)
(*Round trip*)


VerificationTest[
	BlockRandom[
		SeedRandom[20261123];
		With[{positions = Transpose[{RandomInteger[{1, 1048576}, 1000], RandomInteger[{1, 16384}, 1000]}]},
			SpreadsheetIndexToPosition /@ (PositionToSpreadsheetIndex /@ positions) === positions]],
	True,
	TestID -> "round-trip-of-1000-random-positions"]


Clear[SpreadsheetToolkitTests`packageFile];
End[];
