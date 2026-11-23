(* ::Package:: *)

(* ::Title:: *)
(*SpreadsheetToolkit: tests of the contraction functions*)


(* ::Text:: *)
(*SpreadsheetContract, ContractSpreadsheetColumns, ContractSpreadsheetRows, ContractSpreadsheetColumnRanges and*)
(*ContractSpreadsheetRowRanges of Kernel/SpreadsheetToolkit.wl.*)
(*Run TestReport on this file, best in a fresh kernel. The package symbols are cleared and the package file is read*)
(*again with Get, so the tests see its current code and no definitions left over in the session. To test another*)
(*version of the file, see the Setup.*)
(*Outputs are compared as sets (Sort on both sides): the order of the ranges is not tested.*)
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


(* column letters -> number: "A" -> 1, "AA" -> 27, "XFD" -> 16384 *)
columnNumber[letters_String] := Fold[26 #1 + #2 &, 0, LetterNumber[Characters[letters]]]

(* address -> {row, column}, with or without $: "$B$7" -> {7, 2} *)
cellAddress[address_String] :=
	With[{a = StringDelete[address, "$"]},
		{FromDigits[StringDelete[a, CharacterRange["A", "Z"]]], columnNumber[StringDelete[a, CharacterRange["0", "9"]]]}]

(* label -> {sheet, address}, split at the last "!"; the sheet is "" when there is none *)
splitLabel[label_String] :=
	With[{p = StringPosition[label, "!"]},
		If[p === {}, {"", label}, {StringTake[label, p[[-1, 1]] - 1], StringDrop[label, p[[-1, 1]]]}]]

(* cell or range -> {sheet, {{first row, last row}, {first column, last column}}} *)
blockOf[label_String] :=
	With[{sa = splitLabel[label]},
		{sa[[1]], MinMax /@ Transpose[cellAddress /@ StringSplit[sa[[2]], ":"]]}]

(* cell or range -> its cells, each as {sheet, {row, column}} *)
cellsOf[label_String] :=
	With[{b = blockOf[label]},
		Flatten[Table[{b[[1]], {r, c}}, {r, b[[2, 1, 1]], b[[2, 1, 2]]}, {c, b[[2, 2, 1]], b[[2, 2, 2]]}], 1]]

(* the ranges cover exactly the given cells, each cell once *)
exactCoverQ[ranges_List, cells_List] :=
	With[{covered = Join @@ cellsOf /@ ranges},
		DuplicateFreeQ[covered] && Sort[covered] === Union @@ cellsOf /@ cells]

(* two blocks of one sheet that touch and together would make one rectangle *)
touchVerticallyQ[{s_, {{r1_, r2_}, cs_}}, {t_, {{q1_, q2_}, ds_}}] := s === t && cs === ds && (r2 + 1 == q1 || q2 + 1 == r1)
touchHorizontallyQ[{s_, {rs_, {c1_, c2_}}}, {t_, {qs_, {d1_, d2_}}}] := s === t && rs === qs && (c2 + 1 == d1 || d2 + 1 == c1)
noTouchingPairQ[touchQ_, ranges_List] := NoneTrue[Subsets[blockOf /@ ranges, {2}], touchQ @@ # &]

oneColumnQ[label_String] := Equal @@ blockOf[label][[2, 2]]
oneRowQ[label_String] := Equal @@ blockOf[label][[2, 1]]
isRange = StringContainsQ[":"];

(* the pipelines of the Scope example of ResourceDefinition.nb, with ResourceFunction["SelectDiscard"][StringContainsQ[#, ":"] &]
written as two Selects: Flatten@MapAt[ContractSpreadsheetColumns, SelectDiscard[...]@ContractSpreadsheetRows[cells], 2]
(rowsThenColumns), and the same with columns first *)
columnsThenRows[cells_List] :=
	With[{runs = ContractSpreadsheetColumns[cells]},
		Join[Select[runs, isRange], ContractSpreadsheetRows[Select[runs, Not@*isRange]]]]
rowsThenColumns[cells_List] :=
	With[{runs = ContractSpreadsheetRows[cells]},
		Join[Select[runs, isRange], ContractSpreadsheetColumns[Select[runs, Not@*isRange]]]]


(* ::Section:: *)
(*ContractSpreadsheetColumns*)


VerificationTest[
	Sort@ContractSpreadsheetColumns[{"Data!B2", "Data!B3", "Data!B4"}],
	Sort@{"Data!B2:B4"},
	TestID -> "ContractSpreadsheetColumns-consecutive-rows-make-one-range"]

VerificationTest[
	Sort@ContractSpreadsheetColumns[{"Data!B2", "Data!B3", "Data!B5", "Data!B7", "Data!B8", "Data!B9"}],
	Sort@{"Data!B2:B3", "Data!B5", "Data!B7:B9"},
	TestID -> "ContractSpreadsheetColumns-gaps-split-the-column"]

VerificationTest[
	Sort@ContractSpreadsheetColumns[{"Data!C9", "Data!C7", "Data!C8", "Data!C7"}],
	Sort@{"Data!C7:C9"},
	TestID -> "ContractSpreadsheetColumns-unsorted-and-repeated-cells"]

VerificationTest[
	Sort@ContractSpreadsheetColumns[{"Data!B2", "Data!C2", "Data!B3", "Data!C3"}],
	Sort@{"Data!B2:B3", "Data!C2:C3"},
	TestID -> "ContractSpreadsheetColumns-neighbouring-columns-stay-apart"]

VerificationTest[
	Sort@ContractSpreadsheetColumns[{"Data!B2", "Data!C2", "Data!D2"}],
	Sort@{"Data!B2", "Data!C2", "Data!D2"},
	TestID -> "ContractSpreadsheetColumns-cells-in-a-row-stay-single"]

VerificationTest[
	Sort@ContractSpreadsheetColumns[{"Jan!B2", "Jan!B3", "Feb!B4", "Feb!B5"}],
	Sort@{"Jan!B2:B3", "Feb!B4:B5"},
	TestID -> "ContractSpreadsheetColumns-sheets-stay-apart"]

VerificationTest[
	Sort@ContractSpreadsheetColumns[{"'6 - Ratios'!C73", "'6 - Ratios'!C74", "'O''Brien'!B2", "'O''Brien'!B3", "'2024'!B2",
		"'Extra \"Q1\" (year''s end)'!B3", "'Extra \"Q1\" (year''s end)'!B4"}],
	Sort@{"'6 - Ratios'!C73:C74", "'O''Brien'!B2:B3", "'2024'!B2", "'Extra \"Q1\" (year''s end)'!B3:B4"},
	TestID -> "ContractSpreadsheetColumns-quoted-sheet-names"]

VerificationTest[
	Sort@ContractSpreadsheetColumns[{"Data!Z5", "Data!Z6", "Data!AA5", "Data!AA6", "Data!AF10", "Data!AF11", "Data!AF12",
		"Data!XFD1", "Data!XFD2"}],
	Sort@{"Data!Z5:Z6", "Data!AA5:AA6", "Data!AF10:AF12", "Data!XFD1:XFD2"},
	TestID -> "ContractSpreadsheetColumns-columns-after-Z"]

VerificationTest[
	Sort@ContractSpreadsheetColumns[{"Data!B99", "Data!B100", "Data!B101", "Data!A1048575", "Data!A1048576"}],
	Sort@{"Data!B99:B101", "Data!A1048575:A1048576"},
	TestID -> "ContractSpreadsheetColumns-long-row-numbers"]

VerificationTest[
	Sort@ContractSpreadsheetColumns[{"Data!$B$2", "Data!B2", "Data!B$3", "Data!$B4"}],
	Sort@{"Data!B2:B4"},
	TestID -> "ContractSpreadsheetColumns-absolute-references"]

VerificationTest[
	ContractSpreadsheetColumns[{}],
	{},
	TestID -> "ContractSpreadsheetColumns-no-cells"]


(* ::Section:: *)
(*ContractSpreadsheetRows*)


VerificationTest[
	Sort@ContractSpreadsheetRows[{"Data!B2", "Data!C2", "Data!D2"}],
	Sort@{"Data!B2:D2"},
	TestID -> "ContractSpreadsheetRows-consecutive-columns-make-one-range"]

VerificationTest[
	Sort@ContractSpreadsheetRows[{"Data!B2", "Data!C2", "Data!E2", "Data!G2", "Data!H2"}],
	Sort@{"Data!B2:C2", "Data!E2", "Data!G2:H2"},
	TestID -> "ContractSpreadsheetRows-gaps-split-the-row"]

VerificationTest[
	Sort@ContractSpreadsheetRows[{"Data!D7", "Data!B7", "Data!C7", "Data!B7"}],
	Sort@{"Data!B7:D7"},
	TestID -> "ContractSpreadsheetRows-unsorted-and-repeated-cells"]

VerificationTest[
	Sort@ContractSpreadsheetRows[{"Data!B2", "Data!C2", "Data!B3", "Data!C3"}],
	Sort@{"Data!B2:C2", "Data!B3:C3"},
	TestID -> "ContractSpreadsheetRows-neighbouring-rows-stay-apart"]

VerificationTest[
	Sort@ContractSpreadsheetRows[{"Data!B2", "Data!B3", "Data!B4"}],
	Sort@{"Data!B2", "Data!B3", "Data!B4"},
	TestID -> "ContractSpreadsheetRows-cells-in-a-column-stay-single"]

VerificationTest[
	Sort@ContractSpreadsheetRows[{"Jan!B2", "Jan!C2", "Feb!D2", "Feb!E2"}],
	Sort@{"Jan!B2:C2", "Feb!D2:E2"},
	TestID -> "ContractSpreadsheetRows-sheets-stay-apart"]

VerificationTest[
	Sort@ContractSpreadsheetRows[{"'6 - Ratios'!C73", "'6 - Ratios'!D73", "'O''Brien'!B2", "'O''Brien'!C2", "'2024'!B2",
		"'2024'!C2", "'Extra \"Q1\" (year''s end)'!B3", "'Extra \"Q1\" (year''s end)'!C3"}],
	Sort@{"'6 - Ratios'!C73:D73", "'O''Brien'!B2:C2", "'2024'!B2:C2", "'Extra \"Q1\" (year''s end)'!B3:C3"},
	TestID -> "ContractSpreadsheetRows-quoted-sheet-names"]

VerificationTest[
	Sort@ContractSpreadsheetRows[{"Data!Y5", "Data!Z5", "Data!AA5", "Data!AB5", "Data!XFC9", "Data!XFD9"}],
	Sort@{"Data!Y5:AB5", "Data!XFC9:XFD9"},
	TestID -> "ContractSpreadsheetRows-runs-after-Z"]

(* the 30 year columns of example 11, C to AF *)
VerificationTest[
	Sort@ContractSpreadsheetRows[("Data!" <> # <> "6") & /@ Join[CharacterRange["C", "Z"], ("A" <> #) & /@ CharacterRange["A", "F"]]],
	Sort@{"Data!C6:AF6"},
	TestID -> "ContractSpreadsheetRows-thirty-year-columns"]

VerificationTest[
	Sort@ContractSpreadsheetRows[{"Data!$B$2", "Data!B2", "Data!C$2", "Data!$D2"}],
	Sort@{"Data!B2:D2"},
	TestID -> "ContractSpreadsheetRows-absolute-references"]

VerificationTest[
	ContractSpreadsheetRows[{}],
	{},
	TestID -> "ContractSpreadsheetRows-no-cells"]


(* ::Section:: *)
(*ContractSpreadsheetColumnRanges*)


VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[{"Data!B2:B4", "Data!C2:C4", "Data!D2:D4"}],
	Sort@{"Data!B2:D4"},
	TestID -> "ContractSpreadsheetColumnRanges-same-rows-in-neighbouring-columns-merge"]

VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[{"Data!E2:E4", "Data!C2:C4", "Data!D2:D4"}],
	Sort@{"Data!C2:E4"},
	TestID -> "ContractSpreadsheetColumnRanges-unsorted-columns"]

VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[{"Data!B2:B4", "Data!C2:C4", "Data!F2:F4"}],
	Sort@{"Data!B2:C4", "Data!F2:F4"},
	TestID -> "ContractSpreadsheetColumnRanges-a-run-and-a-range-apart"]

VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[{"Data!B2:B4", "Data!C2:C4", "Data!F2:F4", "Data!G2:G4"}],
	Sort@{"Data!B2:C4", "Data!F2:G4"},
	TestID -> "ContractSpreadsheetColumnRanges-two-runs-with-the-same-rows"]

VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[{"Data!B2:B4", "Data!C3:C5", "Data!D2:D5"}],
	Sort@{"Data!B2:B4", "Data!C3:C5", "Data!D2:D5"},
	TestID -> "ContractSpreadsheetColumnRanges-different-rows-stay-apart"]

(* merging is across columns only: row ranges in consecutive rows stay as they are *)
VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[{"Data!B2:D2", "Data!B3:D3"}],
	Sort@{"Data!B2:D2", "Data!B3:D3"},
	TestID -> "ContractSpreadsheetColumnRanges-row-ranges-in-consecutive-rows-stay-apart"]

VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[{"Data!B2:B4", "Data!C2:C4", "Data!E7", "Data!G9"}],
	Sort@{"Data!B2:C4", "Data!E7", "Data!G9"},
	TestID -> "ContractSpreadsheetColumnRanges-single-cells-pass-through"]

VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[{"Jan!B2:B4", "Feb!C2:C4", "Jan!C2:C4"}],
	Sort@{"Jan!B2:C4", "Feb!C2:C4"},
	TestID -> "ContractSpreadsheetColumnRanges-sheets-stay-apart"]

VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[{"'6 - Ratios'!C70:C73", "'6 - Ratios'!D70:D73", "'O''Brien'!B2:B3", "'O''Brien'!C2:C3"}],
	Sort@{"'6 - Ratios'!C70:D73", "'O''Brien'!B2:C3"},
	TestID -> "ContractSpreadsheetColumnRanges-quoted-sheet-names"]

VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[{"Data!AA5:AA9", "Data!Z5:Z9", "Data!AB5:AB9", "Data!XFD1:XFD3", "Data!XFC1:XFC3"}],
	Sort@{"Data!Z5:AB9", "Data!XFC1:XFD3"},
	TestID -> "ContractSpreadsheetColumnRanges-columns-after-Z"]

VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[{"Data!$B$2:$B$4", "Data!C2:C4"}],
	Sort@{"Data!B2:C4"},
	TestID -> "ContractSpreadsheetColumnRanges-absolute-references"]

VerificationTest[
	ContractSpreadsheetColumnRanges[{}],
	{},
	TestID -> "ContractSpreadsheetColumnRanges-no-ranges"]


(* ::Section:: *)
(*ContractSpreadsheetRowRanges*)


VerificationTest[
	Sort@ContractSpreadsheetRowRanges[{"Data!B2:D2", "Data!B3:D3", "Data!B4:D4"}],
	Sort@{"Data!B2:D4"},
	TestID -> "ContractSpreadsheetRowRanges-same-columns-in-consecutive-rows-merge"]

VerificationTest[
	Sort@ContractSpreadsheetRowRanges[{"Data!B10:D10", "Data!B9:D9", "Data!B11:D11"}],
	Sort@{"Data!B9:D11"},
	TestID -> "ContractSpreadsheetRowRanges-unsorted-rows"]

VerificationTest[
	Sort@ContractSpreadsheetRowRanges[{"Data!B2:D2", "Data!B3:D3", "Data!B6:D6"}],
	Sort@{"Data!B2:D3", "Data!B6:D6"},
	TestID -> "ContractSpreadsheetRowRanges-a-run-and-a-range-apart"]

VerificationTest[
	Sort@ContractSpreadsheetRowRanges[{"Data!B2:D2", "Data!B3:D3", "Data!B6:D6", "Data!B7:D7"}],
	Sort@{"Data!B2:D3", "Data!B6:D7"},
	TestID -> "ContractSpreadsheetRowRanges-two-runs-with-the-same-columns"]

VerificationTest[
	Sort@ContractSpreadsheetRowRanges[{"Data!B2:D2", "Data!C3:E3", "Data!B4:E4"}],
	Sort@{"Data!B2:D2", "Data!C3:E3", "Data!B4:E4"},
	TestID -> "ContractSpreadsheetRowRanges-different-columns-stay-apart"]

(* merging is across rows only: column ranges in neighbouring columns stay as they are *)
VerificationTest[
	Sort@ContractSpreadsheetRowRanges[{"Data!B2:B4", "Data!C2:C4"}],
	Sort@{"Data!B2:B4", "Data!C2:C4"},
	TestID -> "ContractSpreadsheetRowRanges-column-ranges-in-neighbouring-columns-stay-apart"]

VerificationTest[
	Sort@ContractSpreadsheetRowRanges[{"Data!B2:D2", "Data!B3:D3", "Data!F7", "Data!H9"}],
	Sort@{"Data!B2:D3", "Data!F7", "Data!H9"},
	TestID -> "ContractSpreadsheetRowRanges-single-cells-pass-through"]

VerificationTest[
	Sort@ContractSpreadsheetRowRanges[{"Jan!B2:D2", "Feb!B3:D3", "Jan!B3:D3"}],
	Sort@{"Jan!B2:D3", "Feb!B3:D3"},
	TestID -> "ContractSpreadsheetRowRanges-sheets-stay-apart"]

VerificationTest[
	Sort@ContractSpreadsheetRowRanges[{"'6 - Ratios'!C70:F70", "'6 - Ratios'!C71:F71", "'O''Brien'!B2:C2", "'O''Brien'!B3:C3"}],
	Sort@{"'6 - Ratios'!C70:F71", "'O''Brien'!B2:C3"},
	TestID -> "ContractSpreadsheetRowRanges-quoted-sheet-names"]

VerificationTest[
	Sort@ContractSpreadsheetRowRanges[{"Data!Y5:AB5", "Data!Y6:AB6", "Data!XFC9:XFD9", "Data!XFC10:XFD10"}],
	Sort@{"Data!Y5:AB6", "Data!XFC9:XFD10"},
	TestID -> "ContractSpreadsheetRowRanges-columns-after-Z"]

VerificationTest[
	Sort@ContractSpreadsheetRowRanges[{"Data!$B$2:$D$2", "Data!B3:D3"}],
	Sort@{"Data!B2:D3"},
	TestID -> "ContractSpreadsheetRowRanges-absolute-references"]

VerificationTest[
	ContractSpreadsheetRowRanges[{}],
	{},
	TestID -> "ContractSpreadsheetRowRanges-no-ranges"]


(* ::Section:: *)
(*Example 11: the inputs of '6 - Ratios'!C73*)


(* ::Text:: *)
(*The 321 cells with a numeric value in SpreadsheetTrace[file11, "6 - Ratios!C73", "TraceDuplicates" -> False], with*)
(*file11 the example 11 workbook (finalcells in the Scope example of ResourceDefinition.nb), in the order of the trace.*)
(*Only cell addresses: no defined names.*)


c73Cells = {
	"'C - Taxes'!C27", "'C - Taxes'!C28", "'C - Taxes'!C38", "'A - Working Capital'!C9", "'Administrative Data'!B72",
	"'Administrative Data'!B73", "'A - Working Capital'!C21", "'Administrative Data'!B74", "'Administrative Data'!B76",
	"'Administrative Data'!B77", "'Administrative Data'!B78", "'Technical Data'!B38", "'Prices and Occupancy (2025)'!C5",
	"'Prices and Occupancy (2025)'!B15", "'Prices and Occupancy (2025)'!E5", "'Prices and Occupancy (2025)'!D5",
	"'Monthly Revenue (1st year, \[Euro])'!B61", "'Prices and Occupancy (2025)'!C15",
	"'Monthly Revenue (1st year, \[Euro])'!C61", "'Prices and Occupancy (2025)'!D15",
	"'Monthly Revenue (1st year, \[Euro])'!D61", "'Prices and Occupancy (2025)'!E15",
	"'Monthly Revenue (1st year, \[Euro])'!E61", "'Prices and Occupancy (2025)'!F15",
	"'Monthly Revenue (1st year, \[Euro])'!F61", "'Prices and Occupancy (2025)'!G15",
	"'Monthly Revenue (1st year, \[Euro])'!G61", "'Prices and Occupancy (2025)'!H15",
	"'Monthly Revenue (1st year, \[Euro])'!H61", "'Prices and Occupancy (2025)'!I15",
	"'Monthly Revenue (1st year, \[Euro])'!I61", "'Prices and Occupancy (2025)'!J15",
	"'Monthly Revenue (1st year, \[Euro])'!J61", "'Prices and Occupancy (2025)'!K15",
	"'Monthly Revenue (1st year, \[Euro])'!K61", "'Prices and Occupancy (2025)'!L15",
	"'Monthly Revenue (1st year, \[Euro])'!L61", "'Prices and Occupancy (2025)'!M15",
	"'Monthly Revenue (1st year, \[Euro])'!M61", "'Prices and Occupancy (2025)'!C6", "'Prices and Occupancy (2025)'!B16",
	"'Prices and Occupancy (2025)'!E6", "'Prices and Occupancy (2025)'!D6", "'Prices and Occupancy (2025)'!C16",
	"'Prices and Occupancy (2025)'!D16", "'Prices and Occupancy (2025)'!E16", "'Prices and Occupancy (2025)'!F16",
	"'Prices and Occupancy (2025)'!G16", "'Prices and Occupancy (2025)'!H16", "'Prices and Occupancy (2025)'!I16",
	"'Prices and Occupancy (2025)'!J16", "'Prices and Occupancy (2025)'!K16", "'Prices and Occupancy (2025)'!L16",
	"'Prices and Occupancy (2025)'!M16", "'Prices and Occupancy (2025)'!C7", "'Prices and Occupancy (2025)'!B17",
	"'Prices and Occupancy (2025)'!E7", "'Prices and Occupancy (2025)'!D7", "'Prices and Occupancy (2025)'!C17",
	"'Prices and Occupancy (2025)'!D17", "'Prices and Occupancy (2025)'!E17", "'Prices and Occupancy (2025)'!F17",
	"'Prices and Occupancy (2025)'!G17", "'Prices and Occupancy (2025)'!H17", "'Prices and Occupancy (2025)'!I17",
	"'Prices and Occupancy (2025)'!J17", "'Prices and Occupancy (2025)'!K17", "'Prices and Occupancy (2025)'!L17",
	"'Prices and Occupancy (2025)'!M17", "'Prices and Occupancy (2025)'!C8", "'Prices and Occupancy (2025)'!B18",
	"'Prices and Occupancy (2025)'!E8", "'Prices and Occupancy (2025)'!D8", "'Prices and Occupancy (2025)'!C18",
	"'Prices and Occupancy (2025)'!D18", "'Prices and Occupancy (2025)'!E18", "'Prices and Occupancy (2025)'!F18",
	"'Prices and Occupancy (2025)'!G18", "'Prices and Occupancy (2025)'!H18", "'Prices and Occupancy (2025)'!I18",
	"'Prices and Occupancy (2025)'!J18", "'Prices and Occupancy (2025)'!K18", "'Prices and Occupancy (2025)'!L18",
	"'Prices and Occupancy (2025)'!M18", "'Prices and Occupancy (2025)'!C9", "'Prices and Occupancy (2025)'!B19",
	"'Prices and Occupancy (2025)'!E9", "'Prices and Occupancy (2025)'!D9", "'Prices and Occupancy (2025)'!C19",
	"'Prices and Occupancy (2025)'!D19", "'Prices and Occupancy (2025)'!E19", "'Prices and Occupancy (2025)'!F19",
	"'Prices and Occupancy (2025)'!G19", "'Prices and Occupancy (2025)'!H19", "'Prices and Occupancy (2025)'!I19",
	"'Prices and Occupancy (2025)'!J19", "'Prices and Occupancy (2025)'!K19", "'Prices and Occupancy (2025)'!L19",
	"'Prices and Occupancy (2025)'!M19", "'Prices and Occupancy (2025)'!C10", "'Prices and Occupancy (2025)'!B20",
	"'Prices and Occupancy (2025)'!E10", "'Prices and Occupancy (2025)'!D10", "'Prices and Occupancy (2025)'!C20",
	"'Prices and Occupancy (2025)'!D20", "'Prices and Occupancy (2025)'!E20", "'Prices and Occupancy (2025)'!F20",
	"'Prices and Occupancy (2025)'!G20", "'Prices and Occupancy (2025)'!H20", "'Prices and Occupancy (2025)'!I20",
	"'Prices and Occupancy (2025)'!J20", "'Prices and Occupancy (2025)'!K20", "'Prices and Occupancy (2025)'!L20",
	"'Prices and Occupancy (2025)'!M20", "'Prices and Occupancy (2025)'!C11", "'Prices and Occupancy (2025)'!B21",
	"'Prices and Occupancy (2025)'!E11", "'Prices and Occupancy (2025)'!D11", "'Prices and Occupancy (2025)'!C21",
	"'Prices and Occupancy (2025)'!D21", "'Prices and Occupancy (2025)'!E21", "'Prices and Occupancy (2025)'!F21",
	"'Prices and Occupancy (2025)'!G21", "'Prices and Occupancy (2025)'!H21", "'Prices and Occupancy (2025)'!I21",
	"'Prices and Occupancy (2025)'!J21", "'Prices and Occupancy (2025)'!K21", "'Prices and Occupancy (2025)'!L21",
	"'Prices and Occupancy (2025)'!M21", "'Prices and Occupancy (2025)'!C25", "'Prices and Occupancy (2025)'!E25",
	"'Prices and Occupancy (2025)'!D25", "'Prices and Occupancy (2025)'!C26", "'Prices and Occupancy (2025)'!E26",
	"'Prices and Occupancy (2025)'!D26", "'Prices and Occupancy (2025)'!C27", "'Prices and Occupancy (2025)'!E27",
	"'Prices and Occupancy (2025)'!D27", "'Prices and Occupancy (2025)'!C28", "'Prices and Occupancy (2025)'!E28",
	"'Prices and Occupancy (2025)'!D28", "'Prices and Occupancy (2025)'!C31", "'Prices and Occupancy (2025)'!E31",
	"'Prices and Occupancy (2025)'!D31", "'Prices and Occupancy (2025)'!C32", "'Prices and Occupancy (2025)'!E32",
	"'Prices and Occupancy (2025)'!D32", "'Prices and Occupancy (2025)'!C33", "'Prices and Occupancy (2025)'!E33",
	"'Prices and Occupancy (2025)'!D33", "'Prices and Occupancy (2025)'!C34", "'Prices and Occupancy (2025)'!E34",
	"'Prices and Occupancy (2025)'!D34", "'Prices and Occupancy (2025)'!C35", "'Prices and Occupancy (2025)'!E35",
	"'Prices and Occupancy (2025)'!D35", "'Prices and Occupancy (2025)'!C36", "'Prices and Occupancy (2025)'!E36",
	"'Prices and Occupancy (2025)'!D36", "'Prices and Occupancy (2025)'!C37", "'Prices and Occupancy (2025)'!E37",
	"'Prices and Occupancy (2025)'!D37", "'Prices and Occupancy (2025)'!C38", "'Prices and Occupancy (2025)'!E38",
	"'Prices and Occupancy (2025)'!D38", "'Prices and Occupancy (2025)'!C41", "'Prices and Occupancy (2025)'!E41",
	"'Prices and Occupancy (2025)'!D41", "'Staff Costs (1st year)'!C50", "'Staff Costs (1st year)'!C51",
	"'Staff Costs (1st year)'!B7", "'Staff Costs (1st year)'!C5", "'Staff Costs (1st year)'!C6",
	"'Staff Costs (1st year)'!C7", "'Staff Costs (1st year)'!D7", "'Staff Costs (1st year)'!E7",
	"'Staff Costs (1st year)'!F7", "'Staff Costs (1st year)'!G7", "'Staff Costs (1st year)'!H7",
	"'Staff Costs (1st year)'!I7", "'Staff Costs (1st year)'!J7", "'Staff Costs (1st year)'!K7",
	"'Staff Costs (1st year)'!L7", "'Staff Costs (1st year)'!M7", "'Staff Costs (1st year)'!B13",
	"'Staff Costs (1st year)'!C11", "'Staff Costs (1st year)'!C12", "'Staff Costs (1st year)'!C13",
	"'Staff Costs (1st year)'!D13", "'Staff Costs (1st year)'!E13", "'Staff Costs (1st year)'!F13",
	"'Staff Costs (1st year)'!G13", "'Staff Costs (1st year)'!H13", "'Staff Costs (1st year)'!I13",
	"'Staff Costs (1st year)'!J13", "'Staff Costs (1st year)'!K13", "'Staff Costs (1st year)'!L13",
	"'Staff Costs (1st year)'!M13", "'Staff Costs (1st year)'!B19", "'Staff Costs (1st year)'!C17",
	"'Staff Costs (1st year)'!C18", "'Staff Costs (1st year)'!C19", "'Staff Costs (1st year)'!D19",
	"'Staff Costs (1st year)'!E19", "'Staff Costs (1st year)'!F19", "'Staff Costs (1st year)'!G19",
	"'Staff Costs (1st year)'!H19", "'Staff Costs (1st year)'!I19", "'Staff Costs (1st year)'!J19",
	"'Staff Costs (1st year)'!K19", "'Staff Costs (1st year)'!L19", "'Staff Costs (1st year)'!M19",
	"'Staff Costs (1st year)'!B25", "'Staff Costs (1st year)'!C23", "'Staff Costs (1st year)'!C24",
	"'Staff Costs (1st year)'!C25", "'Staff Costs (1st year)'!D25", "'Staff Costs (1st year)'!E25",
	"'Staff Costs (1st year)'!F25", "'Staff Costs (1st year)'!G25", "'Staff Costs (1st year)'!H25",
	"'Staff Costs (1st year)'!I25", "'Staff Costs (1st year)'!J25", "'Staff Costs (1st year)'!K25",
	"'Staff Costs (1st year)'!L25", "'Staff Costs (1st year)'!M25", "'Staff Costs (1st year)'!B31",
	"'Staff Costs (1st year)'!C29", "'Staff Costs (1st year)'!C30", "'Staff Costs (1st year)'!C31",
	"'Staff Costs (1st year)'!D31", "'Staff Costs (1st year)'!E31", "'Staff Costs (1st year)'!F31",
	"'Staff Costs (1st year)'!G31", "'Staff Costs (1st year)'!H31", "'Staff Costs (1st year)'!I31",
	"'Staff Costs (1st year)'!J31", "'Staff Costs (1st year)'!K31", "'Staff Costs (1st year)'!L31",
	"'Staff Costs (1st year)'!M31", "'Staff Costs (1st year)'!B37", "'Staff Costs (1st year)'!C35",
	"'Staff Costs (1st year)'!C36", "'Staff Costs (1st year)'!C37", "'Staff Costs (1st year)'!D37",
	"'Staff Costs (1st year)'!E37", "'Staff Costs (1st year)'!F37", "'Staff Costs (1st year)'!G37",
	"'Staff Costs (1st year)'!H37", "'Staff Costs (1st year)'!I37", "'Staff Costs (1st year)'!J37",
	"'Staff Costs (1st year)'!K37", "'Staff Costs (1st year)'!L37", "'Staff Costs (1st year)'!M37",
	"'Monthly Costs (1st year,\[Euro])'!F11", "'Monthly Costs (1st year,\[Euro])'!G11",
	"'Monthly Costs (1st year,\[Euro])'!H11", "'Monthly Costs (1st year,\[Euro])'!I11",
	"'Monthly Costs (1st year,\[Euro])'!J11", "'Monthly Costs (1st year,\[Euro])'!F9",
	"'Monthly Costs (1st year,\[Euro])'!G9", "'Monthly Costs (1st year,\[Euro])'!H9",
	"'Monthly Costs (1st year,\[Euro])'!I9", "'Monthly Costs (1st year,\[Euro])'!J9",
	"'Monthly Costs (1st year,\[Euro])'!B10", "'Monthly Costs (1st year,\[Euro])'!B15",
	"'Monthly Costs (1st year,\[Euro])'!F16", "'Monthly Costs (1st year,\[Euro])'!G16",
	"'Monthly Costs (1st year,\[Euro])'!H16", "'Monthly Costs (1st year,\[Euro])'!I16",
	"'Monthly Costs (1st year,\[Euro])'!J16", "'Monthly Costs (1st year,\[Euro])'!B19",
	"'Monthly Costs (1st year,\[Euro])'!E20", "'Monthly Costs (1st year,\[Euro])'!K20", "'input Leasing'!I20",
	"'input Leasing'!J20", "'input Leasing'!I21", "'input Leasing'!J21", "'input Leasing'!I22", "'input Leasing'!J22",
	"'Administrative Data'!B1", "'Administrative Data'!B101", "'Administrative Data'!B75", "'Technical Data'!B14",
	"'Technical Data'!F14", "'Technical Data'!B15", "'Technical Data'!F15", "'Technical Data'!B16",
	"'Technical Data'!F16", "'Technical Data'!B17", "'Technical Data'!F17", "'Technical Data'!B18",
	"'Technical Data'!F18", "'Technical Data'!B19", "'Technical Data'!F19", "'Technical Data'!B20",
	"'Technical Data'!F20", "'Technical Data'!B21", "'Technical Data'!F21", "'Technical Data'!B22",
	"'Technical Data'!F22", "'Technical Data'!B23", "'Technical Data'!F23", "'Technical Data'!B24",
	"'Technical Data'!F24", "'Technical Data'!B25", "'Technical Data'!F25", "'Technical Data'!C34",
	"'Technical Data'!C14", "'Technical Data'!C15", "'Technical Data'!C16", "'Technical Data'!C17",
	"'Technical Data'!C18", "'Technical Data'!C19", "'Technical Data'!C20", "'Technical Data'!C21",
	"'Technical Data'!C22", "'Technical Data'!C23", "'Technical Data'!C24", "'Technical Data'!C25",
	"'Technical Data'!B36"};


(* ::Text:: *)
(*ContractSpreadsheetColumnRanges after columnsThenRows, and ContractSpreadsheetRowRanges after rowsThenColumns, on c73Cells,*)
(*both checked against a separate implementation.*)


c73RangesByColumns = {
	"'Administrative Data'!B1", "'Administrative Data'!B72:B78", "'Administrative Data'!B101",
	"'A - Working Capital'!C9", "'A - Working Capital'!C21", "'C - Taxes'!C27:C28", "'C - Taxes'!C38",
	"'input Leasing'!I20:J22", "'Monthly Costs (1st year,\[Euro])'!F9:J9", "'Monthly Costs (1st year,\[Euro])'!B10",
	"'Monthly Costs (1st year,\[Euro])'!F11:J11", "'Monthly Costs (1st year,\[Euro])'!B15",
	"'Monthly Costs (1st year,\[Euro])'!F16:J16", "'Monthly Costs (1st year,\[Euro])'!B19",
	"'Monthly Costs (1st year,\[Euro])'!E20", "'Monthly Costs (1st year,\[Euro])'!K20",
	"'Monthly Revenue (1st year, \[Euro])'!B61:M61", "'Prices and Occupancy (2025)'!C5:E11",
	"'Prices and Occupancy (2025)'!B15:M21", "'Prices and Occupancy (2025)'!C25:E28",
	"'Prices and Occupancy (2025)'!C31:E38", "'Prices and Occupancy (2025)'!C41:E41", "'Staff Costs (1st year)'!C5:C7",
	"'Staff Costs (1st year)'!B7", "'Staff Costs (1st year)'!D7:M7", "'Staff Costs (1st year)'!C11:C13",
	"'Staff Costs (1st year)'!B13", "'Staff Costs (1st year)'!D13:M13", "'Staff Costs (1st year)'!C17:C19",
	"'Staff Costs (1st year)'!B19", "'Staff Costs (1st year)'!D19:M19", "'Staff Costs (1st year)'!C23:C25",
	"'Staff Costs (1st year)'!B25", "'Staff Costs (1st year)'!D25:M25", "'Staff Costs (1st year)'!C29:C31",
	"'Staff Costs (1st year)'!B31", "'Staff Costs (1st year)'!D31:M31", "'Staff Costs (1st year)'!C35:C37",
	"'Staff Costs (1st year)'!B37", "'Staff Costs (1st year)'!D37:M37", "'Staff Costs (1st year)'!C50:C51",
	"'Technical Data'!B14:C25", "'Technical Data'!F14:F25", "'Technical Data'!C34", "'Technical Data'!B36",
	"'Technical Data'!B38"};


c73RangesByRows = {
	"'Administrative Data'!B1", "'Administrative Data'!B72:B78", "'Administrative Data'!B101",
	"'A - Working Capital'!C9", "'A - Working Capital'!C21", "'C - Taxes'!C27:C28", "'C - Taxes'!C38",
	"'input Leasing'!I20:J22", "'Monthly Costs (1st year,\[Euro])'!F9:J9", "'Monthly Costs (1st year,\[Euro])'!B10",
	"'Monthly Costs (1st year,\[Euro])'!F11:J11", "'Monthly Costs (1st year,\[Euro])'!B15",
	"'Monthly Costs (1st year,\[Euro])'!F16:J16", "'Monthly Costs (1st year,\[Euro])'!B19",
	"'Monthly Costs (1st year,\[Euro])'!E20", "'Monthly Costs (1st year,\[Euro])'!K20",
	"'Monthly Revenue (1st year, \[Euro])'!B61:M61", "'Prices and Occupancy (2025)'!C5:E11",
	"'Prices and Occupancy (2025)'!B15:M21", "'Prices and Occupancy (2025)'!C25:E28",
	"'Prices and Occupancy (2025)'!C31:E38", "'Prices and Occupancy (2025)'!C41:E41", "'Staff Costs (1st year)'!C5:C6",
	"'Staff Costs (1st year)'!B7:M7", "'Staff Costs (1st year)'!C11:C12", "'Staff Costs (1st year)'!B13:M13",
	"'Staff Costs (1st year)'!C17:C18", "'Staff Costs (1st year)'!B19:M19", "'Staff Costs (1st year)'!C23:C24",
	"'Staff Costs (1st year)'!B25:M25", "'Staff Costs (1st year)'!C29:C30", "'Staff Costs (1st year)'!B31:M31",
	"'Staff Costs (1st year)'!C35:C36", "'Staff Costs (1st year)'!B37:M37", "'Staff Costs (1st year)'!C50:C51",
	"'Technical Data'!B14:C25", "'Technical Data'!F14:F25", "'Technical Data'!C34", "'Technical Data'!B36",
	"'Technical Data'!B38"};


VerificationTest[
	{Length[c73Cells], Length[Union @@ cellsOf /@ c73Cells]},
	{321, 321},
	TestID -> "ex11-C73-321-distinct-cells"]

(* maximal runs in each column: 35 ranges and 109 single cells *)
VerificationTest[
	With[{out = ContractSpreadsheetColumns[c73Cells]},
		{exactCoverQ[out, c73Cells], AllTrue[out, oneColumnQ], noTouchingPairQ[touchVerticallyQ, out],
			Count[out, _?isRange], Count[out, _?(Not@*isRange)]}],
	{True, True, True, 35, 109},
	TestID -> "ex11-C73-ContractSpreadsheetColumns"]

(* maximal runs in each row: 52 ranges and 48 single cells *)
VerificationTest[
	With[{out = ContractSpreadsheetRows[c73Cells]},
		{exactCoverQ[out, c73Cells], AllTrue[out, oneRowQ], noTouchingPairQ[touchHorizontallyQ, out],
			Count[out, _?isRange], Count[out, _?(Not@*isRange)]}],
	{True, True, True, 52, 48},
	TestID -> "ex11-C73-ContractSpreadsheetRows"]

(* columns, then the cells left single by rows: 46 ranges and 19 single cells *)
VerificationTest[
	With[{out = columnsThenRows[c73Cells]},
		{exactCoverQ[out, c73Cells], Count[out, _?isRange], Count[out, _?(Not@*isRange)]}],
	{True, 46, 19},
	TestID -> "ex11-C73-columns-then-rows"]

(* rows, then the cells left single by columns: 62 ranges and 13 single cells *)
VerificationTest[
	With[{out = rowsThenColumns[c73Cells]},
		{exactCoverQ[out, c73Cells], Count[out, _?isRange], Count[out, _?(Not@*isRange)]}],
	{True, 62, 13},
	TestID -> "ex11-C73-rows-then-columns"]

VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[columnsThenRows[c73Cells]],
	Sort@c73RangesByColumns,
	TestID -> "ex11-C73-ContractSpreadsheetColumnRanges"]

(* 27 ranges and 19 single cells, none of them could be joined across columns *)
VerificationTest[
	With[{out = ContractSpreadsheetColumnRanges[columnsThenRows[c73Cells]]},
		{exactCoverQ[out, c73Cells], noTouchingPairQ[touchHorizontallyQ, out], Count[out, _?isRange], Count[out, _?(Not@*isRange)]}],
	{True, True, 27, 19},
	TestID -> "ex11-C73-ContractSpreadsheetColumnRanges-exact-cover"]

VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[c73RangesByColumns],
	Sort@c73RangesByColumns,
	TestID -> "ex11-C73-ContractSpreadsheetColumnRanges-second-pass-changes-nothing"]

VerificationTest[
	Sort@ContractSpreadsheetRowRanges[rowsThenColumns[c73Cells]],
	Sort@c73RangesByRows,
	TestID -> "ex11-C73-ContractSpreadsheetRowRanges"]

(* 27 ranges and 13 single cells, none of them could be joined across rows *)
VerificationTest[
	With[{out = ContractSpreadsheetRowRanges[rowsThenColumns[c73Cells]]},
		{exactCoverQ[out, c73Cells], noTouchingPairQ[touchVerticallyQ, out], Count[out, _?isRange], Count[out, _?(Not@*isRange)]}],
	{True, True, 27, 13},
	TestID -> "ex11-C73-ContractSpreadsheetRowRanges-exact-cover"]

VerificationTest[
	Sort@ContractSpreadsheetRowRanges[c73RangesByRows],
	Sort@c73RangesByRows,
	TestID -> "ex11-C73-ContractSpreadsheetRowRanges-second-pass-changes-nothing"]

(* the steps of the Scope example of ResourceDefinition.nb: 321 cells, 100 after ContractSpreadsheetRows, 75 after
ContractSpreadsheetColumns on the cells left single, 40 after ContractSpreadsheetRowRanges *)
VerificationTest[
	{Length[c73Cells], Length@ContractSpreadsheetRows[c73Cells], Length@rowsThenColumns[c73Cells],
		Length@ContractSpreadsheetRowRanges@rowsThenColumns[c73Cells]},
	{321, 100, 75, 40},
	TestID -> "ex11-C73-Scope-example-of-ResourceDefinition"]

(* SpreadsheetParse expands the ranges back into the cells, each cell once (the last example of ResourceDefinition.nb) *)
VerificationTest[
	Sort@Flatten[SpreadsheetParse /@ ContractSpreadsheetRowRanges[rowsThenColumns[c73Cells]]],
	Sort@c73Cells,
	TestID -> "ex11-C73-SpreadsheetParse-expands-the-row-ranges-back"]

VerificationTest[
	Sort@Flatten[SpreadsheetParse /@ ContractSpreadsheetColumnRanges[columnsThenRows[c73Cells]]],
	Sort@c73Cells,
	TestID -> "ex11-C73-SpreadsheetParse-expands-the-column-ranges-back"]


(* ::Section:: *)
(*SpreadsheetContract*)


(* ::Text:: *)
(*Both pipelines, keeping the shorter result: on c73Cells, rows first gives 40 ranges and columns first 46.*)


VerificationTest[
	Sort@SpreadsheetContract[c73Cells],
	Sort@c73RangesByRows,
	TestID -> "SpreadsheetContract-ex11-C73-shorter-pipeline"]

VerificationTest[
	SpreadsheetContract[{}],
	{},
	TestID -> "SpreadsheetContract-no-cells"]

(* the final cells of the C73 trace also hold the defined names start and end, with numeric values: names are not
cells, so they pass through, once each *)
VerificationTest[
	Sort@SpreadsheetContract[Join[c73Cells, {"start", "end"}]],
	Sort@Join[c73RangesByRows, {"start", "end"}],
	TestID -> "SpreadsheetContract-ex11-C73-with-defined-names"]

VerificationTest[
	Sort@SpreadsheetContract[{"start", "Data!B2", "end", "Data!B3", "start", "'E - Bank Loan Schedule'!Loan_amount"}],
	Sort@{"Data!B2:B3", "start", "end", "'E - Bank Loan Schedule'!Loan_amount"},
	TestID -> "SpreadsheetContract-workbook-and-sheet-names-pass-through"]

(* SpreadsheetExpand undoes SpreadsheetContract, defined names included *)
VerificationTest[
	Sort@Flatten[SpreadsheetExpand /@ SpreadsheetContract[Join[c73Cells, {"start", "end"}]]],
	Sort@Join[c73Cells, {"start", "end"}],
	TestID -> "SpreadsheetContract-ex11-C73-SpreadsheetExpand-gives-the-labels-back"]


(* ::Section:: *)
(*Edge cases*)


(* ::Text:: *)
(*Inputs that the multi-sheet pipelines above do not produce.*)


(* ::Subsection:: *)
(*Cells without sheet (traces of one-sheet workbooks)*)


VerificationTest[
	Sort@ContractSpreadsheetColumns[{"B2", "B3", "C5"}],
	Sort@{"B2:B3", "C5"},
	TestID -> "ContractSpreadsheetColumns-cells-without-sheet"]

VerificationTest[
	Sort@ContractSpreadsheetRows[{"B2", "C2", "E5"}],
	Sort@{"B2:C2", "E5"},
	TestID -> "ContractSpreadsheetRows-cells-without-sheet"]

VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[{"B2:B4", "C2:C4", "E5"}],
	Sort@{"B2:C4", "E5"},
	TestID -> "ContractSpreadsheetColumnRanges-ranges-without-sheet"]

VerificationTest[
	Sort@ContractSpreadsheetRowRanges[{"B2:D2", "B3:D3", "F5"}],
	Sort@{"B2:D3", "F5"},
	TestID -> "ContractSpreadsheetRowRanges-ranges-without-sheet"]


(* ::Subsection:: *)
(*"!" in a sheet name (allowed by Excel, quoted in references)*)


VerificationTest[
	Sort@ContractSpreadsheetColumns[{"'Q1!'!B2", "'Q1!'!B3"}],
	Sort@{"'Q1!'!B2:B3"},
	TestID -> "ContractSpreadsheetColumns-exclamation-mark-in-sheet-name"]

VerificationTest[
	Sort@ContractSpreadsheetRows[{"'Q1!'!B2", "'Q1!'!C2"}],
	Sort@{"'Q1!'!B2:C2"},
	TestID -> "ContractSpreadsheetRows-exclamation-mark-in-sheet-name"]

VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[{"'Q1!'!B2:B3", "'Q1!'!C2:C3"}],
	Sort@{"'Q1!'!B2:C3"},
	TestID -> "ContractSpreadsheetColumnRanges-exclamation-mark-in-sheet-name"]

VerificationTest[
	Sort@ContractSpreadsheetRowRanges[{"'Q1!'!B2:C2", "'Q1!'!B3:C3"}],
	Sort@{"'Q1!'!B2:C3"},
	TestID -> "ContractSpreadsheetRowRanges-exclamation-mark-in-sheet-name"]


(* ::Subsection:: *)
(*Ranges more than one cell thick*)


VerificationTest[
	Sort@ContractSpreadsheetColumnRanges[{"Data!B2:C4", "Data!D2:D4", "Data!E2:F4"}],
	Sort@{"Data!B2:F4"},
	TestID -> "ContractSpreadsheetColumnRanges-wider-ranges-side-by-side"]

VerificationTest[
	Sort@ContractSpreadsheetRowRanges[{"Data!B2:D3", "Data!B4:D4", "Data!B5:D6"}],
	Sort@{"Data!B2:D6"},
	TestID -> "ContractSpreadsheetRowRanges-taller-ranges-on-top-of-each-other"]


(* ::Subsection:: *)
(*Defined names among the cells (final cells of a trace)*)


(* a workbook name used twice and a sheet name: they are not cells, so they pass through, once each *)
VerificationTest[
	Sort@ContractSpreadsheetColumns[{"start", "Data!B2", "end", "Data!B3", "start", "'E - Bank Loan Schedule'!Loan_amount"}],
	Sort@{"Data!B2:B3", "start", "end", "'E - Bank Loan Schedule'!Loan_amount"},
	TestID -> "ContractSpreadsheetColumns-defined-names-pass-through"]

VerificationTest[
	Sort@ContractSpreadsheetRows[{"start", "Data!B2", "end", "Data!C2", "start", "'E - Bank Loan Schedule'!Loan_amount"}],
	Sort@{"Data!B2:C2", "start", "end", "'E - Bank Loan Schedule'!Loan_amount"},
	TestID -> "ContractSpreadsheetRows-defined-names-pass-through"]


Clear[SpreadsheetToolkitTests`packageFile];
End[];
