(* ::Package:: *)

(* ::Title:: *)
(*SpreadsheetToolkit: tests of SpreadsheetTrace*)


(* ::Text:: *)
(*SpreadsheetTrace of Kernel/SpreadsheetToolkit.wl, the code of SpreadsheetTrace 2.0.0.*)
(*The 124 VerificationTests of SpreadsheetTrace-1-2-0-definition.nb, unchanged and in the same order, one section for*)
(*each of its subsections. SpreadsheetTrace on the output of SpreadsheetImport is tested in Import.wlt.*)
(*The example workbooks are downloaded from the Wolfram Cloud into $TemporaryDirectory, as in the definition notebook,*)
(*so the tests need an internet connection.*)
(*Run TestReport on this file, best in a fresh kernel. The package symbols are cleared and the package file is read*)
(*again with Get, so the tests see its current code. To test another version of the file, see the Setup.*)


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
(*Spreadsheet examples*)


file1 = URLDownload["https://www.wolframcloud.com/obj/064eb405-e9b5-4e95-941a-7898dd493f80",
	FileNameJoin[{$TemporaryDirectory, "example_01.xlsx"}]];
file2 = URLDownload["https://www.wolframcloud.com/obj/67cccf43-b7be-471a-9826-378d88ce4534",
	FileNameJoin[{$TemporaryDirectory, "example_02.xlsx"}]];
file3 = URLDownload["https://www.wolframcloud.com/obj/0439d3b1-0a32-4ccc-a01c-c724965a2386",
	FileNameJoin[{$TemporaryDirectory, "example_03.xlsx"}]];
file4 = URLDownload["https://www.wolframcloud.com/obj/cb328fb3-3a35-4601-a7cb-611e09245574",
	FileNameJoin[{$TemporaryDirectory, "example_04.xlsx"}]];
file5 = URLDownload["https://www.wolframcloud.com/obj/066a6a74-e239-40f7-bf4a-851fde0da7ea",
	FileNameJoin[{$TemporaryDirectory, "example_05.xlsx"}]];
file6 = URLDownload["https://www.wolframcloud.com/obj/65f08bd7-34be-4c30-a930-46a9f0233b47",
	FileNameJoin[{$TemporaryDirectory, "example_06.xlsx"}]];
file7 = URLDownload["https://www.wolframcloud.com/obj/ae1b975b-a7f8-4393-99a6-b79314d9a188",
	FileNameJoin[{$TemporaryDirectory, "example_07.xlsx"}]];
file8 = URLDownload["https://www.wolframcloud.com/obj/232ec035-0ce8-4476-8c7c-b3fb28e42005",
	FileNameJoin[{$TemporaryDirectory, "example_08.xlsx"}]];
file9 = URLDownload["https://www.wolframcloud.com/obj/fff979fa-54c1-490a-b0b8-482bcdaceaf1",
	FileNameJoin[{$TemporaryDirectory, "example_09.xlsx"}]];
file10 = URLDownload["https://www.wolframcloud.com/obj/8e58f010-de56-49c9-b481-deac7bfec1f9",
	FileNameJoin[{$TemporaryDirectory, "example_10.xlsx"}]];
file11 = URLDownload["https://www.wolframcloud.com/obj/2b7da83d-0149-4c42-971f-93961c7e7f90",
	FileNameJoin[{$TemporaryDirectory, "example_11.xlsx"}]];
file12 = URLDownload["https://www.wolframcloud.com/obj/12639b65-377b-4b0e-abe5-42626fe61b4e",
	FileNameJoin[{$TemporaryDirectory, "example_12.xlsx"}]];
file13 = URLDownload["https://www.wolframcloud.com/obj/4714edec-310b-445c-aa05-46a318c3cd93",
	FileNameJoin[{$TemporaryDirectory, "example_13.xlsx"}]];


(* ::Section:: *)
(*Validation*)


VerificationTest[
	Head[SpreadsheetTrace[file1, "ZZZ"]],
	SpreadsheetTrace,
	TestID -> "validation-bad-cell-unevaluated"]

VerificationTest[
	Head[SpreadsheetTrace[file1, ""]],
	SpreadsheetTrace,
	TestID -> "validation-empty-string-unevaluated"]

VerificationTest[
	Head[SpreadsheetTrace[file1, 123]],
	SpreadsheetTrace,
	TestID -> "validation-numeric-string-unevaluated"]


(* ::Section:: *)
(*Leafs*)


VerificationTest[
	SpreadsheetTrace[file1, "A1"],
	{"A1", 10.},
	TestID -> "ex1-A1-numeric-leaf"]

VerificationTest[
	SpreadsheetTrace[file1, "A5"],
	{"A5", "hello"},
	TestID -> "ex1-A5-string-leaf"]

VerificationTest[
	SpreadsheetTrace[file1, "C3"],
	{"C3", "B3-A3", {"B3", "A3^2", {"A3", 5.}}, {"A3", 5.}},
	TestID -> "ex1-C3-two-branches-shared-leaf"]


(* ::Section:: *)
(*First level*)


VerificationTest[
	SpreadsheetTrace[file1, "C1"],
	{"C1", "A1+B1", {"A1", 10.}, {"B1", 20.}},
	TestID -> "ex1-C1-two-deps"]

VerificationTest[
	SpreadsheetTrace[file1, "B3"],
	{"B3", "A3^2", {"A3", 5.}},
	TestID -> "ex1-B3-single-dep"]

VerificationTest[
	SpreadsheetTrace[file1, "B5"],
	{"B5", "LEN(A5)", {"A5", "hello"}},
	TestID -> "ex1-B5-LEN-text"]


(* ::Section:: *)
(*Higher levels*)


VerificationTest[
	SpreadsheetTrace[file1, "D1"],
	{"D1", "C1*2", {"C1", "A1+B1", {"A1", 10.}, {"B1", 20.}}},
	TestID -> "ex1-D1-two-level-chain"]

VerificationTest[
	SpreadsheetTrace[file1, "E1"],
	{"E1", "D1+A1", {"D1", "C1*2", {"C1", "A1+B1", {"A1", 10.}, {"B1", 20.}}}, {"A1", 10.}},
	TestID -> "ex1-E1-three-level-chain"]

VerificationTest[
	SpreadsheetTrace[file2, "D1"],
	{"D1", "C1+C5", {"C1", "B1+B2", {"B1", "SUM(A1:A3)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}}, {"B2", "SUM(A4:A6)", {"A4", 31.}, {"A5", 12.}, {"A6", 19.}}}, {"C5", "IF(C4>0,C4,0)", {"C4", "C1-C3", {"C1", "B1+B2", {"B1", "SUM(A1:A3)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}}, {"B2", "SUM(A4:A6)", {"A4", 31.}, {"A5", 12.}, {"A6", 19.}}}, {"C3", "B4*2", {"B4", "AVERAGE(A1:A6)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}, {"A4", 31.}, {"A5", 12.}, {"A6", 19.}}}}, {"C4", "C1-C3", {"C1", "B1+B2", {"B1", "SUM(A1:A3)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}}, {"B2", "SUM(A4:A6)", {"A4", 31.}, {"A5", 12.}, {"A6", 19.}}}, {"C3", "B4*2", {"B4", "AVERAGE(A1:A6)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}, {"A4", 31.}, {"A5", 12.}, {"A6", 19.}}}}}},
	TestID -> "ex2-D1-deep-converging-5-levels"]

VerificationTest[
	SpreadsheetTrace[file3, "B18"],
	{"B18", "IF(B17>0.5,\"Dominant\",\"Minor\")", {"B17", "B16/E9", {"B16", "SUMIF(B2:B7,\"Electronics\",E2:E7)", {"B2", "Electronics"}, {"B3", "Electronics"}, {"B4", "Home"}, {"B5", "Home"}, {"B6", "Industrial"}, {"B7", "Industrial"}, {"E2", "C2*D2", {"C2", 25.5}, {"D2", 100.}}, {"E3", "C3*D3", {"C3", 42.}, {"D3", 75.}}, {"E4", "C4*D4", {"C4", 18.99}, {"D4", 200.}}, {"E5", "C5*D5", {"C5", 35.}, {"D5", 150.}}, {"E6", "C6*D6", {"C6", 89.99}, {"D6", 50.}}, {"E7", "C7*D7", {"C7", 120.}, {"D7", 30.}}}, {"E9", "SUM(E2:E7)", {"E2", "C2*D2", {"C2", 25.5}, {"D2", 100.}}, {"E3", "C3*D3", {"C3", 42.}, {"D3", 75.}}, {"E4", "C4*D4", {"C4", 18.99}, {"D4", 200.}}, {"E5", "C5*D5", {"C5", 35.}, {"D5", 150.}}, {"E6", "C6*D6", {"C6", 89.99}, {"D6", 50.}}, {"E7", "C7*D7", {"C7", 120.}, {"D7", 30.}}}}},
	TestID -> "ex3-B18-IF-formula-string"]


(* ::Section:: *)
(*Dollar*)


VerificationTest[
	SpreadsheetTrace[file3, "F2"],
	{"F2", "E2*$B$10", {"E2", "C2*D2", {"C2", 25.5}, {"D2", 100.}}, {"B10", 0.22}},
	TestID -> "ex3-F2-absolute-ref-leaf"]

VerificationTest[
	SpreadsheetTrace[file3, "G2"],
	{"G2", "E2+F2", {"E2", "C2*D2", {"C2", 25.5}, {"D2", 100.}}, {"F2", "E2*$B$10", {"E2", "C2*D2", {"C2", 25.5}, {"D2", 100.}}, {"B10", 0.22}}},
	TestID -> "ex3-G2-chain-through-absolute-ref"]

VerificationTest[
	SpreadsheetTrace[file3, "F9"],
	{"F9", "SUM(F2:F7)", {"F2", "E2*$B$10", {"E2", "C2*D2", {"C2", 25.5}, {"D2", 100.}}, {"B10", 0.22}}, {"F3", "E3*$B$10", {"E3", "C3*D3", {"C3", 42.}, {"D3", 75.}}, {"B10", 0.22}}, {"F4", "E4*$B$10", {"E4", "C4*D4", {"C4", 18.99}, {"D4", 200.}}, {"B10", 0.22}}, {"F5", "E5*$B$10", {"E5", "C5*D5", {"C5", 35.}, {"D5", 150.}}, {"B10", 0.22}}, {"F6", "E6*$B$10", {"E6", "C6*D6", {"C6", 89.99}, {"D6", 50.}}, {"B10", 0.22}}, {"F7", "E7*$B$10", {"E7", "C7*D7", {"C7", 120.}, {"D7", 30.}}, {"B10", 0.22}}},
	TestID -> "ex3-F9-SUM-of-absolute-ref-formulas"]


(* ::Section:: *)
(*Duplicates*)


VerificationTest[
	SpreadsheetTrace[file2, "C5"],
	{"C5", "IF(C4>0,C4,0)", {"C4", "C1-C3", {"C1", "B1+B2", {"B1", "SUM(A1:A3)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}}, {"B2", "SUM(A4:A6)", {"A4", 31.}, {"A5", 12.}, {"A6", 19.}}}, {"C3", "B4*2", {"B4", "AVERAGE(A1:A6)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}, {"A4", 31.}, {"A5", 12.}, {"A6", 19.}}}}, {"C4", "C1-C3", {"C1", "B1+B2", {"B1", "SUM(A1:A3)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}}, {"B2", "SUM(A4:A6)", {"A4", 31.}, {"A5", 12.}, {"A6", 19.}}}, {"C3", "B4*2", {"B4", "AVERAGE(A1:A6)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}, {"A4", 31.}, {"A5", 12.}, {"A6", 19.}}}}},
	TestID -> "ex2-C5-IF-deep-duplicate-branches"]

VerificationTest[
	SpreadsheetTrace[file2, "C5", "TraceDuplicates" -> False],
	{"C5", "IF(C4>0,C4,0)", {"C4", "C1-C3", {"C1", "B1+B2", {"B1", "SUM(A1:A3)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}}, {"B2", "SUM(A4:A6)", {"A4", 31.}, {"A5", 12.}, {"A6", 19.}}}, {"C3", "B4*2", {"B4", "AVERAGE(A1:A6)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}, {"A4", 31.}, {"A5", 12.}, {"A6", 19.}}}}},
	TestID -> "ex2-C5-IF-deep-no-duplicate-branches"]


(* ::Section:: *)
(*Cell ranges*)


VerificationTest[
	SpreadsheetTrace[file2, "B1"],
	{"B1", "SUM(A1:A3)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}},
	TestID -> "ex2-B1-SUM-range"]

VerificationTest[
	SpreadsheetTrace[file2, "B4"],
	{"B4", "AVERAGE(A1:A6)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}, {"A4", 31.}, {"A5", 12.}, {"A6", 19.}},
	TestID -> "ex2-B4-AVERAGE-full-range"]

VerificationTest[
	SpreadsheetTrace[file2, "B5"],
	{"B5", "MAX(A1:A6)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}, {"A4", 31.}, {"A5", 12.}, {"A6", 19.}},
	TestID -> "ex2-B5-MAX-range"]

VerificationTest[
	SpreadsheetTrace[file2, "C4"],
	{"C4", "C1-C3", {"C1", "B1+B2", {"B1", "SUM(A1:A3)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}}, {"B2", "SUM(A4:A6)", {"A4", 31.}, {"A5", 12.}, {"A6", 19.}}}, {"C3", "B4*2", {"B4", "AVERAGE(A1:A6)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}, {"A4", 31.}, {"A5", 12.}, {"A6", 19.}}}},
	TestID -> "ex2-C4-two-deep-branches"]

VerificationTest[
	SpreadsheetTrace[file2, "D3"],
	{"D3", "B1*B4", {"B1", "SUM(A1:A3)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}}, {"B4", "AVERAGE(A1:A6)", {"A1", 15.}, {"A2", 22.}, {"A3", 8.}, {"A4", 31.}, {"A5", 12.}, {"A6", 19.}}},
	TestID -> "ex2-D3-diamond-shared-leaves"]

VerificationTest[
	SpreadsheetTrace[file3, "E9"],
	{"E9", "SUM(E2:E7)", {"E2", "C2*D2", {"C2", 25.5}, {"D2", 100.}}, {"E3", "C3*D3", {"C3", 42.}, {"D3", 75.}}, {"E4", "C4*D4", {"C4", 18.99}, {"D4", 200.}}, {"E5", "C5*D5", {"C5", 35.}, {"D5", 150.}}, {"E6", "C6*D6", {"C6", 89.99}, {"D6", 50.}}, {"E7", "C7*D7", {"C7", 120.}, {"D7", 30.}}},
	TestID -> "ex3-E9-SUM-of-formulas"]

VerificationTest[
	SpreadsheetTrace[file3, "B15"],
	{"B15", "COUNTIF(B2:B7,\"Electronics\")", {"B2", "Electronics"}, {"B3", "Electronics"}, {"B4", "Home"}, {"B5", "Home"}, {"B6", "Industrial"}, {"B7", "Industrial"}},
	TestID -> "ex3-B15-COUNTIF-string-range"]

VerificationTest[
	SpreadsheetTrace[file3, "B16"],
	{"B16", "SUMIF(B2:B7,\"Electronics\",E2:E7)", {"B2", "Electronics"}, {"B3", "Electronics"}, {"B4", "Home"}, {"B5", "Home"}, {"B6", "Industrial"}, {"B7", "Industrial"}, {"E2", "C2*D2", {"C2", 25.5}, {"D2", 100.}}, {"E3", "C3*D3", {"C3", 42.}, {"D3", 75.}}, {"E4", "C4*D4", {"C4", 18.99}, {"D4", 200.}}, {"E5", "C5*D5", {"C5", 35.}, {"D5", 150.}}, {"E6", "C6*D6", {"C6", 89.99}, {"D6", 50.}}, {"E7", "C7*D7", {"C7", 120.}, {"D7", 30.}}},
	TestID -> "ex3-B16-SUMIF-two-ranges"]

VerificationTest[
	Length[SpreadsheetTrace[file4, "D14"]],
	14,
	TestID -> "ex4-D14-SUM-12-formulas-length-14"]

VerificationTest[
	SpreadsheetTrace[file4, "D14"][[3]],
	{"Input!D2", "B2-C2", {"Input!B2", 12000.}, {"Input!C2", "B2*0.65", {"Input!B2", 12000.}}},
	TestID -> "ex4-D14-first-subtree-D2"]


(* ::Section:: *)
(*Cross sheet*)


VerificationTest[
	SpreadsheetTrace[file4, "C2"],
	{"Input!C2", "B2*0.65", {"Input!B2", 12000.}},
	TestID -> "ex4-C2-single-dep-first-sheet"]

VerificationTest[
	SpreadsheetTrace[file4, "B14"],
	{"Input!B14", "SUM(B2:B13)", {"Input!B2", 12000.}, {"Input!B3", 15000.}, {"Input!B4", 13500.}, {"Input!B5", 16000.}, {"Input!B6", 18000.}, {"Input!B7", 22000.}, {"Input!B8", 25000.}, {"Input!B9", 24000.}, {"Input!B10", 20000.}, {"Input!B11", 17000.}, {"Input!B12", 14000.}, {"Input!B13", 19000.}},
	TestID -> "compat-ex4-B14-first-sheet-default"]

VerificationTest[
	SpreadsheetTrace[file4, "Summary!B3"],
	{"Summary!B3", "Input!B14", {"Input!B14", "SUM(B2:B13)", {"Input!B2", 12000.}, {"Input!B3", 15000.}, {"Input!B4", 13500.}, {"Input!B5", 16000.}, {"Input!B6", 18000.}, {"Input!B7", 22000.}, {"Input!B8", 25000.}, {"Input!B9", 24000.}, {"Input!B10", 20000.}, {"Input!B11", 17000.}, {"Input!B12", 14000.}, {"Input!B13", 19000.}}},
	TestID -> "ex4-Summary-B3-cross-sheet-SUM"]

VerificationTest[
	SpreadsheetTrace[file4, "Summary!B8"],
	{"Summary!B8", "AVERAGE(Input!B2:Input!B13)", {"Input!B2", 12000.}, {"Input!B3", 15000.}, {"Input!B4", 13500.}, {"Input!B5", 16000.}, {"Input!B6", 18000.}, {"Input!B7", 22000.}, {"Input!B8", 25000.}, {"Input!B9", 24000.}, {"Input!B10", 20000.}, {"Input!B11", 17000.}, {"Input!B12", 14000.}, {"Input!B13", 19000.}},
	TestID -> "ex4-Summary-B8-cross-sheet-AVERAGE-range"]

VerificationTest[
	Length[SpreadsheetTrace[file4, "Summary!B5"][[3]]],
	14,
	TestID -> "ex4-Summary-B5-subtree-has-12-formulas"]

VerificationTest[
	Length[SpreadsheetTrace[file4, "Summary!B6"]],
	4,
	TestID -> "ex4-Summary-B6-two-branches"]

VerificationTest[
	SpreadsheetTrace[file6, "Analysis!B3"],
	{"Analysis!B3", "SUM(PnL!B3:H3)", {"PnL!B3", "Revenue!B8", {"Revenue!B8", "B5+B7", {"Revenue!B5", "B3*B4", {"Revenue!B3", "Assumptions!B7", {"Assumptions!B7", 1000.}}, {"Revenue!B4", "Assumptions!B8", {"Assumptions!B8", 50.}}}, {"Revenue!B7", 2000.}}}, {"PnL!C3", "Revenue!C8", {"Revenue!C8", "C5+C7", {"Revenue!C5", "C3*C4", {"Revenue!C3", "ROUND(B3*(1+Assumptions!$B$3),0)", {"Revenue!B3", "Assumptions!B7", {"Assumptions!B7", 1000.}}, {"Assumptions!B3", 0.08}}, {"Revenue!C4", "B4*(1+Assumptions!$B$5)", {"Revenue!B4", "Assumptions!B8", {"Assumptions!B8", 50.}}, {"Assumptions!B5", 0.03}}}, {"Revenue!C7", "B7*1.05", {"Revenue!B7", 2000.}}}}, {"PnL!D3", "Revenue!D8", {"Revenue!D8", "D5+D7", {"Revenue!D5", "D3*D4", {"Revenue!D3", "ROUND(C3*(1+Assumptions!$B$3),0)", {"Revenue!C3", "ROUND(B3*(1+Assumptions!$B$3),0)", {"Revenue!B3", "Assumptions!B7", {"Assumptions!B7", 1000.}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Revenue!D4", "C4*(1+Assumptions!$B$5)", {"Revenue!C4", "B4*(1+Assumptions!$B$5)", {"Revenue!B4", "Assumptions!B8", {"Assumptions!B8", 50.}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}}, {"Revenue!D7", "C7*1.05", {"Revenue!C7", "B7*1.05", {"Revenue!B7", 2000.}}}}}, {"PnL!E3", "Revenue!E8", {"Revenue!E8", "E5+E7", {"Revenue!E5", "E3*E4", {"Revenue!E3", "ROUND(D3*(1+Assumptions!$B$3),0)", {"Revenue!D3", "ROUND(C3*(1+Assumptions!$B$3),0)", {"Revenue!C3", "ROUND(B3*(1+Assumptions!$B$3),0)", {"Revenue!B3", "Assumptions!B7", {"Assumptions!B7", 1000.}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Revenue!E4", "D4*(1+Assumptions!$B$5)", {"Revenue!D4", "C4*(1+Assumptions!$B$5)", {"Revenue!C4", "B4*(1+Assumptions!$B$5)", {"Revenue!B4", "Assumptions!B8", {"Assumptions!B8", 50.}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}}, {"Revenue!E7", "D7*1.05", {"Revenue!D7", "C7*1.05", {"Revenue!C7", "B7*1.05", {"Revenue!B7", 2000.}}}}}}, {"PnL!F3", "Revenue!F8", {"Revenue!F8", "F5+F7", {"Revenue!F5", "F3*F4", {"Revenue!F3", "ROUND(E3*(1+Assumptions!$B$3),0)", {"Revenue!E3", "ROUND(D3*(1+Assumptions!$B$3),0)", {"Revenue!D3", "ROUND(C3*(1+Assumptions!$B$3),0)", {"Revenue!C3", "ROUND(B3*(1+Assumptions!$B$3),0)", {"Revenue!B3", "Assumptions!B7", {"Assumptions!B7", 1000.}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Revenue!F4", "E4*(1+Assumptions!$B$5)", {"Revenue!E4", "D4*(1+Assumptions!$B$5)", {"Revenue!D4", "C4*(1+Assumptions!$B$5)", {"Revenue!C4", "B4*(1+Assumptions!$B$5)", {"Revenue!B4", "Assumptions!B8", {"Assumptions!B8", 50.}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}}, {"Revenue!F7", "E7*1.05", {"Revenue!E7", "D7*1.05", {"Revenue!D7", "C7*1.05", {"Revenue!C7", "B7*1.05", {"Revenue!B7", 2000.}}}}}}}, {"PnL!G3", "Revenue!G8", {"Revenue!G8", "G5+G7", {"Revenue!G5", "G3*G4", {"Revenue!G3", "ROUND(F3*(1+Assumptions!$B$3),0)", {"Revenue!F3", "ROUND(E3*(1+Assumptions!$B$3),0)", {"Revenue!E3", "ROUND(D3*(1+Assumptions!$B$3),0)", {"Revenue!D3", "ROUND(C3*(1+Assumptions!$B$3),0)", {"Revenue!C3", "ROUND(B3*(1+Assumptions!$B$3),0)", {"Revenue!B3", "Assumptions!B7", {"Assumptions!B7", 1000.}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Revenue!G4", "F4*(1+Assumptions!$B$5)", {"Revenue!F4", "E4*(1+Assumptions!$B$5)", {"Revenue!E4", "D4*(1+Assumptions!$B$5)", {"Revenue!D4", "C4*(1+Assumptions!$B$5)", {"Revenue!C4", "B4*(1+Assumptions!$B$5)", {"Revenue!B4", "Assumptions!B8", {"Assumptions!B8", 50.}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}}, {"Revenue!G7", "F7*1.05", {"Revenue!F7", "E7*1.05", {"Revenue!E7", "D7*1.05", {"Revenue!D7", "C7*1.05", {"Revenue!C7", "B7*1.05", {"Revenue!B7", 2000.}}}}}}}}, {"PnL!H3", "Revenue!H8", {"Revenue!H8", "H5+H7", {"Revenue!H5", "H3*H4", {"Revenue!H3", "ROUND(G3*(1+Assumptions!$B$3),0)", {"Revenue!G3", "ROUND(F3*(1+Assumptions!$B$3),0)", {"Revenue!F3", "ROUND(E3*(1+Assumptions!$B$3),0)", {"Revenue!E3", "ROUND(D3*(1+Assumptions!$B$3),0)", {"Revenue!D3", "ROUND(C3*(1+Assumptions!$B$3),0)", {"Revenue!C3", "ROUND(B3*(1+Assumptions!$B$3),0)", {"Revenue!B3", "Assumptions!B7", {"Assumptions!B7", 1000.}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Assumptions!B3", 0.08}}, {"Revenue!H4", "G4*(1+Assumptions!$B$5)", {"Revenue!G4", "F4*(1+Assumptions!$B$5)", {"Revenue!F4", "E4*(1+Assumptions!$B$5)", {"Revenue!E4", "D4*(1+Assumptions!$B$5)", {"Revenue!D4", "C4*(1+Assumptions!$B$5)", {"Revenue!C4", "B4*(1+Assumptions!$B$5)", {"Revenue!B4", "Assumptions!B8", {"Assumptions!B8", 50.}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}, {"Assumptions!B5", 0.03}}}, {"Revenue!H7", "G7*1.05", {"Revenue!G7", "F7*1.05", {"Revenue!F7", "E7*1.05", {"Revenue!E7", "D7*1.05", {"Revenue!D7", "C7*1.05", {"Revenue!C7", "B7*1.05", {"Revenue!B7", 2000.}}}}}}}}}},
	TestID -> "ex6-Analysis-B3-cell-range-cross-sheet"]

VerificationTest[
	SpreadsheetTrace[file9, "Config!B16"],
	{"Config!B16", "AVERAGE(B5,B6)", {"Config!B5", 0.25}, {"Config!B6", 0.3}},
	TestID -> "ex9-Config-B16-sheeted-input"]

VerificationTest[
	SpreadsheetTrace[file9, "B16"],
	{"Config!B16", "AVERAGE(B5,B6)", {"Config!B5", 0.25}, {"Config!B6", 0.3}},
	TestID -> "ex9-B16-plain-matches-Config"]

VerificationTest[
	SpreadsheetTrace[file9, "Config!B17"],
	{"Config!B17", "1-B16", {"Config!B16", "AVERAGE(B5,B6)", {"Config!B5", 0.25}, {"Config!B6", 0.3}}},
	TestID -> "ex9-Config-B17-chain"]

VerificationTest[
	SpreadsheetTrace[file10, "B2"],
	{"Summary!B2", "Data!A1", {"Data!A1", 10.}},
	TestID -> "ex10-B2-cross-sheet-leaf"]

VerificationTest[
	SpreadsheetTrace[file10, "B4"],
	{"Summary!B4", "Data!B1", {"Data!B1", "SUM(A1:A3)", {"Data!A1", 10.}, {"Data!A2", 20.}, {"Data!A3", 30.}}},
	TestID -> "ex10-B4-cross-sheet-to-formula"]

VerificationTest[
	SpreadsheetTrace[file10, "B5"],
	{"Summary!B5", "B2+B3", {"Summary!B2", "Data!A1", {"Data!A1", 10.}}, {"Summary!B3", "Data!A2", {"Data!A2", 20.}}},
	TestID -> "ex10-B5-local-to-cross-sheet"]

VerificationTest[
	SpreadsheetTrace[file10, "B6"],
	{"Summary!B6", "B2+Data!A3", {"Summary!B2", "Data!A1", {"Data!A1", 10.}}, {"Data!A3", 30.}},
	TestID -> "ex10-B6-mixed-deps"]


(* ::Section:: *)
(*Column ranges*)


VerificationTest[
	Length[SpreadsheetTrace[file5, "Orders!D2"]],
	30,
	TestID -> "col-range-ex5-Orders-D2-length-30"]

VerificationTest[
	Dimensions[Table[SpreadsheetTrace[file5, StringJoin["Orders!D", ToString[i]]], {i, 2, 11}]],
	{10, 30},
	TestID -> "col-range-ex5-all-VLOOKUP-rows-dimension"]

VerificationTest[
	SpreadsheetTrace[file5, "Orders!G2"],
	{"Orders!G2", "E2*(1-F2)", {"Orders!E2", "C2*D2", {"Orders!C2", 2.}, {"Orders!D2", "VLOOKUP(B2,Products!A:C,3,FALSE)", {"Orders!B2", 101.}, {"Products!A1", "ProductID"}, {"Products!A2", 101.}, {"Products!A3", 102.}, {"Products!A4", 103.}, {"Products!A5", 104.}, {"Products!A6", 105.}, {"Products!A7", 106.}, {"Products!A8", 107.}, {"Products!A9", 108.}, {"Products!B1", "Name"}, {"Products!B2", "Laptop"}, {"Products!B3", "Mouse"}, {"Products!B4", "Desk"}, {"Products!B5", "Chair"}, {"Products!B6", "Monitor"}, {"Products!B7", "Keyboard"}, {"Products!B8", "Lamp"}, {"Products!B9", "Headset"}, {"Products!C1", "UnitPrice"}, {"Products!C2", 999.99}, {"Products!C3", 29.99}, {"Products!C4", 249.99}, {"Products!C5", 349.99}, {"Products!C6", 449.99}, {"Products!C7", 79.99}, {"Products!C8", 59.99}, {"Products!C9", 129.99}}}, {"Orders!F2", "IF(C2>=5,0.1,0)", {"Orders!C2", 2.}}},
	TestID -> "col-range-ex5-G2-deep-chain-explicit"]

VerificationTest[
	Depth[SpreadsheetTrace[file5, "Dashboard!B3"]],
	6,
	TestID -> "col-range-ex5-Dashboard-B3-depth-6"]

VerificationTest[
	Depth /@ SpreadsheetTrace[file5, "Dashboard!B3"][[3 ;; All]][[1]],
	{1, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4},
	TestID -> "col-range-ex5-Dashboard-B3-depth-list"]

VerificationTest[
	Map[Depth, SpreadsheetTrace[file5, "Dashboard!B3"][[3 ;; All]][[1]][[3 ;; All]], {2}],
	{{1, 1, 2, 3}, {1, 1, 2, 3}, {1, 1, 2, 3}, {1, 1, 2, 3}, {1, 1, 2, 3}, {1, 1, 2, 3}, {1, 1, 2, 3}, {1, 1, 2, 3}, {1, 1, 2, 3}, {1, 1, 2, 3}},
	TestID -> "col-range-ex5-Dashboard-B3-depth-list-2"]

VerificationTest[
	Length[SpreadsheetTrace[file7, "Budget!B4"][[1]]],
	39,
	TestID -> "col-range-ex7-Budget-B4-len-total"]

VerificationTest[
	Length /@ SpreadsheetTrace[file7, "Budget!B4"][[1,1 ;; 22]],
	{0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2},
	TestID -> "col-range-ex7-Budget-B4-len-upto-22"]

VerificationTest[
	Depth /@ SpreadsheetTrace[file7, "Budget!B4"][[1,23 ;; All]],
	{2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3},
	TestID -> "col-range-ex7-Budget-B4-depth-after-22"]


(* ::Section:: *)
(*Nested formulas*)


VerificationTest[
	SpreadsheetTrace[file3, "B19"],
	{"B19", "ROUND(AVERAGE(E2:E7)*1.1, 2)", {"E2", "C2*D2", {"C2", 25.5}, {"D2", 100.}}, {"E3", "C3*D3", {"C3", 42.}, {"D3", 75.}}, {"E4", "C4*D4", {"C4", 18.99}, {"D4", 200.}}, {"E5", "C5*D5", {"C5", 35.}, {"D5", 150.}}, {"E6", "C6*D6", {"C6", 89.99}, {"D6", 50.}}, {"E7", "C7*D7", {"C7", 120.}, {"D7", 30.}}},
	TestID -> "ex3-B19-nested-ROUND-AVERAGE"]

VerificationTest[
	SpreadsheetTrace[file6, "B16"],
	{"Assumptions!B16", "PMT(B13,B14,-B12)", {"Assumptions!B13", 0.05}, {"Assumptions!B14", 5.}, {"Assumptions!B12", 100000.}},
	TestID -> "ex6-B16-PMT-three-leaf-deps"]

VerificationTest[
	SpreadsheetTrace[file6, "B18"],
	{"Assumptions!B18", "ROUND(B10/(B8-B17),0)", {"Assumptions!B10", 5000.}, {"Assumptions!B8", 50.}, {"Assumptions!B17", "B9*(1+B5)", {"Assumptions!B9", 30.}, {"Assumptions!B5", 0.03}}},
	TestID -> "ex6-B18-nested-ROUND-with-chain"]

VerificationTest[
	SpreadsheetTrace[file8, "B10"],
	{"Suppliers!B10", "INDEX(B2:B6,MATCH(MAX(E2:E6),E2:E6,0))", {"Suppliers!B2", "Acme Corp"}, {"Suppliers!B3", "GlobalParts"}, {"Suppliers!B4", "EastMfg"}, {"Suppliers!B5", "NordicSupply"}, {"Suppliers!B6", "MediterraneanCo"}, {"Suppliers!E2", 0.95}, {"Suppliers!E3", 0.92}, {"Suppliers!E4", 0.88}, {"Suppliers!E5", 0.97}, {"Suppliers!E6", 0.9}, {"Suppliers!E2", 0.95}, {"Suppliers!E3", 0.92}, {"Suppliers!E4", 0.88}, {"Suppliers!E5", 0.97}, {"Suppliers!E6", 0.9}},
	TestID -> "ex8-B10-INDEX-MATCH-MAX-nested"]

VerificationTest[
	SpreadsheetTrace[file8, "Catalog!I2"],
	{"Catalog!I2", "IFERROR(INDEX(Suppliers!D:D,MATCH(D2,Suppliers!A:A,0)),\"N/A\")", {"Suppliers!D1", "Lead Time (days)"}, {"Suppliers!D2", 7.}, {"Suppliers!D3", 14.}, {"Suppliers!D4", 21.}, {"Suppliers!D5", 10.}, {"Suppliers!D6", 12.}, {"Suppliers!D7", ""}, {"Suppliers!D8", ""}, {"Suppliers!D9", ""}, {"Suppliers!D10", ""}, {"Suppliers!D11", ""}, {"Catalog!D2", "SUP01"}, {"Suppliers!A1", "SupplierID"}, {"Suppliers!A2", "SUP01"}, {"Suppliers!A3", "SUP02"}, {"Suppliers!A4", "SUP03"}, {"Suppliers!A5", "SUP04"}, {"Suppliers!A6", "SUP05"}, {"Suppliers!A7", ""}, {"Suppliers!A8", "Avg Lead Time"}, {"Suppliers!A9", "Avg Reliability"}, {"Suppliers!A10", "Best Supplier"}, {"Suppliers!A11", "Gold Suppliers"}},
	TestID -> "col-range-ex8-Catalog-I2-nested"]


(* ::Section:: *)
(*Sheet names*)


VerificationTest[
	With[{t = SpreadsheetTrace[file11, "Monthly Costs (1st year,\[Euro])!F22"]}, {t[[1 ;; 2]], t[[3 ;; All,1]]}],
	{{"'Monthly Costs (1st year,\[Euro])'!F22", "+((SUM('Monthly Revenue (1st year, \[Euro])'!F$43:F$46))*36%)+SUM('Monthly Revenue (1st year, \[Euro])'!F$48:F$55)*38%"}, {"'Monthly Revenue (1st year, \[Euro])'!F43", "'Monthly Revenue (1st year, \[Euro])'!F44", "'Monthly Revenue (1st year, \[Euro])'!F45", "'Monthly Revenue (1st year, \[Euro])'!F46", "'Monthly Revenue (1st year, \[Euro])'!F48", "'Monthly Revenue (1st year, \[Euro])'!F49", "'Monthly Revenue (1st year, \[Euro])'!F50", "'Monthly Revenue (1st year, \[Euro])'!F51", "'Monthly Revenue (1st year, \[Euro])'!F52", "'Monthly Revenue (1st year, \[Euro])'!F53", "'Monthly Revenue (1st year, \[Euro])'!F54", "'Monthly Revenue (1st year, \[Euro])'!F55"}},
	TestID -> "ex11-sheet-with-brackets-comma-and-euro-sign-dollar-ranges"]

VerificationTest[
	SpreadsheetTrace[file11, "6 - Ratios!B101"],
	{"'6 - Ratios'!B101", "IF(B86<=0,\"n/a (year's end)\",'Extra \"Q1\" (year''s end)'!B2)+SUM('Extra \"Q1\" (year''s end)'!B3:B4)", {"'6 - Ratios'!B86", 1.2}, {"'Extra \"Q1\" (year''s end)'!B2", 5.}, {"'Extra \"Q1\" (year''s end)'!B3", 6.}, {"'Extra \"Q1\" (year''s end)'!B4", 7.}},
	TestID -> "ex11-sheet-with-brackets-and-quotes"]

VerificationTest[
	SpreadsheetTrace[file11, "'Extra \"Q1\" (year''s end)'!B2"],
	{"'Extra \"Q1\" (year''s end)'!B2", 5.},
	TestID -> "ex11-sheet-with-brackets-and-quotes-quoted-argument"]

VerificationTest[
	SpreadsheetTrace[file11, "Extra \"Q1\" (year's end)!B2"],
	{"'Extra \"Q1\" (year''s end)'!B2", 5.},
	TestID -> "ex11-sheet-with-brackets-and-quotes-plain-argument"]

VerificationTest[
	With[{t = SpreadsheetTrace[file11, "6 - Ratios!B70"]}, {t[[1 ;; 2]], Length[t], t[[3]], t[[-1]]}],
	{{"'6 - Ratios'!B70", "SUM('2 - Operating Revenue and Costs'!C3:AF3)"}, 32, {"'2 - Operating Revenue and Costs'!C3", 153.}, {"'2 - Operating Revenue and Costs'!AF3", 153.}},
	TestID -> "ex11-sheet-starting-with-digit-horizontal-range"]

VerificationTest[
	SpreadsheetTrace[file11, "6 - Ratios!B71"],
	{"'6 - Ratios'!B71", "+' B - Capex and Depreciation'!B5", {"' B - Capex and Depreciation'!B5", "+'Technical Data'!B14", {"'Technical Data'!B14", 320000.}}},
	TestID -> "ex11-sheet-with-leading-space"]

VerificationTest[
	SpreadsheetTrace[file11, "6 - Ratios!B90"],
	{"'6 - Ratios'!B90", "+checks!B2*2", {"checks!B2", 7.}},
	TestID -> "ex11-unquoted-sheet-among-quoted-sheets"]

VerificationTest[
	SpreadsheetTrace[file12, "Calc!B5"],
	{"Calc!B5", "+'2024'!B2+'Q1'!B2", {"'2024'!B2", 100.}, {"'Q1'!B2", 10.}},
	TestID -> "ex12-sheet-names-quoted-without-spaces"]

VerificationTest[
	SpreadsheetTrace[file12, "Calc!B6"],
	{"Calc!B6", "+'O''Brien'!B2", {"'O''Brien'!B2", 3.}},
	TestID -> "ex12-apostrophe-in-sheet-name"]

VerificationTest[
	SpreadsheetTrace[file12, "Collegamenti!B3"],
	{"Collegamenti!B3", "+Altro!B2+SUM(Jan:Altro!B2)+SUM(Altro!A:A)+SUM(Altro!A1:B1)", {"Altro!B2", Missing["UnknownSheet"]}, {"Jan:Altro!B2", Missing["UnknownSheet"]}, {"Altro!A:A", Missing["UnknownSheet"]}, {"Altro!A1", Missing["UnknownSheet"]}, {"Altro!B1", Missing["UnknownSheet"]}},
	TestID -> "ex12-references-to-a-sheet-not-in-the-workbook"]

VerificationTest[
	SpreadsheetTrace[file12, "Altro!B2"],
	{"Altro!B2", Missing["UnknownSheet"]},
	TestID -> "ex12-starting-cell-on-a-sheet-not-in-the-workbook"]

VerificationTest[
	SpreadsheetTrace[file12, "Calc!C2"],
	{"Calc!C2", "Jan!B2+Feb!B2*'2024'!B2-'O''Brien'!B2+SUM(Jan:Mar!B2)+SUM('Q1'!A2:B2)", {"Jan!B2", 1.}, {"Feb!B2", 2.}, {"'2024'!B2", 100.}, {"'O''Brien'!B2", 3.}, {"Jan!B2", 1.}, {"Feb!B2", 2.}, {"Mar!B2", 3.}, {"'Q1'!A2", "Valore"}, {"'Q1'!B2", 10.}},
	TestID -> "ex12-many-sheets-in-one-formula"]

VerificationTest[
	SpreadsheetTrace[file11, "6 - Ratios!B100"],
	{"'6 - Ratios'!B100", "+'O ''''Brien'!B2*2", {"'O ''''Brien'!B2", 3.}},
	TestID -> "ex11-sheet-name-with-two-apostrophes-in-formula"]

VerificationTest[
	SpreadsheetTrace[file11, "'O ''''Brien'!B2"],
	{"'O ''''Brien'!B2", 3.},
	TestID -> "ex11-sheet-name-with-two-apostrophes-quoted-argument"]

VerificationTest[
	SpreadsheetTrace[file11, "O ''Brien!B2"],
	{"'O ''''Brien'!B2", 3.},
	TestID -> "ex11-sheet-name-with-two-apostrophes-plain-argument"]


(* ::Section:: *)
(*Extra ranges*)


VerificationTest[
	SpreadsheetTrace[file11, "input Leasing!B25"],
	{"'input Leasing'!B25", "SUM(I20:J22)", {"'input Leasing'!I20", 4200.}, {"'input Leasing'!J20", 4600.}, {"'input Leasing'!I21", 4200.}, {"'input Leasing'!J21", 4600.}, {"'input Leasing'!I22", 4200.}, {"'input Leasing'!J22", 4600.}},
	TestID -> "ex11-2D-range"]

VerificationTest[
	SpreadsheetTrace[file11, "input Leasing!B30"],
	{"'input Leasing'!B30", "VLOOKUP(B29,'Price Table'!A:C,3,FALSE)", {"'input Leasing'!B29", 102.}, {"'Price Table'!A1", "Code"}, {"'Price Table'!A2", 101.}, {"'Price Table'!A3", 102.}, {"'Price Table'!A4", 103.}, {"'Price Table'!B1", "Description"}, {"'Price Table'!B2", "Umbrella"}, {"'Price Table'!B3", "Sunbed"}, {"'Price Table'!B4", "Cabana"}, {"'Price Table'!C1", "Price"}, {"'Price Table'!C2", 25.}, {"'Price Table'!C3", 12.}, {"'Price Table'!C4", 60.}},
	TestID -> "ex11-whole-column-range"]

VerificationTest[
	SpreadsheetTrace[file11, "input Leasing!B31"],
	{"'input Leasing'!B31", "VLOOKUP(B29,'Price List 2025'!A:B,2,FALSE)", {"'input Leasing'!B29", 102.}, {"'Price List 2025'!A1", "Code"}, {"'Price List 2025'!A2", 101.}, {"'Price List 2025'!A3", 102.}, {"'Price List 2025'!B1", "Price"}, {"'Price List 2025'!B2", 26.}, {"'Price List 2025'!B3", 13.}},
	TestID -> "ex11-whole-column-range-on-sheet-with-digits"]

VerificationTest[
	SpreadsheetTrace[file12, "Calc!B3"],
	{"Calc!B3", "SUM(D:D)", {"Calc!D1", 2.}, {"Calc!D2", 3.}, {"Calc!D3", 4.}, {"Calc!D4", ""}, {"Calc!D5", ""}, {"Calc!D6", ""}, {"Calc!D7", ""}, {"Calc!D8", ""}, {"Calc!D9", ""}},
	TestID -> "ex12-whole-column-range-same-sheet"]

VerificationTest[
	SpreadsheetTrace[file12, "Calc!B7"],
	{"Calc!B7", "SUM(Jan:Mar!B2)", {"Jan!B2", 1.}, {"Feb!B2", 2.}, {"Mar!B2", 3.}},
	TestID -> "ex12-3D-reference"]

VerificationTest[
	SpreadsheetTrace[file12, "Calc!B8"],
	{"Calc!B8", "SUMPRODUCT(D1:D3*E1:E3)", {"Calc!D1", 2.}, {"Calc!D2", 3.}, {"Calc!D3", 4.}, {"Calc!E1", 10.}, {"Calc!E2", 20.}, {"Calc!E3", 30.}},
	TestID -> "ex12-ranges-inside-array-expression"]

VerificationTest[
	SpreadsheetTrace[file12, "Calc!B4"],
	{"Calc!B4", "SUM(D3:D1)+SUM(E1:D3)", {"Calc!D1", 2.}, {"Calc!D2", 3.}, {"Calc!D3", 4.}, {"Calc!D1", 2.}, {"Calc!E1", 10.}, {"Calc!D2", 3.}, {"Calc!E2", 20.}, {"Calc!D3", 4.}, {"Calc!E3", 30.}},
	TestID -> "ex12-ranges-with-corners-in-reverse-order"]


(* ::Section:: *)
(*Special formula contents*)


VerificationTest[
	SpreadsheetTrace[file11, "6 - Ratios!B95"],
	{"'6 - Ratios'!B95", "IF(B86>0,\"Total (year, 1st)\",\"year's end\")", {"'6 - Ratios'!B86", 1.2}},
	TestID -> "ex11-text-with-brackets-commas-apostrophe"]

VerificationTest[
	SpreadsheetTrace[file11, "6 - Ratios!B96"],
	{"'6 - Ratios'!B96", "IF(B86>0,\"Q1\",\"Q2\")", {"'6 - Ratios'!B86", 1.2}},
	TestID -> "ex11-text-looking-like-cells"]

VerificationTest[
	SpreadsheetTrace[file11, "input Leasing!B13"],
	{"'input Leasing'!B13", "+B12*Rate1", {"'input Leasing'!B12", "end-start+1", {"end", 5.}, {"start", 1.}}, {"Rate1", 0.045}},
	TestID -> "ex11-defined-names"]

VerificationTest[
	(SpreadsheetTrace[file11, #1][[3 ;; All,1]] & ) /@ {"E - Bank Loan Schedule!C18", "F - VAT Loan Schedule!C18"},
	{{"'E - Bank Loan Schedule'!Annual_rate", "'E - Bank Loan Schedule'!Payments_per_year", "'E - Bank Loan Schedule'!Years", "'E - Bank Loan Schedule'!Payments_per_year", "'E - Bank Loan Schedule'!Loan_amount"}, {"'F - VAT Loan Schedule'!Annual_rate", "'F - VAT Loan Schedule'!Payments_per_year", "'F - VAT Loan Schedule'!Years", "'F - VAT Loan Schedule'!Payments_per_year", "'F - VAT Loan Schedule'!Loan_amount"}},
	TestID -> "ex11-sheet-names-with-the-same-text-on-two-sheets"]

VerificationTest[
	SpreadsheetTrace[file11, "6 - Ratios!B97"],
	{"'6 - Ratios'!B97", "+B98+Z999", {"'6 - Ratios'!B98", ""}, {"'6 - Ratios'!Z999", ""}},
	TestID -> "ex11-empty-and-out-of-range-cells"]

VerificationTest[
	SpreadsheetTrace[file12, "Calc!B9"],
	{"Calc!B9", "LOG10(D1)+ATAN2(D2,D3)", {"Calc!D1", 2.}, {"Calc!D2", 3.}, {"Calc!D3", 4.}},
	TestID -> "ex12-function-names-with-digits"]


(* ::Section:: *)
(*Special recursions*)


VerificationTest[
	SpreadsheetTrace[file11, "Administrative Data!D12"][[3 ;; All,1]],
	{"'Administrative Data'!D1", "'Administrative Data'!B5", "'Administrative Data'!D14", "'Administrative Data'!C12", "'Administrative Data'!D13", "'Administrative Data'!D14"},
	TestID -> "ex11-year-chain-formula-children"]

VerificationTest[
	SpreadsheetTrace[file11, "Monthly Costs (1st year,\[Euro])!F6"],
	{"'Monthly Costs (1st year,\[Euro])'!F6", "+E6", {"'Monthly Costs (1st year,\[Euro])'!E6", "+D6", {"'Monthly Costs (1st year,\[Euro])'!D6", "+C6", {"'Monthly Costs (1st year,\[Euro])'!C6", "+B6", {"'Monthly Costs (1st year,\[Euro])'!B6", "3200/12*1.1"}}}}},
	TestID -> "ex11-single-dependency-chain"]

VerificationTest[
	SpreadsheetTrace[file11, "Administrative Data!G6", "TraceDuplicates" -> False],
	{"'Administrative Data'!G6", "IF(F6>0,F6-1,0)", {"'Administrative Data'!F6", "IF(E6>0,E6-1,0)", {"'Administrative Data'!E6", "IF(D6>0,D6-1,0)", {"'Administrative Data'!D6", "IF(C6>0,C6-1,0)", {"'Administrative Data'!C6", "B5", {"'Administrative Data'!B5", 30.}}}}}},
	TestID -> "ex11-repeated-references-TraceDuplicates-False"]

VerificationTest[
	SpreadsheetTrace[file11, "Monthly Costs (1st year,\[Euro])!M6", "MaxSteps" -> 5],
	{"'Monthly Costs (1st year,\[Euro])'!M6", "+L6", {"'Monthly Costs (1st year,\[Euro])'!L6", "+K6", {"'Monthly Costs (1st year,\[Euro])'!K6", "+J6", {"'Monthly Costs (1st year,\[Euro])'!J6", "+I6", {"'Monthly Costs (1st year,\[Euro])'!I6", "+H6", {"'Monthly Costs (1st year,\[Euro])'!H6", $Aborted}}}}}},
	TestID -> "ex11-MaxSteps-traces-at-most-n-cells"]


(* ::Section:: *)
(*Symbolic sub-traces*)


VerificationTest[
	Count[SpreadsheetTrace[file11, " B - Capex and Depreciation!D83", "TraceDuplicates" -> False, "SymbolicSubtraces" -> False], {_String, ___}, {0, Infinity}],
	11,
	TestID -> "ex11-shared-cell-plain-trace"]

VerificationTest[
	SpreadsheetTrace[file11, " B - Capex and Depreciation!D83", "TraceDuplicates" -> False, "SymbolicSubtraces" -> True],
	ConditionalExpression[{"' B - Capex and Depreciation'!D83", "+C83+D80/MAX(1,'Administrative Data'!D6)", {"' B - Capex and Depreciation'!C83", "+C80/'Administrative Data'!C6", {"' B - Capex and Depreciation'!C80", "+'Technical Data'!C34", {"'Technical Data'!C34", 0.}}, \[FormalS][1]}, {"' B - Capex and Depreciation'!D80", "+'Technical Data'!D34", {"'Technical Data'!D34", 0.}}, {"'Administrative Data'!D6", "IF(C6>0,C6-1,0)", \[FormalS][1]}}, \[FormalS][1] == {"'Administrative Data'!C6", "B5", {"'Administrative Data'!B5", 30.}}],
	TestID -> "ex11-shared-cell-symbolic-subtraces"]

VerificationTest[
	With[{ce = SpreadsheetTrace[file11, " B - Capex and Depreciation!E83", "TraceDuplicates" -> False, "SymbolicSubtraces" -> True]}, {Length[ce[[2]]], Count[ce[[1]] //. Cases[{ce[[2]]}, (s_) == (d_) :> s -> d, Infinity], {_String, ___}, {0, Infinity}]}],
	{2, 18},
	TestID -> "ex11-symbolic-subtraces-expand-to-plain-trace"]

VerificationTest[
	With[{t = SpreadsheetTrace[file11, "E - Bank Loan Schedule!F29", "TraceDuplicates" -> False]}, {Head[t], Count[t, {_String, ___}, {0, Infinity}]}],
	{List, 277},
	TestID -> "ex11-SymbolicSubtraces-Automatic-plain-up-to-1000-steps"]

VerificationTest[
	With[{ce = SpreadsheetTrace[file11, "F - VAT Loan Schedule!F29", "TraceDuplicates" -> False]}, {Head[ce], Cases[{ce[[2]]}, _ == {lbl_, ___} :> lbl, Infinity]}],
	{ConditionalExpression, {"'F - VAT Loan Schedule'!C29", "'F - VAT Loan Schedule'!C28", "'F - VAT Loan Schedule'!Loan_amount", "'F - VAT Loan Schedule'!Payment", "'F - VAT Loan Schedule'!Annual_rate"}},
	TestID -> "ex11-SymbolicSubtraces-Automatic-symbolic-over-1000-steps"]

VerificationTest[
	With[{ce = SpreadsheetTrace[file11, "F - VAT Loan Schedule!F29", "TraceDuplicates" -> False]}, Count[ce[[1]] //. Cases[{ce[[2]]}, (s_) == (d_) :> s -> d, Infinity], {_String, ___}, {0, Infinity}]],
	1092,
	TestID -> "ex11-SymbolicSubtraces-Automatic-expand-to-plain-trace"]

VerificationTest[
	SpreadsheetTrace[file11, "6 - Ratios!B71", "TraceDuplicates" -> False, "SymbolicSubtraces" -> True],
	{"'6 - Ratios'!B71", "+' B - Capex and Depreciation'!B5", {"' B - Capex and Depreciation'!B5", "+'Technical Data'!B14", {"'Technical Data'!B14", 320000.}}},
	TestID -> "ex11-SymbolicSubtraces-True-without-repeats-is-plain"]

VerificationTest[
	With[{ce = SpreadsheetTrace[file11, "6 - Ratios!B99", "TraceDuplicates" -> False, "SymbolicSubtraces" -> True]}, {ce[[1]], Cases[{ce[[2]]}, (s_) == {lbl_, ___} :> s -> lbl, Infinity]}],
	{{"'6 - Ratios'!B99", "+'input Leasing'!B25+'input Leasing'!B26", \[FormalS][1], {"'input Leasing'!B26", "+B25/term", \[FormalS][1], {"term", "end-start+1", {"end", 5.}, {"start", 1.}}}}, {\[FormalS][1] -> "'input Leasing'!B25"}},
	TestID -> "ex11-same-cell-with-two-labels-one-symbol"]

VerificationTest[
	SpreadsheetTrace[file11, "input Leasing!B16", "TraceDuplicates" -> False, "SymbolicSubtraces" -> True],
	ConditionalExpression[{"'input Leasing'!B16", "+B14+B26", {"'input Leasing'!B14", "term+term*Rate1", \[FormalS][1], {"Rate1", 0.045}}, {"'input Leasing'!B26", "+B25/term", {"'input Leasing'!B25", "SUM(I20:J22)", {"'input Leasing'!I20", 4200.}, {"'input Leasing'!J20", 4600.}, {"'input Leasing'!I21", 4200.}, {"'input Leasing'!J21", 4600.}, {"'input Leasing'!I22", 4200.}, {"'input Leasing'!J22", 4600.}}, \[FormalS][1]}}, \[FormalS][1] == {"term", "end-start+1", {"end", 5.}, {"start", 1.}}],
	TestID -> "ex11-shared-defined-name-keeps-its-name"]

VerificationTest[
	With[{ce = SpreadsheetTrace[file11, "6 - Ratios!C73", "TraceDuplicates" -> False]}, Module[{defs = Association[Cases[{ce[[2]]}, (s_) == (d_) :> s -> d, Infinity]], size}, size[s_\[FormalS]] := size[s] = size[defs[s]]; size[l_List] := 1 + Total[size /@ Select[Drop[l, 2], MatchQ[#1, _List | _\[FormalS]] & ]]; {Head[ce], Length[defs], Block[{$RecursionLimit = 10^5}, size[ce[[1]]]]}]],
	{ConditionalExpression, 335, 20799},
	TestID -> "ex11-long-trace-cash-flow-of-the-first-year"]

VerificationTest[
	With[{ce = SpreadsheetTrace[file11, "6 - Ratios!B83", "TraceDuplicates" -> False]}, Module[{defs = Association[Cases[{ce[[2]]}, (s_) == (d_) :> s -> d, Infinity]], size}, size[s_\[FormalS]] := size[s] = size[defs[s]]; size[l_List] := 1 + Total[size /@ Select[Drop[l, 2], MatchQ[#1, _List | _\[FormalS]] & ]]; {ce[[1,1 ;; 2]], Length[defs], DuplicateFreeQ[Values[defs][[All,1]]], Block[{$RecursionLimit = 10^5}, size[ce[[1]]]]}]],
	{{"'6 - Ratios'!B83", "IFERROR((C73)+NPV(B85,D73:AF73),)"}, 3718, True, 23581109619253964631910899355790477266627510704895650},
	TestID -> "ex11-long-trace-NPV"]

VerificationTest[
	SpreadsheetTrace[file12, "Circ!B2"],
	ConditionalExpression[\[FormalS][1], \[FormalS][1] == {"Circ!B2", "+C2+1", {"Circ!C2", "+B2*0.5", \[FormalS][1]}}],
	TestID -> "ex12-circular-reference-recursive-definition"]

VerificationTest[
	SpreadsheetTrace[file12, "Circ!B2", "SymbolicSubtraces" -> False],
	ConditionalExpression[\[FormalS][1], \[FormalS][1] == {"Circ!B2", "+C2+1", {"Circ!C2", "+B2*0.5", \[FormalS][1]}}],
	TestID -> "ex12-circular-reference-plain-trace-keeps-cycle-symbol"]


(* ::Section:: *)
(*Explicit sheets*)


VerificationTest[
	SpreadsheetTrace[file1, "Sheet1!C3"],
	{"C3", "B3-A3", {"B3", "A3^2", {"A3", 5.}}, {"A3", 5.}},
	TestID -> "ex1-ExplicitSheets-Automatic-one-sheet-starting-cell-without-sheet"]

VerificationTest[
	SpreadsheetTrace[file11, "input Leasing!B25", "ExplicitSheets" -> False],
	{"B25", "SUM(I20:J22)", {"I20", 4200.}, {"J20", 4600.}, {"I21", 4200.}, {"J21", 4600.}, {"I22", 4200.}, {"J22", 4600.}},
	TestID -> "ex11-ExplicitSheets-False-2D-range"]

VerificationTest[
	(SpreadsheetTrace[file11, #1, "ExplicitSheets" -> False] & ) /@ {"Monthly Costs (1st year,\[Euro])!B8", "Monthly Costs (1st year,\[Euro])!F13"},
	{{"B8", "+(B6+B7)*10/100", {"B6", "3200/12*1.1"}, {"B7", "+B6*15%", {"B6", "3200/12*1.1"}}}, {"'Monthly Costs (1st year,\[Euro])'!F13", "+'Monthly Revenue (1st year, \[Euro])'!F61*38+60", {"'Monthly Revenue (1st year, \[Euro])'!F61", 31.}}},
	TestID -> "ex11-ExplicitSheets-False-same-sheet-and-other-sheet-references"]

VerificationTest[
	SpreadsheetTrace[file11, "Administrative Data!D12", "ExplicitSheets" -> False][[3 ;; All,1]],
	{"D1", "B5", "D14", "C12", "D13", "D14"},
	TestID -> "ex11-ExplicitSheets-False-repeated-reference"]

VerificationTest[
	SpreadsheetTrace[file11, "6 - Ratios!B97", "ExplicitSheets" -> False],
	{"B97", "+B98+Z999", {"B98", ""}, {"Z999", ""}},
	TestID -> "ex11-ExplicitSheets-False-empty-and-out-of-range-cells"]

VerificationTest[
	SpreadsheetTrace[file11, "input Leasing!B13", "ExplicitSheets" -> False],
	{"B13", "+B12*Rate1", {"B12", "end-start+1", {"end", 5.}, {"start", 1.}}, {"Rate1", 0.045}},
	TestID -> "ex11-ExplicitSheets-False-defined-names-keep-their-name"]

VerificationTest[
	SpreadsheetTrace[file11, " B - Capex and Depreciation!D83", "TraceDuplicates" -> False, "SymbolicSubtraces" -> True, "ExplicitSheets" -> False],
	ConditionalExpression[{"' B - Capex and Depreciation'!D83", "+C83+D80/MAX(1,'Administrative Data'!D6)", {"C83", "+C80/'Administrative Data'!C6", {"C80", "+'Technical Data'!C34", {"'Technical Data'!C34", 0.}}, \[FormalS][1]}, {"D80", "+'Technical Data'!D34", {"'Technical Data'!D34", 0.}}, {"'Administrative Data'!D6", "IF(C6>0,C6-1,0)", \[FormalS][1]}}, \[FormalS][1] == {"'Administrative Data'!C6", "B5", {"B5", 30.}}],
	TestID -> "ex11-ExplicitSheets-False-symbolic-subtraces"]

VerificationTest[
	SpreadsheetTrace[file12, "Calc!B3"],
	{"Calc!B3", "SUM(D:D)", {"Calc!D1", 2.}, {"Calc!D2", 3.}, {"Calc!D3", 4.}, {"Calc!D4", ""}, {"Calc!D5", ""}, {"Calc!D6", ""}, {"Calc!D7", ""}, {"Calc!D8", ""}, {"Calc!D9", ""}},
	TestID -> "ex12-ExplicitSheets-whole-column-range"]

VerificationTest[
	SpreadsheetTrace[file12, "B5"],
	{"Calc!B5", "+'2024'!B2+'Q1'!B2", {"'2024'!B2", 100.}, {"'Q1'!B2", 10.}},
	TestID -> "ex12-ExplicitSheets-cell-without-sheet-is-on-first-sheet"]

VerificationTest[
	SpreadsheetTrace[file12, "Circ!B2"],
	ConditionalExpression[\[FormalS][1], \[FormalS][1] == {"Circ!B2", "+C2+1", {"Circ!C2", "+B2*0.5", \[FormalS][1]}}],
	TestID -> "ex12-ExplicitSheets-circular-reference"]


(* ::Section:: *)
(*Defined name scopes*)


VerificationTest[
	SpreadsheetTrace[file13, "Riepilogo!B2"],
	{"'Riepilogo'!B2", "'Gen 2025'!C5+'Feb 2025'!C5", {"'Gen 2025'!C5", "Totale*Aliquota", {"'Gen 2025'!Totale", "SUM(Vendite)", {"'Gen 2025'!B2", 100.}, {"'Gen 2025'!B3", 200.}, {"'Gen 2025'!B4", 300.}}, {"Aliquota", 0.22}}, {"'Feb 2025'!C5", "Totale*Aliquota", {"'Feb 2025'!Totale", "SUM(Vendite)", {"'Feb 2025'!B2", 110.}, {"'Feb 2025'!B3", 210.}, {"'Feb 2025'!B4", 310.}}, {"'Feb 2025'!Aliquota", 0.1}}},
	TestID -> "ex13-name-of-the-formula-sheet-else-workbook-name"]

VerificationTest[
	SpreadsheetTrace[file13, "Riepilogo!B3"],
	{"'Riepilogo'!B3", "'Gen 2025'!Totale+'Feb 2025'!Totale", {"'Gen 2025'!Totale", "SUM(Vendite)", {"'Gen 2025'!B2", 100.}, {"'Gen 2025'!B3", 200.}, {"'Gen 2025'!B4", 300.}}, {"'Feb 2025'!Totale", "SUM(Vendite)", {"'Feb 2025'!B2", 110.}, {"'Feb 2025'!B3", 210.}, {"'Feb 2025'!B4", 310.}}},
	TestID -> "ex13-Sheet!Name-references"]

VerificationTest[
	SpreadsheetTrace[file13, "Riepilogo!B4"],
	{"'Riepilogo'!B4", "Aliquota", {"Aliquota", 0.22}},
	TestID -> "ex13-workbook-name"]

VerificationTest[
	SpreadsheetTrace[file13, "Riepilogo!B5"],
	{"'Riepilogo'!B5", "'Gen 2025'!C6+'Feb 2025'!C6", {"'Gen 2025'!C6", "Base", {"'Gen 2025'!Base", 1000.}}, {"'Feb 2025'!C6", "Base", {"Base", 2000.}}},
	TestID -> "ex13-sheet-name-and-workbook-name-on-one-sheet"]

VerificationTest[
	SpreadsheetTrace[file13, "Riepilogo!B6", "SymbolicSubtraces" -> False],
	{"'Riepilogo'!B6", "'Gen 2025'!C7+'Feb 2025'!C7", {"'Gen 2025'!C7", "Totale*Sconto+Totale", {"'Gen 2025'!Totale", "SUM(Vendite)", {"'Gen 2025'!B2", 100.}, {"'Gen 2025'!B3", 200.}, {"'Gen 2025'!B4", 300.}}, {"Sconto", "B2/2", {"'Parametri'!B2", 0.22}}, {"'Gen 2025'!Totale", "SUM(Vendite)", {"'Gen 2025'!B2", 100.}, {"'Gen 2025'!B3", 200.}, {"'Gen 2025'!B4", 300.}}}, {"'Feb 2025'!C7", "Totale*Sconto+Totale", {"'Feb 2025'!Totale", "SUM(Vendite)", {"'Feb 2025'!B2", 110.}, {"'Feb 2025'!B3", 210.}, {"'Feb 2025'!B4", 310.}}, {"Sconto", "B2/2", {"'Parametri'!B2", 0.22}}, {"'Feb 2025'!Totale", "SUM(Vendite)", {"'Feb 2025'!B2", 110.}, {"'Feb 2025'!B3", 210.}, {"'Feb 2025'!B4", 310.}}}},
	TestID -> "ex13-names-used-twice-plain-trace"]

VerificationTest[
	SpreadsheetTrace[file13, "Riepilogo!B6", "SymbolicSubtraces" -> True],
	ConditionalExpression[{"'Riepilogo'!B6", "'Gen 2025'!C7+'Feb 2025'!C7", {"'Gen 2025'!C7", "Totale*Sconto+Totale", \[FormalS][1], \[FormalS][2], \[FormalS][1]}, {"'Feb 2025'!C7", "Totale*Sconto+Totale", \[FormalS][3], \[FormalS][2], \[FormalS][3]}}, \[FormalS][1] == {"'Gen 2025'!Totale", "SUM(Vendite)", {"'Gen 2025'!B2", 100.}, {"'Gen 2025'!B3", 200.}, {"'Gen 2025'!B4", 300.}} && \[FormalS][2] == {"Sconto", "B2/2", {"'Parametri'!B2", 0.22}} && \[FormalS][3] == {"'Feb 2025'!Totale", "SUM(Vendite)", {"'Feb 2025'!B2", 110.}, {"'Feb 2025'!B3", 210.}, {"'Feb 2025'!B4", 310.}}],
	TestID -> "ex13-same-name-on-two-sheets-two-symbols"]

VerificationTest[
	With[{ce = SpreadsheetTrace[file13, "Riepilogo!B6", "SymbolicSubtraces" -> True]}, ce[[1]] //. Cases[{ce[[2]]}, (s_) == (d_) :> s -> d, Infinity]],
	SpreadsheetTrace[file13, "Riepilogo!B6", "SymbolicSubtraces" -> False],
	TestID -> "ex13-symbolic-names-expand-to-plain-trace"]

VerificationTest[
	SpreadsheetTrace[file13, "Riepilogo!B7", "SymbolicSubtraces" -> True],
	ConditionalExpression[{"'Riepilogo'!B7", "'Gen 2025'!Totale+'Gen 2025'!B5", \[FormalS][1], \[FormalS][1]}, \[FormalS][1] == {"'Gen 2025'!Totale", "SUM(Vendite)", {"'Gen 2025'!B2", 100.}, {"'Gen 2025'!B3", 200.}, {"'Gen 2025'!B4", 300.}}],
	TestID -> "ex13-name-and-address-of-one-cell-one-symbol"]

VerificationTest[
	SpreadsheetTrace[file13, "Riepilogo!B6", "SymbolicSubtraces" -> True],
	ConditionalExpression[{"'Riepilogo'!B6", "'Gen 2025'!C7+'Feb 2025'!C7", {"'Gen 2025'!C7", "Totale*Sconto+Totale", \[FormalS][1], \[FormalS][2], \[FormalS][1]}, {"'Feb 2025'!C7", "Totale*Sconto+Totale", \[FormalS][3], \[FormalS][2], \[FormalS][3]}}, \[FormalS][1] == {"'Gen 2025'!Totale", "SUM(Vendite)", {"'Gen 2025'!B2", 100.}, {"'Gen 2025'!B3", 200.}, {"'Gen 2025'!B4", 300.}} && \[FormalS][2] == {"Sconto", "B2/2", {"'Parametri'!B2", 0.22}} && \[FormalS][3] == {"'Feb 2025'!Totale", "SUM(Vendite)", {"'Feb 2025'!B2", 110.}, {"'Feb 2025'!B3", 210.}, {"'Feb 2025'!B4", 310.}}],
	TestID -> "ex13-names-with-ExplicitSheets"]

VerificationTest[
	SpreadsheetTrace[Append[Import[file11, {{"Sheets", "Data", "Formulas"}}], Import[file11, "NamedRanges"]], "input Leasing!B13"],
	{"'input Leasing'!B13", "+B12*Rate1", {"'input Leasing'!B12", "end-start+1", {"end", 5.}, {"start", 1.}}, {"Rate1", 0.045}},
	TestID -> "ex11-list-form-with-NamedRanges"]

VerificationTest[
	SpreadsheetTrace[file13, "Riepilogo!B8"],
	{"'Riepilogo'!B8", "Altro!Totale+Altro!Aliquota", {"Altro!Totale", Missing["UnknownSheet"]}, {"Altro!Aliquota", Missing["UnknownSheet"]}},
	TestID -> "ex13-names-written-with-a-sheet-not-in-the-workbook"]

VerificationTest[
	SpreadsheetTrace[file13, "Riepilogo!B9"],
	{"'Riepilogo'!B9", "Esterno*2+SUM(EsternoRange)", {"Esterno", Missing["UnknownSheet"]}, {"EsternoRange", Missing["UnknownSheet"]}},
	TestID -> "ex13-names-of-cells-on-a-sheet-not-in-the-workbook"]

VerificationTest[
	SpreadsheetTrace[file13, "Riepilogo!B10"],
	{"'Riepilogo'!B10", "Parametri!Totale+Totale", {"Parametri!Totale", Missing["UnknownName"]}, {"Totale", Missing["UnknownName"]}},
	TestID -> "ex13-names-that-do-not-exist"]


Clear[SpreadsheetToolkitTests`packageFile];
End[];
