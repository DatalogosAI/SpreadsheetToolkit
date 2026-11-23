(* ::Package:: *)

(* ::Title:: *)
(*SpreadsheetToolkit: tests of the import functions*)


(* ::Text:: *)
(*SpreadsheetImport, ImportSpreadsheetSheets, ImportSpreadsheetData, ImportSpreadsheetFormulas and ImportSpreadsheetDefinedNames*)
(*of Kernel/SpreadsheetToolkit.wl, and SpreadsheetTrace on the output of SpreadsheetImport.*)
(*The example workbooks are downloaded from the Wolfram Cloud into $TemporaryDirectory, as in Tracing.wlt, so the*)
(*tests need an internet connection. Three small files written with Export (XLS, CSV, XLSX) are deleted at the end.*)
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
file4 = URLDownload["https://www.wolframcloud.com/obj/cb328fb3-3a35-4601-a7cb-611e09245574",
	FileNameJoin[{$TemporaryDirectory, "example_04.xlsx"}]];
file11 = URLDownload["https://www.wolframcloud.com/obj/2b7da83d-0149-4c42-971f-93961c7e7f90",
	FileNameJoin[{$TemporaryDirectory, "example_11.xlsx"}]];
file12 = URLDownload["https://www.wolframcloud.com/obj/12639b65-377b-4b0e-abe5-42626fe61b4e",
	FileNameJoin[{$TemporaryDirectory, "example_12.xlsx"}]];
file13 = URLDownload["https://www.wolframcloud.com/obj/4714edec-310b-445c-aa05-46a318c3cd93",
	FileNameJoin[{$TemporaryDirectory, "example_13.xlsx"}]];
examples = {file1, file4, file11, file12, file13};


(* ::Subsection:: *)
(*Files written with Export*)


(* XLS has no xl/workbook.xml, so ImportSpreadsheetDefinedNames takes the "NamedRanges" path; CSV has no names at all *)
exportDirectory = CreateDirectory[];
xlsFile = Export[FileNameJoin[{exportDirectory, "two-sheets.xls"}], {"First" -> {{1, 2}, {3, 4}}, "Second" -> {{"a"}}}, "XLS"];
csvFile = Export[FileNameJoin[{exportDirectory, "table.csv"}], {{1, 2}, {3, 4}}, "CSV"];
xlsxFile = Export[FileNameJoin[{exportDirectory, "two-sheets.xlsx"}], {"First" -> {{1, 2}, {3, 4}}, "Second" -> {{"a"}}}, "XLSX"];


(* ::Section:: *)
(*SpreadsheetImport*)


VerificationTest[
	SpreadsheetImport[file1],
	{{"Sheet1"},
		{{{10., 20., 0., 0., 0.}, {"", "", "", "", ""}, {5., 0., 0., "", ""}, {"", "", "", "", ""}, {"hello", 0., 0., "", ""},
			{"", "", "", "", ""}, {"Linear chain: E1 depends on D1, which depends on C1, which depends on A1 and B1", "", "", "", ""}}},
		{{{"", "", "A1+B1", "C1*2", "D1+A1"}, {"", "", "", "", ""}, {"", "A3^2", "B3-A3", "", ""}, {"", "", "", "", ""},
			{"", "LEN(A5)", "B5+1", "", ""}, {"", "", "", "", ""}, {"", "", "", "", ""}}},
		{}},
	TestID -> "SpreadsheetImport-example-of-ResourceDefinition"]

VerificationTest[
	(SpreadsheetImport[#] === {ImportSpreadsheetSheets[#], ImportSpreadsheetData[#], ImportSpreadsheetFormulas[#],
		ImportSpreadsheetDefinedNames[#]}) & /@ examples,
	ConstantArray[True, 5],
	TestID -> "SpreadsheetImport-is-the-four-import-functions"]

VerificationTest[
	(Most@SpreadsheetImport[#] === Import[#, {{"Sheets", "Data", "Formulas"}}]) & /@ examples,
	ConstantArray[True, 5],
	TestID -> "SpreadsheetImport-sheets-data-and-formulas-as-Import"]

VerificationTest[
	SpreadsheetImport[First[file1]] === SpreadsheetImport[file1],
	True,
	TestID -> "SpreadsheetImport-file-name-or-File"]

VerificationTest[
	SetDirectory[DirectoryName[First[file4]]];
	With[{sheets = ImportSpreadsheetSheets[FileNameTake[First[file4]]]}, ResetDirectory[]; sheets],
	{"Input", "Summary"},
	TestID -> "SpreadsheetImport-file-name-relative-to-the-directory"]

VerificationTest[
	SpreadsheetImport[xlsFile],
	{{"First", "Second"}, {{{1., 2.}, {3., 4.}}, {{"a"}}}, {{{"", ""}, {"", ""}}, {{""}}}, <|"First" -> <||>, "Second" -> <||>|>},
	TestID -> "SpreadsheetImport-XLS"]


(* ::Section:: *)
(*ImportSpreadsheetSheets*)


(* the names as in the workbook, not quoted as in formulas *)
VerificationTest[
	ImportSpreadsheetSheets[file12],
	{"Calc", "2024", "Q1", "O'Brien", "Jan", "Feb", "Mar", "Circ", "Collegamenti"},
	TestID -> "ImportSpreadsheetSheets-digits-and-apostrophe"]

VerificationTest[
	ImportSpreadsheetSheets[file11],
	{"Contents", "Administrative Data", "Technical Data", "Prices and Occupancy (2025)", "Monthly Revenue (1st year, \[Euro])",
		"Monthly Costs (1st year,\[Euro])", "Staff Costs (1st year)", "input Leasing", "Price Table", "Price List 2025",
		"2 - Operating Revenue and Costs", "3 - Income Statement", "4 - Balance Sheet", "5 - Cash Flow", "6 - Ratios", "7 - WACC",
		"checks", "A - Working Capital", " B - Capex and Depreciation", "C - Taxes", "D - Financing", "E - Bank Loan Schedule",
		"F - VAT Loan Schedule", "O ''Brien", "Extra \"Q1\" (year's end)"},
	TestID -> "ImportSpreadsheetSheets-spaces-brackets-quotes-and-euro-sign"]


(* ::Section:: *)
(*ImportSpreadsheetDefinedNames*)


VerificationTest[
	ImportSpreadsheetDefinedNames /@ {file1, file4, file12},
	{{}, {}, {}},
	TestID -> "ImportSpreadsheetDefinedNames-no-names"]

(* {name, scope, reference}: scope "" for a workbook name, else the sheet of the name *)
VerificationTest[
	ImportSpreadsheetDefinedNames[file11],
	{{"start", "", "'input Leasing'!$E$10"}, {"end", "", "'input Leasing'!$I$10"}, {"Rate1", "", "'input Leasing'!$E$12"},
		{"term", "", "'input Leasing'!$B$12"},
		{"Loan_amount", "E - Bank Loan Schedule", "'E - Bank Loan Schedule'!$C$11"},
		{"Annual_rate", "E - Bank Loan Schedule", "'E - Bank Loan Schedule'!$C$12"},
		{"Years", "E - Bank Loan Schedule", "'E - Bank Loan Schedule'!$C$13"},
		{"Payments_per_year", "E - Bank Loan Schedule", "'E - Bank Loan Schedule'!$C$14"},
		{"Grace_years", "E - Bank Loan Schedule", "'E - Bank Loan Schedule'!$C$15"},
		{"Payment", "E - Bank Loan Schedule", "'E - Bank Loan Schedule'!$C$18"},
		{"Loan_amount", "F - VAT Loan Schedule", "'F - VAT Loan Schedule'!$C$11"},
		{"Annual_rate", "F - VAT Loan Schedule", "'F - VAT Loan Schedule'!$C$12"},
		{"Years", "F - VAT Loan Schedule", "'F - VAT Loan Schedule'!$C$13"},
		{"Payments_per_year", "F - VAT Loan Schedule", "'F - VAT Loan Schedule'!$C$14"},
		{"Grace_years", "F - VAT Loan Schedule", "'F - VAT Loan Schedule'!$C$15"},
		{"Payment", "F - VAT Loan Schedule", "'F - VAT Loan Schedule'!$C$18"}},
	TestID -> "ImportSpreadsheetDefinedNames-workbook-names-and-sheet-names"]

(* the same text as a workbook name and as a sheet name (Base, Aliquota), names of a range, names on a sheet that is
not in the workbook (Altro), and the names Excel adds itself (_xlnm.Print_Area) *)
VerificationTest[
	ImportSpreadsheetDefinedNames[file13],
	{{"Aliquota", "", "Parametri!$B$2"}, {"Base", "", "Parametri!$B$4"}, {"Sconto", "", "Parametri!$B$5"},
		{"Esterno", "", "Altro!$A$1"}, {"EsternoRange", "", "Altro!$A$1:$A$3"},
		{"Vendite", "Gen 2025", "'Gen 2025'!$B$2:$B$4"}, {"Totale", "Gen 2025", "'Gen 2025'!$B$5"},
		{"Base", "Gen 2025", "Parametri!$B$3"}, {"_xlnm.Print_Area", "Gen 2025", "'Gen 2025'!$A$1:$C$8"},
		{"Vendite", "Feb 2025", "'Feb 2025'!$B$2:$B$4"}, {"Totale", "Feb 2025", "'Feb 2025'!$B$5"},
		{"Aliquota", "Feb 2025", "'Feb 2025'!$B$1"}, {"_xlnm.Print_Area", "Feb 2025", "'Feb 2025'!$A$1:$C$8"}},
	TestID -> "ImportSpreadsheetDefinedNames-scopes"]

(* without xl/workbook.xml: the "NamedRanges" of Import, by sheet, or <||> for a format without names *)
VerificationTest[
	{ImportSpreadsheetDefinedNames[xlsFile], ImportSpreadsheetDefinedNames[csvFile], ImportSpreadsheetDefinedNames[xlsxFile]},
	{<|"First" -> <||>, "Second" -> <||>|>, <||>, {}},
	TestID -> "ImportSpreadsheetDefinedNames-other-formats"]


(* ::Section:: *)
(*SpreadsheetTrace on the output of SpreadsheetImport*)


(* the list form traces as the file, names included *)
VerificationTest[
	(SpreadsheetTrace[SpreadsheetImport[#1], #2] === SpreadsheetTrace[#1, #2]) & @@@
		{{file1, "E1"}, {file4, "Summary!B3"}, {file11, "6 - Ratios!B99"}, {file11, "input Leasing!B13"}, {file12, "Calc!B3"},
			{file12, "Circ!B2"}, {file13, "Riepilogo!B6"}},
	ConstantArray[True, 7],
	TestID -> "SpreadsheetTrace-of-SpreadsheetImport-as-of-the-file"]

VerificationTest[
	SpreadsheetTrace[SpreadsheetImport[xlsFile], "Second!A1"],
	{"Second!A1", "a"},
	TestID -> "SpreadsheetTrace-of-SpreadsheetImport-XLS"]


DeleteDirectory[exportDirectory, DeleteContents -> True];
Clear[SpreadsheetToolkitTests`packageFile];
End[];
