BeginPackage["ConnorGray`CellInsertionMenu`"]

Needs["GeneralUtilities`"]

GeneralUtilities`SetUsage[CreateNewCellMenu, "
CreateNewCellMenu[cellobj$] attaches a new cell menu to the cellobj$.
"]

Begin["`Private`"]

CreateNewCellMenu[
	newMenuCell_CellObject,
	eventKey : _?StringQ : None
] := Module[{
	filter,
	items,
	menuItems,
	selectedIndex = CurrentValue[newMenuCell, {TaggingRules, "SelectedIndex"}]
},
	If[CurrentValue[newMenuCell, CellStyle] =!= {"ConnorGray/CellInsertionMenu"},
		Return[$Failed, Module];
	];

	filter = First[
		NotebookImport[
			Notebook[{NotebookRead[newMenuCell]}],
			_ -> "InputText"
		],
		""
	];

	(* Prevent more than one new cell menu from being open at a time. *)
	NotebookDelete @ Flatten @ Map[
		cell |-> Cells[cell, AttachedCell -> True],
		Cells[ParentNotebook[newMenuCell], CellStyle -> "ConnorGray/CellInsertionMenu"]
	];

	items = KeySelect[$stylePreviews, StringStartsQ[filter, IgnoreCase -> True]];

	Assert[MatchQ[items, <| (_?StringQ -> _)... |>]];

	If[StringQ[eventKey],
		(* This rerendering of the attached cell was done in response to the
			user typing a key. If one of those keys was an up or down arrow,
			adjust the selected cell. *)
		Replace[ToCharacterCode[eventKey], {
			(* Enter/Return key *)
			{13} :> (
				If[IntegerQ[selectedIndex],
					(* FIXME: Test that this part exists. *)
					makeSelection[newMenuCell, Part[Keys[items], selectedIndex]];
					Return[Null, Module];
				];
			),
			(* Up Arrow *)
			{63232} :> (
				selectedIndex = Replace[selectedIndex, {
					Inherited :> Length[items],
					index_?IntegerQ :> Max[index - 1, 1]
				}];
			),
			(* Down Arrow *)
			{63233} :> (
				selectedIndex = Replace[selectedIndex, {
					Inherited :> 1,
					index_?IntegerQ :> Min[index + 1, Length[items]]
				}];
			)
		}]
	];

	selectedIndex = Replace[selectedIndex, {
		(* FIXME: What if `items` is empty? *)
		Inherited :> 1,
		index_?IntegerQ :> Clip[index, {1, Length[items]}],
		other_ :> 0
	}];

	Assert[IntegerQ[selectedIndex]];

	selectedIndex = Clip[selectedIndex, {1, Length[items]}];

	CurrentValue[newMenuCell, {TaggingRules, "SelectedIndex"}] = selectedIndex;

	(*-------------------------------------*)
	(* Compute the updated menu to display *)
	(*-------------------------------------*)

	menuItems = KeyValueMap[
		{style, preview} |-> (
			preview :> (
				makeSelection[newMenuCell, style];
			)
		),
		items
	];

	If[selectedIndex <= Length[menuItems],
		menuItems = ReplaceAt[
			menuItems,
			RuleDelayed[preview_, action_] :> (
				Framed[preview, Background -> LightBlue] :> action
			),
			selectedIndex
		];
	];

	AttachCell[
		newMenuCell,
		MakeMenuContent[menuItems],
		{Left, Bottom},
		{0, 0},
		{Left, Top},
		RemovalConditions -> {"MouseClickOutside", "EvaluatorQuit"}
	]
]

(*------------------------------------*)

makeSelection[newMenuCell_CellObject, style_?StringQ] := Module[{
	(* Get the parent notebook before we overwrite newMenuCell *)
	nb = ParentNotebook[newMenuCell]
},
	(* Create an empty cell with the selected style. *)
	NotebookWrite[newMenuCell, Cell["", style], After];
	SelectionMove[nb, Previous, CellContents]
]

(* FIXME *)
makeSelection[args___] := Throw[{"bad args", args}]

(*====================================*)

rasterizeStyle[style_?StringQ] :=
	ImageCrop @ Rasterize @ Notebook[{
		Cell[style, style],
		Cell["", "Text"]
	}]

$stylePreviews = AssociationMap[
	style |-> Image[rasterizeStyle[style], ImageSize -> All],
	{
		"Title", "Subtitle", "Subsubtitle", "Chapter", "Subchapter",
		"Section", "Subsection", "Subsubsection", "Subsubsubsection",
		"Subsubsubsubsection",
		"Item", "Subitem", "Subsubitem",
		"ItemNumbered", "SubitemNumbered",  "SubsubitemNumbered",
		"ItemParagraph", "SubitemParagraph", "SubsubitemParagraph"
	}
];

(*========================================================*)

MakeMenuContent[items0_?ListQ] := Module[{
	items = items0
},
	items = Map[
		Replace[{
			RuleDelayed[label_, action_] :> (
				RawBoxes @ TemplateBox[
					{ToBoxes[label], Hold[action]},
					"ConnorGray/CellInsertionMenuItem"
				]
			),
			other_ :> Throw["FIXME: Unsupported form for popup menu item"]
		}],
		items0
	];

	Framed[Column[items],
		Frame -> All,
		Background -> White,
		ImageSize -> {250, Automatic}
	]
]

(*========================================================*)

End[] (* End `Private` *)

EndPackage[]
