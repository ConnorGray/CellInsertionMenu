BeginPackage["ConnorGray`CellInsertionMenu`"]

$CellInsertionMenuWidth = 300

Needs["GeneralUtilities`"]

GeneralUtilities`SetUsage[UpdateCellInsertionMenu, "
UpdateCellInsertionMenu[cellobj$] updates the new cell menu attached to the cellobj$, creating it if necessary.
"]

Begin["`Private`"]

UpdateCellInsertionMenu[
	newMenuCell_CellObject,
	eventKey : _?StringQ : None
] := Module[{
	filter,
	items,
	menuItems,
	visibleItems,
	selectedIndex = CurrentValue[newMenuCell, {TaggingRules, "SelectedIndex"}],
	attachedMenuCell
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

	visibleItems = Map[
		StringStartsQ[filter, IgnoreCase -> True],
		Keys[$stylePreviews]
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
			(* Escape key *)
			{27} :> (
				NotebookDelete[EvaluationCell[]];
				Return[Null, Module];
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
	CurrentValue[newMenuCell, {TaggingRules, "VisibleItems"}] = visibleItems;

	(*-------------------------------------*)
	(* Compute the updated menu to display *)
	(*-------------------------------------*)

	Replace[Cells[newMenuCell, AttachedCell -> True], {
		{} :> Null,
		(* TODO: What if this cell doesn't exist? *)
		(* An attached menu cell already exists, so don't try to attach it again. *)
		{cell_CellObject} :> (
			Return[Null, Module];
		),
		other_ :> Throw["FIXME: Unexpected Cells[..] result in CellInsertionMenu"]
	}];

	menuItems = KeyValueMap[
		{style, preview} |-> (
			preview :> (
				makeSelection[newMenuCell, style];
			)
		),
		items
	];

	AttachCell[
		newMenuCell,
		MakeCompletionMenuContent[menuItems],
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

rasterizeStyle[style_?StringQ] := Module[{
	image
},
	image = Row[{
		Replace[CurrentValue[{StyleDefinitions, style, CellDingbat}], {
			None -> Nothing,
			boxes_ :> Splice @ {
				(* Wrap StyleBox[_, style] so that the cell dingbat is rendered
					as if it was inheriting styles from the parent cell. *)
				RawBoxes[StyleBox[boxes, style]],
				Spacer[
					AbsoluteCurrentValue[{
						StyleDefinitions,
						style,
						CellDingbatMargin
					}]
				]
			}
		}],
		RawBoxes @ Cell[style, style]
	}];

	leftMargin = Replace[CurrentValue[{StyleDefinitions, style, CellMargins}], {
		constant_?NumberQ :> constant,
		{{left_?NumberQ, _}, {_, _}} :> left,
		other_ :> 10
	}];

	(*
		Subtract 27, which is the left margin of "Title" cells, and is the
		smallest left margin of all of the common built-in cell styles.

		Prevent this from being less than 10 to avoid poor asthetics of having
		one of these preview touching the left edge of the menu.
	*)
	leftMargin = Max[leftMargin - 27, 10];

	Pane[image, ImageMargins -> {{leftMargin, 0}, {0, 0}}]
]

$stylePreviews = AssociationMap[
	style |-> rasterizeStyle[style],
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

MakeCompletionMenuContent[items0_?ListQ] := Module[{
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

	items = MapIndexed[
		{item, pos} |-> With[{
			index = pos[[1]]
		},
			PaneSelector[
				{
					True -> Framed[
						item,
						Background -> Dynamic[
							If[
								CurrentValue[
									ParentCell[EvaluationCell[]],
									{TaggingRules, "SelectedIndex"}
								] === index
								,
								Lighter[Blue]
								,
								Automatic
							]
						],
						ImageSize -> Full,
						ImageMargins -> 0
					],
					False -> ""
				},
				Dynamic[
					FEPrivate`Part[
						CurrentValue[
							ParentCell[EvaluationCell[]],
							{TaggingRules, "VisibleItems"}
						],
						index
					]
				],
				ImageSize -> Automatic
			]
		],
		items
	];

	Pane[
		Column[items,
			Spacings -> 0
		],
		Background -> White,
		ImageSize -> {$CellInsertionMenuWidth, Automatic}
	]
]

(*========================================================*)

End[] (* End `Private` *)

EndPackage[]
