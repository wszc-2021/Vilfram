(*(* ::Package:: *)

(* ::Input:: *)
(*ConnorGray`Vilfram`EnableVilfram[EvaluationNotebook[]]*)


(* ::Input:: *)
(*Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules,"Vilfram"}]]*)
*)

Remove[test, processKeyDown, $VilframCommands, EnableVilfram, DisableVilfram,
     $RetainKeyCommandSequence];

BeginPackage["Vilfram`"];

Needs["GeneralUtilities`"]


GeneralUtilities`SetUsage[setVilframOptions, "
setVilframOptions[]: export for debugging
"];

GeneralUtilities`SetUsage[setTiming, "
setTiming[] set the global variable $timing
"];

GeneralUtilities`SetUsage[$VilframCommands, "
$VilframCommands contains the recognized command sequences and actions to be executed.
"];

GeneralUtilities`SetUsage[EnableVilfram, "
EnableVilfram[] enables Vilfram for the current and future front end sessions.

EnableVilfram[$FrontEndSession] enables Vilfram for the current front end session.

EnableVilfram[$FrontEndSession] is called automatically at the beginning of
every front end session if the ConnorGray/Vilfram paclet is installed and enabled.
"];

(* EnableVilfram[nbobj$] enables Vilfram keyboard behavior in the notebook represented by the notebook object nbobj$. *)

GeneralUtilities`SetUsage[DisableVilfram, "
DisableVilfram[] disables Vilfram for the current and future front end sessions.

DisableVilfram[$FrontEndSession] disables Vilfram for the current front end session.

DisableVilfram uninstalls the global event handlers used by Vilfram, and uses
PacletDisable to prevent the ConnorGray/Vilfram paclet from automatically
loading in future front end sessions.
"];

GeneralUtilities`SetUsage[$RetainKeyCommandSequence, "
RetainKeySequence is a special value that, when returned from a Vilfram command handler, indicates
that the current key sequence should not be reset.

This is intended to be used by Vilfram command that is 'sticky'.
"];


Begin["`Private`"];

(* Define recognized commands and their actions *)

$VilframCommands =
    <|
        (* ------------------------------- normal mode ------------------------------ *)

        "Normal" :>
            {
                {"i"} :> setState[EvaluationNotebook[], "Mode" -> "Insert" ] ,
                {"a"} :> setState[EvaluationNotebook[], "Mode" -> "Insert" ] ,
                {"A"} :> (
                        FrontEndTokenExecute["MoveLineEnd"];
                        setState[EvaluationNotebook[], "Mode" -> "Insert" ]
                    ) ,
                {"I"} :>
                    (
                    FrontEndTokenExecute["MoveLineBeginning"];
                    setState[EvaluationNotebook[], "Mode" -> "Insert" ]
                    )
                ,
                {n___?DigitQ, "j"} :> Do[FrontEndTokenExecute["MoveNextLine"], If[n === "", 1, ToExpression[n]]]
                ,

                {n___?DigitQ, "J"} :> Do[FrontEndTokenExecute["MoveNextCell"], If[n === "", 1, ToExpression[n]]]
                ,

                {n___?DigitQ, "k"} :> Do[FrontEndTokenExecute["MovePreviousLine"], If[n === "", 1, ToExpression[n]]]
                ,

                {n___?DigitQ, "K"} :> Do[FrontEndTokenExecute["MovePreviousCell"], If[n === "", 1, ToExpression[n]]]
                ,

                {n___?DigitQ, "h"} :> Do[FrontEndTokenExecute["MovePrevious"], If[n === "", 1, ToExpression[n]]]
                ,

                {n___?DigitQ, "l"} :> Do[FrontEndTokenExecute["MoveNext"], If[n === "", 1, ToExpression[n]]]
                ,

                (* NaturalWord: word, Word: WORD *)
                {n___?DigitQ, "b"} :> Do[FrontEndTokenExecute["MovePreviousNaturalWord", If[n === "", 1, ToExpression[n]]]
                    ]
                ,

                {n___?DigitQ, "B"} :> Do[FrontEndTokenExecute["MovePreviousWord"], If[n === "", 1, ToExpression[n]]]
                ,

                {n___?DigitQ, "e"} :> Do[FrontEndTokenExecute["MoveNextNaturalWord"], If[n === "", 1, ToExpression[n]]]
                ,

                {n___?DigitQ, "E"} :> Do[FrontEndTokenExecute["MoveNextWord"], If[n === "", 1, ToExpression[n]]]
                ,

                {n___?DigitQ, "w"} :>
                    Do[ 
                        FrontEndTokenExecute["MoveNextNaturalWord"];
                        FrontEndTokenExecute["MoveNext"],
                        If[n === "", 1, ToExpression[n]]
                      ]
                ,

                {n___?DigitQ, "W"} :>
                    Do[
                        FrontEndTokenExecute["MovePreviousNaturalWord" ];
                        FrontEndTokenExecute["MoveNext"],
                        If[n === "", 1, ToExpression[n]]
                      ]
                ,

                {"^"} :> FrontEndTokenExecute["MoveLineBeginning"]
                ,

                {"$"} :> FrontEndTokenExecute["MoveLineEnd"]
                ,

                {"G"} :> SelectionMove[EvaluationNotebook[], After, Notebook
                    ]
                ,

                {"g", "g"} :> SelectionMove[EvaluationNotebook[], Before,
                     Notebook]
                ,

                {"u"} :> FrontEndTokenExecute["Undo"]
                ,

                {n___, "x"} :> Do[FrontEndTokenExecute["DeleteNext"], If[n === "", 1, ToExpression[n]]]
                ,

                {n___, "X"} :> Do[FrontEndTokenExecute["DeletePrevious"], If[n === "", 1, ToExpression[n]]]
                ,

                {"y", n___?DigitQ, "w"} :> 
                (
                   Do[FrontEndTokenExecute["SelectNextWord"], If[n === "", 1, ToExpression[n]]]    
                   FrontEndTokenExecute["Copy"]
                ), 

                {"y", n___?DigitQ, "W"} :> 
                (
                   Do[FrontEndTokenExecute["SelectPreviousWord"], If[n === "", 1, ToExpression[n]]]    
                   FrontEndTokenExecute["Copy"]
                ), 

                {"y", n___?DigitQ, "j"} :> 
                (
                    FrontEndTokenExecute["MoveLineBeginning"];
                    Do[FrontEndTokenExecute["SelectNextLine"], 
                        If[n === "", 1, ToExpression[n]]];
                    FrontEndTokenExecute["Copy"]
                ),

                {"y", n___?DigitQ, "k"} :> 
                (
                    FrontEndTokenExecute["MoveLineBeginning"];
                    Do[FrontEndTokenExecute["SelectPreviousLine"], 
                        If[n === "", 1, ToExpression[n]]];
                    FrontEndTokenExecute["Copy"]
                ),
                
                {"y", n___?DigitQ, "c"} :> 
                (
                    SelectionMove[EvaluationNotebook[], All, Cell];
                    Do[FrontEndTokenExecute["SelectPreviousLine"], 
                        If[n === "", 1, ToExpression[n] - 1]];
                    FrontEndTokenExecute["Copy"]
                ),

                {"y", n___?DigitQ, "C"} :> 
                (
                    SelectionMove[EvaluationNotebook[], All, Cell];
                    Do[FrontEndTokenExecute["SelectPreviousLine"], 
                        If[n === "", 1, ToExpression[n] - 1]];
                    FrontEndTokenExecute["Copy"]
                ),

                {"p"} :> FrontEndTokenExecute["Paste"]
                ,

                {"P"} :> FrontEndTokenExecute["PastePrevious"]
                ,

                (* delete(cut) a line *)
                {"d", n___?DigitQ, "l"} :> 
                (
                    FrontEndTokenExecute["MoveLineBeginning"];
                    Do[FrontEndTokenExecute["SelectNextLine"], 
                        If[n === "", 1, ToExpression[n]]];
                    FrontEndTokenExecute["Cut"]
                ),

                (* delete word/Word *)
                {"d", n___?DigitQ, "w"} :>
                    Do[FrontEndTokenExecute["DeleteNextNaturalWord"];
                       (* FrontEndTokenExecute["DeleteNext"],  *)
                       If[n === "", 1, ToExpression[n]]
                    ],

                {"d", "i", "w"} :> FrontEndExecute[
                    {
                        FrontEndToken["DeleteNextNaturalWord" ],
                        FrontEndToken["DeletePreviousNaturalWord"]
                    }]
                ,

                {"d", "a", "w"} :> 
                         FrontEndExecute[
                            {
                                FrontEndToken["DeleteNextNaturalWord" ], 
                                FrontEndToken["DeleteNext"], 
                                FrontEndToken["DeletePreviousNaturalWord"]
                            }
                            ]
                ,

                {"Backspace"} :> FrontEndTokenExecute["DeletePrevious"]
                ,

                {"Delete"} :> Do[Identity[1];]
                ,

                {"d", "b"} :> FrontEndTokenExecute["DeletePreviousNaturalWord"]
                ,

                {"d", "B"} :> FrontEndTokenExecute["DeletePreviousWord"]
                ,

                {"d", "e"} :> FrontEndTokenExecute["DeleteNextNaturalWord"]
                ,

                {"d", "E"} :> FrontEndTokenExecute["DeleteWord"]
                ,

                {"d", "^"} :> FrontEndTokenExecute["DeleteLineBeginning"]
                ,

                {"d", "$"} :> FrontEndTokenExecute["DeleteLineEnd"]
                ,

                {"c", "l"} :> (FrontEndTokenExecute["DeleteNext"];setState[EvaluationNotebook[], "Mode"->"Insert"])
                ,

                {"c", "w"} :> (FrontEndTokenExecute["DeleteNextNaturalWord"];setState[EvaluationNotebook[], "Mode"->"Insert"])
                ,

                {"c", "b"} :> (FrontEndTokenExecute["DeletePreviousNaturalWord"];setState[EvaluationNotebook[], "Mode"->"Insert"])
                ,

                {"c", "B"} :> (FrontEndTokenExecute["DeletePreviousWord"];setState[EvaluationNotebook[], "Mode"->"Insert"])
                ,

                {"c", "e"} :> (FrontEndTokenExecute["DeleteNextNaturalWord"];setState[EvaluationNotebook[], "Mode"->"Insert"])
                ,

                {"c", "E"} :> (FrontEndTokenExecute["DeleteWord"];setState[EvaluationNotebook[], "Mode"->"Insert"])
                ,

                {"c", "^"} :> (FrontEndTokenExecute["DeleteLineBeginning"];setState[EvaluationNotebook[], "Mode"->"Insert"])
                ,

                {"c", "$"} :> (FrontEndTokenExecute["DeleteLineEnd"];setState[EvaluationNotebook[], "Mode"->"Insert"])
                ,

                {"c", "i", "w"} :> 
                (
                    FrontEndExecute[{FrontEndToken["DeleteNextNaturalWord" ], FrontEndToken["DeletePreviousNaturalWord"]}];
                    setState[EvaluationNotebook[], "Mode" -> "Insert"]
                ),

                {"c", "a", "w"} :> 
                ( 
                    FrontEndExecute[
                    {
                        FrontEndToken["DeleteNextNaturalWord" ], 
                        FrontEndToken["DeleteNext"], 
                        FrontEndToken["DeletePreviousNaturalWord"]
                    }
                    ];
                    setState[EvaluationNotebook[], "Mode" -> "Insert"]
                )
                ,
                (* cil *)
                {"r"} :> (
                    FrontEndTokenExecute["DeleteNext"];
                    setState[EvaluationNotebook[], "Mode" -> "Insert", False];
                    $fromR = True;
                ),

                (* new cell *)
                {"o"} :>
                (
                    SelectionMove[EvaluationNotebook[], Next, Cell];
                    NotebookWrite[EvaluationNotebook[], ""];
                    (* FrontEndTokenExecute["MoveNextLine"]; *)
                    setState[EvaluationNotebook[], "Mode" -> "Insert"]
                )
                ,
                {"O"} :>
                (
                    SelectionMove[EvaluationNotebook[], Previous, Cell];
                    NotebookWrite[EvaluationNotebook[], ""];
                    setState[EvaluationNotebook[], "Mode" -> "Insert"]
                )
                ,
                (* delete(cut) cell *)
                {"d", "p", "c"} :>
                (
                    SelectionMove[EvaluationNotebook[], Previous,Cell];
                    FrontEndTokenExecute["Cut"]
                )
                ,

                {"d", "n", "c"} :>
                (
                    SelectionMove[EvaluationNotebook[], Next, Cell];
                    FrontEndTokenExecute["Cut"]
                )
                ,

                {"d", "c"} :>
                (
                    SelectionMove[EvaluationNotebook[], All, Cell ];
                    FrontEndTokenExecute["Cut"]
                )
                ,

                {"d", "d"} :>
                (
                    SelectionMove[EvaluationNotebook[], All, CellContents];
                    FrontEndTokenExecute["Cut"]
                )
                ,
                
                (* split cell *)
                {"\\", "s"} :> FrontEndTokenExecute["CellSplit"]
                ,

                {"\\", "j", "b"} :> 
                (
                    SelectionMove[EvaluationNotebook[], All, Cell];
                    FrontEndExecute[{FrontEndToken["SelectNextLine"], FrontEndToken["CellMerge"]}];
                )
                ,

                {"\\", "j", "a"} :> 
                (
                    SelectionMove[EvaluationNotebook[], All, Cell];
                    FrontEndExecute[{FrontEndToken["SelectPreviousLine"], FrontEndToken["CellMerge"]}];
                ),

                (* cell group/ungroup *)
                {"["} :> FrontEndTokenExecute["CellGroup"]
                ,

                {"]"} :> FrontEndTokenExecute["CellUnGroup"]
                ,

                {"z", "a"} :> FrontEndTokenExecute["OpenCloseGroup"]
                ,

                (* insert line *)
                {"L"} :> FrontEndExecute[
                    {
                        FrontEndToken["MoveLineEnd"], 
                        FrontEndToken["InsertSoftReturn"]
                    }]
                ,

                (* mouse click *)
                {"q"} :> FrontEndExecute[FrontEnd`SimulateMouseClick[
                    EvaluationNotebook[], MousePosition[]]]
                ,
                (* currentMode switch *)
                {"/"} :>
                (
                    setState[EvaluationNotebook[], "Mode" -> "Search"];
                )
                ,

                {":"} :>
                (
                    setState[EvaluationNotebook[], "Mode" -> "Command"];
                )
                ,

                {"v"} :>
                (
                    setState[EvaluationNotebook[], "Mode" -> "Visual"];
                ),

                {"V"} :> 
                (
                    setState[EvaluationNotebook[], "Mode" -> "VisualCell"];
                    SelectionMove[EvaluationNotebook[], All, Cell];
                )

            }
        ,

        (* ------------------------------- search mode ------------------------------ *)
        (* TODO *)
        "Search" :>
            {
                {"n"} :> FrontEndTokenExecute["FindNextMatch"]
                ,
                {"N"} :> FrontEndTokenExecute["FindPreviousMatch"]
            }
        ,

        (* ---------------------------- command-line mode --------------------------- *)
        "Command" :>
            {
                {"f", "Return"} :> FrontEndExecute[FrontEnd`Value[FEPrivate`NotebookToggleFullScreen[]]]
                ,

                {"n", "Return"} :> FrontEndTokenExecute["New"]
                ,

                {"a", "Return"} :> FrontEndTokenExecute["EvaluatorAbort"]
                ,

                {"w", "Return"} :> FrontEndTokenExecute["Save"]
                ,

                {"q", "Return"} :> FrontEndTokenExecute["Close"]
                ,

                {"w", "q", "Return"} :>
                (
                    FrontEndTokenExecute["Save"];
                    FrontEndTokenExecute["Close"]
                )
            },

        (* ------------------------------- visual mode ------------------------------ *)
        "VisualCell" :> 
        {
            {n___?DigitQ, "k"} :> Do[FrontEndTokenExecute["SelectPreviousLine"], If[n === "", 1, ToExpression[n]]],

            {n___?DigitQ, "j"} :> Do[FrontEndTokenExecute["SelectNextLine"], If[n === "", 1, ToExpression[n]]],

            {___, "Return"} :> setState[EvaluationNotebook[], "KeySequence" -> {}],
            
            {"m"} :> FrontEndTokenExecute["CellMerge"],

            {___, "D"} :> 
            (
                FrontEndTokenExecute["Clear"];
                setState[EvaluationNotebook[], "Mode" -> "Normal"];
            ),

            {___, "d"} :> 
            (
                FrontEndTokenExecute["Cut"];
                setState[EvaluationNotebook[], "Mode" -> "Normal"];
            )
        },

        "Visual" :>
            {
                {___, "h"} :> FrontEndTokenExecute["SelectPrevious"],

                {___, "l"} :> FrontEndTokenExecute["SelectNext"],

                {___, "j"} :> FrontEndTokenExecute["SelectNextLine"],

                {___, "k"} :> FrontEndTokenExecute["SelectPreviousLine"],

                {___, "e"} :> FrontEndTokenExecute["SelectNextWord"],

                {___, "b"} :>
                (
                    FrontEndTokenExecute["SelectPreviousWord"];
                    FrontEndTokenExecute["SelectLineEnd"];
                ),

                {___, "^"} :> FrontEndTokenExecute["SelectLineBeginning"];

                {___, "i"} :> FrontEndTokenExecute["ExpandSelection"],

                {___, "y"} :> FrontEndTokenExecute["Copy"] ,

                {___, "d"} :> 
                (
                    FrontEndTokenExecute["Cut"];
                    setState[EvaluationNotebook[], "Mode" -> "Normal"]; 
                ),

                {___, "D"} :> 
                (
                    FrontEndTokenExecute["Clear"];
                    setState[EvaluationNotebook[], "Mode" -> "Normal"];
                ),

                {___, "Return"} :> FrontEndTokenExecute["ExpandSelection"]
                (* ,
                
                {"d", Repeated["i"]} :> FrontEndTokenExecute["ExpandSelection"]
                ,

                {"d", Repeated["i"], "Return"} :> NotebookDelete[EvaluationNotebook[]] *)

                }
         
    |>;


(* Enable Vilfram in the specified notebook or globally *)

EnableVilfram[] :=
    (
        EnableVilfram[$FrontEndSession]; (* PacletEnable["ConnorGray/Vilfram"] *)
    );

EnableVilfram[obj:$FrontEndSession] :=
    setVilframOptions[obj];



(* TODO implement passdown logic for key events *)
passQ := Module[
    {
        nb=EvaluationNotebook[], 
        key = CurrentValue["EventKey"], 
        keySequence, mode, time, modeQ, keyQ, timeQ
    },

    keySequence = getState[nb, "KeySequence"];  
    keyQ = key =!= "Escape";

    mode = getState[nb, "Mode"];
    modeQ = mode === "Insert"; 

    (* time interval *)
    preTime = getState[nb, "Time"];
    currentTime = AbsoluteTime[];
    time = currentTime - preTime;
    timeQ = time <= $timing;
    
    modeQ && keyQ
] 




setVilframOptions[obj : $FrontEndSession | _NotebookObject] :=
    
    (
        setState[EvaluationNotebook[], "Mode" -> "Normal"]; 
        setState[EvaluationNotebook[], "KeySequence" -> {}];
        setState[EvaluationNotebook[], "Time" -> Inherited];
        SetOptions[obj, 
        {
            NotebookEventActions -> {
                PassEventsDown :> ((getState[EvaluationNotebook[], "Mode"] === "Insert" && CurrentValue["EventKey"] =!= "Escape") || CurrentValue["EventKey"] === "Delete"), 
                                "KeyDown" :> processKeyDown[EvaluationNotebook[], CurrentValue["EventKey"]]}
                                }
                                ];
    )

(* Disable Vilfram in the specified notebook or globally *)

DisableVilfram[] :=
    (
        DisableVilfram[$FrontEndSession];
        PacletDisable["ConnorGray/Vilfram"]
    );

DisableVilfram[$FrontEndSession] :=
    SetOptions[$FrontEndSession, {NotebookEventActions -> Inherited, 
        WindowStatusArea -> Inherited}];

execute[nb_, key_, keySequence_, currentMode_] :=
    With[
        {setKeySequence = ks |-> setState[nb, "KeySequence" -> ks]} ,
        (* no keymap *)
        historyKeySequenceName = "History" <> currentMode <> "KeySequence" ;

        keySequenceNew = Replace[
            keySequence, {Inherited -> {key},
            keys: {___?StringQ} :> Append[keys, key],
            other_ :> Throw["ERROR: Unrecognized Vilfram KeySequence stored: ", InputForm[other]]}];

        result = Replace[
            keySequenceNew,
            Append[$VilframCommands[currentMode], _ :> Missing["NoCommandKeySequenceMatches"]]];

        Replace[
            result ,
            {
                (* not matched *)
                Missing["NoCommandKeySequenceMatches"] | $RetainKeyCommandSequence :> setState[nb, "KeySequence" -> keySequenceNew ] ,
                (* matched *)
                _ :> (
                        setState[nb, historyKeySequenceName -> keySequenceNew, False];
                        setState[nb, "KeySequence" -> {}];
                        Switch[currentMode, 
                            "Command", setState[nb, "Mode" -> "Normal"]
                            ]
                    )
            }
        ];
        keySequenceNew
    ]

processNormalMode[nb_, key_?StringQ, keySequence_, time_] :=
    (
        Which[
            key === "Return" && Length[keySequence] === 0 , FrontEndTokenExecute["Linebreak"];Return[Null], 
            (key === "Escape" || key === "Return") && Length[keySequence] > 0 ,  setState[EvaluationNotebook[], "KeySequence" -> {}, True]; Return[Null]
            ];

        If[TrueQ[$fromInsert], FrontEndTokenExecute["DeletePrevious"]; $fromInsert = False];

        execute[nb, key, keySequence, "Normal"];
    )

processCommandMode[nb_, key_?StringQ, keySequence_, time_] :=
    execute[nb, key, keySequence, "Command"];

processVisualMode[nb_, key_?StringQ, keySequence_, time_] :=

    execute[nb, key, keySequence, "Visual"];

processSerachMode[nb_, key_?StringQ, keySequence_, time_] :=
(
    If[key === "Return",
        NotebookFind[nb, keySequencej];
        setState[nb, "Mode" -> "Normal"],
        setState[nb, "KeySequence" -> Append[keySequence,
             key]];
    ];
)

processKeyJ[nb_, key_, keySequence_, time_] :=
    (
        preKey = Quiet[Last[keySequence]];
        If[preKey =!= "j",
            keySequenceNew = {"j"};
            setState[nb, "KeySequence" -> keySequenceNew, True];
            Return[Null]
        ];
        
        If[time <= $timing,
            $fromInsert = True;
            FrontEndTokenExecute["DeletePrevious"]; (* delete previous j *)
            setState[nb, "Mode" -> "Normal"];
            setState[nb, "KeySequence" -> {}];
            Return[Null]
        ];
    )

processInsertMode[nb_, key_?StringQ, keySequence_, time_] :=
(
    (* from r*)
    If[TrueQ[$fromR], $fromR = False;setState[nb, "Mode" -> "Normal", True]; Return[Null]];

    Switch[key,
        "Escape",
            setState[nb, "KeySequence" -> {}, False];
            setState[nb, "Mode" -> "Normal"];
            Return[Null],
        "j",
            processKeyJ[nb, key, keySequence, time],
        _,
            setState[nb, "KeySequence" -> {key}, True]
    ]
)

processKeyDown[nb_NotebookObject, key_?StringQ] :=
    Module[{currentMode, keySequence, preTime, currentTime, time},

            (* Get the current mode and ensure it is initialized *)
            currentMode = getState[nb, "Mode"];
            If[currentMode === Inherited,
                setMode["Insert"];
                currentMode = "Insert"
            ];
            
            (* Get the current key sequence and ensure it is initialized *)
            keySequence = getState[nb, "KeySequence"];
            If[keySequence === Inherited,
                setKeySequence[{}];
                keySequence = {}
            ];
            
            (* Calculate the time interval since last key press *)
            preTime = getState[nb, "Time"];
            currentTime = AbsoluteTime[];
            If[preTime === Inherited,
                preTime = currentTime
            ];
            setState[nb, "Time" -> currentTime, False];
            time = currentTime - preTime;
            
            (* Handle the "Escape" key to reset mode and key sequence *)
            If[key === "Escape",
                setState[nb, "KeySequence" -> {}, True];
                setState[nb, "Mode" -> "Normal"];
                Return[Null]
            ];
            
            (* Process the key event based on the current mode *)
            Switch[currentMode,
                "Normal",
                    (
                        processNormalMode[nb, key, keySequence, time];
                    ),
                "Insert",
                    (
                        processInsertMode[nb, key, keySequence, time];
                    ),
                "Operator-Pending",
                    (
                        processOperatorPendingMode[nb, key, keySequence, time];
                    ),
                "Search",
                    (
                        processSerachMode[nb, key, keySequence, time];
                    ),
                "Command",
                    (
                        processCommandMode[nb, key, keySequence, time];
                    ),
                "Visual",
                    (
                        processVisualMode[nb, key, keySequence, time];
                    ),
                "VisualCell",
                    (
                        execute[nb, key, keySequence, "VisualCell"];
                    ),
                _,
                    (* Throw an error for unknown modes *)
                    Throw["ERROR: Unknown Vilfram Mode: ", ToString[currentMode, InputForm]]
            ];
            setState[nb, "HistoryMode" -> currentMode, False]
        ]

(* Update the status bar to reflect the current mode and key sequence *)
updateStatusBar[nb : $FrontEndSession | _NotebookObject] :=
    Module[
        {currentMode, keySequence, key} ,

        (* Get the current mode *)
        currentMode = getState[nb, "Mode"];
        
        (* Get the current key sequence *)
        keySequence =
            If[ currentMode === "Search", 
                getState[nb, "SearchKeySequence"] ,
                getState[nb, "KeySequence"]] /. Inherited -> {};
        
        (* Set the window status area *)
        SetOptions[nb,
            WindowStatusArea -> 
                StringJoin["Mode:", ToString[currentMode, InputForm], " | Key Sequence:", ToString[keySequence, InputForm]]]
    ]


getState[nb_NotebookObject, key_?StringQ] :=
    CurrentValue[$FrontEndSession, {PrivateFrontEndOptions, "InterfaceSettings",
         "ConnorGray/Vilfram", nb, key}];

setState[nb_NotebookObject, key_?StringQ -> value_, update_:True] :=
(
        CurrentValue[$FrontEndSession, 
        {PrivateFrontEndOptions, "InterfaceSettings", "ConnorGray/Vilfram", nb, key}] = value;
        If[update, updateStatusBar[nb]]
);

(* Set the time interval between key events in seconds *)
$timing = 0.5;
setTiming[time_]:=  ($timing = time);

End[];

EndPackage[];

