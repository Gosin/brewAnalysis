Clear[BrewPackageGraph];
BrewPackageGraph[filePath_String] := Module[
    {
        (* {0, Global} is a dummy head to represent global environment. *)
        dependencyChain = {{0, Global}},
        lines,
        result
    },

    lines = StringSplit[Import[filePath], "\n"];
    result = Reap[
        Function[{line},
            If [line === "",
                (* Reset dependency chain. *)
                dependencyChain = {{0, Global}}
                ,
                With[{packName = StringCases[line, (WordCharacter|"-"|"_")..] // First},
                    (* Collect vertices. *)
                    Sow[packName, Vertex];
                    With[{indentation = StringPosition[line, packName][[1, 1]]},
                        With[{dependencyPos = FirstPosition[dependencyChain, {indentation, _}]},
                            dependencyChain = If [MissingQ[dependencyPos],
                                (* Add package to the dependency chain. *)
                                Append[dependencyChain, {indentation, packName}]
                                ,
                                ReplacePart[
                                    (* Go back to upper dependency, drop the rest dependencies. *)
                                    Take[dependencyChain, First[dependencyPos]],
                                    (* Replace same level dependency with new package. *)
                                    First[dependencyPos] -> {indentation, packName}
                                ]
                            ];
                            (* Collect dependency edge. *)
                            Sow[
                                Last[SelectFirst[Reverse[dependencyChain], First[#] < indentation &]] -> packName,
                                Edge
                            ]
                        ]
                    ]
                ]
            ]
        ]  /@ lines
    ];

    Graph[
        (* Vertices list. *)
        DeleteDuplicates[result[[2, 1]]],
        (* Edges list. *)
        DeleteDuplicates[DeleteCases[result[[2, 2]], Global -> _]],
        VertexLabels -> All
    ]
]

depsFile = FileNameJoin[{$HomeDirectory, "Garage", "brewAnalysis", "brewPackageInfo.txt"}];
graph = BrewPackageGraph[depsFile];