:- initialization(main).
main :-
    [stack],
    corpus_loader:load_all_testsets,
    config:param(suppression_metric_name, SName),
    format("suppression_metric_name = ~w~n", [SName]),
    findall(C, corpus_loader:corpus_constraint(C), Cs),
    length(Cs, N),
    format("corpus N = ~w~n", [N]),
    % suppression value distribution
    findall(V, ( member(C, Cs), drl_core:get_raw_suppression(C, V) ), Vs),
    include([X]>>(X==unknown), Vs, Unk), length(Unk, NUnk),
    include([X]>>(number(X)), Vs, Nums), length(Nums, NNum),
    format("suppression: known(number)=~w  unknown=~w~n", [NNum, NUnk]),
    ( Nums \= [] ->
        min_list(Nums, Mn), max_list(Nums, Mx), sum_list(Nums, Sm), Mean is Sm/NNum,
        format("suppression numeric: min=~4f max=~4f mean=~4f~n", [Mn, Mx, Mean]),
        include([X]>>(X >= 0.60), Nums, HiSnare), length(HiSnare, NHiSnare),
        include([X]>>(X >= 0.40), Nums, HiTR), length(HiTR, NHiTR),
        format("suppression >= snare_floor(0.60): ~w   >= TR_floor(0.40): ~w~n", [NHiSnare, NHiTR])
    ; true ),
    nl,
    % constraint-level naturalized count
    findall(C, ( member(C, Cs), drl_core:dr_type(C, naturalized) ), NatC),
    length(NatC, NNatC),
    format("constraint-level naturalized: ~w  ~w~n", [NNatC, NatC]),
    % per-seat naturalized count
    findall(C-Name, ( member(C, Cs),
                      stakeholder_seats:dr_type_for_stakeholder(C, Name, naturalized) ), NatSeats),
    length(NatSeats, NNatSeats),
    format("per-seat naturalized readings: ~w~n", [NNatSeats]),
    % beneficiary-seat naturalized specifically
    findall(C-Name, ( member(C, Cs),
                      narrative_ontology:constraint_stakeholder(C, Name, beneficiary, _,_,_,_),
                      stakeholder_seats:dr_type_for_stakeholder(C, Name, naturalized) ), NatBen),
    length(NatBen, NNatBen),
    format("per-seat naturalized on BENEFICIARY role: ~w~n", [NNatBen]),
    halt.
