/* OQ-114 inspection pass — ALL C-only stories (9 + 19 <= 25 per archive, no
   sampling). Instruments PINNED IN ADVANCE, derived from neither candidate:
   (i) classify_from_metrics-level type at the canonical contexts (pre-signature),
   (ii) snare-floor position (suppression value vs snare_suppression_floor),
   (iii) FSM/theater signals (beneficiary count, theater_ratio).
   Plus per-story inspectables: the disagreeing seats' authored types, eps.
   Plus the RIDER data: beneficiary-declaration co-occurrence with
   seat-disagreement among the C-certified population (both-cell vs C-only).

   Run from prolog/:
     swipl -g "consult('../audits/2026-06-12_oq114_archive_probe/inspection_probe.pl'), run('archives/datasets/kernel_v1', kernel_v1), halt" -t "halt(1)"
     swipl -g "consult('../audits/2026-06-12_oq114_archive_probe/inspection_probe.pl'), run('archives/datasets/original_v6', original_v6), halt" -t "halt(1)"
*/

:- [stack].

chain_c(C) :-
    narrative_ontology:constraint_claim(C, mountain),
    drl_core:emerges_naturally(C),
    narrative_ontology:constraint_metric(C, accessibility_collapse, AC),
    number(AC),
    config:param(natural_law_collapse_min, CollapseMin),
    AC >= CollapseMin,
    narrative_ontology:constraint_metric(C, resistance, R),
    number(R),
    config:param(natural_law_resistance_max, ResMax),
    R =< ResMax.

unanimity_u(C) :-
    constraint_indexing:constraint_classification(C, _, _),
    \+ (constraint_indexing:constraint_classification(C, Type, _), Type \= mountain).

has_cells(C) :- constraint_indexing:constraint_classification(C, _, _), !.

inspect(C) :-
    format("~n## ~w~n", [C]),
    % authored seats (the disagreement itself)
    findall(P-T, ( constraint_indexing:constraint_classification(C, T,
                       context(agent_power(P), _, _, _)) ), Seats0),
    sort(Seats0, Seats),
    format("  authored seats: ~w~n", [Seats]),
    % instrument (i): pre-signature metric type at canonical contexts
    constraint_indexing:site_contexts_canonical(C4),
    findall(L-T2, ( member(Ctx, C4),
                    ( drl_core:metric_based_type_indexed(C, Ctx, T2) -> true ; T2 = 'FAIL' ),
                    temporal_residual:context_label(Ctx, L) ), MTypes),
    format("  metric-level types (pre-signature): ~w~n", [MTypes]),
    % instrument (ii): snare-floor position
    config:param(snare_suppression_floor, Floor),
    (   narrative_ontology:constraint_metric(C, suppression_requirement, Supp)
    ->  ( Supp >= Floor -> Pos = at_or_above ; Pos = below ),
        format("  suppression: ~w (~w snare floor ~w)~n", [Supp, Pos, Floor])
    ;   format("  suppression: none authored~n")
    ),
    % instrument (iii): FSM/theater signals
    findall(B, narrative_ontology:constraint_beneficiary(C, B), Bs0), sort(Bs0, Bs),
    length(Bs, NB),
    (   narrative_ontology:constraint_metric(C, theater_ratio, TR) -> true ; TR = none ),
    (   drl_core:base_extractiveness(C, E) -> true ; E = none ),
    format("  beneficiaries: ~w ~w | theater_ratio: ~w | eps: ~w~n", [NB, Bs, TR, E]).

run(ArchivePath, Label) :-
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, ArchivePath)),
    corpus_loader:load_all_testsets,
    findall(X, corpus_loader:corpus_constraint(X), Cs0), sort(Cs0, Cs),
    findall(C, ( member(C, Cs), narrative_ontology:constraint_claim(C, mountain) ), M0),
    sort(M0, MClaim),
    include(has_cells, MClaim, WithCells),
    include(unanimity_u, WithCells, UT),
    exclude(unanimity_u, WithCells, UF),
    include(chain_c, UT, Both),
    include(chain_c, UF, COnly),
    format("ARCHIVE ~w — inspecting ALL ~w C-only stories~n", [Label, '_']),
    forall(member(C, COnly), inspect(C)),
    % RIDER: beneficiary co-occurrence among C-certified
    include([C]>>(narrative_ontology:constraint_beneficiary(C, _)), Both, BothBen),
    include([C]>>(narrative_ontology:constraint_beneficiary(C, _)), COnly, COnlyBen),
    length(Both, NBoth), length(BothBen, NBothBen),
    length(COnly, NCOnly), length(COnlyBen, NCOnlyBen),
    format("~nRIDER (beneficiary co-occurrence among C-certified): both-cell ~w/~w declare beneficiaries; C-only ~w/~w declare beneficiaries~n",
           [NBothBen, NBoth, NCOnlyBen, NCOnly]).
