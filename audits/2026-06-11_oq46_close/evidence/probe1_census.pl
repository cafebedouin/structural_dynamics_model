/* OQ-46 read-only evidence probe (2026-06-11). Run from prolog/:
   swipl -g "['/tmp/oq46_probe.pl'], run_probe, halt" -t "halt(1)"
   Variant of audits/2026-06-11_oq33_close/evidence/probe_a_live_census.pl
   that NAMES the scalar-only constraints and their per-constraint row counts. */

:- [stack].

ctx(Ctx) :- constraint_indexing:default_context(Ctx).

run_probe :-
    get_time(T0), format_time(atom(TS), '%FT%T%z', T0),
    format("=== OQ-46 probe: suppression representation census, live corpus ===~nas-of: ~w~n", [TS]),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NCC),
    format("corpus_constraint/1 count: ~w~n", [NCC]),
    controls,
    census.

controls :-
    ctx(Ctx),
    config:param(extractiveness_metric_name, ExtName),
    % Control 1: no suppression anywhere -> unknown floor must fire
    assertz(narrative_ontology:constraint_metric(oq46ctrl_nosupp, ExtName, 0.42)),
    (   drl_composition:classify_at_time(oq46ctrl_nosupp, 1000, Ctx, T1, I1),
        T1 == unknown, I1 == snap(none, false, none, none, none)
    ->  format("CONTROL-1 unknown-floor: FIRED~n")
    ;   format("CONTROL-1 unknown-floor: FAILED — zeros invalid~n"), halt(3)
    ),
    % Control 2: scalar-only -> stopgap branch must fire (Backed=false, Supp=scalar)
    assertz(narrative_ontology:constraint_metric(oq46ctrl_scalar, ExtName, 0.42)),
    assertz(narrative_ontology:constraint_metric(oq46ctrl_scalar, suppression_requirement, 0.7)),
    (   drl_composition:classify_at_time(oq46ctrl_scalar, 1000, Ctx, T2, I2),
        I2 = snap(_, false, _, 0.7, _), T2 \== unknown
    ->  format("CONTROL-2 stopgap-scalar: FIRED  type=~w~n", [T2])
    ;   format("CONTROL-2 stopgap-scalar: FAILED — zeros invalid~n"), halt(3)
    ),
    % Control 3: temporal series present -> temporal branch must fire (that arm of the census fires)
    assertz(narrative_ontology:constraint_metric(oq46ctrl_temp, ExtName, 0.42)),
    assertz(narrative_ontology:measurement(oq46m1, oq46ctrl_temp, suppression_requirement, 1000, 0.8)),
    (   drl_composition:classify_at_time(oq46ctrl_temp, 1000, Ctx, T3, I3),
        I3 = snap(_, _, _, 0.8, _), T3 \== unknown
    ->  format("CONTROL-3 temporal-branch: FIRED  type=~w~n", [T3])
    ;   format("CONTROL-3 temporal-branch: FAILED — zeros invalid~n"), halt(3)
    ),
    retractall(narrative_ontology:constraint_metric(oq46ctrl_nosupp, _, _)),
    retractall(narrative_ontology:constraint_metric(oq46ctrl_scalar, _, _)),
    retractall(narrative_ontology:constraint_metric(oq46ctrl_temp, _, _)),
    retractall(narrative_ontology:measurement(oq46m1, _, _, _, _)),
    format("controls retracted~n").

row_time(C, T) :- narrative_ontology:measurement(_, C, _, T, _).

census :-
    findall(C, corpus_loader:corpus_constraint(C), Cs), length(Cs, NC),
    findall(C, ( member(C, Cs),
                 narrative_ontology:measurement(_, C, suppression_requirement, _, _)
               ), TCs0), sort(TCs0, TCs), length(TCs, NTemporal),
    findall(C, ( member(C, Cs),
                 \+ narrative_ontology:measurement(_, C, suppression_requirement, _, _),
                 narrative_ontology:constraint_metric(C, suppression_requirement, _)
               ), SCs0), sort(SCs0, SCs), length(SCs, NScalarOnly),
    findall(C, ( member(C, Cs),
                 \+ narrative_ontology:measurement(_, C, suppression_requirement, _, _),
                 \+ narrative_ontology:constraint_metric(C, suppression_requirement, _)
               ), UCs0), sort(UCs0, UCs), length(UCs, NNeither),
    format("~n--- constraint-denominated (denominator = corpus_constraint/1 = ~w) ---~n", [NC]),
    format("temporal series authored : ~w~n", [NTemporal]),
    format("scalar-only (STOPGAP)    : ~w~n", [NScalarOnly]),
    format("neither (unknown floor)  : ~w  ~w~n", [NNeither, UCs]),
    format("~n--- scalar-only constraints, named, with row counts + scalar value + other temporal metrics authored ---~n"),
    forall(member(C, SCs),
           ( aggregate_all(count, row_time(C, _), NR0),
             findall(T, row_time(C, T), Ts0), sort(Ts0, Ts), length(Ts, NR),
             narrative_ontology:constraint_metric(C, suppression_requirement, SV),
             findall(M, ( narrative_ontology:measurement(_, C, M, _, _) ), Ms0),
             sort(Ms0, Ms),
             format("  ~w: rows=~w (raw ~w), scalar_supp=~w, temporal_metrics=~w~n",
                    [C, NR, NR0, SV, Ms]) )),
    format("~n--- temporal constraints: how many ALSO carry the scalar (dual representation)? ---~n"),
    findall(C, ( member(C, TCs),
                 narrative_ontology:constraint_metric(C, suppression_requirement, _)
               ), Duals), length(Duals, NDual),
    format("temporal AND scalar authored: ~w of ~w~n", [NDual, NTemporal]).
