/* Probe A — classify_at_time branch census, LIVE corpus (OQ-33 close, 2026-06-11).
   Run from prolog/:  swipl -g "[<this file>], run_probe, halt" -t "halt(1)"
   Per-process positive controls run BEFORE the census, in this process.       */

:- [stack].

ctx(Ctx) :- constraint_indexing:default_context(Ctx).

run_probe :-
    get_time(T0), format_time(atom(TS), '%FT%T%z', T0),
    format("=== Probe A: branch census, live corpus ===~nas-of: ~w~n", [TS]),
    corpus_loader:load_all_testsets,
    config:param(corpus_path, CP),
    corpus_loader:resolve_corpus_dir(CP, Abs),
    format("resolved corpus dir: ~w~n", [Abs]),
    aggregate_all(count, corpus_loader:corpus_constraint(_), NCC),
    format("corpus_constraint/1 count: ~w~n", [NCC]),
    covering_analysis:all_corpus_constraints(ACs), length(ACs, NAC),
    format("all_corpus_constraints/1 count (incl. any engine demos): ~w~n", [NAC]),
    controls,
    census.

% ---------- positive controls (this process) ----------
controls :-
    ctx(Ctx),
    % Control 1: in-denominator-shaped synthetic, NO authored suppression anywhere
    %            -> must reach the unknown floor branch.
    config:param(extractiveness_metric_name, ExtName),
    assertz(narrative_ontology:constraint_metric(oq33ctrl_nosupp, ExtName, 0.42)),
    (   drl_composition:classify_at_time(oq33ctrl_nosupp, 1000, Ctx, T1, I1),
        T1 == unknown, I1 == snap(none, false, none, none, none)
    ->  format("CONTROL-1 unknown-floor: FIRED  type=~w info=~w~n", [T1, I1])
    ;   format("CONTROL-1 unknown-floor: FAILED — process zeros are INVALID~n"),
        halt(3)
    ),
    % Control 2: scalar-only synthetic -> must take the STOPGAP branch
    %            (Info Supp = the authored scalar; Backed=false).
    assertz(narrative_ontology:constraint_metric(oq33ctrl_scalar, ExtName, 0.42)),
    assertz(narrative_ontology:constraint_metric(oq33ctrl_scalar,
                                                 suppression_requirement, 0.7)),
    (   drl_composition:classify_at_time(oq33ctrl_scalar, 1000, Ctx, T2, I2),
        I2 = snap(_, B2, _, S2, _), S2 == 0.7, B2 == false, T2 \== unknown
    ->  format("CONTROL-2 stopgap-scalar: FIRED  type=~w info=~w~n", [T2, I2])
    ;   format("CONTROL-2 stopgap-scalar: FAILED — process zeros are INVALID~n"),
        halt(3)
    ),
    retractall(narrative_ontology:constraint_metric(oq33ctrl_nosupp, _, _)),
    retractall(narrative_ontology:constraint_metric(oq33ctrl_scalar, _, _)),
    (   covering_analysis:all_corpus_constraints(ACs2),
        \+ member(oq33ctrl_nosupp, ACs2), \+ member(oq33ctrl_scalar, ACs2)
    ->  format("controls retracted; denominator clean~n")
    ;   format("CONTROL RETRACTION FAILED — denominator polluted~n"), halt(3)
    ).

% ---------- census ----------
% Branch per (C,T) row, cross-checked against the engine call itself:
% the Info Supp slot must equal the authored value the branch claims to read.
row_branch(Ctx, C, T, Branch, Backed) :-
    drl_composition:classify_at_time(C, T, Ctx, Type, Info),
    Info = snap(_, Backed, _, SuppI, _),
    (   narrative_ontology:measurement(_, C, suppression_requirement, T, SuppT)
    ->  ( SuppI == SuppT -> Branch = temporal
        ; Branch = anomaly(temporal_mismatch(C, T, SuppI, SuppT)) )
    ;   narrative_ontology:constraint_metric(C, suppression_requirement, SuppS)
    ->  ( SuppI == SuppS -> Branch = scalar_stopgap
        ; Branch = anomaly(scalar_mismatch(C, T, SuppI, SuppS)) )
    ;   ( Type == unknown, Info == snap(none, false, none, none, none)
        -> Branch = unknown_floor
        ;  Branch = anomaly(residual(C, T, Type, Info)) )
    ).

% helper: avoids the ^/module-qualification precedence trap inside setof
row_time(C, T) :- narrative_ontology:measurement(_, C, _, T, _).

census :-
    ctx(Ctx),
    findall(C, corpus_loader:corpus_constraint(C), Cs), length(Cs, NC),
    % constraint-denominated coverage
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
    aggregate_all(count,
        narrative_ontology:measurement(_, _, suppression_requirement, _, _), NSuppFacts),
    format("~n--- constraint-denominated (denominator = corpus_constraint/1 = ~w) ---~n", [NC]),
    format("temporal suppression series authored : ~w~n", [NTemporal]),
    format("scalar-only (STOPGAP branch)         : ~w~n", [NScalarOnly]),
    format("neither (unknown-floor class)        : ~w  ~w~n", [NNeither, UCs]),
    format("suppression_requirement measurement/5 facts total: ~w~n", [NSuppFacts]),
    % row-denominated census via the engine call
    findall(C-T,
        ( member(C, Cs),
          setof(Tm, row_time(C, Tm), Ts),
          member(T, Ts) ),
        Rows),
    length(Rows, NRows),
    findall(B-K, ( member(C-T, Rows), row_branch(Ctx, C, T, B, K) ), BKs),
    length(BKs, NClassified),
    aggregate_all(count, member(temporal-_, BKs), NRowTemporal),
    aggregate_all(count, member(scalar_stopgap-_, BKs), NRowScalar),
    aggregate_all(count, member(unknown_floor-_, BKs), NRowUnknown),
    findall(A, ( member(B-_, BKs), B = anomaly(A) ), Anoms), length(Anoms, NAnom),
    aggregate_all(count, member(_-true,  BKs), NBackedT),
    aggregate_all(count, member(_-false, BKs), NBackedF),
    format("~n--- row-denominated (rows = constraint x measurement-time) ---~n"),
    format("rows total                 : ~w  (engine-classified: ~w)~n", [NRows, NClassified]),
    format("temporal-measurement branch: ~w~n", [NRowTemporal]),
    format("scalar-STOPGAP branch      : ~w~n", [NRowScalar]),
    format("unknown-floor branch       : ~w~n", [NRowUnknown]),
    format("anomalies (residual / mismatch): ~w  ~w~n", [NAnom, Anoms]),
    format("Backed=true rows  : ~w~nBacked=false rows : ~w~n", [NBackedT, NBackedF]),
    (   NRows =:= NClassified
    ->  format("row-coverage check: every row engine-classified~n")
    ;   format("ROW-COVERAGE GAP: ~w rows, ~w classified — classify_at_time FAILED on some rows~n",
               [NRows, NClassified])
    ).
