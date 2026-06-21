% Gate 0 controls probe for OQ-119 (read-only; NO SPEND).
% The two-sided / no-fabrication witnesses that license the roster counts:
%   G0.3 temporal: prove the temporal probe SEPARATES known-rich from known-thin
%        (incl. a series-present-but-Backed=false case) — the byte-identical-read guard.
%   G0.1 observer: 4-seat chi spread non-degenerate on roster stories + stakeholder
%        authoring (power_witness_map) so a spread is measured, not inference-only.
%   G0.2 axiom: witness readings actually classify differently (cs_kernel_divergence).

:- initialization(main).

main :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, 'testsets_haiku')),
    [stack],
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NCorp),
    format('corpus_constraint/1 = ~w~n~n', [NCorp]),

    g03_temporal_control,
    g01_observer_control,
    g02_axiom_control,
    halt(0).
main :- format('CONTROLS PROBE FAILED~n', []), halt(1).

% ====================================================================
% G0.3 — temporal two-sided control (THE positive control)
% ====================================================================
g03_temporal_control :-
    format('================ G0.3 TEMPORAL TWO-SIDED CONTROL ================~n', []),
    RichKnown = acceptable_risk_energy__expected_value_dominant,
    ThinScalar = digital_money_emergence_boundary__consumer_holdings_reading,
    ThinScalar2 = monopoly_rulebook__social_scaffold_reading,
    format('-- KNOWN RICH: ~w~n', [RichKnown]),
    report_temporal(RichKnown),
    format('-- KNOWN THIN (scalar-only, measurements=0): ~w~n', [ThinScalar]),
    report_temporal(ThinScalar),
    format('-- KNOWN THIN (scalar-only, measurements=0): ~w~n', [ThinScalar2]),
    report_temporal(ThinScalar2),
    % Sharpest control: find a story with a base_extractiveness SERIES (>=2 times)
    % whose snapshots read Backed=FALSE (suppression not authored at those times).
    ( find_series_but_unbacked(BadC, NTimes, NBacked)
    ->  format('-- SHARP THIN (series present but Backed=false): ~w~n', [BadC]),
        format('   base_extractiveness timepoints=~w  Backed timepoints=~w~n', [NTimes, NBacked]),
        report_temporal(BadC)
    ;   format('-- SHARP THIN: no series-present-but-Backed=false story found (all series fully backed)~n', [])
    ),
    format('VERDICT G0.3: probe separates rich (PASS) from thin (FAIL) iff the booleans below differ.~n~n', []).

report_temporal(C) :-
    ( backed_timepoints(C, TVs) -> true ; TVs = [] ),
    length(TVs, NT),
    ( (NT >= 2, nonzero_rate(TVs)) -> Pass = pass ; Pass = fail ),
    findall(T, narrative_ontology:measurement(_, C, base_extractiveness, T, _), AllT0),
    sort(AllT0, AllT), length(AllT, NAll),
    format('   base_extractiveness series timepoints=~w  Backed timepoints=~w  reading_temporal_nonvacuous=~w~n~n',
           [NAll, NT, Pass]).

% backed_timepoints(+C,-TVs): sorted T-Eps at times where classify_at_time/5 Backed=true.
backed_timepoints(C, TVs) :-
    logical_fingerprint:standard_context_for_power(analytical, Ctx),
    findall(T, narrative_ontology:measurement(_, C, base_extractiveness, T, _), Ts0),
    sort(Ts0, Ts),
    findall(T-Eps,
            ( member(T, Ts),
              once(drl_composition:classify_at_time(C, T, Ctx, _Type, snap(_,true,Eps,_,_))),
              number(Eps) ),
            TVs0),
    sort(TVs0, TVs).

nonzero_rate([T1-V1, T2-V2 | _]) :- D is T2 - T1, D > 0, R is abs((V2 - V1)/D), R > 0.0, !.
nonzero_rate([_|Rest]) :- nonzero_rate(Rest).

% A story whose ε series exists at >=2 times but NOT all are Backed (supp gap).
find_series_but_unbacked(C, NTimes, NBacked) :-
    distinct(C, ( narrative_ontology:cs_kernel_id(C, _),
              findall(T, narrative_ontology:measurement(_, C, base_extractiveness, T, _), Ts0),
              sort(Ts0, Ts), length(Ts, NTimes), NTimes >= 2,
              backed_timepoints(C, TVs), length(TVs, NBacked),
              NBacked < NTimes )), !.

% ====================================================================
% G0.1 — observer non-degeneracy + stakeholder authoring
% ====================================================================
g01_observer_control :-
    format('================ G0.1 OBSERVER CONTROL ================~n', []),
    Samples = [acceptable_risk_energy__expected_value_dominant,
               westphalia_sovereignty__absolute_non_intervention,
               woman_category__sex_biology_reading],
    forall(member(C, Samples), report_observer(C)),
    % No-fabrication justification (code-read, json_report.pl:934-941): a seat that
    % cannot compute returns null/fail, NOT a fabricated default. Witness: a thin
    % scalar story still yields its 4 seats from the authored scalar ε (no 0.5 floor).
    format('-- no-fabrication witness: thin scalar story 4-seat spread (from authored scalar eps):~n', []),
    report_observer(digital_money_emergence_boundary__consumer_holdings_reading),
    format('VERDICT G0.1: spread non-degenerate (4 chi not all equal) where authored; stakeholder 0s flag inference-only seats.~n~n', []).

report_observer(C) :-
    findall(P-Chi,
            ( member(P, [powerless, moderate, institutional, analytical]),
              once(seat_chi(C, P, Chi)) ),
            Seats),
    ( catch(stakeholder_seats:power_witness_map(C, WMap), _, WMap=unavailable) -> true ; WMap = unavailable ),
    ( degenerate(Seats) -> Deg = DEGENERATE ; Deg = 'non-degenerate' ),
    format('   ~w~n     seats=~w  [~w]~n     stakeholder_witness=~w~n', [C, Seats, Deg, WMap]).

seat_chi(C, P, Chi) :-
    logical_fingerprint:standard_context_for_power(P, Ctx),
    ( catch(constraint_indexing:extractiveness_for_agent(C, Ctx, Chi0), _, fail)
    -> Chi is round(Chi0*1000)/1000 ; Chi = null ).

degenerate(Seats) :-
    findall(V, (member(_-V, Seats), number(V)), Vs),
    Vs = [First|_],
    forall(member(X, Vs), X =:= First).

% ====================================================================
% G0.2 — axiom divergence witness (readings classify differently)
% ====================================================================
g02_axiom_control :-
    format('================ G0.2 AXIOM CONTROL ================~n', []),
    Kernels = [acceptable_risk_energy, westphalia_sovereignty, woman_category],
    forall(member(K, Kernels), report_divergence(K)),
    format('VERDICT G0.2: a fired divergence = readings disagree on type at some context (non-trivial cross-examination).~n', []),
    format('   no-fabrication justification: cs_kernel_obstruction_status is FAIL-CLOSED on absence~n', []),
    format('   (untyped, not silently glued; cs_kernel_registry.pl:113) — agreement cannot be fabricated.~n~n', []).

report_divergence(K) :-
    cs_kernel_registry:cs_kernel_coverage(K, NR),
    ( cs_kernel_registry:cs_kernel_divergence(K, Ctx, U1-C1, U2-C2)
    ->  Ctx = context(_,_,_,spatial_scope(Sc)),
        format('   ~w (readings=~w): DIVERGES at scope=~w: ~w vs ~w~n', [K, NR, Sc, C1, C2])
    ;   format('   ~w (readings=~w): no divergence found across contexts~n', [K, NR])
    ),
    ( cs_kernel_registry:cs_kernel_obstruction_status(K, St) -> true ; St = '?' ),
    format('     obstruction_status=~w~n', [St]),
    ignore(U1), ignore(U2).
