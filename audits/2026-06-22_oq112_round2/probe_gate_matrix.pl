% OQ-112 Round-2 — the verdict_join fail-closed gate, four-case matrix + cross-term.
% Drives the REAL diagnostic path (diagnostic_summary -> verdict_join) over one target
% constraint under controlled maxent-stage states. Each arm reports the headline Joined,
% the raw Base, and which maxent_voided alerts the join carries (per stage).
%
%   COMPLETE        both stages done            -> no void alert ; Joined == Base (gate no-op)
%   THROW indexed   classical done, indexed throws (absorbed) -> maxent_voided(indexed) ; Joined floored
%   FAIL  indexed   indexed plain-FAILS via the :871-874 no-priors guard (catch-blind)
%                                                 -> maxent_voided(indexed) ; Joined floored
%   THROW classical classical throws            -> maxent_voided(classical) ; Joined floored
%   N=0 legal       completion fact present, N=0 -> NO void alert ; Joined == Base (legal-empty passes)
%   CROSS-TERM      classical PRESENT + indexed VOID -> maxent_voided(indexed) only, NOT classical
%                                                 (the gate reads each stage's OWN fact, never "any present")

:- consult(stack).
:- consult(maxent_classifier).
:- consult(dirac_classification).
:- consult(diagnostic_summary).
:- consult(post_synthesis).
:- consult(json_report).
:- corpus_loader:load_all_testsets.
:- json_report:load_abductive_data.

ctxs(WCtxs)   :- measurement_layer:wasserstein_contexts(WCtxs).
dctx(Ctx)     :- constraint_indexing:default_context(Ctx).
thrower(polaris_document_status_contradictions).

% compute join for C, extract headline + void flags
jv(C, Joined, Base, VoidC, VoidI) :-
    dctx(Ctx),
    diagnostic_summary:diagnostic_summary(C, Summary),
    diagnostic_summary:verdict_join(C, Summary, verdict_join(Joined, Base, _Cap, Alerts, _G, _M, _Sg)),
    ( member(alert(maxent_voided(classical), _, _), Alerts) -> VoidC = yes ; VoidC = no ),
    ( member(alert(maxent_voided(indexed),   _, _), Alerts) -> VoidI = yes ; VoidI = no ).

setup_complete :-
    maxent_classifier:maxent_cleanup, retractall(diagnostic_summary:maxent_attempted(_)),
    ctxs(WCtxs), dctx(Ctx),
    assertz(diagnostic_summary:maxent_attempted(classical)),
    maxent_classifier:maxent_multi_run(WCtxs, _),
    assertz(diagnostic_summary:maxent_attempted(indexed)),
    maxent_classifier:maxent_indexed_run(Ctx, _).

report(Tag, C) :-
    ( jv(C, J, B, VC, VI) -> true ; J = err, B = err, VC = '?', VI = '?' ),
    format('~w~t~22|J=~w~tB=~w~t  void[classical=~w indexed=~w]~n', [Tag, J, B, VC, VI]).

main :-
    ctxs(WCtxs), dctx(Ctx), thrower(TH),

    % --- find a green-Joined target under COMPLETE state (so green->yellow is visible) ---
    setup_complete,
    ( corpus_loader:corpus_constraint(Cand), jv(Cand, green, _, no, no), ! -> Target = Cand
    ; corpus_loader:corpus_constraint(Target), ! ),
    format('TARGET = ~w~n~n', [Target]),

    % ===== ARM: COMPLETE =====
    setup_complete, report('COMPLETE', Target),

    % ===== ARM: THROW indexed (classical present) =====
    maxent_classifier:maxent_cleanup, retractall(diagnostic_summary:maxent_attempted(_)),
    assertz(diagnostic_summary:maxent_attempted(classical)),
    maxent_classifier:maxent_multi_run(WCtxs, _),                       % classical completes
    assertz(diagnostic_summary:maxent_attempted(indexed)),
    setup_call_cleanup(
        assertz(narrative_ontology:constraint_claim(TH, throw_test_claim)),
        ( ( catch(maxent_classifier:maxent_indexed_run(Ctx, _), _, fail) -> true ; true ) ),  % absorbed throw
        retractall(narrative_ontology:constraint_claim(TH, throw_test_claim))
    ),
    ( maxent_classifier:maxent_indexed_run_info(Ctx,_,_) -> IT = present ; IT = absent ),
    format('  (indexed_run_info after throw: ~w)~n', [IT]),
    report('THROW-indexed', Target),

    % ===== ARM: FAIL indexed (no-priors guard :871-874, plain failure, catch-blind) =====
    maxent_classifier:maxent_cleanup, retractall(diagnostic_summary:maxent_attempted(_)),
    assertz(diagnostic_summary:maxent_attempted(indexed)),
    ( catch(maxent_classifier:maxent_indexed_run(Ctx, _), _, fail) -> RIDX = succeeded ; RIDX = failed_plain ),
    ( maxent_classifier:maxent_indexed_run_info(Ctx,_,_) -> IT2 = present ; IT2 = absent ),
    format('  (no-priors indexed_run -> ~w ; indexed_run_info: ~w)~n', [RIDX, IT2]),
    report('FAIL-indexed', Target),

    % ===== ARM: THROW classical =====
    maxent_classifier:maxent_cleanup, retractall(diagnostic_summary:maxent_attempted(_)),
    assertz(diagnostic_summary:maxent_attempted(classical)),
    setup_call_cleanup(
        assertz(narrative_ontology:constraint_claim(TH, throw_test_claim)),
        ( ( catch(maxent_classifier:maxent_multi_run(WCtxs, _), _, fail) -> true ; true ) ),
        retractall(narrative_ontology:constraint_claim(TH, throw_test_claim))
    ),
    ( maxent_classifier:maxent_run_info(Ctx,_,_) -> CT = present ; CT = absent ),
    format('  (classical run_info after throw: ~w)~n', [CT]),
    report('THROW-classical', Target),

    % ===== ARM: N=0 legal (completion fact present, N=0) =====
    maxent_classifier:maxent_cleanup, retractall(diagnostic_summary:maxent_attempted(_)),
    assertz(diagnostic_summary:maxent_attempted(classical)),
    assertz(diagnostic_summary:maxent_attempted(indexed)),
    get_time(T0),
    assertz(maxent_classifier:maxent_run_info(Ctx, 0, T0)),
    assertz(maxent_classifier:maxent_indexed_run_info(Ctx, 0, T0)),
    report('N0-legal', Target),

    % ===== latency on 92: in COMPLETE state, no constraint gets a void alert =====
    setup_complete,
    findall(C2, ( corpus_loader:corpus_constraint(C2), jv(C2, _, _, VC2, VI2),
                  (VC2 == yes ; VI2 == yes) ), Voided92),
    length(Voided92, NV),
    format('~nLATENCY/92: constraints with a maxent_voided alert under COMPLETE state = ~w  (expect 0)~n', [NV]),
    ( NV > 0 -> format('  VOIDED: ~w~n', [Voided92]) ; true ),
    halt.

:- initialization(main).
