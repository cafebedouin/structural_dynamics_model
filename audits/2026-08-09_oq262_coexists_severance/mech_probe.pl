% ============================================================================
% OQ-262 Phase C mechanical tier — M1 / M2 per PREREGISTRATION.md §D (frozen)
% ============================================================================
% Run from prolog/ (two processes, one per substrate):
%   swipl -l ../audits/2026-08-09_oq262_coexists_severance/mech_probe.pl \
%         -g "mech_live, halt" -t "halt(1)"
%   swipl -l ../audits/2026-08-09_oq262_coexists_severance/mech_probe.pl \
%         -g "mech_kernel_test, halt" -t "halt(1)"
% All edge access via cs_kernel_registry:kernel_pair_edge/5 (the mandated
% accessor); enumerates ALL firings so "fires nowhere else" is witnessed, then
% checks the pinned must-fire / must-not-fire targets and FAILS (halt 1) on any
% control miss.
% ============================================================================
:- [stack].

% M1: coexists pair (>=1 direction) whose members own declared-contradictory axioms
m1(K, C1, C2) :-
    cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    member(U1-C1, Pairs), member(U2-C2, Pairs), U1 @< U2,
    once(( cs_kernel_registry:kernel_pair_edge(K, Pairs, U1, C2, coexists_with)
         ; cs_kernel_registry:kernel_pair_edge(K, Pairs, U2, C1, coexists_with) )),
    once(( narrative_ontology:cs_axiom(U1, _, A),
           narrative_ontology:cs_axiom(U2, _, B),
           ( narrative_ontology:cs_axiom_contradiction(A, B)
           ; narrative_ontology:cs_axiom_contradiction(B, A) ) )).

% M2: coexists_with in one direction AND forecloses in the reverse.
% Reported directionally: ForeSrc -forecloses-> CoexSrc, CoexSrc -coexists-> ForeSrc.
m2(K, ForeSrc, CoexSrc) :-
    cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    member(UF-ForeSrc, Pairs), member(UC-CoexSrc, Pairs), UF \== UC,
    cs_kernel_registry:kernel_pair_edge(K, Pairs, UF, CoexSrc, forecloses),
    cs_kernel_registry:kernel_pair_edge(K, Pairs, UC, ForeSrc, coexists_with).

all_kernels(Ks) :-
    setof(K, C^(narrative_ontology:cs_kernel_id(C, K)), Ks).

report :-
    all_kernels(Ks),
    format("-- M1 firings (severance_candidate), ALL kernels --~n"),
    forall(( member(K, Ks), m1(K, C1, C2) ),
           format("  M1 ~w : ~w | ~w~n", [K, C1, C2])),
    aggregate_all(count, ( member(K, Ks), m1(K, _, _) ), NM1),
    format("  M1 total: ~w~n", [NM1]),
    format("-- M2 firings (miscoded_asymmetry), ALL kernels --~n"),
    forall(( member(K, Ks), m2(K, F, X) ),
           format("  M2 ~w : ~w -forecloses-> ~w (reverse coexists)~n", [K, F, X])),
    aggregate_all(count, ( member(K, Ks), m2(K, _, _) ), NM2),
    format("  M2 total: ~w~n", [NM2]).

check(Desc, Goal, Expect) :-
    ( Goal -> R = true ; R = false ),
    ( R == Expect
    -> format("  CONTROL OK: ~w (~w)~n", [Desc, Expect])
    ;  format("  CONTROL FAIL: ~w — got ~w expected ~w~n", [Desc, R, Expect]),
       halt(1) ).

mech_live :-
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format("live corpus: ~w constraints~n", [NC]),
    report,
    format("-- pinned controls (fiat) --~n"),
    check('M1 fires empathy_simulation|empirical_precedent',
          m1(fiat_efficacy_kernel, empathy_simulation_reading, empirical_precedent_reading),
          true),
    check('M1 does NOT fire truth_procedure|utopian_fiction (contradiction, no coexists edge)',
          ( m1(fiat_efficacy_kernel, truth_procedure_reading, utopian_fiction_reading)
          ; m1(fiat_efficacy_kernel, utopian_fiction_reading, truth_procedure_reading) ),
          false),
    check('M2 fires empirical_precedent -forecloses-> utopian_fiction',
          m2(fiat_efficacy_kernel, empirical_precedent_reading, utopian_fiction_reading),
          true),
    check('M2 fires predictive_synthesis -forecloses-> utopian_fiction',
          m2(fiat_efficacy_kernel, predictive_synthesis_reading, utopian_fiction_reading),
          true),
    check('M2 fires nowhere else in fiat (exactly 2)',
          ( aggregate_all(count, m2(fiat_efficacy_kernel, _, _), N2), N2 =:= 2 ),
          true),
    format("mech_live: ALL CONTROLS PASS~n").

mech_kernel_test :-
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, 'archives/datasets/kernel_test')),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format("kernel_test corpus: ~w constraints~n", [NC]),
    report,
    format("-- pinned controls (kernel_test, Arm B) --~n"),
    check('M1 fires categorical_abolition|deterrence_instrument',
          m1(state_killing_authority, categorical_abolition, deterrence_instrument),
          true),
    check('M1 does NOT fire retributive_desert|categorical_abolition (contradiction, mutual forecloses)',
          ( m1(state_killing_authority, retributive_desert, categorical_abolition)
          ; m1(state_killing_authority, categorical_abolition, retributive_desert) ),
          false),
    check('M2 fires nowhere on kernel_test (all pair profiles symmetric)',
          ( all_kernels(Ks), aggregate_all(count, (member(K, Ks), m2(K, _, _)), N2), N2 =:= 0 ),
          true),
    format("mech_kernel_test: ALL CONTROLS PASS~n").
