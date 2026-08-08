% ============================================================================
% OQ-261 C3 positive control — state_execution_authority triplet (amendment 4)
% ============================================================================
% Run from prolog/:
%   swipl -l ../audits/2026-08-07_oq261_forced_gluing/positive_control_probe.pl \
%         -g "control, halt" -t "halt(1)"
% Additive -l of the three kernel_test archive files alongside the live corpus
% (no corpus_path overlay). Criteria (pre-committed, PROPOSAL v2):
%   1. contexts with H1>0 over the 3-reading vector >= 85 (= ceil(253/3))
%   2. sum over contexts of H1 == same-run cs_kernel_divergence/4 count
% R2 rider 1 caveat applies: criterion 2 is NOT independent of the red-test
% predicate; declared in WRITEUP residue with a re-run pre-commitment.
% ============================================================================
:- [stack].
:- corpus_loader:load_all_testsets.
:- consult('archives/datasets/kernel_test/abolition_reading.pl').
:- consult('archives/datasets/kernel_test/retributive_reading.pl').
:- consult('archives/datasets/kernel_test/deterrence_reading.pl').

control :-
    K = state_execution_authority,
    cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    length(Pairs, NR),
    format("triplet readings registered: ~w (~w)~n", [NR, Pairs]),
    constraint_indexing:site_contexts_product(Ctxs),
    length(Ctxs, NCtx),
    findall(H1,
            ( member(Ctx, Ctxs),
              findall(T, ( member(_-C, Pairs), once(drl_core:dr_type(C, Ctx, T)) ), Vec),
              grothendieck_cohomology:obstruction_from_vector(Vec, _, H1) ),
            H1s),
    aggregate_all(count, ( member(H, H1s), number(H), H > 0 ), NObstructed),
    aggregate_all(sum(H2), ( member(H2, H1s), number(H2) ), SumH1),
    aggregate_all(count, cs_kernel_registry:cs_kernel_divergence(K, _, _, _), NDiv),
    format("contexts=~w obstructed=~w sum_H1=~w live_divergence_count=~w~n",
           [NCtx, NObstructed, SumH1, NDiv]),
    ( NObstructed >= 85
    -> format("CRITERION 1 PASS: obstructed ~w >= 85~n", [NObstructed])
    ;  format("CRITERION 1 FAIL: obstructed ~w < 85 — PROBE INVALID, halt C3~n", [NObstructed]) ),
    ( SumH1 =:= NDiv
    -> format("CRITERION 2 PASS: sum_H1 == divergence_count (~w) [rider-1 caveat applies]~n", [SumH1])
    ;  format("CRITERION 2 FAIL: sum_H1 ~w \\== divergence_count ~w — PROBE BUG~n", [SumH1, NDiv]) ).
