% ============================================================================
% cs_trifurcation.pl — within-kernel reading-disagreement router (OQ-55)
% ============================================================================
% Classifies WHY a kernel's readings disagree, into the debugging_philosophy.md
% §6 trifurcation:
%
%   Type A (drift)     — frame slides within one seat (unmarked state mutation,
%                        treated as stable). §6 Stage 2.
%   Type B (structure) — readings' commitments are inconsistent; one forecloses
%                        another, no global section. §6 Stage 3.
%   Type C (ambiguity) — distinct declared seats coexist; genuine plurality,
%                        specify the index. §6 Stage 1.
%
% The router supplies the obstruction-status → A/B/C mapping that
% cs_kernel_registry.pl:116-117 explicitly defers to OQ-55. Dispatch is on the
% AUTHORED obstruction edge (cs_kernel_obstruction_status/2), refined by two
% COMPUTED within-kernel diagnostics (cs_drift_unacknowledged/2 for Type A,
% cs_axiom_foreclosed/2 to confirm Type B).
%
% INPUT-BOUNDARY DISCIPLINE (the OQ-55→OQ-56 re-scope witness): every input is
% a per-kernel / per-member-reading fact. This module reads NO reading_orbits.json,
% NO cross-kernel orbit label, NO OQ-56 cross-kernel vocabulary. Cross-kernel
% disagreement-labeling stays OQ-56-gated (OQ-53's transpose leg); only the
% within-kernel router is built here.
%
% COMMENTARY-GRADE (verdict-grade distinction): the verdict ANNOTATES the
% cs_kernel_comparison entry; it never overrides classify_from_metrics/6 or any
% headline verdict.
%
% FAIL-CLOSED (build_discipline Pattern 5): untyped kernel with NO drift signal
% returns `unknown`, never a plausible default type — "no authored edge + no
% drift" is didn't-look, not Type-X. Singletons yield no verdict (fail).
% ============================================================================

:- module(cs_trifurcation, [
    cs_reading_trifurcation/3,
    cs_trifurcation_report/0
]).

:- use_module(cs_kernel_registry, [cs_kernel_obstruction_status/2,
                                    cs_readings_for_kernel/2]).
:- use_module(cs_pattern_detection, [cs_drift_unacknowledged/2]).
:- use_module(cs_axiom_engine, [cs_axiom_foreclosed/2]).

%% cs_reading_trifurcation(+K, -Type, -Provenance)
%  Type ∈ {type_a_drift, type_b_structure, type_c_ambiguity, unknown}.
%  FAILS on a singleton kernel (<2 readings) — not contested, no verdict.
%  Provenance = provenance(scope(within_kernel), obstruction(Status), Diag)
%  stamps the derivation inline so the JSON field is self-describing and the
%  within-kernel scope travels with the value (cannot be misread cross-kernel).
cs_reading_trifurcation(K, Type, provenance(scope(within_kernel), obstruction(Status), Diag)) :-
    cs_kernel_registry:cs_kernel_obstruction_status(K, Status),
    Status \== singleton,
    once(trif_dispatch(K, Status, Type, Diag)).

%% trif_dispatch(+K, +Status, -Type, -Diagnostic)
%  real_closure: authored `forecloses` edge ⇒ Type B. cs_axiom_foreclosed on a
%  member CONFIRMS the structural fracture but is not a gate — the authored edge
%  is the signal; absence of a computed foreclosure just means `edge_only`.
trif_dispatch(K, real_closure, type_b_structure, diagnostic(axiom_foreclosed(Confirm))) :-
    (   kernel_has_foreclosure(K)
    ->  Confirm = confirmed
    ;   Confirm = edge_only
    ).
%  licensed_plurality: authored `coexists_with` edge ⇒ Type C. Both seats stand.
trif_dispatch(_, licensed_plurality, type_c_ambiguity, diagnostic(coexist_edge)).
%  untyped: no authored edge. The ONLY computed verdict. Drift is the
%  discriminator — fires Type A iff a member carries unacknowledged drift;
%  otherwise fail-closed to `unknown` (Pattern 5).
trif_dispatch(K, untyped, Type, Diag) :-
    (   kernel_has_drift(K)
    ->  Type = type_a_drift, Diag = diagnostic(drift_unacknowledged)
    ;   Type = unknown,      Diag = diagnostic(no_drift_signal_fail_closed)
    ).

%% kernel_has_drift(+K) — a member reading carries unacknowledged drift.
kernel_has_drift(K) :-
    cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    member(UID-_, Pairs),
    cs_pattern_detection:cs_drift_unacknowledged(UID, _),
    !.

%% kernel_has_foreclosure(+K) — a member reading carries a computed axiom foreclosure.
kernel_has_foreclosure(K) :-
    cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    member(UID-_, Pairs),
    cs_axiom_engine:cs_axiom_foreclosed(UID, _),
    !.

%% cs_trifurcation_report/0
%  Corpus consumer: type histogram over all kernels (queryable witness so the
%  producer is not produced-but-not-consumed pending the JSON field, Pattern 1).
cs_trifurcation_report :-
    findall(K0, (narrative_ontology:cs_kernel_id(_, K0), atom(K0)), KsRaw),
    sort(KsRaw, Kernels),
    findall(T,
            ( member(K, Kernels), cs_reading_trifurcation(K, T, _) ),
            Types),
    length(Kernels, NK),
    format("== Within-kernel trifurcation (~w kernels) ==~n", [NK]),
    forall(member(Ty, [type_a_drift, type_b_structure, type_c_ambiguity, unknown]),
           ( aggregate_all(count, member(Ty, Types), N),
             format("  ~w~t~26|~w~n", [Ty, N]) )).
