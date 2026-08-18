:- module(test_dispatch_bound_call, []).
:- use_module(library(plunit)).

/* dispatch_bound_call — a bound call to a dispatch predicate means "the engine assigns".

   Post-fix semantics of the fresh-variable-head + unify-after-cut transformation
   (2026-08-17 pilot, audits/2026-08-17_bound_dispatch_hardening/): a call with the
   output argument bound to an atom succeeds IFF the engine's own cascade assigns that
   atom — never because a later clause body happens to hold in isolation.

   Discrimination record (the before-commit free pair): at pre-fix HEAD the two
   over_accept tests are RED — bound calls succeed on types the engine does not assign
   (witnessed in-session: engine_first=scaffold yet bound rope SUCCEEDS; sig_first=unknown
   yet bound ambiguous SUCCEEDS, the constraint_signature(C, ambiguous) 276-vs-0 artifact
   of 2026-08-17). At the fix commit they are GREEN. Runs recorded in the audit dir's
   audit_log.md.

   Fixture: bdh_synth_overlap authors ONLY a constraint_beneficiary (multifile), making
   the scaffold body AND the rope body both true at the metrics below — the engine's
   clause order commits to scaffold; the old shape let a bound rope call skip that
   commitment. bdh_synth_bare authors NOTHING — the honest-abstain lock assigns unknown;
   the old shape let a bound ambiguous call fall through to classify_by_signature's
   unconditional terminal.

   Run: cd prolog && swipl -g "[stack], [tests/test_dispatch_bound_call], run_tests, halt" -t "halt(1)"
   ([stack] suffices: classify_from_metrics/6 is metric-only and the signature fixture
   exercises the authored-absence lock, not MaxEnt.) */

:- multifile narrative_ontology:constraint_beneficiary/2.
narrative_ontology:constraint_beneficiary(bdh_synth_overlap, bdh_synth_group).

overlap_ctx(context(agent_power(moderate), time_horizon(biographical),
                    exit_options(mobile), spatial_scope(national))).

:- begin_tests(dispatch_bound_call).

% --- Stability anchors (GREEN at both ends; a failure here means the fixture rotted,
%     not that the dispatch semantics moved) ---
test(engine_assigns_scaffold_at_overlap_metrics) :-
    overlap_ctx(Ctx),
    once(drl_core:classify_from_metrics(bdh_synth_overlap, 0.20, 0.20, 0.50, Ctx, T)),
    T == scaffold.

test(rope_body_holds_in_isolation_control, [nondet]) :-
    % The overlap is REAL: rope's own conditions hold at these metrics/context
    % (chi 0.20 =< 0.35, eps 0.20 =< 0.45, biographical+mobile -> rope immutability).
    % Without this control, the over_accept test below could pass vacuously on a
    % fixture where rope was never reachable at all.
    overlap_ctx(Ctx),
    constraint_indexing:effective_immutability_for_context(Ctx, rope).

test(engine_assigns_unknown_on_bare_story) :-
    once(signature_detection:constraint_signature(bdh_synth_bare, S)),
    S == unknown.

% --- The post-fix semantics (RED at pre-fix HEAD, GREEN at the fix commit) ---
test(bound_call_cannot_manufacture_rope_over_scaffold) :-
    overlap_ctx(Ctx),
    \+ drl_core:classify_from_metrics(bdh_synth_overlap, 0.20, 0.20, 0.50, Ctx, rope).

test(bound_call_cannot_manufacture_ambiguous_over_unknown) :-
    \+ signature_detection:constraint_signature(bdh_synth_bare, ambiguous).

% --- Bound call WITH the engine's own type still succeeds (the transformation must
%     not break honest bound queries) ---
test(bound_call_with_engine_type_succeeds) :-
    overlap_ctx(Ctx),
    drl_core:classify_from_metrics(bdh_synth_overlap, 0.20, 0.20, 0.50, Ctx, scaffold).

test(bound_unknown_matches_engine_on_bare_story) :-
    signature_detection:constraint_signature(bdh_synth_bare, unknown).

:- end_tests(dispatch_bound_call).
