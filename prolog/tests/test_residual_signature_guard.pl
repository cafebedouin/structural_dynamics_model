:- module(test_residual_signature_guard, []).
:- use_module(library(plunit)).

/* OQ-138 (2026-07-14) — the residual-signature guard's OWN positive control.
   The guard routes the seven residual clauses to abstain. The load-bearing correctness fact is
   the abstain TOKEN: it must be `unknown` (which is_real_type/1 filters out of the H¹ real-seat
   set), NEVER `untyped` (which is_real_type/1 counts as a real disagreeing type → inflates H¹,
   the exact pathology OQ-138 exists to kill). These tests witness the guard abstains AND lands on
   the filtered token — so "abstain passing" is not byte-indistinguishable from "untyped leak
   passing." Run: swipl -g "[stack],[tests/test_residual_signature_guard],run_tests,halt" -t "halt(1)"
*/

:- begin_tests(residual_signature_guard).

% --- The footgun and its avoidance (the discriminating counterfactual) ---
test(unknown_is_filtered_from_h1) :-
    \+ grothendieck_cohomology:is_real_type(unknown).

test(untyped_would_leak_into_h1) :-           % counterfactual: proves the token choice is load-bearing
    grothendieck_cohomology:is_real_type(untyped).

% --- The guard abstains by default, restores legacy under the lever ---
test(guard_abstains_to_unknown_by_default) :-
    signature_detection:residual_route(rope, R),
    R == unknown.

test(guard_restores_legacy_under_lever, [setup(set_lever(1)), cleanup(set_lever(0))]) :-
    signature_detection:residual_route(rope, R),
    R == rope.

% --- End-to-end through the REAL dispatch: a residual seat routes to the FILTERED abstain token ---
test(real_dispatch_routes_residual_to_filtered_unknown) :-
    signature_detection:resolve_with_perspectival_check(synth_c, mountain, coordination_scaffold, R),
    R == unknown,
    \+ grothendieck_cohomology:is_real_type(R).   % the routed token is filtered — no H1 leak

% --- The monitor recognizes the residual shape (positive control it can read non-zero) ---
test(monitor_recognizes_residual_shape) :-
    signature_detection:residual_signature_pattern(mountain, coordination_scaffold).

test(monitor_excludes_nonresidual_shape) :-
    \+ signature_detection:residual_signature_pattern(rope, coupling_invariant_rope).

:- end_tests(residual_signature_guard).

set_lever(V) :-
    retractall(config:param(residual_signature_override_enabled, _)),
    asserta(config:param(residual_signature_override_enabled, V)).
