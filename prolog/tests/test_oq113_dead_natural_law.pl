% ============================================================================
% test_oq113_dead_natural_law.pl — OQ-113: natural_law_signature/1 is
% unsatisfiable by construction; pure_natural_law subtype unreachable.
%
% Locks the §9b.2 KILL as a regression: the natural_law detector's
% HasAlternatives==false leg is DEAD-BY-RANGE (has_viable_alternatives/2 emits
% only {true, unknown}). These tests prove the probe FIRES (positive control)
% AND that it fires on ZERO live constraints — so a future corpus/schema change
% that powers the detector breaks (b)/(c) loudly rather than silently emitting
% pure_natural_law off burned metrics.
%
% Run: cd prolog && swipl -g "[stack], corpus_loader:load_all_testsets, \
%   [tests/test_oq113_dead_natural_law], run_tests(oq113_dead_natural_law), halt" \
%   -t "halt(1)"
% ============================================================================

:- begin_tests(oq113_dead_natural_law).

% --- (a) POSITIVE CONTROL — the probe fires on a constructed false-slot profile.
%   `false` lands in the HasAlternatives slot per the head arg order at
%   signature_detection.pl:378 (cite refreshed 2026-08-17, OQ-251 audit; the
%   former :359 had drifted onto the section banner comment)
%   (profile/7: Access, Suppression, Resistance, BeneficiaryCount,
%   HasAlternatives, TemporalStability, CoordinationSuccess). If this FAILS the
%   probe is dead and (b)/(c) below would be vacuously satisfied.
test(positive_control_signature_fires) :-
    signature_detection:natural_law_signature(
        profile(0.92, 0.02, 0.04, 0, false, stable, _)).

% --- (b) LIVE CORPUS — natural_law_signature fires on ZERO constraints.
test(live_corpus_zero_firings) :-
    corpus_loader:ensure_corpus_loaded,
    findall(C,
            ( corpus_loader:corpus_constraint(C),
              signature_detection:get_constraint_profile(C, P),
              signature_detection:natural_law_signature(P) ),
            L),
    assertion(L == []).

% --- (c) RANGE WITNESS — has_viable_alternatives/2 never returns `false`.
test(has_viable_alternatives_never_false) :-
    corpus_loader:ensure_corpus_loaded,
    forall(corpus_loader:corpus_constraint(C),
           ( signature_detection:has_viable_alternatives(C, V),
             V \== false )).

% ===========================================================================
% OQ-296 EXTENSION (2026-08-18) — lock the SECOND dead signature.
%
% natural_law is dead-by-RANGE (needs `false`, never gets it). Its sibling
% coordination_scaffold is dead-by-EMPTY-TABLE (needs `true`, which only
% intent_viable_alternative/3 can supply, and that table is empty — GAP-08).
% Together they make has_viable_alternatives/2 a CONSTANT function: `unknown`
% everywhere. (a)-(c) above lock the first signature; (d)-(f) lock the second
% and the constancy itself.
%
% AUTHORSHIP MUST NOT LEAK. D1 refused repair-by-authorship, so no planted
% `true` may reach a loaded corpus file or the load chain — a fixture there
% would satisfy the signature by fiat through the back door. (d) follows (a)'s
% safe shape: the value is planted INSIDE a profile/7 term passed directly to
% the predicate, never asserted into any fact table.
% ===========================================================================

% --- (d) POSITIVE CONTROL — the coordination_scaffold probe FIRES on a
%   constructed true-slot profile. Without this, (e)/(f) are vacuous: a probe
%   that cannot fire proves nothing by not firing.
%   profile/7: Access, Suppression, Resistance, BeneficiaryCount,
%   HasAlternatives, TemporalStability, CoordinationSuccess.
test(positive_control_coordination_scaffold_fires) :-
    signature_detection:coordination_scaffold_signature(
        profile(0.92, 0.02, 0.04, 0, true, stable, _)).

% --- (e) DISCRIMINATION — the same probe DECLINES on the value the engine
%   actually produces. Fires on `true`, declines on `unknown`: the firing in
%   (d) carries information rather than merely showing the probe is alive.
test(coordination_scaffold_declines_on_unknown) :-
    \+ signature_detection:coordination_scaffold_signature(
        profile(0.92, 0.02, 0.04, 0, unknown, stable, _)).

% --- (f) LIVE CORPUS — coordination_scaffold fires on ZERO constraints.
test(coordination_scaffold_corpus_zero) :-
    corpus_loader:ensure_corpus_loaded,
    findall(C,
            ( corpus_loader:corpus_constraint(C),
              signature_detection:get_constraint_profile(C, P),
              signature_detection:coordination_scaffold_signature(P) ),
            L),
    assertion(L == []).

% --- (g) RANGE WITNESS, other side — has_viable_alternatives/2 never returns
%   `true` either. Complements (c). (c) + (g) together are the CONSTANCY claim:
%   neither branch value is reachable, so the only live value is `unknown`.
%   If GAP-08 §7 lands and the predicate starts discriminating, THIS test goes
%   red first — which is the intended alarm, not a regression.
test(has_viable_alternatives_never_true) :-
    corpus_loader:ensure_corpus_loaded,
    forall(corpus_loader:corpus_constraint(C),
           ( signature_detection:has_viable_alternatives(C, V),
             V \== true )).

% --- (h) CONSTANCY — the range is the single value {unknown}. Stated directly
%   rather than inferred from (c)+(g), so the failure message names the fact.
test(has_viable_alternatives_is_constant_unknown) :-
    corpus_loader:ensure_corpus_loaded,
    findall(V, ( corpus_loader:corpus_constraint(C),
                 signature_detection:has_viable_alternatives(C, V) ), Vs),
    sort(Vs, Range),
    assertion(Range == [unknown]).

:- end_tests(oq113_dead_natural_law).
