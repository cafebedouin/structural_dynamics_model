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

:- end_tests(oq113_dead_natural_law).
