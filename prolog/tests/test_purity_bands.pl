% ============================================================================
% test_purity_bands.pl — OQ-62 purity-bander negative-guard regression tests.
%
% The repo bands the purity scalar in FOUR places with overlapping vocabularies
% (OQ-62). This file owns ONE property, held by all four: a band input that is
% not a purity VALUE must fail closed to zone `unknown`, never to the bander's
% worst zone.
%
% Two absence tokens reach these banders (OQ-60):
%   -1.0    epistemic-gate-fail sentinel
%   unknown no-data (no authored coordination_type -> no Boltzmann floor)
% Pre-fix, the four banders mishandled BOTH in two different ways, witnessed:
%   -1.0    -> silently banded WORST (fpn_zone=critical, spec/GCA=degraded,
%              fpn_report=critical) — a no-access constraint reads as the most
%              contaminated thing in the corpus.
%   unknown -> GCA and fpn_zone THREW type_error(evaluable, unknown/0); the
%              other two already had a `\+ number` guard.
% The `< 0.0` clause must sit AFTER the `\+ number` clause: the comparison
% itself throws on the atom, so the order is load-bearing, not cosmetic.
%
% Division of labour with test_purity_absence.pl: that file owns OQ-60 token
% totality (`unknown` must not throw downstream, section (c)); this file owns
% OQ-62's negative-guard cases and covers all four banders rather than the
% spec one alone.
%
% Run (needs an EXTENDED load chain — [stack] loads logical_fingerprint and
% abductive_helpers but NOT fpn_report or giant_component_analysis, so a bare
% `[stack]` run fails on the load chain rather than on the logic):
%   cd prolog && swipl -l stack.pl -l fpn_report.pl -l giant_component_analysis.pl \
%     -g "[tests/test_purity_bands], run_tests(purity_bands), halt" -t "halt(1)"
% ============================================================================

:- begin_tests(purity_bands).

% ----------------------------------------------------------------------------
% (0) Load-chain control — the suite must not pass by never dispatching.
% ----------------------------------------------------------------------------
% Without this, a short load chain makes every bander call below raise
% existence_error and the SUITE still reports the guard property as untested
% rather than as unreachable. This is the "absence satisfies the gate" shape:
% assert the four predicates are actually here before asserting anything about
% what they return.

test(all_four_banders_are_loaded) :-
    forall(
        member(M:P/A, [ logical_fingerprint:purity_zone/2,
                        fpn_report:purity_zone/2,
                        giant_component_analysis:purity_zone/2,
                        abductive_helpers:fpn_zone/2 ]),
        (   current_predicate(M:P/A)
        ->  true
        ;   format(user_error,
                   '~n[test_purity_bands] MISSING ~w:~w/~w — load chain is short.~n',
                   [M, P, A]),
            fail
        )).

% ----------------------------------------------------------------------------
% (1) Positive controls — each bander dispatches on a known-good in-range value.
% ----------------------------------------------------------------------------
% Two-sided: if a guard were written so broadly that it swallowed real scores,
% these catch it. They also prove the call path works, so an `unknown` verdict
% below is the guard firing and not a silent misdispatch.

test(control_spec_bander_dispatches) :-
    logical_fingerprint:purity_zone(0.75, Z),
    Z == sound.

test(control_fpn_report_bander_dispatches) :-
    fpn_report:purity_zone(0.75, Z),
    Z == sound.

test(control_gca_bander_dispatches) :-
    giant_component_analysis:purity_zone(0.75, Z),
    Z == sound.

test(control_fpn_zone_bander_dispatches) :-
    abductive_helpers:fpn_zone(0.75, Z),
    Z == clean.

% Lower-band controls: the guard must not shadow legitimately LOW scores.
% 0.10 is a real, terrible purity score and must still band worst — this is
% what separates "fail closed on absence" from "suppress bad news".

test(control_low_but_real_still_bands_worst) :-
    logical_fingerprint:purity_zone(0.10, Z1),      Z1 == degraded,
    fpn_report:purity_zone(0.10, Z2),               Z2 == critical,
    giant_component_analysis:purity_zone(0.10, Z3), Z3 == degraded,
    abductive_helpers:fpn_zone(0.10, Z4),           Z4 == critical.

% Boundary control: 0.0 is a real score (perfectly impure), NOT an absence.
% The guard is `< 0.0`, so exactly 0.0 must still reach the worst band.

test(control_zero_is_a_value_not_an_absence) :-
    logical_fingerprint:purity_zone(0.0, Z1),      Z1 == degraded,
    fpn_report:purity_zone(0.0, Z2),               Z2 == critical,
    giant_component_analysis:purity_zone(0.0, Z3), Z3 == degraded,
    abductive_helpers:fpn_zone(0.0, Z4),           Z4 == critical.

% ----------------------------------------------------------------------------
% (2) The -1.0 sentinel must fail closed to `unknown`, not to the worst zone.
% ----------------------------------------------------------------------------
% Pre-fix witnessed values, for the record:
%   spec -> degraded | fpn_report -> critical | GCA -> degraded | fpn_zone -> critical

test(spec_bander_sentinel_is_unknown) :-
    logical_fingerprint:purity_zone(-1.0, Z),
    Z == unknown.

test(fpn_report_bander_sentinel_is_unknown) :-
    fpn_report:purity_zone(-1.0, Z),
    Z == unknown.

test(gca_bander_sentinel_is_unknown) :-
    giant_component_analysis:purity_zone(-1.0, Z),
    Z == unknown.

test(fpn_zone_bander_sentinel_is_unknown) :-
    abductive_helpers:fpn_zone(-1.0, Z),
    Z == unknown.

% ----------------------------------------------------------------------------
% (3) The `unknown` atom must return `unknown`, not throw.
% ----------------------------------------------------------------------------
% Pre-fix, the two unguarded banders raised type_error(evaluable, unknown/0).
% These two cases are the standing record that a LOUD failure was converted to
% a silent fail-closed token — the call-site census (audits/2026-07-25_oq62_
% band_vocabulary_fork/CALL_SITE_CENSUS.md) is what licenses that conversion.

test(spec_bander_atom_is_unknown) :-
    logical_fingerprint:purity_zone(unknown, Z),
    Z == unknown.

test(fpn_report_bander_atom_is_unknown) :-
    fpn_report:purity_zone(unknown, Z),
    Z == unknown.

test(gca_bander_atom_is_unknown) :-
    giant_component_analysis:purity_zone(unknown, Z),
    Z == unknown.

test(fpn_zone_bander_atom_is_unknown) :-
    abductive_helpers:fpn_zone(unknown, Z),
    Z == unknown.

% ----------------------------------------------------------------------------
% (4) Clause ORDER is load-bearing, not cosmetic.
% ----------------------------------------------------------------------------
% If `< 0.0` were placed before `\+ number`, the comparison would throw on the
% atom. Section (3) already covers that, but this states the property directly
% so a future reordering fails with a name that says why.

test(guard_order_atom_before_comparison) :-
    forall(
        member(Goal, [ logical_fingerprint:purity_zone(unknown, _),
                       fpn_report:purity_zone(unknown, _),
                       giant_component_analysis:purity_zone(unknown, _),
                       abductive_helpers:fpn_zone(unknown, _) ]),
        catch(( call(Goal) -> true ; true ), E,
              ( format(user_error,
                       '~n[test_purity_bands] ~w threw ~w — is `< 0.0` ordered before the `\\+ number` guard?~n',
                       [Goal, E]),
                fail ))).

:- end_tests(purity_bands).
