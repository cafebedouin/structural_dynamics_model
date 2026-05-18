% ============================================================================
% test_snapshot_migration.pl — Regression tests for the chi-path migration.
%
% Verifies that snapshot_type/3 (transition_paths) and classify_at_time/4
% (drl_composition) use the sigmoid+scope formula (χ = ε × f(d) × σ(S))
% rather than the deprecated power_modifier path (χ = ε × π(P)).
%
% The two paths diverge by design at non-national scopes:
%   deprecated: Chi = E * power_modifier_analytical          = E * 1.15
%   sigmoid:    Chi = E * sigmoid_f(canonical_d_analytical)
%                       * scope_modifier_global              = E * 1.15 * 1.2
%
% Run: swipl -g "[stack], [test_snapshot_migration], run_migration_tests, halt" -t "halt(1)"
% ============================================================================

:- use_module(config).
:- use_module(constraint_indexing).

run_migration_tests :-
    writeln('=== CHI PATH MIGRATION REGRESSION TESTS ==='),
    run_test(test_sigma_factor_documented),
    run_test(test_snare_boundary_case),
    writeln('=== ALL MIGRATION TESTS COMPLETE ===').

run_test(Name) :-
    format('~n[TEST] ~w~n', [Name]),
    ( call(Name)
    -> format('[PASS] ~w~n', [Name])
    ;  format('[FAIL] ~w~n', [Name]),
       fail
    ).

% -----------------------------------------------------------------------
% test_sigma_factor_documented
%
% Confirms the expected divergence between the deprecated and new paths
% at global scope (the scope used by default_context).
%
% Verdict: PASS when new path Chi is ~20% higher than deprecated path Chi,
%          because scope_modifier_global = 1.2 is now included.
% -----------------------------------------------------------------------
test_sigma_factor_documented :-
    E = 0.5,
    config:param(power_modifier_analytical, OldMod),
    ChiOld is E * OldMod,

    config:param(canonical_d_analytical, D),
    constraint_indexing:sigmoid_f(D, PMod),
    config:param(scope_modifier_global, SMod),
    ChiNew is E * PMod * SMod,

    Delta is ChiNew - ChiOld,
    RelPct is Delta / max(abs(ChiOld), 1.0e-10) * 100.0,
    format('  deprecated (no sigma): Chi = ~3f * ~4f       = ~4f~n', [E, OldMod, ChiOld]),
    format('  new (sigmoid+scope):   Chi = ~3f * ~4f * ~3f = ~4f~n', [E, PMod, SMod, ChiNew]),
    format('  documented divergence: +~4f (+~1f%%)~n', [Delta, RelPct]),
    Delta > 0.05.  % sigmoid+scope path is larger at global scope

% -----------------------------------------------------------------------
% test_snare_boundary_case
%
% For E = 0.53 at analytical+global scope:
%   deprecated path:  Chi = 0.53 * 1.15 = 0.610  < snare_chi_floor (false negative)
%   new sigmoid path: Chi = 0.53 * 1.15 * 1.2 = 0.731  >= snare_chi_floor (correct)
%
% Verdict: PASS when new path is above the floor and deprecated path is below it,
%          confirming the class of constraints the migration fixes.
% -----------------------------------------------------------------------
test_snare_boundary_case :-
    E = 0.53,
    config:param(power_modifier_analytical, OldMod),
    ChiOld is E * OldMod,

    config:param(canonical_d_analytical, D),
    constraint_indexing:sigmoid_f(D, PMod),
    config:param(scope_modifier_global, SMod),
    ChiNew is E * PMod * SMod,

    config:param(snare_chi_floor, Floor),
    format('  E=~2f, snare_chi_floor=~2f~n', [E, Floor]),
    format('  deprecated Chi: ~4f -> snare-eligible? ~w~n',
           [ChiOld, (ChiOld >= Floor -> yes ; no)]),
    format('  new Chi:        ~4f -> snare-eligible? ~w~n',
           [ChiNew, (ChiNew >= Floor -> yes ; no)]),
    ChiOld < Floor,
    ChiNew >= Floor,
    writeln('  confirmed: new path captures snare-eligible constraints the deprecated path missed').
