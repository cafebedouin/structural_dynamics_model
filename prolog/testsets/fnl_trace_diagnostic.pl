% lint: skip
% ============================================================================
% FNL SHADOW MODE DIAGNOSTIC TRACE
% ============================================================================
% Purpose: Traces the full execution path for fnl_shadow_probe to prove
% whether Boltzmann-derived overrides are LIVE or truly shadow-only.
%
% Usage:
%   cd prolog/
%   swipl -g "[validation_suite], consult('testsets/fnl_trace_diagnostic'), halt."
% ============================================================================

:- use_module(scenario_manager).
:- use_module(structural_signatures).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(narrative_ontology).

:- initialization(run_fnl_trace).

run_fnl_trace :-
    format('~n~`=t~60|~n'),
    format('  FNL SHADOW MODE DIAGNOSTIC TRACE~n'),
    format('~`=t~60|~n~n'),

    % Step 1: Load the probe through normal pipeline
    format('[STEP 1] Loading fnl_shadow_probe through pipeline...~n'),
    scenario_manager:load_and_run('testsets/fnl_shadow_probe.pl', fnl_shadow_probe),

    format('~n~`=t~60|~n'),
    format('  DIAGNOSTIC RESULTS~n'),
    format('~`=t~60|~n~n'),

    % Step 2: Capture metric-based classification (before override)
    format('[STEP 2] Metric-based classification (pre-override):~n'),
    constraint_indexing:default_context(DefaultCtx),
    (   drl_core:metric_based_type_indexed(fnl_shadow_probe, DefaultCtx, MetricType)
    ->  format('  metric_based_type_indexed = ~w~n', [MetricType])
    ;   format('  metric_based_type_indexed = FAILED~n'),
        MetricType = unknown
    ),

    % Step 3: Capture structural signature
    format('~n[STEP 3] Structural signature:~n'),
    (   structural_signatures:constraint_signature(fnl_shadow_probe, Sig)
    ->  format('  constraint_signature = ~w~n', [Sig])
    ;   format('  constraint_signature = FAILED~n'),
        Sig = none
    ),

    % Step 4: Capture Boltzmann compliance
    format('~n[STEP 4] Boltzmann compliance:~n'),
    (   structural_signatures:boltzmann_compliant(fnl_shadow_probe, BResult)
    ->  format('  boltzmann_compliant = ~w~n', [BResult])
    ;   format('  boltzmann_compliant = FAILED~n')
    ),

    % Step 5: Capture cross-index coupling
    format('~n[STEP 5] Cross-index coupling:~n'),
    (   structural_signatures:cross_index_coupling(fnl_shadow_probe, CScore)
    ->  format('  cross_index_coupling = ~w~n', [CScore])
    ;   format('  cross_index_coupling = FAILED~n')
    ),

    % Step 6: Capture final classification (post-override)
    format('~n[STEP 6] Final classification (post-override via dr_type/3):~n'),
    (   drl_core:dr_type(fnl_shadow_probe, DefaultCtx, FinalType)
    ->  format('  dr_type = ~w~n', [FinalType])
    ;   format('  dr_type = FAILED~n'),
        FinalType = unknown
    ),

    % Step 7: Compare and report
    format('~n~`=t~60|~n'),
    format('  OVERRIDE ANALYSIS~n'),
    format('~`=t~60|~n~n'),
    format('  MetricType  = ~w~n', [MetricType]),
    format('  Signature   = ~w~n', [Sig]),
    format('  FinalType   = ~w~n', [FinalType]),

    (   MetricType \= FinalType
    ->  format('~n  >>> OVERRIDE IS ACTIVE <<<~n'),
        format('  The structural signature (~w) overrode the metric~n', [Sig]),
        format('  classification (~w -> ~w).~n', [MetricType, FinalType]),
        format('  "SHADOW MODE" comments are STALE.~n')
    ;   format('~n  >>> NO OVERRIDE DETECTED <<<~n'),
        format('  MetricType == FinalType. Either the override did not~n'),
        format('  fire, or both paths agree on the same type.~n')
    ),

    % Step 8: Check all override rules
    format('~n~`=t~60|~n'),
    format('  OVERRIDE RULE STATUS~n'),
    format('~`=t~60|~n~n'),

    format('  Rule NL  (natural_law -> mountain):      '),
    check_rule_live(natural_law, mountain),

    format('  Rule FNL (false_natural_law -> tangled):  '),
    check_rule_live(false_natural_law, tangled_rope),

    format('  Rule CIR (coupling_invariant -> rope):    '),
    check_rule_live(coupling_invariant_rope, rope),

    format('  Rule FCR (false_ci_rope -> tangled):      '),
    check_rule_live(false_ci_rope, tangled_rope),

    format('~n~`=t~60|~n'),
    format('  END OF DIAGNOSTIC~n'),
    format('~`=t~60|~n~n').

%% check_rule_live(+Signature, +ExpectedOverride)
%  Verifies that resolve_modal_signature_conflict actually maps
%  the given signature to the expected override type.
check_rule_live(Signature, Expected) :-
    (   structural_signatures:resolve_modal_signature_conflict(unknown, Signature, Result)
    ->  (   Result == Expected
        ->  format('ACTIVE (maps to ~w)~n', [Result])
        ;   format('UNEXPECTED (maps to ~w, expected ~w)~n', [Result, Expected])
        )
    ;   format('NOT REACHABLE~n')
    ).
