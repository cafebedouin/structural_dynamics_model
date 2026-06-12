:- module(test_harness, [
    load_scenario/1,
    run_all_tests/0,    % New: One-button alias
    run_all_tests/1,
    run_all_tests/2,
    quick_check/1
]).

:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(coercion_projection).
:- use_module(data_verification).
:- use_module(pattern_analysis).
:- use_module(intent_engine).
:- use_module(constraint_bridge).
:- use_module(constraint_indexing).
:- use_module(drl_core).
:- use_module(drl_lifecycle).
:- use_module(report_generator).
:- use_module(logical_fingerprint).
:- use_module(measurement_layer).

% Run all registered test cases from validation_suite, or fall back to single ID
run_all_tests :-
    (   current_predicate(validation_suite:test_case/4)
    ->  findall(ID, validation_suite:test_case(_, ID, _, _), IDs),
        sort(IDs, UniqueIDs),
        length(UniqueIDs, N),
        format('~n>>> Running ~w registered test intervals~n', [N]),
        forall(member(IntervalID, UniqueIDs),
               (   catch(run_all_tests(IntervalID), E,
                         format('[ERROR] ~w failed: ~w~n', [IntervalID, E]))
               ))
    ;   format('~n>>> No validation_suite loaded, running default interval~n'),
        run_all_tests('tax_code_section_469')
    ).

run_all_tests(IntervalID) :-
    constraint_indexing:default_context(Ctx),
    run_all_tests(IntervalID, Ctx).

run_all_tests(IntervalID, _Context) :-
    format('~n>>> INITIATING DR-AUDIT SUITE: ~w~n', [IntervalID]),

    % Step 1: Verification Gate
    (   data_verification:verify_all,
        data_verification:check_paired_measurements
    ->  format('[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).~n')
    ;   format('[FAIL] Verification failed for ~w.~n', [IntervalID]), fail),

    % Step 2: Per-Index Validation
    % For each declared classification, compute dr_type and compare.
    % Mismatches are informational - they show where domain priors
    % differ from computed classifications (desired behavior).
    validate_per_index(IntervalID),

    % Step 4: Intent and Reporting
    intent_engine:analyze_intent(IntervalID),
    constraint_bridge:derive_diagnostic_state(IntervalID),

    % Step 5: Lifecycle Drift Analysis
    format('~n--- LIFECYCLE DRIFT ANALYSIS ---~n'),
    drl_lifecycle:generate_drift_report,

    % Step 5b: Scope Effect Analysis
    format('~n--- SCOPE EFFECT ANALYSIS ---~n'),
    format('  Formula: χ = ε × f(d) × σ(S)~n'),
    forall(narrative_ontology:constraint_claim(C, _),
           report_scope_effect(C)),

    % Step 5b2: Wasserstein Transport Analysis
    format('~n--- WASSERSTEIN TRANSPORT ---~n'),
    forall(narrative_ontology:constraint_claim(CW, _),
           report_wasserstein_transport(CW)),
    (   catch(measurement_layer:wasserstein_corpus_fracture(CorpusW1), _, fail)
    ->  format('  Corpus perspectival fracture (W1): ~4f~n', [CorpusW1])
    ;   true
    ),

    % Step 5c: Logical Fingerprint
    format('~n--- LOGICAL FINGERPRINT ---~n'),
    forall(narrative_ontology:constraint_claim(C2, _),
           ( catch(logical_fingerprint:print_fingerprint(C2), _, true) )),

    % Step 6: Full Report (includes omega generation + triage)
    report_generator:generate_full_report(IntervalID).

% Ensure quick_check uses the predicates from the authoritative module
quick_check(IntervalID) :-
    format('--- Diagnostic: ~w ---~n', [IntervalID]),
    (   drl_core:base_extractiveness(Name, E), E > 0.8
    ->  format('CRITICAL EXTRACTIVENESS: ~w (~2f)~n', [Name, E])
    ;   format('No critical extractiveness detected.~n')).

%% load_scenario(+Path)
load_scenario(Path) :-
    exists_file(Path),
    consult(Path),
    format('~n[SCENARIO] Successfully loaded: ~w~n', [Path]).
load_scenario(Path) :-
    \+ exists_file(Path),
    format('~n[ERROR] Scenario file not found: ~w~n', [Path]),
    fail.

%% validate_per_index(+IntervalID)
%  For each declared classification, compute dr_type and compare.
%  Mismatches between declared and computed types are informational -
%  they indicate sites where domain priors differ from metric-based
%  computation, which is the desired behavior (priors as input, not ground truth).
validate_per_index(IntervalID) :-
    format('~n--- PER-INDEX VALIDATION ---~n'),
    % Empty-table census A2 fix (OQ-109 B3, 2026-06-12): the bare forall was
    % VACUOUSLY TRUE on a perspectives-free story — the section printed a clean
    % header having checked nothing (silent fail-open). Carry the ran-witness:
    % count first, log vacuity explicitly when zero.
    findall(Ctx-DeclaredType,
            constraint_indexing:constraint_classification(IntervalID, DeclaredType, Ctx),
            Pairs),
    length(Pairs, NPairs),
    (   NPairs =:= 0
    ->  format('  [INDEX VACUOUS] ~w: no authored classifications — ZERO per-index checks ran (not a clean pass)~n',
               [IntervalID])
    ;   format('  (~w authored classifications examined)~n', [NPairs]),
        forall(
            member(Ctx-DeclaredType, Pairs),
            (   catch(drl_core:dr_type(IntervalID, Ctx, ComputedType), _, fail)
            ->  (   DeclaredType = ComputedType
                ->  format('  [INDEX OK] ~w from ~w: declared=~w, computed=~w~n',
                           [IntervalID, Ctx, DeclaredType, ComputedType])
                ;   format('  [INDEX MISMATCH] ~w from ~w: declared=~w, computed=~w~n',
                           [IntervalID, Ctx, DeclaredType, ComputedType])
                )
            ;   format('  [INDEX ERROR] ~w: dr_type failed for context ~w~n',
                       [IntervalID, Ctx])
            )
        )
    ).

%% report_scope_effect(+Constraint)
%  Shows how scope modifier differentiates chi across standard contexts.
%  Formula: χ = ε × f(d) × σ(S)
%  v6.0: Uses structural directionality derivation chain instead of legacy π(P).
report_scope_effect(C) :-
    (   drl_core:base_extractiveness(C, Epsilon)
    ->  format('  ~w (ε=~2f):~n', [C, Epsilon]),
        forall(
            member(Power, [powerless, moderate, institutional, analytical]),
            (   scope_effect_standard_context(Power, Ctx),
                Ctx = context(agent_power(Power), _, _, spatial_scope(Scope)),
                constraint_indexing:resolve_coalition_power(Power, C, ResolvedPower),
                Ctx = context(_, T, E, S),
                ResolvedCtx = context(agent_power(ResolvedPower), T, E, S),
                constraint_indexing:derive_directionality(C, ResolvedCtx, D),
                constraint_indexing:sigmoid_f(D, FD),
                constraint_indexing:scope_modifier(Scope, Sigma),
                Chi is Epsilon * FD * Sigma,
                (   ResolvedPower \= Power
                ->  format('    ~w→~w@~w: d=~3f f(d)=~2f χ = ~2f × ~2f × ~2f = ~3f~n',
                           [Power, ResolvedPower, Scope, D, FD, Epsilon, FD, Sigma, Chi])
                ;   format('    ~w@~w: d=~3f f(d)=~2f χ = ~2f × ~2f × ~2f = ~3f~n',
                           [Power, Scope, D, FD, Epsilon, FD, Sigma, Chi])
                )
            )
        )
    ;   format('  ~w: no extractiveness data~n', [C])
    ).

%% scope_effect_standard_context(+PowerLevel, -Context)
%  Standard contexts for scope effect analysis, matching logical_fingerprint.pl.
scope_effect_standard_context(powerless,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).
scope_effect_standard_context(moderate,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).
scope_effect_standard_context(institutional,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).
scope_effect_standard_context(analytical,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

%% report_wasserstein_transport(+Constraint)
%  Shows Wasserstein L1 transport profile across the 4 canonical observer
%  positions. Requires MaxEnt distributions at all 4 contexts (silently
%  skips if unavailable).
report_wasserstein_transport(C) :-
    (   catch(measurement_layer:wasserstein_transport_profile(C, Profile), _, fail)
    ->  Profile = transport_profile(edge(u1_u2, W12), edge(u2_u3, W23), edge(u3_u4, W34)),
        TotalW is W12 + W23 + W34,
        format('  ~w:~n', [C]),
        format('    Edge U1~cU2: ~4f | U2~cU3: ~4f | U3~cU4: ~4f | Total: ~4f~n',
               [0x2192, W12, 0x2192, W23, 0x2192, W34, TotalW]),
        measurement_layer:wasserstein_contexts([Ctx1, Ctx2, Ctx3, Ctx4]),
        forall(
            (   member(Ctx-Label, [Ctx1-u1, Ctx2-u2, Ctx3-u3, Ctx4-u4]),
                catch(measurement_layer:wasserstein_incomparable_mass(C, Ctx, Mass), _, fail),
                Mass > 0.4
            ),
            format('    ~c High incomparable mass at ~w: ~4f ~c W1 estimate partial~n',
                   [0x26A0, Label, Mass, 0x2014])
        )
    ;   true  % silently skip — MaxEnt data not available at all 4 contexts
    ).
