% lint: skip
% ============================================================================
% CONSTRAINT STORY: repair_probe_incomplete
% ============================================================================
% Purpose: Investigation 3 probe -- deliberately incomplete testset to verify
% data_repair.pl bridge logic. Missing constraint_claim, suppression metric,
% and beneficiary/victim facts. Has constraint_classification and
% base_extractiveness to provide bridge sources.
% ============================================================================

:- module(constraint_repair_probe_incomplete, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: repair_probe_incomplete
 * human_readable: Data Repair Bridge Probe (Deliberately Incomplete)
 * domain: investigation/testing
 *
 * SUMMARY:
 * A minimal testset designed to trigger data_repair.pl bridge paths.
 * Deliberately omits constraint_claim, suppression metric, and
 * beneficiary/victim facts to verify bridge derivation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Provide extractiveness but OMIT suppression_score to test metric bridging
domain_priors:base_extractiveness(repair_probe_incomplete, 0.55).
% suppression_score deliberately MISSING -- bridge should derive from config default
domain_priors:theater_ratio(repair_probe_incomplete, 0.15).

% Provide extractiveness metric directly but OMIT suppression_requirement
narrative_ontology:constraint_metric(repair_probe_incomplete, extractiveness, 0.55).
narrative_ontology:constraint_metric(repair_probe_incomplete, theater_ratio, 0.15).
% suppression_requirement metric deliberately MISSING

% constraint_claim deliberately MISSING -- bridge should derive from classification
% constraint_beneficiary deliberately MISSING -- bridge should derive from metrics
% constraint_victim deliberately MISSING -- bridge should derive from metrics

% Binary flags
domain_priors:requires_active_enforcement(repair_probe_incomplete).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS SUBJECT (SNARE)
constraint_indexing:constraint_classification(repair_probe_incomplete, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INSTITUTIONAL POWER (ROPE)
constraint_indexing:constraint_classification(repair_probe_incomplete, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE)
constraint_indexing:constraint_classification(repair_probe_incomplete, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(repair_probe_incomplete_tests).

test(bridge_creates_constraint_claim) :-
    % After repair, constraint_claim should exist (bridged from analytical classification)
    narrative_ontology:constraint_claim(repair_probe_incomplete, _Type).
narrative_ontology:human_readable(repair_probe_incomplete, "Data Repair Bridge Probe (Deliberately Incomplete)").

test(bridge_creates_suppression_metric) :-
    % After repair, suppression metric should exist (bridged from domain_priors or config default)
    narrative_ontology:constraint_metric(repair_probe_incomplete, suppression_requirement, _V).

test(extractiveness_preserved) :-
    % Extractiveness should remain as declared
    narrative_ontology:constraint_metric(repair_probe_incomplete, extractiveness, E),
    E =:= 0.55.

test(perspectival_gap) :-
    % Verify perspectival gap exists
    constraint_indexing:constraint_classification(repair_probe_incomplete, TypeP, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(repair_probe_incomplete, TypeI, context(agent_power(institutional), _, _, _)),
    TypeP \= TypeI.

test(bridge_v34_returns_constraint_claim) :-
    % Pure API test: bridge_v34_data returns bridge_result(constraint_claim(...))
    % for a constraint with domain priors but no pre-existing constraint_claim.
    % This exercises the setup_call_cleanup bracket in bridge_constraint_claim_pure.
    data_repair:bridge_v34_data(repair_probe_incomplete, Results),
    member(bridge_result(constraint_claim(repair_probe_incomplete, Type)), Results),
    Type \= unknown.

test(bridge_v34_no_stale_constraint_metric) :-
    % Cleanup proof: after bridge_v34_data returns, no temp-asserted
    % constraint_metric facts from the bracket should remain in the DB
    % beyond what was declared in the testset. The setup_call_cleanup
    % bracket in bridge_constraint_claim_pure/3 must erase all Refs.
    %
    % Strategy: collect metrics before bridge, run bridge, collect after.
    % Any metric present after but not before (and not in bridge results)
    % would be a stale leaked fact.
    findall(K-V, narrative_ontology:constraint_metric(repair_probe_incomplete, K, V), MetricsBefore),
    data_repair:bridge_v34_data(repair_probe_incomplete, Results),
    findall(K2-V2, narrative_ontology:constraint_metric(repair_probe_incomplete, K2, V2), MetricsAfter),
    % After bridge, the only new metrics should be ones in the Results list
    findall(K3-V3,
        (   member(K3-V3, MetricsAfter),
            \+ member(K3-V3, MetricsBefore),
            \+ member(bridge_result(constraint_metric(repair_probe_incomplete, K3, V3)), Results)
        ),
        Stale),
    Stale == [].

:- end_tests(repair_probe_incomplete_tests).

/* ==========================================================================
   5. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(repair_probe_incomplete, 0, 10).

/* ==========================================================================
   6. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

narrative_ontology:measurement(rpi_tr_t0, repair_probe_incomplete, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rpi_tr_t5, repair_probe_incomplete, theater_ratio, 5, 0.12).
narrative_ontology:measurement(rpi_tr_t10, repair_probe_incomplete, theater_ratio, 10, 0.15).

narrative_ontology:measurement(rpi_ex_t0, repair_probe_incomplete, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(rpi_ex_t5, repair_probe_incomplete, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(rpi_ex_t10, repair_probe_incomplete, base_extractiveness, 10, 0.55).

/* ==========================================================================
   7. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(repair_probe_incomplete, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
