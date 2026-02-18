% ============================================================================
% CONSTRAINT STORY: global_water_bankruptcy
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_global_water_bankruptcy, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: global_water_bankruptcy
 * human_readable: The Global Water Bankruptcy Constraint
 * domain: environmental/economic
 * * SUMMARY:
 * Humanity is currently living beyond its hydrological means, creating a
 * state of "bankruptcy" where demand exceeds sustainable supply.
 * This imposes a severe constraint on peace, food security, and
 * biodiversity protection.
 * * KEY AGENTS:
 * - Vulnerable Local Populations: Subject (Powerless/Trapped in scarcity)
 * - UN Water / Water-Secure States: Beneficiary (Institutional/Coordinating)
 * - UNU-INWEH Researchers: Auditor (Analytical/Diagnostic)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is extremely high (0.85) as bankruptcy extracts the physical
% basis for life and stability.
domain_priors:base_extractiveness(global_water_bankruptcy, 0.85).
domain_priors:suppression_score(global_water_bankruptcy, 0.60).
domain_priors:theater_ratio(global_water_bankruptcy, 0.40).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(global_water_bankruptcy, extractiveness, 0.85).
narrative_ontology:constraint_metric(global_water_bankruptcy, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(global_water_bankruptcy, theater_ratio, 0.40).

% Constraint self-claim (what the constraint's proponents claim it is)
% The UN and member states frame their interventions as coordination.
narrative_ontology:constraint_claim(global_water_bankruptcy, tangled_rope).
narrative_ontology:human_readable(global_water_bankruptcy, "The Global Water Bankruptcy Constraint").
narrative_ontology:topic_domain(global_water_bankruptcy, "environmental/economic").

% Binary flags
domain_priors:requires_active_enforcement(global_water_bankruptcy). % Mandatory monitoring and policy enforcement required.

% Structural property derivation hooks for Tangled Rope:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(global_water_bankruptcy, water_secure_nations).
narrative_ontology:constraint_beneficiary(global_water_bankruptcy, agribusiness_conglomerates).
narrative_ontology:constraint_victim(global_water_bankruptcy, vulnerable_populations).
narrative_ontology:constraint_victim(global_water_bankruptcy, local_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SCARCITY-IMPACTED INDIVIDUAL (SNARE)
% Water bankruptcy is experienced as a predatory trap where basic needs
% are extracted by hydrological debt.
constraint_indexing:constraint_classification(global_water_bankruptcy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE UN WATER SYSTEM (ROPE)
% Viewed as a coordination framework (e.g., Water Action Decade) to
% reset the global agenda and manage the crisis.
constraint_indexing:constraint_classification(global_water_bankruptcy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE HYDROLOGICAL SCIENTIST (TANGLED ROPE)
% Detects how policy "milestones" act as coordination (Rope) while
% failing to stop the deep, asymmetric extraction of the bankruptcy itself.
constraint_indexing:constraint_classification(global_water_bankruptcy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_water_bankruptcy_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institutional actors.
    constraint_indexing:constraint_classification(global_water_bankruptcy, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_water_bankruptcy, rope, context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    % The analytical observer must classify this as a tangled_rope, resolving the gap.
    constraint_indexing:constraint_classification(global_water_bankruptcy, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties_are_met) :-
    % Verify the structural requirements for a Tangled Rope are declared.
    domain_priors:requires_active_enforcement(global_water_bankruptcy),
    narrative_ontology:constraint_beneficiary(global_water_bankruptcy, _),
    narrative_ontology:constraint_victim(global_water_bankruptcy, _).

:- end_tests(global_water_bankruptcy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) is critical because living beyond
 * "hydrological means" is a net-loss scenario that extracts the physical
 * basis for life and stability from vulnerable groups. The perspectival gap
 * is stark: institutional actors view their conferences and frameworks as
 * coordination 'Ropes', while those trapped in scarcity experience a 'Snare'.
 * The analytical observer resolves this gap by classifying the entire system
 * as a 'Tangled Rope'. This acknowledges the genuine (but failing) coordination
 * functions while correctly identifying the severe, asymmetric extraction
 * that defines the constraint's primary effect.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * Mandatrophy is resolved by the Tangled Rope classification. It prevents the
 * system from making two key errors: 1) misclassifying the constraint as a
 * pure Snare, which would ignore the real coordination efforts underway, or
 * 2) misclassifying it as a Rope, which would ignore the devastating and
 * asymmetric extraction. The Tangled Rope captures the hybrid reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_bankruptcy_reset,
    'Will the international coordination frameworks successfully "reset" the agenda and reverse the deficit?',
    'Analysis of global hydrological monitoring data and policy implementation effectiveness by 2030.',
    'Success = Transition toward a stable Rope; Failure = Degeneration into a kinetic Snare (resource conflict).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
% The interval represents the worsening crisis period from the mid-2010s to late 2020s.
narrative_ontology:interval(global_water_bankruptcy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint models the worsening
% of the water bankruptcy over the interval. Extraction increases as deficits
% grow, and theater increases as international conferences produce declarations
% with limited impact on the ground.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(gwb_tr_t0, global_water_bankruptcy, theater_ratio, 0, 0.30).
narrative_ontology:measurement(gwb_tr_t5, global_water_bankruptcy, theater_ratio, 5, 0.35).
narrative_ontology:measurement(gwb_tr_t10, global_water_bankruptcy, theater_ratio, 10, 0.40).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(gwb_ex_t0, global_water_bankruptcy, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(gwb_ex_t5, global_water_bankruptcy, base_extractiveness, 5, 0.80).
narrative_ontology:measurement(gwb_ex_t10, global_water_bankruptcy, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The UN frameworks are a form of global resource allocation planning.
narrative_ontology:coordination_type(global_water_bankruptcy, resource_allocation).

% Network relationships (structural influence edges)
% Water bankruptcy is a direct driver of food insecurity.
narrative_ontology:affects_constraint(global_water_bankruptcy, global_food_security).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */