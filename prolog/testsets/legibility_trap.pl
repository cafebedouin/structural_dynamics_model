% ============================================================================
% CONSTRAINT STORY: legibility_trap
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_legibility_trap, []).

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
 * * constraint_id: legibility_trap
 * human_readable: The Grid-Map Displacement
 * domain: political/social/economic
 * * SUMMARY:
 * A scenario where a state or institution imposes simplified, standardized
 * metrics (legibility) onto a complex, organic social system to make it
 * "governable." This "Rope" for administrative coordination becomes a
 * "Snare" as the high-resolution local knowledge and informal safety nets
 * are liquidated by the rigid "map," trapping the subject in a brittle
 * system that cannot account for their actual needs.
 * * KEY AGENTS:
 * - Local Resident: Subject (Powerless)
 * - Central Planner: Beneficiary (Institutional)
 * - Systems Ecologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) reflects the total liquidation of the subject's
% informal agency and local diversity to feed the center's "legibility" need.
domain_priors:base_extractiveness(legibility_trap, 0.88).
domain_priors:suppression_score(legibility_trap, 0.79). % Informal alternatives are suppressed by the requirement for "standardized" data.
domain_priors:theater_ratio(legibility_trap, 0.92).    % Extreme theater: high-resolution "Dashboards" masking systemic hollowing.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(legibility_trap, extractiveness, 0.88).
narrative_ontology:constraint_metric(legibility_trap, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(legibility_trap, theater_ratio, 0.92).

% Constraint self-claim (what does the constraint claim to be?)
% The system claims to be a necessary tool for coordination and governance.
narrative_ontology:constraint_claim(legibility_trap, tangled_rope).
narrative_ontology:human_readable(legibility_trap, "The Grid-Map Displacement").

% Binary flags
domain_priors:requires_active_enforcement(legibility_trap). % Mandatory for Tangled Rope

% Structural property derivation hooks:
% Both are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(legibility_trap, central_planner).
narrative_ontology:constraint_victim(legibility_trap, local_resident).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The resident is trapped: they must fit into a standardized "box" to access
% resources, but the box liquidates the informal networks that actually kept them safe.
constraint_indexing:constraint_classification(legibility_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The planner views legibility as a Rope—the only way to coordinate large-scale
% taxation, service delivery, and order across a diverse population.
constraint_indexing:constraint_classification(legibility_trap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.88) and suppression (0.79) masking as essential
% coordination. The system has both a coordination function (beneficiary exists)
% and asymmetric extraction (victim exists), and requires enforcement. This is the
% canonical signature of a Tangled Rope. The high theater ratio (0.92) is a
% symptom of its degradation, not a reclassification to Piton, as extraction
% remains extremely high.
constraint_indexing:constraint_classification(legibility_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legibility_trap_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless resident vs Rope for the institutional planner.
    constraint_indexing:constraint_classification(legibility_trap, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legibility_trap, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(legibility_trap, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_conditions) :-
    % Verify that the structural conditions for a Tangled Rope are met.
    domain_priors:base_extractiveness(legibility_trap, E), E >= 0.50,
    domain_priors:suppression_score(legibility_trap, S), S >= 0.40,
    domain_priors:requires_active_enforcement(legibility_trap),
    narrative_ontology:constraint_beneficiary(legibility_trap, _),
    narrative_ontology:constraint_victim(legibility_trap, _).

:- end_tests(legibility_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a "Mandatrophy" state where the
 * "coordination" benefit of state-seeing is achieved by liquidating the
 * high-resolution agency of the subject population. The suppression score (0.79)
 * represents the active delegitimization of informal or local systems that
 * compete with the state's map. The extremely high theater ratio (0.92)
 * indicates that the system's outputs (dashboards, reports) are almost entirely
 * performative and disconnected from the reality on the ground, a common
 * failure mode for such systems.
 *
 * * PERSPECTIVAL GAP:
 * The Local Resident feels a Snare because their "legibility" to the state
 * makes them easier to extract from and harder to survive for. The Planner
 * sees a Rope because the "grid" coordinates the functioning of a modern
 * administrative state. The Analytical Observer sees a Tangled Rope, acknowledging
 * both the coordination claim and the severe, asymmetric extraction it enables.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. An observer might be confused by
 * the high theater ratio (0.92), which suggests a Piton. However, a Piton is a
 * low-extraction constraint. This system's extraction is extremely high (0.88).
 * The correct analysis is that this is a severely degraded Tangled Rope where
 * the coordination function has become almost entirely theatrical, but the
 * extractive function remains brutally effective. The system correctly avoids
 * misclassifying it as a harmless, inert Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_legibility_trap_resolution,
    'Can digital high-resolution sensing solve the "Trap", or is simplification an irreducible property of statecraft (Snare vs Mountain)?',
    'Tracking the success rate of AI-driven "informal economy" integration into formal state systems.',
    'If integration holds: Snare of current tech. If it fails: Mountain of Social Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(legibility_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the constraint's evolution from a well-intentioned coordination
% tool into a highly extractive and theatrical system.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(legibility_trap_tr_t0, legibility_trap, theater_ratio, 0, 0.10).
narrative_ontology:measurement(legibility_trap_tr_t5, legibility_trap, theater_ratio, 5, 0.50).
narrative_ontology:measurement(legibility_trap_tr_t10, legibility_trap, theater_ratio, 10, 0.92).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(legibility_trap_ex_t0, legibility_trap, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(legibility_trap_ex_t5, legibility_trap, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(legibility_trap_ex_t10, legibility_trap, base_extractiveness, 10, 0.88).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The state's legibility project is fundamentally about allocating resources
% and services based on its simplified map of the world.
narrative_ontology:coordination_type(legibility_trap, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */