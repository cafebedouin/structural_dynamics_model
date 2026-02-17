% ============================================================================
% CONSTRAINT STORY: container_capacity_mismatch
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_container_capacity_mismatch, []).

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
 * * constraint_id: container_capacity_mismatch
 * human_readable: The Volume-Infrastructure Paradox
 * domain: logistics/physical_infrastructure/economic
 * * SUMMARY:
 * A scenario where the volume of commodities or data produced by a
 * hyper-efficient "source" (Rope) vastly exceeds the physical or logical
 * capacity of the "containers" or "conduits" designed to transport them.
 * This mismatch acts as a "Snare" for the small-scale operator, whose
 * inventory agency is liquidated as they are forced to discard surplus or
 * pay exorbitant "congestion rent" to access the bottlenecked infrastructure,
 * trapping them in a state of terminal unprofitability despite high productivity.
 *
 * * KEY AGENTS:
 * - Independent Producer: Subject (Powerless)
 * - Infrastructure Gatekeeper: Beneficiary (Institutional)
 * - Supply Chain Forensic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(container_capacity_mismatch, 0.85). % High extraction reflects systematic liquidation of producer's surplus.
domain_priors:suppression_score(container_capacity_mismatch, 0.74).   % Alternative transport methods are suppressed by regulatory or physical monopoly.
domain_priors:theater_ratio(container_capacity_mismatch, 0.82).       % High theater: "Efficiency optimization" software that masks the physical bottleneck.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(container_capacity_mismatch, extractiveness, 0.85).
narrative_ontology:constraint_metric(container_capacity_mismatch, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(container_capacity_mismatch, theater_ratio, 0.82).

% Constraint self-claim (what does the constraint claim to be?)
% The system claims to be a necessary coordination mechanism for managing scarce resources.
narrative_ontology:constraint_claim(container_capacity_mismatch, tangled_rope).
narrative_ontology:human_readable(container_capacity_mismatch, "The Volume-Infrastructure Paradox").

% Binary flags
% Required for Tangled Rope: enforcement maintains the monopoly on infrastructure.
domain_priors:requires_active_enforcement(container_capacity_mismatch).

% Structural property derivation hooks:
% Required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(container_capacity_mismatch, infrastructure_gatekeeper).
narrative_ontology:constraint_victim(container_capacity_mismatch, independent_producer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The producer is trapped: they produce more value than ever, but the
% lack of "container" space liquidates their ability to realize that value.
constraint_indexing:constraint_classification(container_capacity_mismatch, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The gatekeeper views the mismatch as a Rope—the essential coordination
% substrate for maintaining high prices and ensuring "systemic stability"
% through throttled supply.
constraint_indexing:constraint_classification(container_capacity_mismatch, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.85) and suppression (0.74) masking as essential
% coordination. The presence of beneficiaries, victims, and active enforcement
% confirms the Tangled Rope structure.
constraint_indexing:constraint_classification(container_capacity_mismatch, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.82) > 0.70 triggers Piton: the "Congestion Management"
% protocol is an inertial spike; it signals fairness while enabling extraction.
% This is a valid sub-classification focusing on the system's decay.
constraint_indexing:constraint_classification(container_capacity_mismatch, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(container_capacity_mismatch_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless producer vs Rope for the institutional gatekeeper.
    constraint_indexing:constraint_classification(container_capacity_mismatch, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(container_capacity_mismatch, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(container_capacity_mismatch, tangled_rope, context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.82) correctly triggers the Piton classification.
    domain_priors:theater_ratio(container_capacity_mismatch, TR), TR > 0.70,
    constraint_indexing:constraint_classification(container_capacity_mismatch, piton, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify all three structural requirements for Tangled Rope are met.
    domain_priors:requires_active_enforcement(container_capacity_mismatch),
    narrative_ontology:constraint_beneficiary(container_capacity_mismatch, _),
    narrative_ontology:constraint_victim(container_capacity_mismatch, _).

:- end_tests(container_capacity_mismatch_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) reflects a "Mandatrophy" state where the
 * "coordination" benefit of organized logistics is achieved by liquidating
 * the subject's primary productive agency. The suppression score (0.74)
 * confirms that alternatives are actively prevented. The high theater ratio
 * (0.82) indicates that the system's stated purpose ("efficiency") has been
 * replaced by a performative one that masks the underlying extraction.
 *
 * * PERSPECTIVAL GAP:
 * The Independent Producer feels a Snare because their success creates
 * the very volume that destroys their margins. The Gatekeeper sees a Rope
 * because the bottleneck coordinates a lucrative, legible scarcity. The
 * Analytical Observer sees a Tangled Rope, recognizing both the coordination
 * function and the severe asymmetric extraction it enables.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. This prevents the system from
 * collapsing into a simple Snare (ignoring the coordination function) or a
 * Rope (ignoring the extraction). The Piton classification further refines
 * this by identifying the high degree of institutional decay and theatricality
 * that maintains the extractive equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_container_capacity_mismatch,
    'Is the infrastructure bottleneck a fundamental physical limit (Mountain) or an artificially maintained policy choice (Snare)?',
    'Tracking the delta between production growth and transit-infrastructure throughput over 20 years, correlated with capital investment in that infrastructure.',
    'If throughput plateaus despite investment: Mountain of Physical Geometry. If it scales only with investment: Snare of policy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(container_capacity_mismatch, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the constraint's degradation from a functional
% coordination mechanism into an extractive, theatrical one.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (metric_substitution drift):
narrative_ontology:measurement(ccm_tr_t0, container_capacity_mismatch, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ccm_tr_t5, container_capacity_mismatch, theater_ratio, 5, 0.45).
narrative_ontology:measurement(ccm_tr_t10, container_capacity_mismatch, theater_ratio, 10, 0.82).

% Extraction over time (extraction_accumulation drift):
narrative_ontology:measurement(ccm_ex_t0, container_capacity_mismatch, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(ccm_ex_t5, container_capacity_mismatch, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(ccm_ex_t10, container_capacity_mismatch, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint manages the flow of goods/data through a bottleneck.
narrative_ontology:coordination_type(container_capacity_mismatch, resource_allocation).

% Network relationships (structural influence edges)
% This bottleneck issue directly impacts the stability of supply chains.
narrative_ontology:affects_constraint(container_capacity_mismatch, global_supply_chain_fragility).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */