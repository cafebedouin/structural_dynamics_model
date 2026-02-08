% ============================================================================
% CONSTRAINT STORY: incentive_surface_warping
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_incentive_surface_warping, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: incentive_surface_warping
 * human_readable: The Perverse Optimization Trap
 * domain: economic/organizational/technological
 * * SUMMARY:
 * A scenario where a "Rope" designed to align individual behavior with
 * collective goals (e.g., performance bonuses, metrics-based funding,
 * or algorithmic rewards) inadvertently "warps" the behavioral landscape.
 * This coordination substrate becomes a "Snare" for the subject, as the
 * system liquidates their agency to pursue the actual intended outcome,
 * trapping them in a territory where they must optimize for the "proxy
 * metric" rather than the "real value" to remain viable.
 * * KEY AGENTS:
 * - Managed Participant: Subject (Powerless)
 * - Metric Designer: Beneficiary (Institutional)
 * - Behavioral Alignment Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) reflects the siphoning of the subject's
% primary intent-agency into the maintenance of the warped proxy-metric.
domain_priors:base_extractiveness(incentive_surface_warping, 0.86).
domain_priors:suppression_score(incentive_surface_warping, 0.75). % Non-metric-aligned behavior is suppressed by lack of reward or funding.
domain_priors:theater_ratio(incentive_surface_warping, 0.92).    % Extreme theater: "Alignment Reports" showing 100% metric success while the actual goal fails.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(incentive_surface_warping, extractiveness, 0.86).
narrative_ontology:constraint_metric(incentive_surface_warping, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(incentive_surface_warping, theater_ratio, 0.92).

% The system claims to be a coordination mechanism for performance.
narrative_ontology:constraint_claim(incentive_surface_warping, coordination).

% Binary flags and structural properties for Tangled Rope classification.
domain_priors:requires_active_enforcement(incentive_surface_warping).
narrative_ontology:constraint_beneficiary(incentive_surface_warping, metric_designers).
narrative_ontology:constraint_victim(incentive_surface_warping, managed_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The participant is trapped: they must follow the warped incentive to survive,
% but doing so liquidates their ability to achieve the original goal.
constraint_indexing:constraint_classification(incentive_surface_warping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The designer views the incentive as a Rope—the essential coordination
% substrate for managing large-scale behavior and ensuring legible performance.
constraint_indexing:constraint_classification(incentive_surface_warping, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The default analytical view detects the hybrid nature: a coordination
% function for beneficiaries that relies on asymmetric extraction from victims.
constraint_indexing:constraint_classification(incentive_surface_warping, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% An alternative analytical view focusing on functional decay. The extreme
% theater ratio (0.92) triggers a Piton classification, seeing the incentive
% system as a non-functional, performative relic.
constraint_indexing:constraint_classification(incentive_surface_warping, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incentive_warping_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless participant vs Rope for the institutional designer.
    constraint_indexing:constraint_classification(incentive_surface_warping, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incentive_surface_warping, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(incentive_surface_warping, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.92) correctly triggers the Piton classification.
    domain_priors:theater_ratio(incentive_surface_warping, TR),
    TR > 0.70,
    constraint_indexing:constraint_classification(incentive_surface_warping, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements) :-
    % Verify that the structural requirements for Tangled Rope are met.
    domain_priors:requires_active_enforcement(incentive_surface_warping),
    narrative_ontology:constraint_beneficiary(incentive_surface_warping, _),
    narrative_ontology:constraint_victim(incentive_surface_warping, _).

:- end_tests(incentive_warping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The high extraction score (0.86) represents a "Mandatrophy" state where the
 * coordination benefit of a universal incentive is achieved by liquidating the
 * subject's primary capacity for mission-aligned agency. The suppression score
 * (0.75) reflects how non-compliance (i.e., pursuing the actual goal instead
 * of the proxy metric) is punished via lack of rewards or funding. The extreme
 * theater ratio (0.92) captures the state where performance dashboards are
 * "all green" while the underlying mission is failing, a classic sign of
 * Goodhart's Law drift.
 *
 * PERSPECTIVAL GAP:
 * The Managed Participant feels a Snare because they are forced to "game" the
 * system to remain employed, even as it destroys the work's quality.
 * The Designer sees a Rope because the metric coordinates a perfectly legible,
 * rankable, and manageable workforce. The Analytical Observer sees a Tangled
 * Rope, recognizing both the coordination function and the extractive harm.
 *
 * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical for resolving the Mandatrophy
 * inherent in this constraint. A simpler model might classify this purely as a
 * Snare (from the victim's view) or a Piton (due to high theater), missing the
 * crucial fact that the system *still performs a coordination function* for the
 * metric designers. Tangled Rope correctly identifies the hybrid nature: it
 * coordinates behavior for one group (the beneficiaries) by asymmetrically
 * extracting agency and value from another (the victims). This prevents the
 * system from collapsing the analysis into a simple "good vs. bad" binary and
 * instead highlights the structural conflict at its core.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_proxy_alignment_limit,
    'Can "Holistic Review" restore the Rope, or is warping a physical law of optimization (Snare vs Mountain)?',
    'Tracking the delta between "Metric Performance" and "Client Satisfaction" in 2026-style healthcare systems.',
    'If satisfaction holds: Snare of current technique. If it diverges: Mountain of Organizational Physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(incentive_surface_warping, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint models a system that degrades over time, starting as a
% well-intentioned coordination mechanism and warping into an extractive trap.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(isw_tr_t0, incentive_surface_warping, theater_ratio, 0, 0.10).
narrative_ontology:measurement(isw_tr_t5, incentive_surface_warping, theater_ratio, 5, 0.55).
narrative_ontology:measurement(isw_tr_t10, incentive_surface_warping, theater_ratio, 10, 0.92).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(isw_ex_t0, incentive_surface_warping, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(isw_ex_t5, incentive_surface_warping, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(isw_ex_t10, incentive_surface_warping, base_extractiveness, 10, 0.86).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The incentive system is a form of resource allocation (bonuses, funding).
narrative_ontology:coordination_type(incentive_surface_warping, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */