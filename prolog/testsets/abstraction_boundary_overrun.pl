% ============================================================================
% CONSTRAINT STORY: abstraction_boundary_overrun
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_abstraction_boundary_overrun, []).

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
 * * constraint_id: abstraction_boundary_overrun
 * human_readable: The Leaky Black-Box Collapse
 * domain: technological/computational
 * * SUMMARY:
 * A scenario where a high-level system (the abstraction) fails to hide the
 * messy, low-level complexity it was designed to simplify. The
 * "leaky abstraction" causes implementation details to "overrun" the
 * boundary, forcing the user to handle variables they lack the agency
 * to control. This "Rope" for computational efficiency becomes a
 * "Snare" for the operator, as their agency is liquidated by
 * high-frequency errors emerging from a substrate they cannot see or repair.
 *
 * * KEY AGENTS:
 * - System Operator: Subject (Powerless)
 * - Abstraction Architect: Beneficiary (Institutional)
 * - Cybernetic Forensic Analyst: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.81) reflects the siphoning of the subject's
% cognitive labor into managing "unhandled" low-level side effects.
domain_priors:base_extractiveness(abstraction_boundary_overrun, 0.81).
domain_priors:suppression_score(abstraction_boundary_overrun, 0.69). % Access to the underlying substrate is suppressed by "security" protocols.
domain_priors:theater_ratio(abstraction_boundary_overrun, 0.88).    % High theater: "Clean Interfaces" that performatively hide systemic instability.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(abstraction_boundary_overrun, extractiveness, 0.81).
narrative_ontology:constraint_metric(abstraction_boundary_overrun, suppression_requirement, 0.69).
narrative_ontology:constraint_metric(abstraction_boundary_overrun, theater_ratio, 0.88).

% Constraint self-claim (what does the constraint claim to be?)
% The abstraction claims to be a pure coordination mechanism (a clean API).
narrative_ontology:constraint_claim(abstraction_boundary_overrun, tangled_rope).

% Binary flags and structural properties for Tangled Rope classification.
domain_priors:requires_active_enforcement(abstraction_boundary_overrun). % The system architecture actively prevents access to the substrate.
narrative_ontology:constraint_beneficiary(abstraction_boundary_overrun, abstraction_architects). % Derives has_coordination_function/1.
narrative_ontology:constraint_victim(abstraction_boundary_overrun, system_operators). % Derives has_asymmetric_extraction/1.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The operator is trapped: the abstraction is broken, but they are forbidden
% or unable to bypass it, liquidating their ability to fix the root cause.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the overrun as a Rope—the "cost of doing business"
% while coordinating massive system complexity via a simplified front-end.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.81) and suppression (0.69) masking as essential
% coordination. The system has a genuine coordination function but also
% imposes severe, asymmetric costs, requiring active enforcement to maintain.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abstraction_boundary_overrun_tests).

test(perspectival_gap_snare_vs_rope) :-
    % Verify the core perspectival gap between the powerless operator and the institutional architect.
    constraint_indexing:constraint_classification(abstraction_boundary_overrun, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abstraction_boundary_overrun, rope, context(agent_power(institutional), _, _, _)).

test(analytical_observer_detects_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature of the constraint.
    constraint_indexing:constraint_classification(abstraction_boundary_overrun, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(abstraction_boundary_overrun_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.81) reflects a state where the "coordination"
 * benefit of a clean API is achieved by liquidating the operator's primary
 * troubleshooting agency and cognitive bandwidth. The suppression score (0.69)
 * represents the architectural and security policies that prevent operators
 * from accessing the underlying substrate to fix problems, thus enforcing the
 * leaky abstraction. The high theater ratio (0.88) indicates that the API's
 * claim of simplicity is largely performative, hiding deep instability.
 *
 * * PERSPECTIVAL GAP:
 * The System Operator feels a Snare because they are fighting ghosts in
 * the machine that they are officially told do not exist. The Architect
 * sees a Rope because the abstraction allows thousands of users to
 * coordinate without needing to understand the underlying silicon.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction (0.81) indicates a potential Mandatrophy state, where a
 * system's claimed purpose (coordination) is subverted by its actual function
 * (extraction). This is resolved by the Tangled Rope classification. This
 * classification correctly identifies that the system possesses BOTH a genuine
 * coordination function (benefiting architects) AND a severe, asymmetric
 * extractive component (victimizing operators). It prevents misclassification
 * as a pure Snare (which would ignore the coordination value) or a pure Rope
 * (which would ignore the crippling extraction), thus providing a precise
 * diagnosis of the system's hybrid, pathological state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_abstraction_leakage_limit,
    'Can a perfect abstraction exist, or is leakage a physical law of computation (making this a Mountain, not a constructed Snare)?',
    'Tracking the frequency of "low-level" errors in high-level managed environments over 15 years, correlated with system complexity.',
    'If leakage plateaus: Snare of current design. If it rises predictably with complexity: Mountain of Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(abstraction_boundary_overrun, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(abstraction_boundary_overrun_tr_t0, abstraction_boundary_overrun, theater_ratio, 0, 0.70).
narrative_ontology:measurement(abstraction_boundary_overrun_tr_t5, abstraction_boundary_overrun, theater_ratio, 5, 0.80).
narrative_ontology:measurement(abstraction_boundary_overrun_tr_t10, abstraction_boundary_overrun, theater_ratio, 10, 0.88).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(abstraction_boundary_overrun_ex_t0, abstraction_boundary_overrun, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(abstraction_boundary_overrun_ex_t5, abstraction_boundary_overrun, base_extractiveness, 5, 0.78).
narrative_ontology:measurement(abstraction_boundary_overrun_ex_t10, abstraction_boundary_overrun, base_extractiveness, 10, 0.81).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% An API or abstraction layer is a form of information standard.
narrative_ontology:coordination_type(abstraction_boundary_overrun, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */