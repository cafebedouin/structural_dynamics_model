% ============================================================================
% CONSTRAINT STORY: abstraction_boundary_overrun
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(abstraction_boundary_overrun, []).

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
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:measurement/5.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: abstraction_boundary_overrun
 * human_readable: The Leaky Black-Box Collapse
 * domain: technological/computational/cybernetic
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
 * - Systems Operator: Subject (Powerless)
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

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(abstraction_boundary_overrun, extractiveness, 0.81).
narrative_ontology:constraint_metric(abstraction_boundary_overrun, suppression_requirement, 0.69).
narrative_ontology:constraint_metric(abstraction_boundary_overrun, theater_ratio, 0.88).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
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

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.88) > 0.70 triggers Piton: the "User Manual" is an 
% inertial spike; it performatively charts a territory that no longer exists.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.81) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(abstraction_boundary_overrun, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(abstraction_boundary_overrun, E), E >= 0.50,
    domain_priors:suppression_score(abstraction_boundary_overrun, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abstraction_overrun_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless operator vs Rope for the institutional architect.
    constraint_indexing:constraint_classification(abstraction_boundary_overrun, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abstraction_boundary_overrun, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.88) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(abstraction_boundary_overrun, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(abstraction_boundary_overrun, E),

    E > 0.70.

:- end_tests(abstraction_overrun_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.81) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of a clean API is achieved by liquidating the 
 * operator's primary troubleshooting agency.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Systems Operator feels a Snare because they are fighting ghosts in 
 * the machine that they are officially told do not exist. The Architect 
 * sees a Rope because the abstraction allows thousands of users to 
 * coordinate without needing to understand the underlying silicon.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Abstraction" is no longer functional (Theater 0.88); 
 * it is an inert spike siphoning 0.81 of the operator's creative agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_abstraction_leakage_limit,
    'Can a perfect abstraction exist, or is leakage a physical law of computation (Snare vs Mountain)?',
    'Tracking the frequency of "low-level" errors in high-level managed environments over 15 years.',
    'If leakage plateaus: Snare of current design. If it rises with complexity: Mountain of Entropy.',
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
% Model how the constraint intensified or changed across the interval.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(abstraction_boundary_overrun_tr_t0, abstraction_boundary_overrun, theater_ratio, 0, 0.7).
narrative_ontology:measurement(abstraction_boundary_overrun_tr_t5, abstraction_boundary_overrun, theater_ratio, 5, 0.8).
narrative_ontology:measurement(abstraction_boundary_overrun_tr_t10, abstraction_boundary_overrun, theater_ratio, 10, 0.88).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(abstraction_boundary_overrun_ex_t0, abstraction_boundary_overrun, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(abstraction_boundary_overrun_ex_t5, abstraction_boundary_overrun, base_extractiveness, 5, 0.78).
narrative_ontology:measurement(abstraction_boundary_overrun_ex_t10, abstraction_boundary_overrun, base_extractiveness, 10, 0.81).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
