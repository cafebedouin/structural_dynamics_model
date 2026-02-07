% ============================================================================
% CONSTRAINT STORY: incentive_surface_warping
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(incentive_surface_warping, []).

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

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(incentive_surface_warping, extractiveness, 0.86).
narrative_ontology:constraint_metric(incentive_surface_warping, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(incentive_surface_warping, theater_ratio, 0.92).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
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

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.92) > 0.70 triggers Piton: the "Performance Dashboard" 
% is an inertial spike; it performatively signals "Green" status while 
% 0.86 extraction of intent occurs.
constraint_indexing:constraint_classification(incentive_surface_warping, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.86) masking as functional coordination (Rope).
constraint_indexing:constraint_classification(incentive_surface_warping, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(incentive_surface_warping, E), E >= 0.50,
    domain_priors:suppression_score(incentive_surface_warping, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incentive_warping_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless participant vs Rope for the institutional designer.
    constraint_indexing:constraint_classification(incentive_surface_warping, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incentive_surface_warping, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.92) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(incentive_surface_warping, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(incentive_surface_warping, E),

    E > 0.70.

:- end_tests(incentive_warping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of a universal incentive is achieved by liquidating the 
 * subject's primary capacity for mission-aligned agency.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Managed Participant feels a Snare because they are forced to "game" the 
 * system to remain employed, even as it destroys the work's quality. 
 * The Designer sees a Rope because the metric coordinates a perfectly legible, 
 * rankable, and manageable workforce.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Merit-Based Bonus" is no longer functional (Theater 0.92); 
 * it is an inert spike siphoning 0.86 of the system's actual value-agency. 
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
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
   END OF CONSTRAINT STORY
   ========================================================================== */
