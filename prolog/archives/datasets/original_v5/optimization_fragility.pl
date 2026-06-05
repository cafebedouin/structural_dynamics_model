% ============================================================================
% CONSTRAINT STORY: optimization_fragility
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(optimization_fragility, []).

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
 * * constraint_id: optimization_fragility
 * human_readable: The Efficiency-Resilience Tradeoff
 * domain: economic/technological/infrastructural
 * * SUMMARY:
 * A scenario where a system has been hyper-optimized for "Just-in-Time" efficiency, 
 * removing all "wasteful" buffers. While this functions as a Rope for profit 
 * coordination during stability, it creates a brittle Snare for subjects when 
 * minor shocks trigger total systemic collapse.
 * * KEY AGENTS:
 * - Local Retailer: Subject (Powerless)
 * - Logistics Hegemon: Beneficiary (Institutional)
 * - Complexity Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.82) because the removal of buffers siphons the system's 
% resilience into immediate shareholder profit, leaving subjects to pay for 
% the eventual failure.
domain_priors:base_extractiveness(optimization_fragility, 0.82). 
domain_priors:suppression_score(optimization_fragility, 0.75). % Alternatives are blocked by the cost-dominance of the optimized path.
domain_priors:theater_ratio(optimization_fragility, 0.40).    % Moderate theater; efficiency is marketed as "consumer benefit."

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(optimization_fragility, extractiveness, 0.82).
narrative_ontology:constraint_metric(optimization_fragility, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(optimization_fragility, theater_ratio, 0.4).

% This is an inherent property of lean systems, not a temporary scaffold.
% narrative_ontology:has_sunset_clause(optimization_fragility). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the powerless agent, the fragility is a snare: they enjoy low prices 
% until a supply chain shock leaves them with zero survival options.
constraint_indexing:constraint_classification(optimization_fragility, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views optimization as a vital Rope for coordinating 
% global resources with zero friction and maximum throughput.
constraint_indexing:constraint_classification(optimization_fragility, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of extreme coordination efficiency (Ropes) 
% entangled with massive, hidden systemic risk (Snare).
constraint_indexing:constraint_classification(optimization_fragility, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(optimization_fragility, E), E >= 0.50,
    domain_priors:suppression_score(optimization_fragility, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(optimization_fragility_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(optimization_fragility, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(optimization_fragility, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    % Ensure high extraction (0.82) correctly triggers mandatory resolution logic.
    domain_priors:base_extractiveness(optimization_fragility, E),

    E >= 0.70.

:- end_tests(optimization_fragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.82) reflects a "Mandatrophy" state where the 
 * "coordination" is actually a parasitic liquidation of systemic safety buffers.
 *  * * PERSPECTIVAL GAP:
 * The Individual feels a Snare because they are the terminal point of 
 * failure when the "lean" system breaks. The Institution sees a Rope 
 * because the lack of buffers allows for unprecedented coordination 
 * of global capital and goods.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. This recognizes that 
 * hyper-efficiency provides genuine coordination (Rope), but the 0.82 
 * extraction identifies that the cost of this coordination is the 
 * permanent removal of the subject's agency during a crisis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_resilience_delta,
    'Is the lack of buffers a physical limit of the market (Mountain) or a policy of cost-externalization (Snare)?',
    'Stress-test modeling of system throughput under 5%, 10%, and 20% random node failures.',
    'If throughput remains high: Rope of Innovation. If throughput crashes: Snare of Fragility.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing and structural linter.
narrative_ontology:interval(optimization_fragility, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
