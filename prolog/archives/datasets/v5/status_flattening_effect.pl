% ============================================================================
% CONSTRAINT STORY: status_flattening_effect
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(status_flattening_effect, []).

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
 * * constraint_id: status_flattening_effect
 * human_readable: The Algorithmic Leveler
 * domain: social/technological/economic
 * * SUMMARY:
 * This constraint represents the phenomenon where digital platforms or 
 * standardized bureaucratic systems collapse complex, multi-dimensional 
 * social hierarchies into a single, legible metric (e.g., follower count, 
 * credit score, or engagement rate). This "Rope" for mass-scale legibility 
 * becomes a "Snare" for individuals whose unique value or niche expertise 
 * cannot be captured by the flattened metric, liquidating their social capital.
 * * KEY AGENTS:
 * - Niche Domain Expert: Subject (Powerless)
 * - Platform Aggregator: Beneficiary (Institutional)
 * - Social Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.82) because the system siphons the nuanced social value 
% of the individual into a standardized "score" that the platform controls 
% and monetizes.
domain_priors:base_extractiveness(status_flattening_effect, 0.82). 
domain_priors:suppression_score(status_flattening_effect, 0.75). % High suppression: Alternative signals are ignored by the market.
domain_priors:theater_ratio(status_flattening_effect, 0.68).    % Near-Piton; the metric is performatively equated with "worth."

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(status_flattening_effect, extractiveness, 0.82).
narrative_ontology:constraint_metric(status_flattening_effect, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(status_flattening_effect, theater_ratio, 0.68).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: their actual expertise is invisible to the 
% system unless it can be translated into the flattened metric.
constraint_indexing:constraint_classification(status_flattening_effect, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the flattened metric as a Rope—the only way to 
% coordinate the attention of millions of users simultaneously.
constraint_indexing:constraint_classification(status_flattening_effect, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.82) and high suppression (0.75) trigger the hybrid 
% Tangled Rope signature at the historical scale.
constraint_indexing:constraint_classification(status_flattening_effect, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(status_flattening_effect, E), E >= 0.50,
    domain_priors:suppression_score(status_flattening_effect, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(status_flattening_effect_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(status_flattening_effect, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(status_flattening_effect, rope, 
        context(agent_power(institutional), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.82) triggers mandatory resolution logic.
    domain_priors:base_extractiveness(status_flattening_effect, E),

    E > 0.70.

:- end_tests(status_flattening_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.82) reflects a "Mandatrophy" state where the 
 * "coordination" of social attention is achieved by liquidating the 
 * subject's multi-dimensional identity.
 * 
 * * PERSPECTIVAL GAP:
 * The Niche Expert feels a Snare because their 20 years of study are 
 * rendered equal to a viral post by a non-expert. The Platform 
 * sees a Rope because the single metric is the only thing that allows 
 * their algorithm to sort the vast influx of data.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. This recognizes the 
 * coordination necessity of the platform while identifying the 0.82 
 * extraction of the subject's agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_metric_pluralism,
    'Can a system support multi-dimensional legibility without losing coordination (Snare vs Mountain)?',
    'Tracking the efficiency of platforms using multi-variate reputation vs single-metric scoring.',
    'If efficient: Snare of current design. If inefficient: Mountain of Cognitive Limits.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(status_flattening_effect, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
