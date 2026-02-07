% ============================================================================
% CONSTRAINT STORY: epistemic_overload_collapse
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(epistemic_overload_collapse, []).

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
 * * constraint_id: epistemic_overload_collapse
 * human_readable: The Signal-Drowning Vortex
 * domain: cognitive/informational/technological
 * * SUMMARY:
 * A scenario where the volume, velocity, and contradictory nature of 
 * available information (Rope) exceed the biological and cognitive limits 
 * of the human subject to process it. This coordination tool for "total 
 * information awareness" becomes a "Snare" as the subject's primary 
 * truth-seeking agency is liquidated, trapping them in a terminal state 
 * of apathy, paralysis, or reliance on low-fidelity heuristics, effectively 
 * collapsing their ability to participate in meaningful collective action. 
 *
 * * KEY AGENTS:
 * - Information Citizen: Subject (Powerless)
 * - Attention/Data Aggregator: Beneficiary (Institutional)
 * - Cognitive Integrity Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.89) reflects the liquidation of the subject's 
% primary cognitive autonomy to feed the institutional need for constant data consumption.
domain_priors:base_extractiveness(epistemic_overload_collapse, 0.89). 
domain_priors:suppression_score(epistemic_overload_collapse, 0.81). % Silence or "low-data" environments are suppressed by social and economic mandates for connectivity.
domain_priors:theater_ratio(epistemic_overload_collapse, 0.92).    % Extreme theater: "Fact-Checkers" and "Context Notes" that add more data-load while signaling truth.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(epistemic_overload_collapse, extractiveness, 0.89).
narrative_ontology:constraint_metric(epistemic_overload_collapse, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(epistemic_overload_collapse, theater_ratio, 0.92).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The citizen is trapped: they must use the information flood to remain 
% socially and economically legible, but doing so liquidates their capacity for discernment.
constraint_indexing:constraint_classification(epistemic_overload_collapse, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The aggregator views the flood as a Rope—the essential coordination 
% substrate for achieving a "Fully Transparent" global society.
constraint_indexing:constraint_classification(epistemic_overload_collapse, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.92) > 0.70 triggers Piton: the "Information Literacy" 
% seminar is an inertial spike; it performatively addresses the overload while 0.89 extraction continues.
constraint_indexing:constraint_classification(epistemic_overload_collapse, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.89) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(epistemic_overload_collapse, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(epistemic_overload_collapse, E), E >= 0.50,
    domain_priors:suppression_score(epistemic_overload_collapse, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_overload_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless citizen vs Rope for the institutional aggregator.
    constraint_indexing:constraint_classification(epistemic_overload_collapse, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_overload_collapse, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.92) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(epistemic_overload_collapse, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(epistemic_overload_collapse, E),

    E > 0.70.

:- end_tests(epistemic_overload_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.89) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of total information access is achieved by 
 * liquidating the subject's primary capacity for epistemic agency.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Information Citizen feels a Snare because their cognitive environment is 
 * fundamentally hostile to truth-seeking. The Aggregator sees a Rope because 
 * the total capture of data coordinates a perfectly legible and monetizeable 
 * global attention market.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Critical Thinking" initiative is no longer functional (Theater 0.92); 
 * it is an inert spike siphoning 0.89 of the species' collective surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_cognitive_saturation,
    'Can AI "Information Filters" restore the Rope, or is overload an entropic "Mountain" (Snare vs Mountain)?',
    'Tracking the belief-delta between subjects with human-curated vs AI-curated news feeds over 24 months.',
    'If belief stabilizes: Snare of current technique. If it diverges: Mountain of Cognitive Limits.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(epistemic_overload_collapse, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
