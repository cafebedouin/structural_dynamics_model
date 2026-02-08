% ============================================================================
% CONSTRAINT STORY: shadow_pricing_failure
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(shadow_pricing_failure, []).

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
 * * constraint_id: shadow_pricing_failure
 * human_readable: The Invisible Market Collapse
 * domain: economic/technological
 * * SUMMARY:
 * A scenario where the "shadow price" (the internal, non-market value) of a 
 * resource like data, carbon, or attention is incorrectly modeled or 
 * suppressed by institutional frameworks. This creates a Rope for 
 * administrative efficiency but functions as a Snare for the ecosystem, 
 * as the true cost of depletion is hidden until a threshold failure occurs.
 * * KEY AGENTS:
 * - Data Contributor: Subject (Powerless)
 * - Platform Architect: Beneficiary (Institutional)
 * - Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.82) because the failure to price the resource correctly 
% siphons the subject's long-term value into the beneficiary's short-term 
% operational margins.
domain_priors:base_extractiveness(shadow_pricing_failure, 0.82). 
domain_priors:suppression_score(shadow_pricing_failure, 0.75). % High suppression: Alternative pricing models are ignored.
domain_priors:theater_ratio(shadow_pricing_failure, 0.72).    % Piton threshold (> 0.70) triggered by performative auditing.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(shadow_pricing_failure, extractiveness, 0.82).
narrative_ontology:constraint_metric(shadow_pricing_failure, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(shadow_pricing_failure, theater_ratio, 0.72).

% This is a structural information failure, not a temporary scaffold.
% narrative_ontology:has_sunset_clause(shadow_pricing_failure). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped in an exchange where their contribution is 
% systematically undervalued by the hidden pricing mechanism.
constraint_indexing:constraint_classification(shadow_pricing_failure, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the simplified (failed) shadow price as a Rope, 
% enabling mass coordination without the friction of complex valuation.
constraint_indexing:constraint_classification(shadow_pricing_failure, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.72) > 0.70 triggers Piton: the internal pricing is 
% an inertial spike that no longer reflects reality.
constraint_indexing:constraint_classification(shadow_pricing_failure, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(shadow_pricing_failure, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.82) and high suppression (0.75) as a Tangled Rope.
constraint_indexing:constraint_classification(shadow_pricing_failure, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(shadow_pricing_failure, E), E >= 0.50,
    domain_priors:suppression_score(shadow_pricing_failure, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shadow_pricing_failure_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless but Rope for the institution.
    constraint_indexing:constraint_classification(shadow_pricing_failure, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shadow_pricing_failure, rope, context(agent_power(institutional), _, _, _)).

test(piton_audit_logic) :-
    % Ensure high theater results in Piton detection.
    constraint_indexing:constraint_classification(shadow_pricing_failure, piton, context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % Ensure high extraction (0.82) triggers mandatory resolution.
    domain_priors:base_extractiveness(shadow_pricing_failure, E),

    E >= 0.70.

:- end_tests(shadow_pricing_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.82) reflects a "Mandatrophy" threshold where the 
 * administrative convenience of a simple price is paid for by the systemic 
 * depletion of the subject's resource.
 * * 
 * * * PERSPECTIVAL GAP:
 * The Data Contributor feels a Snare because they cannot negotiate a 
 * fair price in an opaque system. The Architect sees a Rope because 
 * the uniform price allows for the coordination of millions of users.
 * * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. This recognizes the 
 * coordination function while identifying the high extraction (0.82) 
 * inherent in ignoring the resource's true value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_pricing_fidelity,
    'Can the true shadow price be calculated, or is it an irreducible mystery (Snare vs Mountain)?',
    'Auditing the delta between internal valuation models and secondary market outcomes.',
    'If calculable: Snare of policy. If incalculable: Mountain of Information Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing and structural linter.
narrative_ontology:interval(shadow_pricing_failure, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
