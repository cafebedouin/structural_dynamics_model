% ============================================================================
% CONSTRAINT STORY: latent_regulatory_bomb
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(latent_regulatory_bomb, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: latent_regulatory_bomb
 * human_readable: The Compliance Time-Trigger
 * domain: political/technological
 * * SUMMARY:
 * A scenario involving a "poison pill" regulation embedded in a legacy 
 * framework that remains dormant until a specific technological or 
 * economic threshold is crossed. Once triggered, it creates an immediate 
 * extraction event (Snare) by criminalizing existing common practices.
 * * KEY AGENTS:
 * - Independent Developer: Subject (Powerless)
 * - Regulatory Agency: Beneficiary (Institutional)
 * - Forensic Legal Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) because the "bomb" creates an immediate, massive 
% transfer of optionality/wealth to the enforcing entity upon activation.
domain_priors:base_extractiveness(latent_regulatory_bomb, 0.84). 
domain_priors:suppression_score(latent_regulatory_bomb, 0.72). % High suppression; alternatives are legally barred.
domain_priors:theater_ratio(latent_regulatory_bomb, 0.10).    % Low theater; the enforcement is direct and functional.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(latent_regulatory_bomb, extractiveness, 0.84).
narrative_ontology:constraint_metric(latent_regulatory_bomb, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(latent_regulatory_bomb, theater_ratio, 0.1).

% This is not a scaffold; it is a structural trap awaiting activation.
% narrative_ontology:has_sunset_clause(latent_regulatory_bomb). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the developer, the activated bomb is a pure extraction trap with no benefit.
constraint_indexing:constraint_classification(latent_regulatory_bomb, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The agency views the latent rule as a vital 'Rope' for maintaining 
% systemic order during rapid technological scaling.
constraint_indexing:constraint_classification(latent_regulatory_bomb, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects both the coordination intent (order) and the predatory extraction (trap).
constraint_indexing:constraint_classification(latent_regulatory_bomb, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(latent_regulatory_bomb, E), E >= 0.50,
    domain_priors:suppression_score(latent_regulatory_bomb, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latent_regulatory_bomb_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the subject but a Rope for the beneficiary.
    constraint_indexing:constraint_classification(latent_regulatory_bomb, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(latent_regulatory_bomb, rope, 
        context(agent_power(institutional), _, _, _)).

test(extraction_mandatrophy_trigger) :-
    % Ensures extraction is high enough to require mandatrophy resolution.
    domain_priors:base_extractiveness(latent_regulatory_bomb, E),

    E > 0.70.

:- end_tests(latent_regulatory_bomb_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) is extremely high because the "bomb" represents 
 * a retroactive capture of value that participants believed was safe.
 * * PERSPECTIVAL GAP:
 * The Individual feels a Snare because the rule was invisible/dormant until 
 * they were already committed. The Institution sees a Rope as it prevents 
 * "unregulated" growth from destabilizing their control.
 * * [RESOLVED MANDATROPHY]:
 * This is resolved by the Tangled Rope classification. While extraction is high, 
 * the system acknowledges it as a coordination failure—a "Rope" that was 
 * designed poorly for its time horizon, resulting in a predatory "Tangle."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_dormancy_intent,
    'Is the bomb a result of unintentional obsolescence (Mountain) or strategic dormancy (Snare)?',
    'Legislative intent audit of the original subcommittee minutes.',
    'If intentional: Snare. If accidental: Mountain of historical complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(latent_regulatory_bomb, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
