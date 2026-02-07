% ============================================================================
% CONSTRAINT STORY: constraint_interaction_explosion
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_interaction_explosion, []).

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
 * * constraint_id: constraint_interaction_explosion
 * human_readable: The Combinatorial Complexity Trap
 * domain: technological/social
 * * SUMMARY:
 * This constraint occurs when multiple simple coordination mechanisms (Ropes) 
 * interact in a non-linear fashion, creating a "feedback explosion" of 
 * emergent constraints. While each individual rule is logical, their 
 * intersection creates a Snare that extracts all operational optionality 
 * from the subjects trapped at the junction.
 * * KEY AGENTS:
 * - Systems Operator: Subject (Powerless)
 * - Regulatory Architect: Beneficiary (Institutional)
 * - Complexity Theorist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.81) because the "explosion" of rules siphons all 
% productivity into purely managing the intersections of the rules.
domain_priors:base_extractiveness(constraint_interaction_explosion, 0.81). 
domain_priors:suppression_score(constraint_interaction_explosion, 0.74). 
domain_priors:theater_ratio(constraint_interaction_explosion, 0.40).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(constraint_interaction_explosion, extractiveness, 0.81).
narrative_ontology:constraint_metric(constraint_interaction_explosion, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(constraint_interaction_explosion, theater_ratio, 0.4).

% This is an emergent property of complex systems, not a temporary scaffold.
% narrative_ontology:has_sunset_clause(constraint_interaction_explosion). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the operator, the interaction is a snare: they cannot move without 
% violating one of the intersecting, contradictory constraints.
constraint_indexing:constraint_classification(constraint_interaction_explosion, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views each individual constraint as a vital Rope for safety, 
% often ignoring the "explosion" caused by their sum.
constraint_indexing:constraint_classification(constraint_interaction_explosion, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature where coordination intent (Ropes) leads to 
% emergent predatory extraction (Snare).
constraint_indexing:constraint_classification(constraint_interaction_explosion, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(constraint_interaction_explosion, E), E >= 0.50,
    domain_priors:suppression_score(constraint_interaction_explosion, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constraint_interaction_explosion_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(constraint_interaction_explosion, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constraint_interaction_explosion, rope, 
        context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    % Ensure high extraction (0.81) triggers the correct logic gates.
    domain_priors:base_extractiveness(constraint_interaction_explosion, E),

    E >= 0.70.

:- end_tests(constraint_interaction_explosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.81) reflects the total paralysis of the subject 
 * due to combinatorial complexity. Each rule is a Rope, but their sum is a Snare.
 * * PERSPECTIVAL GAP:
 * 
 * The Operator experiences a Snare because they are the "ground" where 
 * these rules collide. The Architect sees a Rope because they only monitor 
 * the performance of individual rules in isolation.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. This recognizes that the 
 * "explosion" is an emergent coordination failure—it is a "Tangle" of 
 * Ropes that has become extractive through scale and complexity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_emergence_intent,
    'Is the interaction explosion an accidental system property (Mountain) or a designed friction (Snare)?',
    'Computational simulation of rule-intersections using automated "stress-test" agents.',
    'If accidental: Mountain of Complexity. If designed: Snare of Bureaucracy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing and structural linter.
narrative_ontology:interval(constraint_interaction_explosion, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
