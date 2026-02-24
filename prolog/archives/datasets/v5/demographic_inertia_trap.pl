% ============================================================================
% CONSTRAINT STORY: demographic_inertia_trap
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(demographic_inertia_trap, []).

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
 * * constraint_id: demographic_inertia_trap
 * human_readable: The Generational Wealth Siphon
 * domain: social/economic
 * * SUMMARY:
 * A scenario where a massive, aging demographic majority maintains political 
 * control to enforce economic transfers (pensions, healthcare, zoning) from 
 * a shrinking youth minority. Because the demographic shift is a "slow-motion 
 * collision," it appears as an immutable Mountain to the young, while serving 
 * as a critical coordination Rope for the stability of the elderly.
 * * KEY AGENTS:
 * - Young Professional: Subject (Powerless)
 * - Pensioner Bloc: Beneficiary (Institutional)
 * - Actuarial Historian: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.78) as it liquidates the lifetime optionality of the 
% youth to service the maintenance costs of the legacy demographic.
domain_priors:base_extractiveness(demographic_inertia_trap, 0.78). 
domain_priors:suppression_score(demographic_inertia_trap, 0.65).
domain_priors:theater_ratio(demographic_inertia_trap, 0.40).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(demographic_inertia_trap, extractiveness, 0.78).
narrative_ontology:constraint_metric(demographic_inertia_trap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(demographic_inertia_trap, theater_ratio, 0.4).

% This is an inherent property of biological decay and birth rates, not a scaffold.
% narrative_ontology:has_sunset_clause(demographic_inertia_trap). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% To a young individual, the "demographic pyramid" is an unchangeable law of nature.
constraint_indexing:constraint_classification(demographic_inertia_trap, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The elderly bloc views the trap as a Rope—it is the only way to coordinate 
% societal resources to prevent a collapse of the elder-care social contract.
constraint_indexing:constraint_classification(demographic_inertia_trap, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the present coordination (Rope) inextricably tied to predatory extraction (Snare).
constraint_indexing:constraint_classification(demographic_inertia_trap, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(demographic_inertia_trap, E), E >= 0.50,
    domain_priors:suppression_score(demographic_inertia_trap, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(demographic_inertia_trap_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Mountain for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(demographic_inertia_trap, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(demographic_inertia_trap, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(demographic_inertia_trap, E),

    E >= 0.46. % Ensures high-extraction triggers Omega/Mandatrophy rules.

:- end_tests(demographic_inertia_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.78) reflects the 'Mandatrophy' threshold where 
 * the social contract has shifted from mutual support to generational extraction.
 * * PERSPECTIVAL GAP:
 * 
 * The Young Professional feels a Mountain because they cannot vote away 
 * the existence of 100 million retirees. The Pensioner Bloc sees a Rope 
 * because their life depends on the institutionalized coordination of wealth 
 * transfers.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via Tangled Rope classification. This recognizes the coordination 
 * intent (preventing elderly poverty) while explicitly flagging the 0.78 
 * extraction of youth optionality as a systemic failure point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_productivity_scaling,
    'Can AI-driven productivity outpace the dependency ratio (Mountain or Snare)?',
    'Tracking the delta between GDP per worker and the elder-support tax rate.',
    'If growth > tax: Rope. If tax > growth: Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(demographic_inertia_trap, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
