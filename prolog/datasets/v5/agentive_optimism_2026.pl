% ============================================================================
% CONSTRAINT STORY: agentive_optimism_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_agentive_optimism_2026, []).

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
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: agentive_optimism_2026
 * human_readable: The Agentive Optimism Gap
 * domain: political/social
 * * SUMMARY:
 * A fundamental divide has emerged between the "weird" policy-making class, 
 * defined by a rare sense of personal agency and optimism, and a segment 
 * of the public defined by "overpowering pessimism". 
 * This pessimism, rooted in the belief that the economy is stuck and resources 
 * are zero-sum, creates a structural barrier to democratic representation 
 *.
 * * KEY AGENTS:
 * - The Pessimistic Public (Protestors): Subject (Powerless)
 * - The Policy-Making Class: Beneficiary (Institutional)
 * - Contrarion Analysts (Dan Davies): Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.70). Systemic pessimism extracts the baseline 
% sense of hope necessary for intergenerational investment.
domain_priors:base_extractiveness(agent_opt_2026, 0.70). 

% Suppression is moderate-high (0.65). The requirement for optimism to enter 
% the policy world effectively suppresses pessimistic worldviews from 
% official representation.
domain_priors:suppression_score(agent_opt_2026, 0.65).   

% Theater ratio is high (0.74). Reactionary politics is identified as 
% a performative domain for grifters and cynics.
domain_priors:theater_ratio(agent_opt_2026, 0.74).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(agent_opt_2026, extractiveness, 0.7).
narrative_ontology:constraint_metric(agent_opt_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(agent_opt_2026, theater_ratio, 0.74).

% Primary keys for the classification engine
% High-extraction stakeholders
narrative_ontology:constraint_beneficiary(agent_opt_2026, policy_elites).
narrative_ontology:constraint_victim(agent_opt_2026, social_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PESSIMIST (SNARE)
% For the protestor, the economy is a Snare: an inescapable zero-sum trap 
% where providing help to others is seen as impossible.
constraint_indexing:constraint_classification(agent_opt_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE POLICY MAKER (ROPE)
% Policy elites view "agentive optimism" as a Rope: the essential 
% coordination tool required to tackle solvable problems.
constraint_indexing:constraint_classification(agent_opt_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Analysts view the lack of representation for the pessimistic view as a 
% Piton: an inertial institutional blind spot.
constraint_indexing:constraint_classification(agent_opt_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agent_opt_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agent_opt_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agent_opt_2026, rope, context(agent_power(institutional), _, _, _)).

test(theater_check) :-
    domain_priors:theater_ratio(agent_opt_2026, TR),
    TR >= 0.70.

:- end_tests(agent_opt_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.70) reflects how pessimism erodes the "baseline 
 * sense of agency" vital for a functioning society. 
 * The high Theater Ratio (0.74) reflects that reactionary politics 
 * often defaults to "grifters, cynics, and wreckers" because true 
 * belief in improvement is absent.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system identifies the Piton by the structural exclusion of the 
 * pessimistic worldview from policy circles, as optimism is a prerequisite 
 * for the "agent" role.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_democratic_representation,
    'Can a worldview of "overpowering pessimism" ever be represented in policy?',
    'Analysis of Reform Party internal rationalization structures.',
    'Success implies a new Rope; failure leads to a permanent social Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agent_opt_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Modeling the drift from "Baseline Optimism" (T=0) to "Overpowering Pessimism" (T=10).
% Theater ratio rises as grifters fill the representational void of the pessimistic.
narrative_ontology:measurement(ao_tr_t0, agent_opt_2026, theater_ratio, 0, 0.20).
narrative_ontology:measurement(ao_tr_t5, agent_opt_2026, theater_ratio, 5, 0.50).
narrative_ontology:measurement(ao_tr_t10, agent_opt_2026, theater_ratio, 10, 0.74).

% Extraction rises as the sense of soluble problems declines among the public.
narrative_ontology:measurement(ao_ex_t0, agent_opt_2026, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ao_ex_t5, agent_opt_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ao_ex_t10, agent_opt_2026, base_extractiveness, 10, 0.70).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
