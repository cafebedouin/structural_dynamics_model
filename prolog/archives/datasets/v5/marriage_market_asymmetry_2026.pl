% ============================================================================
% CONSTRAINT STORY: marriage_market_asymmetry_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_marriage_market, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: marriage_market_asymmetry_2026
 * human_readable: The Asymmetric Information Snare (Women Asking Out)
 * domain: social/psychological/economic
 * * SUMMARY:
 * This constraint analyzes the "Gale-Shapley" paradox in real-world dating. 
 * While the algorithm suggests "asker-optimality," biological and social 
 * asymmetries (time-cost of rounds, information scarcity upon acceptance) 
 * transform "asking" into a high-extraction Snare for women.
 * * KEY AGENTS:
 * - [Women]: Subject (Powerless) - Face higher opportunity costs per dating 
 * round and gain low information from male acceptance.
 * - [Men]: Beneficiary (Institutional) - Benefit from lower rejection 
 * thresholds and extended "biochemical clocks" for matching.
 * - [Game Theorists]: Auditor (Analytical) - Model the marriage market via 
 * stable matching algorithms like Gale-Shapley.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.65) because "uncertain rounds" extract significant 
% reproductive/opportunity time from women more than men.
domain_priors:base_extractiveness(marriage_market_asymmetry_2026, 0.65). 

% Suppression (0.75) of information; male acceptance of a female-initiated 
% ask provides "very little additional information" on preference.
domain_priors:suppression_score(marriage_market_asymmetry_2026, 0.75).   

% Moderate theater (0.50); "dating" often functions as a theatrical 
% placeholder for a decision that could take years to resolve.
domain_priors:theater_ratio(marriage_market_asymmetry_2026, 0.50).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(marriage_market_asymmetry_2026, extractiveness, 0.65).
narrative_ontology:constraint_metric(marriage_market_asymmetry_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_market_asymmetry_2026, theater_ratio, 0.5).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For women, initiating is often a snare: the "information measure of 
% surprise" is low, trapping them in ambiguous, non-committal dates.
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For men, the "asker-optimal" model is a rope: it coordinates sexual/romantic 
% access with lower personal rejection risk if women lead the ask.
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects a hybrid: Gale-Shapley provides a coordination framework (Rope) 
% that ignores the extraction of "time-value" (Snare).
constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(marriage_market_asymmetry_2026, E), E >= 0.50,
    domain_priors:suppression_score(marriage_market_asymmetry_2026, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_asymmetry_tests).

test(perspectival_gap) :-
    % Women see a Snare (Ambiguity/Cost), Men see a Rope (Coordination).
    constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(marriage_market_asymmetry_2026, E),

    E > 0.46. % Correctly identifies the "time-value" extraction of dating rounds.

:- end_tests(marriage_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.65) reflects the "opportunity cost" of uncertain 
 * dating rounds, which is higher for women due to compressed biological 
 * timelines. 
 * The Perspectival Gap exists because Gale-Shapley ignores the asymmetry 
 * of information; a man's "Yes" to a female ask contains less signal than 
 * a woman's "Yes" to a male ask.
 *
 * MANDATROPHY ANALYSIS:
 * Tangled Rope is used because the algorithm successfully models stable 
 * matching (coordination) but fails as a life-strategy for the Subject 
 * due to unmodeled asymmetries in "round costs".
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_chastity_stabilization,
    'Does a religious social group (SCAFFOLD) nullify the information asymmetry?',
    'Comparative study of female-initiation success in chaste vs. secular cohorts.',
    'If chastity works, the Snare is a policy/norm; if not, it is a Mountain of biology.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================= */

narrative_ontology:interval(marriage_market_asymmetry_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Model the drift from "Youth/Time Surplus" (T=0) to "Biological Ceiling" (T=10).

% Theater ratio: Increases as dating rounds lengthen without a "marriage" 
% decision, turning coordination into performative "milk for free".
narrative_ontology:measurement(mm_tr_t0, marriage_market_asymmetry_2026, theater_ratio, 0, 0.20).
narrative_ontology:measurement(mm_tr_t5, marriage_market_asymmetry_2026, theater_ratio, 5, 0.35).
narrative_ontology:measurement(mm_tr_t10, marriage_market_asymmetry_2026, theater_ratio, 10, 0.50).

% Extraction: The cost of a "round" increases exponentially for women as 
% the time-horizon approaches biological limits.
narrative_ontology:measurement(mm_ex_t0, marriage_market_asymmetry_2026, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(mm_ex_t5, marriage_market_asymmetry_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mm_ex_t10, marriage_market_asymmetry_2026, base_extractiveness, 10, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
