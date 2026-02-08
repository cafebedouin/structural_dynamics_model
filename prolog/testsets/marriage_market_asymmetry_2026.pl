% ============================================================================
% CONSTRAINT STORY: marriage_market_asymmetry_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_marriage_market_asymmetry_2026, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 * - Women in dating market: Subject (Powerless) - Face higher opportunity costs per dating
 * round and gain low information from male acceptance.
 * - Men in dating market: Beneficiary (Institutional) - Benefit from lower rejection
 * thresholds and extended "biochemical clocks" for matching.
 * - Game Theorists: Auditor (Analytical) - Model the marriage market via
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

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(marriage_market_asymmetry_2026, extractiveness, 0.65).
narrative_ontology:constraint_metric(marriage_market_asymmetry_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_market_asymmetry_2026, theater_ratio, 0.50).

% Constraint self-claim: The norms are presented as social constructs, not natural law.
narrative_ontology:constraint_claim(marriage_market_asymmetry_2026, tangled_rope).

% Binary flags & Structural properties for Tangled Rope
domain_priors:requires_active_enforcement(marriage_market_asymmetry_2026).
narrative_ontology:constraint_beneficiary(marriage_market_asymmetry_2026, men_in_dating_market).
narrative_ontology:constraint_victim(marriage_market_asymmetry_2026, women_in_dating_market).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
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
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_market_asymmetry_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_market_asymmetry_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(marriage_market_asymmetry_2026, ExtMetricName, E),
    E >= 0.46. % Correctly identifies the "time-value" extraction as meeting the Snare/Tangled Rope threshold.

:- end_tests(marriage_market_asymmetry_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.65) reflects the "opportunity cost" of uncertain
 * dating rounds, which is higher for women due to compressed biological
 * timelines. The Perspectival Gap exists because Gale-Shapley ignores the asymmetry
 * of information; a man's "Yes" to a female ask contains less signal than
 * a woman's "Yes" to a male ask. Enforcement is maintained by social norms
 * and biological pressures.
 *
 * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical here. A simpler model might classify this as a pure Snare,
 * ignoring the genuine coordination function of dating markets (stable matching). Tangled Rope prevents
 * this mandatrophy by acknowledging both the coordination aspect (the "Rope") and the severe,
 * asymmetric extraction of time and opportunity cost from one group (the "Snare" element),
 * providing a more complete structural diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_marriage_market_asymmetry_2026,
    'Does a religious social group (SCAFFOLD) nullify the information asymmetry?',
    'Comparative study of female-initiation success in chaste vs. secular cohorts.',
    'If chastity works, the Snare is a policy/norm; if not, it is a Mountain of biology.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_market_asymmetry_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Model the drift from "Youth/Time Surplus" (T=0) to "Biological Ceiling" (T=10).

% Theater ratio: Increases as dating rounds lengthen without a "marriage"
% decision, turning coordination into performative "milk for free".
narrative_ontology:measurement(marriage_market_asymmetry_2026_tr_t0, marriage_market_asymmetry_2026, theater_ratio, 0, 0.20).
narrative_ontology:measurement(marriage_market_asymmetry_2026_tr_t5, marriage_market_asymmetry_2026, theater_ratio, 5, 0.35).
narrative_ontology:measurement(marriage_market_asymmetry_2026_tr_t10, marriage_market_asymmetry_2026, theater_ratio, 10, 0.50).

% Extraction: The cost of a "round" increases exponentially for women as
% the time-horizon approaches biological limits.
narrative_ontology:measurement(marriage_market_asymmetry_2026_ex_t0, marriage_market_asymmetry_2026, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(marriage_market_asymmetry_2026_ex_t5, marriage_market_asymmetry_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(marriage_market_asymmetry_2026_ex_t10, marriage_market_asymmetry_2026, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The dating market functions as a mechanism for allocating partners.
narrative_ontology:coordination_type(marriage_market_asymmetry_2026, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */