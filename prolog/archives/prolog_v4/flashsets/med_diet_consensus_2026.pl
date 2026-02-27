% ============================================================================
% CONSTRAINT STORY: med_diet_consensus_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_med_diet_consensus_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: med_diet_consensus_2026
 *   human_readable: Mediterranean Diet Scientific Hegemony
 *   domain: health/scientific/economic
 *
 * SUMMARY:
 *   The Mediterranean diet is widely recognized as a healthy eating pattern,
 *   but this consensus creates a system with both coordination and extraction
 *   aspects. The promotion of the diet can benefit local economies and food
 *   producers, but it can also marginalize alternative dietary approaches and
 *   create barriers to accessing healthy foods for low-income populations.
 *   The 'gold standard' status creates a scientific and economic hegemony.
 *
 * KEY AGENTS:
 *   - Olive Oil Producers: Primary beneficiary (institutional/arbitrage) - benefit from increased demand and premium pricing.
 *   - Alternative Diet Proponents: Primary victim (powerless/trapped) - struggle to gain scientific acceptance and funding.
 *   - Nutrition Researchers (Med Diet): Moderate Beneficiary (moderate/constrained) - benefit from funding, but face conformity pressures.
 *   - Mediterranean Restaurants: Moderate Beneficiary (powerful/mobile) - benefit from popularity, but constrained by authenticity requirements
 *   - Mediterranean Tourism: Primary Beneficiary (institutional/arbitrage) - Benefits from diet-related tourism
 *   - Nutrition Researchers (Non-Med Diet): Primary Victim (powerless/trapped) - Struggle to gain funding and recognition
 *   - Low Income Populations (Non Med Regions): Secondary Victim (powerless/trapped) - Unable to afford high-priced ingredients
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(med_diet_consensus_2026, 0.55).
domain_priors:suppression_score(med_diet_consensus_2026, 0.45).
domain_priors:theater_ratio(med_diet_consensus_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(med_diet_consensus_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(med_diet_consensus_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(med_diet_consensus_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(med_diet_consensus_2026, tangled_rope).
narrative_ontology:human_readable(med_diet_consensus_2026, "Mediterranean Diet Scientific Hegemony").
narrative_ontology:topic_domain(med_diet_consensus_2026, "health/scientific/economic").

domain_priors:requires_active_enforcement(med_diet_consensus_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, olive_oil_producers).
narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, mediterranean_restaurants).
narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, nutrition_researchers_med_diet).
narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, mediterranean_tourism).
narrative_ontology:constraint_victim(med_diet_consensus_2026, alternative_diet_proponents).
narrative_ontology:constraint_victim(med_diet_consensus_2026, low_income_populations_non_med_regions).
narrative_ontology:constraint_victim(med_diet_consensus_2026, researchers_non_med_diets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Proponents of alternative diets (e.g., ketogenic, vegan, carnivore) find it difficult to gain mainstream scientific acceptance and funding due to the established consensus. Their careers and research are suppressed.
constraint_indexing:constraint_classification(med_diet_consensus_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Nutrition researchers focused on the Mediterranean diet benefit from grant funding and publication opportunities, but also face pressure to conform to established findings and avoid contradicting the consensus.
constraint_indexing:constraint_classification(med_diet_consensus_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Olive oil producers benefit from the increased demand and premium pricing associated with the Mediterranean diet. They can leverage the diet's reputation for marketing purposes.
constraint_indexing:constraint_classification(med_diet_consensus_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Mediterranean restaurants benefit from the diet's popularity, but are also constrained by the need to adhere to perceived authentic ingredients and preparations.
constraint_indexing:constraint_classification(med_diet_consensus_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Regions promoting Mediterranean tourism benefit from increased visitor numbers due to the association with health and lifestyle.
constraint_indexing:constraint_classification(med_diet_consensus_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational perspective, the Mediterranean diet's perceived benefits may be overstated or culturally biased. This perspective sees the tension between legitimate health recommendations and economic/cultural interests as a mixed coordination and extraction scheme.
constraint_indexing:constraint_classification(med_diet_consensus_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(med_diet_consensus_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(med_diet_consensus_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(med_diet_consensus_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(med_diet_consensus_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(med_diet_consensus_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.55) because the consensus creates barriers for alternative diets and can increase the cost of healthy eating. Suppression is also moderate (0.45) because while alternative diets can exist, they have difficulty gaining mainstream acceptance. The theater ratio is low (0.30) because the focus is primarily on health outcomes rather than performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   The proponents of the Mediterranean diet see it as a rope, a beneficial health standard. The proponents of alternative diets see it as a snare, limiting their ability to gain recognition and funding. The analytical observer sees it as a tangled rope, a system that promotes health but also has extractive aspects that need to be addressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (e.g., olive oil producers) experience positive effects from the Mediterranean diet's promotion. Victims (e.g., proponents of alternative diets) experience negative effects, such as difficulty obtaining funding. This directionality influences the classification from each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by recognizing that the Mediterranean diet is neither purely beneficial nor purely extractive. It is a complex system with elements of both. The key is to identify and mitigate the extractive aspects while preserving the benefits of promoting a healthy eating pattern. Understanding the different perspectives helps to manage the tensions and avoid unintended consequences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_bias_in_evidence,
    'To what extent are the observed benefits of the Mediterranean diet confounded by cultural and lifestyle factors?',
    'Conduct randomized controlled trials that isolate dietary factors from other lifestyle variables; perform meta-analyses that account for cultural differences.',
    'If cultural factors are significant confounders, the diet''s benefits may be overstated. If not, the diet''s benefits are more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_bias_in_evidence, empirical, 'Impact of cultural and lifestyle factors on Mediterranean diet benefits.').

omega_variable(
    generalizability_across_populations,
    'How well do the benefits of the Mediterranean diet generalize to populations outside the Mediterranean region?',
    'Conduct large-scale prospective studies in diverse populations; compare health outcomes across different dietary patterns.',
    'If the diet''s benefits are not generalizable, its widespread promotion may be inappropriate. If the diet''s benefits generalize, its promotion is more justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generalizability_across_populations, empirical, 'Generalizability of Mediterranean diet benefits to non-Mediterranean populations.').

omega_variable(
    economic_interests_influence,
    'To what extent do economic interests (e.g., olive oil industry, tourism) influence research and promotion of the Mediterranean diet?',
    'Analyze funding sources and potential conflicts of interest in research studies; examine marketing campaigns and promotional materials.',
    'If economic interests significantly influence research and promotion, the diet''s benefits may be overstated. If not, the diet''s promotion is more objective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_interests_influence, empirical, 'Influence of economic interests on Mediterranean diet research and promotion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(med_diet_consensus_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(med__tr_t0, med_diet_consensus_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(med__tr_t5, med_diet_consensus_2026, theater_ratio, 5, 0.22).
narrative_ontology:measurement(med__tr_t10, med_diet_consensus_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(med__be_t0, med_diet_consensus_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(med__be_t5, med_diet_consensus_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(med__be_t10, med_diet_consensus_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(med_diet_consensus_2026, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
