% ============================================================================
% CONSTRAINT STORY: us_visa_lottery
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_visa_lottery, []).

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
 *   constraint_id: us_visa_lottery
 *   human_readable: US Diversity Visa Lottery
 *   domain: political/economic
 *
 * SUMMARY:
 *   The US Diversity Visa (DV) lottery offers 50,000 immigrant visas
 *   annually, drawn randomly from countries with historically low rates of
 *   immigration to the US. The program aims to diversify the immigrant
 *   population in the United States. However, the lottery system creates a
 *   situation where a large number of applicants face significant costs
 *   (application fees, document preparation, opportunity costs) with only a
 *   small chance of success. This creates a tension between the purported
 *   benefits of increased diversity and the extractive nature of a lottery
 *   system.
 *
 * KEY AGENTS:
 *   - Successful Visa Applicants: Moderate benefits, constrained by immigration requirements
 *   - Unsuccessful Applicants: Powerless, trapped in a lottery system
 *   - US Economy: Institutional beneficiary of diverse talent
 *   - Potential Immigrants from Other Countries: Limited opportunities compared to DV countries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_visa_lottery, 0.55).
domain_priors:suppression_score(us_visa_lottery, 0.7).
domain_priors:theater_ratio(us_visa_lottery, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_visa_lottery, extractiveness, 0.55).
narrative_ontology:constraint_metric(us_visa_lottery, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_visa_lottery, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_visa_lottery, tangled_rope).
narrative_ontology:human_readable(us_visa_lottery, "US Diversity Visa Lottery").
narrative_ontology:topic_domain(us_visa_lottery, "political/economic").

domain_priors:requires_active_enforcement(us_visa_lottery).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_visa_lottery, successful_visa_applicants).
narrative_ontology:constraint_beneficiary(us_visa_lottery, us_economy).
narrative_ontology:constraint_victim(us_visa_lottery, unsuccessful_applicants).
narrative_ontology:constraint_victim(us_visa_lottery, potential_immigrants_from_other_countries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Unsuccessful applicants, especially those with limited resources, face significant costs (application fees, opportunity costs) with no guarantee of success. Their options are limited and they are essentially trapped in a lottery system with a high chance of failure.
constraint_indexing:constraint_classification(us_visa_lottery, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The US economy benefits from the inflow of diverse talent and labor, contributing to economic growth and innovation. From this perspective, the lottery acts as a relatively efficient mechanism for attracting immigrants.
constraint_indexing:constraint_classification(us_visa_lottery, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Successful applicants benefit significantly, but are also constrained by the requirements of immigration and integration into a new society. The lottery provides opportunity, but also imposes obligations.
constraint_indexing:constraint_classification(us_visa_lottery, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% From an analytical perspective, the lottery combines coordination (attracting needed immigrants) with extraction (creating a system where many applicants lose time and money, while some win big). It represents a calculated balancing act.
constraint_indexing:constraint_classification(us_visa_lottery, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_visa_lottery_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_visa_lottery, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_visa_lottery, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_visa_lottery, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_visa_lottery_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The program extracts resources (application fees, preparation costs) from a large pool of applicants, most of whom are unsuccessful. The benefit concentrates among the successful applicants and the US economy. Suppression (0.70): High. The program restricts eligibility based on nationality, suppressing opportunities for individuals from countries with higher immigration rates to the US. Theater Ratio (0.30): Low. The actual selection process is relatively transparent and involves minimal performative elements.
 *
 * PERSPECTIVAL GAP:
 *   Unsuccessful applicants perceive the lottery as a snare due to the costs and low probability of success. The US economy perceives it as a rope because it efficiently attracts immigrants. Successful applicants view it as tangled rope because while they gain significant benefits, they are also constrained by the requirements of immigration. An analytical observer sees the complexity involved in a system with both extractive and coordinative elements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality reflects the relationship between each agent and the constraint. Unsuccessful applicants have a high 'd' value because they bear most of the costs. The US economy has a low 'd' value because it receives the benefits. Successful applicants have a moderate 'd' value because they experience both benefits and constraints.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_economic_impact,
    'What is the long-term economic impact of the diversity visa lottery on the US economy?',
    'Economic modeling and longitudinal studies tracking the contributions of diversity visa recipients',
    'If the long-term impact is significantly positive, the perspective shifts towards ''rope''. If neutral or negative, the perspective shifts towards ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_economic_impact, empirical, 'Assesses the overall economic contribution of lottery immigrants').

omega_variable(
    fairness_of_selection,
    'Is the lottery selection process truly random and unbiased?',
    'Independent audits and statistical analysis of the selection process.',
    'If the process is biased, the perspective shifts towards ''snare'' for those disadvantaged. If fair, the perspective remains ''tangled_rope''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fairness_of_selection, empirical, 'Evaluates the integrity of the randomization process').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_visa_lottery, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_v_tr_t0, us_visa_lottery, theater_ratio, 0, 0.25).
narrative_ontology:measurement(us_v_tr_t10, us_visa_lottery, theater_ratio, 10, 0.3).
narrative_ontology:measurement(us_v_tr_t20, us_visa_lottery, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(us_v_be_t0, us_visa_lottery, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(us_v_be_t10, us_visa_lottery, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(us_v_be_t20, us_visa_lottery, base_extractiveness, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_visa_lottery, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
