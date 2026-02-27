% ============================================================================
% CONSTRAINT STORY: carrying_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_carrying_capacity, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: carrying_capacity
 *   human_readable: Management of Ecological Carrying Capacity (K)
 *   domain: economic/technological/social
 *
 * SUMMARY:
 *   The management of ecological carrying capacity (K) is a critical
 *   challenge that involves balancing human needs with the ability of the
 *   environment to sustain them. This constraint highlights the tensions
 *   between short-term economic growth, long-term environmental
 *   sustainability, and intergenerational equity. The exploitation of
 *   resources beyond sustainable levels can lead to environmental
 *   degradation, resource depletion, and social instability.
 *
 * KEY AGENTS:
 *   - Resource Extraction Industries: Primary beneficiaries (institutional/arbitrage) - profit from exceeding K in the short term.
 *   - Long-Term Environmental Sustainability: Primary victim (powerless/trapped) - cannot organize or exit and bears the long-term costs.
 *   - National Governments: Moderate position (moderate/constrained) - balance economic development with environmental protection.
 *   - Future Generations: Victims (powerless/trapped) - inherit degraded environment.
 *   - Vulnerable Populations: Victims (powerless/trapped) - disproportionately affected by environmental degradation.
 *   - Analytical Observer: Analytical (analytical/analytical) - attempts to understand the overall system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(carrying_capacity, 0.6).
domain_priors:suppression_score(carrying_capacity, 0.7).
domain_priors:theater_ratio(carrying_capacity, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(carrying_capacity, extractiveness, 0.6).
narrative_ontology:constraint_metric(carrying_capacity, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(carrying_capacity, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(carrying_capacity, tangled_rope).
narrative_ontology:human_readable(carrying_capacity, "Management of Ecological Carrying Capacity (K)").
narrative_ontology:topic_domain(carrying_capacity, "economic/technological/social").

domain_priors:requires_active_enforcement(carrying_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(carrying_capacity, resource_extraction_industries).
narrative_ontology:constraint_beneficiary(carrying_capacity, short_term_economic_growth).
narrative_ontology:constraint_victim(carrying_capacity, long_term_environmental_sustainability).
narrative_ontology:constraint_victim(carrying_capacity, future_generations).
narrative_ontology:constraint_victim(carrying_capacity, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Future generations bear the long-term consequences of exceeding carrying capacity, with limited ability to influence current practices or escape the resulting environmental degradation.
constraint_indexing:constraint_classification(carrying_capacity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Governments are constrained by competing interests: short-term economic gains vs. long-term sustainability. They benefit from economic activity within their borders, but also bear the costs of environmental degradation and resource depletion. They can enact some regulations, but are also subject to lobbying and international pressures.
constraint_indexing:constraint_classification(carrying_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Industries that extract resources benefit from exceeding carrying capacity in the short term. They can arbitrage regulations by moving to countries with weaker environmental protections. Their primary concern is profit maximization.
constraint_indexing:constraint_classification(carrying_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational perspective, the management of carrying capacity represents a complex interplay of economic, social, and environmental factors. Exceeding carrying capacity provides short-term benefits for some, but leads to long-term consequences for all. Mitigation requires global cooperation and sustainable practices.
constraint_indexing:constraint_classification(carrying_capacity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carrying_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(carrying_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(carrying_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(carrying_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(carrying_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The extraction from the environment is significant, as resources are depleted beyond the rate of regeneration. Suppression (0.7): Efforts to implement sustainable practices are often suppressed by short-term economic interests and political pressures. Theater ratio (0.3): There is some effort put into sustainability projects, but these often have limited impact compared to the scale of resource extraction.
 *
 * PERSPECTIVAL GAP:
 *   Different actors experience this constraint differently. Resource extraction industries benefit in the short term, while future generations bear the long-term consequences. National governments face pressure to balance economic growth with environmental protection. An analytical observer can see the overall system and the inherent risks of exceeding carrying capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (resource extraction industries) have the power and ability to arbitrage regulations and prioritize short term economic gains. Victims (future generations, long term sustainability) are often powerless, trapped, and unable to effectively mitigate the harm caused by over extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the management of carrying capacity as simply 'good' or 'bad'. While exceeding K can provide short-term economic benefits, it ultimately leads to unsustainable practices and long-term negative consequences. The 'tangled rope' classification captures the complex interplay of coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    k_measurement_uncertainty,
    'How accurately can ecological carrying capacity be measured, given complex interactions and feedback loops?',
    'Improved modeling techniques, long-term ecological monitoring, and cross-disciplinary research',
    'Underestimation of K leads to premature resource depletion. Overestimation leads to ecological collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(k_measurement_uncertainty, empirical, 'Uncertainty in measuring carrying capacity').

omega_variable(
    discount_rate,
    'How should future environmental damage be discounted relative to present economic benefits?',
    'Ethical and economic debates on intergenerational equity, incorporating non-monetary values',
    'High discount rates prioritize short-term gains, leading to unsustainable practices. Low discount rates prioritize long-term sustainability, potentially hindering economic growth.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discount_rate, preference, 'Value of future ecological damage vs present economic benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(carrying_capacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carr_tr_t0, carrying_capacity, theater_ratio, 0, 0.1).
narrative_ontology:measurement(carr_tr_t5, carrying_capacity, theater_ratio, 5, 0.2).
narrative_ontology:measurement(carr_tr_t10, carrying_capacity, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(carr_be_t0, carrying_capacity, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(carr_be_t5, carrying_capacity, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(carr_be_t10, carrying_capacity, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(carrying_capacity, resource_allocation).
narrative_ontology:affects_constraint(carrying_capacity, climate_change).
narrative_ontology:affects_constraint(carrying_capacity, resource_depletion).
narrative_ontology:affects_constraint(carrying_capacity, biodiversity_loss).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
