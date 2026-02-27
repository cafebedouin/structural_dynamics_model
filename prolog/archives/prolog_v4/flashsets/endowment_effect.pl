% ============================================================================
% CONSTRAINT STORY: endowment_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_endowment_effect, []).

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
 *   constraint_id: endowment_effect
 *   human_readable: The Endowment Effect
 *   domain: economic/cognitive
 *
 * SUMMARY:
 *   The endowment effect describes the tendency for people to place a higher
 *   value on objects they own compared to objects they do not. This bias
 *   affects market transactions, negotiations, and resource allocation. It's
 *   an example of how cognitive biases distort rational economic behavior.
 *   The extractiveness represents the welfare loss due to inefficient
 *   allocation of resources.
 *
 * KEY AGENTS:
 *   - Potential Buyers: Primary target (powerless/trapped) — unable to acquire goods at reasonable prices.
 *   - Sellers/Owners: Primary beneficiary (moderate/constrained) — benefits from inflated prices, but constrained by market demand.
 *   - Behavioral Economics Researchers: Analytical observer (institutional/arbitrage) — benefits from studying and understanding the effect.
 *   - Market Efficiency: Systemic victim (analytical/constrained) — overall efficiency is reduced.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(endowment_effect, 0.5).
domain_priors:suppression_score(endowment_effect, 0.3).
domain_priors:theater_ratio(endowment_effect, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(endowment_effect, extractiveness, 0.5).
narrative_ontology:constraint_metric(endowment_effect, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(endowment_effect, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(endowment_effect, tangled_rope).
narrative_ontology:human_readable(endowment_effect, "The Endowment Effect").
narrative_ontology:topic_domain(endowment_effect, "economic/cognitive").

domain_priors:requires_active_enforcement(endowment_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(endowment_effect, sellers).
narrative_ontology:constraint_beneficiary(endowment_effect, incumbent_owners).
narrative_ontology:constraint_victim(endowment_effect, potential_buyers).
narrative_ontology:constraint_victim(endowment_effect, market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Potential buyers, lacking the endowment, find it harder to acquire goods at the inflated prices set by sellers, especially when they have limited access to alternatives or are emotionally attached to acquiring a specific item. They are essentially trapped by the seller's inflated valuation.
constraint_indexing:constraint_classification(endowment_effect, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Sellers benefit from the endowment effect by being able to set higher prices, extracting more value than the market might otherwise bear. However, they are constrained by the risk of not selling the item at all if their price is too high. This perspective captures the mixed nature of the effect: a benefit to the seller but constrained by market realities.
constraint_indexing:constraint_classification(endowment_effect, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Researchers benefit from studying this effect, gaining insights into cognitive biases and market inefficiencies. They are able to arbitrage this knowledge to refine economic models and potentially design interventions. They benefit from the predictable irrationality.
constraint_indexing:constraint_classification(endowment_effect, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From the perspective of overall market efficiency, the endowment effect introduces inefficiencies and distortions. While it might not be completely trapped, the market is constrained by these biases, leading to suboptimal resource allocation. The analytical perspective sees the extraction as a reduction in overall welfare but also recognizes the potential coordination function of established ownership and trust.
constraint_indexing:constraint_classification(endowment_effect, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(endowment_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(endowment_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(endowment_effect, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(endowment_effect, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(endowment_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50): Moderate. Reflects the inefficient allocation of resources and potential welfare loss due to inflated prices. Suppression (0.30): Low-Moderate. While alternatives exist, the endowment effect creates a barrier for potential buyers, limiting their choices and suppressing the overall market activity. Theater ratio (0.20): Low. Not much theater in the manifestation of the effect - the inflated prices are a direct expression of the bias, not a performative ritual.
 *
 * PERSPECTIVAL GAP:
 *   Potential buyers experience a snare because the endowment effect limits their access to goods at fair prices, effectively trapping them in a disadvantageous position. Sellers, on the other hand, experience a tangled rope. They benefit from the increased valuation they place on their possessions, enabling them to demand higher prices. However, they are also constrained by the market's willingness to pay, as overly inflated prices can deter potential buyers, leading to a failure to sell. The analytical observer can arbitrage this knowledge.
 *
 * DIRECTIONALITY LOGIC:
 *   Buyers have a trapped exit so see a snare. Sellers are constrained but benefit from inflated prices. Researchers are analytical. The market itself is constrained by cognitive biases.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification resolves the mandatrophy by acknowledging the mixed nature of the endowment effect. It's not pure extraction because sellers are still subject to market forces and must balance their inflated valuation with the risk of not selling. It's not pure coordination because potential buyers are disadvantaged by the effect, leading to inefficiencies and potential welfare loss. The effect isn't a natural law but it is a genuine cognitive bias.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magnitude_variability,
    'How much does the magnitude of the endowment effect vary across different goods, populations, and experimental designs?',
    'Meta-analysis of endowment effect studies; identification of moderating variables (e.g., good type, cultural context)',
    'If magnitude is highly variable: effect is context-dependent, limiting generalizability. If magnitude is relatively stable: effect is robust, supporting universal cognitive bias.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magnitude_variability, empirical, 'Magnitude of endowment effect variability.').

omega_variable(
    cognitive_mechanism,
    'What are the underlying cognitive mechanisms driving the endowment effect (e.g., loss aversion, psychological ownership)?',
    'Neuroimaging studies; cognitive modeling; experimental manipulations targeting specific cognitive processes',
    'Identifying the mechanism informs interventions. Loss aversion suggests framing effects, psychological ownership implies strategies targeting attachment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_mechanism, conceptual, 'Cognitive mechanism driving endowment effect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(endowment_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(endo_tr_t0, endowment_effect, theater_ratio, 0, 0.1).
narrative_ontology:measurement(endo_tr_t5, endowment_effect, theater_ratio, 5, 0.15).
narrative_ontology:measurement(endo_tr_t10, endowment_effect, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(endo_be_t0, endowment_effect, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(endo_be_t5, endowment_effect, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(endo_be_t10, endowment_effect, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(endowment_effect, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
