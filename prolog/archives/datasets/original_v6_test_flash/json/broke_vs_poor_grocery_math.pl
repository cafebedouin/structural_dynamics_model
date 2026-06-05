% ============================================================================
% CONSTRAINT STORY: broke_vs_poor_grocery_math
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_broke_vs_poor_grocery_math, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: broke_vs_poor_grocery_math
 *   human_readable: The Cognitive Load of Poverty (Grocery Math)
 *   domain: economic/social
 *
 * SUMMARY:
 *   This constraint models the cognitive load imposed by poverty, exemplified
 *   by "grocery math"—the mandatory mental tallying of every item's cost to
 *   avoid exceeding available funds at checkout. This constant calculation
 *   and trade-off creates a significant cognitive burden, limiting attention
 *   available for other areas of life. While retailers may see this as
 *   serving a segment of consumers and community organizations try to
 *   alleviate the burden, the individual experiencing poverty is trapped by
 *   financial circumstances, and the analytical observer sees how this
 *   contributes to ongoing inequality.
 *
 * KEY AGENTS:
 *   - Individuals Experiencing Poverty: Primary victim (powerless/trapped) - bears the burden of constant mental calculation.
 *   - Retailers and Food Producers: Primary beneficiary (institutional/arbitrage) - benefits from stable demand even in poverty.
 *   - Local Community Support Organizations: Moderate/Constrained. Attempt to alleviate but are themselves constrained.
 *   - Analytical Observer: Analytical/Analytical - Understands the complexity and interplay.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(broke_vs_poor_grocery_math, 0.65).
domain_priors:suppression_score(broke_vs_poor_grocery_math, 0.7).
domain_priors:theater_ratio(broke_vs_poor_grocery_math, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, extractiveness, 0.65).
narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(broke_vs_poor_grocery_math, tangled_rope).
narrative_ontology:human_readable(broke_vs_poor_grocery_math, "The Cognitive Load of Poverty (Grocery Math)").
narrative_ontology:topic_domain(broke_vs_poor_grocery_math, "economic/social").

domain_priors:requires_active_enforcement(broke_vs_poor_grocery_math).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(broke_vs_poor_grocery_math, retailers_and_food_producers).
narrative_ontology:constraint_victim(broke_vs_poor_grocery_math, individuals_experiencing_poverty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of an individual experiencing poverty: The constant mental calculations and trade-offs necessary to avoid overspending at the grocery store create a significant cognitive burden. The individual is trapped by their financial circumstances, limiting their options for affordable and nutritious food choices. They bear the full burden of the constraint.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of community support organizations: These organizations attempt to alleviate the cognitive load by providing resources like food banks or budgeting assistance, but they are themselves constrained by funding limitations and can only offer partial solutions. They experience a tangled rope: coordination to provide aid, but constrained exit options and limited resources.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% Perspective of retailers and food producers: They benefit from the consistent consumption driven by necessity, even amidst poverty. While individual purchases may be small due to careful budgeting, the overall demand is stable. They can arbitrage by offering lower-priced options to those with limited budgets, creating profit within the constraint. From their point of view, this appears as a helpful coordination of supply to meet demand. There is also extraction insofar as they capture some of the surplus relative to higher income consumers who don't bother with price comparison.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Analytical observer: The observer recognizes the complex interplay of economic forces, social policies, and individual choices. They see how the cognitive load of poverty is both a consequence of and a contributor to ongoing inequality. From a global perspective, this is a tangled rope, requiring both coordination and significant redistribution to address.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(broke_vs_poor_grocery_math_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(broke_vs_poor_grocery_math, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(broke_vs_poor_grocery_math_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The cognitive effort exerted by the individual benefits retailers and food producers by ensuring consistent consumption. It also extracts mental resources that could be used for other beneficial purposes, like job searching or education. Suppression (0.70): High. Limited financial resources suppress access to affordable and nutritious food choices, making 'grocery math' a necessary survival skill. Lack of financial literacy, access to information, and time also contribute to this suppression. Theater Ratio (0.20): Low. There is little performative content.
 *
 * PERSPECTIVAL GAP:
 *   The individual sees a snare, trapped in a cycle of poverty. Retailers, from a distance, may see it as a rope—a market segment they serve. Support organizations attempt to mediate but are constrained. The analytical observer sees a tangled rope, recognizing the need for both coordination (e.g. food banks) and redistribution (e.g. policy changes).
 *
 * DIRECTIONALITY LOGIC:
 *   The individual experiencing poverty (powerless/trapped) has high directionality as the primary target. Retailers (institutional/arbitrage) have low directionality as they benefit. Community Organizations are moderate/constrained and have a directionality closer to neutral.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_cognitive_impact,
    'What is the long-term impact of chronic ''grocery math'' stress on cognitive function and decision-making?',
    'Longitudinal studies comparing cognitive performance of individuals experiencing different levels of financial stress.',
    'Determines the severity of the snare and informs policies aimed at mitigating cognitive load.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_cognitive_impact, empirical, 'Assessment of long-term impact of financial stress.').

omega_variable(
    effectiveness_of_intervention_strategies,
    'How effective are different intervention strategies (e.g., SNAP benefits, financial literacy programs) in reducing the cognitive load of poverty?',
    'Comparative analysis of outcomes for individuals participating in different intervention programs.',
    'Identifies the most promising avenues for policy intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_intervention_strategies, empirical, 'Evaluation of intervention strategy effectiveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(broke_vs_poor_grocery_math, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brok_tr_t0, broke_vs_poor_grocery_math, theater_ratio, 0, 0.15).
narrative_ontology:measurement(brok_tr_t5, broke_vs_poor_grocery_math, theater_ratio, 5, 0.2).
narrative_ontology:measurement(brok_tr_t10, broke_vs_poor_grocery_math, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(brok_be_t0, broke_vs_poor_grocery_math, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(brok_be_t5, broke_vs_poor_grocery_math, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(brok_be_t10, broke_vs_poor_grocery_math, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
