% ============================================================================
% CONSTRAINT STORY: indian_import_tariffs_eu
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indian_import_tariffs_eu, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: indian_import_tariffs_eu
 *   human_readable: Indian Protective Tariffs on European Union Imports (Autos & Spirits)
 *   domain: economic/political
 *
 * SUMMARY:
 *   India imposes high tariffs on certain European imports, notably cars and
 *   alcoholic beverages. These tariffs are intended to protect domestic
 *   industries, generate revenue, and reduce trade deficits. However, they
 *   also increase prices for consumers and potentially harm international
 *   trade relations. The effect of these protective tariffs is viewed
 *   differently across various agents.
 *
 * KEY AGENTS:
 *   - Indian Domestic Producers: Primary beneficiary (institutional/arbitrage) - benefits from reduced competition.
 *   - Indian Government Revenue: Secondary beneficiary (institutional/constrained) - benefits from tariff collection.
 *   - European Union Exporters: Primary victim (moderate/constrained) - faces reduced market access.
 *   - Indian Consumers: Secondary victim (powerless/trapped) - pays higher prices.
 *   - Analytical Observer: Global perspective (analytical/analytical) - evaluates the overall impact on trade and welfare.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_import_tariffs_eu, 0.65).
domain_priors:suppression_score(indian_import_tariffs_eu, 0.7).
domain_priors:theater_ratio(indian_import_tariffs_eu, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_import_tariffs_eu, extractiveness, 0.65).
narrative_ontology:constraint_metric(indian_import_tariffs_eu, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(indian_import_tariffs_eu, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indian_import_tariffs_eu, tangled_rope).
narrative_ontology:human_readable(indian_import_tariffs_eu, "Indian Protective Tariffs on European Union Imports (Autos & Spirits)").
narrative_ontology:topic_domain(indian_import_tariffs_eu, "economic/political").

domain_priors:requires_active_enforcement(indian_import_tariffs_eu).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indian_import_tariffs_eu, indian_domestic_producers).
narrative_ontology:constraint_beneficiary(indian_import_tariffs_eu, indian_government_revenue).
narrative_ontology:constraint_victim(indian_import_tariffs_eu, european_union_exporters).
narrative_ontology:constraint_victim(indian_import_tariffs_eu, indian_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Indian consumers face higher prices and reduced choices due to the tariffs, with limited ability to influence policy or switch to alternative products. Trapped due to limited import options and high prices, powerless to change the policy.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% EU exporters, particularly car manufacturers and spirits producers, face reduced market access and competitiveness in India. They are constrained by the tariffs but can seek alternative markets or lobby for policy changes. Moderate power, exit is constrained but not fully trapped - could focus on other markets. Benefits in the long-term due to Indian market growth/investment.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Indian domestic producers benefit from reduced competition, allowing them to maintain higher prices and market share. They have arbitrage exit options as they can always sell domestically and capture larger market share. Institutional power, arbitrage opportunities, benefits from protection.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Indian government revenue benefits from tariff collection, providing funding for various programs. However, it is constrained by potential retaliatory tariffs from the EU and the impact on consumer welfare. Constrained exit (need the revenue, don't want a trade war).
constraint_indexing:constraint_classification(indian_import_tariffs_eu, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% An analytical observer sees the tariffs as a complex interplay of protectionism, revenue generation, and international trade dynamics. The system is actively enforced to benefit specific actors while extracting from others, hence a Tangled Rope classification. Trade-offs exist between protecting domestic industry and consumer welfare. Analytical perspective, civilizational scope.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_import_tariffs_eu_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indian_import_tariffs_eu, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_import_tariffs_eu, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indian_import_tariffs_eu, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(indian_import_tariffs_eu_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High - tariffs extract significant value from EU exporters and Indian consumers. Suppression (0.70): High - tariffs create substantial barriers to entry for EU products. Theater Ratio (0.30): Low - the tariffs have a clear functional purpose (protectionism and revenue generation), with limited performative elements. The economic impact is direct and measurable.
 *
 * PERSPECTIVAL GAP:
 *   Indian consumers experience the tariffs as a Snare, limiting their choices and increasing prices. EU exporters view it as a Tangled Rope, where they are constrained by reduced market access but still maintain some export volume. Indian domestic producers see the tariffs as a beneficial Rope, facilitating their growth and profitability. The Indian government experiences it as a Tangled Rope due to benefits from revenue vs diplomatic considerations. The analytical observer sees the full picture as a Tangled Rope – active enforcement to benefit one set of actors by extracting from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: Indian domestic producers and government revenue benefit from the tariffs. Victims: EU exporters and Indian consumers bear the costs. The power and exit options of each agent influence their perception of the constraint. High tariffs mean the directional relationship is significantly skewed towards the beneficiaries at the expense of the victims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tariff_elasticity,
    'What is the price elasticity of demand for imported cars and spirits in India? ',
    'Econometric analysis of historical sales data and tariff changes.',
    'High elasticity: tariffs reduce sales significantly. Low elasticity: tariffs generate revenue with minimal impact on consumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tariff_elasticity, empirical, 'Determines the revenue generation potential and consumption impact of the tariffs.').

omega_variable(
    retaliation_risk,
    'What is the likelihood of the EU imposing retaliatory tariffs on Indian exports?',
    'Analysis of EU trade policy and diplomatic relations with India.',
    'High risk: trade war reduces overall welfare. Low risk: India benefits from protectionism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_risk, empirical, 'Assesses the potential for retaliatory measures from the EU.').

omega_variable(
    long_term_competitiveness,
    'Do the tariffs promote long-term competitiveness of Indian domestic producers, or do they create inefficiencies?',
    'Comparative analysis of productivity and innovation in protected vs. unprotected industries.',
    'Promote competitiveness: tariffs are beneficial. Create inefficiencies: tariffs are harmful.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_competitiveness, empirical, 'Impact on long-term competitiveness of domestic producers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_import_tariffs_eu, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indi_tr_t0, indian_import_tariffs_eu, theater_ratio, 0, 0.1).
narrative_ontology:measurement(indi_tr_t5, indian_import_tariffs_eu, theater_ratio, 5, 0.2).
narrative_ontology:measurement(indi_tr_t10, indian_import_tariffs_eu, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(indi_be_t0, indian_import_tariffs_eu, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(indi_be_t5, indian_import_tariffs_eu, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(indi_be_t10, indian_import_tariffs_eu, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(indian_import_tariffs_eu, global_trade_organization_rules).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
