% ============================================================================
% CONSTRAINT STORY: financialization_drag
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_financialization_drag, []).

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
 *   constraint_id: financialization_drag
 *   human_readable: The Financialization Gravity Well
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Financialization Gravity Well describes a scenario where the primary
 *   mechanism for resource allocation shifts from real-world production to
 *   financial engineering. This leads to an increasing proportion of economic
 *   activity being devoted to financial transactions and complex financial
 *   instruments, often at the expense of long-term real economic growth and
 *   social well-being. The beneficiaries are primarily those involved in the
 *   financial sector and corporate managers incentivized by short-term stock
 *   performance. The victims are real economy workers, long-term investors,
 *   and future generations who bear the costs of underinvestment in real
 *   assets and increasing inequality.
 *
 * KEY AGENTS:
 *   - Financial Sector Elites: Primary beneficiary (institutional/arbitrage) - benefits from fees, commissions, and increased financial activity.
 *   - Corporate Managers compensated by stock price: Secondary beneficiary (powerful/constrained) - benefits from short-term stock performance but constrained by the pressure to deliver quarterly results.
 *   - Real Economy Workers: Primary victim (powerless/trapped) - suffer from wage stagnation and job insecurity as companies prioritize financial returns over real investment.
 *   - Long Term Investors: Secondary victim (moderate/mobile) - face lower returns due to the misallocation of capital towards financial assets rather than productive investment.
 *   - Future Generations: Ultimate victim (powerless/trapped) - inherit the long-term costs of unsustainable debt and underinvestment in essential services and infrastructure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(financialization_drag, 0.65).
domain_priors:suppression_score(financialization_drag, 0.7).
domain_priors:theater_ratio(financialization_drag, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(financialization_drag, extractiveness, 0.65).
narrative_ontology:constraint_metric(financialization_drag, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(financialization_drag, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(financialization_drag, tangled_rope).
narrative_ontology:human_readable(financialization_drag, "The Financialization Gravity Well").
narrative_ontology:topic_domain(financialization_drag, "economic/technological").

domain_priors:requires_active_enforcement(financialization_drag).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(financialization_drag, financial_sector_elites).
narrative_ontology:constraint_beneficiary(financialization_drag, corporate_managers_compensated_by_stock_price).
narrative_ontology:constraint_victim(financialization_drag, real_economy_workers).
narrative_ontology:constraint_victim(financialization_drag, long_term_investors).
narrative_ontology:constraint_victim(financialization_drag, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Real economy workers are trapped in a system where wages are suppressed to boost short-term profits and stock prices, with limited exit options and long-term consequences.
constraint_indexing:constraint_classification(financialization_drag, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: Financial sector elites benefit from increased financial activity and complex financial instruments, viewing it as a coordination mechanism for capital allocation, with arbitrage opportunities and immediate gains.
constraint_indexing:constraint_classification(financialization_drag, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 3: From a civilizational perspective, financialization appears as a tangled rope: a hybrid of coordination and extraction where short-term gains are extracted at the expense of long-term sustainability and social equity.
constraint_indexing:constraint_classification(financialization_drag, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective 4: Corporate managers whose compensation is tied to stock price experience a tangled rope. They benefit in the immediate term but are constrained by the pressure to prioritize short-term financial metrics over long-term real economic value creation.
constraint_indexing:constraint_classification(financialization_drag, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 5: Future generations are trapped, inheriting the long-term costs of underinvestment in real assets, environmental degradation, and unsustainable debt levels.
constraint_indexing:constraint_classification(financialization_drag, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(financialization_drag_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(financialization_drag, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(financialization_drag, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(financialization_drag, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(financialization_drag_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. Financialization diverts capital away from productive investment and towards financial assets, extracting value from the real economy. Suppression (0.70): High. Regulatory capture and lobbying efforts by the financial sector suppress alternative economic models and policies that would promote sustainable growth and equitable distribution of wealth. Theater Ratio (0.40): Moderate. While there is some performative compliance with regulations, the financial sector actively seeks to circumvent restrictions and expand its influence.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different structural positions of the agents involved. Financial sector elites see financialization as a legitimate and efficient mechanism for capital allocation, while real economy workers and future generations experience it as a snare that extracts value and undermines their long-term well-being. The analytical observer sees the overall system as a tangled rope, where short-term gains are intertwined with long-term costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Financial sector elites have a low 'd' value because they benefit from financialization and have arbitrage exit options. Real economy workers and future generations have high 'd' values because they are trapped in a system that extracts value from them and have limited exit options. Corporate managers have a moderate 'd' value because they benefit in the short-term, but are constrained by the pressure to deliver immediate financial results.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'tangled rope' classification resolves the mandatrophy by recognizing that financialization is not purely extractive but also involves a degree of coordination, albeit one that is often distorted and ultimately unsustainable. The rope-like aspect of providing liquidity and potentially allocating capital is overshadowed by the extractive elements and the increased systemic risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_productivity_vs_financial_engineering,
    'What is the ratio of genuine productivity gains to purely financial engineering as a source of corporate profit growth?',
    'Detailed sectoral productivity analysis, accounting for real output vs. financial returns.',
    'High ratio implies benign growth; low ratio confirms extractive financialization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_productivity_vs_financial_engineering, empirical, 'Ratio of true productivity gains vs financial engineering').

omega_variable(
    discount_rate_horizon,
    'What is the effective discount rate used by corporations and investors when evaluating long-term investments?',
    'Analysis of investment patterns, capital allocation decisions, and stated corporate strategies.',
    'High discount rate leads to short-termism; low discount rate supports sustainable investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discount_rate_horizon, empirical, 'Effective discount rate used by corporations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(financialization_drag, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fina_tr_t0, financialization_drag, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fina_tr_t10, financialization_drag, theater_ratio, 10, 0.32).
narrative_ontology:measurement(fina_tr_t20, financialization_drag, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(fina_be_t0, financialization_drag, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fina_be_t10, financialization_drag, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(fina_be_t20, financialization_drag, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(financialization_drag, resource_allocation).
narrative_ontology:affects_constraint(financialization_drag, regulatory_capture).
narrative_ontology:affects_constraint(financialization_drag, income_inequality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
