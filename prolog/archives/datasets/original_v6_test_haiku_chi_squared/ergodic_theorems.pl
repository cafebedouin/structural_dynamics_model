% ============================================================================
% CONSTRAINT STORY: ergodic_theorems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergodic_theorems, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ergodic_theorems
 *   human_readable: The Misapplication of Ergodic Theorems in Non-Ergodic Systems
 *   domain: economic/scientific
 *
 * SUMMARY:
 *   The ergodic hypothesis — that a system's time-averaged behavior equals
 *   its ensemble-averaged behavior — is a powerful mathematical tool for
 *   physics and economics. But real financial markets are fundamentally
 *   non-ergodic: they contain absorbing barriers (bankruptcy), path-dependent
 *   collapses (cascading margin calls), and fat-tailed rare events that
 *   ergodic models systematically underestimate. The constraint emerges when
 *   institutional actors (regulators, banks, academic researchers) enforce
 *   the application of ergodic theorems to financial systems despite growing
 *   evidence of non-ergodicity. This constraint exhibits hybrid
 *   characteristics: it coordinates financial modeling across institutions
 *   (shared language for risk reporting, Basel III standardization) while
 *   extracting risk from long-horizon investors and the financial system
 *   itself through the systematic underpricing of tail events and systemic
 *   stability risks. Theater has increased over the measurement interval as
 *   regulatory scrutiny and model validation have become more elaborate while
 *   risk model accuracy has not improved proportionally — regulatory reviews
 *   now consume substantial resources but do not fundamentally change the
 *   ergodic axioms underlying the models. The constraint is neither purely
 *   coordination (it suppresses non-ergodic alternatives and stabilizes the
 *   ergodic paradigm) nor purely extraction (institutions genuinely benefit
 *   from analytical tractability), making Tangled Rope the analytical
 *   classification.
 *
 * KEY AGENTS:
 *   - Tail Risk Bearers: Primary victims (powerless/trapped) — long-horizon investors and pension funds that absorb hidden tail risks; cannot exit without liquidating portfolios; systematically extracting portfolio value through underestimated collapse risk
 *   - Financial System Stability: Primary victim (powerless/trapped) — systemic risk from correlated tail events that ergodic models do not predict; trapped by institutional belief in ergodic soundness
 *   - Equilibrium Economics Researchers: Primary beneficiaries (institutional/arbitrage) — benefit from ergodic framework as foundational tool for tractable models; can exit to non-equilibrium alternatives but prefer not to
 *   - Risk Modeling Industry: Secondary beneficiary (institutional/arbitrage) — profitable to sell VaR and stress tests; exit options abundant but profitable to maintain backward compatibility
 *   - Financial Regulators: Institutional actor (institutional/constrained) — maintain ergodic-based capital requirements despite known empirical failures; trapped by institutional legitimacy and lack of agreed-upon alternatives
 *   - Risk-Aware Portfolio Managers: Secondary victim (moderate/constrained) — constrained by regulatory requirements to use ergodic models; benefit from access to research and standardized metrics but face career risk if they deviate from peer benchmarks
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes both the mathematical validity of ergodic theorems and their misapplication as an institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergodic_theorems, 0.52).
domain_priors:suppression_score(ergodic_theorems, 0.68).
domain_priors:theater_ratio(ergodic_theorems, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergodic_theorems, extractiveness, 0.52).
narrative_ontology:constraint_metric(ergodic_theorems, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ergodic_theorems, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergodic_theorems, tangled_rope).
narrative_ontology:human_readable(ergodic_theorems, "The Misapplication of Ergodic Theorems in Non-Ergodic Systems").
narrative_ontology:topic_domain(ergodic_theorems, "economic/scientific").

domain_priors:requires_active_enforcement(ergodic_theorems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergodic_theorems, equilibrium_economics_researchers).
narrative_ontology:constraint_beneficiary(ergodic_theorems, portfolio_risk_modelers).
narrative_ontology:constraint_beneficiary(ergodic_theorems, macroeconomic_forecasters).
narrative_ontology:constraint_victim(ergodic_theorems, tail_risk_bearers).
narrative_ontology:constraint_victim(ergodic_theorems, long_horizon_investors).
narrative_ontology:constraint_victim(ergodic_theorems, systemic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAIL RISK BEARER (SNARE) — Investors with long-horizon portfolios face extraction through systematic underestimation of rare events and path-dependent collapse risks. Cannot exit without abandoning portfolio; ergodic assumption naturalizes risks as impossible. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.87.
constraint_indexing:constraint_classification(ergodic_theorems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SYSTEMIC STABILITY (SNARE) — The financial system itself bears extraction through models that assume away fat tails, duration risk, and cascade failures. Trapped by the institutional belief that ergodic models are foundationally sound. No exit mechanism when rare events materialize. d≈0.94, f(d)≈1.40, σ=1.2 → χ≈0.92.
constraint_indexing:constraint_classification(ergodic_theorems, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: RISK-AWARE PORTFOLIO MANAGERS (TANGLED ROPE) — Constrained by peer benchmarking and regulator-mandated ergodic-based VaR models, but benefit from access to research infrastructure and pricing models. Can exit only by building alternative models (high cost). d≈0.72, f(d)≈1.08, σ=1.0 → χ≈0.56.
constraint_indexing:constraint_classification(ergodic_theorems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EQUILIBRIUM ECONOMICS RESEARCHERS (ROPE) — Benefit from ergodic framework as foundation for entire research program. Coordination function: ergodic models enable parsimony and analytical tractability. Exit options abundant: can switch to non-equilibrium dynamics or agent-based simulation. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Net beneficiaries.
constraint_indexing:constraint_classification(ergodic_theorems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RISK MODELING INDUSTRY (ROPE) — Institutional beneficiary. Ergodic models enable efficient pricing and standardized risk metrics. Sell VaR, stress tests, portfolio optimization algorithms based on ergodic axioms. High exit cost (retraining, recalibration), but arbitrage opportunities: can offer 'enhanced' models while maintaining backward compatibility. d≈0.12, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(ergodic_theorems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: FINANCIAL REGULATORS (PITON) — Maintain Basel III ergodic-based capital requirements and Value-at-Risk mandates not because they work (empirical failures in 2008, 2020 market dislocations) but because alternatives lack institutional legitimacy. Theater_ratio≈0.78: regulatory reviews, stress tests, and model validation are largely performative — they do not substantially reduce systemic risk but provide the appearance of oversight. d≈0.45, f(d)≈0.48, σ=1.1 → χ≈0.20 (piton gate fires on theater ≥0.70).
constraint_indexing:constraint_classification(ergodic_theorems, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: MATHEMATICAL NATURALIST (MOUNTAIN) — Ergodic theorems are true mathematical statements about systems with specific properties. If a system is ergodic, time and ensemble averages converge. The constraint appears as a natural law: ergodicity IS a property of some systems, not a human choice. However, the structural data (ε=0.52, suppression=0.68, theater=0.78) contradicts mountain classification — the problem is not mathematical but institutional: the misapplication of the theorem to non-ergodic systems. Engine detects false summit.
constraint_indexing:constraint_classification(ergodic_theorems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint is coordination (economists need a shared baseline model language for cross-firm comparison) PLUS asymmetric extraction (the baseline model suppresses risks that favor financial institutions). The theorem itself is true; the misapplication is a choice. Institutional inertia enforces the misapplication despite accumulating evidence of non-ergodicity in real markets. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.41.
constraint_indexing:constraint_classification(ergodic_theorems, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergodic_theorems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergodic_theorems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergodic_theorems, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ergodic_theorems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ergodic_theorems, TR),
    TR >= 0.70.

:- end_tests(ergodic_theorems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The constraint extracts through systematic underestimation of tail risks. Starting value (0.28) reflects the early period when ergodic models were reasonable first approximations. Current value (0.52) reflects accumulated evidence of non-ergodicity in real markets (volatility clustering, path-dependent collapses, frequency of margin calls exceeding VaR predictions) combined with institutional persistence in using the models despite this evidence. This is not pure extraction (ε would be >0.70) because the ergodic framework also provides genuine coordination benefits: standardized risk language enables cross-institutional comparison and regulatory harmonization. Suppression (0.68): Significant barriers prevent adoption of non-ergodic models: (1) regulatory inertia — Basel III is written in ergodic language, changing it requires international treaty-level coordination; (2) institutional validation — non-ergodic models lack the centuries of mathematical authority that ergodic theory carries; (3) path-dependent lock-in — financial systems have built trillion-dollar portfolios priced under ergodic assumptions; (4) publication bias — ergodic models are more 'publishable' in mainstream finance because they appear mathematically rigorous. Theater ratio (0.78): Regulatory stress tests, model backtests, and capital requirement reviews consume substantial institutional resources but do not materially change the ergodic axioms. The theater has increased as regulators have added more elaborate validation procedures (counterparty risk stress tests, liquidity coverage ratios) without moving to non-ergodic foundations. The performative content increased after 2008, when ergodic models failed to predict the crisis, but rather than replace them, institutions layered additional stress-test theater on top.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a profound perspectival gap between beneficiaries and victims. The equilibrium researcher and risk modeler see a coordination mechanism (Rope): ergodic models provide tractable language for cross-firm comparison and enable analytical solutions. The tail risk bearer sees pure extraction (Snare): their capital is systematically extracted through underpriced rare events. The financial system sees systemic extraction (Snare): the models suppress recognition of cascade failures. The regulator sees a piton: the ergodic framework is maintained through institutional inertia despite known empirical failures (theater_ratio=0.78, indicating degradation). The analytical observer sees Tangled Rope: genuine coordination (shared modeling language) plus asymmetric extraction (hidden tail risks), with suppression mechanisms (regulatory lock-in, publication bias) enforcing the coordination function. The mathematical naturalist might see a Mountain (ergodic theorems are true), but this is a false summit — the constraint is the institutional choice to apply true theorems to non-ergodic systems.
 *
 * DIRECTIONALITY LOGIC:
 *   Tail risk bearers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Financial system: Victim + trapped → d≈0.94, f(d)≈1.40. Maximum extraction. Risk-aware managers: Victim + constrained → d≈0.72, f(d)≈1.08. High extraction but constrained by peer pressure and regulatory requirements. Equilibrium researchers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Low d because they have exit options and genuinely benefit. Risk modeling industry: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.08. Low d because profitable and exit options abundant. Regulators: Mixed + constrained → d≈0.45, f(d)≈0.48. Piton classification derives from theater gate (0.78 ≥ 0.70), not from high directionality. Analytical observer: analytical → d≈0.50, f(d)≈0.65, derives from balanced view of coordination + extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves mandatrophy by differentiating between the mathematical claim and its institutional application. The ergodic theorem itself is a Mountain — true for systems that satisfy the ergodic property. But the institutional choice to apply ergodic models to non-ergodic financial systems is a Tangled Rope — it provides coordination (standardized risk language, analytical tractability) while extracting through suppression of tail risk and systemic risk awareness. The piton perspective (regulators maintaining degraded ergodic-based oversight) shows how the coordination function has atrophied (theater increased while model accuracy stagnated) while the institutional enforcement persisted. The misapplication is not inevitable — it is a choice made by actors with asymmetric incentives. Beneficiaries (risk modelers, equilibrium researchers) have arbitrage options but profit from the status quo. Victims (tail risk bearers, systemic stability) have no exit. The constraint persists because the beneficiaries have sufficient institutional power to suppress non-ergodic alternatives through regulatory capture, publication bias, and educational lock-in (graduate finance programs teach ergodic finance as foundation). The mandatrophy is resolved: the constraint is real extraction, not coordination failure, because the extraction is maintained despite clear evidence of non-ergodicity and superior alternatives (path-dependent risk models, Monte Carlo simulation, agent-based models) being available.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_ergodicity_threshold,
    'What criterion defines when a market transitions from ergodic to non-ergodic regime? Is it path-dependent collapse risk, fat tails, or phase transitions in correlation structure?',
    'Empirical testing of ergodic hypothesis for major asset classes (equities, bonds, commodities) using turnover, holding period distribution, and rare event frequency. Spectral analysis of correlation matrices for eigenvalue delocalization.',
    'If threshold is well-defined: misapplication is a choice, not ambiguity (strengthens snare classification). If threshold is context-dependent: some ergodic approximations are justified in some regimes (weakens to scaffold, uncertainty about sunset).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_ergodicity_threshold, empirical, 'Criterion for ergodic-to-non-ergodic transition in financial markets').

omega_variable(
    tail_event_frequency_estimation,
    'Can ensemble averages over historical data reliably estimate tail risk frequencies when the data comes from a single, non-stationary time path?',
    'Comparison of tail event predictions (VaR, expected shortfall) from ergodic models vs non-ergodic path-dependent models using out-of-sample validation. Backtesting frequency of tail breaks.',
    'If ergodic models consistently underestimate tail frequency: extraction is severe (supports snare). If they provide unbiased estimates within historical ranges: misapplication is moderate risk (supports tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tail_event_frequency_estimation, empirical, 'Reliability of ergodic ensemble averaging for tail risk estimation').

omega_variable(
    institutional_exit_cost_measurement,
    'What is the actual cost to a regulated financial institution of adopting non-ergodic risk models in place of Basel III ergodic VaR requirements?',
    'Cost audit: regulatory re-approval, IT infrastructure changes, model validation cycles, capital requirement recalculation. Institutional economic analysis of why major banks have not migrated to non-ergodic frameworks despite academic availability.',
    'If exit cost is low: constraint is choice, not trap (supports snare/piton). If exit cost is prohibitive: constraint has genuine structural power (supports tangled rope with high suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_exit_cost_measurement, empirical, 'Regulatory and operational cost of adopting non-ergodic risk models').

omega_variable(
    regulatory_capture_mechanism,
    'Is the continuation of ergodic-based regulation a form of regulatory capture by financial institutions that benefit from underestimated tail risk?',
    'Institutional analysis: track revolving doors between banks and regulators, funding sources for risk model research, publication bias in peer review of risk models.',
    'If capture is strong: snare classification strengthened, beneficiaries are financial institutions (not academic researchers). If capture is weak: misapplication is shared institutional inertia (tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, conceptual, 'Degree of regulatory capture in perpetuating ergodic-based risk regulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergodic_theorems, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergo_tr_t0, ergodic_theorems, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ergo_tr_t10, ergodic_theorems, theater_ratio, 10, 0.68).
narrative_ontology:measurement(ergo_tr_t20, ergodic_theorems, theater_ratio, 20, 0.78).

% Extraction over time
narrative_ontology:measurement(ergo_be_t0, ergodic_theorems, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ergo_be_t10, ergodic_theorems, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(ergo_be_t20, ergodic_theorems, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergodic_theorems, information_standard).
narrative_ontology:affects_constraint(ergodic_theorems, value_at_risk_methodology).
narrative_ontology:affects_constraint(ergodic_theorems, long_term_capital_management_paradigm).
narrative_ontology:affects_constraint(ergodic_theorems, systemic_financial_risk_underestimation).

% DUAL FORMULATION NOTE:
% The ergodic misapplication constraint decomposes into two: (1) the mathematical ergodic theorem itself (ε≈0.05, Mountain) and (2) the institutional choice to apply it to non-ergodic systems (ε≈0.52, Tangled Rope). The measurement interval tracks the increasing institutional enforcement of the misapplication despite accumulating evidence of non-ergodicity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ergodic_theorems, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
