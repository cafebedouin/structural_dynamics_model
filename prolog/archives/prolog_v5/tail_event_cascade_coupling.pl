% ============================================================================
% CONSTRAINT STORY: tail_event_cascade_coupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tail_event_cascade_coupling, []).

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
 *   constraint_id: tail_event_cascade_coupling
 *   human_readable: Tail Event Cascade Coupling: Systemic Risk Hidden in Correlation Assumptions
 *   domain: financial_systems/risk_modeling/systemic_collapse
 *
 * SUMMARY:
 *   Tail event cascade coupling is a structural constraint in modern
 *   financial systems where the correlation assumptions embedded in standard
 *   risk models break down precisely when most needed — during systemic
 *   crises. When volatility spikes, previously low correlations between asset
 *   classes collapse toward 1.0 as common causes (liquidity evaporation,
 *   margin calls, credit spread widening) trigger forced selling across all
 *   positions simultaneously. This eliminates the diversification benefit
 *   that portfolios rely on for risk management. The constraint operates as a
 *   snare because: (1) the coupling is suppressed from public awareness
 *   through model vendor disclaimers and regulatory theater, (2) no
 *   individual actor can profitably exit or disclose the risk without
 *   triggering the cascade itself, and (3) the beneficiaries (model vendors,
 *   systemically important banks with hedging capacity) have incentives to
 *   maintain the fiction that diversification works as advertised. The
 *   extractiveness measurement shows increasing severity (0.35 to 0.68 over a
 *   20-year interval) as financial leverage increased, interconnectedness
 *   deepened, and tail events became more correlated. Theater ratio increased
 *   moderately (0.40 to 0.55) reflecting growing gap between what regulatory
 *   stress tests claim to measure and what actually happens in market
 *   dislocations.
 *
 * KEY AGENTS:
 *   - Risk Model Vendors: Primary beneficiaries (institutional/arbitrage) — sell models that earn subscription fees and consulting revenue; protected from liability by disclaimers; benefit from assumption that models are adequate
 *   - Systemically Important Banks: Secondary beneficiaries (institutional/constrained) — can afford sophisticated hedging; can exit trades faster than retail investors; extract through information asymmetry about true tail coupling
 *   - Retail Investors: Primary victims (powerless/trapped) — hold diversified portfolios based on false correlation assumptions; cannot detect coupling from public data; bear losses when correlations collapse; cannot exit before cascade hits
 *   - Pension Funds: Secondary victims (moderate/constrained) — manage long-term liabilities that assume tail risk is lower than true coupling would suggest; constrained from hedging by cost drag; suppressed from disclosing liability underestimation
 *   - Systemic Financial Stability: Tertiary victim (powerless/trapped) — the abstract institutional framework that sustains credit flows; contaminated by hidden tail coupling; no mechanism for collective self-correction
 *   - Regulatory Framework: Institutional performer (institutional/arbitrage) — maintains theater of stress testing and capital requirements; knows models are incomplete; cannot admit this without triggering panic; persists through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tail_event_cascade_coupling, 0.68).
domain_priors:suppression_score(tail_event_cascade_coupling, 0.72).
domain_priors:theater_ratio(tail_event_cascade_coupling, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tail_event_cascade_coupling, extractiveness, 0.68).
narrative_ontology:constraint_metric(tail_event_cascade_coupling, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tail_event_cascade_coupling, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tail_event_cascade_coupling, snare).
narrative_ontology:human_readable(tail_event_cascade_coupling, "Tail Event Cascade Coupling: Systemic Risk Hidden in Correlation Assumptions").
narrative_ontology:topic_domain(tail_event_cascade_coupling, "financial_systems/risk_modeling/systemic_collapse").

domain_priors:requires_active_enforcement(tail_event_cascade_coupling).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tail_event_cascade_coupling, risk_model_vendors).
narrative_ontology:constraint_beneficiary(tail_event_cascade_coupling, financial_institutions_with_hedging_capacity).
narrative_ontology:constraint_victim(tail_event_cascade_coupling, retail_investors).
narrative_ontology:constraint_victim(tail_event_cascade_coupling, pension_funds).
narrative_ontology:constraint_victim(tail_event_cascade_coupling, systemic_financial_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Holds diversified portfolio based on correlation assumptions embedded in standard risk models. During tail events, correlations collapse to 1.0, eliminating diversification benefit. Cannot exit before the cascade hits; bears full loss during systemic downturn. No way to detect the coupling risk from available public data.
constraint_indexing:constraint_classification(tail_event_cascade_coupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PENSION FUND MANAGER (SNARE) — Manages long-term liabilities (30+ years) based on volatility estimates that assume tail event correlation holds. When coupling occurs, losses cascade across asset classes simultaneously. Can attempt hedging but cannot fully escape — hedging costs exhaust returns during normal years. Suppressed from acknowledging the risk because doing so would require admitting liability estimates are understated.
constraint_indexing:constraint_classification(tail_event_cascade_coupling, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: RISK MODEL VENDOR (ROPE) — Sells variance-covariance models, Value-at-Risk algorithms, and correlation matrices to financial institutions. The coupling constraint is a coordination mechanism: it coordinates expectations about tail risk (however incorrectly) and enables pricing consistency across the financial system. Vendor benefits from the ongoing subscription model and cannot be held liable for tail events (models include disclaimers). Experiences coordination, not extraction.
constraint_indexing:constraint_classification(tail_event_cascade_coupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SYSTEMICALLY IMPORTANT BANK (TANGLED ROPE) — Genuinely coordinates systemic credit flows and settlement infrastructure; also extracts through asymmetric exposure management. The bank has sophisticated understanding of tail coupling but cannot disclose it without triggering panic. Suppression operates through regulatory compliance and reputation management. The constraint has both coordination function (enabling credit markets to operate) and asymmetric extraction (the bank's exposure management advantage).
constraint_indexing:constraint_classification(tail_event_cascade_coupling, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Stress testing, capital requirements, and risk disclosure rules are theater: they assume tail correlations remain below 1.0 and that diversification provides the promised risk reduction. Regulators know the models are incomplete but maintain them because alternatives (individual bank-by-bank assessment, subjective reserves) require unacceptable regulatory discretion. The framework persists through institutional inertia despite known limitations. Theater ratio (0.55) reflects that the regulatory process has real functions but performance is hollow — the rules test for scenarios that don't capture the true coupling mechanism.
constraint_indexing:constraint_classification(tail_event_cascade_coupling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational view, the coupling is a real structural feature of interconnected financial systems: correlations DO collapse to 1.0 in tail events because common causes (liquidity squeeze, credit spread widening, margin calls) trigger forced selling across all asset classes simultaneously. This is not theater — it's a genuine structural fact. But the analytical observer sees that the constraint operates as a snare because the suppression (nobody can profitably disclose or price the true risk) means the coupling remains hidden until cascade occurs.
constraint_indexing:constraint_classification(tail_event_cascade_coupling, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tail_event_cascade_coupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tail_event_cascade_coupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tail_event_cascade_coupling, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tail_event_cascade_coupling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tail_event_cascade_coupling, TR),
    TR >= 0.70.

:- end_tests(tail_event_cascade_coupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The coupling extracts from retail and pension investors through hidden tail risk that standard models fail to price. When correlations collapse, losses compound across all asset classes, eliminating diversification protection. The extraction is not externally coercive but structural: it operates through the financial system's own mechanics once tail coupling occurs. Measurement shows increase from 0.35 (20 years ago, lower leverage and interconnection) to 0.68 (today, higher leverage and tighter coupling across global markets). Suppression (0.72): Very high. Multiple barriers prevent disclosure of true tail coupling risk: (1) vendor liability concerns, (2) regulatory framework that assumes models are adequate, (3) institutional reluctance to admit leverage and exposure calculations are understated, (4) market design that punishes early disclosure (liquidation runs), (5) epistemological issue that sophisticated tail-risk hedging requires admitting ordinary diversification doesn't work. Theater ratio (0.55): Moderate. Regulatory stress tests and capital requirements have real functions (they do identify some forms of systemic risk and require buffer capital) but are theater regarding tail coupling because they assume correlations during extreme stress remain below what actually occurs. The theater has increased moderately as the gap between model assumptions and market behavior widened.
 *
 * PERSPECTIVAL GAP:
 *   The vendor sees rope (coordination mechanism enabling consistent risk pricing across the financial system). The bank sees tangled rope (coordinates credit while extracting through hedging advantage). The regulatory framework sees piton (maintains theater for stability). The retail investor and pension fund see snare (trapped by false diversification assumption). The analytical observer from civilizational view sees snare (real structural feature — tail coupling IS real, and the suppression mechanism ensures losses are maximized when coupling occurs). The perspectival gap reveals that the coupling constraint operates as a redistribution mechanism: wealth flows from powerless agents (retail, pensions) who cannot hedge tail risk to institutional agents (banks, model vendors) who can. This redistribution is not coordinated extraction through explicit policy but emerges from the structure of asymmetric knowledge and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation places powerless retail investors at d=0.95 (full targets) because they lack exit capacity and hold victim status in the coupling mechanism. Pension funds are moderate/constrained at d=0.65 (victims with some capacity to manage but constrained by liability structure and regulatory requirements). Model vendors are institutional/arbitrage at d=0.15 (beneficiaries with full exit capacity — they can change models or disclose limitations at any time). Banks are institutional/constrained at d=0.40-0.50 (they benefit from the status quo but face regulatory constraints and systemic contagion risk if coupling becomes widely known). The beneficiary/victim declarations map directly: retail and pensions are victims because they bear losses during tail events; vendors and banks are beneficiaries because they profit from the coupling's suppression. The f(d) sigmoid maps these into effective extraction values that show asymmetry: what the vendor experiences as low extraction (they profit from stable arrangements) the retail investor experiences as very high extraction (they bear tail losses).
 *
 * MANDATROPHY ANALYSIS:
 *   The snare classification avoids the mandatrophy by correctly identifying that this constraint has minimal genuine coordination function — risk models do provide some signal about ordinary volatility (which helps coordination), but the primary function is to organize extraction by hiding tail coupling from the victims. The tangled rope perspective (for banks) correctly includes both coordination (systemic credit) and extraction (tail hedging advantage), which requires active enforcement (regulatory capital rules) to maintain the bank's privileged position. The piton perspective (for regulatory framework) correctly identifies performative activity: stress tests maintain the fiction that models capture tail risk while knowing they don't. The snare perspectives (retail, pension, analytical) correctly identify extraction with minimal coordination benefit — diversification supposedly coordinates risk across portfolios but fails at the moment of need. The constraint is not naturally a rope (pure coordination) because it has no beneficiary who is NOT extracting value from suppression. Even the banks extract through information asymmetry, not through coordination benefit. This rules out rope classification and confirms snare as the dominant type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_correlation_mechanism,
    'Is tail correlation collapse driven by market microstructure (liquidity evaporation, forced selling) or by genuine economic commonality across asset classes?',
    'Causal analysis of cascade events: timeline of liquidations, margin calls, credit spread widening; comparison of exogenous shocks (rate changes, geopolitical events) to endogenous contagion (forced selling) in each cascade',
    'If microstructure-driven: correlation collapse is partially avoidable through market design changes (circuit breakers, liquidity requirements). If commonality-driven: correlations are structural and unavoidable; only containment is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_correlation_mechanism, empirical, 'Mechanism driving tail correlation collapse').

omega_variable(
    model_vendor_knowledge,
    'Do major risk model vendors know that their models underestimate tail correlation and suppress this knowledge, or do they genuinely believe the models are adequate?',
    'Internal documentation review; comparison of models sold to institutional clients vs models used for vendor''s own risk management; interview evidence of what assumptions vendors made about tail coupling',
    'If vendors know and suppress: the coupling is intentional snare maintained through asymmetric information. If vendors genuinely believe: the coupling is a coordination failure (Rope) with unintended extraction consequences. Classification shifts from Snare to Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(model_vendor_knowledge, empirical, 'Vendor knowledge of model limitations').

omega_variable(
    regulatory_constraint_binding,
    'Can financial institutions actually implement more sophisticated tail-risk management within current regulatory constraints, or do regulations actively prevent prudent hedging for tail coupling?',
    'Regulatory compliance analysis: capital charge for tail-hedging positions, liquidity requirements, Basel III constraints on derivative usage; comparison of institutional risk limits to actual tail hedges in use during stable periods',
    'If regulations prevent hedging: suppression is structural (regulatory enforcement). If hedging is permitted but not used: suppression is epistemic (vendors don''t offer products, institutions don''t demand them). Different resolution pathways.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_constraint_binding, empirical, 'Whether regulations enable or prevent tail-risk hedging').

omega_variable(
    measurement_basis_circularity,
    'Does tail event cascade coupling exist as a structural property or only as a measurement artifact of variance-covariance models that assume normality?',
    'Alternative measurement frameworks: copula models, extreme value theory, realized correlation in actual cascade events; test whether non-normal models show persistent tail coupling or whether coupling is an artifact of normal-distribution assumption',
    'If structural: coupling is real and snare classification holds. If artifact: the constraint is really a Piton (degraded risk model persisting through inertia). This is the ε-invariance test — if changing measurement basis changes whether coupling exists, decompose into two stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_basis_circularity, empirical, 'Whether tail coupling is structural or measurement artifact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tail_event_cascade_coupling, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tail_tr_t0, tail_event_cascade_coupling, theater_ratio, 0, 0.4).
narrative_ontology:measurement(tail_tr_t10, tail_event_cascade_coupling, theater_ratio, 10, 0.48).
narrative_ontology:measurement(tail_tr_t20, tail_event_cascade_coupling, theater_ratio, 20, 0.55).
narrative_ontology:measurement(tail_tr_t5, tail_event_cascade_coupling, theater_ratio, 5, 0.43).

% Extraction over time
narrative_ontology:measurement(tail_be_t0, tail_event_cascade_coupling, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tail_be_t10, tail_event_cascade_coupling, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(tail_be_t20, tail_event_cascade_coupling, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(tail_be_t5, tail_event_cascade_coupling, base_extractiveness, 5, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tail_event_cascade_coupling, information_standard).
narrative_ontology:affects_constraint(tail_event_cascade_coupling, portfolio_margin_mechanics).
narrative_ontology:affects_constraint(tail_event_cascade_coupling, liquidity_provision_procyclicality).
narrative_ontology:affects_constraint(tail_event_cascade_coupling, credit_default_interconnection).

% DUAL FORMULATION NOTE:
% Tail coupling has two structurally distinct manifestations: (1) mathematical/statistical feature of extreme-value distributions (very high ε, near-mountain properties — tail events have real structural commonality), and (2) market mechanism feature of forced liquidations and margin calls (behavioral/economic ε, snare properties — the coupling is amplified by institutional constraints). These could be decomposed into separate stories: tail_event_correlation_structure (mathematical mountain) and tail_event_cascade_forced_selling (institutional snare). This story treats them as a single constraint because the suppression mechanism operates on the economic consequence, not the mathematical fact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tail_event_cascade_coupling, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
