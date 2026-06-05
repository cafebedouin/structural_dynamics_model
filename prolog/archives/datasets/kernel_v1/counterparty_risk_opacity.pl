% ============================================================================
% CONSTRAINT STORY: counterparty_risk_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_counterparty_risk_opacity, []).

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
 *   constraint_id: counterparty_risk_opacity
 *   human_readable: Counterparty Risk Opacity in Financial Markets
 *   domain: finance/systemic_risk
 *
 * SUMMARY:
 *   Counterparty risk opacity in financial markets is a foundational
 *   constraint that enables decentralized credit systems to function while
 *   creating systemic contagion pathways. The constraint operates at multiple
 *   scales: bilateral institution-to-institution relationships, systemwide
 *   interconnection networks, and the macroeconomic level where cascading
 *   defaults propagate. The opacity itself is not accidental — it is
 *   structurally maintained through the complexity of off-balance-sheet
 *   derivatives, the cost of comprehensive verification, and most
 *   importantly, the institutional incentive structure that rewards
 *   participants who exploit information asymmetry. The 2008 financial crisis
 *   revealed that institutions had built entire profit centers on
 *   counterparty opacity while simultaneously being shielded from worst-case
 *   outcomes through implicit government backstops. The post-2008 regulatory
 *   response (stress testing, capital requirements, central counterparty
 *   clearing mandates) has reduced opacity in some segments (standardized
 *   derivatives) while opacity persists in bespoke instruments, lending
 *   relationships, and interconnection measurement. The constraint exhibits a
 *   classic Tangled Rope signature: genuine coordination function
 *   (decentralized credit markets require some opacity to avoid prohibitive
 *   verification overhead), combined with substantial asymmetric extraction
 *   (large institutions profit from opacity while transferring tail risk to
 *   system and smaller counterparties), and active enforcement (internal risk
 *   systems, counterparty agreements, regulatory capital buffers all maintain
 *   the opacity regime). The theater ratio (0.65) reflects that regulatory
 *   surveillance (stress tests, CDS monitoring, interconnection mapping) is
 *   substantially performative — regulators measure opacity retrospectively
 *   but cannot prevent cascade once opacity-induced mispricing becomes
 *   manifest.
 *
 * KEY AGENTS:
 *   - Systemically Important Financial Institutions (SIFIs): Primary beneficiaries (institutional/arbitrage) — capture opacity-driven pricing advantages, implicit government backstop, counterparty risk mispricing arbitrage. Net recipients of effective extraction flow.
 *   - Financial System Stability Collective: Primary victim (powerless/trapped) — abstract collective of depositors, counterparties, and economy. Bears contagion cost and tail risk. Cannot organize or exit.
 *   - Collateral-Strained Counterparties: Secondary victim (moderate/constrained) — mid-size institutions face binding margin calls and repo haircuts during opacity cascades; cannot fully exit due to market dependency.
 *   - Derivatives Trading Desks: Mixed position (powerful/mobile) — derive profits from opacity-enabled arbitrage but also face regulatory constraints and execution pressure. Active participants in maintaining constraint through counterparty agreements and position management.
 *   - Regulatory Surveillance Apparatus: Institutional actor (institutional/arbitrage) — maintains performative oversight (stress tests, reporting) while constraint persists; benefits from opacity through reduced supervision burden (unmonitored tail risk avoids political pressure).
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing opacity as inherent to decentralized credit, missing institutional choice dimension.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(counterparty_risk_opacity, 0.58).
domain_priors:suppression_score(counterparty_risk_opacity, 0.62).
domain_priors:theater_ratio(counterparty_risk_opacity, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(counterparty_risk_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(counterparty_risk_opacity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(counterparty_risk_opacity, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(counterparty_risk_opacity, tangled_rope).
narrative_ontology:human_readable(counterparty_risk_opacity, "Counterparty Risk Opacity in Financial Markets").
narrative_ontology:topic_domain(counterparty_risk_opacity, "finance/systemic_risk").

domain_priors:requires_active_enforcement(counterparty_risk_opacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(counterparty_risk_opacity, large_financial_institutions).
narrative_ontology:constraint_beneficiary(counterparty_risk_opacity, derivatives_market_participants).
narrative_ontology:constraint_victim(counterparty_risk_opacity, financial_system_stability).
narrative_ontology:constraint_victim(counterparty_risk_opacity, counterparty_verification_capacity).
narrative_ontology:constraint_victim(counterparty_risk_opacity, collateral_strained_entities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYSTEM STABILITY (SNARE) — Cannot exit the opacity regime; bears full contagion cost during crises. The abstract collective of depositors, counterparties, and systemic participants has no ability to withdraw from interconnected markets or organize collective verification. Experiences maximum extraction through tail-risk concentration without compensation or agency.
constraint_indexing:constraint_classification(counterparty_risk_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COLLATERAL-STRAINED COUNTERPARTY (SNARE) — Mid-size institutions face binding collateral requirements and mark-to-market losses during opacity cascades. Cannot fully exit due to market dependency and funding needs, but bears disproportionate losses relative to access to opacity. Faces both direct extraction (margin calls, repo haircuts) and indirect extraction (contagion absorption).
constraint_indexing:constraint_classification(counterparty_risk_opacity, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SIFI (ROPE) — Large institutions experience opacity as pure coordination mechanism: information asymmetry enables price discrimination, opacity-based collateral advantage, and implicit government backstop. Net beneficiary. Sees the constraint as solving the coordination problem of decentralized credit — opacity is the price of avoiding centralized clearing. Effective extraction runs toward this actor.
constraint_indexing:constraint_classification(counterparty_risk_opacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DERIVATIVES TRADING DESK (TANGLED ROPE) — Derives substantial profits from opacity-enabled pricing arbitrage and counterparty risk mispricing, but also faces execution constraints and regulatory pressure. Has agency to adjust exposure but operates within market structure constraints. Benefits from coordination function (decentralized credit market) AND from extraction (counterparty risk mispricing). Active enforcement through internal controls and counterparty agreements.
constraint_indexing:constraint_classification(counterparty_risk_opacity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY SURVEILLANCE (PITON) — Stress testing and capital requirements simulate counterparty risk verification without enabling true real-time transparency. Regulatory frameworks measure opacity as 'concentration risk' and 'CDS basis widening' but cannot reverse the structural information asymmetry. Theater ratio high because models predict systemic failure but cannot prevent it once opacity cascade begins. Maintains performative oversight (quarterly reporting, stress tests) while the constraint persists largely unchanged.
constraint_indexing:constraint_classification(counterparty_risk_opacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, information asymmetry in credit markets is a fundamental property of decentralized finance: verification of complex derivatives positions is inherently costly, and some opacity is inevitable in systems that do not require centralized clearing. However, this perspective risks naturalizing what is an institutional choice (decentralized over centralized clearing), and the presence of beneficiaries suggests false summit dynamics.
constraint_indexing:constraint_classification(counterparty_risk_opacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(counterparty_risk_opacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(counterparty_risk_opacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(counterparty_risk_opacity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(counterparty_risk_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(counterparty_risk_opacity, TR),
    TR >= 0.70.

:- end_tests(counterparty_risk_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from counterparties and system stability through multiple mechanisms: (1) opacity-enabled mispricing of counterparty risk (derivatives traders price credit risk below true levels because counterparty solvency is unmeasured), (2) collateral arbitrage (large institutions access cheap funding because counterparties cannot accurately assess their leverage), (3) implicit government backstop (large institutions extract value from public sector rescue insurance that smaller counterparties do not receive), (4) contagion transfer (opacity enables cascade where concentrated institutions shed risk onto fragmented counterparties). However, extractiveness is not maximal (not >0.70) because some genuine coordination value exists — decentralized credit markets do solve the problem of funding allocation without requiring centralized clearing for every position. Suppression (0.62): High. Multiple barriers prevent accurate counterparty risk measurement: (1) technical complexity of derivative positions (off-balance-sheet exposures, model dependencies, nonlinear sensitivities), (2) institutional barriers (confidentiality agreements, competitive secrecy, information asymmetry incentivized by profit-taking), (3) regulatory gaps (regulators can measure some systemic risks but cannot enforce real-time transparency), (4) market structure (fragmented bilateral relationships prevent coordination on information sharing). Theater ratio (0.65): Moderate-high. Post-2008 regulatory apparatus (stress testing, capital requirements, CDS monitoring) is substantially performative. Stress tests predict crisis modes but cannot prevent them once opacity-induced mispricing manifest. Regulators measure interconnection retrospectively through CDS spreads and credit event data, not prospectively. Capital requirements set to levels that assume counterparty risk is correctly priced — but opacity means the pricing assumption is violated precisely when constraint matters most.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates classification divergence driven by agent structural position and exit capacity. The SIFI sees pure coordination (Rope) — opacity enables the decentralized credit function they depend on. The collateral-strained counterparty sees extraction (Snare or Tangled Rope) — they face binding constraints precisely because opacity prevents accurate risk measurement, yet they have constrained exit options. The derivatives desk sees mixed extraction and coordination (Tangled Rope) — they profit from mispricing but operate within regulatory and execution constraints. The regulatory apparatus sees degraded oversight (Piton) — stress tests and reporting create appearance of monitoring without enabling prevention. The system-level observer sees extraction (Snare) — the collective contagion cost vastly exceeds coordination benefits. The civilizational analytical observer risks seeing necessity (Mountain) — opacity seems inherent to decentralized credit — but this naturalizes an institutional choice (decentralized over centralized clearing).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position: (1) SIFIs are beneficiaries with arbitrage-level exit options (can switch counterparties, access multiple funding sources, shape regulatory environment). Their d is low (~0.15), producing negative chi — they experience opacity as net benefit. (2) Collateral-strained counterparties are victims with constrained exit (depend on funding relationships, face regulatory and operational barriers to leaving). Their d is high (~0.80), producing high chi — they experience opacity as extraction. (3) System stability collective is victim with trapped exit (cannot exit interconnected system). Their d is very high (~0.95), producing maximum chi — they bear full contagion cost. (4) Derivatives desks are beneficiaries with mobile-to-arbitrage exit (can adjust positions, reallocate capital). Their d is moderate (~0.45), producing moderate chi — they profit but within constraints. (5) Regulatory apparatus sees opacity as beneficiary through reduced supervision burden, with arbitrage-level exit (can adjust surveillance focus). Their d is low (~0.20), producing negative chi — they benefit from not having to police opacity. (6) Analytical observer's d is computed from observer position (~0.72), revealing the natural law perspective as downstream of SIFI framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through differentiated perspectives rather than type uncertainty. All institutional perspectives agree the constraint involves both coordination (enabling decentralized credit) and extraction (beneficiary profit from information asymmetry). The question is not 'is this really extraction or coordination?' but 'whose extraction and whose coordination?' The SIFI sees coordination (benefits from opacity). The system sees extraction (bears contagion). The collateral-strained counterparty sees both — coordination benefits from market access, extraction costs from margin calls. The mandatrophy dissolves when the analytical framework captures that the same structural feature (opacity) enables coordination for beneficiaries and extraction for victims. This is the essential feature of Tangled Rope: real coordination function + asymmetric extraction + active enforcement to maintain both simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_cost_threshold,
    'At what point do real-time counterparty verification costs exceed the coordination benefits of decentralized credit markets?',
    'Cost-benefit analysis of centralized clearing (fixed verification overhead) versus decentralized (variable verification costs scaled with interconnection density); historical data from central counterparty adoption periods (post-2008 regulatory changes)',
    'If threshold exceeded: constraint shifts from coordination (Rope/Tangled Rope) to pure extraction (Snare) — opacity is maintained despite technical solvability. If not exceeded: opacity remains a genuine tradeoff between coordination and verification burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_threshold, empirical, 'Threshold at which verification costs exceed decentralized credit benefits').

omega_variable(
    counterparty_information_asymmetry_mechanism,
    'Is counterparty opacity maintained by technical barriers (complexity of off-balance-sheet exposures) or by institutional incentive structures (opacity-driven profit extraction)?',
    'Cross-institutional comparison: firms with highest opacity levels versus those with greater transparency; correlation between opacity reduction and profit margin compression; analysis of competitive advantage from opacity.',
    'If technical: constraint is genuine coordination tradeoff (higher ε for legitimacy, lower suppression). If institutional: constraint is extractive mechanism disguised as necessity (lower ε, higher suppression, likely Snare rather than Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counterparty_information_asymmetry_mechanism, empirical, 'Whether opacity is technical necessity or institutional extraction mechanism').

omega_variable(
    implicit_backstop_extraction,
    'To what degree does counterparty opacity profit-taking depend on implicit government guarantees (too-big-to-fail backstop)?',
    'CDS spread analysis during systemic stress events; comparison of pricing during periods of explicit vs implicit backstop clarity; historical comparison pre/post-2008 regulatory clarity on rescue criteria.',
    'If high dependence: opacity is systematized rent extraction subsidized by public sector. Reclassify as Snare with public sector as hidden beneficiary. If low dependence: constraint reflects genuine market coordination tradeoff.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_backstop_extraction, empirical, 'Dependence of opacity-enabled extraction on implicit government backstop').

omega_variable(
    central_clearing_adoption_dynamics,
    'Why do institutions resist mandatory central counterparty clearing (CCP) adoption for opaque derivative positions despite post-2008 regulatory pressure?',
    'Regulatory timeline analysis: mandated CCP adoption rates versus voluntary adoption; cost-benefit calculations presented by industry in regulatory comment periods; correlation between resistance intensity and opacity-profit dependency.',
    'If resistance is rent protection: constraint is institutional extraction (supports Snare classification). If resistance is execution/cost concerns: constraint is genuine coordination tradeoff (supports Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(central_clearing_adoption_dynamics, empirical, 'Resistance to CCP adoption as indicator of extraction versus tradeoff').

omega_variable(
    cascade_asymmetry_measurement,
    'During opacity-induced cascade events (2008 credit crisis, March 2020 volatility spike), does contagion impact correlate with institutional size/power or with exposure level alone?',
    'Empirical analysis of crisis propagation: did large but highly-exposed institutions face greater pressure than smaller well-capitalized institutions? Did crisis asymmetry protect larger players through implicit backstop?',
    'If contagion asymmetric to size: indicates suppression mechanism that protects institutional beneficiaries from worst outcomes (supports Snare or Tangled Rope with protected beneficiary). If contagion asymmetric to exposure: constraint operates more equitably, supporting Tangled Rope over Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cascade_asymmetry_measurement, empirical, 'Whether cascade harm correlates with institutional size or actual exposure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(counterparty_risk_opacity, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cpo_theater_t0, counterparty_risk_opacity, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cpo_theater_t8, counterparty_risk_opacity, theater_ratio, 8, 0.6).
narrative_ontology:measurement(cpo_theater_t16, counterparty_risk_opacity, theater_ratio, 16, 0.65).

% Extraction over time
narrative_ontology:measurement(cpo_extractiveness_t0, counterparty_risk_opacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cpo_extractiveness_t8, counterparty_risk_opacity, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(cpo_extractiveness_t16, counterparty_risk_opacity, base_extractiveness, 16, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cpo_suppression_t0, counterparty_risk_opacity, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(cpo_suppression_t8, counterparty_risk_opacity, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(cpo_suppression_t16, counterparty_risk_opacity, suppression_requirement, 16, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(counterparty_risk_opacity, resource_allocation).
narrative_ontology:affects_constraint(counterparty_risk_opacity, credit_default_swap_basis_mispricing).
narrative_ontology:affects_constraint(counterparty_risk_opacity, margin_amplification_cascade).
narrative_ontology:affects_constraint(counterparty_risk_opacity, implicit_government_backstop_pricing).

% DUAL FORMULATION NOTE:
% Counterparty risk opacity is upstream of specific pricing mechanisms (CDS basis mispricing, margin amplification) and institutional dependencies (implicit backstop pricing). Each downstream constraint has its own ε and structural data but inherits the opacity regime as a causal precondition. Network links enable contamination propagation analysis: if opacity constraints degrade (due to regulatory mandate or competitive pressure), downstream constraints shift in predictable ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(counterparty_risk_opacity, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
