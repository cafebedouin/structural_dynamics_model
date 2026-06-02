% ============================================================================
% CONSTRAINT STORY: shadow_banking_leverage_amplification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shadow_banking_leverage_amplification, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: shadow_banking_leverage_amplification
 *   human_readable: Shadow Banking Leverage Amplification Trap
 *   domain: financial_systemic_risk
 *
 * SUMMARY:
 *   Shadow banking leverage amplification creates a globally coupled
 *   extraction mechanism where financial intermediaries benefit from
 *   recursive debt structures while shifting tail risk to trapped retail
 *   investors and constrained emerging market policymakers. The constraint
 *   exhibits the full range of DR classifications: it appears as pure
 *   extraction (snare) to powerless participants, as coordination (rope) to
 *   institutional beneficiaries, as a temporary regulatory problem (scaffold)
 *   to policymakers, as a degraded reserve system (piton) to traditional
 *   banking authorities, and as an immutable financial law (mountain) to
 *   civilizational analysts. The core mechanism: unregulated shadow banks
 *   provide margin credit to retail investors; collateral is rehypothecated
 *   across counterparties; leverage multiplies through repo and prime
 *   brokerage networks; volatility forces cascading margin calls; retail
 *   investors face liquidation spirals they cannot exit. Simultaneously,
 *   dollar liquidity absorbed by leverage in core markets drains reserves
 *   from emerging market central banks, forcing currency peg abandonment. The
 *   extractiveness has increased from 0.35 to 0.68 over the interval as
 *   regulatory arbitrage has outpaced regulatory closure and financial
 *   innovation has created new leverage pathways faster than regulators can
 *   formalize restrictions.
 *
 * KEY AGENTS:
 *   - Retail Investors: Primary victims (powerless/trapped) — face recursive leverage and margin call cascades with no exit
 *   - Shadow Banking Intermediaries: Primary beneficiaries (institutional/arbitrage) — capture credit spreads, term premiums, and liquidity fees; avoid tail risk through collateral chains
 *   - Emerging Market Central Banks: Secondary victims (moderate/constrained) — currency pegs force reserve accumulation in dollars; global leverage drains force peg abandonment under political cost
 *   - Systemically Important Banks: Dual position (powerful/mobile) — benefit from intermediation but face contagion risk; can reduce exposure but choose not to
 *   - Regulatory Coalition: Temporary governance (organized/constrained) — Basel III, Dodd-Frank attempt to cap leverage through capital and collateral rules; sunset intended but repeatedly deferred
 *   - Central Banking System: Degraded mechanism (institutional/arbitrage) — reserve requirements and deposit insurance historically contained leverage; now largely bypassed by shadow banking
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent regulatory arbitrage as inherent financial law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shadow_banking_leverage_amplification, 0.68).
domain_priors:suppression_score(shadow_banking_leverage_amplification, 0.75).
domain_priors:theater_ratio(shadow_banking_leverage_amplification, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shadow_banking_leverage_amplification, extractiveness, 0.68).
narrative_ontology:constraint_metric(shadow_banking_leverage_amplification, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(shadow_banking_leverage_amplification, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shadow_banking_leverage_amplification, snare).
narrative_ontology:human_readable(shadow_banking_leverage_amplification, "Shadow Banking Leverage Amplification Trap").
narrative_ontology:topic_domain(shadow_banking_leverage_amplification, "financial_systemic_risk").

domain_priors:requires_active_enforcement(shadow_banking_leverage_amplification).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shadow_banking_leverage_amplification, financial_intermediaries).
narrative_ontology:constraint_beneficiary(shadow_banking_leverage_amplification, institutional_arbitrageurs).
narrative_ontology:constraint_victim(shadow_banking_leverage_amplification, retail_investors).
narrative_ontology:constraint_victim(shadow_banking_leverage_amplification, systemic_stability).
narrative_ontology:constraint_victim(shadow_banking_leverage_amplification, emerging_market_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Trapped in recursive leverage: brokers offer margin credit; retail investors borrow against volatile assets; volatility forces liquidations; cascading margin calls eliminate exit. The trap requires no active enforcement — structural incentive alignment completes it. Investor perceives the constraint as unchangeable at biographical horizon; they cannot escape the leverage loop without catastrophic loss.
constraint_indexing:constraint_classification(shadow_banking_leverage_amplification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING MARKET CURRENCY PEG (SNARE) — Pegged currencies require domestic central banks to maintain foreign reserves via dollar debt. When shadow banking leverage inflates globally, dollar liquidity drains into carry trades; pegs collapse under redemption pressure. Emerging market policymakers are structurally constrained (not trapped) because they can abandon the peg, but the costs of devaluation are politically catastrophic. Generational horizon shows this as a snare: each policy generation inherits the legacy peg and repeats the cycle.
constraint_indexing:constraint_classification(shadow_banking_leverage_amplification, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: FINANCIAL INTERMEDIARY (ROPE) — Experiences the constraint as pure coordination: leverage amplification solves the lending problem (how to deploy capital efficiently across counterparties). Intermediaries capture term premiums and credit spreads without directly bearing the tail risk. Exit option is arbitrage: they can adjust leverage, hedge counterparty exposure, or exit positions. Perceived as coordination mechanism from their vantage point.
constraint_indexing:constraint_classification(shadow_banking_leverage_amplification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Basel III, Dodd-Frank, Volcker rule, and emerging regulatory architecture (capital buffers, leverage ratios, OTC clearing mandates) are temporary coordination frameworks with intended sunset: as transparency and bilateral collateral infrastructure mature, the regulatory scaffolding should become unnecessary. Current suppression (regulatory complexity) is intentionally declining as leverage reporting standardizes. However, the sunset is perpetually deferred because financial innovation continuously creates new leverage pathways.
constraint_indexing:constraint_classification(shadow_banking_leverage_amplification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SYSTEMICALLY IMPORTANT FI (TANGLED ROPE) — Large banks both benefit from and bear risk of the leverage amplification system. They extract through liquidity provision and maturity transformation (genuine coordination functions), but also face tail risk contagion and regulatory capital burdens (asymmetric costs). Mobile exit option: they can reduce leverage, divest shadow banking subsidiaries, or reposition into lower-risk activities. But the coordination function (intermediation) is valuable enough that they do not fully exit. At biographical horizon, they experience mixed benefit and extraction.
constraint_indexing:constraint_classification(shadow_banking_leverage_amplification, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL BANKING RESERVE SYSTEM (PITON) — Central bank reserve requirements and deposit insurance were designed to coordinate safe lending. But shadow banking has atrophied these mechanisms: reserves have migrated to unregulated intermediaries; deposit insurance no longer covers the majority of financial activity; the reserve system persists through regulatory mandate but has lost functional teeth. The theater ratio (0.58) reflects substantial performative regulation — stress tests, capital adequacy reviews, and reserve audits are conducted but do not prevent leverage accumulation in off-balance-sheet vehicles. Classical coordination mechanism degraded by institutional inertia.
constraint_indexing:constraint_classification(shadow_banking_leverage_amplification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical horizon, leverage amplification appears as an immutable consequence of financial markets: whenever capital is intermediated across time and counterparties, agents have incentives to borrow against collateral and rehypothecate. The constraint appears as a mathematical law of financial dynamics. However, the base extractiveness (0.68) contradicts the mountain classification — the analytical observer risks naturalizing a contingent institutional arrangement (permissive collateral regulations, regulatory arbitrage, information opacity) as an inherent feature of finance.
constraint_indexing:constraint_classification(shadow_banking_leverage_amplification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shadow_banking_leverage_amplification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shadow_banking_leverage_amplification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shadow_banking_leverage_amplification, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shadow_banking_leverage_amplification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(shadow_banking_leverage_amplification, TR),
    TR >= 0.70.

:- end_tests(shadow_banking_leverage_amplification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts significantly from trapped and constrained agents (retail investors, emerging markets) while delivering clear benefits to institutional intermediaries. The trajectory from 0.35 to 0.68 reflects regulatory arbitrage outpacing regulatory closure — each wave of reforms (Dodd-Frank 2010–2015, Basel III 2013–2019) is followed by innovation creating equivalent leverage pathways outside the regulatory perimeter. Suppression (0.75): High. Retail investors face compounded barriers: margin requirements are opaque, collateral chains are hidden, counterparty risk is not transparently disclosed, and forced liquidation occurs under market stress when exit is most costly. Emerging markets face sovereign-level suppression: reserve requirements for currency pegs force accumulation of depreciating dollar assets; peg abandonment carries political and economic costs. Theater ratio (0.58): Moderate. Substantial regulatory theater exists (stress tests, capital adequacy reviews, leverage ratio reporting) but does not prevent shadow leverage accumulation. However, the shadow system's core function (credit intermediation at lower cost than traditional banking) is genuine, so theater is not dominant. The measurement trajectory (0.42→0.58) reflects increasing regulatory response theater without corresponding deleveraging.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates divergent structural experiences from identical base metrics. The retail investor sees a snare (maximum experienced extraction, zero exit at margin call time). The financial intermediary sees rope (genuine coordination service, net positive benefit flow). The regulatory coalition sees a temporary coordination failure being solved (scaffold with sunset). The traditional banking system sees its own degraded reserves mechanism (piton with performative stress tests). The SIFI sees tangled rope (mixed benefit and risk). The civilizational analyst risks seeing immutable leverage law (mountain), but the data reveals this as false naturalizing: pre-1980s regulatory structures (Glass-Steagall, collateral segregation, mandated haircuts) suppressed equivalent leverage, showing the constraint is contingent on institutional choice, not financial law. The perspectival gap reveals how the same constraint generates legitimating narratives (coordination, market efficiency, natural limits) for beneficiaries while extracting from trapped participants.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from agent structural position within the leverage extraction flow. Retail investors are pure targets: they provide collateral, face margin calls, bear liquidation costs (d≈0.95). Financial intermediaries are beneficiaries: they collect spreads and fees without proportional tail risk exposure (d≈0.10). Emerging market central banks are partial targets: they are constrained to accumulate depreciating reserves by peg structure (d≈0.70). SIFIs are partial beneficiaries with moderate exposure: they benefit from intermediation but face regulatory capital charges and contagion (d≈0.45). Regulatory coalition members have constrained but not trapped exits: they can tighten rules but face regulatory arbitrage substitution (d≈0.55). The f(d) sigmoid converts these d values to effective extractiveness modifiers, producing the χ formula outcome. High d for retail investors magnifies base extractiveness to perceived chi; low d for intermediaries produces negative perceived extraction (they experience subsidization).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The mandatrophy is dissolved by showing that shadow banking leverage is fundamentally a snare with theatrical coordination narratives. The 'coordination' perspective (rope) is genuine in the narrow sense that intermediaries do solve a credit-allocation problem. But the asymmetric extraction — retail investors trapped in margin spirals, emerging markets forced into reserve depletion, systemic stability bearing tail risk — violates the balanced benefit assumption of pure coordination. The constraint is a snare (extractiveness 0.68, suppression 0.75) that narrates itself as rope (credit market efficiency, financial innovation) through the beneficiary perspective. The mandatrophy resolves when we distinguish the beneficiary's genuine experience (rope: they do coordinate lending) from the systemic extraction they cause (snare: trapped agents bear concentrated costs). The regulatory scaffold perspective is aspirational rather than structural — regulatory closure is repeatedly outpaced by arbitrage innovation, and the sunset is perpetually deferred. The analytical mountain is a false summit: leverage amplification appears as an immutable financial law only when institutional history (pre-1980s Glass-Steagall) is erased from the frame. The constrainthood persists because the extractive capture is durable, not because it is physically inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    leverage_ceiling_mechanism,
    'Is there a structural limit to leverage amplification, or does the cascade continue until forced deleveraging event?',
    'Historical analysis of financial cycles; identification of leverage thresholds that precede forced margin calls; comparison across regulatory regimes with different leverage caps',
    'If ceiling exists: leverage amplification classifies as rope with inherent stabilizer. If no ceiling: pure snare with only crisis as exit mechanism. Extractiveness would range 0.45–0.72 depending on answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leverage_ceiling_mechanism, empirical, 'Whether leverage has structural ceiling or cascade continues to crisis').

omega_variable(
    collateral_interconnection_opacity,
    'Do shadow banking participants (repo traders, prime brokers, asset managers) actually know the full chain of collateral rehypothecation and counterparty exposure?',
    'Audit of collateral tracking systems; interviews with risk managers on collateral visibility; analysis of unwind difficulty during liquidity events',
    'If opacity is high: suppression is structural (agents cannot assess risk). If transparency exists: suppression is policy-chosen (regulators permit non-disclosure). Changes classified extraction from structural snare to policy snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collateral_interconnection_opacity, empirical, 'Whether collateral chain opacity is structural or regulatory choice').

omega_variable(
    emerging_market_reserve_drain,
    'Does shadow banking leverage expansion in core markets directly cause emerging market currency pressure via dollar drain?',
    'Correlation analysis between US leverage cycles and EM reserve depletion; causal inference from carry trade positions; timeline alignment of liquidity tightening and peg pressure',
    'If direct causal: emerging market constraint is downstream snare with clear transmission. If coincidental/indirect: constraint may be independent policy failure. Changes victim group scope and causality model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emerging_market_reserve_drain, empirical, 'Whether shadow leverage directly causes emerging market currency pressure').

omega_variable(
    regulatory_arbitrage_substitution,
    'When one leverage pathway is closed (repo regulation, bank balance sheet requirements), do financial actors immediately find equivalent unregulated pathways, or is leverage genuinely reduced?',
    'Cross-border tracking of capital flows pre/post regulation; identification of substitute leverage mechanisms; measurement of total system leverage vs. regulated banking leverage',
    'If substitution is immediate: regulatory scaffold is piton (theater only), extractiveness unchanged. If substitution is slow: scaffold has real sunset, extractiveness does decline. Changes classification of regulatory perspective from piton to genuine scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_arbitrage_substitution, empirical, 'Whether regulatory closure triggers genuine deleveraging or arbitrage substitution').

omega_variable(
    mandatrophy_false_summit,
    'Is the analytical mountain perspective a false summit naturalizing contingent regulatory arrangements as inherent financial laws?',
    'Comparative institutional analysis: did pre-1980s regulatory regime (Glass-Steagall, collateral segregation, margin requirements) suppress leverage amplification? If yes, structure is contingent; mountain is false summit.',
    'If false summit confirmed: analytical perspective should reclassify to snare or tangled_rope depending on analytical agent''s power. Reveals how structural naturalizing enables continued extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_false_summit, conceptual, 'Whether analytical mountain is false summit or genuine natural law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shadow_banking_leverage_amplification, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sble_tr_t0, shadow_banking_leverage_amplification, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sble_tr_t5, shadow_banking_leverage_amplification, theater_ratio, 5, 0.5).
narrative_ontology:measurement(sble_tr_t10, shadow_banking_leverage_amplification, theater_ratio, 10, 0.58).
narrative_ontology:measurement(sble_tr_t15, shadow_banking_leverage_amplification, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(sble_be_t0, shadow_banking_leverage_amplification, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sble_be_t5, shadow_banking_leverage_amplification, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(sble_be_t10, shadow_banking_leverage_amplification, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(sble_be_t15, shadow_banking_leverage_amplification, base_extractiveness, 15, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shadow_banking_leverage_amplification, resource_allocation).
narrative_ontology:boltzmann_floor_override(shadow_banking_leverage_amplification, 0.12).
narrative_ontology:affects_constraint(shadow_banking_leverage_amplification, currency_peg_sustainability).
narrative_ontology:affects_constraint(shadow_banking_leverage_amplification, margin_call_cascade).
narrative_ontology:affects_constraint(shadow_banking_leverage_amplification, repo_market_maturity_transformation).

% DUAL FORMULATION NOTE:
% Shadow banking leverage amplification decomposes into three structurally distinct constraints: (1) margin_call_cascade (ε≈0.75, retail snare), (2) currency_peg_sustainability (ε≈0.55, emerging market tangled_rope), (3) repo_market_maturity_transformation (ε≈0.40, institutional rope). This story integrates the system-level extraction; upstream stories track domain-specific mechanisms. All three are downstream of regulatory_arbitrage_cycle (ε≈0.62, piton).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shadow_banking_leverage_amplification, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
