% ============================================================================
% CONSTRAINT STORY: algorithmic_front_running
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_front_running, []).

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
 *   constraint_id: algorithmic_front_running
 *   human_readable: Algorithmic Front Running in Financial Markets
 *   domain: financial_markets/technology
 *
 * SUMMARY:
 *   Algorithmic front running in electronic financial markets represents a
 *   structural constraint where high-frequency trading algorithms extract
 *   value from retail and institutional order flow by detecting, predicting,
 *   and positioning ahead of incoming trades microseconds before execution.
 *   The constraint exhibits hybrid character: front running provides genuine
 *   liquidity provision and price discovery coordination while simultaneously
 *   enabling extractive rent capture through information asymmetry
 *   exploitation. The extractiveness value (0.68) reflects that extraction
 *   has intensified over the 20-year interval as algorithmic sophistication
 *   and market fragmentation have increased detection and positioning
 *   accuracy. The suppression metric (0.72) captures both structural barriers
 *   (technical opacity of algorithmic decision-making, information asymmetry,
 *   regulatory latency, geographic fragmentation across venues) and the
 *   powerlessness of retail participants to organize collective defense. The
 *   theater ratio (0.58) indicates moderate performative content in
 *   regulatory compliance (audit trails, disclosure mandates, market-conduct
 *   rules) that enforce a fairness narrative while millisecond-scale
 *   extraction mechanisms remain largely undetected and unconstrained.
 *
 * KEY AGENTS:
 *   - Retail Investors: Primary victims (powerless/trapped) — must participate in electronic markets for savings/retirement; face systematic extraction they cannot detect or avoid
 *   - Institutional Order Flow: Secondary victims (moderate/constrained) — large pension funds and asset managers can partially exit through dark pools but face incomplete information barriers
 *   - High-Frequency Trading Firms: Primary beneficiaries (powerful/mobile) — extract rent through latency arbitrage and information leakage detection; genuinely provide liquidity coordination
 *   - Exchange Operators: Secondary beneficiaries (institutional/arbitrage) — capture network effects and fee revenue from high-volume algorithmic trading; have arbitrage options in rule design
 *   - Regulatory Frameworks: Institutional enforcer (institutional/constrained) — attempt fairness maintenance through performative compliance theater (audit trails, disclosure) with limited real-time detection capacity
 *   - Price Discovery Mechanism: Tertiary victim (powerless/trapped) — abstract collective good polluted by strategic order flow information; no organizing capacity or exit path
 *   - Analytical Observer: Civilizational context (analytical/analytical) — sees genuine hybrid structure where liquidity coordination and extractive rent capture are structurally entangled
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_front_running, 0.68).
domain_priors:suppression_score(algorithmic_front_running, 0.72).
domain_priors:theater_ratio(algorithmic_front_running, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_front_running, extractiveness, 0.68).
narrative_ontology:constraint_metric(algorithmic_front_running, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(algorithmic_front_running, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_front_running, snare).
narrative_ontology:human_readable(algorithmic_front_running, "Algorithmic Front Running in Financial Markets").
narrative_ontology:topic_domain(algorithmic_front_running, "financial_markets/technology").

domain_priors:requires_active_enforcement(algorithmic_front_running).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_front_running, high_frequency_traders).
narrative_ontology:constraint_beneficiary(algorithmic_front_running, exchange_operators).
narrative_ontology:constraint_victim(algorithmic_front_running, retail_investors).
narrative_ontology:constraint_victim(algorithmic_front_running, institutional_order_flow).
narrative_ontology:constraint_victim(algorithmic_front_running, market_price_discovery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Trapped in electronic markets with no realistic exit. Market participation is necessary for retirement savings, but algorithmic front running extracts value from every trade. Suppression is complete: retail traders cannot detect the extraction, cannot organize collective defense, and face information asymmetry that prevents exit timing optimization. Maximum experienced extraction.
constraint_indexing:constraint_classification(algorithmic_front_running, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL ORDER FLOW (SNARE) — Large asset managers can partially exit through dark pools and algorithmic execution, but this exit is expensive and incomplete. Information leakage to fast traders occurs through market microstructure (order splitting detection, queue position inference). The constraint extracts measurably from pension funds and index trackers. Suppression is high but not total — some institutional actors have developed counter-strategies.
constraint_indexing:constraint_classification(algorithmic_front_running, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: HIGH FREQUENCY TRADER (TANGLED ROPE) — Experiences genuine coordination function: market liquidity and price discovery mechanisms require rapid repricing and order fulfillment. Front running generates extraction asymmetrically while simultaneously providing real liquidity services. Able to exit or adjust strategies with millisecond timing. The constraint coordinates price discovery while enabling extractive rent capture.
constraint_indexing:constraint_classification(algorithmic_front_running, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: EXCHANGE OPERATOR (ROPE) — Captures network effects and liquidity aggregation benefits from high-frequency trading activity. Extraction toward the exchange is minimal; the exchange experiences the constraint as pure coordination gain. Benefits from higher trading volumes, greater price efficiency, and fee revenue. Has arbitrage options (regulatory compliance choices, market rules design).
constraint_indexing:constraint_classification(algorithmic_front_running, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Market regulations (Reg SHO, Regulation SCI, MiFID II) maintain performative enforcement of market fairness without structural constraint on millisecond-scale front running. Regulators cannot inspect algorithmic decision-making in real time. Compliance theater (audit trails, disclosure rules) persists while the core extraction mechanism remains untouched. Theater ratio elevated due to measurement latency and inspector knowledge gaps.
constraint_indexing:constraint_classification(algorithmic_front_running, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PRICE DISCOVERY (SNARE) — Abstract collective good. Information efficiency and fair price signals are polluted by strategic front running that separates order flow information from actual supply/demand fundamentals. The mechanism cannot organize, exit, or defend itself. Trapped under the extraction's weight across all asset classes globally. This is the epistemic commons perspective.
constraint_indexing:constraint_classification(algorithmic_front_running, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, front running coordinates liquidity provision (genuine function) while extracting through information asymmetry exploitation (measurable asymmetry). The constraint both enables and violates fair market price discovery. Neither pure coordination nor pure extraction — genuinely hybrid. Regulatory arbitrage and latency arms races are active enforcement mechanisms. The system's complexity makes detection and prevention technically difficult but not theoretically impossible.
constraint_indexing:constraint_classification(algorithmic_front_running, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_front_running_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_front_running, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_front_running, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_front_running, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_front_running, TR),
    TR >= 0.70.

:- end_tests(algorithmic_front_running_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, increasing over interval. Base extraction reflects asymmetric capture of value from order flow information leakage. The value increased from 0.42 to 0.68 over 20 years as market fragmentation, algorithmic sophistication, and colocation arms races amplified extraction mechanisms. Institutional measures (dark pools, algorithmic execution) have partially contained extraction for large actors but worsened it for retail participants. Suppression (0.72): High. Structural barriers include: (1) millisecond-scale detection latency exceeds human and regulatory inspection timescales; (2) algorithmic decision-making opacity prevents direct observation of causal chain; (3) information asymmetry makes it impossible for victims to know if they are being extracted from; (4) regulatory arbitrage across fragmented venues prevents unified detection; (5) retail organization is technically and economically infeasible. Theater ratio (0.58): Moderate. Regulatory theater includes SEC/FINRA enforcement (audit trails, order-routing rules), MiFID II transparency mandates, and best-execution requirements — performative measures that create fairness appearance without constraining millisecond-scale extraction. The theater has grown as regulatory awareness increased but detection capacity remained latency-bound.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates a stark perspectival divide. Retail investors and the price discovery mechanism perceive pure extraction (Snare) — they are trapped, unable to exit, bearing full extraction cost with no coordination benefit. Institutional traders perceive mixed extraction and benefit (Tangled Rope) — they benefit from the liquidity coordination that HFT provides while suffering information leakage extraction; they can partially exit through dark pools and algorithmic execution. High-frequency traders perceive genuine coordination (Tangled Rope or Rope, depending on emphasis) — they are solving the real problem of providing liquidity and repricing when order flow arrives; the extraction they capture appears to them as legitimate value capture for risk-bearing and rapid execution. Exchange operators perceive pure coordination (Rope) — they are the beneficiaries of network effects and fee capture without bearing extraction cost. Regulators perceive a partially controlled extraction (Piton) — they have constructed compliance frameworks that give the appearance of fairness maintenance while the extraction mechanism persists largely unchecked at timescales below regulatory detection. The analytical observer sees the true hybrid structure (Tangled Rope) — genuine liquidity provision is structurally entangled with extraction, and the two cannot be easily separated without redesigning market microstructure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position relative to extraction flow. Retail investors with no exit options (trapped) and victim status yield high d ≈ 0.95, producing maximum f(d) ≈ 1.42 and high χ. Institutional traders with constrained but available exits (dark pools) and mixed victim-beneficiary status yield moderate d ≈ 0.60, producing moderate f(d) ≈ 0.75 and moderate χ. High-frequency traders with mobile exits (algorithm adjustment, venue switching) and primary beneficiary status yield low d ≈ 0.25, producing low f(d) ≈ 0.18 and negative or near-zero χ. Exchange operators with arbitrage options (regulatory choice, fee structure design) and pure beneficiary status yield very low d ≈ 0.05, producing negative f(d) ≈ -0.12 and negative χ. The price discovery mechanism, as an abstract collective good with powerless status and trapped exit, yields d ≈ 0.95 and maximum extraction experience. Regulatory frameworks with constrained exit (political pressure, treaty obligations) and split victim-beneficiary status (capturing some fees while bearing fairness obligations) yield moderate d ≈ 0.50, producing moderate f(d) ≈ 0.65.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: Front running is demonstrably extractive (ε > 0.46, χ > 0.66, high suppression) yet simultaneously provides genuine coordination benefit (liquidity provision, price repricing). The mandatrophy is resolved by recognizing the constraint as Snare from powerless victims' perspectives (retail, price discovery) and Tangled Rope from powerful actors' perspectives (HFT firms, institutional traders). No single classification is 'correct' — the perspectival presheaf is the answer. The constraint demonstrates why mandatrophy exists: extraction and coordination are structurally entangled in market microstructure. Attempts to eliminate front running without destroying liquidity provision fail because the liquidity IS the extraction mechanism — same actors, same information asymmetry, same millisecond timing. Resolution requires either (1) accepting the extraction as the price of liquidity, (2) redesigning market structure to separate liquidity provision from information-advantage extraction (encrypted order books, latency parity), or (3) recognizing that 'coordination' is a cover story for extraction in this domain. The constraint is not a coordination failure waiting for solution — it is coordination-entangled extraction by design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latency_detection_threshold,
    'At what order-to-execution latency does front running transition from legitimate liquidity provision to predatory extraction?',
    'Empirical measurement: statistical comparison of fill prices for identical order profiles executed with vs without algorithmic latency advantages; correlation with market-wide price impact studies',
    'If threshold < 100 microseconds: most high-frequency trading qualifies as front running (pure snare). If threshold > 10 milliseconds: little extraction is detected (constraint appears as rope). Current regulatory ambiguity sits near threshold boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(latency_detection_threshold, empirical, 'Order latency threshold distinguishing liquidity provision from predatory front running').

omega_variable(
    information_leakage_necessity,
    'Is order flow information leakage a necessary byproduct of market microstructure, or an avoidable design choice?',
    'Comparative analysis of market designs: encrypted order books vs transparent queues; dark pool execution vs lit market execution; latency-parity mandates vs unlimited speed asymmetry',
    'If necessary byproduct: constraint is coordination cost (coordination_type=resource_allocation floor elevation justified). If design choice: front running is avoidable extraction (snare reclassifies with lower theater_ratio).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_leakage_necessity, conceptual, 'Whether order information leakage is necessary or avoidable in market design').

omega_variable(
    retail_exit_capacity,
    'Can retail investors realistically access alternative trading venues (decentralized exchanges, direct indexing) with sufficient scale to constitute a genuine exit option?',
    'Market adoption tracking: volume migration to DEXs and alternative protocols; retail participation surveys; transaction cost comparison across venues',
    'If exit capacity grows: trapped exit_options downgrade to constrained or mobile (snare classification softens to tangled_rope). If stalled: trapped status confirmed, snare classification hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_exit_capacity, empirical, 'Retail investor capacity to exit to alternative trading venues').

omega_variable(
    price_discovery_degradation_magnitude,
    'How much of observed market inefficiency and volatility is attributable to front running noise vs fundamental information processing?',
    'Decomposition analysis: vector autoregression models isolating algorithmic contribution to price impact; comparison of price discovery efficiency pre-HFT vs post-HFT eras',
    'If front running dominates: price discovery snare classification confirmed, extractiveness at 0.68+ justified. If minor component: most price discovery inefficiency has other roots, and price discovery constraint''s victimhood is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_discovery_degradation_magnitude, empirical, 'Attribution of price discovery degradation to algorithmic front running vs other sources').

omega_variable(
    suppression_internalization,
    'Is the high suppression metric (0.72) structural (regulatory gaps, technical barriers to detection) or internalized (retail traders have accepted market unfairness as normal)?',
    'Behavioral tracking: retail market participation trends and sentiment surveys; political mobilization for market regulation; knowledge surveys measuring awareness of front running mechanisms',
    'If structural: suppression persists regardless of awareness; constraint remains snare. If internalized: awareness campaigns and retail organizing could reduce suppression rapidly (reclassify to tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether high suppression is structural or internalized in retail market participation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_front_running, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algofr_tr_t0, algorithmic_front_running, theater_ratio, 0, 0.38).
narrative_ontology:measurement(algofr_tr_t10, algorithmic_front_running, theater_ratio, 10, 0.48).
narrative_ontology:measurement(algofr_tr_t20, algorithmic_front_running, theater_ratio, 20, 0.58).
narrative_ontology:measurement(algofr_tr_t5, algorithmic_front_running, theater_ratio, 5, 0.43).

% Extraction over time
narrative_ontology:measurement(algofr_be_t0, algorithmic_front_running, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(algofr_be_t10, algorithmic_front_running, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(algofr_be_t20, algorithmic_front_running, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(algofr_be_t5, algorithmic_front_running, base_extractiveness, 5, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_front_running, resource_allocation).
narrative_ontology:boltzmann_floor_override(algorithmic_front_running, 0.2).
narrative_ontology:affects_constraint(algorithmic_front_running, market_information_asymmetry).
narrative_ontology:affects_constraint(algorithmic_front_running, regulatory_latency_arbitrage).

% DUAL FORMULATION NOTE:
% Algorithmic front running decomposes into two structurally distinct constraints: (1) order flow information leakage (ε ≈ 0.55, resource allocation coordination with asymmetric extraction) and (2) latency arbitrage enforcement (ε ≈ 0.68, pure extraction enabled by detection opacity). Both stories are linked as family members; this story represents the unified view. Upstream constraint: market microstructure design. Downstream constraints: retail market participation rates, institutional execution cost inflation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_front_running, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
