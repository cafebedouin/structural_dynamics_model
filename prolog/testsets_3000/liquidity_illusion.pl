% ============================================================================
% CONSTRAINT STORY: liquidity_illusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liquidity_illusion, []).

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
 *   constraint_id: liquidity_illusion
 *   human_readable: The Exit Door Mirage
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The liquidity illusion is a structural constraint where financial or
 *   digital markets maintain the appearance of high liquidity (easy entry and
 *   exit at posted prices) during normal market conditions, but this
 *   appearance collapses during stress periods, trapping late-stage retail
 *   participants in positions they cannot exit without catastrophic loss. The
 *   constraint exhibits the defining snare signature: (1) high base
 *   extractiveness (0.58) — the gap between promised and actual exit capacity
 *   creates value extraction for those who exit early, (2) high suppression
 *   (0.68) — barriers to recognizing the illusion during normal conditions
 *   include regulatory assurances, positive feedback loops, and information
 *   asymmetry, and (3) vulnerability of trapped agents with no exit. The
 *   theater ratio (0.65) reflects that liquidity metrics displayed during
 *   normal times (bid-ask spreads, trading volume, order book depth) are
 *   genuine but systematically underpredict behavior during stress. The
 *   constraint operates through a temporal mechanism: early participants and
 *   informed actors (market makers, algorithmic traders) exit during the
 *   transition period while public metrics still show adequate liquidity; by
 *   the time remaining participants recognize the illusion, exit doors have
 *   closed. This is extraction via temporal information asymmetry, not via
 *   fixed barriers.
 *
 * KEY AGENTS:
 *   - Late-Stage Retail Participants: Primary victims (powerless/trapped) — enter during apparent liquidity, discover exit door is mirages; locked in during price decline
 *   - Market Makers: Primary beneficiaries (institutional/arbitrage) — capture spread revenue during normal periods; exit before liquidity collapses via privileged order flow knowledge
 *   - Sophisticated Institutional Investors: Secondary beneficiaries (organized/constrained) — benefit from coordination (liquid market infrastructure) while front-running retail exit attempts; constrained by position size but not by information
 *   - Algorithmic Traders: Informed extractors (organized/arbitrage) — detect liquidity transition microseconds before retail participants; establish advantageous exit positions before spreads widen
 *   - Regulatory Framework (SEC, CFTC, prudential regulators): Oversight actors (organized/mobile) — design circuit breakers and position limits to replace mirage with genuine resilience; sunset logic embedded in regulatory reform cycles
 *   - Liquidity Coverage Ratio Protocol: Institutional theater (institutional/arbitrage) — maintains performative verification of bank liquidity without testing true crisis-time exit capacity; degraded piton
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liquidity_illusion, 0.58).
domain_priors:suppression_score(liquidity_illusion, 0.68).
domain_priors:theater_ratio(liquidity_illusion, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liquidity_illusion, extractiveness, 0.58).
narrative_ontology:constraint_metric(liquidity_illusion, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(liquidity_illusion, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liquidity_illusion, snare).
narrative_ontology:human_readable(liquidity_illusion, "The Exit Door Mirage").
narrative_ontology:topic_domain(liquidity_illusion, "economic/technological").

domain_priors:requires_active_enforcement(liquidity_illusion).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liquidity_illusion, market_makers).
narrative_ontology:constraint_beneficiary(liquidity_illusion, early_exit_beneficiaries).
narrative_ontology:constraint_victim(liquidity_illusion, late_stage_retail_participants).
narrative_ontology:constraint_victim(liquidity_illusion, illiquidity_trap_victims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ILLIQUIDITY-TRAPPED RETAIL PARTICIPANT (SNARE) — Enters during period of apparent high liquidity; discovers exit option is illusory when attempting withdrawal during market stress. No exit: all capital locked. Experiences maximum extraction through forced holding during depreciation. Zero degrees of freedom.
constraint_indexing:constraint_classification(liquidity_illusion, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARKET MAKER (ROPE) — Sees liquidity mirage as coordination mechanism: maintains bid-ask spreads during normal conditions, capturing spread revenue while distributing price discovery information. Arbitrage option: can exit position at will through matched orders. Net beneficiary.
constraint_indexing:constraint_classification(liquidity_illusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SOPHISTICATED INSTITUTIONAL INVESTOR (TANGLED ROPE) — Constrained by size and position concentration; benefits from liquidity protocols and market-making infrastructure (coordination), but also participates in the illusion by front-running information about deteriorating liquidity conditions. Mixed extraction: some coordination benefit, but also uses information asymmetry to exit early before liquidity dries up.
constraint_indexing:constraint_classification(liquidity_illusion, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY INTERVENTION COALITION (SCAFFOLD) — Sees liquidity mirage as a temporary coordination failure solvable through circuit breakers, position limits, and stress-testing mandates. Sunset logic: regulations like SEC halts, volume limits, and transparent order book requirements are designed to replace the mirage with genuine market resilience, enabling real exit pathways. Theater declining as real oversight mechanisms mature.
constraint_indexing:constraint_classification(liquidity_illusion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LIQUIDITY COVERAGE RATIO PROTOCOL (PITON) — Banking regulations (LCR, NSFR) impose stress-test illusion of liquidity control, but the actual verification of true under-stress liquidity is limited. Protocol maintains itself through compliance theater despite questionable functional value during systemic stress. Banks satisfy ratio metrics without genuinely ensuring exit capacity in crisis. Theater ratio high; functional coordination low.
constraint_indexing:constraint_classification(liquidity_illusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INFORMATION ASYMMETRY VIEW (MOUNTAIN) — From a civilizational perspective, the liquidity mirage is an irreducible feature of market information: buyers and sellers can never have perfectly synchronized knowledge of true exit availability; information gaps are inherent to exchange mechanisms. This perspective sees the mirage as natural law. However, structural data contradicts mountain classification — the degree of asymmetry is institutional, not natural.
constraint_indexing:constraint_classification(liquidity_illusion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liquidity_illusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liquidity_illusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liquidity_illusion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(liquidity_illusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(liquidity_illusion, TR),
    TR >= 0.70.

:- end_tests(liquidity_illusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The illusion extracts from late-stage retail participants through temporal information asymmetry. Market makers and algorithmic traders see the transition before retail participants and exit first, capturing a window where spreads are tight but decline is predictable. This is not instantaneous extraction but a systematic temporal flow — early money exits at good prices while late money is forced to wait for worse prices or cannot exit at all. The base extractiveness reflects that the mirage mechanism itself is structural (it is how modern markets communicate liquidity via posted prices) but that exploitation of the mirage's breakdown is not universal — it requires information advantage. Suppression (0.68): High. During normal conditions, multiple forces suppress recognition of the illusion: (1) regulatory assurances and stress tests provide false confidence, (2) positive feedback loops reinforce the mirage (more volume → more liquidity → more participants → more volume), (3) information asymmetry means retail participants cannot see order flow that algorithmic traders observe, (4) incentive alignment makes market makers and exchanges promote the appearance of liquidity, and (5) rare-event recognition — the liquidity collapse is infrequent enough that retail participants do not internalize the risk. Theater Ratio (0.65): Moderate-high and rising. During normal periods, the theater ratio is moderate — spreads, volume, and order book depth are genuinely informative about liquidity at posted prices. But as time passes and positions concentrate, the theater rises: spreads stay tight while the market's actual ability to absorb large orders declines (a widening gap between micro liquidity and macro liquidity). By the time stress arrives, the theater ratio reaches critical levels — the posted metrics no longer predict true exit capacity. The rise in theater over the interval reflects Goodhart degradation: as market participants rely on liquidity metrics to guide entry decisions, those metrics become less predictive of actual liquidity, increasing the mirage effect.
 *
 * PERSPECTIVAL GAP:
 *   The liquidity illusion demonstrates how a single structural constraint produces six distinct classifications based on the observer's temporal position and information access. Retail participants see a snare — they are trapped after the fact. Market makers see coordination (rope) — they are solving the genuine problem of price discovery and matching orders. Sophisticated institutions see tangled rope — mixed coordination benefit and extraction opportunity through position exit timing. The regulatory coalition sees a temporary problem with a sunset (scaffold) — circuit breakers and position limits will replace the mirage with genuine stress resilience. The regulatory protocol itself appears as degraded theater (piton) — liquidity coverage ratios check compliance without verifying true crisis capacity. The analytical observer risks seeing an immutable natural law (mountain) — information gaps and temporal asymmetry are inherent to any trading system. But the structural data contradicts the mountain: the degree of asymmetry and the severity of the mirage are institutional choices (market structure, information rules, position limits) not natural laws. The largest perspectival gap is temporal: early observers during normal liquidity conditions see rope; late observers after the mirage collapses see snare. The same agent at different times experiences different constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's structural position determines their experienced extractiveness via the directionality pipeline. Retail participants are trapped (d → 0.95 → high f(d)) with no exit option; they bear full extraction cost. Market makers are institutional beneficiaries with arbitrage options (d → 0.05 → negative f(d)); they experience the constraint as pure coordination revenue. Sophisticated institutions are organized but constrained by size (d → 0.40 → moderate f(d)); they benefit from coordination but also participate in the extraction through front-running. Algorithmic traders have perfect arbitrage (d → 0.00 → minimum f(d)); they experience only coordination benefit. The regulatory framework and piton protocols are institutional arbitrage actors (d → 0.05) but with low functional effectiveness. The critical insight: the same constraint produces radically different experienced extractiveness depending on the agent's information access and exit timing — this temporal asymmetry is the extraction mechanism itself.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSIS: The liquidity illusion resolves mandatrophy by distinguishing between legitimate coordination (market-making that enables price discovery) and extraction (temporal asymmetry in exit capacity). The snare classification prevents misidentifying this as pure coordination by flagging the high suppression (0.68) and the distribution of costs — trapped agents bear them, informed agents escape them. The tangled rope perspective (sophisticated institutions) reveals that the constraint has a genuine coordination function (liquid markets enable efficient capital allocation) alongside asymmetric extraction (information advantage determines exit quality). The scaffold perspective is critical: it shows that the constraint's extractiveness is not immutable — regulatory interventions (circuit breakers, position limits, consolidated tape access) can reduce the information asymmetry and temporal advantage that create the mirage. The piton perspective reveals that regulatory theater (LCR protocols) can mimic the appearance of supervision without reducing the mirage's extractiveness. The mandatrophy resolves by showing that the classification depends entirely on whether you measure from before or after the transition: pre-transition, the constraint looks like rope; post-transition, it looks like snare. The true constraint is the temporal information asymmetry that enables the mirage itself — a snare for those who do not see it, a coordination mechanism for those who do.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liquidity_threshold_transition,
    'What specific price-volume threshold triggers the transition from illusion to revealed illiquidity? Does it depend on market conditions, asset class, or participant composition?',
    'Empirical analysis of historical market crashes and flash crashes; identification of consistent volume/spread thresholds before liquidity collapse; stress-test simulations with varying participation profiles',
    'If threshold is predictable and consistent: sophisticated participants can hedge against transition. If threshold is dependent on unobservable state: the mirage is deeper and extraction more severe for retail participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liquidity_threshold_transition, empirical, 'Threshold for liquidity collapse transition').

omega_variable(
    market_maker_inventory_constraints,
    'Do market makers actively suppress liquidity signals through inventory management to maintain the appearance of liquidity, or is liquidity genuinely available but constrained by inventory risk?',
    'Analysis of market maker order placement behavior during stress periods; correlation between inventory positions and quote availability; comparison of quoted vs actual available liquidity',
    'If active suppression: extraction mechanism is enforced. If passive constraint: mirage is shared delusion rather than rent extraction by informed actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_maker_inventory_constraints, empirical, 'Whether market makers actively suppress liquidity signals').

omega_variable(
    retail_vs_algorithmic_information_gap,
    'Can algorithmic traders identify the liquidity transition before retail participants due to superior latency and order flow analysis, creating a temporal window for privileged exit?',
    'Microsecond-level timestamp analysis of order placement and execution; latency measurements between retail platforms and exchange; identification of consistent algorithmic exit patterns preceding retail-visible liquidity collapse',
    'If gap exists and is exploitable: organized extraction mechanism confirmed (snare). If no consistent gap: the mirage affects all participants symmetrically (rope or tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retail_vs_algorithmic_information_gap, empirical, 'Information gap between algorithmic and retail participants in liquidity detection').

omega_variable(
    regulatory_stress_test_validity,
    'Do regulatory liquidity stress tests (LCR, NSFR) accurately predict institution ability to exit under actual systemic stress, or do they rely on assumptions about market participant behavior that break down during crisis?',
    'Comparison of bank pass rates on stress tests vs actual liquidity need fulfillment in recent financial crises; analysis of test assumptions vs observed market behavior (bid-ask spread expansion, volume drying, counterparty availability)',
    'If tests invalid: regulatory theater (piton) confirmed. If tests predictive: regulatory framework is genuine coordination mechanism (scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_stress_test_validity, empirical, 'Validity of regulatory liquidity stress tests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liquidity_illusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liq_tr_t0, liquidity_illusion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(liq_tr_t5, liquidity_illusion, theater_ratio, 5, 0.5).
narrative_ontology:measurement(liq_tr_t10, liquidity_illusion, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(liq_be_t0, liquidity_illusion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(liq_be_t5, liquidity_illusion, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(liq_be_t10, liquidity_illusion, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liquidity_illusion, information_standard).
narrative_ontology:affects_constraint(liquidity_illusion, circuit_breaker_theater).
narrative_ontology:affects_constraint(liquidity_illusion, algorithmic_front_running).
narrative_ontology:affects_constraint(liquidity_illusion, regulatory_capital_arbitrage).

% DUAL FORMULATION NOTE:
% The liquidity illusion can be decomposed into two structurally distinct constraints: (1) Market-making coordination (ε ≈ 0.15, Rope) — the genuine service of providing narrow spreads and price discovery, and (2) Mirage-dependent extraction (ε ≈ 0.58, Snare) — the temporal asymmetry in exit capacity that exploits the illusion's breakdown. These stories are related but structurally distinct; the extraction story is downstream of the coordination story because the illusion depends on the existence of the normal-times liquidity flow to create false expectations. See dual_formulation_note for decomposition rationale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liquidity_illusion, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
