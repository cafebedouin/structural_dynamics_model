% ============================================================================
% CONSTRAINT STORY: crypto_retail_pump_and_dump
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_crypto_retail_pump_and_dump, []).

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
 *   constraint_id: crypto_retail_pump_and_dump
 *   human_readable: Cryptocurrency Retail Pump-and-Dump Extraction
 *   domain: financial_markets/cryptocurrency
 *
 * SUMMARY:
 *   Cryptocurrency retail pump-and-dump schemes exemplify a snare constraint
 *   where early insiders and institutional beneficiaries extract wealth from
 *   retail investors through coordinated price manipulation, influencer hype,
 *   and information asymmetry. The constraint operates in phases:
 *   accumulation (quiet period, low theater, moderate extraction as insiders
 *   quietly acquire), pump (visible hype through social media and
 *   influencers, theater rises, extraction increases as retail enters), and
 *   dump (coordinated selling by insiders, theater remains high as narratives
 *   shift, extraction peaks as retail panic-sells at losses). Retail
 *   participants experience maximum suppression through FOMO-driven
 *   decision-making, psychological barriers to selling at loss, and
 *   asymmetric access to exit timing. The scheme is not merely coordination
 *   among insiders but a structural trap that exploits behavioral finance
 *   properties (recency bias, sunk-cost fallacy, momentum-following) that are
 *   themselves part of the psychological landscape. Unlike traditional
 *   pump-and-dumps in regulated markets, crypto schemes benefit from
 *   jurisdictional fragmentation, minimal enforcement, and the technical
 *   opacity of blockchain transactions that obscure insider wallet movements
 *   until patterns become obvious in hindsight.
 *
 * KEY AGENTS:
 *   - Early Insiders (Pre-Pump Holders): Primary beneficiary (institutional/arbitrage) — accumulate during quiet phase, coordinate pump signal, exit at peak, capture extraction flow
 *   - Retail Investors: Primary victim (powerless/trapped) — drawn by FOMO and hype; enter during visible pump; lack information on insider positions and timing; exit during cascading losses
 *   - Influencer Promoters: Secondary beneficiary (organized/constrained) — mobilize attention and hype through social platforms; receive payment or token allocation from insiders; face reputational risk if exposed; partly extracted from by insiders (underpaid relative to audience reach)
 *   - Exchange Operators: Institutional beneficiary (institutional/arbitrage) — collect transaction fees on both pump and dump volume; user acquisition from retail participants; no extraction cost
 *   - Mid-Wave Entrants: Secondary victim (moderate/constrained) — enter during visible but still-rising pump; constrained by sunk-cost psychology and difficulty timing exit before dump; experience severe extraction
 *   - Regulatory Authorities: Institutional observer (powerful/mobile) — traditional market manipulation rules exist but enforcement is degraded to theater; rules unenforceable across fragmented global crypto markets; see own rulebooks as outdated
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function (insiders timing each other) and asymmetric extraction (retail losses subsidize insider gains); recognizes suppression as structural to asymmetric information
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crypto_retail_pump_and_dump, 0.68).
domain_priors:suppression_score(crypto_retail_pump_and_dump, 0.72).
domain_priors:theater_ratio(crypto_retail_pump_and_dump, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crypto_retail_pump_and_dump, extractiveness, 0.68).
narrative_ontology:constraint_metric(crypto_retail_pump_and_dump, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(crypto_retail_pump_and_dump, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crypto_retail_pump_and_dump, snare).
narrative_ontology:human_readable(crypto_retail_pump_and_dump, "Cryptocurrency Retail Pump-and-Dump Extraction").
narrative_ontology:topic_domain(crypto_retail_pump_and_dump, "financial_markets/cryptocurrency").

domain_priors:requires_active_enforcement(crypto_retail_pump_and_dump).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(crypto_retail_pump_and_dump, early_insiders).
narrative_ontology:constraint_beneficiary(crypto_retail_pump_and_dump, exchange_operators).
narrative_ontology:constraint_beneficiary(crypto_retail_pump_and_dump, influencer_promoters).
narrative_ontology:constraint_victim(crypto_retail_pump_and_dump, retail_investors).
narrative_ontology:constraint_victim(crypto_retail_pump_and_dump, late_entrants).
narrative_ontology:constraint_victim(crypto_retail_pump_and_dump, market_price_discovery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Trapped by information asymmetry, FOMO-driven decision-making, and sunk-cost psychology. Entry occurs during peak hype; exit during cascading losses. No meaningful exit option: the psychological mechanics of panic selling during dumps are not negotiable — the investor's own behavior becomes the extraction mechanism. Maximum suppression through artificial scarcity of price history, wallet movement opacity, and coordinated social media amplification.
constraint_indexing:constraint_classification(crypto_retail_pump_and_dump, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY INSIDERS (ROPE) — Experience the constraint as pure coordination: signal the potential to each other through promotional channels, accumulate during quiet phase, then coordinate the pump signal to drive price. The constraint solves the collective action problem of timing entry/exit and managing information release. Arbitrage options abound — they can exit whenever desired. Net beneficiary with no extraction cost.
constraint_indexing:constraint_classification(crypto_retail_pump_and_dump, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: INFLUENCER PROMOTERS (TANGLED ROPE) — Coordinate the attention-capture mechanism (YouTube, Twitter, Discord hype channels) while being partially extracted from by the insiders who created the token or hired them. Constrained by reputational risk if the scheme is exposed; career income depends on continued access to pump tokens. Both coordination (mobilizing followers) and asymmetric extraction (insider payment often below what the influencer's reach is worth).
constraint_indexing:constraint_classification(crypto_retail_pump_and_dump, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MID-WAVE ENTRANT (SNARE) — Enters during the visible pump (hype is apparent but still rising). Constrained by the cost of entry and the psychological difficulty of selling at a loss before the peak. Experiences maximum extraction because they bought high and sell low. The constraint's suppression operates through momentum-ignoring price information and technical trading signals that encourage hold-through-dump behavior.
constraint_indexing:constraint_classification(crypto_retail_pump_and_dump, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EXCHANGE OPERATOR (ROPE) — Pure beneficiary. Collects transaction fees on every pump and dump transaction, faces no constraint cost, and enjoys increased trading volume and user acquisition from pump schemes. No enforcement overhead required — the market's own greed provides the coordination signal. Arbitrage options available at all phases.
constraint_indexing:constraint_classification(crypto_retail_pump_and_dump, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY AUTHORITY (PITON) — Traditional securities regulation (pump-and-dump prohibitions, insider trading rules, market manipulation statutes) exists on paper but has degraded into theater for crypto markets. Enforcement is slow, penalties are small relative to profits, and jurisdiction is fragmented globally. Regulators see their own rulebooks as outdated and unenforceable in this domain. Theater ratio is high because rule-making persists despite acknowledged inability to enforce. The constraint (the pump-and-dump scheme itself) persists through institutional inertia in investor behavior, not through regulatory force.
constraint_indexing:constraint_classification(crypto_retail_pump_and_dump, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the pump-and-dump constraint exhibits both genuine coordination (timing mechanisms among insiders, mobilizing attention through influencers) and asymmetric extraction (retail losses funnel to insiders). The constraint persists because both functions are real. Coordination enables the scheme; extraction is the point. Suppression is high and structural: price discovery is degraded by information asymmetry and coordinated misinformation. The constraint cannot be removed by education alone because the extraction is guaranteed by the power asymmetry and the exit trap.
constraint_indexing:constraint_classification(crypto_retail_pump_and_dump, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crypto_retail_pump_and_dump_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(crypto_retail_pump_and_dump, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crypto_retail_pump_and_dump, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(crypto_retail_pump_and_dump, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(crypto_retail_pump_and_dump, TR),
    TR >= 0.70.

:- end_tests(crypto_retail_pump_and_dump_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The scheme's primary function is transfer of wealth from retail to insiders. Measurements show extractiveness rising from 0.35 (quiet accumulation phase) to 0.78 (peak dump phase), stabilizing at 0.68 in aftermath as survivors reassess and new retail cohorts join. The 0.68 final value reflects that most initiated schemes extract 50-90% of retail entrants' capital, and the ecosystem perpetuates itself through continuous new retail cohorts attracted by earlier success stories. Suppression (0.72): High. Multiple suppression mechanisms operate simultaneously: (1) Information asymmetry — insiders know accumulation patterns and exit timing; retail does not. (2) Technical opacity — wallet movements on blockchain are pseudonymous and forensically difficult for retail to interpret in real-time. (3) Behavioral exploitation — FOMO, recency bias, and momentum-following override rational price evaluation. (4) Coordinated narrative control — influencers and insiders synchronize message timing to create perception of genuine adoption rather than coordinated manipulation. (5) Regulatory absence — unlike traditional markets, pump-and-dumps face minimal enforcement, so schemes repeat with low criminal cost. Theater ratio (0.58): Moderate-high. The performative components are: promotional claims about project fundamentals, influencer testimonials about 'genuine potential,' technical-sounding narratives about ecosystem opportunities. These are performance designed to rationalize price movement that is actually driven by insider exit timing. The theater ratio rises during the pump (when claims are most assertive) and falls in aftermath (when reality becomes undeniable). The moderate-final value reflects that some legitimate retail participants hold for genuinely speculative reasons (true speculation on protocol adoption), creating a blend of functional and performative holding.
 *
 * PERSPECTIVAL GAP:
 *   The schism between insider and retail perspectives is not merely observational but structural. Insiders with pre-announcement information and wallet mobility see a coordination problem (Rope) — 'how do we time our entry and exit to maximize our collective benefit?' Retail without information and with behavioral lock-in see a snare — 'why can I not exit without loss?' These are not different framings of the same constraint; they are different constraints from each perspective. For insiders, the constraint is 'coordinate timing among ourselves to maximize profit.' For retail, the constraint is 'capital preservation in the face of coordinated extraction.' The gap reveals that the constraint's name ('pump-and-dump') naturalizes the insider perspective (it is a tactical pump-and-dump for benefit) while obscuring the victim perspective (it is a wealth-transfer trap). The analytical observer must hold both perspectives simultaneously to classify correctly as Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary-victim structure is stark. Beneficiaries: early insiders (d≈0.05, arbitrage options, pre-announcement information), exchange operators (d≈0.00, neutral facilitators earning fees on both sides), influencer promoters (d≈0.30, paid for promotion, some underpayment but still net positive). Victims: retail investors (d≈0.95, trapped by FOMO and sunk-cost psychology), mid-wave entrants (d≈0.85, constrained by entry cost and psychological barriers to loss-realization), market price discovery (d≈1.0, abstract victim, noise injection into signal). The chi formula amplifies the asymmetry: beneficiaries with arbitrage exit experience low/negative chi; victims with trapped/constrained exit experience chi > 1.0 (extraction exceeds base ε). The global scope (σ=1.2) further amplifies extraction for victims because they are geographically dispersed and cannot coordinate countermeasures. Local-scope retail (e.g., a single university community catching on to a pump scheme) would experience lower chi due to σ=0.8, reflecting that local-scale schemes have higher detection and coordination risk.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY: The constraint's classification as SNARE is firm. The challenge is not distinguishing snare from tangled_rope but recognizing that the constraint contains both: genuine coordination among insiders (tangled_rope function) and maximum extraction for retail (snare function). The extraction function is the primary one — the constraint would not exist without retail participation. The analytical observer sees tangled_rope because both components are visible. The retail observer sees snare because only extraction is experienced. The insider observer sees rope because only coordination is experienced. No mislabeling of snare-as-rope occurs here because the power asymmetry is transparent and measured. The mandatrophy resolves: the constraint is snare for retail, rope for insiders, and tangled_rope for the analytical observer who must integrate both. The contradiction is resolved by accepting that classification is index-relative and that the constraint's PURPOSE is extraction (snare), though it CONTAINS coordination mechanisms (tangled_rope from analytical view).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    insider_vs_lucky_early,
    'What distinguishes a coordinated pump-and-dump from a lucky early investor who promotes a project they genuinely believed in?',
    'Forensic wallet analysis and communication records: pre-coordinated message exchanges, token allocation patterns, timing of exit relative to promotion. Distinguish pre-planned dumps from post-hoc selling.',
    'If coordination is provable: snare classification holds. If organic hype: classification shifts toward rope (coordination) or scaffold (early adoption with eventual correction). Current high suppression reflects that this distinction is often forensically invisible to retail observers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insider_vs_lucky_early, empirical, 'Distinguishing coordinated insider pump-and-dump from organic early-adoption').

omega_variable(
    influencer_agency_vs_capture,
    'Are influencer promoters autonomous agents using their platform for profit, or identity-locked actors captured by the promise of access and income from pump-token ecosystem?',
    'Post-exposure analysis: Do influencers continue promoting after a scheme is exposed? Do they shift to other schemes with identical mechanics? Do they express remorse or rationalizations? Identity-locked actors continue the behavior; captured agents withdraw.',
    'If identity-locked: influencers are partly trapped (exit requires identity rebuild). If autonomous: they are willing extractors alongside insiders. Classification shifts influencer perspective toward rope (from tangled_rope) if truly autonomous, or toward snare (from tangled_rope) if identity-locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(influencer_agency_vs_capture, empirical, 'Whether influencer promoters are identity-locked or autonomous agents').

omega_variable(
    retail_fomo_mechanism_source,
    'Is FOMO-driven retail buying a structural result of coordinated hype, or a failure of individual investor judgment independent of insider coordination?',
    'Comparative analysis: Retail entry timing and volume on tokens with zero influencer promotion vs heavy influencer promotion. Distinguish supply-driven (insiders coordinate, retail follows) from demand-driven (retail demand creates opportunity for exit timing).',
    'If supply-driven: suppression value (0.72) is justified. If demand-driven: retail participation is less trapped; suppression could be lower, classification shifts toward scaffold (early boom with predictable correction). Current assignment assumes supply-driven mechanics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_fomo_mechanism_source, empirical, 'Whether FOMO is coordinated-hype-driven or autonomous-demand-driven').

omega_variable(
    exit_liquidity_availability,
    'During the dump phase, is there enough buy-side liquidity for mid-wave retail to exit at prices above their entry cost?',
    'Order book analysis during pump-dump cycles: depth of bid side during the downslope. Measure the percentage of retail positions that could exit profitably before price collapses.',
    'If high liquidity: trapped classification is partially incorrect (mobile becomes possible). If low liquidity: trapped classification is correct (exit is a mirage). Current suppression (0.72) assumes liquidity evaporates during dump.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_liquidity_availability, empirical, 'Whether exit liquidity remains available during dump phase').

omega_variable(
    information_asymmetry_structural,
    'Is the information asymmetry between insiders and retail structural to cryptocurrency markets, or contingent to regulatory absence?',
    'Comparison to traditional equity pump-and-dumps: Do regulated markets exhibit similar retail loss rates? Has enforcement reduced loss magnitude where it exists? Current enforcement in crypto: ~0% for most schemes.',
    'If structural: snare persists regardless of regulation. If contingent: stronger enforcement and transparency requirements (exchange transaction reporting, influencer disclosure rules) would shift classification toward rope or scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_structural, empirical, 'Whether information asymmetry is structural or enforcement-contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crypto_retail_pump_and_dump, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crypd_theater_early, crypto_retail_pump_and_dump, theater_ratio, 0, 0.42).
narrative_ontology:measurement(crypd_theater_pump, crypto_retail_pump_and_dump, theater_ratio, 2, 0.58).
narrative_ontology:measurement(crypd_theater_dump, crypto_retail_pump_and_dump, theater_ratio, 4, 0.68).
narrative_ontology:measurement(crypd_theater_aftermath, crypto_retail_pump_and_dump, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(crypd_extract_early, crypto_retail_pump_and_dump, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(crypd_extract_pump, crypto_retail_pump_and_dump, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(crypd_extract_dump, crypto_retail_pump_and_dump, base_extractiveness, 4, 0.78).
narrative_ontology:measurement(crypd_extract_aftermath, crypto_retail_pump_and_dump, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(crypto_retail_pump_and_dump, resource_allocation).
narrative_ontology:boltzmann_floor_override(crypto_retail_pump_and_dump, 0.12).
narrative_ontology:affects_constraint(crypto_retail_pump_and_dump, cryptocurrency_price_volatility).
narrative_ontology:affects_constraint(crypto_retail_pump_and_dump, regulatory_fragmentation_crypto).
narrative_ontology:affects_constraint(crypto_retail_pump_and_dump, influencer_capture_ecosystem).

% DUAL FORMULATION NOTE:
% Pump-and-dump schemes decompose into several structurally distinct constraints: (1) insider_coordination (ε≈0.15, Rope) — the coordination problem among early holders; (2) retail_snare (ε≈0.78, Snare) — the extraction trap for late entrants; (3) influencer_asymmetric_extraction (ε≈0.45, Tangled Rope) — promoters benefit but are partly exploited by insiders. This story focuses on the integrated constraint (ε≈0.68, Snare) that includes all three components and their interaction. The upstream constraint is regulatory_fragmentation_crypto (fragmented jurisdiction enables low enforcement); downstream constraints are specific token schemes that instantiate the general pump-and-dump template.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(crypto_retail_pump_and_dump, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
