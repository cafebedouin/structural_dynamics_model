% ============================================================================
% CONSTRAINT STORY: herding_amplification_feedback
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herding_amplification_feedback, []).

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
 *   constraint_id: herding_amplification_feedback
 *   human_readable: Herding Amplification Feedback Loop
 *   domain: collective_behavior/behavioral_economics
 *
 * SUMMARY:
 *   Herding amplification feedback loops are self-reinforcing dynamics in
 *   which agent participation in an emerging trend increases the credibility
 *   and attractiveness of that trend, recruiting additional agents, which
 *   further increases credibility, until the cascade reverses when late
 *   entrants bear losses. The constraint creates a temporal asymmetry: early
 *   participants benefit from information advantage and momentum gains; late
 *   participants absorb reversal losses. This structure exhibits both genuine
 *   coordination (price discovery, liquidity aggregation) and genuine
 *   extraction (momentum-driven losses, information asymmetry exploitation),
 *   making it a paradigmatic tangled rope. The theatrical layer involves
 *   regulatory oversight (circuit breakers, position limits) that performs
 *   the function of preventing cascades without substantially preventing them
 *   — the theater ratio has increased as regulations have accumulated without
 *   proportional reduction in cascade frequency or severity. Emerging
 *   information access infrastructure (retail trading platforms, real-time
 *   data feeds, algorithmic transparency advocates) offers a potential sunset
 *   mechanism by reducing the information asymmetry that drives extraction,
 *   though the sufficiency of this mechanism remains contested.
 *
 * KEY AGENTS:
 *   - First-moving institutions: Primary beneficiaries (institutional/arbitrage) — capture information advantage and momentum gains during cascade initialization; can exit or reposition before reversal.
 *   - Early retail participants: Secondary beneficiaries (moderate/mobile) — benefit from liquidity and discovery of consensus; can exit with reduced loss if informed by platform data.
 *   - Late entrants: Primary victims (powerless/trapped) — absorb reversal losses; no exit option without bearing full opportunity cost and social penalty of standing apart.
 *   - Contrarians: Secondary victims (moderate/constrained) — bear opportunity cost and social proof pressure for divergent positions; face career risk for dissent.
 *   - Epistemic integrity: Systemic victim (powerless/trapped) — accurate price signals are degraded by momentum cascade; information efficiency declines during herding phases.
 *   - Regulatory agencies: Institutional theater performers (institutional/arbitrage) — maintain performative oversight; lack enforcement capacity or incentive to prevent cascades.
 *   - Information access platforms: Organized coalition members (organized/constrained) — building alternative information architecture that could reduce asymmetry and extract late-entrant losses.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herding_amplification_feedback, 0.58).
domain_priors:suppression_score(herding_amplification_feedback, 0.65).
domain_priors:theater_ratio(herding_amplification_feedback, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herding_amplification_feedback, extractiveness, 0.58).
narrative_ontology:constraint_metric(herding_amplification_feedback, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(herding_amplification_feedback, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herding_amplification_feedback, tangled_rope).
narrative_ontology:human_readable(herding_amplification_feedback, "Herding Amplification Feedback Loop").
narrative_ontology:topic_domain(herding_amplification_feedback, "collective_behavior/behavioral_economics").

domain_priors:requires_active_enforcement(herding_amplification_feedback).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herding_amplification_feedback, first_movers).
narrative_ontology:constraint_beneficiary(herding_amplification_feedback, information_asymmetry_exploiters).
narrative_ontology:constraint_victim(herding_amplification_feedback, late_entrants).
narrative_ontology:constraint_victim(herding_amplification_feedback, contrarians).
narrative_ontology:constraint_victim(herding_amplification_feedback, epistemic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTRARIAN / LATE ENTRANT (SNARE) — Trapped by coordination dynamics: cannot exit the herd without bearing full opportunity cost and social penalty. If the crowd is buying assets, the contrarian who stands apart loses career capital and liquidity access. Maximum experienced extraction — high suppression (social proof, FOMO, opportunity cost), high d (victim status), severe asymmetry.
constraint_indexing:constraint_classification(herding_amplification_feedback, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-MOVING INSTITUTION (ROPE) — Benefits from information advantage during cascade initialization. Experiences the constraint as coordination: identifying and participating in emerging consensus solves the collective action problem of uncertain asset valuation. Low effective extraction because arbitrage options exist (exit when asymmetry closes). Beneficiary with full mobility.
constraint_indexing:constraint_classification(herding_amplification_feedback, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: RETAIL PARTICIPANT (TANGLED ROPE) — Faces coordination benefit (finds liquidity, discovers consensus price) alongside extraction (momentum forces losses when herd reverses). Constrained by information barriers and opportunity costs. Mixed experience: genuine coordination function (price discovery) coupled with asymmetric extraction (late entrants absorb reversal losses).
constraint_indexing:constraint_classification(herding_amplification_feedback, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY OVERSIGHT (PITON) — Market surveillance and herding detection rules exist but are largely performative. Circuit breakers, position limits, and behavioral finance disclosure rules persist through regulatory inertia despite their minimal impact on cascade dynamics. Theater ratio ≥ 0.60: the ritual of regulation substitutes for actual cascade prevention. Institutions can arbitrage around limits; the rules perform compliance without preventing herding.
constraint_indexing:constraint_classification(herding_amplification_feedback, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INFORMATION ACCESS MOVEMENT (SCAFFOLD) — Organized actors (real-time data providers, algorithmic trading transparency advocates, retail investor platforms) are building alternative information architectures that reduce asymmetry. Lower effective extraction under this scenario because coalition agents have agency and see an exit path: reducing information lag (via direct market data access, algorithmic disclosure, retail research tools) undermines first-mover advantage. Sunset clause: 10-15 years as retail platforms mature.
constraint_indexing:constraint_classification(herding_amplification_feedback, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Herding cascade has genuine coordination function (price discovery, liquidity aggregation) and genuine extraction mechanism (momentum-driven losses, information asymmetry exploitation). The constraint cannot be reduced to either pure coordination or pure extraction. The feedback loop solves a real collective problem while systematically extracting from agents without early access or contrarian courage. Civilian-scale measurement: moderate extractiveness (0.58) reflects both functions present.
constraint_indexing:constraint_classification(herding_amplification_feedback, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herding_amplification_feedback_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(herding_amplification_feedback, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(herding_amplification_feedback, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(herding_amplification_feedback, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(herding_amplification_feedback, TR),
    TR >= 0.70.

:- end_tests(herding_amplification_feedback_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint systematically transfers wealth from late entrants to early movers, but the magnitude is not maximal (snare levels) because: (a) early movers' gains are partly legitimate first-mover compensation rather than pure extraction, (b) some late entrants do profit if they accurately predict cascade continuation, and (c) price discovery provides real value to markets. The measurement trajectory (0.35 → 0.58 over 20 periods) shows accumulation: as cascades become larger and more frequent (due to faster information diffusion, leverage availability, passive index flows), extraction deepens. Suppression (0.65): Moderate-high. Significant barriers to exiting herding include: social proof dynamics (psychological), opportunity cost (economic), career risk (institutional), information access limitations (structural). Suppression is both structural and partly internalized (FOMO, identity fusion with trend consensus). Theater ratio (0.48): Moderate. Regulatory oversight is substantially performative — circuit breakers and position limits persist despite minimal impact on cascade initiation or severity. But the theater is not dominant (≤0.70): actual trading happens, real price discovery occurs, genuine liquidity is aggregated. The constraint is not purely performative; it has material function alongside theatrical regulation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a classic perspectival inversion: the beneficiary (institutional/arbitrage) perceives coordination (rope) because they can exit and arbitrage; the victim (powerless/trapped) perceives extraction (snare) because they cannot exit. The analytical perspective resolves this as both true simultaneously (tangled rope): the coordination function is real (price discovery, liquidity), and the extraction mechanism is real (momentum losses, information asymmetry). The regulatory perspective (piton) is a view of the constraint's theatrical layer, not its structural layer — it sees itself, not the dynamics it claims to oversee.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is computed from power, exit options, and beneficiary/victim status. First movers (institutional/arbitrage/beneficiary) derive d ≈ 0.05-0.15 (low/negative f(d)), experiencing coordination. Late entrants (powerless/trapped/victim) derive d ≈ 0.95 (high f(d) ≈ 1.42), experiencing maximum extraction. Retail participants (moderate/constrained/mixed) derive d ≈ 0.70-0.80 (moderate-high f(d)), experiencing mixed coordination and extraction. The scope modifier σ(S) = 1.2 (global) amplifies effective extraction by 20% relative to national scope, reflecting that global cascades involve larger late-entrant populations. The piton perspective's low extraction (despite high d) derives from theater ratio gate: performative regulation reduces experienced extractiveness even when structural extraction is high.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by exemplifying why tangled rope requires both beneficiary/victim declarations. Without the victim declaration, the constraint collapses to rope (pure coordination for price discovery). Without the beneficiary declaration, it collapses to snare (pure extraction). The mandatrophy is resolved by the presence of BOTH: early movers genuinely benefit and solve coordination problems; late entrants genuinely lose and bear extraction. No single type is accurate; the presheaf over agent perspectives is the correct representation. The regulatory theater (piton) is not an alternative reading but a performative layer that masks the underlying tangled rope structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_cascade_vs_momentum_extraction,
    'Is herding cascade primarily a self-reinforcing coordination mechanism for price discovery or primarily an extraction mechanism exploiting information asymmetry?',
    'Empirical analysis of reversal patterns: If reversals correlate with information shocks, cascade is mainly coordination. If reversals correlate with late-entrant participation timing, cascade is mainly extraction. Comparison with efficient market prices in high-transparency markets.',
    'If coordination-dominant: lower effective extraction, rope or scaffold classification more accurate. If extraction-dominant: higher effective extraction, snare classification warranted for late entrants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_cascade_vs_momentum_extraction, empirical, 'Whether herding is coordination or extraction mechanism').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is participant suppression (inability to exit herd) primarily structural (liquidity constraints, career risk, no alternative information sources) or primarily internalized (social proof bias, FOMO, identity fusion with consensus)?',
    'Post-exit behavior tracking: If suppression persists after participant removes themselves from herding environment (via sabbatical, isolated decision-making), suppression is partly internalized. If suppression dissolves upon access to independent information, suppression is primarily structural. Cognitive intervention studies (debiasing protocols) effectiveness.',
    'If internalized: effective suppression higher than measured, constraint is more binding. If structural: interventions targeting information access and career incentives could reduce suppression significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    circuit_breaker_actual_efficacy,
    'Do trading halts, circuit breakers, and position limits actually reduce herding cascade severity or merely delay and redirect the cascade?',
    'Comparative analysis: pre- vs post-circuit-breaker implementation in multiple markets. Identification of cascade resumption vs genuine prevention. Analysis of short-term delaying effects vs long-term frequency reduction.',
    'If efficacious: regulatory scaffold perspective is partly correct, suggesting sunset is possible. If ineffective: piton classification is accurate (ritual without function), and alternative mechanisms (information access) are the genuine exit path.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(circuit_breaker_actual_efficacy, empirical, 'Whether circuit breakers prevent or merely delay herding').

omega_variable(
    retail_data_access_sufficiency,
    'Does real-time data access for retail participants (via platforms like Robinhood, interactive brokers, modern fintech) actually reduce information asymmetry relative to institutional traders, or does it create a false sense of parity while maintaining hidden structural advantages?',
    'Time-series analysis of retail vs institutional entry timing in cascades. Measurement of execution latency and hidden information access (dark pools, algorithmic prediction). Testing whether retail agents with ''equal'' data actually receive it with equivalent latency and can exploit it with equivalent sophistication.',
    'If sufficient: information access coalition''s sunset is real and herding extraction could decline. If insufficient: ''democratized data'' is theater masking persistent asymmetries, and herding extraction will persist despite apparent information parity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_data_access_sufficiency, empirical, 'Whether retail data access reduces or masks information asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herding_amplification_feedback, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herd_tr_t0, herding_amplification_feedback, theater_ratio, 0, 0.3).
narrative_ontology:measurement(herd_tr_t10, herding_amplification_feedback, theater_ratio, 10, 0.42).
narrative_ontology:measurement(herd_tr_t20, herding_amplification_feedback, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(herd_be_t0, herding_amplification_feedback, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(herd_be_t10, herding_amplification_feedback, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(herd_be_t20, herding_amplification_feedback, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herding_amplification_feedback, resource_allocation).
narrative_ontology:affects_constraint(herding_amplification_feedback, information_asymmetry_in_markets).
narrative_ontology:affects_constraint(herding_amplification_feedback, momentum_trading_cycles).
narrative_ontology:affects_constraint(herding_amplification_feedback, liquidity_provision_extraction).

% DUAL FORMULATION NOTE:
% Herding amplification feedback is downstream of information asymmetry but represents a distinct structural constraint. The upstream information asymmetry constraint has its own ε reflecting epistemic barriers; herding feedback has its own ε reflecting behavioral amplification and momentum extraction. The two constraints are linked: information asymmetry enables herding, but herding persists even when information is symmetric (via social proof and coordination benefits).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(herding_amplification_feedback, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
