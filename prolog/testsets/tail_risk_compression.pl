% ============================================================================
% CONSTRAINT STORY: tail_risk_compression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tail_risk_compression, []).

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
 *   constraint_id: tail_risk_compression
 *   human_readable: The Volatility Suppression Trap
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The volatility suppression trap emerges when authorities and market
 *   participants coordinate to suppress short-term volatility through policy
 *   mechanisms (circuit breakers, liquidity facilities, forward guidance,
 *   leverage caps), creating stability in nominal price movements while
 *   accumulating unobserved tail risks in the underlying system. The
 *   constraint manifests as a structural tension: suppressing volatility in
 *   regulated markets displaces risk into shadow structures or compresses it
 *   into delayed, catastrophic release events. This constraint exhibits all
 *   six DR types, each capturing a legitimate perspectival reality. The
 *   short-term beneficiaries (volatility sellers, liquidity providers, risk
 *   managers) experience coordination benefits during stable periods.
 *   Organized authorities recognize the constraint as temporary, implementing
 *   stress testing and counter-cyclical buffers as sunset mechanisms. Yet the
 *   system accumulates moral hazard, fragility, and concentration of tail
 *   risk, ultimately forcing tail event absorbers (pension funds, taxpayers,
 *   future market participants) to bear catastrophic losses. The theater
 *   ratio (0.68) reflects that much of the visible volatility suppression is
 *   announcement effect (central bank forward guidance, circuit breaker
 *   spectacle) rather than genuine capacity to absorb tail events. The
 *   constraint's extractiveness (0.58) reflects moderate-to-high structural
 *   asymmetry: real coordination benefits during normal times, but cascading
 *   costs concentrated into tail events.
 *
 * KEY AGENTS:
 *   - Short-Term Risk Managers: Primary beneficiary (institutional/arbitrage) — maintain stability, reduce immediate volatility, sell volatility derivatives at favorable pricing
 *   - Volatility Sellers: Primary beneficiary (institutional/arbitrage) — capture premium from volatility suppression, retain arbitrage optionality to reduce exposure before tail events
 *   - Tail Event Absorbers: Primary victim (powerless/trapped) — forced to absorb catastrophic losses when compressed volatility releases; pension funds, retail investors, future participants with no exit
 *   - Central Banks and Macroprudential Authorities: Secondary actor (organized/constrained) — constrained to implement suppression but building alternative pathways (stress testing, counter-cyclical buffers); see the constraint as temporary
 *   - Moral Hazard Exploiters: Secondary beneficiary (moderate/arbitrage) — leverage above rational levels using volatility suppression as implicit guarantee
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy choice as mathematical inevitability (tail risk cannot be destroyed)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tail_risk_compression, 0.58).
domain_priors:suppression_score(tail_risk_compression, 0.72).
domain_priors:theater_ratio(tail_risk_compression, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tail_risk_compression, extractiveness, 0.58).
narrative_ontology:constraint_metric(tail_risk_compression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tail_risk_compression, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tail_risk_compression, tangled_rope).
narrative_ontology:human_readable(tail_risk_compression, "The Volatility Suppression Trap").
narrative_ontology:topic_domain(tail_risk_compression, "economic/technological").

domain_priors:requires_active_enforcement(tail_risk_compression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tail_risk_compression, short_term_risk_managers).
narrative_ontology:constraint_beneficiary(tail_risk_compression, liquidity_providers).
narrative_ontology:constraint_beneficiary(tail_risk_compression, volatility_sellers).
narrative_ontology:constraint_victim(tail_risk_compression, tail_event_absorbers).
narrative_ontology:constraint_victim(tail_risk_compression, systemic_stability).
narrative_ontology:constraint_victim(tail_risk_compression, future_market_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAIL EVENT ABSORBER (SNARE) — Forced to absorb catastrophic losses when suppressed volatility releases. No exit mechanism; bears maximum structural extraction. Trapped in the system's tail risk externality with no individual remedy.
constraint_indexing:constraint_classification(tail_risk_compression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RISK MANAGER (TANGLED ROPE) — Constrained to implement volatility suppression policies (circuit breakers, risk limits, lending facilities) to maintain market function. Experiences both coordination benefit (prevents panic spirals) and extraction cost (delayed recognition of real risks accumulates into larger tail events). Cannot fully exit regulatory obligations but retains some discretion.
constraint_indexing:constraint_classification(tail_risk_compression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: VOLATILITY SELLER (ROPE) — Primary beneficiary. Sells volatility suppression as a coordination good: financial institutions reduce panic, maintain credit flow, enable long-term investment. Net beneficiary during stable periods; experiences the constraint as pure coordination with arbitrage optionality to reduce exposure before tail events.
constraint_indexing:constraint_classification(tail_risk_compression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MACROPRUDENTIAL AUTHORITY (SCAFFOLD) — Organized agents (central banks, regulatory bodies, Basel committees) treat volatility suppression as a temporary coordination mechanism with recognized sunset. Stress testing, counter-cyclical capital buffers, and tail-risk surcharges represent sunset logic: acknowledge and price tail risk rather than suppress it. Constraint has declining extraction because alternatives (realistic risk pricing) are being built into regulatory frameworks.
constraint_indexing:constraint_classification(tail_risk_compression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MARKET CONFIDENCE RITUAL (PITON) — The constraint persists through performative stability maintenance: central bank forward guidance, circuit breaker announcements, and liquidity facility establishment create the theater of control. The underlying function (actual volatility suppression) has degraded as tail risks have grown more complex, but the ritual persists through institutional inertia. Theater ratio high because much of the visible volatility management is announcement effect rather than real capacity.
constraint_indexing:constraint_classification(tail_risk_compression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TAIL RISK PHYSICS (MOUNTAIN) — From a mathematical finance and systems perspective, tail risk compression appears as an immutable structural property: suppressing volatility in one market locus forces it into another (displacement effect). Tail risk cannot be destroyed, only compressed and relocated. This perspective risks naturalizing what is actually a contingent policy choice. The false summit detector will identify this as naturalization of institutional arrangement as mathematical law.
constraint_indexing:constraint_classification(tail_risk_compression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tail_risk_compression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tail_risk_compression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tail_risk_compression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tail_risk_compression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tail_risk_compression, TR),
    TR >= 0.70.

:- end_tests(tail_risk_compression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint transfers tail risk from short-term risk managers and liquidity providers to powerless agents (pension funds, retail participants, future generations) who cannot observe or price compressed risk. The asymmetry is not absolute (some risk managers do accumulate tail exposure), but the structural beneficiaries have clear arbitrage optionality to reduce exposure before events. Suppression (0.72): High. Barriers to recognizing and pricing tail risk include: regulatory incentives for stability maintenance, behavioral bias toward recency effects, opacity of shadow finance structures, coordination barriers to collective deleveraging, and moral hazard erosion of individual risk perception. Escape routes exist (stress testing, tail-risk surcharges) but are constrained by coordination failures. Theater ratio (0.68): High. Central bank communications, circuit breaker announcements, liquidity facility establishment, and forward guidance create spectacle of control. The underlying real capacity has degraded as tail risks have become more complex (cyber-systemic interactions, flash crash dynamics, geopolitical feedback loops) and harder to suppress. Claimed type (tangled_rope): The constraint combines genuine coordination (prevented cascade losses, maintained credit flow) with asymmetric extraction (tail risk concentration, moral hazard). Active enforcement is present (circuit breakers, position limits, lending facilities). Both beneficiaries (volatility sellers) and victims (tail absorbers) exist structurally.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is extreme and irreconcilable during normal periods. Volatility sellers experience pure coordination (Rope) — they are solving the real problem of preventing panic and maintaining market function. They have full observability of their own tail risk exposure and arbitrage optionality to reduce it. Tail event absorbers experience pure extraction (Snare) — they are forced into a system where their risks are masked, their exit is blocked, and their losses are inevitable. They have no observability of compressed tail risk and no individual remedy. The macroprudential authority mediates this gap with scaffold logic: acknowledging tail risk exists, building stress-testing infrastructure, and implementing counter-cyclical capital buffers that represent a sunset pathway to realistic risk pricing. The market confidence ritual (piton perspective) shows that the constraint persists through performative stability announcements rather than real functional capacity. The analytical observer risks naturalizing the tension as an immutable property of financial markets (mountain), when the constraint is actually a contingent policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the extraction flow. Volatility sellers have low d (beneficiary status + arbitrage optionality → d ≈ 0.10) and experience negative effective extraction chi. Risk managers have moderate d (mixed beneficiary/victim + constrained exit → d ≈ 0.50) and experience moderate chi. Tail event absorbers have high d (victim status + trapped exit → d ≈ 0.90) and experience maximum chi. Organized authorities have lower d (victim status in long term + constrained but organized exit → d ≈ 0.35) because they retain agency to build alternative systems. The analytical observer has elevated d (observer position + analytical exit → d ≈ 0.72) because they see the full structure but have no direct intervention capacity. The directionality derivation chain moves from beneficiary/victim declarations through exit options to constraint-relative power assessment, producing the d values that feed the sigmoid f(d) and ultimately determine experienced chi.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that volatility suppression is genuinely both coordination and extraction, but the ratio shifts across time and across agent positions. In the short term (months to years), suppression provides real coordination benefits: reduced cascade losses, maintained credit flow, lower volatility-driven forced selling. In the long term (years to decades), accumulating moral hazard and hidden tail risk concentration reverses the coordination-to-extraction ratio. From the volatility seller's perspective, the constraint is pure coordination (Rope). From the tail absorber's perspective, the constraint is pure extraction (Snare). The macroprudential authority recognizes both modes and implements sunset mechanisms (stress testing, counter-cyclical buffers) that represent transition from suppression-based coordination to risk-pricing-based coordination. The false summit (mountain perspective) naturalizes the policy choice as an immutable property of finance, when the architecture is actually contingent on regulatory choices, coordination incentives, and moral hazard dynamics. The mandatrophy is resolved by: (1) acknowledging the real coordination benefits during normal periods, (2) pricing the genuine tail risk concentration in long-term volatility measures, (3) implementing scaffold mechanisms that transition to realistic risk-based coordination, and (4) avoiding the false summit that treats volatility suppression as inevitable law rather than policy choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_risk_displacement_mechanism,
    'Does volatility suppression in regulated markets displace tail risk into unregulated shadow structures, or does it genuinely reduce systemic tail exposure?',
    'Cross-market volatility correlation analysis; measurement of tail risk metrics (skewness, kurtosis, VaR) before and after suppression policies; shadow finance systemic importance studies',
    'If genuinely reduced: suppression is coordination benefit (Rope from more perspectives). If displaced: suppression is extraction mechanism masking concentration of tail risk (Snare). Classification hinges on whether global tail risk increases, decreases, or merely migrates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_risk_displacement_mechanism, empirical, 'Whether volatility suppression reduces or displaces tail risk').

omega_variable(
    moral_hazard_accumulation_rate,
    'What is the rate at which volatility suppression induces moral hazard (risk-taking leverage) relative to the rate at which it prevents panic cascades?',
    'Time series analysis of leverage ratios and risk-taking metrics; quantification of prevented losses from reduced cascade severity vs accumulated losses from moral hazard; modeling of critical cross-over points',
    'If cascade prevention dominates: suppression provides net coordination (shift to Rope from snare perspectives). If moral hazard dominates: suppression accumulates tail risk faster than it prevents cascade losses (shift to pure Snare). Extraction classification depends on which effect dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_hazard_accumulation_rate, empirical, 'Rate of moral hazard accumulation vs cascade prevention benefit').

omega_variable(
    stress_test_realism_gap,
    'Do regulatory stress tests capture the true distribution of plausible tail events, or do they mechanically underestimate the severity of novel system configurations?',
    'Backtesting stress test scenarios against realized tail events; measurement of gap between stress test VaR and realized losses; identification of scenario classes excluded from test matrices',
    'If tests are realistic: regulatory scaffold has real sunset logic and is functioning (sustained Scaffold). If tests are optimistic: scaffold is theater without functional tail risk reduction (degradation to Piton). This determines whether the macroprudential authority is building genuine alternatives or maintaining performative control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stress_test_realism_gap, empirical, 'Whether regulatory stress tests realistically capture tail event severity').

omega_variable(
    systemic_fragility_accumulation,
    'Is the system''s vulnerability to disruption increasing (fragility accumulation) despite apparent volatility stability, or is stability genuinely improving?',
    'Systemic fragility metrics: measuring interconnectedness concentration, illiquidity under stress, correlation breakdown during tail events; comparison of pre- and post-suppression fragility metrics',
    'If fragility increasing: suppression is pure extraction concentrated into catastrophic release (Snare dominates). If fragility stable or declining: suppression provides genuine coordination benefit (Rope or Scaffold dominates). This determines whether the tail event is inevitable or contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_fragility_accumulation, empirical, 'Whether system fragility is accumulating despite apparent stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tail_risk_compression, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tailrisk_tr_t0, tail_risk_compression, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tailrisk_tr_t7, tail_risk_compression, theater_ratio, 7, 0.52).
narrative_ontology:measurement(tailrisk_tr_t14, tail_risk_compression, theater_ratio, 14, 0.68).

% Extraction over time
narrative_ontology:measurement(tailrisk_be_t0, tail_risk_compression, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(tailrisk_be_t7, tail_risk_compression, base_extractiveness, 7, 0.43).
narrative_ontology:measurement(tailrisk_be_t14, tail_risk_compression, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tail_risk_compression, resource_allocation).
narrative_ontology:affects_constraint(tail_risk_compression, moral_hazard_leverage_accumulation).
narrative_ontology:affects_constraint(tail_risk_compression, shadow_finance_opacity).
narrative_ontology:affects_constraint(tail_risk_compression, tail_event_cascade_coupling).

% DUAL FORMULATION NOTE:
% The volatility suppression trap decomposes into three distinct constraints: (1) suppression-induced moral hazard (ε≈0.65, snare), (2) shadow finance opacity enabling risk displacement (ε≈0.42, tangled_rope), and (3) cascade coupling creating systemic fragility (ε≈0.48, tangled_rope). Each has different measurement bases but shares the upstream volatility suppression mechanism. The tail_risk_compression story represents the integrated view; downstream stories decompose into specific failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tail_risk_compression, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
