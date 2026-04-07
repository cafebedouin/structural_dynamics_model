% ============================================================================
% CONSTRAINT STORY: market_concentration_feedback_loops
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_concentration_feedback_loops, []).

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
 *   constraint_id: market_concentration_feedback_loops
 *   human_readable: Market Concentration Feedback Loops
 *   domain: economic/industrial_organization
 *
 * SUMMARY:
 *   Market concentration feedback loops represent a structural constraint
 *   where initial market power generates mechanisms that entrench and amplify
 *   that power over time. Larger firms achieve lower unit costs (scale),
 *   which enables lower prices or higher margins (capital accumulation),
 *   which attracts customers and investment capital, which increases scale
 *   further. Simultaneously, network effects, switching costs, data
 *   advantages, and acquisition strategies for competitive threats create
 *   asymmetric barriers to entry that protect incumbents. The constraint
 *   exhibits tangled rope characteristics: genuine coordination benefits
 *   (standards, platform stability, global competitiveness) coexist with
 *   systematic extraction from new entrants and consumer choice restriction.
 *   The extractiveness value (0.58) reflects moderate base extraction — some
 *   consolidation is welfare-improving, but the rate and magnitude of
 *   concentration in recent decades exceeds what scale economies alone would
 *   justify. Suppression (0.65) is high: capital requirements, network
 *   effects, regulatory approval timelines, and incumbent acquisition of
 *   nascent competitors create durable barriers. Theater ratio (0.48)
 *   indicates moderate performative content: antitrust doctrine uses consumer
 *   welfare and efficiency arguments to justify consolidation while obscuring
 *   dynamic competition effects.
 *
 * KEY AGENTS:
 *   - Incumbent Firms: Primary beneficiary (institutional/arbitrage) — capture market power, margins, and acquisition channels for competitive threats
 *   - New Entrants: Primary victim (powerless/trapped) — face capital barriers, network effects, incumbent acquisition strategies, and regulatory approval delays
 *   - Consumers: Secondary victim (powerless/trapped) — reduced choice set, lock-in effects, price increases, and diminished switching optionality
 *   - Financial Intermediaries: Secondary beneficiary (powerful/mobile) — benefit from acquisition exit channels and portfolio concentration; constrained by regulatory scrutiny
 *   - Antitrust Regulator: Organized enforcer (organized/constrained) — faces genuine trade-offs between dynamic and allocative efficiency; constrained by budget, expertise, and political pressure
 *   - Antitrust Doctrine: Institutional framework (institutional/arbitrage) — provides legitimation framework for consolidation; theater persists through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_concentration_feedback_loops, 0.58).
domain_priors:suppression_score(market_concentration_feedback_loops, 0.65).
domain_priors:theater_ratio(market_concentration_feedback_loops, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_concentration_feedback_loops, extractiveness, 0.58).
narrative_ontology:constraint_metric(market_concentration_feedback_loops, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(market_concentration_feedback_loops, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_concentration_feedback_loops, tangled_rope).
narrative_ontology:human_readable(market_concentration_feedback_loops, "Market Concentration Feedback Loops").
narrative_ontology:topic_domain(market_concentration_feedback_loops, "economic/industrial_organization").

domain_priors:requires_active_enforcement(market_concentration_feedback_loops).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_concentration_feedback_loops, incumbent_firms).
narrative_ontology:constraint_beneficiary(market_concentration_feedback_loops, financial_intermediaries).
narrative_ontology:constraint_victim(market_concentration_feedback_loops, new_entrants).
narrative_ontology:constraint_victim(market_concentration_feedback_loops, consumers).
narrative_ontology:constraint_victim(market_concentration_feedback_loops, competitive_market_function).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POTENTIAL ENTRANT (SNARE) — New firms face capital requirements, network effects, and incumbent acquisition strategies that eliminate viable entry pathways. Cannot exit the barrier structure; must accept non-participation or acquisition on incumbent terms. Maximum extraction through suppression of alternatives.
constraint_indexing:constraint_classification(market_concentration_feedback_loops, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSUMER BASE (SNARE) — Reduced choice set, price increases, and lock-in effects from network externalities. Trapped by switching costs and lack of alternatives. Over a generational horizon, consumer welfare deteriorates and exit becomes unavailable for subsequent cohorts born into the concentrated market.
constraint_indexing:constraint_classification(market_concentration_feedback_loops, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INCUMBENT FIRM (ROPE) — Experiences the constraint as a coordination mechanism: controlling market share enables investment in standards, platforms, and ecosystem development. Network effects and data advantages are genuine coordination benefits. Net beneficiary — extraction flows toward this agent through entry barriers, but the agent derives real coordination value.
constraint_indexing:constraint_classification(market_concentration_feedback_loops, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANTITRUST REGULATOR (TANGLED ROPE) — Enforces coordination through merger review and conduct rules, but faces genuine trade-offs: some consolidation enables R&D efficiency and global competitiveness. Constrained by budget limits, technical expertise gaps, and political pressure. Benefits from cooperative relationships with incumbents (information access, compliance incentives) while bearing costs of enforcement labor and political backlash. Mixed coordination and extraction.
constraint_indexing:constraint_classification(market_concentration_feedback_loops, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANTITRUST DOCTRINE FRAMEWORK (PITON) — Consumer welfare standard, efficiency defenses, and rule-of-reason analysis have become largely performative. Incumbents use consumer welfare arguments to justify concentration while avoiding scrutiny of dynamic competition effects. The doctrinal apparatus persists through institutional inertia despite theoretical and empirical challenges to its prescriptive power. Theater ratio high: economic reasoning serves legitimation function more than constraint function.
constraint_indexing:constraint_classification(market_concentration_feedback_loops, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SCALE ECONOMY VIEW (MOUNTAIN) — From a universal economic-theory perspective, positive feedback loops are inherent to markets with increasing returns to scale. Larger firms reduce unit costs, which enables lower prices, which attracts customers, which increases scale — creating a self-reinforcing dynamic. This perspective naturalizes concentration as an immutable consequence of technology and economics. However, structural data reveals this as a false summit: concentration's magnitude and rate depend on contingent institutional choices (IP regime, merger enforcement, platform neutrality rules), not on scale economies alone.
constraint_indexing:constraint_classification(market_concentration_feedback_loops, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: FINANCIAL INTERMEDIARY (TANGLED ROPE) — Venture capital and private equity investors benefit from consolidation: they have exit optionality through acquisition channels and portfolio diversification. But they also face genuine coordination challenges in funding breakthrough innovation and managing founder incentives. Mobile at global scale, but constrained by regulatory scrutiny and reputational risk. Mixed but asymmetric — benefits exceed costs for capital holders.
constraint_indexing:constraint_classification(market_concentration_feedback_loops, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_concentration_feedback_loops_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(market_concentration_feedback_loops, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(market_concentration_feedback_loops, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_concentration_feedback_loops, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(market_concentration_feedback_loops, TR),
    TR >= 0.70.

:- end_tests(market_concentration_feedback_loops_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over the measurement interval. The trajectory (0.35 → 0.58) reflects acceleration of feedback loops. Initial consolidation may be justified by scale economies or global competitiveness. But the acceleration beyond technological requirements suggests institutional factors (weaker enforcement, IP strengthening, platform immunity) amplifying extraction. Suppression (0.65): High. Entry barriers include capital requirements ($1B+ for competitive platforms in cloud, semiconductors, telecommunications), network effects (lock-in to dominant platforms), switching costs (data migration, application dependencies), and incumbent acquisition of competitive threats (over 400 acquisitions annually by tech incumbents). These are not surmountable obstacles — they are system-level barriers that scale with incumbent advantage. Theater ratio (0.48): Moderate. Antitrust doctrine has substantial performative elements (efficiency defenses, consumer welfare standard) but retains some genuine constraint force (merger blocking, conduct remedies). The ratio increases over time as doctrine accommodates consolidation while maintaining legitimacy through economic reasoning.
 *
 * PERSPECTIVAL GAP:
 *   Incumbent firms see the constraint as coordination (rope): they solve real problems (platform stability, standards, ecosystem governance). New entrants see it as pure extraction (snare): they face impassable barriers. Consumers see it as mixed constraint and extraction (tangled rope graduating to snare): they benefit from ecosystem stability but lose choice and face lock-in. Regulators see it as a constrained problem to manage (tangled rope): they recognize trade-offs but lack tools to address dynamic effects. The doctrine sees its own function as degraded (piton): economic arguments legitimize consolidation through performative reasoning. The analytical observer at universal scale risks naturalizing contingent arrangements as inevitable (false mountain).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position. Incumbent firms benefit from market power and barriers — low d (0.15-0.20). New entrants face barriers and acquisition threat — high d (0.90+). Consumers face lock-in and reduced choice — high d (0.85+). Regulators experience mixed: they benefit from incumbent cooperation (information, compliance) but bear enforcement costs and political pressure — moderate d (0.50-0.60). Financial intermediaries benefit from acquisition exits but face regulatory constraints — low-moderate d (0.35-0.45). The asymmetry in directionality is the core extraction mechanism: the constraint channels costs toward high-d agents (powerless new entrants, consumers) and benefits toward low-d agents (incumbents, financiers). The feedback loop amplifies this asymmetry over time.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STRUCTURE: The constraint avoids mandate collapse by resolving the tangled_rope ambiguity through perspectival differentiation. From the incumbent's view, the constraint is coordination (genuine network effects, scale economies). From the new entrant's view, it is extraction (barriers that prevent viable competition). From the consumer's view, it is mixed (ecosystem benefits traded for choice restriction). The mandatrophy is resolved not by choosing a single type but by mapping the constraint family across the observation site: the same structural phenomena (network effects, scale economies, capital requirements) appear as coordination from positions of incumbency and as extraction from positions of exclusion. The theater ratio increase (0.32 → 0.48) indicates doctrinal legitimation drifting upward — efficiency arguments doing more work than constraint work — suggesting piton degradation. If theater ratio reaches 0.70+, reclassification to piton becomes warranted, indicating the doctrine has become primarily legitimation apparatus rather than functional constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dynamic_efficiency_trade_off,
    'Do efficiency gains from consolidation (R&D investment, network effects, global scale) outweigh allocative efficiency losses from reduced competition?',
    'Longitudinal productivity analysis of concentrated vs competitive sectors; correlation between market concentration and innovation metrics (patent quality, time-to-market, fundamental breakthroughs); consumer surplus decomposition (price vs choice set vs quality changes)',
    'If dynamic efficiency dominates: concentration is welfare-positive (rope predominates from multiple perspectives). If allocative efficiency dominates: concentration is welfare-negative (snare predominates). Mandatrophy hinges on this empirical fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dynamic_efficiency_trade_off, empirical, 'Trade-off between dynamic efficiency gains and allocative efficiency losses').

omega_variable(
    feedback_loop_acceleration,
    'What institutional factors determine the speed at which concentration feedback loops intensify — IP enforcement strength, acquisition barriers, data accumulation rates, platform neutrality rules?',
    'Comparative institutional analysis across sectors with varying legal regimes; measurement of feedback loop kinetics (time to market dominance, rate of price increases post-consolidation); identification of regulatory interventions that interrupt loops',
    'If institutional factors are dominant: concentration is contingent and policy-malleable (tangled_rope with sunset possibilities). If technological factors dominate: feedback loops are quasi-inevitable (mountain or immutable snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feedback_loop_acceleration, empirical, 'Role of institutional factors in determining feedback loop speed').

omega_variable(
    entry_barrier_reversibility,
    'Are entry barriers created by concentration (network effects, switching costs, data advantages) reversible through technology disruption or policy intervention, or are they durably locked in?',
    'Historical case studies of market disruption across sectors (mobile phones disrupting telecommunications, cloud computing disrupting enterprise software); identification of conditions under which new entrants overcome network effects; measurement of barrier durability post-disruption',
    'If barriers are reversible: powerless agents have latent exit options (scaffold or temporary snare). If barriers are durable: powerless agents are permanently trapped (permanent snare). Classification trajectory depends on reversibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entry_barrier_reversibility, empirical, 'Whether entry barriers are reversible or durably locked in').

omega_variable(
    regulator_capture_ambiguity,
    'Is the antitrust regulator''s constrained exit the result of genuine trade-offs (some consolidation is welfare-positive) or regulatory capture (incumbents have coopted the enforcement apparatus)?',
    'Analysis of enforcement discretion: case selection patterns, settlement terms, lobbying influence; comparison with other regulatory domains; measurement of revolving-door effects (regulator-to-incumbent movement); international regulator comparison (EU vs US stringency differences)',
    'If genuine trade-offs: regulator''s tangled_rope classification stands (constrained by real limits). If capture: regulator should be reclassified as institutional victim or beneficiary accomplice (identity_locked or mobile with high d).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulator_capture_ambiguity, empirical, 'Whether regulator is constrained by genuine trade-offs or regulatory capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_concentration_feedback_loops, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mconc_tr_t0, market_concentration_feedback_loops, theater_ratio, 0, 0.32).
narrative_ontology:measurement(mconc_tr_t5, market_concentration_feedback_loops, theater_ratio, 5, 0.4).
narrative_ontology:measurement(mconc_tr_t10, market_concentration_feedback_loops, theater_ratio, 10, 0.48).
narrative_ontology:measurement(mconc_tr_t15, market_concentration_feedback_loops, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(mconc_be_t0, market_concentration_feedback_loops, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mconc_be_t5, market_concentration_feedback_loops, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mconc_be_t10, market_concentration_feedback_loops, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(mconc_be_t15, market_concentration_feedback_loops, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_concentration_feedback_loops, resource_allocation).
narrative_ontology:affects_constraint(market_concentration_feedback_loops, platform_gatekeeping).
narrative_ontology:affects_constraint(market_concentration_feedback_loops, data_moat_accumulation).
narrative_ontology:affects_constraint(market_concentration_feedback_loops, venture_capital_acquisition_channel).

% DUAL FORMULATION NOTE:
% Market concentration feedback loops is the parent constraint affecting downstream institutional mechanisms. Platform gatekeeping represents the coordination function that generates entry barriers; data moat accumulation represents the feedback mechanism that entrenches market power; venture capital acquisition channel represents the exit route that absorbs competitive threats. Each downstream constraint has distinct ε values reflecting different coordination vs extraction ratios.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_concentration_feedback_loops, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
