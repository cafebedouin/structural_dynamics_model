% ============================================================================
% CONSTRAINT STORY: crisis_signal_saturation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_crisis_signal_saturation, []).

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
 *   constraint_id: crisis_signal_saturation
 *   human_readable: The Perpetual Alarm Fatigue
 *   domain: informational/psychological/sociological
 *
 * SUMMARY:
 *   Crisis signal saturation represents the degeneration of legitimate
 *   risk-monitoring coordination into perpetual alarm extraction. In the past
 *   15 years, the proliferation of real-time alert systems (emergency
 *   broadcasts, market volatility notifications, pandemic trackers, weather
 *   warnings, security bulletins, platform notifications) has created a
 *   structural condition where civilian populations receive 40-120 crisis
 *   signals daily. Individual nervous systems habituate to this baseline,
 *   rendering urgent warnings indistinguishable from routine noise. The
 *   constraint exhibits tangled rope characteristics: alert systems
 *   coordinate genuine risk information (coordination function) while
 *   simultaneously extracting attention through false positives, algorithmic
 *   amplification, and engagement optimization (extraction function). The
 *   perpetual character — crisis as normalized baseline — is neither a
 *   natural law nor an inevitable feature of risk monitoring. It is a choice
 *   made by alert system operators, platform algorithms, and institutional
 *   risk managers who benefit from high-frequency signaling and bear minimal
 *   cost for false positives. The habituation is real neurobiology, but the
 *   saturation is contingent institutional design.
 *
 * KEY AGENTS:
 *   - Mass Population / Habituated Civilians: Primary victim (powerless/trapped) — bears full attentional extraction cost, cannot opt out without forfeiting genuine emergency awareness
 *   - Alert System Operators: Primary beneficiary and institutional enforcer (institutional/arbitrage) — capture engagement metrics, funding justification, institutional importance; operate the alert generation threshold
 *   - Attention Capture Platforms: Secondary extractor (powerful/arbitrage) — engage algorithmic amplification of crisis content; revenue model incentivizes engagement over accuracy
 *   - Emergency Response Officials: Secondary victim (moderate/constrained) — benefit from alert infrastructure but harmed by false-positive load and loss of public credibility
 *   - Digital Wellness Coalition: Organized actors (organized/mobile) — building filtering and prioritization systems as temporary scaffolding toward AI-driven personalization
 *   - Broadcast Emergency Systems: Institutional degraded actors (institutional/arbitrage) — legacy infrastructure persisting through inertia; function largely migrated to private platforms
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional design as inherent property of human neurobiology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crisis_signal_saturation, 0.52).
domain_priors:suppression_score(crisis_signal_saturation, 0.58).
domain_priors:theater_ratio(crisis_signal_saturation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crisis_signal_saturation, extractiveness, 0.52).
narrative_ontology:constraint_metric(crisis_signal_saturation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(crisis_signal_saturation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crisis_signal_saturation, tangled_rope).
narrative_ontology:human_readable(crisis_signal_saturation, "The Perpetual Alarm Fatigue").
narrative_ontology:topic_domain(crisis_signal_saturation, "informational/psychological/sociological").

domain_priors:requires_active_enforcement(crisis_signal_saturation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(crisis_signal_saturation, alert_system_operators).
narrative_ontology:constraint_beneficiary(crisis_signal_saturation, institutional_risk_managers).
narrative_ontology:constraint_beneficiary(crisis_signal_saturation, attention_capture_platforms).
narrative_ontology:constraint_victim(crisis_signal_saturation, mass_population_attention_reserves).
narrative_ontology:constraint_victim(crisis_signal_saturation, genuine_emergency_responsiveness).
narrative_ontology:constraint_victim(crisis_signal_saturation, collective_decision_fatigue).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HABITUATED CIVILIAN (SNARE) — Receives 40-120 crisis signals daily (market alerts, weather warnings, health updates, security notices). Cannot opt out without sacrificing genuine emergency awareness. Nervous system habituates to crisis baseline; critical signals are ignored alongside noise. Maximum experienced extraction: temporal and attentional resources are extracted without reciprocal benefit. No exit option.
constraint_indexing:constraint_classification(crisis_signal_saturation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGENCY RESPONSE OFFICIAL (TANGLED ROPE) — Benefits from alert infrastructure (genuine emergencies are detected and communicated). But also victim: false-positive saturation reduces response rates, increases costs of verification, degrades trust in their communications. Coordination function (alerts enable response) mixed with asymmetric extraction (false positives impose verification burden). Constrained exit: cannot abandon alerts but could design less aggressive systems.
constraint_indexing:constraint_classification(crisis_signal_saturation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALERT SYSTEM OPERATORS (ROPE) — Primary beneficiary. Extraction runs toward this agent: they capture user attention, engagement metrics, funding justification, and institutional importance. Experience the constraint as coordination: 'We are communicating risk information efficiently.' Arbitrage exit: can reallocate resources, upgrade algorithms, shift to subscription models. Coordination benefit is genuine (alerts do communicate real information) but bundled with extraction (maximum attention capture).
constraint_indexing:constraint_classification(crisis_signal_saturation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL WELLNESS COALITION (SCAFFOLD) — Organized response to alert fatigue: signal filtering, priority-based notification hierarchies, attention-budget standards, scheduled crisis briefings rather than perpetual pings. These interventions are temporary scaffolding with a sunset: as AI-driven personalized alerting matures, false-positive rates will decline and the coordination problem becomes solvable without suppression. Estimated sunset: 8-12 years for deployed personalization in emergency systems.
constraint_indexing:constraint_classification(crisis_signal_saturation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: BROADCAST EMERGENCY SYSTEMS (PITON) — Legacy infrastructure (Emergency Alert System, Weather Radio, National Warning Systems) persists despite attenuation of function. Designed for rare, high-stakes events; now firing constantly. Theater ratio is high: the ritual of broadcasting is performed but not heard. Inertia maintains systems because alternatives (decentralized, algorithm-driven, personalized alerts) are not yet mature enough to fully displace them. The functional crisis is actually routed to private platforms (Twitter/X, Bluesky, TikTok), leaving official systems as theatrical continuity.
constraint_indexing:constraint_classification(crisis_signal_saturation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ATTENTION CAPTURE PLATFORMS (TANGLED ROPE) — Primary extractors. Coordination function: they distribute emergency information at scale (genuine benefit). But asymmetric extraction: engagement algorithms amplify alarming content, priority is engagement over accuracy, false positives are profitable (more engagement, more ad delivery). Active enforcement: algorithmic ranking systems enforce the extraction. Powerful with arbitrage options but depend on user attention; threatened by user defection if fatigue becomes intolerable.
constraint_indexing:constraint_classification(crisis_signal_saturation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — Risk perspective: biological habituation to crisis stimulus is an inherent property of vertebrate nervous systems. Repeated warnings at sub-lethal threat levels trigger habituation. This appears as a natural law ('you cannot change how human attention works'). But the structural data contradicts mountain classification: alert frequency is not biologically determined; it is a choice made by system operators. The habituation is real neurobiology, but the saturation is contingent institutional design. Engine will detect false summit.
constraint_indexing:constraint_classification(crisis_signal_saturation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crisis_signal_saturation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(crisis_signal_saturation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crisis_signal_saturation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(crisis_signal_saturation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(crisis_signal_saturation, TR),
    TR >= 0.70.

:- end_tests(crisis_signal_saturation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts attention, decision-making capacity, and emotional bandwidth from the mass population. The value has risen from 0.28 to 0.52 over the interval as alert density increased (from ~20 daily alerts circa 2014 to 40-120 by 2024). The extraction is not maximal (0.66+) because some legitimate emergency coordination does occur — the structure is hybrid. Suppression (0.58): Moderate-high. Exit barriers include the dependency of genuine emergency awareness on alert systems, the ubiquity of alert networks (opting out of all systems is impractical), and the cognitive cost of evaluating alert validity independently. But suppression is not total (0.60+) because some populations have developed filtering strategies, alert prioritization habits, and skeptical evaluation practices. Theater ratio (0.68): High and rising. The performative element is substantial: many alerts are issued ritualistically (legal compliance), broadcast without verification, aggregated algorithmically, and consumed without action. The trend shows theater_ratio increasing from 0.38 (2014) to 0.68 (2024) as alert density increased, indicating that the ratio of performative to functional alerting has grown. Legacy broadcast systems fire constantly but are largely unheard; private platforms distribute crisis content but with engagement algorithms rather than verification standards.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon — perpetual crisis signaling — can be classified as coordination (rope) from the operator's perspective, extraction (snare) from the civilian's perspective, or a temporary problem being solved (scaffold) from the organized coalition's perspective. The gap reflects genuinely different experienced extractiveness based on power level and exit options. Operators with arbitrage exit experience the constraint as a coordination mechanism that benefits them. Civilians with trapped exit experience it as pure extraction. Officials with constrained exit and mixed beneficiary/victim status experience the hybrid tangled rope. The perspectival divergence is not a measurement problem — it is a reflection of the constraint's actual structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the extraction flow. Alert system operators and platforms are beneficiaries with arbitrage exit options (d ≈ 0.10-0.20), experiencing low or negative χ because they capture attention and profit. Habituated civilians are victims with trapped exit (d ≈ 0.95), experiencing high χ because they cannot opt out and bear full extraction cost. Emergency officials occupy middle ground (d ≈ 0.50-0.65): they are both coordinators (benefit from alert infrastructure) and harmed parties (false positives damage their credibility and operational efficiency). The organized wellness coalition has mobile exit (d ≈ 0.40): they can develop alternative systems and deploy filtering, reducing their experienced extraction. This directionality derivation explains why the same structural constraint appears as rope to operators, snare to civilians, tangled rope to officials and platforms, and scaffold to the organized coalition.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The tangled rope classification resolves the potential confusion between 'crisis signal saturation is coordination infrastructure' (which would suggest rope) and 'it is pure extraction' (snare). The structure combines both: (1) genuine coordination function — alert systems do communicate risk information, enabling response to real emergencies; (2) asymmetric extraction — false positives and algorithmic amplification extract attention disproportionately from mass populations. The tangled rope classification requires both beneficiaries (alert operators, platforms) and victims (habituated populations), active enforcement (algorithmic alert generation and ranking), and extractiveness in the 0.40-0.90 range. All conditions are met. The mandatrophy is resolved by demonstrating that the constraint is neither pure coordination nor pure extraction, but a hybrid where the coordination function is genuine enough to justify the infrastructure while the extraction asymmetry is severe enough to harm decision-making and emergency responsiveness. The scaffold perspective (organized filtering systems) suggests a future where the tangled rope could transition toward rope (coordination dominates) if personalized filtering and AI-driven prioritization mature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    habituation_reversal_threshold,
    'What reduction in alert frequency would restore civilian responsiveness to genuine high-stakes warnings without losing coverage of important-but-non-critical events?',
    'Empirical studies: measure response times and adherence rates for genuine vs false alarms across populations exposed to different alert densities (e.g., cohorts receiving 10, 30, 60, 120 daily alerts); correlate with actual outcomes',
    'If threshold exists below current density (e.g., 20 alerts/day would restore responsiveness): system is reformable via constraint redesign. If threshold is near current density: saturation is structural and alternative architectures (AI filtering, priority hierarchies) are required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(habituation_reversal_threshold, empirical, 'Alert frequency threshold for restoring civilian responsiveness').

omega_variable(
    algorithm_bias_in_false_positives,
    'Are false alerts produced by algorithmic systems that structurally prefer sensitivity (catching rare events) over specificity (avoiding false alarms) — and are these algorithms under operator control?',
    'Audit of alert generation algorithms; measurement of false-positive rates by system; comparison of operator-set thresholds vs recommended defaults; incentive analysis of alert system funding (does it reward volume or accuracy?)',
    'If false positives are algorithmic defaults maintained under operator control: constraint is pure extraction (Snare dominates). If algorithms are poorly understood or defaults override operator intent: constraint appears as tangled rope (coordination + extraction bundled). If operators cannot adjust: constraint appears as mountain (inherent to the technology).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_bias_in_false_positives, empirical, 'Whether false-positive bias in alert algorithms is operator-controllable').

omega_variable(
    genuine_emergency_signal_preservation,
    'Can filtering systems (AI-driven prioritization, attention budgets, scheduled briefings) preserve detection and response to actual high-stakes emergencies while reducing false-positive load?',
    'Deployment trials: measure detection rates and response times for real emergencies in populations using filtered vs unfiltered systems; track outcomes (lives saved, resources deployed correctly) over 2-3 year period',
    'If filtering preserves detection: scaffold sunset is real — filtering systems can replace fatigue-inducing perpetual alerts. If filtering degrades detection (important events filtered as false positives): the constraint is unsolvable via filtering and requires redesign of alert generation itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_emergency_signal_preservation, empirical, 'Whether filtered alert systems preserve genuine emergency detection').

omega_variable(
    platform_incentive_alignment,
    'Do attention-capture platforms'' revenue models (engagement-based advertising) structurally incentivize false-positive generation, or can alignment with accuracy be achieved through regulation or market competition?',
    'Comparative analysis: platforms with different incentive structures (engagement-based vs subscription vs public broadcast); measurement of false-positive rates, alert density, user retention across platforms; regulatory natural experiments (EU Digital Services Act, emerging attention-protection legislation)',
    'If incentives are structurally misaligned: platforms remain primary extractors and regulation is necessary. If alignment can be achieved: tangled rope transitions toward rope (coordination dominates) as platforms compete on accuracy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_incentive_alignment, preference, 'Whether platform incentives can be aligned with alert accuracy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crisis_signal_saturation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(css_tr_t0, crisis_signal_saturation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(css_tr_t5, crisis_signal_saturation, theater_ratio, 5, 0.53).
narrative_ontology:measurement(css_tr_t10, crisis_signal_saturation, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(css_be_t0, crisis_signal_saturation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(css_be_t5, crisis_signal_saturation, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(css_be_t10, crisis_signal_saturation, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(crisis_signal_saturation, information_standard).
narrative_ontology:affects_constraint(crisis_signal_saturation, pandemic_fatigue_compliance).
narrative_ontology:affects_constraint(crisis_signal_saturation, market_signal_flooding).
narrative_ontology:affects_constraint(crisis_signal_saturation, institutional_credibility_erosion).

% DUAL FORMULATION NOTE:
% Crisis signal saturation is downstream of specific alert systems (pandemic trackers, market feeds, emergency broadcasts) and platforms (social media, news aggregators) but represents a distinct structural constraint on the coordination function itself. The upstream constraints concern individual alert systems; this constraint concerns the aggregate effect of signal flooding on population responsiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(crisis_signal_saturation, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
