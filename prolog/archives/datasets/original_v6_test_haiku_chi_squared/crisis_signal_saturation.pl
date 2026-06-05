% ============================================================================
% CONSTRAINT STORY: crisis_signal_saturation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   Crisis signal saturation represents a structural transition in
 *   information ecosystems where the coordination mechanism for genuine
 *   emergencies (shared alert protocols) becomes self-defeating through
 *   overuse and institutional misalignment. The constraint emerges from the
 *   interaction of three structural forces: (1) legitimate increase in
 *   real-time risk communication needs (pandemic tracking, climate hazards,
 *   market volatility, geopolitical escalation), (2) institutional incentive
 *   structures that reward alert frequency and user engagement metrics over
 *   signal accuracy, and (3) human neurophysiology that habituates to
 *   constant threat cues, converting baseline urgency into background noise.
 *   This creates a paradox: the more alerts are issued, the less effective
 *   each alert becomes, yet the institutional operators continue amplifying
 *   alert volume to maintain engagement. The constraint is not a pure
 *   coordination problem (which would be Rope) because the alert system
 *   benefits a subset of actors (platform operators, attention-capture
 *   industries, institutional actors whose budgets depend on demonstrated
 *   emergency preparedness) while extracting costs from the mass audience and
 *   genuine emergency responders. Simultaneously, it is not pure extraction
 *   (which would be Snare) because the underlying signal infrastructure does
 *   perform real coordination work — the challenge is recovering signal
 *   integrity from within a degraded ecosystem. This makes it structurally a
 *   Tangled Rope: genuine coordination function (emergency alert) coupled
 *   with asymmetric extraction (attention capture, institutional performance
 *   metrics). The theater ratio (0.65) reflects that much of the alert volume
 *   is performative: alerts issued to justify institutional presence rather
 *   than to communicate genuine hazards. The extractiveness has risen over
 *   the interval (0.28 → 0.52) as alert fatigue has deepened and
 *   institutional operators have doubled down on frequency to maintain
 *   engagement against declining marginal response. Theater has similarly
 *   increased (0.38 → 0.65) as the gap between alert frequency and actual
 *   threat incidence has widened. Signal recovery represents a scaffold
 *   perspective: organized technical and social protocols (signal
 *   rarity-weighting, graduated urgency, attention budget allocation, curated
 *   briefing services) can restore alert meaning without eliminating
 *   emergency capability, with a sunset horizon of 10-20 years as norms
 *   mature.
 *
 * KEY AGENTS:
 *   - Habituated Receiver: Primary victim (powerless/trapped) — mass population in perpetual alert stream; cannot exit without abandoning genuine emergency warning capability; nervous system habituates to baseline crisis
 *   - Alert Platform Operators: Primary beneficiary (institutional/arbitrage) — capture attention and engagement metrics; benefit from alert frequency; can exit by reducing alerts but business model incentivizes maximization
 *   - Emergency Response Community: Secondary victim (moderate/constrained) — fire, hospital, public health agencies need alert signals but suffer degraded decision-making and response reliability under saturation conditions; also pressured to amplify alerts to justify institutional budgets
 *   - Attention-Capture Industries: Secondary beneficiary (institutional/arbitrage) — news media, social networks, advertising platforms profit from crisis engagement; reinforces alert frequency through engagement loops
 *   - Disengagement Movement: Organized resistance (organized/mobile) — digital minimalism communities, slow news networks, offline collectives building alternative information systems with intentional time horizons and signal filtering
 *   - Legacy Warning Infrastructure: Piton actor (institutional/arbitrage) — traditional emergency broadcast systems (sirens, radio alerts) maintain performative function while real coordination work happens in specialized networks
 *   - Analytical Observer: Signal recovery framework (analytical/analytical) — civilizational perspective on information ecosystem redesign; identifies structural pathways to restore alert meaning through graduated urgency protocols and attention budget allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crisis_signal_saturation, 0.52).
domain_priors:suppression_score(crisis_signal_saturation, 0.68).
domain_priors:theater_ratio(crisis_signal_saturation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crisis_signal_saturation, extractiveness, 0.52).
narrative_ontology:constraint_metric(crisis_signal_saturation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(crisis_signal_saturation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crisis_signal_saturation, tangled_rope).
narrative_ontology:human_readable(crisis_signal_saturation, "The Perpetual Alarm Fatigue").
narrative_ontology:topic_domain(crisis_signal_saturation, "informational/psychological/sociological").

domain_priors:requires_active_enforcement(crisis_signal_saturation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(crisis_signal_saturation, alert_system_operators).
narrative_ontology:constraint_beneficiary(crisis_signal_saturation, risk_aggregation_platforms).
narrative_ontology:constraint_beneficiary(crisis_signal_saturation, attention_capture_industries).
narrative_ontology:constraint_victim(crisis_signal_saturation, signal_integrity).
narrative_ontology:constraint_victim(crisis_signal_saturation, cognitive_capacity).
narrative_ontology:constraint_victim(crisis_signal_saturation, genuine_emergency_response).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HABITUATED RECEIVER (SNARE) — Individual subject trapped in perpetual alert stream. Cannot exit the alert ecosystem without abandoning genuine emergency warning capability. Nervous system habituates to baseline crisis. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.87. High effective extraction: subject pays attention cost with no meaningful return.
constraint_indexing:constraint_classification(crisis_signal_saturation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGENCY RESPONSE COMMUNITY (TANGLED ROPE) — Fire departments, hospitals, public health agencies need alert signals to coordinate genuine emergencies. But signal saturation degrades their own response reliability: dispatchers suffer alert fatigue; decision-making slows when all threats appear equally urgent. Mixed coordination (signal sharing for real emergencies) and extraction (performance metrics drive false-positive amplification). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55. Requires active enforcement: alert protocols maintain artificial urgency to justify institutional budgets.
constraint_indexing:constraint_classification(crisis_signal_saturation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALERT PLATFORM OPERATOR (ROPE) — Experiences constraint as pure coordination: aggregating diverse signals (weather, market, health, geopolitical) into a unified stream enables users to stay informed. Extraction benefit comes from attention capture and engagement metrics. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Net beneficiary. Can exit by reducing alert frequency (arbitrage), but chooses not to: business model depends on keeping users in active monitoring state.
constraint_indexing:constraint_classification(crisis_signal_saturation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISENGAGEMENT MOVEMENT (TANGLED ROPE) — Organized users (digital minimalism communities, slow news networks, offline collectives) see alert saturation as extraction mechanism and respond by building alternative systems: curated briefing services, news-free communities, intentional communication protocols with longer time horizons. Mixed coordination (functioning alternative pathways) and extraction resistance (low-intensity information diet requires active enforcement of norms). d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.18. Mobile exit options available but require coordination cost; organized actors can access them.
constraint_indexing:constraint_classification(crisis_signal_saturation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY WARNING INFRASTRUCTURE (PITON) — Traditional emergency broadcast systems (sirens, radio alerts, tornado warnings) persist as performative ritual despite degraded function. The real coordination work happens silently in specialized networks (hospital alerts, trading floors, meteorological agencies); the public broadcast ritual maintains theater to preserve institutional legitimacy. theater_ratio=0.65 indicates significant performative content. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.06. Institutional beneficiary but system's primary function (urgent warning transmission) is atrophied by saturation.
constraint_indexing:constraint_classification(crisis_signal_saturation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL SIGNAL RECOVERY (SCAFFOLD) — Civilizational view of information ecosystem redesign. New protocols (signal rarity-weighting, graduated urgency levels, attention budget allocation, signal-to-noise ratio feedback) aim to restore meaning to alerts without losing genuine emergency capability. This represents a temporary coordination problem with a sunset: if signal recovery protocols mature, alert fatigue becomes a historical artifact. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.40. Sunset logic: as protocols mature (10-20 years), the constraint's extraction mechanism weakens.
constraint_indexing:constraint_classification(crisis_signal_saturation, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.52): High-moderate, reflecting the asymmetry between institutional beneficiaries (operators, attention-capture industries) and mass population (receivers bearing attention cost). The value is not higher (≥0.66 for snare) because genuine coordination function persists: alerts do sometimes communicate real emergencies, and disengagement carries genuine safety cost. The constraint is hybrid extraction-coordination, not pure extraction. Suppression (0.68): High. Barriers to individual exit include: (a) genuine risk of missing actual emergencies (institutional lock-in), (b) social pressure to stay informed, (c) infrastructure ubiquity making disengagement costly, (d) fragmentation of alternative alert systems (no unified alternative yet). Suppression rises over time as alert systems become more embedded in daily life. Theater ratio (0.65): Moderate-high. A substantial portion of alert volume is performative: issued to justify institutional budgets, maintain user engagement against declining novelty, or serve as backdrop for other communications. This is lower than Piton theater (≥0.70) because signal infrastructure does perform genuine function; it is not purely ritualistic. But theater has increased over the interval (0.38 → 0.65) as the signal-to-threat ratio has degraded. Claimed type (Tangled Rope): Justified by (a) beneficiary declaration (platform operators, attention industries) indicating coordination benefit, (b) victim declaration (signal integrity, genuine emergency response) indicating asymmetric extraction, (c) requires_active_enforcement=true because institutional operators actively maintain alert frequency to sustain extraction despite declining signal utility. The constraint is not locked as Snare because organized alternatives (disengagement movement) have achieved mobile exit, and scaffold logic suggests feasible protocol redesign. The constraint is not pure Rope because the extraction costs to signal integrity and emergency response reliability are substantial.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between institutional operators (who experience Rope or beneficiary perspective) and mass receivers (who experience Snare or victim perspective). The operator sees coordination work being done: signals aggregated, platforms built, users informed. The receiver sees extraction: constant attention demand with declining marginal relevance, habituation to emergency as baseline, degraded ability to distinguish genuine from performative alerts. The emergency responder (Tangled Rope perspective) sees both: alerts enable coordination of genuine responses, but saturation degrades decision-making and creates perverse incentive to amplify alerts to justify preparedness. The analytical observer (Scaffold perspective) recognizes the constraint as a temporary coordination problem resolvable through protocol redesign: graduated urgency levels, signal rarity-weighting, attention budgeting. This perspective is not yet accessible to powerless receivers or institutional operators locked in engagement optimization loops. The Piton perspective (legacy warning systems) observes degraded function masked by performative ritual — traditional sirens and broadcasts persist not because they effectively warn (modern alerts bypass them) but because they maintain institutional legitimacy and ceremonial presence. The disengagement movement (organized actors with mobile exit) shows that alternative coordination is feasible but requires sustained coordination cost to maintain information diets while staying connected enough to genuine emergencies. The perspectival gap is not resolvable by any single perspective: the constraint's structure requires simultaneous protocol redesign (scaffold), institutional incentive realignment (beneficiary perspective), emergency response capability preservation (moderate perspective), and mass participation in signal filtering (powerless perspective).
 *
 * DIRECTIONALITY LOGIC:
 *   Alert Platform Operators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Negative effective extraction because institutional operators benefit from the status quo and can exit by reducing alert frequency (arbitrage). Yet they choose not to exit because engagement metrics incentivize maximization. This represents captured arbitrage: they technically have exit options but use them to amplify extraction rather than reduce it. Emergency Response Community: Victim + constrained → d≈0.68, f(d)≈1.05. Moderate extraction. They suffer from alert saturation (degraded decision-making, slower responses under noise) but also depend on alert systems for genuine emergencies. Their constrained exit (cannot fully disengage without abandoning emergency capability) results in d roughly 2/3 toward victimhood. Habituated Receivers: Victim + trapped → d≈0.92, f(d)≈1.38. High extraction. Individual receivers cannot exit without accepting genuine emergency risk. Trapped exit combined with victim status produces high directionality toward extraction. Disengagement Movement: Organized + mobile → d≈0.35, f(d)≈0.35. Low-moderate extraction. Organized actors have discovered mobile exit paths (curated briefing services, signal-filtered networks, intentional communication protocols) and can implement them at population scale if norms shift. Their mobile exit option prevents high d, but active coordination cost to maintain alternative systems prevents d from approaching 0. Signal Integrity and Genuine Emergency Response: Abstract victims (treated as beneficiary group in victimhood frame) → d≈0.95, f(d)≈1.42. Maximum extraction. These abstract collective goods cannot organize, cannot exit, and are directly degraded by constraint operation. Signal integrity is contaminated by false positives; genuine emergency response is impaired by noise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    signal_baseline_definition,
    'How is ''baseline crisis'' structurally defined — as an absolute frequency threshold, as a relative increase from historical norms, or as a mismatch between signal frequency and actual threat incidence?',
    'Comparative analysis of alert frequency vs actual emergency incidence rates; psychological measurement of habituation baseline; historical data on signal-to-threat ratio across decades',
    'If absolute threshold: constraint is contingent on technology (can be engineered away). If relative increase: constraint reflects genuine acceleration of threat communication. If mismatch: constraint reveals systematic bias in alert generation incentives (most likely).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signal_baseline_definition, empirical, 'Definition of baseline crisis state for habituation measurement').

omega_variable(
    genuine_emergency_preservation,
    'Can signal filtering and graduated urgency protocols preserve detection of actual life-threatening emergencies while reducing alert fatigue, or does any reduction in alert frequency inevitably degrade emergency response effectiveness?',
    'Pilot testing of signal rarity-weighting and graduated urgency in emergency response systems; correlation between alert reduction and emergency detection failure rates; analysis of false-negative costs vs false-positive signal fatigue',
    'If feasible: scaffold perspective is structurally sound; signal recovery can have a genuine sunset without sacrificing safety. If not feasible: constraint is locked (tangled rope with no true exit); trade-off between habituation and emergency response is irreducible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genuine_emergency_preservation, empirical, 'Whether emergency response capability survives alert frequency reduction').

omega_variable(
    institutional_incentive_coupling,
    'Is the perpetual alert state driven by genuine risk increase, or primarily by institutional incentive structures that reward alert frequency and engagement metrics?',
    'Decomposition of alert volume by source category (genuine threat increase vs institutional proliferation); analysis of alert operator business models and performance metrics; comparison of threat incidence to alert frequency trends',
    'If genuine risk increase: constraint reflects immutable increase in actual hazards (closer to mountain than snare). If institutional coupling: constraint is extraction mechanism sustained by misaligned incentives (snare/tangled rope); changes to incentive structures could reduce extraction without compromising safety.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_incentive_coupling, empirical, 'Attribution of alert saturation to risk increase vs institutional incentives').

omega_variable(
    collective_action_feasibility,
    'Can disengagement from alert systems be coordinated at population scale, or is individual exit too costly to be stable without institutional change?',
    'Analysis of disengagement movement participation rates; cost-benefit modeling of alert non-engagement; observation of whether low-alert communities maintain equivalent emergency response capability',
    'If collective action is feasible: constraint can be resolved through organized alternative system adoption (scaffold/piton transition). If exit is locked: constraint is structural (snare/tangled rope with trapped population); only top-down signal redesign can break the cycle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_feasibility, empirical, 'Feasibility of population-scale disengagement from alert systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crisis_signal_saturation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crsis_tr_t0, crisis_signal_saturation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(crsis_tr_t10, crisis_signal_saturation, theater_ratio, 10, 0.52).
narrative_ontology:measurement(crsis_tr_t20, crisis_signal_saturation, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(crsis_be_t0, crisis_signal_saturation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(crsis_be_t10, crisis_signal_saturation, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(crsis_be_t20, crisis_signal_saturation, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(crisis_signal_saturation, information_standard).
narrative_ontology:affects_constraint(crisis_signal_saturation, attention_economy_extraction).
narrative_ontology:affects_constraint(crisis_signal_saturation, institutional_legitimacy_performance).

% DUAL FORMULATION NOTE:
% Crisis signal saturation can be decomposed into two related constraints: (1) attention economy extraction (ε≈0.55, institutional incentive misalignment driving alert frequency), (2) signal integrity degradation (ε≈0.65, epistemic cost of false-positive saturation). The current story integrates both via the Tangled Rope classification; the upstream constraint is attention economy coupling; the downstream constraint is behavioral habituation. Signal recovery protocols address the hybrid system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(crisis_signal_saturation, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
