% ============================================================================
% CONSTRAINT STORY: gravitational_wave_source_localization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gravitational_wave_source_localization, []).

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
 *   constraint_id: gravitational_wave_source_localization
 *   human_readable: Gravitational Wave Source Localization Coordination
 *   domain: observational_astrophysics/multi_messenger_astronomy
 *
 * SUMMARY:
 *   Gravitational wave source localization creates a multi-dimensional
 *   coordination problem that exhibits genuine collective action benefits
 *   alongside structural extraction. When LIGO, Virgo, and KAGRA detect a
 *   gravitational wave, rapid sky-position determination requires rapid
 *   follow-up observations across the electromagnetic spectrum — radio,
 *   optical, infrared, X-ray. No single observatory can perform this
 *   coordinated response alone. The constraint mechanism is real: distributed
 *   observations across Earth improve angular resolution through
 *   triangulation. However, the coordination requirement creates an
 *   institutional bottleneck where major detector collaborations control
 *   alert distribution and priority-setting for follow-up resources. Smaller
 *   observatories and developing-nation facilities experience this as an
 *   extractive constraint: they must conform to externally-set protocols in
 *   real-time or be excluded. Simultaneously, open alert infrastructure
 *   (VOEvent, GCN, Skyalert) is building alternative coordination pathways
 *   that decentralize alert distribution and reduce the gatekeeper role of
 *   detector collaborations. The constraint exhibits a tractable sunset: as
 *   distributed alert systems mature and computational localization improves,
 *   the coordinative bottleneck decreases. Theater ratio (0.55) reflects that
 *   institutional coordination protocols (multi-wavelength trigger decisions,
 *   observation scheduling, data-sharing agreements) contain significant
 *   performative elements — the genuine coordination could be achieved with
 *   simpler infrastructure, but institutional structures persist.
 *
 * KEY AGENTS:
 *   - Gravitational Wave Detector Networks (LIGO/Virgo/KAGRA): Primary institutional beneficiary (institutional/arbitrage) — controls alert distribution and real-time observation prioritization; benefits from distributed follow-up data
 *   - Electromagnetic Follow-up Observatories: Distributed beneficiary (institutional/arbitrage) — receive early gravitational wave alerts enabling high-value transient observations; benefits from multi-messenger data fusion
 *   - Resource-Constrained Observatories: Primary victim (powerless/trapped) — small telescopes in developing nations cannot independently localize sources; must coordinate in real-time or miss transients; no exit option
 *   - Mid-Tier National Observatories: Secondary victim (moderate/constrained) — have sufficient aperture to contribute but face latency and protocol conformance barriers; experience mixed coordination and extraction
 *   - Open Alert System Coalition: Organized alternatives (organized/constrained) — VOEvent, GCN, Skyalert working to decouple alert distribution from detector collaboration control; building sunset pathways
 *   - Official Notification Systems: Degraded institutional actor (institutional/arbitrage) — GCN email system persists through inertia; theater-high but still operationally relevant during legacy period
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gravitational_wave_source_localization, 0.32).
domain_priors:suppression_score(gravitational_wave_source_localization, 0.38).
domain_priors:theater_ratio(gravitational_wave_source_localization, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gravitational_wave_source_localization, extractiveness, 0.32).
narrative_ontology:constraint_metric(gravitational_wave_source_localization, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gravitational_wave_source_localization, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gravitational_wave_source_localization, tangled_rope).
narrative_ontology:human_readable(gravitational_wave_source_localization, "Gravitational Wave Source Localization Coordination").
narrative_ontology:topic_domain(gravitational_wave_source_localization, "observational_astrophysics/multi_messenger_astronomy").

domain_priors:requires_active_enforcement(gravitational_wave_source_localization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gravitational_wave_source_localization, gravitational_wave_detector_networks).
narrative_ontology:constraint_beneficiary(gravitational_wave_source_localization, electromagnetic_follow_up_resources).
narrative_ontology:constraint_victim(gravitational_wave_source_localization, individual_observatory_autonomy).
narrative_ontology:constraint_victim(gravitational_wave_source_localization, developing_nation_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE-CONSTRAINED OBSERVATORY (SNARE) — Small telescopes and observatories in developing nations face a structural trap. When gravitational wave alerts arrive, they must coordinate immediately with the global follow-up network or miss the transient entirely. However, their small aperture and observing capacity mean they cannot independently localize sources. They are trapped in a real-time coordination mechanism where larger, well-funded observatories set priorities. No exit option exists without abandoning transient science altogether. Maximum experienced extraction.
constraint_indexing:constraint_classification(gravitational_wave_source_localization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER NATIONAL OBSERVATORY (TANGLED ROPE) — Has sufficient aperture to contribute meaningful follow-up observations but lacks the computational resources or trigger-response infrastructure of major detector networks. Constrained by latency requirements and protocol standards. Benefits from shared data pipelines and collaborative access to gravitational wave localizations. Experiences both coordination function (accessing early alerts enables follow-up science) and extraction (must conform to externally-set protocols and observation schedules).
constraint_indexing:constraint_classification(gravitational_wave_source_localization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: GRAVITATIONAL WAVE DETECTOR NETWORK (ROPE) — LIGO, Virgo, KAGRA benefit from distributed follow-up observations that improve source localization. The detector network broadcasts alerts and coordinates response, solving a genuine collective action problem: no individual observatory can localize sources independently; distributed networks dramatically improve sky position accuracy. Net beneficiary through multi-messenger data fusion. Low extraction experienced because the coordination is genuinely mutual.
constraint_indexing:constraint_classification(gravitational_wave_source_localization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN TRANSIENT ALERT SYSTEM COALITION (SCAFFOLD) — Organized initiatives (VOEvent standard, Skyalert, RIFT, GCN) create decoupled alert protocols with sunset logic. These systems aim to replace proprietary coordinator roles with open infrastructure. Sees the current localization bottleneck as a temporary coordination failure being solved by distributed alert systems and open data APIs. Has both agency and a clear exit path as standardized protocols mature.
constraint_indexing:constraint_classification(gravitational_wave_source_localization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OFFICIAL CIRCULAR SYSTEM (PITON) — The Gamma-ray Burst Coordinates Network (GCN), originally designed for gamma-ray satellites, now carries gravitational wave alerts via email distribution. This 30-year-old system persists through institutional inertia despite having been superseded by modern alert infrastructure (VOEvent streams, APIs, streaming databases). The theater is high: email-based notification is now largely performative given that computer-to-computer systems handle the actual alert distribution. Theater ratio reflects degradation of function while institutional form persists.
constraint_indexing:constraint_classification(gravitational_wave_source_localization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL LIMIT VIEW (MOUNTAIN) — From a universal scope, gravitational wave source localization faces a fundamental physical constraint: the detector network's angular resolution is set by the wavelength and baseline length. Improving localization requires either larger wavelengths (impossible — GW frequencies are set by source physics) or larger baselines (requires more detectors across Earth). This perspective frames the bottleneck as a natural law of wave physics. However, the structural data reveals this as a false summit — the real constraint is not physics but the coordination mechanism for allocating follow-up resources among distributed observatories.
constraint_indexing:constraint_classification(gravitational_wave_source_localization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gravitational_wave_source_localization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gravitational_wave_source_localization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gravitational_wave_source_localization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(gravitational_wave_source_localization, TR),
    TR >= 0.70.

:- end_tests(gravitational_wave_source_localization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate-low. The constraint involves genuine coordination benefits: distributed follow-up observations measurably improve source localization and multi-messenger science. The 'extraction' component reflects asymmetric priority-setting (major observatories get first alert notification and are preferentially included in joint proposals) rather than pure rent-seeking. The intermediate value reflects that extraction is real but not the constraint's primary function. Suppression (0.38): Moderate. Technical and institutional barriers exist — real-time coordination requires latency < 10 seconds, protocol conformance is mandatory, major observatories control early alert access. But suppression is not total: alternatives exist (external alert systems, post-hoc electromagnetic searches, independent gravitational wave analysis). The value reflects meaningful but surmountable barriers. Theater ratio (0.55): Moderate-high. The constraint has genuine function (coordinating distributed follow-up) but institutional form is partially divorced from function. Multi-wavelength trigger decisions involve committee processes that could be automated. Data-sharing agreements are more about formal recognition than technical necessity. Email-based alerts persist alongside modern APIs. Theater has increased over the measurement interval as computerized alert systems have outpaced institutional decision-making.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence. The gravitational wave detector networks see a coordination mechanism (Rope) — they genuinely solve the collective action problem of alert distribution and multi-messenger follow-up. Electromagnetic observatories see a mixed system (Tangled Rope) — real coordination benefits alongside extraction through priority asymmetries. Mid-tier observatories see a constrained system (Tangled Rope) with more weight on extraction. Resource-constrained observatories see a pure extraction trap (Snare) — no exit, no alternatives, must conform to external protocols. The open alert coalition sees a sunset scenario (Scaffold) — coordinated alert infrastructure is building alternatives that will reduce the gatekeeper role. The degraded GCN sees a piton perspective — email notification persists through institutional inertia despite being technologically superseded. The analytical observer risks seeing a physical law (Mountain) but the structural data reveals this as naturalization of an institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (detector networks, electromagnetic observatories) experience low directionality values (d ≈ 0.15-0.25) because they hold arbitrage options — they can access follow-up data through multiple pathways, negotiate priority, or leverage computational resources. Victims (resource-constrained observatories) experience high directionality (d ≈ 0.85-0.95) because they are trapped: they have no alternative coordination mechanism and must accept the detector networks' alerting decisions to participate in transient science. Mid-tier observatories with constrained exit experience intermediate directionality (d ≈ 0.55-0.65). The sigmoid f(d) amplifies the extraction experienced by powerless agents while dampening the cost to beneficiaries. Scope is global (σ = 1.2), slightly amplifying extractiveness due to the difficulty of coordinating verification across geographically dispersed observatories.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION STRUCTURE: The mandatrophy resolves through the ε-invariance principle. The constraint is legitimately a Tangled Rope: it performs genuine coordination (multi-messenger follow-up requires distributed observations) while simultaneously enabling extraction (priority asymmetries concentrate early alerts and follow-up access in major detector collaborations). The false summit (mountain perspective) naturalizes this hybrid as 'physics necessitates centralization' when the real constraint is institutional (alert protocols, priority committees, data-sharing agreements). The scaffold perspective is structurally real — open alert systems are demonstrably reducing the gatekeeper role of detector collaborations, with measurable sunset dynamics. The snare perspective is not over-claim but rather legitimate perception from excluded agents: when you have no alternatives and must conform to externally-set protocols, the system appears extractive regardless of whether coordination benefits exist (which they do, but are distributed to others). The rope perspective (detector networks) is also legitimate — they do solve a coordination problem. The constraint enables all six valid classifications from different structural positions without contradiction. This demonstrates the framework's diagnostic power: it reveals that 'gravitational wave localization is just physics' is a false simplification that obscures institutional extraction from powerless agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alert_latency_threshold,
    'What alert latency threshold distinguishes genuine coordination from extractive delay?',
    'Measurement of follow-up success rates vs alert latency; correlation between network latency and transient detection efficiency',
    'If threshold < 10 seconds: many legitimate coordination costs misclassified as extraction. If threshold > 60 seconds: extractive gatekeeping persists undetected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alert_latency_threshold, empirical, 'Alert latency threshold for distinguishing coordination from extraction').

omega_variable(
    localization_accuracy_floor,
    'Does the current multi-detector array represent a fundamental physical floor for source localization, or does it reflect underfunded infrastructure in developing regions?',
    'Simulation of counterfactual detector network configurations; assessment of what localization could be achieved with distributed ground-based detectors in southern hemisphere and tropics',
    'If physical floor: resource disparity is unavoidable (Rope persists). If infrastructure-determined: current disparity is extractive choice (Snare for excluded observatories intensifies).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(localization_accuracy_floor, empirical, 'Whether localization floor is physical or infrastructure-determined').

omega_variable(
    proprietary_follow_up_data_flow,
    'What fraction of successful electromagnetic follow-ups remain proprietary within detector collaboration networks vs openly available?',
    'Audit of GW170817-era follow-up data; tracking of proprietary vs open publication timelines for electromagnetic counterparts',
    'If mostly open: coordination is genuine (Rope dominates). If mostly proprietary: extraction concentrates in detector collaborations (Snare/Tangled Rope dynamics intensify).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_follow_up_data_flow, empirical, 'Proprietary vs open electromagnetic follow-up data flows').

omega_variable(
    computational_bottleneck_location,
    'Is the primary localization bottleneck computational (processing detector data to produce sky maps) or coordinative (directing follow-up observations)?',
    'Timing analysis of each pipeline stage; measurement of latency contributions from detector networks vs alert distribution vs follow-up planning',
    'If computational: problem is technical (infrastructure investment reduces extraction). If coordinative: problem is institutional (requires protocol change, not hardware).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_bottleneck_location, empirical, 'Whether bottleneck is computational or coordinative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gravitational_wave_source_localization, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gwsl_tr_t0, gravitational_wave_source_localization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gwsl_tr_t3, gravitational_wave_source_localization, theater_ratio, 3, 0.48).
narrative_ontology:measurement(gwsl_tr_t6, gravitational_wave_source_localization, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(gwsl_be_t0, gravitational_wave_source_localization, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(gwsl_be_t3, gravitational_wave_source_localization, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(gwsl_be_t6, gravitational_wave_source_localization, base_extractiveness, 6, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gravitational_wave_source_localization, information_standard).
narrative_ontology:affects_constraint(gravitational_wave_source_localization, electromagnetic_survey_coordination).
narrative_ontology:affects_constraint(gravitational_wave_source_localization, multi_messenger_astronomy_infrastructure).

% DUAL FORMULATION NOTE:
% Gravitational wave source localization decomposes into two structurally distinct constraints: (1) physical/computational localization (accuracy determined by detector network baselines and analysis algorithms) and (2) institutional alert distribution and follow-up resource allocation. This story addresses the second. The first would have lower extractiveness (0.08-0.15, primarily Mountain) and represents genuine physical limits. They are linked because institutional constraints cite physical limits as justification — false summit detection requires decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gravitational_wave_source_localization, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
