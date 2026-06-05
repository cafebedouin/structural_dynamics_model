% ============================================================================
% CONSTRAINT STORY: transient_event_detection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transient_event_detection, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: transient_event_detection
 *   human_readable: Automated Transient Event Detection in Astronomical Surveys
 *   domain: astronomical_science/observational_infrastructure
 *
 * SUMMARY:
 *   The automated detection of transient astronomical events — supernovae,
 *   gamma-ray bursts, gravitational wave counterparts, kilonovae, and other
 *   fleeting phenomena — represents a transition from serendipitous discovery
 *   ('happy accidents' of visual inspection) to engineered detection ('tactic
 *   in its own right' via conveyor belt systems). This constraint exhibits
 *   structural tension between the genuine coordination benefit of automated
 *   sky monitoring and the extractive advantage of institutional monopoly
 *   over discovery priority. Large survey institutions (Pan-STARRS, ZTF,
 *   LSST/Vera Rubin Observatory) have deployed massive automated systems that
 *   identify and alert the community to transient candidates. These systems
 *   solve a real collective action problem: monitoring the entire accessible
 *   sky every clear night is impossible for any individual observatory, yet
 *   scientific value flows from rapid discovery and characterization.
 *   However, the same infrastructure creates an asymmetric advantage: survey
 *   institutions know about transients hours before the broader community and
 *   control which discoveries are announced and in what order. This creates a
 *   'discovery race' dynamic where non-survey observatories are pressured to
 *   follow up quickly or lose scientific priority. The theater_ratio (0.58)
 *   reflects continued romanticization of 'incidental discovery' in
 *   scientific narratives while actual discovery flows through automated
 *   pipelines. The constraint demonstrates classic Tangled Rope structure:
 *   genuine coordination function (solving sky monitoring problem) coupled
 *   with asymmetric extraction (discovery priority accrues to survey
 *   institutions).
 *
 * KEY AGENTS:
 *   - Survey Institutions (Pan-STARRS, ZTF, LSST/Vera Rubin, etc.): Primary beneficiary (institutional/arbitrage) — control detection priority, set alert protocols, shape follow-up culture
 *   - Non-Survey Observatories: Primary victim (powerless/trapped) — lack resources to deploy comparable detection networks; depend on survey alerts for research agenda
 *   - Independent Observers and Amateur Networks: Secondary victim (moderate/constrained) — face resource barriers and pressure to follow up quickly; also benefit from survey alerts enabling opportunistic science
 *   - Open Data and Collaborative Standards Movement: Organized agents (organized/constrained) — VOEvent standards, time-domain forums, archival transparency mandates building alternative pathways to democratize alerts
 *   - Manual Observational Tradition (historiographical): Institutional actor (institutional/arbitrage) — maintains romantic narrative of visual discovery despite obsolescence; theaters the 'sharp-eyed observer' in funding and education
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing infrastructure choice as inherent random-rarity constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transient_event_detection, 0.38).
domain_priors:suppression_score(transient_event_detection, 0.42).
domain_priors:theater_ratio(transient_event_detection, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transient_event_detection, extractiveness, 0.38).
narrative_ontology:constraint_metric(transient_event_detection, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(transient_event_detection, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transient_event_detection, tangled_rope).
narrative_ontology:human_readable(transient_event_detection, "Automated Transient Event Detection in Astronomical Surveys").
narrative_ontology:topic_domain(transient_event_detection, "astronomical_science/observational_infrastructure").

domain_priors:requires_active_enforcement(transient_event_detection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transient_event_detection, survey_institutions).
narrative_ontology:constraint_beneficiary(transient_event_detection, astrophysical_theory).
narrative_ontology:constraint_victim(transient_event_detection, non_survey_observatories).
narrative_ontology:constraint_victim(transient_event_detection, independent_observers).
narrative_ontology:constraint_victim(transient_event_detection, scientific_discovery_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-SURVEY ASTRONOMER (SNARE) — Cannot exit the survey dominance; lacks resources to deploy comparable detection networks. Career pathways depend on access to survey data, reducing autonomy over research agenda. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.64.
constraint_indexing:constraint_classification(transient_event_detection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT OBSERVATORY (TANGLED ROPE) — Constrained by resource barriers and the 'discovery race' (must observe quickly or lose priority), but benefits from survey alerts enabling rapid follow-up observations. d≈0.70, f(d)≈1.08, σ=1.2 → χ≈0.40.
constraint_indexing:constraint_classification(transient_event_detection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SURVEY INSTITUTION (ROPE) — Benefits from discovery priority and funding concentration. Experiences constraint as pure coordination: automated detection solves the collective action problem of monitoring vast sky volume efficiently. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.04. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(transient_event_detection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COLLABORATIVE OPEN DATA MOVEMENT (SCAFFOLD) — Organized agents (time-domain forums, VOEvent standards, archival transparency mandates) are building infrastructure to democratize transient alerts and data. Sees survey dominance as a temporary coordination failure with a sunset: open alert systems and data pipelines promise to bypass survey gatekeeping. d≈0.38, f(d)≈0.38, σ=1.2 → χ≈0.17.
constraint_indexing:constraint_classification(transient_event_detection, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MANUAL OBSERVATIONAL TRADITION (PITON) — The idealized 'sharp-eyed observer' discovering supernovae by visual inspection persists in funding narratives and science education despite being functionally obsolete. Theater_ratio=0.58 reflects significant ritual maintenance: papers celebrate 'incidental discovery,' press releases emphasize human intuition, but actual discovery flow now runs through automated pipelines. The tradition endures through institutional inertia and romantic historiography.
constraint_indexing:constraint_classification(transient_event_detection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, transient events are rare and randomly distributed across the sky. The constraint emerges as a natural consequence: only massive, coordinated observation can capture them. No alternative detection modality exists that bypasses this fundamental fact. However, the structural data (ε=0.38, suppression=0.42, theater=0.58) contradicts mountain classification — the engine will compute this as a false summit, revealing that the 'random rarity' framing naturalizes what is actually a contingent infrastructure choice.
constraint_indexing:constraint_classification(transient_event_detection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transient_event_detection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transient_event_detection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transient_event_detection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(transient_event_detection, TR),
    TR >= 0.70.

:- end_tests(transient_event_detection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Survey institutions gain discovery priority and career/citation advantage during the window between automated detection and community follow-up. This advantage is real but bounded: follow-up observations are distributed, and survey institutions do not monopolize scientific understanding of transients — they monopolize priority. The value (0.38) reflects this: extraction is present but not total, and much of the 'extraction' serves genuine coordination (ensuring that alerts are issued and propagated). Suppression (0.42): Moderate. Barriers to independent transient discovery include: (a) detector cost ($10M-$100M+ for wide-field survey-scale systems), (b) continuous operation requirements (sky monitoring is 24/7), (c) real-time alert infrastructure (computing, network, personnel), (d) publication bias favoring 'novel discovery' over 'confirmation and characterization'. However, suppression is not total — independent observatories can and do make discoveries through manual searches, targeted monitoring, or archival mining. Theater ratio (0.58): Moderate-high, increasing over time. The temporal trend from 0.35 to 0.58 reflects growing gap between the romantic historiography of serendipitous discovery (featured in press releases, textbooks, funding narratives as 'the human observer's insight') and the procedural reality (automated software pipeline detecting candidate, human verification post-hoc). The 'incidental discovery' framing persists as institutional mythology despite being functionally obsolete.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals how a single infrastructure can appear as pure coordination (survey institutions) and pure extraction (non-survey astronomers) depending on structural position. The survey institution sees the automated system as solving the collective action problem of monitoring vast sky volume — they classify it as Rope, a coordination success. The non-survey astronomer experiences the same system as a barrier to entry and a gatekeeper on discovery priority — they classify it as Snare, an extractive trap. The independent observatory sees mixed structure: they benefit from rapid alert infrastructure but are constrained by the speed-race dynamic and resource barriers — Tangled Rope. The open data movement sees a temporary problem with an architectural solution path: decentralized alert standards and data democratization would transition the system from survey-controlled to community-owned, changing the classification from Snare/Tangled Rope to Rope. The civilizational analytical observer risks seeing transient rarity as an immutable natural law requiring centralized infrastructure, but the structural data reveals this as a contingent institutional choice — alternatives (distributed networks, archival mining, amateur detection) are viable, not inherent natural limits.
 *
 * DIRECTIONALITY LOGIC:
 *   Survey Institution: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. They experience the constraint as pure coordination because they designed it and benefit from it. Non-Survey Astronomer: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. They cannot exit survey dependence and lack resources to build alternatives. Independent Observatory: Victim + constrained → d≈0.70, f(d)≈1.08. Significant extraction. They can make some discoveries independently but are pressured by the speed-race dynamic. Open Data Movement: Organized + constrained → d≈0.38, f(d)≈0.38. Low effective extraction because this coalition has agency and an architectural solution (decentralization). Manual Observational Tradition: Institutional + arbitrage → d≈0.08, f(d)≈-0.08. Piton classification comes from theater gate (0.58 ≥ 0.70 would trigger piton fully, but at 0.58 it's high theater with degraded function). Analytical Observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival risk (observer naturalizes transient rarity as constraint); false summit detector catches this.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE STRUCTURE: This constraint satisfies all three Tangled Rope gates: (a) ε=0.38 ≥ 0.30 (extraction present), (b) suppression=0.42 ≥ 0.40 (significant barriers to exit), (c) requires_active_enforcement=true (survey institutions actively maintain infrastructure and control alert protocols). The genuine coordination function (solving sky monitoring) is present: automated detection provides scientific value to the entire field by ensuring transients are captured and communicated. But it is coupled with asymmetric extraction: survey institutions control discovery priority and gate-keep alert issuance. The mandatrophy is resolved by showing that decentralization is possible but requires institutional change (open alerts, data democratization) — the constraint is not a natural law but a contingent infrastructure choice that can be restructured. The false summit risk (perspective 6) is that some analysts naturalize transient rarity as requiring centralized institutional monopoly, but distributed detection systems (VLA, amateur networks, archival reprocessing) can solve the monitoring problem without granting single institutions extraction privilege. The theater ratio (0.58) indicates the system is partly ideological performance — the romantic narrative of serendipitous discovery masks the reality of engineered automated detection, reflecting Goodhart drift where the narrative goal (emphasizing human discovery insight) has replaced the functional goal (detecting all transients).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survey_data_accessibility_threshold,
    'At what level of data accessibility and latency does survey dominance cease to be structurally extractive?',
    'Comparative analysis of discovery rates: non-survey institutions with real-time alert access vs delayed access vs no access; correlation between data release delay and follow-up discovery contribution',
    'If real-time open alerts eliminate extraction: constraint reclassifies to Rope (pure coordination). If delays persist or data remains proprietary: extraction persists and constraint remains Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(survey_data_accessibility_threshold, empirical, 'Data accessibility threshold for eliminating survey extraction').

omega_variable(
    follow_up_observation_equity,
    'Do independent observatories have equal probability of detecting and characterizing transients post-discovery, or does survey institutional advantage extend through follow-up phases?',
    'Analysis of follow-up observation datasets: fraction of transients followed up by survey vs non-survey institutions; publication authorship distribution; career advancement correlation with follow-up participation',
    'If follow-up is equitable: extraction is limited to discovery priority. If survey institutions dominate follow-up too: extraction mechanism is deeper (learning extraction, not just discovery extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(follow_up_observation_equity, empirical, 'Whether follow-up opportunities are equitably distributed').

omega_variable(
    community_alert_system_viability,
    'Can distributed community-based alert systems (VLA, amateur networks, archival mining) achieve detection completeness comparable to survey conveyor belts without centralized institutional coordination?',
    'Comparison of discovery completeness and contamination rates: centralized survey vs distributed alternatives; analysis of systematic detection biases in each approach',
    'If distributed systems achieve parity: scaffold sunset is real; constraint can transition to Rope via decentralization. If centralized systems remain necessary: architectural dependence on survey institutions is structural, not extractive policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_alert_system_viability, empirical, 'Whether distributed alert systems can match survey completeness').

omega_variable(
    rapid_response_bottleneck,
    'Is the extraction mechanism primarily about discovery priority, or is it about rapid-response capability (first spectrum/imaging gives scientific advantage)?',
    'Temporal analysis of transient evolution: measure whether discoveries made 2-3 hours after automated detection yield scientifically different results than same-night observations; compare sample completeness and parameter constraints',
    'If rapid response is critical to science: extraction is justified (genuine coordination advantage, not rent-seeking). If delayed response yields equivalent science: extraction is pure priority-rent, justifying stronger decentralization push.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rapid_response_bottleneck, empirical, 'Whether rapid response provides essential scientific advantage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transient_event_detection, 0, 11).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ted_theater_t0, transient_event_detection, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ted_theater_t6, transient_event_detection, theater_ratio, 6, 0.48).
narrative_ontology:measurement(ted_theater_t11, transient_event_detection, theater_ratio, 11, 0.58).

% Extraction over time
narrative_ontology:measurement(ted_extract_t0, transient_event_detection, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ted_extract_t6, transient_event_detection, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(ted_extract_t11, transient_event_detection, base_extractiveness, 11, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transient_event_detection, information_standard).
narrative_ontology:affects_constraint(transient_event_detection, gravitational_wave_follow_up).
narrative_ontology:affects_constraint(transient_event_detection, multi_messenger_observation_equity).
narrative_ontology:affects_constraint(transient_event_detection, astronomical_survey_funding_concentration).

% DUAL FORMULATION NOTE:
% Automated transient detection is downstream of survey infrastructure decisions but represents a distinct structural constraint. Upstream constraints include resource allocation to survey institutions and funding concentration in mega-surveys; downstream constraints include follow-up observation equity and multi-messenger coordination. The architecture choice (centralized survey vs distributed monitoring) determines ε: centralized = 0.38 (current), decentralized = ~0.15 (hypothetical open-data regime).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
