% ============================================================================
% CONSTRAINT STORY: transient_event_detection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: transient_event_detection
 *   human_readable: Automated Transient Event Detection
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   Astronomy's relationship with transient cosmic events has undergone a
 *   structural transformation from the 11th-century detection of the Crab
 *   Nebula supernova (observed by chance, recorded by Chinese astronomers) to
 *   21st-century automated survey systems that detect thousands of transients
 *   annually through algorithmic filtering. This transition is not merely
 *   technological — it is an institutional constraint that redistributes
 *   discovery credit, access to observational resources, and scientific
 *   authority. The constraint exhibits tangled_rope characteristics:
 *   automated detection provides genuine coordination benefits (events are
 *   reliably identified and distributed to the community) but creates
 *   asymmetric extraction (discovery priority and scientific leadership
 *   concentrate in survey-operating institutions while independent and
 *   resource-constrained observers lose competitive access). The constraint's
 *   extractiveness has grown from ~0.15 (early 20th century) as automation
 *   began, through ~0.28 (late 20th century with CCD surveys), to ~0.38 today
 *   as real-time alert systems dominate. Theater ratio has correspondingly
 *   risen as the narrative of 'telescopic discovery' persists despite
 *   algorithmic reality.
 *
 * KEY AGENTS:
 *   - Survey Instrument Operators (institutional/arbitrage): Primary beneficiaries — control alert streams, publish discovery papers, set research priorities for the field
 *   - Major Astrophysics Institutions (institutional/arbitrage): Secondary beneficiaries — institutional prestige and publication volume from survey leadership
 *   - Independent Transient Discoverers (powerless/trapped): Primary victims — historically contributed discoveries; now structurally excluded from competition
 *   - Resource-Constrained Observatories (powerless/trapped): Primary victims — lack real-time data access and computing infrastructure to operate competing systems
 *   - Secondary Research Groups (moderate/constrained): Secondary victims — can participate in follow-up but not discovery; asymmetric scientific credit
 *   - Open Alert Distribution Networks (organized/mobile): Structural alternative — brokers and open protocols building democratized access with sunset logic
 *   - Analytical Observer (analytical/analytical): Measures whether constraint is technical necessity or institutional choice
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
narrative_ontology:human_readable(transient_event_detection, "Automated Transient Event Detection").
narrative_ontology:topic_domain(transient_event_detection, "technological/scientific").

domain_priors:requires_active_enforcement(transient_event_detection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transient_event_detection, survey_instrument_operators).
narrative_ontology:constraint_beneficiary(transient_event_detection, astrophysics_research_institutions).
narrative_ontology:constraint_victim(transient_event_detection, small_telescope_observers).
narrative_ontology:constraint_victim(transient_event_detection, independent_discovery_culture).
narrative_ontology:constraint_victim(transient_event_detection, resource_constrained_observatories).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT TRANSIENT DISCOVERERS (SNARE) — Amateur astronomers and small-telescope observers who historically contributed transient discoveries now find themselves structurally excluded from the discovery ecosystem. Automated survey systems process events at machine speed; human observers cannot compete. The extraction is severe: discovery priority, publication opportunity, and scientific credit flow exclusively to survey operators. No exit option exists — the observational commons has been automated away.
constraint_indexing:constraint_classification(transient_event_detection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESOURCE-CONSTRAINED OBSERVATORIES (SNARE) — Regional observatories and developing-world institutions that lack access to real-time automated survey data experience the constraint as pure extraction. They cannot detect transients through classical observation — the events are already known by machine networks. Their observational capacity becomes worthless for original discovery. Trapped by lack of computing infrastructure and data feed access.
constraint_indexing:constraint_classification(transient_event_detection, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SECONDARY RESEARCH GROUPS (TANGLED ROPE) — Mid-tier institutions that follow up on automated survey alerts experience a hybrid: they benefit from the conveyor belt system (transients are reliably reported) but bear extraction costs (they cannot be first discoverers, only verifiers/analyzers). Resource constraints limit follow-up observation. Real agency and real benefits — but also systematic asymmetry.
constraint_indexing:constraint_classification(transient_event_detection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SURVEY INSTRUMENT OPERATORS (ROPE) — Large survey collaborations (ZTF, LSST, Pan-STARRS operators) experience the constraint as coordination: automated systems solve the collective action problem of detecting rare events. The operators benefit from discovery priority, publication leadership, and scientific authority. For them, the system is a coordination mechanism with net benefit.
constraint_indexing:constraint_classification(transient_event_detection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MAJOR ASTROPHYSICS INSTITUTIONS (ROPE) — Universities and national laboratories hosting survey instruments experience the constraint as pure coordination: automated detection enables science at scale. They solve the technical problem of identifying fleeting events and gain institutional prestige, publication volume, and research capacity. Their exit options are strong — they can redirect surveys or modify detection algorithms.
constraint_indexing:constraint_classification(transient_event_detection, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CLASSICAL OBSERVATIONAL ASTRONOMY NORMS (PITON) — The tradition of the dedicated observer at the telescope remains culturally central to astronomy (stories of serendipitous discovery, the Eureka narrative) yet is functionally degraded by automation. Professional identity remains anchored in 'being the discoverer,' but the mechanism has shifted to algorithmic filters. Theater ratio is high: astronomical societies still celebrate visual discovery and communicate telescopic observation as the primary scientific activity, even as the actual discovery mechanism is automated software. Institutional inertia maintains the narrative.
constraint_indexing:constraint_classification(transient_event_detection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: OPEN ALERT DISTRIBUTION NETWORKS (SCAFFOLD) — Emerging protocols (ATEL, GCN, broker systems like Alerce and Fink) distribute transient alerts openly to all observers with network access. These systems lower barriers to follow-up observation and democratize participation in transient science. The sunset logic is real: as alert systems mature and brokers standardize, the monopoly on discovery priority erodes. Smaller institutions can detect alerts and contribute analysis. The extraction mechanism is temporary — designed to be replaced by more distributed access.
constraint_indexing:constraint_classification(transient_event_detection, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a long-term analytical perspective, transient events are intrinsically fleeting and detection requires speed exceeding human reaction time. As events become rarer and faster, automated detection is an inescapable technical requirement — not an institutional choice. The constraint appears as a natural law of observational astronomy. However, the base metrics contradict this mountain classification: the extraction values, suppression levels, and structural data reveal this as a false summit. The temporal constraint is real (events are fast), but the institutional monopoly structure is contingent.
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
 *   Extractiveness (0.38): Moderate. The survey system does extract discoverer privilege and publication priority from smaller institutions — a real asymmetry. But extractiveness is not extreme (0.46+) because: (1) the system provides genuine value (transients are efficiently detected and alerts are distributed), (2) follow-up science remains valuable and accessible to non-discoverers, (3) alert brokers and open-source detection pipelines are developing alternative pathways. The value reflects legitimate technical monopoly (detection at machine speed) plus institutional monopoly (priority norms). Suppression (0.42): Moderate. Barriers exist (computing infrastructure, real-time alert access, algorithmic expertise) but are not insurmountable. Smaller facilities can operate detection systems; brokers provide open alerts; open-source software is available. Suppression is institutional and economic, not absolute. Theater ratio (0.58): Moderate-high. The culture of astronomy retains 'the discoverer at the telescope' as its dominant origin narrative, yet the actual mechanism is algorithmic filtering. Professional identity discourse emphasizes observation and discovery despite structural shift to data analysis and automated triage. Theater has increased over time as the gap between narrative and mechanism widened.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal between survey operators (Rope, institutional/arbitrage) and independent observers (Snare, powerless/trapped). The operators experience the constraint as solving the coordination problem of detecting rare events efficiently — a genuine good. Independent observers experience the same constraint as competitive exclusion — pure extraction. Secondary research groups occupy an intermediate position (Tangled Rope) — they benefit from the system (alerts are provided) but bear asymmetric costs (they cannot be discoverers). The open-science networks (Scaffold) project a future state where brokers and transparent algorithms decompose the institutional monopoly into temporary coordination. The classical astronomy norms (Piton) represent the persistence of discovery-narrative culture despite functional displacement by automation. The analytical observer (Mountain) risks naturalizing a contingent institutional structure as a law of physics — transient latency is real, but the monopoly structure is not.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position relative to the extraction flow. Survey operators are beneficiaries with strong exit options (arbitrage) — they can modify detection algorithms, redirect surveys, or sell data access. Their d value is low, and f(d) produces near-zero or negative chi, reflecting that they experience the constraint as beneficial coordination. Independent observers are victims with no exit (trapped) — they cannot access real-time alerts at machine latency or operate competing survey systems at equivalent scale. Their d is high (~0.95), f(d) is maximal (~1.42), and chi is severe, reflecting experienced extraction. Resource-constrained observatories have weak exit options (trapped by infrastructure deficits), so their d is similarly high. Secondary research groups have some exit (constrained) — they can conduct follow-up science, contribute to surveys, or operate smaller systems. Their d is moderate (~0.70-0.75), and chi is moderate, reflecting mixed experience. Brokers and open-science networks have mobile exit options — they can deploy alternatives and build parallel infrastructure. Their d is low-to-moderate, and chi is low, reflecting their role in reducing the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint is legitimately Tangled Rope, not mislabeled as pure Rope (coordination myth) or pure Snare (extraction myth). The coordination function is real and valuable — automated detection has enabled discovery of entire new transient classes (tidal disruption events, kilonovae, fast radio bursts) that would be undetectable through classical observation alone. The system genuinely solves a coordination problem. The extraction is also real and structural — discovery priority and scientific authority concentrate in survey-operating institutions, while smaller institutions are systematically disadvantaged. The constraint is not pure coordination (which would require symmetric benefits) and is not pure extraction (which would require no coordination function). The tangled_rope classification is correct because it captures both: the coordination is essential (the system enables new science), and the extraction is asymmetric (benefits and authority are asymmetrically distributed). The scaffold perspective (open alert brokers, transparent algorithms, distributed follow-up) represents a genuine pathway toward reducing the extraction component while retaining the coordination function — a sunset clause that is structurally realizable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alert_latency_floor,
    'What is the minimum alert latency below which human-in-the-loop follow-up becomes impossible for any telescope, regardless of automation?',
    'Empirical measurement of event timescales (orphan afterglows, fast radio burst components) and human response times; determination of physical threshold',
    'If floor < 1 minute: detection must be fully automated (natural law). If floor > 10 minutes: distributed human networks could theoretically compete. Current evidence suggests ~30-second floor for optimal follow-up on fastest events.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alert_latency_floor, empirical, 'Physical latency floor for human follow-up on transient events').

omega_variable(
    discovery_credit_attribution,
    'Should discovery credit flow to the automated system operator, the algorithm designer, the telescope builder, or the first independent confirmer? What determines legitimate claim to priority?',
    'Historical analysis of discovery attribution norms; comparison with other scientific fields (genome sequencing, particle detection); community consensus on priority rules',
    'If operator gets credit: extraction persists and scalable (snare). If distributed to all contributors: extraction dissolves (rope or open science). If independent confirmer has claim: incentivizes follow-up (scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discovery_credit_attribution, conceptual, 'Attribution norms for automated discovery').

omega_variable(
    broker_sufficiency,
    'Do existing transient brokers (Alerce, Fink, MARS) provide sufficient real-time access and filtering that smaller observatories can compete on follow-up science, or do latency/data delays preserve survey operator advantage?',
    'Benchmarking broker latency; tracking number of follow-up observations from non-survey institutions; measuring scientific contribution rate vs discovery institution affiliation',
    'If brokers sufficiently level the field: scaffold perspective confirmed, extraction is temporary. If brokers remain second-class (delayed alerts, filtered subsets): snare persists despite open-science rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broker_sufficiency, empirical, 'Whether alert brokers sufficiently democratize follow-up participation').

omega_variable(
    algorithm_transparency_decoupling,
    'Can transparent, open-source transient detection algorithms deployed on smaller facilities genuinely compete with proprietary survey pipelines optimized over decades, or is there a complexity threshold beyond which open algorithms cannot match performance?',
    'Comparative false-positive and false-negative rates; independent deployment of survey algorithms on smaller telescopes; analysis of optimization ceiling',
    'If open algorithms match performance: the extraction is primarily institutional/political (tangled rope with scaffold promise). If proprietary pipelines are fundamentally superior: the extraction is partly technical (mountain-adjacent), harder to decompose.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithm_transparency_decoupling, empirical, 'Whether open-source detection algorithms can match proprietary survey performance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transient_event_detection, 0, 915).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ted_tr_t0, transient_event_detection, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ted_tr_t550, transient_event_detection, theater_ratio, 550, 0.48).
narrative_ontology:measurement(ted_tr_t915, transient_event_detection, theater_ratio, 915, 0.58).

% Extraction over time
narrative_ontology:measurement(ted_be_t0, transient_event_detection, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ted_be_t550, transient_event_detection, base_extractiveness, 550, 0.28).
narrative_ontology:measurement(ted_be_t915, transient_event_detection, base_extractiveness, 915, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transient_event_detection, information_standard).
narrative_ontology:affects_constraint(transient_event_detection, gravitational_wave_source_localization).
narrative_ontology:affects_constraint(transient_event_detection, fast_radio_burst_catalog_completeness).
narrative_ontology:affects_constraint(transient_event_detection, supernova_early_warning_hierarchy).

% DUAL FORMULATION NOTE:
% Automated transient detection decomposes into two structurally distinct constraints: (1) the technical requirement for machine-speed event processing (approaching a natural law as event timescales shrink), and (2) the institutional monopoly on discovery priority and alert distribution (clearly contingent institutional structure). This story models the hybrid (technical + institutional); downstream constraints inherit both elements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transient_event_detection, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
