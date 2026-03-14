% ============================================================================
% CONSTRAINT STORY: astronomical_discovery_priority_attribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_astronomical_discovery_priority_attribution, []).

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
 *   constraint_id: astronomical_discovery_priority_attribution
 *   human_readable: Astronomical Discovery Priority Attribution System
 *   domain: astronomy/institutional_science
 *
 * SUMMARY:
 *   Astronomical discovery priority attribution creates a structural tension
 *   between the legitimate coordination problem of resolving simultaneous
 *   discoveries and the extractive gatekeeping function performed by major
 *   institutional observers. When multiple astronomers or automated surveys
 *   detect the same transient phenomenon, priority must be attributed to a
 *   single discoverer to establish the authoritative historical record and
 *   credit the observer's effort and insight. However, the modern system
 *   concentrates this authority in major institutional programs with
 *   real-time telescope access and publishing infrastructure, systematically
 *   extracting priority credit from independent and amateur observers who
 *   often document discoveries earlier but lack institutional credibility and
 *   publication channels. The constraint exhibits all six DR types from
 *   different perspectives: institutional beneficiaries experience
 *   coordination (Rope), independent observers experience pure extraction
 *   (Snare), secondary institutions experience mixed coordination-extraction
 *   (Tangled Rope), organized coalitions building alternative attribution
 *   systems see a temporary problem with a sunset (Scaffold), the historical
 *   naming convention has degraded into theater (Piton), and the
 *   civilizational observer risks naturalizing institutional gatekeeping as a
 *   physical law (Mountain, false summit). The theater_ratio (0.68) reflects
 *   that modern discovery attribution has become substantially performative:
 *   major institutions apply automated algorithms to detect transients
 *   (removing human insight from discovery) yet maintain the convention of
 *   assigning human discoverers for historical and prestige purposes. The
 *   rising extractiveness (0.35 → 0.52 over 20 years) shows that as automated
 *   surveys have multiplied discovery volume, the institutional gatekeeping
 *   function has intensified — concentration of attribution authority has
 *   increased even as the coordination problem (resolving simultaneous
 *   discoveries) has not substantially changed.
 *
 * KEY AGENTS:
 *   - Major Survey Programs: Primary beneficiary (institutional/arbitrage) — capture first-author priority, funding advantage, telescope allocation through discovery credit; experience system as coordination mechanism accelerating follow-up observations
 *   - Independent & Amateur Observers: Primary victim (powerless/trapped) — lack institutional credibility and publishing infrastructure; even with documented observations, priority flows to institutional confirming observations; cannot exit system
 *   - Secondary Institutions: Secondary beneficiary/victim (moderate/constrained) — benefit from survey data access and collaborative science, but lose first-author priority to major programs; face career risk from publishing contradictions to major institutions
 *   - Open Data Coalition: Organized reformers (organized/mobile) — LSST, IVOA, Pan-STARRS, Transient Name Server building alternative attribution through real-time data sharing and automated timestamp precedence; see institutional gatekeeping as temporary coordination failure with sunset
 *   - Discoverer Naming Convention: Institutional artifact (institutional/arbitrage) — performative ritual of naming objects after discoverers; original function (crediting human effort) has atrophied under automation; persists through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional speed-of-access as physical information-speed asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(astronomical_discovery_priority_attribution, 0.52).
domain_priors:suppression_score(astronomical_discovery_priority_attribution, 0.58).
domain_priors:theater_ratio(astronomical_discovery_priority_attribution, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(astronomical_discovery_priority_attribution, extractiveness, 0.52).
narrative_ontology:constraint_metric(astronomical_discovery_priority_attribution, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(astronomical_discovery_priority_attribution, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(astronomical_discovery_priority_attribution, tangled_rope).
narrative_ontology:human_readable(astronomical_discovery_priority_attribution, "Astronomical Discovery Priority Attribution System").
narrative_ontology:topic_domain(astronomical_discovery_priority_attribution, "astronomy/institutional_science").

domain_priors:requires_active_enforcement(astronomical_discovery_priority_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(astronomical_discovery_priority_attribution, discoverer_institutions).
narrative_ontology:constraint_beneficiary(astronomical_discovery_priority_attribution, major_survey_programs).
narrative_ontology:constraint_victim(astronomical_discovery_priority_attribution, independent_observers).
narrative_ontology:constraint_victim(astronomical_discovery_priority_attribution, field_epistemic_consistency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT OBSERVER (SNARE) — Amateur and independent astronomers who discover transient phenomena face institutional barriers to priority attribution. Even with documented observations, discovery credit flows to institutional observers with access to major telescopes and publishing infrastructure. Cannot exit the system — all discovery claims require institutional validation to receive recognition. Maximum extraction.
constraint_indexing:constraint_classification(astronomical_discovery_priority_attribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SECONDARY INSTITUTION (TANGLED ROPE) — Mid-tier astronomical institutions benefit from collaborative access to major surveys while bearing coordination costs and replication labor. Have some agency through publication channels and observing time proposals, but face extraction: major institutions capture first-author priority despite collaborative contributions. Constrained exit — can publish independently but sacrifices visibility and telescope access.
constraint_indexing:constraint_classification(astronomical_discovery_priority_attribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR SURVEY PROGRAM (ROPE) — Large telescope programs (Gaia, Sloan, ZTF, Vera Rubin) experience the priority system as a coordination mechanism: broadcasting discovery data accelerates follow-up observations and collaborative science. Net beneficiary through citation networks and institutional prestige, but also genuine coordination function — the system accelerates knowledge accumulation across the field. Can arbitrage to other discovery channels if needed.
constraint_indexing:constraint_classification(astronomical_discovery_priority_attribution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN DATA COALITION (SCAFFOLD) — International open-data initiatives (Transient Name Server, Asteroids@Home, Pan-STARRS public data releases) see priority disputes as a temporary coordination failure with a natural sunset: real-time data sharing and automated discovery pipelines reduce the importance of human priority attribution. Organized actors (LSST, ADASS, IVOA) are building alternative attribution models (timestamp precedence, contributor ledgers). Sunset clause implicit: as automated surveys dominate, discovery becomes collective data processing, not individual claim-staking.
constraint_indexing:constraint_classification(astronomical_discovery_priority_attribution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DISCOVERER NAMING CONVENTION (PITON) — The historical convention of naming astronomical objects after discoverers (Messier objects, Herschel discoveries) has atrophied into a performative ritual. Modern surveys produce thousands of objects per night using automated algorithms; naming each after a human discoverer is theatrically maintained through designations (e.g., 'supernova YYYY-institute-index') but the underlying function (crediting the human discoverer for effort and insight) has degraded. The convention persists through institutional inertia despite low functionality.
constraint_indexing:constraint_classification(astronomical_discovery_priority_attribution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the speed-of-light information constraint creates an inherent asymmetry: institutional observers with real-time access to major telescopes will always detect time-variable phenomena before distributed observers separated by communication delays. This perspective naturalizes the priority system as a law of physics. However, the structural data contradicts the mountain classification — the engine's false summit detector will identify this as naturalization. Modern networks transmit observations across the globe in seconds; the information asymmetry is institutional (who controls telescope time) not physical (information speed).
constraint_indexing:constraint_classification(astronomical_discovery_priority_attribution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(astronomical_discovery_priority_attribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(astronomical_discovery_priority_attribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(astronomical_discovery_priority_attribution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(astronomical_discovery_priority_attribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(astronomical_discovery_priority_attribution, TR),
    TR >= 0.70.

:- end_tests(astronomical_discovery_priority_attribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The major survey programs systematically extract priority credit from independent observers through institutional gatekeeping. However, extraction is not maximal because legitimate coordination benefits exist — survey programs do accelerate follow-up science and field knowledge accumulation. The 0.52 value reflects the hybrid coordination-extraction nature of the system. Suppression (0.58): Moderate-high. Independent observers face institutional barriers including lack of publishing venues for discoveries without institutional affiliation, lack of real-time telescope access for confirmation observations, and cultural biases against 'amateur' contributors in academic priority systems. But suppression is not total — some independent discoveries do receive recognition through amateur astronomy networks and citizen science programs. Theater ratio (0.68): High and increasing. Modern automated surveys detect transients through algorithmic pipelines with no human discovery insight required, yet the convention of assigning human discoverers and naming objects after them persists as a performative ritual. The naming convention maintains prestige associations and historical continuity but has decoupled from its original function of crediting human effort. Rising theater_ratio (0.42 → 0.68 over 20 years) indicates the gap between ritual and function has widened as automation has expanded discovery volume.
 *
 * PERSPECTIVAL GAP:
 *   The gap between major survey programs (Rope) and independent observers (Snare) is the central diagnostic signal. Both are operating under identical base constraint metrics (ε=0.52, suppression=0.58), but arrive at completely opposite classifications due to their power levels and exit options. The beneficiary with arbitrage exit perceives coordination; the victim with trapped exit perceives extraction. This gap reveals the hybrid nature of the constraint: it does coordinate legitimate discovery conflicts (Rope function) while simultaneously gatekeeping priority credit through institutional access (Snare extraction). The secondary institution perspective (Tangled Rope) confirms both functions coexist. The scaffold perspective shows that alternative attribution systems are building genuine exits. The piton perspective shows the naming ritual has decoupled from coordination function. The mountain perspective reveals the risk of naturalizing institutional gatekeeping as inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value is computed from their structural position relative to the priority system. Major survey programs benefit from gatekeeping and can arbitrage (publish through multiple channels, propose observations at competing telescopes), producing low d → low experienced extraction. Independent observers are excluded from the gatekeeping benefit and have no exit (all credible astronomical publishing requires institutional validation or collaboration), producing high d → high experienced extraction. Secondary institutions are partially beneficiaries (gain survey data access) but also victims (lose first-author priority to major programs), and are constrained (high costs to exiting survey collaborations), producing moderate d → moderate experienced extraction. The organized coalition (open data initiatives) is working to reduce institutional gatekeeping through alternative systems (real-time data sharing, timestamp precedence, automated attribution), producing moderate-high d but with mobile exit (they have agency to build alternatives), producing lower experienced extraction than their d value alone would suggest. These directionality values explain why the same base constraint (ε=0.52) produces different classifications across perspectives: experienced extraction χ = ε × f(d) × σ(S) varies substantially with d and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR FOR TANGLED ROPE: This constraint resolves the mandatrophy by demonstrating that the tangled_rope classification correctly captures both the coordination function AND the asymmetric extraction. The system simultaneously solves a genuine problem (resolving simultaneous discoveries, maintaining scientific priority records) while extracting priority credit through institutional gatekeeping. Single-type interpretations fail: calling it pure Rope (coordination) ignores the systematic extraction from independent observers; calling it pure Snare (extraction) ignores the legitimate coordination benefits. The tangled_rope classification confirms both functions coexist with the following structural markers: (1) beneficiaries declared (major survey programs + secondary institutions gain from survey collaboration); (2) victims declared (independent observers systematically excluded from priority); (3) active enforcement required (institutional gatekeeping requires enforcement of credibility standards and publishing norms); (4) perspectival gap shows beneficiaries perceive coordination (Rope) while victims perceive extraction (Snare). The hybrid nature is diagnostic: if the constraint were pure coordination, independent observers would still perceive some coordination benefit (faster follow-up, broader data access); instead, they perceive none. If the constraint were pure extraction, major survey programs would experience no coordination benefit; instead, they do. The 0.52 extractiveness reflects that the gatekeeper benefits are genuine (~35% value) but systematically asymmetric, with high redistribution to beneficiaries and high blockade of victims (~52% value). The mandatrophy is resolved by accepting both dimensions as real and structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_gatekeeping_threshold,
    'What level of institutional gatekeeping is inherent to astronomical discovery coordination versus extractive rent-seeking?',
    'Historical comparison of discovery attribution across discovery era transitions: pre-telegram (1800s), pre-radio (early 1900s), pre-internet (1980s), and post-real-time-data-sharing (2020s). Identify whether gatekeeping function declined proportionally with communication infrastructure improvement.',
    'If gatekeeping is primarily institutional (not physical): constraints classification shifts toward Snare for independent observers; pressure for alternative priority systems increases. If primarily coordination: system persists because it solves genuine conflicts-of-interest problems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_gatekeeping_threshold, empirical, 'Proportion of gatekeeping that is institutional versus coordination-necessary').

omega_variable(
    automated_discovery_attribution_feasibility,
    'Can automated discovery attribution systems (timestamp precedence, data-track records, algorithm attribution) achieve comparable credibility to human institutional attribution?',
    'Deployment trials of automated attribution systems in live survey data; comparison of citation impact and community acceptance of automatically-attributed discoveries versus traditionally-attributed discoveries over 5-year period.',
    'If feasible: scaffold sunset timeline accelerates — institutional gatekeeper function becomes obsolete within 10-15 years. If not feasible: institutional gatekeeping persists as necessary for legitimacy, tangled_rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automated_discovery_attribution_feasibility, empirical, 'Feasibility of automated discovery attribution systems').

omega_variable(
    independent_observer_coalition_formation,
    'Can independent and amateur astronomers form coalition structures that achieve institutional-equivalent credibility in priority attribution?',
    'Longitudinal tracking of amateur astronomer organization (astronomy clubs, citizen science networks, observer collectives) and their ability to publish discoveries under group attribution. Measurement of publication acceptance rates and citation impact.',
    'If coalition formation succeeds: independent observers transition from ''trapped'' to ''constrained'' or ''mobile'' exit options; snare classification shifts toward tangled_rope. If coalition formation fails: powerless agents remain structurally trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independent_observer_coalition_formation, empirical, 'Capacity of independent observers to form credible coalitions').

omega_variable(
    supernova_attribution_dispute_escalation,
    'Are recent supernova naming disputes (transient designation conflicts, simultaneous discoveries from different institutions) evidence of system degradation or legitimate coordination problems being resolved?',
    'Analysis of dispute resolution outcomes: are compromises and co-attribution increasing? Are dispute timelines shortening due to faster communication? Are independent observers successfully contesting institutional attribution?',
    'If coordination problems: theater_ratio decreases, system self-corrects toward Rope. If extraction escalation: theater_ratio increases, system drifts toward Snare, pressuring reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supernova_attribution_dispute_escalation, empirical, 'Whether recent attribution disputes signal system degradation or resolution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(astronomical_discovery_priority_attribution, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(astro_prio_tr_t0, astronomical_discovery_priority_attribution, theater_ratio, 0, 0.42).
narrative_ontology:measurement(astro_prio_tr_t10, astronomical_discovery_priority_attribution, theater_ratio, 10, 0.55).
narrative_ontology:measurement(astro_prio_tr_t20, astronomical_discovery_priority_attribution, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(astro_prio_be_t0, astronomical_discovery_priority_attribution, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(astro_prio_be_t10, astronomical_discovery_priority_attribution, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(astro_prio_be_t20, astronomical_discovery_priority_attribution, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(astronomical_discovery_priority_attribution, information_standard).
narrative_ontology:affects_constraint(astronomical_discovery_priority_attribution, supernova_transient_detection).
narrative_ontology:affects_constraint(astronomical_discovery_priority_attribution, gravitational_wave_source_localization).
narrative_ontology:affects_constraint(astronomical_discovery_priority_attribution, fast_radio_burst_attribution).

% DUAL FORMULATION NOTE:
% Discovery priority attribution operates across multiple transient-detection domains (supernovae, GW events, FRBs, asteroid discoveries). Each domain has unique institutional structures and alternative attribution systems, but all instantiate the same core tension between coordination (resolving simultaneous discoveries) and gatekeeping extraction. This story models the abstract constraint; domain-specific variants should decompose institutional actors and exit options per domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
