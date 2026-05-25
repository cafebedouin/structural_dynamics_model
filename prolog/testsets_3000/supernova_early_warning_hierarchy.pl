% ============================================================================
% CONSTRAINT STORY: supernova_early_warning_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supernova_early_warning_hierarchy, []).

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
 *   constraint_id: supernova_early_warning_hierarchy
 *   human_readable: Supernova Early Warning Hierarchy
 *   domain: astronomy/astrophysics/institutional_governance
 *
 * SUMMARY:
 *   The supernova early warning hierarchy represents an institutional
 *   structure that coordinates rapid astronomical response to transient
 *   events while simultaneously extracting value from non-institutional
 *   discoverers. The hierarchy exists because coordinating follow-up
 *   observations on limited telescopes requires prioritization; however, the
 *   institutional control mechanisms have evolved beyond coordination
 *   necessity into gatekeeping that blocks access for capable independent
 *   observers. The constraint exhibits all six DR types from different
 *   perspectives: pure extraction (Snare) for excluded amateurs, mixed
 *   coordination-extraction (Tangled Rope) for independent research groups,
 *   pure coordination (Rope) for beneficiary institutions, temporary
 *   coordination problem with sunset (Scaffold) for organized open-science
 *   networks, degraded ritual (Piton) for legacy classification conventions,
 *   and potential false natural law (Mountain) for civilizational analysis.
 *   The theater_ratio (0.68) reflects that much of the institutional
 *   apparatus — naming conventions, discoverer credits, notification
 *   protocols — is now substantially performative, maintained through
 *   historical precedent even as automation and open surveys make manual
 *   gatekeeping obsolete.
 *
 * KEY AGENTS:
 *   - Amateur Astronomers: Primary victims (powerless/trapped) — detect supernovae but cannot claim priority or publish without institutional validation
 *   - Rapid Response Observers: Secondary victims (powerless/trapped) — specialized equipment and expertise excluded from real-time alert networks
 *   - Independent Research Groups: Moderate victims (moderate/constrained) — benefit from early warning coordination but pay extraction costs through delays and publication gatekeeping
 *   - Professional Observatories: Primary beneficiaries (institutional/arbitrage) — control alert hierarchy, first-mover advantage, resource allocation authority
 *   - First-Discoverer Institutions: Beneficiaries (institutional/arbitrage) — publish priority, citation advantage, follow-up observing time
 *   - Open Alert Networks: Organized agents (organized/mobile) — LSST, ZTF, automated surveys building alternative pathways with eventual sunset of traditional hierarchy
 *   - Legacy Classification System: Institutional actor (institutional/arbitrage) — maintains performative naming and discoverer conventions through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as necessary laws of astronomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supernova_early_warning_hierarchy, 0.58).
domain_priors:suppression_score(supernova_early_warning_hierarchy, 0.62).
domain_priors:theater_ratio(supernova_early_warning_hierarchy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supernova_early_warning_hierarchy, extractiveness, 0.58).
narrative_ontology:constraint_metric(supernova_early_warning_hierarchy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(supernova_early_warning_hierarchy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supernova_early_warning_hierarchy, tangled_rope).
narrative_ontology:human_readable(supernova_early_warning_hierarchy, "Supernova Early Warning Hierarchy").
narrative_ontology:topic_domain(supernova_early_warning_hierarchy, "astronomy/astrophysics/institutional_governance").

domain_priors:requires_active_enforcement(supernova_early_warning_hierarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supernova_early_warning_hierarchy, professional_astronomy_institutions).
narrative_ontology:constraint_beneficiary(supernova_early_warning_hierarchy, first_discoverers).
narrative_ontology:constraint_victim(supernova_early_warning_hierarchy, rapid_response_observers).
narrative_ontology:constraint_victim(supernova_early_warning_hierarchy, amateur_astronomers).
narrative_ontology:constraint_victim(supernova_early_warning_hierarchy, public_science_access).
narrative_ontology:constraint_victim(supernova_early_warning_hierarchy, follow_up_research_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AMATEUR ASTRONOMER (SNARE) — Trapped by information asymmetry and institutional gatekeeping. Amateur observers detect supernovae but cannot publish discoveries or claim priority without institutional validation. Career pathways for rapid response are blocked. Maximum experienced extraction with no exit option.
constraint_indexing:constraint_classification(supernova_early_warning_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RAPID RESPONSE OBSERVERS (SNARE) — Structurally excluded from early notification networks despite having specialized equipment and proven track records. Cannot access real-time alert hierarchies. Forced to work around institutional gatekeepers. Suppression is high — alternative detection networks are suppressed through resource concentration and publication gating.
constraint_indexing:constraint_classification(supernova_early_warning_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INDEPENDENT RESEARCH GROUPS (TANGLED ROPE) — Constrained by publication requirements and access to large survey data. Benefit from early warning coordination (follow-up science becomes possible) but pay extraction costs: delays in access to public data, dependency on institutional networks for real-time alerts, pressure to conform to publication norms controlled by professional societies. Mixed coordination and extraction.
constraint_indexing:constraint_classification(supernova_early_warning_hierarchy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PROFESSIONAL OBSERVATORIES (ROPE) — Primary beneficiary (institutional/arbitrage). Control the alert hierarchy and first-mover advantage. Experience the constraint as coordination: organizing rapid response among professional facilities enables science. Net beneficiary — publishing priority, resource allocation authority, and prestige flow toward this agent.
constraint_indexing:constraint_classification(supernova_early_warning_hierarchy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FIRST-DISCOVERER INSTITUTIONS (ROPE) — Institutional beneficiary with arbitrage exit. Benefits from priority in accessing public data, citation advantage, and control over follow-up observing time allocation. Experienced as pure coordination — the hierarchy organizes their own first-mover advantage. Low or negative extraction from their structural position.
constraint_indexing:constraint_classification(supernova_early_warning_hierarchy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN ALERT NETWORKS (SCAFFOLD) — Organized agents (LSST, ZTF public stream, Transient Name Server, grassroots networks) building alternative verification and alert pathways that bypass the traditional institutional hierarchy. See the bottleneck as a temporary coordination failure with sunset: distributed, public-access real-time alert systems will replace gatekeeping. Declining theater as automation moves verification to open systems. Sunset estimated within 10-15 years as public survey stream become dominant.
constraint_indexing:constraint_classification(supernova_early_warning_hierarchy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: LEGACY CLASSIFICATION SYSTEM (PITON) — Historical naming and discovery credit conventions (supernova designations, discoverer naming rights) are largely performative and preserved through institutional inertia. Automated surveys and spectroscopic follow-up make manual classification theatricality, yet the ritual persists through convention. High theater_ratio masks diminishing functional role. Maintained by professional prestige rather than scientific necessity.
constraint_indexing:constraint_classification(supernova_early_warning_hierarchy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, information asymmetry in detection is inherent to astronomy: distant events are causally attenuated, and the temporal ordering of discovery vs. occurrence creates irreducible uncertainty. Some hierarchy is 'natural law.' However, the structural data contradicts this — the observed suppression and theater are produced by institutional gatekeeping, not by physics. Engine will flag this as false summit.
constraint_indexing:constraint_classification(supernova_early_warning_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supernova_early_warning_hierarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(supernova_early_warning_hierarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(supernova_early_warning_hierarchy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(supernova_early_warning_hierarchy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(supernova_early_warning_hierarchy, TR),
    TR >= 0.70.

:- end_tests(supernova_early_warning_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The original research group captures genuine first-mover advantage and publishing priority during the discovery-to-publication window. However, the extraction is neither total nor immutable — modern survey automation is reducing the temporal window where institutional gatekeeping has force. The value reflects current institutional control over access, not irreducible physics. Suppression (0.62): Moderate-high. Multiple barriers constrain excluded observers: institutional access to real-time alert feeds, publication norms requiring institutional affiliation for priority claims, spectroscopic confirmation gated through professional facilities, and resource concentration (limited follow-up telescope time). But suppression is not total — some independent observers have succeeded through workarounds, and open data releases are expanding available pathways. Theater ratio (0.68): High and rising. The supernova naming conventions, discoverer credits, and institutional notification protocols are substantially performative. Spectroscopic follow-up is now automated; verification is machine-readable; yet the manual discovery ritual persists through convention. Theater has increased over the interval as automated surveys (LSST, ZTF) made manual first-announcement less central to actual science.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates wide perspectival divergence. The excluded observer sees pure extraction (Snare) — the hierarchy serves no coordination function for them, only gatekeeping. The independent researcher sees mixed coordination and extraction (Tangled Rope) — follow-up science genuinely requires coordination, but institutional control extracts costs. The professional institution sees pure coordination (Rope) — the hierarchy organizes their own prioritization. The open science coalition sees a temporary coordination problem with sunset (Scaffold) — automated systems will replace the institutional hierarchy within a decade. The legacy classification system sees its own degraded function (Piton) — naming conventions persist through inertia despite automation. The civilizational observer risks the false summit — naturalizing information asymmetry in astronomy as immutable. The perspectival gap reveals that the constraint's character depends entirely on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The structural directionality differs dramatically across agents based on their power and exit options. Amateur astronomers (powerless/trapped) experience maximum extraction — they have no alternative pathway to priority claims and cannot organize alternatives. Independent researchers (moderate/constrained) face high costs for exit but can publish through alternative venues and participate in open surveys, giving them partial agency — moderate experienced extraction. Professional institutions (institutional/arbitrage) experience low or negative extraction — they benefit from the hierarchy and can choose when to engage with alternative systems, giving them full agency. The organized open-science coalition (organized/mobile) has explicit exit options through automated surveys and public data, reducing their experienced extraction to low despite the institutional suppression — they see the constraint as surmountable. The analytical observer risks naturalizing institutional arrangements as natural law, but the engine's false summit detector catches the misclassification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that the institutional hierarchy serves dual functions — genuine coordination for resource-constrained follow-up observations, and gatekeeping that extracts priority from non-institutional discoverers. The constraint is legitimately Tangled Rope: it coordinates (follow-up resource allocation requires prioritization) and simultaneously extracts (exclusion from early access, publication gatekeeping, priority denial). The resolution is not a single classification but recognition of the bifurcation: as automated systems mature, the coordination function transfers to open platforms while the extraction mechanism becomes purely institutional rent-seeking. The scaffold perspective captures this trajectory — the sunset is real as automation reduces coordination necessity. The piton perspective captures the theater that will remain after sunset — legacy naming conventions will persist as pure institutional ritual. The mandatrophy is resolved by decomposing: the structural constraint (coordination of limited follow-up resources) is separable from the institutional control mechanism (gatekeeping discovery priority). Future analysis should split these into two stories: 'follow-up resource coordination' (genuine rope, low extraction) and 'discovery priority gatekeeping' (pure snare, high extraction). For now, the tangled rope classification captures the hybrid accurately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_technical_bottleneck,
    'Is the observed hierarchy primarily an institutional gatekeeping mechanism or a genuine technical necessity for coordinating limited observing resources?',
    'Comparison of alert latency with and without professional gatekeeping; analysis of whether independent observers with access to real-time data achieve equivalent follow-up science quality',
    'If technical: reclassify as Rope (pure coordination) from multiple perspectives. If institutional: confirms Snare and Tangled Rope classifications for excluded agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_technical_bottleneck, empirical, 'Whether bottleneck is technical coordination need or institutional gatekeeping').

omega_variable(
    alternative_detection_sufficiency,
    'Do automated survey streams (LSST, ZTF, Pan-STARRS) provide equal detection sensitivity and speed to traditional institutional discovery pathways?',
    'Detection statistics: comparison of completeness curves, latency distributions, and spectroscopic confirmation rates for automated vs. manual discovery workflows over 5-year period',
    'If equivalent or superior: scaffold sunset is real and timeline accelerates. If inferior: institutional hierarchy serves genuine coordination function and extraction is legitimate. If complementary: constraint decomposes into separate technical and institutional stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_detection_sufficiency, empirical, 'Whether automated surveys achieve detection parity with manual pathways').

omega_variable(
    suppression_mechanism_structural_or_cultural,
    'Is the observed suppression (amateur discoveries blocked from priority claims) enforced through explicit policy/gatekeeping or through implicit cultural norms and resource concentration?',
    'Historical analysis of amateur discovery attempts and institutional responses; comparison of discovery rates and publication success rates for institutional vs. non-institutional first observers; survey of explicit vs. implicit barriers',
    'If explicit gatekeeping: enables targeted policy change and faster scaffold transition. If cultural: requires norm change (slower, may require generational turnover). Affects mandatrophy resolution timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_or_cultural, empirical, 'Whether suppression is explicit institutional policy or internalized cultural norm').

omega_variable(
    public_access_readiness,
    'Are open alert systems (Transient Name Server, LSST public data release, community spectroscopy programs) sufficiently mature to replace the traditional hierarchy without significant science loss?',
    'Capability audit: real-time data latency, spectroscopic confirmation turnaround, follow-up resource coordination through open platforms vs. traditional channels',
    'If ready: scaffold transition becomes imminent and theater_ratio should drop below 0.50. If not ready: piton classification confirmed; institution persists through necessity, not pure inertia.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_access_readiness, empirical, 'Maturity of open-access alternatives to institutional hierarchy').

omega_variable(
    identity_locked_professional_astronomy,
    'Is the professional astronomers'' stake in the hierarchy partially an identity lock (career identity, disciplinary prestige) rather than purely structural benefit?',
    'Ethnographic study: interviews with early-career astronomers about perceived consequences of decentralized discovery; analysis of institutional prestige distribution as automation increases',
    'If significant identity lock: professional resistance to scaffold transition will persist beyond technical readiness. Affects implementation timeline for open systems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_professional_astronomy, conceptual, 'Whether professional identity is fused with discovery priority mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supernova_early_warning_hierarchy, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sne_warn_tr_t0, supernova_early_warning_hierarchy, theater_ratio, 0, 0.55).
narrative_ontology:measurement(sne_warn_tr_t7, supernova_early_warning_hierarchy, theater_ratio, 7, 0.62).
narrative_ontology:measurement(sne_warn_tr_t15, supernova_early_warning_hierarchy, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(sne_warn_be_t0, supernova_early_warning_hierarchy, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(sne_warn_be_t7, supernova_early_warning_hierarchy, base_extractiveness, 7, 0.53).
narrative_ontology:measurement(sne_warn_be_t15, supernova_early_warning_hierarchy, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supernova_early_warning_hierarchy, resource_allocation).
narrative_ontology:boltzmann_floor_override(supernova_early_warning_hierarchy, 0.18).
narrative_ontology:affects_constraint(supernova_early_warning_hierarchy, transient_survey_publication_lag).
narrative_ontology:affects_constraint(supernova_early_warning_hierarchy, spectroscopic_follow_up_bottleneck).
narrative_ontology:affects_constraint(supernova_early_warning_hierarchy, institutional_data_access_asymmetry).

% DUAL FORMULATION NOTE:
% The supernova early warning hierarchy is upstream of specific follow-up constraints (spectroscopic bottleneck, publication lag, data access asymmetry). The hierarchy has its own extractiveness reflecting institutional gatekeeping; the downstream constraints have their own extractiveness reflecting publication norms and resource barriers. Decomposition recommended: separate the genuine coordination problem (follow-up resource allocation with low ε) from the institutional control mechanism (discovery gatekeeping with high ε).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supernova_early_warning_hierarchy, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
