% ============================================================================
% CONSTRAINT STORY: legacy_media_institutional_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legacy_media_institutional_collapse, []).

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
 *   constraint_id: legacy_media_institutional_collapse
 *   human_readable: Legacy Media Institutional Collapse and Information Gatekeeping
 *   domain: media/institutional/political
 *
 * SUMMARY:
 *   The institutional collapse of legacy media represents a structural
 *   transition from a gatekeeping model (centralized editorial authority
 *   controlling information flow) to a decentralized model (distributed
 *   verification, direct-to-audience platforms, open-source credentialing).
 *   This constraint exhibits the full range of DR classifications from
 *   different perspectives: a snare for local newsrooms with no economic
 *   exit, a tangled rope for regional outlets balancing independence and
 *   platform dependency, a rope for legacy corporations managing their own
 *   decline, a scaffold for decentralized information networks building
 *   alternatives with sunset logic, a piton for the editorial authority
 *   institution whose performative credentialing persists through inertia,
 *   and a false mountain for the analytical observer who naturalizes
 *   institutional gatekeeping as a law of information flow. The
 *   extractiveness has increased from 0.28 to 0.58 over the interval as
 *   advertising collapse forces consolidation and financial dependency on
 *   political advertising, while theater has increased from 0.35 to 0.82 as
 *   editorial institutions respond to declining functional authority by
 *   increasing performative credentialing (prestigious bylines, editorial
 *   review visibility, masthead authority). The constraint is not inevitable
 *   decline but a specific institutional arrangement — centralized editorial
 *   gatekeeping bundled with advertising revenue — collapsing under pressure
 *   from economics (programmatic digital advertising), technology
 *   (decentralized verification platforms), and changed audience expectations
 *   (direct source access, crowdsourced fact-checking).
 *
 * KEY AGENTS:
 *   - Local Newsrooms: Primary victim (powerless/trapped) — economic collapse of local news advertising with no alternative revenue source. Suppression is near-total; only exit is acquisition.
 *   - Regional Independent Outlets: Secondary victim (moderate/constrained) — balance editorial independence with platform/advertising dependency. Some agency through alternative revenue but significant extraction through audience reach asymmetry.
 *   - Legacy Media Corporations: Primary beneficiary (institutional/arbitrage) — capture remaining advertising, political access, and aggregation value. Can arbitrage between news-generating and news-commenting functions.
 *   - Political Advertisers: Secondary beneficiary (organized/arbitrage) — benefit from centralized reach and gatekeeping asymmetry. Political advertising dependency creates structural extraction mechanism.
 *   - Decentralized Information Ecosystem: Organized agents (organized/mobile) — SubStack writers, community journalism networks, open-source verification platforms, blockchain-based credentialing. Building alternatives with visible sunset path.
 *   - Audiences/Information Democracy: Victim (powerless/trapped) — depend on legacy gatekeeping for credible information in the short term but increasingly aware of asymmetry and seeking alternatives.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (centralized expertise, credentialing scarcity) as immutable laws of information flow.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legacy_media_institutional_collapse, 0.58).
domain_priors:suppression_score(legacy_media_institutional_collapse, 0.65).
domain_priors:theater_ratio(legacy_media_institutional_collapse, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legacy_media_institutional_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(legacy_media_institutional_collapse, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(legacy_media_institutional_collapse, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legacy_media_institutional_collapse, tangled_rope).
narrative_ontology:human_readable(legacy_media_institutional_collapse, "Legacy Media Institutional Collapse and Information Gatekeeping").
narrative_ontology:topic_domain(legacy_media_institutional_collapse, "media/institutional/political").

domain_priors:requires_active_enforcement(legacy_media_institutional_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legacy_media_institutional_collapse, legacy_media_corporations).
narrative_ontology:constraint_beneficiary(legacy_media_institutional_collapse, political_advertisers).
narrative_ontology:constraint_beneficiary(legacy_media_institutional_collapse, incumbent_power_brokers).
narrative_ontology:constraint_victim(legacy_media_institutional_collapse, independent_journalists).
narrative_ontology:constraint_victim(legacy_media_institutional_collapse, local_news_ecosystems).
narrative_ontology:constraint_victim(legacy_media_institutional_collapse, information_diversity).
narrative_ontology:constraint_victim(legacy_media_institutional_collapse, democratic_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL NEWSROOM (SNARE) — Trapped by advertising collapse, unable to sustain independent reporting. No exit: the economics of local news have shifted irreversibly. Suppression is near-total: acquisition by legacy chains or consolidation is the only path to survival. Experiences pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(legacy_media_institutional_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL INDEPENDENT OUTLET (TANGLED ROPE) — Constrained by network effects and advertising dependency but maintains some editorial independence through alternative revenue (subscriptions, events, grants). Genuine coordination function: aggregates local information that serves community. Asymmetric extraction: must surrender audience reach through platform dependency. High suppression but not absolute — some agencies exist.
constraint_indexing:constraint_classification(legacy_media_institutional_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LEGACY MEDIA CORPORATION (ROPE) — Experiences constraint as coordination of declining but still-valuable information distribution network. Benefits from advertising, political access, and content aggregation. Can arbitrage between news-generating and news-commenting functions. Net beneficiary relative to other actors in the constraint. Theater-heavy but coordination function remains.
constraint_indexing:constraint_classification(legacy_media_institutional_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DECENTRALIZED INFORMATION ECOSYSTEM (SCAFFOLD) — Organized actors (SubStack writers, community journalism networks, open-source verification platforms, social media creators) are building alternative information pathways with built-in sunset logic. Low effective extraction because the coalition has agency and visibility of an exit path. Theater declining as direct-to-audience platforms bypass editorial gatekeeping. Sunset timeline: 10-20 years as decentralized trust networks mature.
constraint_indexing:constraint_classification(legacy_media_institutional_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: EDITORIAL AUTHORITY INSTITUTION (PITON) — The assumption that centralized editorial gatekeeping improves information quality is largely performative. Editorial boards maintain theatrical credentialing (bylines, masthead affiliation, editorial review) despite widespread evidence that distributed verification (crowdsourced fact-checking, open commentary, direct-source access) often detects errors faster and more accurately. The institution persists through inertia and audience habit rather than demonstrated functional superiority. Theater ratio at 0.78 confirms degradation — most editorial work is now managing reputation theater rather than verifying claims.
constraint_indexing:constraint_classification(legacy_media_institutional_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, information gatekeeping through centralized editorial authority appears immutable: complex claims always require credentialing, expertise must be concentrated to be trustworthy, and verification is inherently slow. This perspective naturalizes institutional gatekeeping as a law of information flow. However, structural data contradicts mountain classification — the engine will compute this as a false summit, revealing that 'expertise requires centralization' is actually a contingent institutional arrangement being disrupted by decentralized verification technologies and open-source credentialing.
constraint_indexing:constraint_classification(legacy_media_institutional_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legacy_media_institutional_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legacy_media_institutional_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legacy_media_institutional_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legacy_media_institutional_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legacy_media_institutional_collapse, TR),
    TR >= 0.70.

:- end_tests(legacy_media_institutional_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Legacy media extract value from multiple sources: political advertising dependency (creates incentive to favor incumbent power brokers), audience reach asymmetry (platforms route traffic through legacy outlets), and information gatekeeping (control of credentialing). The extraction is not total — genuine editorial function remains, audiences still benefit from aggregation — but the asymmetry has increased as alternative information sources proliferate and legacy outlets become more dependent on political advertising to offset declining commercial revenue. The v1.0 assessment of 0.72 overstated the extraction by ignoring the genuine coordination function (information aggregation) and the visibility of alternatives. The reduced value reflects that extraction is now salient to audiences and economically unsustainable. Suppression (0.65): High. Barriers to independent information flow include: advertising collapse (commercial and political) making independent journalism economically unviable; platform dependency (Twitter, Facebook, Google control reach and algorithmic visibility); incumbent power broker coordination (political and corporate advertising concentrated in legacy outlets); and epistemic barriers (credentialing scarcity making decentralized alternatives less trusted). Suppression has increased as consolidation proceeds and political advertising becomes the survival mechanism for legacy outlets. Theater ratio (0.78): Very high. Editorial institutions now spend substantially more time maintaining theatrical credibility (prestigious bylines, editorial review visibility, masthead authority, awards ceremonies) than verifying claims through novel reporting. The theater has increased as functional authority has declined — editorial institutions respond to declining trust by increasing performative signals of credibility. The interval measurements show this progression clearly: theater was 0.35 when editorial institutions were still producing substantial original reporting; it has increased to 0.78 as newsrooms shrink and editorial work becomes primarily commentary on and aggregation of information produced elsewhere.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is between agents who see institutional gatekeeping as immutable law (analytical mountain perspective) and agents who see it as contingent institutional arrangement collapsing under economic and technological pressure (organized scaffold perspective with visible exit path). The gap reveals how institutional inertia naturalizes arrangements that are structurally contingent. Local newsrooms cannot see the exit path because the exit requires ecosystem-level coordination (alternative credentialing, decentralized verification, direct-to-audience revenue) that has not yet matured. Legacy institutions see a rope because they are still capturing value, but this perspective ignores the powerless agents (local newsrooms) who see a snare. The analytical observer's mountain perspective is a false summit: it naturalizes the gatekeeping asymmetry as a law of information flow when it is actually an institutional arrangement dependent on advertising revenue and technological scarcity of verification mechanisms, both of which are changing.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by the agent's structural position within the constraint. Local newsrooms have d ≈ 0.95 (full target): they are trapped by economics with no exit, bearing full cost of gatekeeping asymmetry and advertising collapse. Regional outlets have d ≈ 0.70 (partial target): constrained by platform dependency and advertising decline but with some agency through alternative revenue and editorial independence. Legacy media corporations have d ≈ 0.10 (near-full beneficiary): they capture advertising, political access, and aggregation value; they have arbitrage options (subscription, digital, political advertising). Political advertisers have d ≈ 0.15 (beneficiary with leverage): they benefit from centralized reach but also have alternative platforms. Decentralized information ecosystem has d ≈ 0.45 (symmetric): these actors are building alternatives but also constrained by trust-building and network effects. The constraint enforces high suppression (0.65) across all contexts because the asymmetry persists at structural level: centralized reach, credentialing scarcity, political advertising dependency create barriers to exit independent of individual agent position.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED through perspectival decomposition: The constraint is not a single type but a presheaf of types over different observer positions. The false mountain perspective (naturalized gatekeeping) is revealed as such by the structural data: accessibility_collapse is not extreme (0.60, well below the 0.85 threshold for mountain), resistance is not minimal (asymmetry is being actively resisted through decentralized platforms), and extractiveness is context-dependent rather than invariant. The snare perspective (local newsrooms) and rope perspective (legacy corporations) both have merit but are incommensurable — they reflect real structural asymmetries, not disagreement about a single fact. The scaffold perspective (organized exit path) is credible: alternative information pathways are materially developing, and timeline estimates (10-20 years) for decentralized credentialing maturity are empirically grounded. The piton perspective (degraded authority institution) is supported by theater ratio increase: editorial institutions are spending more time on performative credentialing and less on novel reporting as functional authority declines. The mandatrophy resolution is that ALL perspectives are partially correct and their disagreement reflects real structural heterogeneity: the constraint is simultaneously a snare (for powerless agents), a rope (for legacy institutions), a tangled rope (for regional outlets), a scaffold (for organized alternatives), and a degraded institution (for editorial authority) — the classification presheaf itself is the answer, not any single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decentralized_verification_efficacy,
    'Do decentralized verification networks (crowdsourced fact-checking, distributed peer review, community commentary) achieve comparable accuracy and speed to centralized editorial gatekeeping?',
    'Comparative analysis of error detection rates: traditional editorial review vs decentralized verification for identical claims; longitudinal tracking of retraction rates and correction speed across platforms',
    'If decentralized achieves parity or superiority: scaffold perspective confirmed — institutional gatekeeping is contingent, not natural law. If decentralized significantly underperforms: mountain perspective has merit — centralized curation genuinely adds epistemic value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_verification_efficacy, empirical, 'Effectiveness of decentralized verification vs editorial gatekeeping').

omega_variable(
    advertising_market_structural_floor,
    'Is the collapse of legacy media advertising revenue a temporary market failure or a structural shift reflecting genuine reduction in advertising''s information-distribution value?',
    'Analysis of advertiser behavior: are advertisers reallocating to higher-ROI platforms (programmatic digital) or reducing spending overall? Correlation between advertising shifts and product/platform changes.',
    'If temporary failure: legacy media can stabilize through business model innovation (paywall, subscriptions) and remain a rope. If structural shift: advertising collapse is irreversible, and local news becomes snare unless alternative revenue sources mature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(advertising_market_structural_floor, empirical, 'Whether media advertising collapse is temporary or structural').

omega_variable(
    institutional_capture_vs_alignment,
    'To what degree do legacy media serve power brokers because of capture (extraction mechanism) versus genuine editorial alignment (coordination mechanism)? Is the asymmetry extractive or editorial?',
    'Content analysis of coverage patterns relative to advertiser/political donor interests; comparison of coverage decisions made autonomously vs those influenced by external pressure; tracking editorial departures when outlets change ownership',
    'If capture dominant: legacy media are snares for their audiences and tangled ropes for journalists. If alignment dominant: legacy media are ropes — editorial choices reflect genuine coordination with advertiser/political preferences. Classification shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_vs_alignment, empirical, 'Degree of institutional capture vs editorial alignment in legacy media').

omega_variable(
    trust_metric_stability,
    'Are declining trust metrics for legacy media (polls showing trust in journalists/outlets) caused by institutional degradation or by visibility of structural asymmetries that were always present?',
    'Historical comparison: did editorial standards degrade measurably or did audience expectations shift? Correlation between trust decline and specific editorial failures vs correlation with increased audience access to competing information sources.',
    'If institutional degradation: piton classification is correct — function has declined. If visibility shift: mountain perspective correct — the gatekeeping asymmetry was always there, now visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trust_metric_stability, empirical, 'Whether trust decline reflects institutional degradation or shifted visibility').

omega_variable(
    political_advertising_dependency,
    'How much of legacy media''s economic extraction derives from political advertising (campaigns, issue ads) that depends on centralized reach, and how much from general commercial advertising that could be met by decentralized platforms?',
    'Disaggregation of advertising revenue by source (political vs commercial); tracking of advertiser migration away from legacy platforms; correlation between political advertising peaks (election cycles) and legacy media financial health',
    'If political advertising dominant: legacy media become snares for democratic accountability (politicians and donors extract value by controlling information flow). If commercial dominant: institutional decline is market-driven coordination problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_advertising_dependency, empirical, 'Degree of legacy media dependency on political advertising').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legacy_media_institutional_collapse, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legmedia_tr_t0, legacy_media_institutional_collapse, theater_ratio, 0, 0.35).
narrative_ontology:measurement(legmedia_tr_t5, legacy_media_institutional_collapse, theater_ratio, 5, 0.55).
narrative_ontology:measurement(legmedia_tr_t10, legacy_media_institutional_collapse, theater_ratio, 10, 0.78).
narrative_ontology:measurement(legmedia_tr_t15, legacy_media_institutional_collapse, theater_ratio, 15, 0.82).

% Extraction over time
narrative_ontology:measurement(legmedia_be_t0, legacy_media_institutional_collapse, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(legmedia_be_t5, legacy_media_institutional_collapse, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(legmedia_be_t10, legacy_media_institutional_collapse, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(legmedia_be_t15, legacy_media_institutional_collapse, base_extractiveness, 15, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legacy_media_institutional_collapse, information_standard).
narrative_ontology:boltzmann_floor_override(legacy_media_institutional_collapse, 0.12).
narrative_ontology:affects_constraint(legacy_media_institutional_collapse, democratic_accountability_collapse).
narrative_ontology:affects_constraint(legacy_media_institutional_collapse, epistemic_commons_degradation).
narrative_ontology:affects_constraint(legacy_media_institutional_collapse, platform_information_monopoly).

% DUAL FORMULATION NOTE:
% Legacy media institutional collapse is upstream of multiple downstream constraints: democratic accountability depends on news ecosystems capable of exposing wrongdoing; epistemic commons degradation follows from loss of investigative capacity; platform information monopoly is accelerated as legacy outlets consolidate or disappear. Each downstream constraint has its own ε value reflecting empirical status of specific claims about democracy/epistemics/platforms. This story captures the structural institutional constraint (centralized gatekeeping + advertising revenue bundling); downstream stories capture empirical claims about consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legacy_media_institutional_collapse, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
