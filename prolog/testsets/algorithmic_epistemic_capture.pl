% ============================================================================
% CONSTRAINT STORY: algorithmic_epistemic_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_epistemic_capture, []).

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
 *   constraint_id: algorithmic_epistemic_capture
 *   human_readable: The Feedback Loop Blindfold: Algorithmic Epistemic Capture
 *   domain: technological/social/cognitive
 *
 * SUMMARY:
 *   Algorithmic epistemic capture describes the constraint where an
 *   individual's informational worldview is entirely mediated by machine
 *   learning systems optimizing for engagement rather than accuracy. The
 *   captured user cannot perceive the algorithmic curation shaping their
 *   epistemic diet, cannot audit the mechanism, and cannot exit without
 *   sacrificing social participation. Simultaneously, the platform,
 *   advertisers, and engagement-optimizing systems experience this as a
 *   coordination mechanism — efficiently matching users to content and
 *   advertisers to audiences. The constraint exhibits all six DR types from
 *   different structural positions, revealing how the same mechanism appears
 *   as natural law (inevitable information filtering), coordination
 *   (platform-user matching), temporary problem with regulatory sunset
 *   (scaffold), degraded institutional gatekeeping (piton), mixed
 *   coordination-extraction hybrid (tangled rope), and pure extraction
 *   (snare), depending on whether the observer has algorithmic transparency,
 *   exit capacity, and epistemic agency. The theater ratio (0.68) reflects
 *   that algorithmic recommendation systems increasingly rely on performance
 *   metrics (engagement, click-through rate, watch time) that are decoupled
 *   from information accuracy or user epistemic benefit. The mechanism
 *   appears neutral and technical ('we optimize for what users engage with')
 *   while obscuring the preference for sensationalism, polarization, and
 *   false certainty embedded in engagement metrics themselves.
 *
 * KEY AGENTS:
 *   - Captured User: Primary victim (powerless/trapped) — entire information diet algorithmically mediated, no transparency, no exit without social cost
 *   - Engagement-Optimizing Platform: Primary beneficiary (institutional/arbitrage) — controls algorithm, harvests attention and behavioral data, arbitrages user data to advertisers
 *   - Advertiser Coalition: Secondary beneficiary (powerful/mobile) — accesses precision-targeted audiences, extracts behavioral predictions, can exit platform via platform switching
 *   - Independent News Organization: Secondary victim (moderate/constrained) — must optimize for algorithmic visibility, benefits from distribution while being constrained by algorithmic gatekeeping
 *   - Regulatory Intervention Coalition: Organized agent (organized/constrained) — EU DSA, algorithmic transparency mandates, building interoperability standards with sunset logic
 *   - Legacy Editorial Institution: Institutional actor (institutional/arbitrage) — maintains performative credentialing function despite algorithmic bypass; piton-class degraded gatekeeping
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing engagement optimization as inevitable filtering rather than engineered extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_epistemic_capture, 0.58).
domain_priors:suppression_score(algorithmic_epistemic_capture, 0.72).
domain_priors:theater_ratio(algorithmic_epistemic_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_epistemic_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_epistemic_capture, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(algorithmic_epistemic_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_epistemic_capture, tangled_rope).
narrative_ontology:human_readable(algorithmic_epistemic_capture, "The Feedback Loop Blindfold: Algorithmic Epistemic Capture").
narrative_ontology:topic_domain(algorithmic_epistemic_capture, "technological/social/cognitive").

domain_priors:requires_active_enforcement(algorithmic_epistemic_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_epistemic_capture, engagement_optimizing_platforms).
narrative_ontology:constraint_beneficiary(algorithmic_epistemic_capture, attention_extraction_advertisers).
narrative_ontology:constraint_victim(algorithmic_epistemic_capture, user_epistemic_autonomy).
narrative_ontology:constraint_victim(algorithmic_epistemic_capture, epistemic_commons_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CAPTURED USER (SNARE) — User's entire information diet is algorithmically curated by systems they cannot see, audit, or exit without sacrificing social participation. Cannot perceive alternative frames or foundational claims. High suppression (no algorithm transparency), high extraction (attention and behavioral data harvested continuously), no coordination benefit. Maximum experienced extraction relative to this agent's position.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT NEWS ORGANIZATION (TANGLED ROPE) — Constrained by algorithmic visibility: must optimize editorial voice for engagement metrics to reach audience. Coordinating function: platforms enable information distribution. Extraction function: platforms extract distribution value and audience attention. Mixed experience — genuine coordination benefit (reach) paired with asymmetric extraction (data harvesting, distribution gatekeeping). Exit requires abandoning primary distribution channel but is theoretically possible.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENGAGEMENT-OPTIMIZING PLATFORM (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: connecting users to content and advertisers. No suppression from platform's perspective (full control over algorithm). Effective extraction runs toward the platform, not away. Net beneficiary with arbitrage options (can deploy same algorithmic infrastructure for alternative purposes). Theater present but functional from platform perspective.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISER COALITION (TANGLED ROPE) — Powerful agents with mobile exit options (can shift platforms). Benefit from algorithmic targeting (coordination function: precision matching of products to users). Extract value through behavior prediction and attention capture. Moderate extraction because these agents have exit capacity and can negotiate with platforms. Suppress alternatives through programmatic bidding that crowds out non-optimized content.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY INTERVENTION COALITION (SCAFFOLD) — Digital Services Act, Digital Markets Act, algorithmic transparency mandates. Organized agents (civil society, regulatory bodies) see the capture as a temporary coordination failure with sunset clause: transparency requirements, algorithmic auditing, interoperability mandates are building alternative information pathways. Low effective extraction because the coalition has agency and sees regulatory exit path (forced platform change or algorithmic modification). Theater moderate: regulatory compliance involves performative transparency (explainability that obscures rather than illuminates).
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY EDITORIAL INSTITUTION (PITON) — Traditional editorial gatekeeping (editorial boards, fact-checking standards) persists as performative ritual. Institutional inertia maintains the credentialing function despite algorithmic information flow bypassing it entirely. Theater ratio high (0.68): editorial review survives in legacy media but has lost information priority. Degraded function: editorial standards no longer determine what most users see. Maintained through professional identity and institutional funding, not because it functionally excludes false information.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some epistemic asymmetry is inherent to mediated information systems: algorithms must filter; filtering always produces bias; users cannot verify all claims themselves. This perspective sees the bottleneck as structural to human cognition and information scarcity. Engine false summit detection will identify this as naturalization of a contingent institutional arrangement — the extraction mechanism is engineered, not inevitable.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_epistemic_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_epistemic_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_epistemic_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_epistemic_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_epistemic_capture, TR),
    TR >= 0.70.

:- end_tests(algorithmic_epistemic_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The platform and advertisers extract substantial value from user attention and behavioral data. The trajectory from 0.35 to 0.58 reflects increasing sophistication of engagement optimization and data harvesting. However, not maximal snare because: (1) some users develop algorithmic literacy and regain partial agency; (2) regulatory interventions are beginning to mandate transparency; (3) news organizations and civil society are building alternative distribution channels. The measurement shows extraction accumulating over the interval, consistent with tangled_rope classification (coordination function + asymmetric extraction). Suppression (0.72): High. Multiple barriers prevent user exit: (1) social network effects (everyone uses platform); (2) algorithmic opacity (users cannot see the mechanism); (3) false transparency (explainability systems often obscure rather than clarify); (4) career/social costs of non-participation. Suppression increased over interval as algorithmic opacity became more sophisticated and network effects deepened. Theater ratio (0.68): High-moderate. Algorithmic recommendations perform the function of connecting users to content, but increasingly rely on performative metrics (engagement, watch time) decoupled from accuracy. Algorithmic explainability (required by EU DSA) is often theatrical: explaining why an algorithm recommended something often obscures the preference structure optimizing for sensationalism. Theater increased over interval as regulatory compliance compliance added performative transparency layers.
 *
 * PERSPECTIVAL GAP:
 *   Massive perspectival gap between platform's experience (rope: coordination benefit, low chi) and captured user's experience (snare: pure extraction, high chi). Platform sees users + advertisers + content distributed efficiently. Users see their worldview constrained by invisible mechanisms. News organizations experience tangled rope: gain distribution reach while losing editorial control to algorithmic metrics. Regulatory coalition sees temporary failure with sunset: transparency mandates and interoperability standards will build alternatives. Legacy media sees its gatekeeping function degraded to piton-class performance (still maintained, no longer functional). Analytical observer at civilizational timescale risks seeing this as inevitable information filtering (mountain) rather than as engineered extraction mechanism optimizing for engagement rather than accuracy. The perspectival gaps reveal that 'algorithmic recommendation' is not a single constraint but a multi-agent extraction game with different structural positions generating radically different classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to the extraction flow. Captured users: beneficiary = false, victim = true, exit = trapped → d ≈ 0.95 → high f(d) ≈ 1.42 → maximum experienced extraction (snare classification). Platform: beneficiary = true, victim = false, exit = arbitrage → d ≈ 0.05 → low f(d) ≈ -0.12 → negative experienced extraction (rope classification from platform's perspective). Advertisers: beneficiary = true, victim = false, exit = mobile → d ≈ 0.15 → f(d) ≈ -0.01 → low extraction (rope classification). News organizations: beneficiary = partial (distribution reach), victim = partial (algorithmic gatekeeping), exit = constrained → d ≈ 0.55 → f(d) ≈ 0.75 → moderate extraction (tangled rope classification). Regulatory coalition: beneficiary = false, victim = no, exit = constrained → d ≈ 0.60 → f(d) ≈ 0.85 → moderate extraction (scaffold classification because exit is real but requires coordination). Analytical observer: d ≈ 0.72 → f(d) ≈ 1.15 → mountain classification is perspectival (risks naturalizing contingent arrangements).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy by clarifying that algorithmic epistemic capture contains BOTH genuine coordination (platform connecting users to content, advertisers to audiences) AND asymmetric extraction (attention harvesting, behavioral data monetization, algorithmic opacity). The tangled rope classification integrates both functions: the platform's engagement optimization algorithm simultaneously coordinates information matching AND extracts value through behavioral prediction and engagement maximization. The suppression (0.72) and beneficiary/victim declarations confirm the asymmetry. The regulatory scaffold (EU DSA, algorithmic transparency, interoperability mandates) provides the real structural feature: alternative information pathways that reduce platform dependence are genuinely being built, giving the constraint a sunset. The snare classification from the captured user's perspective is NOT the constraint's true type — it's one valid perspectival reading from a powerless/trapped position. The piton classification (legacy editorial gatekeeping) identifies performative institutional survival. The mountain classification (analytical/civilization view) is a false summit: engagement optimization is not an inevitable law of information but an engineered extraction mechanism. The mandatrophy resolves by showing that all six readings are structurally correct from their respective positions — the presheaf over the constraint IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    user_metacognitive_awareness_threshold,
    'At what point does algorithmic mediation become so complete that users lose capacity to recognize they are mediated?',
    'Cross-cultural studies of user algorithm literacy; surveys measuring users'' ability to identify algorithmic curation vs. organic information flow; longitudinal tracking of metacognitive decline in heavily-mediated populations',
    'If threshold low (< 30% awareness): epistemic capture approaches totality (Snare becomes inevitable). If threshold high (> 70% awareness): some users retain epistemic agency (constrained rather than trapped exit). Classification sensitivity: determines whether powerless_trapped perspective dominates or is mitigated by distributed awareness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_metacognitive_awareness_threshold, empirical, 'User capacity to recognize algorithmic mediation').

omega_variable(
    algorithmic_transparency_effectiveness,
    'Do transparency and auditability requirements (EU DSA algorithmic auditing, explainability systems) actually reduce epistemic capture or merely provide theatrical compliance?',
    'Comparative analysis of audit findings vs. platform behavior change; user studies on comprehension and usefulness of explainability systems; measurement of behavioral change in users receiving transparency reports',
    'If effective: scaffold perspective confirmed — regulatory sunset is structural. If theatrical: transparency becomes piton-class performative requirement. Classification consequence: determines whether regulatory coalition escape is real or aspirational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_transparency_effectiveness, empirical, 'Whether algorithmic transparency mandates reduce epistemic capture').

omega_variable(
    distributed_alternative_viability,
    'Can decentralized, non-algorithmic information distribution (email newsletters, RSS aggregators, community curation) scale to compete with algorithmic platforms, or are network effects structurally insurmountable?',
    'Historical adoption data for alternative platforms (Mastodon, Bluesky, independent media hubs); network analysis of reach and information velocity in federated systems vs. algorithmic platforms; user retention and engagement comparison',
    'If viable: tangled_rope classification holds (exit remains constrained but real). If insurmountable: snare classification deepens (exit becomes trapped). Affects all constrained/mobile exit option perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_alternative_viability, empirical, 'Viability of decentralized alternatives to algorithmic platforms').

omega_variable(
    engagement_optimization_inherent_bias,
    'Is engagement optimization algorithmically neutral (technical artifact) or inherently biased toward sensationalism, polarization, and false certainty?',
    'Algorithmic audit comparing engagement signals for high-accuracy vs. high-sensationalism content; controlled experiments on user engagement with algorithmically-ranked truthful vs. false claims; analysis of engagement metrics for different claim types',
    'If neutral: suppression is contingent (could be re-engineered). If inherent: suppression is structural to engagement optimization (optimization itself becomes the extraction mechanism). Affects baseline ε and whether alternatives are possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engagement_optimization_inherent_bias, empirical, 'Whether engagement optimization inherently favors misinformation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_epistemic_capture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algec_tr_t0, algorithmic_epistemic_capture, theater_ratio, 0, 0.42).
narrative_ontology:measurement(algec_tr_t5, algorithmic_epistemic_capture, theater_ratio, 5, 0.58).
narrative_ontology:measurement(algec_tr_t10, algorithmic_epistemic_capture, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(algec_be_t0, algorithmic_epistemic_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algec_be_t5, algorithmic_epistemic_capture, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(algec_be_t10, algorithmic_epistemic_capture, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_epistemic_capture, information_standard).
narrative_ontology:affects_constraint(algorithmic_epistemic_capture, attention_economy_extraction).
narrative_ontology:affects_constraint(algorithmic_epistemic_capture, behavioral_data_commodification).
narrative_ontology:affects_constraint(algorithmic_epistemic_capture, platform_regulatory_capture).

% DUAL FORMULATION NOTE:
% Algorithmic epistemic capture is downstream of specific engagement optimization mechanisms (content ranking, recommendation algorithms, behavioral prediction) and upstream of their social consequences (polarization, misinformation spread, epistemic inequality). The constraint represents the structural coupling between technical optimization and social extraction — engineered at the platform level, experienced as epistemic constraint at the user level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_epistemic_capture, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
