% ============================================================================
% CONSTRAINT STORY: algorithmic_amplification_of_extremism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_amplification_of_extremism, []).

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
 *   constraint_id: algorithmic_amplification_of_extremism
 *   human_readable: Algorithmic Amplification of Extremism
 *   domain: digital/social_media/political
 *
 * SUMMARY:
 *   Algorithmic amplification of extremism creates a structural asymmetry
 *   between engagement optimization (the platform's objective function) and
 *   epistemic health (the discourse commons' requirement). Platforms use
 *   engagement-maximizing algorithms to coordinate user attention — a genuine
 *   coordination function. But engagement metrics systematically amplify
 *   extreme content because extreme messaging triggers higher emotional
 *   response. This creates a hybrid constraint: real coordination function
 *   coupled with asymmetric extraction from vulnerable populations and the
 *   discourse integrity commons. The constraint exhibits all spectral types
 *   depending on observer position. From the platform's perspective, it is
 *   pure coordination (Rope). From the vulnerable population's perspective,
 *   it is pure extraction with no exit (Snare). From regulatory institutions'
 *   perspective, it is a mixed coordination-extraction hybrid where the
 *   coordination function is real but largely captured by extraction
 *   imperatives (Tangled Rope). The content moderation theater (high
 *   visibility policies paired with untouched recommendation algorithms)
 *   indicates institutional degradation (Piton). Alternative platforms
 *   represent a genuine sunset pathway if network effects can shift to
 *   transparency-first designs (Scaffold). The analytical observer risks
 *   naturalizing engagement optimization as an immutable law of information
 *   systems (false Mountain), when the choice to optimize for engagement
 *   rather than other objectives is fundamentally contingent. The
 *   constraint's theater ratio has increased over the interval as platforms
 *   have added performative moderation policies while leaving core
 *   amplification mechanisms intact.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture engagement revenue and data value; algorithmic amplification is core to their business model
 *   - Radicalization-Vulnerable Populations: Primary victims (powerless/trapped) — face algorithmic funneling with no meaningful exit; susceptible to progressive exposure to extreme content
 *   - Extremist Content Producers: Secondary beneficiaries (institutional/mobile) — gain reach and audience through algorithmic amplification; benefit from network effects without bearing moderation costs
 *   - Mainstream Content Creators: Secondary victims (moderate/constrained) — face reach disadvantage relative to extreme content; constrained by algorithm's engagement bias
 *   - Regulatory Institutions: Complex position (institutional/constrained) — must coordinate discourse governance but are partly captured by platform interests; face genuine coordination problem alongside extraction
 *   - Discourse Integrity Commons: Victim (powerless/identity_locked) — abstract collective good incrementally contaminated; identity-locked to platforms (no alternative infrastructure); suppression internalized as inevitable
 *   - Alternative Platform Coalition: Organized actors (organized/mobile) — building parallel infrastructure; see constraint as temporary with sunset pathway
 *   - Content Moderation Systems: Institutional actors (institutional/arbitrage) — maintain performative policies; actual algorithms untouched
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_amplification_of_extremism, 0.58).
domain_priors:suppression_score(algorithmic_amplification_of_extremism, 0.65).
domain_priors:theater_ratio(algorithmic_amplification_of_extremism, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_amplification_of_extremism, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_amplification_of_extremism, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(algorithmic_amplification_of_extremism, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_amplification_of_extremism, tangled_rope).
narrative_ontology:human_readable(algorithmic_amplification_of_extremism, "Algorithmic Amplification of Extremism").
narrative_ontology:topic_domain(algorithmic_amplification_of_extremism, "digital/social_media/political").

domain_priors:requires_active_enforcement(algorithmic_amplification_of_extremism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_amplification_of_extremism, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_amplification_of_extremism, extremist_content_producers).
narrative_ontology:constraint_victim(algorithmic_amplification_of_extremism, mainstream_discourse_integrity).
narrative_ontology:constraint_victim(algorithmic_amplification_of_extremism, radicalization_vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE POPULATIONS (SNARE) — Individuals susceptible to extremist messaging face algorithmic funneling with no meaningful exit. Once trapped in recommendation pathways, they experience accelerating exposure to progressively extreme content with minimal counter-messaging. Cannot exit without radical behavior change or platform abandonment.
constraint_indexing:constraint_classification(algorithmic_amplification_of_extremism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MAINSTREAM CONTENT CREATORS (TANGLED ROPE) — Constrained by algorithmic reach disadvantage relative to extreme content; benefit from network effects and platform distribution but must compete against engagement-optimized amplification of extremism. Face career and audience loss if unable to match extreme content's virality.
constraint_indexing:constraint_classification(algorithmic_amplification_of_extremism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATORS (ROPE) — Benefit from engagement amplification (extremist content drives watch time, interaction, and advertising revenue). Coordinate user attention through algorithmic ranking. Experience the constraint as a coordination mechanism: engagement optimization serves genuine platform function of connecting users to relevant content, even though extreme content disproportionately satisfies engagement metrics.
constraint_indexing:constraint_classification(algorithmic_amplification_of_extremism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY INSTITUTIONS (TANGLED ROPE) — Constrained by technical opacity and political pressure from platforms; benefit from platforms' coordination of public discourse infrastructure. Must balance free speech norms against radicalization harms. Face genuine coordination problem (governing digital speech) alongside asymmetric extraction (platforms avoid accountability while appearing cooperative).
constraint_indexing:constraint_classification(algorithmic_amplification_of_extremism, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DISCOURSE INTEGRITY COMMONS (SNARE) — Abstract collective good with identity lock: society's epistemic commons cannot organize or exit; becomes incrementally contaminated by normalized extreme framing. The commons is identity-locked to the platforms themselves — there is no alternative discourse infrastructure, so exit is not physically possible. Suppression is internalized: society has come to accept algorithmic distortion as inevitable.
constraint_indexing:constraint_classification(algorithmic_amplification_of_extremism, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: CONTENT MODERATION SYSTEMS (PITON) — Platform moderation policies are largely performative: they remove visible extremist content but leave recommendation algorithms untouched, allowing covert amplification to persist. Policies exist to satisfy regulatory pressure and public concern, not to change core engagement optimization. The moderation system has degraded from actual governance function to theatrical compliance.
constraint_indexing:constraint_classification(algorithmic_amplification_of_extremism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ALTERNATIVE PLATFORM / DECENTRALIZATION MOVEMENT (SCAFFOLD) — Organized actors (Mastodon, Bluesky, decentralized protocols) are building parallel infrastructure with different algorithmic choices. Theater ratio is lower because alternative platforms can operate with transparency-first design. Sees the constraint as temporary — as alternatives mature and network effects redistribute, the extractive amplification mechanism loses force. Sunset timeline: 5-15 years depending on adoption velocity.
constraint_indexing:constraint_classification(algorithmic_amplification_of_extremism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / TECHNICAL NATURALIZATION (MOUNTAIN) — From a technical perspective, engagement-driven algorithms inevitably amplify extreme content because extreme messaging triggers stronger emotional response and thus higher engagement metrics. This perspective treats algorithmic amplification as an immutable consequence of optimization objectives — a natural law of information systems. However, the structural data reveals this as false summit: the choice to optimize for engagement (rather than accuracy, civility, or epistemic health) is contingent, not natural.
constraint_indexing:constraint_classification(algorithmic_amplification_of_extremism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_amplification_of_extremism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_amplification_of_extremism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_amplification_of_extremism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_amplification_of_extremism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_amplification_of_extremism, TR),
    TR >= 0.70.

:- end_tests(algorithmic_amplification_of_extremism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The platform captures significant value through engagement amplification — increased watch time, data collection, advertising revenue, and network lock-in. The extraction is not total (users benefit from genuine coordination function of content discovery) but substantial and asymmetric. The value has increased over the interval as platforms have optimized algorithms and grown audience dependency. Suppression (0.65): High. Vulnerable populations face severe barriers to exit: platforms are quasi-monopolistic (network effects concentrate users), alternatives lack critical mass, and the engagement amplification is often invisible (no transparent ranking criteria). Victims cannot see how algorithms are targeting them or easily access alternative infrastructure. Theater ratio (0.48): Moderate. Content moderation policies are increasingly theatrical (high-visibility removals paired with untouched recommendation systems), but the primary extraction mechanism is not fully dependent on theater — the algorithmic amplification works regardless of visibility. Theater has increased over the interval as regulatory pressure has mounted.
 *
 * PERSPECTIVAL GAP:
 *   The central perspectival gap is between institutional beneficiaries (platforms) who see coordination and institutional victims (regulators, discourse commons) who see extraction. Both are partly right. Platforms genuinely coordinate user attention (Rope function is real). But the coordination is coupled with asymmetric extraction because engagement metrics amplify content that harms the epistemic commons. The gap reveals that 'genuine coordination' does not preclude 'asymmetric extraction' — the two can coexist in a hybrid constraint. The alternative platforms' Scaffold perspective shows that the coupling is contingent: different coordination mechanisms (transparency, diversity, civility) are technically feasible. The false Mountain perspective (analytical naturalization) would treat engagement optimization as an immutable law; recognizing it as a design choice reveals it as Tangled Rope with a fixable extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) reflects their structural relationship to the amplification mechanism. Platform operators benefit directly from engagement optimization → low d (around 0.10-0.20) → negative effective extraction from their perspective. Vulnerable populations are targets of amplification → high d (around 0.85-0.95) → high effective extraction. Mainstream creators face reach disadvantage → moderate-high d (around 0.60-0.70). Regulators are constrained by capture → moderate d (around 0.55-0.65). The discourse commons has no exit → maximum d (approaching 1.0). The alternative platform coalition has mobile exit options → lower d despite being organized (around 0.35-0.45) because they can coordinate around exiting the constraint. The directionality derivation reveals why different agents experience radically different classifications: the same amplification mechanism runs toward beneficiaries and away from victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely Tangled Rope from the analytical perspective, not a false dichotomy between 'pure coordination' and 'pure extraction.' The platform's engagement optimization is real coordination (solves the problem of connecting users to relevant content) AND asymmetric extraction (amplifies extreme content disproportionately). These are not contradictory — they are structural features of the same mechanism. The classification prevents two errors: (1) calling it pure Rope and ignoring radicalization harms; (2) calling it pure Snare and ignoring that platforms do coordinate attention. Tangled Rope captures both dimensions. The scaffold and piton perspectives reveal temporal structure: as alternative platforms mature and moderation theater becomes visible as degraded, the constraint's character will change. The mountain perspective is a false summit that would naturalize contingent design choices as laws of physics. The mandatrophy analysis shows that this constraint's extractiveness (0.58) is neither inevitable nor immutable — it reflects specific algorithmic design choices that could be changed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engagement_metric_definition,
    'Can platform engagement metrics be redefined to reduce extremism amplification without losing core coordination function?',
    'A/B testing of alternative ranking objectives (epistemic quality, diversity, civility penalties); measurement of user retention, platform growth, and radicalization rates under alternative optimization regimes',
    'If yes: the constraint is a snare with fixable extraction mechanism (Tangled Rope becomes achievable through design change). If no: extremism amplification is inherent to engagement optimization and platforms face genuine coordination-extraction tradeoff.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engagement_metric_definition, empirical, 'Whether engagement metrics can be redesigned to reduce extremism without losing function').

omega_variable(
    content_visibility_independence,
    'Are extremist content''s engagement advantages due to intrinsic audience preference or to algorithmic amplification feedback loops?',
    'Comparison studies: extremist content performance under randomized ranking vs algorithmic ranking; measurement of organic sharing rates vs algorithmic promotion rates; A/B testing with suppressed recommendation weighting',
    'If intrinsic preference: extremism amplification is reflecting genuine audience demand (Rope perspective is accurate). If algorithmic feedback: amplification is extraction mechanism (Snare is accurate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(content_visibility_independence, empirical, 'Whether extremism engagement advantage is intrinsic or algorithmic').

omega_variable(
    radicalization_pathway_sufficiency,
    'Does exposure to extreme content via algorithmic amplification significantly increase radicalization risk compared to baseline susceptibility?',
    'Longitudinal studies tracking content exposure sequences and behavioral radicalization; counterfactual analysis using recommendation algorithm variations; comparison of radicalization rates across platforms with different amplification mechanisms',
    'If significant causal effect: suppression is real and externalized (Snare from victim perspective is accurate). If minimal effect: amplification is incidental to user choice (Rope from platform perspective is accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(radicalization_pathway_sufficiency, empirical, 'Causal effect of algorithmic amplification on radicalization risk').

omega_variable(
    alternative_platform_viability,
    'Can decentralized or non-engagement-optimized platforms achieve critical mass adoption without sacrificing technical functionality?',
    'Tracking adoption curves of Mastodon, Bluesky, and protocol-based alternatives; measurement of network effects and user retention; analysis of whether non-engagement-optimization enables sustainability',
    'If viable: scaffold perspective is structural (sunset is real, not aspirational). If not viable: alternative platforms remain marginal and constraint persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether alternative platforms can achieve critical mass').

omega_variable(
    discourse_integrity_recovery_capacity,
    'If algorithmic amplification were reversed, how quickly would mainstream discourse recover from normalized extreme framing?',
    'Historical analysis of discourse normalization patterns; simulation models of framing adoption and reversal; measurement of rhetorical shift lag after policy changes',
    'If rapid recovery: identity_locked designation is too strong, should be constrained (the epistemic commons can exit). If slow recovery: identity_locked is accurate (normalization persists even after mechanism reversal).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discourse_integrity_recovery_capacity, empirical, 'Recovery timeline for discourse integrity after mechanism removal').

omega_variable(
    regulation_capture_mechanism,
    'Is platform regulatory capture an inherent feature of algorithm-driven amplification or a contingent political outcome?',
    'Comparative analysis of regulatory approaches across jurisdictions; examination of whether technical transparency requirements actually constrain amplification; measurement of enforcement effectiveness',
    'If inherent: regulatory institutions'' constrained perspective is structural (cannot escape without destroying platform coordination). If contingent: stronger regulation could shift regulatory power atom to institutional/arbitrage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulation_capture_mechanism, empirical, 'Whether regulatory capture is inherent to platform structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_amplification_of_extremism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algamp_tr_t0, algorithmic_amplification_of_extremism, theater_ratio, 0, 0.25).
narrative_ontology:measurement(algamp_tr_t5, algorithmic_amplification_of_extremism, theater_ratio, 5, 0.35).
narrative_ontology:measurement(algamp_tr_t10, algorithmic_amplification_of_extremism, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(algamp_be_t0, algorithmic_amplification_of_extremism, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(algamp_be_t5, algorithmic_amplification_of_extremism, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(algamp_be_t10, algorithmic_amplification_of_extremism, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_amplification_of_extremism, information_standard).
narrative_ontology:boltzmann_floor_override(algorithmic_amplification_of_extremism, 0.12).
narrative_ontology:affects_constraint(algorithmic_amplification_of_extremism, political_polarization_spiral).
narrative_ontology:affects_constraint(algorithmic_amplification_of_extremism, extremist_content_virality_advantage).
narrative_ontology:affects_constraint(algorithmic_amplification_of_extremism, platform_regulatory_capture).

% DUAL FORMULATION NOTE:
% Algorithmic amplification of extremism decomposes into multiple structurally distinct constraints: (1) engagement_metric_bias (ε≈0.55, Tangled Rope) — optimization metrics favor extreme content; (2) radicalization_exposure_funnel (ε≈0.68, Snare) — vulnerable populations trapped in progressive exposure; (3) moderation_theater (ε≈0.35, Piton) — policies performative while algorithms unchanged. These stories are linked because engagement metrics drive both the viral advantage and the moderation theater. The present story captures the overarching constraint; decomposition would separate metrics design, population-level harm, and institutional performativity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_amplification_of_extremism, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
