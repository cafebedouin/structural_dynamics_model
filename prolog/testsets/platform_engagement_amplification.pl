% ============================================================================
% CONSTRAINT STORY: platform_engagement_amplification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_engagement_amplification, []).

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
 *   constraint_id: platform_engagement_amplification
 *   human_readable: Platform Engagement Amplification Through Algorithmic Curation
 *   domain: digital_platforms/social_media/attention_economics
 *
 * SUMMARY:
 *   Platform engagement amplification through algorithmic curation represents
 *   a global-scale constraint operating on billions of users' attention,
 *   cognition, and social coordination. The constraint exhibits the full
 *   spectrum of DR classification: from snare (general users trapped in
 *   attention-capture machinery) to rope (platform operators solving
 *   coordination problems) to scaffold (federated alternatives with sunset
 *   pathways) to piton (content moderation theater). The amplification
 *   algorithm serves a genuine coordination function (connecting dispersed
 *   users at scale) while simultaneously extracting massive value from user
 *   attention and behavioral data. The constraint's extractiveness has
 *   increased over the measurement interval (0.38 → 0.58) as competitive
 *   engagement dynamics intensify and algorithmic sophistication deepens.
 *   Theater ratio has also increased (0.52 → 0.68), reflecting the growth of
 *   performative governance (content moderation, policy statements,
 *   transparency reports) that operates independent of the
 *   engagement-optimization mechanisms driving real user behavior.
 *
 * KEY AGENTS:
 *   - General Users: Primary victims (powerless/trapped) — dependent on platforms for social coordination; cannot exit without severing social ties
 *   - Identity-Locked Users: Primary victims (powerless/identity_locked) — structurally mobile but identity-fused; cannot exit without abandoning constructed social identity
 *   - Content Creators: Mixed position (moderate/constrained) — benefit from amplification reach but suppressed by algorithmic opacity and career contingency
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — solve scaling coordination problem; capture attention value and behavioral data; experience negative extraction
 *   - Advertisers: Secondary beneficiary (powerful/arbitrage) — gain access to high-intent users at unprecedented efficiency; pure coordination from advertiser perspective
 *   - Content Moderation System: Performative actor (institutional/constrained) — maintains theater of safety governance while engagement algorithm operates under opposite incentives
 *   - Regulatory Institution: Constrained actor (institutional/constrained) — coordinates baseline constraints on platforms but lacks technical capacity and political leverage for enforcement
 *   - Federated Alternative Ecosystem: Organized agents (organized/mobile) — building parallel infrastructure with algorithmic transparency; perceive current constraint as temporary
 *   - Analytical Observer: Systemic view (analytical/analytical) — observes constraint as cognitive autonomy extraction at civilizational scale; risks naturalizing designed mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_engagement_amplification, 0.58).
domain_priors:suppression_score(platform_engagement_amplification, 0.65).
domain_priors:theater_ratio(platform_engagement_amplification, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_engagement_amplification, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_engagement_amplification, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(platform_engagement_amplification, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_engagement_amplification, tangled_rope).
narrative_ontology:human_readable(platform_engagement_amplification, "Platform Engagement Amplification Through Algorithmic Curation").
narrative_ontology:topic_domain(platform_engagement_amplification, "digital_platforms/social_media/attention_economics").

domain_priors:requires_active_enforcement(platform_engagement_amplification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_engagement_amplification, platform_operators).
narrative_ontology:constraint_beneficiary(platform_engagement_amplification, content_creators_with_algorithmic_access).
narrative_ontology:constraint_beneficiary(platform_engagement_amplification, advertisers).
narrative_ontology:constraint_victim(platform_engagement_amplification, general_user_base).
narrative_ontology:constraint_victim(platform_engagement_amplification, user_attention_autonomy).
narrative_ontology:constraint_victim(platform_engagement_amplification, information_ecosystem_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERAL USER (SNARE) — Structurally dependent on platforms for social connection, news, and coordination. Cannot meaningfully exit without severing social ties. Algorithm amplifies engagement-optimized content (often outrage, novelty, social comparison) that maximizes time-on-platform rather than user autonomy or information quality. Suppression operates through network effects (everyone else is on the platform) and switching costs (social capital loss). Experienced extractiveness is maximum: user's attention is extracted and resold to advertisers; user's behavioral data is harvested; user's cognitive autonomy is compromised by attention-capture mechanisms.
constraint_indexing:constraint_classification(platform_engagement_amplification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IDENTITY-LOCKED USER (SNARE) — User's identity is constituted through social media presence: professional networking, subcultural membership, family relationship maintenance, self-expression architecture. User is structurally mobile (could delete account; has alternative communication channels) but identity-locked — exiting would require abandoning the social identity constructed within the platform. Cannot think exit from within the identity frame because the identity IS the frame. The algorithm amplifies identity-confirming content, creating reinforcement loops that deepen the lock. Experienced extractiveness approaches maximum because even the internal escape route (changing identity frame) is cut off by design-induced identity fusion.
constraint_indexing:constraint_classification(platform_engagement_amplification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: CONTENT CREATOR (TANGLED ROPE) — Receives genuine coordination benefits from the platform: audience reach, monetization, community building. Algorithm amplification enables discovery. BUT: access to amplification is opaque and contingent on algorithmic favor, creating extraction layer. Creator must optimize for the algorithm's engagement metrics, suppressing authentic expression in favor of algorithmic-friendly formats. Career is contingent on maintaining algorithmic goodwill — platform can reduce reach without explanation. Extraction is real but mixed with genuine coordination benefit. Cost of exit is high (audience loss, income disruption) but non-infinite — many creators can migrate. Moderate experienced extraction: benefits are real, costs are high, exit is constrained.
constraint_indexing:constraint_classification(platform_engagement_amplification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Genuine coordination function: connecting users at massive scale, enabling communication across geography and culture. Algorithm amplification is a scaling mechanism — it allows the platform to surface relevant content in the attention economy. Operator experiences the constraint as pure coordination: solving the attention allocation problem while maintaining user engagement. Net beneficiary across all metrics: revenue from advertising, network effects from engagement, data value from behavioral harvesting. No suppression experienced. Negative effective extraction (constraint subsidizes this agent). Exit option: arbitrage — can shift business model without existential threat.
constraint_indexing:constraint_classification(platform_engagement_amplification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ADVERTISER (ROPE) — Direct beneficiary of engagement amplification. Algorithm targets high-intent users at scale. Coordinated access to attention at unprecedented efficiency. No suppression — can walk away and use alternative channels. Exit option: arbitrage — can shift budget to other platforms or media. Experienced extractiveness is negative: the constraint subsidizes the advertiser's access to attention. Pure coordination from advertiser perspective.
constraint_indexing:constraint_classification(platform_engagement_amplification, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONTENT MODERATION SYSTEM (PITON) — Maintains performative compliance with community standards while the engagement algorithm operates under opposite incentives (amplifying outrage, divisiveness, boundary-pushing content). Theater ratio is high: moderation reviews, appeals processes, policy statements create appearance of safety governance while algorithmic amplification actively undermines stated values. Moderation cost grows but functional impact on engagement-optimization declines. System persists through institutional inertia: required for regulatory legitimacy and brand protection, but real verification function has atrophied. Theater dominates function — piton classification.
constraint_indexing:constraint_classification(platform_engagement_amplification, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: REGULATORY INSTITUTION (TANGLED ROPE) — Constrains platform behavior through platform-as-infrastructure requirements (data protection, content liability, algorithmic transparency) but is also constrained by the platforms' economic and political power. Genuine coordination function: establishing baseline behavioral boundaries for platforms serving billions. BUT: regulatory capacity is outpaced by platform scale and technical complexity; enforcement mechanisms are weak; platforms can arbitrage across jurisdictions. Regulatory institution experiences both constraint (limited enforcement power, high technical barriers to verification) and extraction (time-on-task to police each platform scales with user base). Mixed extraction: some coordination benefit (platform provides social infrastructure), but subject to high operational cost and technical capture by platform experts. Exit option: constrained — reducing platform regulation withdraws coordination benefit, but can shift enforcement strategy.
constraint_indexing:constraint_classification(platform_engagement_amplification, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: FEDERATED ALTERNATIVES (SCAFFOLD) — Organized actors (open-source networks like Mastodon, Bluesky's decentralized protocol, local community networks) are building alternative social infrastructure with algorithmic transparency and user control as core design features. Low effective extraction because these agents see a clear exit path: as federation protocols mature and network effects shift, the extraction mechanism of centralized amplification becomes obsolete. Experienced extraction is low (mobile exit option), and the constraint itself is perceived as temporary (sunset horizon ~10-15 years as network effects equilibrate). This perspective confirms scaffold classification from base properties.
constraint_indexing:constraint_classification(platform_engagement_amplification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (SNARE) — From civilizational scope, platform engagement amplification represents a constraint on human cognitive autonomy at scale: the extraction of attention and behavioral data, the compression of information diversity, the systematic distortion of social epistemic commons toward engagement optimization rather than truth-seeking. The constraint is not natural law — it is a contingent institutional design choice (algorithms could be designed for quality, diversity, or user autonomy) — but from the analytical perspective observing the global system, it functions like a snare: the cognitive autonomy of billions is trapped in attention-capture machinery with suppression so thorough (network effects, behavioral addiction by design, switching costs, identity fusion) that exit appears impossible. The false summit would be claiming this is inherent to technology or human psychology; the accurate classification is that this is a designed extraction mechanism with sufficient institutional power to appear natural.
constraint_indexing:constraint_classification(platform_engagement_amplification, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_engagement_amplification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_engagement_amplification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_engagement_amplification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_engagement_amplification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_engagement_amplification, TR),
    TR >= 0.70.

:- end_tests(platform_engagement_amplification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximal. The algorithm extracts user attention (harvested and resold to advertisers) and behavioral data (aggregated into predictive models). User autonomy over information consumption is suppressed through algorithmic filtering and reinforcement-loop design. However, extraction is not total snare-level (≥0.66) because: (1) coordination benefit is genuine (users do gain social connection value), (2) users retain some constrained agency (can adjust time-on-platform, curate followings, use privacy features), (3) alternative platforms exist (constrained exit option, not trapped). Suppression (0.65): High. Network effects (all social contacts on platform) and behavioral addiction by design create strong suppression. Switching costs include social capital loss, communication channel disruption, and identity reconstruction. Informational suppression: algorithmic curation limits information diversity; engagement-optimization amplifies outrage and novelty over breadth. Theater ratio (0.68): High and rising. Content moderation maintains appearance of safety governance while engagement algorithm amplifies the content moderation flags against (divisiveness, boundary-pushing, outrage). Transparency initiatives (algorithmic explainability reports, algorithmic audits, algorithmic choice tools) create theater without fundamentally changing extraction mechanisms. As platforms respond to regulatory pressure with performative governance, theater ratio increases while functional constraint change stagnates.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence here is driven by position in the extraction flow and differential exit options. Platform operators experience the constraint as pure coordination because they set the rules and capture the value. Users experience it as extraction because they pay the attention cost with no negotiating power. The critical gap separates those who can exit (powerful/arbitrage: advertisers, some creators) from those who cannot (powerless/trapped: general users). The secondary gap distinguishes structural mobility from identity lock: users who are structurally mobile (could download an alternative app) but identity-locked (cannot imagine themselves off the platform) experience different constraints than users who have rejected identity fusion. The regulatory institution occupies an instructive middle position: ostensibly powerful but constrained by technical asymmetry and political economy, experiencing mixed extraction while claiming to coordinate.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation maps each agent's power level and exit options to a d-value that feeds into f(d) to compute experienced chi. This is crucial for the platform constraint because it shows that the same base extractiveness (0.58) produces vastly different experienced extraction depending on position: Platform operators (d ≈ 0.08, f(d) ≈ -0.10): chi ≈ -0.06 (negative: constraint subsidizes them). General users (d ≈ 0.90, f(d) ≈ 1.32): chi ≈ 1.01 (maximum: constraint extracts severely). The beneficiary-victim asymmetry is baked into the classification not through different ε values but through differentiated d-values derived from the same structural position. This is why platform constraints naturally produce snare classification for users and rope classification for operators — not because the mechanisms are different (they operate on the same infrastructure) but because the agents occupy opposite positions in the extraction pipeline.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through perspectival differentiation. A naive observer might say: 'Is platform engagement amplification coordination (rope) or extraction (snare)?' The mandatrophy answer is: 'Both, depending on perspective. It solves a genuine coordination problem (connecting dispersed users at scale), which makes it rope from the beneficiary's position. Simultaneously, it extracts from users' attention and autonomy, which makes it snare from the victim's position. The constraint is tangled_rope at the institutional level (platform operators experience both coordination benefit and asymmetric extraction to users) because they actively design the extraction layer into the coordination mechanism. Fake snare (fraud): would claim to be connecting users while deliberately causing harm. Fake rope (naive coordination): would claim extraction is incidental to coordination. This constraint is honest tangled_rope: coordination function is real; extraction layer is intentional; suppression is high enough to ensure users cannot renegotiate terms.' The analytical observer must resist the false mountain ('this is inherent to social technology') and the false piton ('this is just how platforms work'). The constraint is designed and maintainable; its persistence depends on suppression mechanisms, not natural law or institutional inertia. This distinguishes tangled_rope from both mountain and piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engagement_optimization_threshold,
    'What level of algorithmic optimization for engagement becomes extractive rather than coordinative?',
    'Comparative analysis of engagement metrics vs user autonomy metrics (user-reported agency, attention control, information diversity consumed). Threshold at which increasing engagement correlates with decreasing autonomy indicators.',
    'If threshold is low (engagement/autonomy correlation negative at current levels): snare classification confirmed for all user perspectives. If threshold is high (significant autonomy remains despite high engagement): tangled_rope or rope classification more accurate for some perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engagement_optimization_threshold, empirical, 'Threshold distinguishing engagement coordination from attention extraction').

omega_variable(
    algorithmic_transparency_sufficiency,
    'Can user-comprehensible algorithmic transparency (explainable AI, tunable preferences, audit logs) reduce extraction below the suppression floor, converting snare to tangled_rope?',
    'Randomized control trials on transparency interventions: user autonomy measures, attention distribution, engagement metrics, reported control perception. Comparison to baseline opaque algorithms.',
    'If effective: transparency mechanisms can reduce experienced suppression, enabling users to exercise constrained exit. Snare → tangled_rope reclassification. If ineffective: transparency is theater (piton degradation); suppression remains structural; snare classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transparency_sufficiency, empirical, 'Whether algorithmic transparency reduces extraction').

omega_variable(
    identity_lock_cognitive_mechanism,
    'Is identity fusion with social media presence a structural property of networked identity or a contingent design choice amplified by platform incentives?',
    'Historical comparison: identity fusion rates on platforms with different algorithmic incentive structures (identity-optimizing vs diversity-optimizing algorithms). Cross-cultural variation in fusion depth by platform design. Measurement of identity-lock persistence post-exit (does lock release or does it persist internally?).',
    'If structural: identity_locked exit option is intractable; users cannot exit without identity reconstruction regardless of platform design. If design-contingent: identity_locked classification reflects platform choice, not human nature. Different platform architecture could reduce lock strength, enabling mobile or constrained exit options.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_cognitive_mechanism, conceptual, 'Whether identity fusion is structural or design-contingent').

omega_variable(
    collective_action_threshold_for_powerless,
    'Can general users achieve sufficient coordination to negotiate platform terms or migrate collectively, converting powerless to organized?',
    'Analysis of user coalition formation attempts (digital rights advocacy, platform strikes, migration coordination). Measurement of coordination capacity vs platform scale asymmetry. Historical cases where user coalitions achieved renegotiation of platform terms.',
    'If threshold is insurmountable (platform scale and network effects prevent user coordination): powerless/trapped classification persists. If threshold is reachable (critical mass coalition can shift bargaining power): powerless → organized reclassification possible; experienced extraction reduces; snare → tangled_rope or rope conversion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_threshold_for_powerless, empirical, 'User coalition formation capacity and critical mass threshold').

omega_variable(
    regulatory_capture_mechanism,
    'Is regulatory institution''s constrained exit position due to technical capacity asymmetry or political economy capture by platforms?',
    'Comparative analysis of regulatory outcomes across jurisdictions (EU DMA vs FTC vs national frameworks). Measurement of regulatory agency independence (funding sources, revolving door patterns, technical expertise gaps). Counterfactual: would regulatory outcome change with 10x technical capacity increase?',
    'If technical asymmetry: tangled_rope classification accurate for regulatory institution. Capacity investment could improve enforcement. If political capture: regulatory classification approaches snare or scaffold (captured by platforms) or rope (captured by different interest). Fundamental institutional redesign required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Whether regulatory constraint is technical or political-economic').

omega_variable(
    federation_network_effect_transition,
    'At what network effect threshold does federated social infrastructure become viable alternative to centralized platforms?',
    'Network size measurement for federated platforms. Critical mass analysis: minimum users needed for federation to provide equivalent value (discovery, social density, resource availability). Comparison to historical technology transitions (email federation maturation, open-source ecosystem growth).',
    'If transition feasible within 10-15 years: scaffold classification confirmed; sunset path is real; long-term constraint is temporary. If transition requires 50+ years or is fundamentally blocked by network effect lock-in: scaffold sunset is aspirational; long-term classification approaches piton (Theater-driven persistence rather than functional necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federation_network_effect_transition, empirical, 'Network effect threshold for federated platform viability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_engagement_amplification, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pea_tr_t0, platform_engagement_amplification, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pea_tr_t5, platform_engagement_amplification, theater_ratio, 5, 0.62).
narrative_ontology:measurement(pea_tr_t10, platform_engagement_amplification, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(pea_be_t0, platform_engagement_amplification, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(pea_be_t5, platform_engagement_amplification, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(pea_be_t10, platform_engagement_amplification, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_engagement_amplification, resource_allocation).
narrative_ontology:boltzmann_floor_override(platform_engagement_amplification, 0.18).
narrative_ontology:affects_constraint(platform_engagement_amplification, attention_economy_concentration).
narrative_ontology:affects_constraint(platform_engagement_amplification, behavioral_data_extraction).
narrative_ontology:affects_constraint(platform_engagement_amplification, information_ecosystem_polarization).
narrative_ontology:affects_constraint(platform_engagement_amplification, content_creator_labor_extraction).

% DUAL FORMULATION NOTE:
% Platform engagement amplification is the upstream constraint in a family of social media extraction mechanisms. Downstream constraints (attention concentration, data harvesting, polarization, creator labor extraction) are each distinct structurally but all depend on the amplification algorithm as their enabling mechanism. The base extractiveness (0.58) represents the amplification constraint proper; downstream stories decompose specific extraction modalities with their own ε values and victim groups.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_engagement_amplification, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
