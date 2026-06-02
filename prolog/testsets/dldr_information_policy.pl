% ============================================================================
% CONSTRAINT STORY: dldr_information_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dldr_information_policy, []).

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
 *   constraint_id: dldr_information_policy
 *   human_readable: Don't Like, Don't Read (DLDR) Information Policy
 *   domain: technological/social
 *
 * SUMMARY:
 *   The 'Don't Like, Don't Read' (DLDR) information policy represents a
 *   structural constraint that has evolved from an explicit preservation
 *   principle in Archive of Our Own (AO3) — protecting fanfiction and
 *   marginalized creators from platform censorship and copyright claims —
 *   into an implicit design pattern across commercial platforms, NSFW
 *   archives, and fan communities. DLDR shifts responsibility for content
 *   filtering and harm avoidance from platform/creator to reader, justified
 *   through autonomy and freedom rhetoric. The constraint exhibits genuine
 *   coordination benefits (protecting creator freedom, reducing censorship
 *   pressure, enabling community self-governance) alongside measurable
 *   extraction: disproportionate filtering burden falls on readers with
 *   lowest filtering capacity (neurodivergent readers, trauma survivors,
 *   children, low-literacy users, non-native speakers), while benefits accrue
 *   to creators, platforms, and high-capacity readers. The theater ratio has
 *   increased over time as DLDR has migrated from nonprofit institutional
 *   context (AO3, with robust metadata and community support) to commercial
 *   platforms (lacking filtering affordances and harm-reduction
 *   infrastructure). The constraint is a canonical exemplar of Tangled Rope:
 *   genuine coordination function (creator protection, user autonomy) bundled
 *   inseparably from asymmetric extraction (burden on vulnerable readers).
 *   The rise in theater_ratio reflects increasing rhetoric about 'reader
 *   choice' and 'creator freedom' while actual filtering tools and support
 *   infrastructure have stagnated or degraded. Suppression has increased as
 *   platforms have removed granular content filters and forced-feed
 *   recommendation algorithms that amplify DLDR-untagged content into reader
 *   streams.
 *
 * KEY AGENTS:
 *   - Vulnerable Readers: Primary victims (powerless/trapped) — neurodivergent readers, trauma survivors, children, non-native speakers with low filtering capacity and no meaningful exit
 *   - Low-Capacity Readers: Secondary victims (moderate/constrained) — accessible technology users, low-literacy users, those with attention or memory limitations who face disproportionate filtering burden
 *   - Content Creators: Primary beneficiaries (powerful/arbitrage) — protected from institutional moderation, censorship, and content removal via DLDR shield
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — offload moderation labor and liability; reduce content policy costs while maintaining autonomy rhetoric
 *   - Community Moderators: Secondary agents (moderate/constrained) — implement DLDR governance without compensation; bear harm-reduction labor and psychological cost
 *   - Archive of Our Own: Institutional actor (institutional/arbitrage) — originated DLDR as genuine preservation principle but maintains it increasingly as theater as commercial platforms adopt DLDR without support infrastructure
 *   - Analytical Observer: Civilizational (analytical/analytical) — sees genuine coordination function buried under extraction mechanism and mounting theater ratio
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dldr_information_policy, 0.58).
domain_priors:suppression_score(dldr_information_policy, 0.48).
domain_priors:theater_ratio(dldr_information_policy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dldr_information_policy, extractiveness, 0.58).
narrative_ontology:constraint_metric(dldr_information_policy, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dldr_information_policy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dldr_information_policy, tangled_rope).
narrative_ontology:human_readable(dldr_information_policy, "Don't Like, Don't Read (DLDR) Information Policy").
narrative_ontology:topic_domain(dldr_information_policy, "technological/social").

domain_priors:requires_active_enforcement(dldr_information_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dldr_information_policy, content_creators).
narrative_ontology:constraint_beneficiary(dldr_information_policy, platform_operators).
narrative_ontology:constraint_beneficiary(dldr_information_policy, high_capacity_readers).
narrative_ontology:constraint_victim(dldr_information_policy, vulnerable_readers).
narrative_ontology:constraint_victim(dldr_information_policy, low_capacity_readers).
narrative_ontology:constraint_victim(dldr_information_policy, involuntary_exposees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE READER (SNARE) — Trapped within platforms that host DLDR-governed content; cannot effectively filter, curate, or avoid exposure without abandoning platform participation entirely. Bears full cost of involuntary encounter and psychological harm. No meaningful exit option. Maximum extraction.
constraint_indexing:constraint_classification(dldr_information_policy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOW-CAPACITY READER (TANGLED ROPE) — Technical or cognitive barriers to effective filtering (accessibility needs, language barriers, neurodivergence, low digital literacy). Benefits from platform access and community participation but experiences asymmetric extraction through disproportionate filtering burden. Can constrain behavior but at significant cost.
constraint_indexing:constraint_classification(dldr_information_policy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONTENT CREATOR (ROPE) — Experiences DLDR as pure coordination mechanism: freedom to create without content moderation overhead, reader autonomy respected, community norms maintained through metadata and filtering rather than censorship. Benefits accrue without perceived extraction.
constraint_indexing:constraint_classification(dldr_information_policy, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Offloads content moderation labor and legal liability to users and creators. DLDR operates as pure coordination: reduces operational costs while maintaining the appearance of user choice and freedom. Net beneficiary with minimal direct extraction perception.
constraint_indexing:constraint_classification(dldr_information_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COMMUNITY MODERATOR (TANGLED ROPE) — Experiences genuine coordination function (DLDR enables decentralized curation) alongside asymmetric extraction of labor. Moderators implement filtering infrastructure without compensation. Benefits from community function but bears disproportionate burden of harm reduction. Constrained by social obligation and career identity.
constraint_indexing:constraint_classification(dldr_information_policy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ARCHIVE INSTITUTION (PITON) — DLDR originated as explicit preservation principle in Archive of Our Own (AO3) — a genuine coordination mechanism for protecting creator freedom against platform censorship. But as DLDR has migrated to commercial platforms and implicit design patterns, the institutional function has degraded. Archive maintains the rhetoric (protecting reader autonomy, respecting creator choice) long after the structural justification has atrophied. Piton classification: theater ratio high (defensive principle), coordination function diminished on commercial platforms.
constraint_indexing:constraint_classification(dldr_information_policy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, DLDR coordinates legitimate interests (creator freedom, reader agency, platform cost reduction) while extracting from those with lowest filtering capacity. The constraint is not a natural law but a negotiated institutional arrangement that shifted responsibility asymmetrically. Genuine coordination function exists alongside measurable extraction from vulnerable populations.
constraint_indexing:constraint_classification(dldr_information_policy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dldr_information_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dldr_information_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dldr_information_policy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dldr_information_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dldr_information_policy, TR),
    TR >= 0.70.

:- end_tests(dldr_information_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Base extraction is substantial because the responsibility shift creates measurable burden on low-capacity readers while benefits accumulate to beneficiaries. The extraction is not total (snare-level) because genuine coordination function exists — creator protection and content autonomy are real and valuable. The measurement trajectory (0.30 → 0.45 → 0.58) reflects increasing extraction as DLDR has migrated to platforms without supporting infrastructure. Early-stage DLDR in AO3 included robust tagging, filtering tools, and community support, reducing extraction. Commercial platform DLDR lacks these affordances, concentrating burden on vulnerable users. Suppression (0.48): Moderate-high. Multiple mechanisms suppress reader alternatives: (1) Platform architecture forces exposure (recommendation algorithms, forced feeds, notification systems override DLDR barriers); (2) Social/community pressure discourages 'demanding' content filters or trigger warnings; (3) Infrastructure barriers — filtering tools are scattered across third-party apps, browser extensions, and user-created tools rather than integrated platform features; (4) Responsibility framing shifts moral burden to reader ('if you can't handle it, you shouldn't be here'). Theater ratio (0.65): High and rising. The theater manifests as: (1) Rhetoric about 'reader autonomy' and 'creator freedom' exceeds actual design affordances; (2) DLDR is presented as protection of reader choice when many readers have never chosen DLDR — it was chosen for them by platforms; (3) Metadata and tagging systems are performative — tags are optional, often incomplete, and not enforced; (4) The underlying friction (readers must actively avoid content rather than platforms actively filtering) is naturalized as 'how the internet works' rather than acknowledged as a design choice.
 *
 * PERSPECTIVAL GAP:
 *   The constraint is NOT a simple disagreement about whether DLDR is good policy. The perspectival gap reveals structural asymmetry in how the same policy affects different agents. Creators and platforms experience DLDR as pure coordination: a mechanism to balance freedom and community norms without institutional censorship. Vulnerable readers experience it as pure extraction: they bear burden of filtering while benefits (creator protection, platform cost reduction) accrue elsewhere. The gap is not epistemic — both perspectives are empirically accurate within their context. The gap is structural: agents located differently in the constraint experience it fundamentally differently. The tangled rope classification captures this: the constraint IS both coordination and extraction; the perspectival gap reveals where each function operates. The false-summit risk: treating DLDR as a natural feature of how 'the internet works' or 'community norms' rather than as a designed institutional choice that benefits identifiable agents.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality vector d encodes each agent's location in the extraction flow. Vulnerable readers face maximum extraction because they are trapped (no exit) and victimized (bear burden). Low-capacity readers face moderate extraction because they are constrained (expensive exit) and victimized. Content creators face negative extraction (they benefit) because they are beneficiaries and mobile (can arbitrage to other platforms if needed). Platform operators face the most negative extraction (maximum benefit) because they are beneficiaries and arbitrageurs (can implement any content policy). The archival institution faces moderate benefit with high theater: they benefit from creator freedom and community autonomy but must maintain the rhetorical commitment to DLDR even as the institutional substrate atrophies. The engine computes chi = ε × f(d) × σ(S) for each perspective, where f(d) is the sigmoid function mapping d to experienced extraction. At global scope (σ = 1.2), vulnerable readers experience χ ≈ 0.58 × 1.35 × 1.2 ≈ 0.94 (extraction approaching snare-level). Content creators experience χ ≈ 0.58 × (-0.11) × 1.2 ≈ negative (pure benefit). The scope amplifier is critical: DLDR's extraction grows at larger scales because platform-level decisions affect billions of readers, yet platforms maintain identical no-moderation policies across all geographies and demographics.
 *
 * MANDATROPHY ANALYSIS:
 *   DLDR resolves the mandatrophy by showing that the constraint is genuinely tangled — coordination and extraction coexist inseparably. The beneficiary's coordination (creator protection) is real and valuable. The victim's extraction (filtering burden on low-capacity readers) is real and measurable. The constraint is not mislabeled; it is correctly classified as tangled rope. The mandatrophy was the temptation to call it pure rope (pure coordination) or pure snare (pure extraction) rather than accepting the hybrid classification. The rising theater ratio indicates that the equilibrium is degrading: as commercial platforms adopt DLDR without the supporting infrastructure AO3 provides, the coordination function weakens relative to extraction. The archive institution's piton classification suggests the original institutional purpose (preservation of marginalized content) has atrophied while the rhetoric persists. This is not a falsification of the tangled rope classification but a diagnostic signal: institutional drift is occurring, and the theater ratio is the leading indicator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    filtering_capacity_distribution,
    'What is the actual distribution of reader filtering capacity across demographic groups, and does DLDR burden track inversely with capacity?',
    'Longitudinal survey of content discovery patterns, filtering tool usage, and exposure outcomes across reader demographic profiles (age, neurodivergence, literacy level, digital native status, trauma history). Correlate filtering burden with exposure harm.',
    'If burden tracks inversely with capacity: DLDR''s asymmetric extraction is severe and measurable — reclassifies toward snare from more perspectives. If burden is distributed equally: extraction is genuine but smaller — remains tangled rope. If capacity is self-selected by user choice: extraction disappears and classification becomes pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(filtering_capacity_distribution, empirical, 'Distribution of reader content-filtering capacity and DLDR burden correlation').

omega_variable(
    platform_moderation_cost_transfer,
    'Does DLDR genuinely reduce platform moderation costs, or does cost simply transfer from institutional moderation to distributed user filtering and community harm management?',
    'Cost accounting: compare moderation labor (institutional + crowd-sourced) under DLDR vs. traditional content policies. Include externalities: user-side filtering tool development, community moderator unpaid labor, psychological harm and support costs borne by vulnerable users.',
    'If genuine cost reduction: DLDR is coordination mechanism (Rope from institutional perspective confirmed). If cost transfer: hidden extraction is higher — reclassifies toward snare. If externalized costs exceed institutional moderation savings: DLDR is pure extraction masked as efficiency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_moderation_cost_transfer, empirical, 'Cost transfer analysis: institutional moderation vs. distributed filtering burden').

omega_variable(
    creator_freedom_counterfactual,
    'Would creator freedom actually be threatened without DLDR, or does DLDR primarily benefit platforms by shifting liability and cost?',
    'Comparative analysis: platforms with explicit content policies (Reddit, Twitter, TikTok) vs. DLDR-first platforms (AO3, fan archives). Measure creator chilling effect, content takedown rates, and creator-reported autonomy across policy regimes.',
    'If DLDR is necessary for creator freedom: coordination function is genuine (Rope confirmed). If creators experience similar autonomy under explicit policies: DLDR is framing that naturalizes platform cost-shifting — reclassifies as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_freedom_counterfactual, empirical, 'Counterfactual analysis of creator freedom under different content policies').

omega_variable(
    responsibility_framing_legitimacy,
    'Is the shift of filtering responsibility to readers justified by autonomy principles, or does it exploit autonomy rhetoric to avoid platform liability?',
    'Textual analysis: compare DLDR justification rhetoric in institutional documents (AO3 policy statements, platform ToS) with actual design affordances (filtering tool quality, metadata completeness, user control granularity). Conduct user interviews: do readers perceive themselves as autonomous agents or as responsible for their own harm avoidance?',
    'If design affordances match autonomy rhetoric: responsibility shift is justified — Rope. If rhetoric exceeds affordances: DLDR is deceptive framing for extraction — reclassifies toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(responsibility_framing_legitimacy, conceptual, 'Legitimacy of autonomy-based responsibility framing for DLDR').

omega_variable(
    archive_institutional_drift,
    'Has the Archive of Our Own''s DLDR principle drifted from its original preservation intent (protecting fanfiction from copyright claims and platform censorship) to implicit platform design (offloading moderation labor)?',
    'Historical institutional analysis: trace DLDR adoption timeline in AO3 vs. commercial platforms. Analyze policy documents, community discussions, and design changes. Compare AO3''s implementation (with institutional commitment to transparency and creator support) vs. implicit DLDR on commercial platforms (without support infrastructure).',
    'If drift is substantial: archive institution''s piton classification confirmed — original coordination function has atrophied while theater persists. If AO3 maintains original function: AO3''s perspective should reclassify as rope, while commercial platforms'' DLDR is a distinct constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(archive_institutional_drift, empirical, 'Institutional drift of DLDR principle from preservation to cost-shifting').

omega_variable(
    involuntary_exposure_mechanism,
    'How many readers experience involuntary exposure to harmful content through DLDR platforms, and does exposure frequency exceed thresholds at which filtering burden becomes impossible?',
    'User research: tracking studies of exposure events, filtering tool effectiveness, and failure modes. Qualitative interviews with readers reporting harm. Analysis of platform-generated exposure data (recommendation algorithms, content surface, forced feeds).',
    'If involuntary exposure is common and filtering tools fail predictably: trapped exit classification is justified — snare perspective confirmed. If exposure is rare and tools are effective: constrained exit is correct — tangled rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(involuntary_exposure_mechanism, empirical, 'Frequency and severity of involuntary exposure under DLDR policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dldr_information_policy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dldr_tr_t0, dldr_information_policy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dldr_tr_t5, dldr_information_policy, theater_ratio, 5, 0.52).
narrative_ontology:measurement(dldr_tr_t10, dldr_information_policy, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(dldr_be_t0, dldr_information_policy, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(dldr_be_t5, dldr_information_policy, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(dldr_be_t10, dldr_information_policy, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dldr_su_t0, dldr_information_policy, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(dldr_su_t5, dldr_information_policy, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(dldr_su_t10, dldr_information_policy, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dldr_information_policy, identity_coordination).
narrative_ontology:boltzmann_floor_override(dldr_information_policy, 0.12).
narrative_ontology:affects_constraint(dldr_information_policy, platform_content_moderation).
narrative_ontology:affects_constraint(dldr_information_policy, community_safety_responsibility).
narrative_ontology:affects_constraint(dldr_information_policy, metadata_infrastructure).

% DUAL FORMULATION NOTE:
% DLDR as explicit institutional principle (AO3 preservation policy) is structurally distinct from DLDR as implicit platform design (commercial social networks). The AO3 constraint has lower extractiveness (genuine coordination with supporting infrastructure); commercial platform DLDR has higher extractiveness (coordination rhetoric masking cost-shifting). Both are linked through the network: commercial platforms' DLDR adoption was legitimated by AO3's original institutional success, but the structural conditions that made AO3's DLDR workable (dedicated metadata team, filtering tools, small community norms) do not transfer to billion-user platforms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dldr_information_policy, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
