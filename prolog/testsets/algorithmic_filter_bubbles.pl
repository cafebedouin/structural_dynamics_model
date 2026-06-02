% ============================================================================
% CONSTRAINT STORY: algorithmic_filter_bubbles
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_filter_bubbles, []).

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
 *   constraint_id: algorithmic_filter_bubbles
 *   human_readable: Algorithmic Filter Bubbles in Social Media and Content Platforms
 *   domain: digital_platforms/epistemic_regulation
 *
 * SUMMARY:
 *   Algorithmic filter bubbles represent a structural constraint where
 *   platform operators have designed information distribution systems
 *   optimized for engagement and revenue, not for epistemic diversity or user
 *   autonomy. The constraint is simultaneously presented as natural
 *   (engagement is what users want), beneficial (personalization improves
 *   experience), and inevitable (scale requires some filtering). Yet the
 *   design choices are contingent: platforms could prioritize epistemic
 *   diversity, user algorithmic control, feed transparency, or
 *   interoperability. Instead, they have built systems that extract user
 *   attention, behavioral data, and epistemic agency in exchange for
 *   personalized content curation. The constraint exhibits all six
 *   classification types from different observer positions: immutable natural
 *   law (analytical risk), degraded theater (content moderation system),
 *   temporary coordination failure with regulatory sunset (coalition
 *   advocates), beneficial coordination (platform operators), mixed
 *   extraction-coordination (content creators), and pure extraction
 *   (epistemic commons and identity-locked users). The extractiveness
 *   trajectory shows accumulation: from 0.35 (early algorithmic curation) to
 *   0.58 (contemporary engagement-maximizing systems), driven by increasing
 *   sophistication in behavioral prediction and A/B testing for retention.
 *   Theater has increased in parallel as content moderation and algorithmic
 *   transparency claims proliferate without corresponding structural change.
 *   Suppression has intensified as identity-lock mechanisms (social graph
 *   dependency, algorithmic preference learning, switching costs) have
 *   deepened.
 *
 * KEY AGENTS:
 *   - Platform Operators (Meta, Google, TikTok, etc.): Primary beneficiaries (institutional/arbitrage) — capture advertising revenue, user engagement data, attention extraction. Can exit or modify constraints with minimal cost.
 *   - Engaged Users (billions of daily active): Primary victims (powerless/identity_locked) — identity fused with algorithmic feeds; experience curation as personalization; structural mobility exists but identity frame makes exit unthinkable
 *   - Information Commons / Epistemic Diversity: Secondary victim (powerless/trapped) — abstract collective good; no advocacy structure; bears cost of polarization and degraded information quality
 *   - Content Creators (YouTubers, TikTokers, newsletter writers): Mixed victims and beneficiaries (moderate/constrained) — depend on algorithmic reach but constrained by feed suppression and unpredictable curation changes
 *   - Marginalized Information Sources (nonprofit journalism, academic research, niche expertise): Tertiary victims (powerless/trapped) — algorithmically suppressed below high-engagement content; no structural power to compete
 *   - Content Moderation Systems (human and automated): Institutional theater (institutional/arbitrage) — policies exist but functional verification is degraded; real platform behavior driven by engagement, not moderation rules
 *   - Regulatory Coalitions (EU DMA, DSA advocates, civil society): Organized agents (organized/constrained) — building alternative architectures with sunset logic; have agency but face institutional resistance
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing design choices as inherent limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_filter_bubbles, 0.58).
domain_priors:suppression_score(algorithmic_filter_bubbles, 0.68).
domain_priors:theater_ratio(algorithmic_filter_bubbles, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_filter_bubbles, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_filter_bubbles, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algorithmic_filter_bubbles, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_filter_bubbles, tangled_rope).
narrative_ontology:human_readable(algorithmic_filter_bubbles, "Algorithmic Filter Bubbles in Social Media and Content Platforms").
narrative_ontology:topic_domain(algorithmic_filter_bubbles, "digital_platforms/epistemic_regulation").

domain_priors:requires_active_enforcement(algorithmic_filter_bubbles).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_filter_bubbles, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_filter_bubbles, high_engagement_content_creators).
narrative_ontology:constraint_victim(algorithmic_filter_bubbles, user_epistemic_autonomy).
narrative_ontology:constraint_victim(algorithmic_filter_bubbles, information_commons_diversity).
narrative_ontology:constraint_victim(algorithmic_filter_bubbles, marginalized_information_sources).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATION COMMONS (SNARE) — No exit from algorithmic curation; cannot organize or advocate for itself. Bears full cost of polarization, epistemic fragmentation, and degraded information quality. The commons is an abstract collective with no structural power to resist the extraction. Maximum vulnerability.
constraint_indexing:constraint_classification(algorithmic_filter_bubbles, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENGAGED USER (SNARE / IDENTITY_LOCKED) — Structurally mobile (could delete account, use alternative platforms) but identity-fused with algorithmic feed. Social identity, professional reputation, peer connections, and daily cognitive patterns are constituted through platform engagement. Exit would require abandoning not just the tool but the relational identity built within it. The algorithm learns this lock and deepens it — feed curation becomes increasingly identity-confirming. Suppression operates through both material barriers (network effects, switching costs) and internalized framing (the algorithm knows 'me' better than alternatives do).
constraint_indexing:constraint_classification(algorithmic_filter_bubbles, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: CONTENT CREATOR (TANGLED ROPE) — Constrained by algorithm dependency for audience discovery and reach; bears cost of unpredictable curation changes and feed suppression. But also benefits from algorithmic amplification of engaging content and access to micro-targeting tools. Mixed extraction and coordination — the constraint simultaneously enables and captures creator output.
constraint_indexing:constraint_classification(algorithmic_filter_bubbles, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATORS (ROPE) — Experience the constraint as coordination mechanism: algorithmic curation solves the genuine problem of distributing content to heterogeneous users at scale. Engagement optimization is presented as serving user preferences. Net beneficiary with full exit flexibility — operators can modify algorithms, change incentive structures, or sunset the constraint entirely without cost. Experiences extraction as flowing inward.
constraint_indexing:constraint_classification(algorithmic_filter_bubbles, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COALITIONS (SCAFFOLD) — EU Digital Services Act, proposed platform regulation, algorithmic transparency mandates, and interoperability requirements are building alternative architectures (federation, user control, algorithmic choice) with sunset logic. Organized agents see the filter bubble as a temporary regulatory failure with enforcement mechanisms designed to create exit ramps. Effective extraction is dampened by the coalition's agency and the visible path to structural change. Theater is moderate — regulation is both performative and substantive.
constraint_indexing:constraint_classification(algorithmic_filter_bubbles, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CONTENT MODERATION SYSTEM (PITON) — Platform content policies and community standards are largely performative relative to the scale of algorithmic curation. Moderation rules establish visible norms, but the algorithm's real sorting mechanism (engagement, retention, ad revenue) operates independently of stated content policy. Community guidelines persist through institutional inertia — they signal responsibility and provide legal cover — but their functional verification is degraded. Moderators know their work is theater; the real platform behavior is algorithmic. High theater ratio (0.65) without corresponding extraction severity indicates institutional decline.
constraint_indexing:constraint_classification(algorithmic_filter_bubbles, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some degree of information filtering is inherent to any distribution system at scale: perfect neutrality is mathematically impossible, and any algorithm that learns user preferences will create feedback loops. This perspective risks naturalizing what is actually a contingent design choice — engagement optimization vs. epistemic diversity — as an immutable property of digital communication. The engine's false summit detector identifies this as a false natural law: the 'inherent to scale' framing naturalizes an institutional design decision.
constraint_indexing:constraint_classification(algorithmic_filter_bubbles, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_filter_bubbles_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_filter_bubbles, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_filter_bubbles, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_filter_bubbles, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_filter_bubbles, TR),
    TR >= 0.70.

:- end_tests(algorithmic_filter_bubbles_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platforms extract user attention, behavioral data, and epistemic agency in exchange for content curation. The extraction is not maximal (users do receive curated content; creators do reach audiences; engagement genuinely reflects some preferences) but is substantial and asymmetric. The trajectory from 0.35 to 0.58 over the measurement interval reflects intensifying engagement optimization — platforms have invested heavily in behavioral prediction, A/B testing, and retention mechanics. The value avoids 0.70+ because genuine coordination benefits exist (users do prefer personalized feeds over chronological chaos; creators do benefit from algorithmic reach) alongside the extraction. Suppression (0.68): High. Multiple, overlapping suppression mechanisms: network effects (cost of switching, data lock-in, social graph lock), identity-lock (preference internalization, algorithmic self-confirmation), behavioral capture (dopamine-reinforced engagement patterns), and structural dependency (no alternative platforms at equivalent scale). The suppression trajectory from 0.52 to 0.68 reflects deepening psychological and structural locks as users spend more time and identity-invest in platforms. Theater ratio (0.65): Moderate-high. Content moderation policies, community standards, algorithmic transparency reports, and diversity commitments are substantially performative. The real platform behavior — optimization for engagement and retention — operates largely independently of stated content policies. Moderation is visible and rule-bound; algorithmic curation is opaque and metrics-driven. As theater ratio has increased (0.42 to 0.65), platforms have added more performative governance structures (content review boards, transparency reports, creator councils) without changing underlying algorithmic incentives. This is classic piton dynamics: institutional cover growing while functional verification degrades.
 *
 * PERSPECTIVAL GAP:
 *   The constraint shows maximum perspectival divergence between platform operators (Rope) and powerless agents (Snare). Operators see coordination; users see extraction. The regulatory scaffold exists because organized agents recognize the constraint as temporary — but platforms experience it as immutable (their business model) or beneficial (their logic). Content creators occupy the tangled middle — benefiting from reach while being constrained by algorithmic capriciousness. The analytical observer risks the false natural law trap: presenting engagement optimization as inevitable when it is a design choice. The deepening identity-lock for engaged users creates a classification tension: at biographical time, users classify the constraint as mountain (unchangeable, 'the algorithm knows me') from within the identity frame, but cross-position analysis reveals rope-to-snare extraction dynamics that the identity frame obscures.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators derive d from beneficiary status + arbitrage exit → d ≈ 0.05 → negative χ. Engaged users derive d from victim status + identity_locked exit → d ≈ 0.89 → high χ. The identity_lock is critical: users are structurally mobile (could switch platforms) but identity-fused (switching would require abandoning social identity, professional reputation, daily cognitive patterns built within the feed). The algorithm exploits this: feed curation becomes increasingly identity-confirming, deepening the lock. Suppression operates at two levels: structural (network effects, data lock-in) and internalized (the algorithm 'knows me' better than alternatives). Content creators derive d from mixed victim-beneficiary status + constrained exit → d ≈ 0.55-0.60 → moderate extraction. The regulatory coalition derives d from organized agent status + constrained exit → d ≈ 0.50, but the scaffold classification is driven by finite enforcement (sunset clause) not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy through structural differentiation by observer position. Platform operators experience Rope (genuine coordination: solving scale problem) and have agency to change it. Powerless users experience Snare (pure extraction with identity lock) and have no effective exit. Regulatory coalitions experience Scaffold (temporary problem with enforcement sunset). Content creators experience Tangled Rope (mixed benefits and costs). The information commons experiences Snare (pure extraction). Content moderation experiences Piton (theater). The analytical observer risks Mountain (naturalizing design choice). No single type is 'correct' — the perspectival presheaf is the answer. The mandatrophy resolves when the framework shows that different agents are experiencing genuinely different structural constraints whose classifications differ legitimately. Platform operators are not wrong to see coordination; they are correct from their position. Powerless users are not wrong to see extraction; they are correct from theirs. The constraint is the difference in their positions, not an objective property independent of position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engagement_optimization_necessity,
    'Is algorithmic optimization for engagement a necessary property of content distribution at scale, or a contingent design choice of current platforms?',
    'Analysis of alternative platform architectures (federated systems, algorithmic choice systems, user-controlled feeds) that scale without engagement optimization; measurement of epistemic diversity outcomes under alternative designs',
    'If necessary: filter bubble is a mountain (inherent limit). If contingent: filter bubble is a snare/tangled_rope (extractive institutional design). This is the false summit test.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engagement_optimization_necessity, conceptual, 'Whether engagement optimization is inherent to scale or a design choice').

omega_variable(
    identity_lock_reversibility,
    'Can users who have developed identity-lock with algorithmic feeds recover epistemic autonomy after platform exit, or does the lock persist as internalized curation preference?',
    'Longitudinal study of users who delete/abandon accounts and measure cognitive patterns, information-seeking behavior, and restoration of epistemic diversity in non-platform contexts; tracking of former-user return rates to platforms after exit',
    'If reversible: users retain capacity for exit despite identity lock, reducing suppression to constrained range. If irreversible: identity lock becomes a structural feature of suppression, elevating constraint to pure snare for locked users.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity-lock with algorithmic feeds is reversible after exit').

omega_variable(
    regulatory_sunset_efficacy,
    'Do algorithmic transparency mandates and interoperability requirements actually reduce filter bubble effects, or do they become theater (platforms comply minimally while optimizing engagement through opaque mechanisms)?',
    'Post-regulation measurement of algorithmic diversity: comparison of feed diversity before/after regulatory implementation; analysis of compliance audit findings; tracking of user platform migration to alternative architectures',
    'If effective: scaffold perspective is confirmed, sunset is real, constraint enters terminal decline. If theater: regulations become piton properties (performative compliance), and platforms absorb regulatory cost without structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_sunset_efficacy, empirical, 'Whether regulatory mandates reduce filter bubble effects or become theater').

omega_variable(
    content_creator_coalition_power,
    'Can content creators organize against algorithmic suppression to shift platform incentives, or is their dependent relationship on algorithms structurally irreversible?',
    'Analysis of creator collective action (YouTube Creator Association, TikTok creator councils, platform union organizing); measurement of collective power to negotiate algorithmic terms; tracking of successful creator-led platform transitions or regulatory interventions',
    'If creators can organize: moderate agent power increases to organized, perspectives shift toward tangled_rope from snare. If irreversible dependency: creators remain moderate/constrained, snare dynamics dominate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_creator_coalition_power, empirical, 'Whether content creators can organize to shift platform algorithmic incentives').

omega_variable(
    suppression_mechanism_taxonomy,
    'What proportion of measured suppression is structural (network effects, switching costs, data lock-in) vs. identity-internalized (cognitive capture, preference lock, algorithmic self-confirmation)?',
    'Decomposition analysis: measure structural barriers independently (cost-to-switch, network density, data portability) vs. behavioral barriers (retention rates, information-seeking pattern changes, preference drift studies); separate identity-locked cohorts from trapped cohorts',
    'If mostly structural: suppress suppression value downward, reclassify to constrained-exit, reduce snare→tangled_rope ratio. If mostly internalized: suppress value is accurate, identity_locked exit is appropriate, intensifies snare classification from locked-user perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_taxonomy, empirical, 'Proportion of suppression that is structural vs. identity-internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_filter_bubbles, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(afb_tr_t0, algorithmic_filter_bubbles, theater_ratio, 0, 0.42).
narrative_ontology:measurement(afb_tr_t5, algorithmic_filter_bubbles, theater_ratio, 5, 0.55).
narrative_ontology:measurement(afb_tr_t10, algorithmic_filter_bubbles, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(afb_be_t0, algorithmic_filter_bubbles, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(afb_be_t5, algorithmic_filter_bubbles, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(afb_be_t10, algorithmic_filter_bubbles, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(afb_su_t0, algorithmic_filter_bubbles, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(afb_su_t5, algorithmic_filter_bubbles, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(afb_su_t10, algorithmic_filter_bubbles, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_filter_bubbles, identity_coordination).
narrative_ontology:affects_constraint(algorithmic_filter_bubbles, platform_algorithmic_transparency).
narrative_ontology:affects_constraint(algorithmic_filter_bubbles, content_creator_labor_extraction).
narrative_ontology:affects_constraint(algorithmic_filter_bubbles, epistemic_polarization_dynamics).
narrative_ontology:affects_constraint(algorithmic_filter_bubbles, platform_network_lock_in).

% DUAL FORMULATION NOTE:
% The algorithmic filter bubble decomposes into multiple distinct constraints with different ε values: (1) engagement optimization mechanism (ε=0.58, this story) coordinates content distribution while extracting user attention; (2) platform transparency theater (ε=0.62, separate story) provides visibility into algorithms while maintaining opacity of engagement metrics; (3) creator dependency dynamics (ε=0.52, separate story) create mixed coordination-extraction for content generators. Upstream influence: network lock-in (ε=0.70) and data capture mechanisms enable filter bubble extraction. Downstream effects: epistemic polarization (ε=0.65) and creator labor extraction (ε=0.61) follow from algorithmic curation choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_filter_bubbles, powerless, 0.89).
constraint_indexing:directionality_override(algorithmic_filter_bubbles, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
