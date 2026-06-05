% ============================================================================
% CONSTRAINT STORY: attention_economy_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_economy_capture, []).

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
 *   constraint_id: attention_economy_capture
 *   human_readable: Attention Economy Capture
 *   domain: technology/economic/cognitive
 *
 * SUMMARY:
 *   The attention economy constraint describes how digital platforms
 *   systematize the extraction of human cognitive capacity through engineered
 *   behavioral manipulation. The constraint operates across multiple nested
 *   levels: individual cognition (reward hacking of dopamine systems), social
 *   coordination (network effects that prevent exit), economic extraction
 *   (attention monetized through advertising), and developmental capture
 *   (cognitive identity formation in attention-optimized environments). This
 *   constraint is a diagnostic exemplar for how seemingly 'natural' market
 *   mechanisms (matching users to content they engage with) mask severe
 *   extraction when examined through multiple structural positions. The
 *   constraint exhibits all six classification types from different
 *   perspectives, demonstrating how the same underlying mechanism appears as
 *   pure coordination (from the platform's perspective), pure extraction
 *   (from the trapped user's perspective), and temporally-bounded
 *   coordination failure (from the digital literacy coalition's perspective).
 *   The theater_ratio (0.68) reflects that the engagement metrics (watch
 *   time, interaction counts, daily active users) that justify the extraction
 *   mechanism to advertisers and shareholders correlate poorly with stated
 *   user wellbeing — the metrics have become performative cover for the
 *   extraction architecture.
 *
 * KEY AGENTS:
 *   - Attention-Trapped Users: Primary victims (powerless/trapped) — bear cognitive and temporal costs of behavioral manipulation with minimal coordination benefit
 *   - Cognitively-Captured Children: Vulnerable victims (powerless/identity_locked) — identity formation optimized for extraction; exit requires becoming different person
 *   - Content Creators: Secondary actors (moderate/constrained) — experience genuine platform coordination (distribution, discovery) alongside asymmetric extraction (algorithm gatekeeping, data rent)
 *   - Advertising Industry: Primary beneficiary (institutional/arbitrage) — platforms solve targeting and reach problems; extraction runs toward this agent
 *   - Attention Extraction Platforms: Architect beneficiary (institutional/arbitrage) — designed extraction mechanism; sees extraction as coordination from internal perspective
 *   - Digital Literacy Coalition: Organized agents (organized/constrained) — building exit pathways through regulation, alternative platforms, and norm change; sunset logic operative
 *   - Attention Metrics Theater: Institutional degradation (institutional/mobile) — engagement metrics maintain performative legitimacy for extraction architecture despite misalignment with user preference
 *   - Analytical Observer: Civilizational view (analytical/analytical) — reveals false natural law fallacy in platform apologetics that treat extraction as inevitable market outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_economy_capture, 0.58).
domain_priors:suppression_score(attention_economy_capture, 0.65).
domain_priors:theater_ratio(attention_economy_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_economy_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(attention_economy_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(attention_economy_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_economy_capture, tangled_rope).
narrative_ontology:human_readable(attention_economy_capture, "Attention Economy Capture").
narrative_ontology:topic_domain(attention_economy_capture, "technology/economic/cognitive").

domain_priors:requires_active_enforcement(attention_economy_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_economy_capture, attention_extraction_platforms).
narrative_ontology:constraint_beneficiary(attention_economy_capture, advertising_networks).
narrative_ontology:constraint_victim(attention_economy_capture, user_autonomy).
narrative_ontology:constraint_victim(attention_economy_capture, cognitive_commons).
narrative_ontology:constraint_victim(attention_economy_capture, child_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATTENTION-TRAPPED USER (SNARE) — Faces engineered behavioral reinforcement loops with minimal exit capacity. Network effects, social dependency, and platform ubiquity create structural traps. User autonomy bears full extraction cost with near-zero coordination benefit. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(attention_economy_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COGNITIVELY-CAPTURED CHILD (SNARE) — Structurally mobile (could theoretically be placed elsewhere) but identity-locked through formative exposure. Brain development optimized for platform engagement patterns creates self-perpetuating capture. The child's developing cognitive identity is constituted through algorithmic reward structures. Exit would require becoming a different person — identity developed under extraction conditions.
constraint_indexing:constraint_classification(attention_economy_capture, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: CONTENT CREATOR (TANGLED ROPE) — Experiences genuine coordination benefit (platform distributes content to audience, solves discovery problem) alongside asymmetric extraction (platform extracts attention, algorithmic amplification privilege, data rent). Constrained exit due to audience lock-in and lack of alternative distribution channels. Mixed extraction and coordination.
constraint_indexing:constraint_classification(attention_economy_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ADVERTISING INDUSTRY (ROPE) — Primary beneficiary. Experiences the constraint as coordination mechanism: platform attention infrastructure solves the problem of efficient ad targeting and audience reach. Extraction runs toward this agent. Net positive returns; arbitrage exit available (can shift spend across platforms).
constraint_indexing:constraint_classification(attention_economy_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ATTENTION EXTRACTION PLATFORM (ROPE) — Architect and primary beneficiary. Sees the constraint as pure coordination: maximizing engagement solves the problem of monetizing user attention. Engineered behavioral architecture (infinite scroll, algorithmic feeds, notification interrupts) is the coordination mechanism. Extraction asymmetry is invisible from this perspective — the platform experiences its own interest as alignment with user preference.
constraint_indexing:constraint_classification(attention_economy_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DIGITAL LITERACY COALITION (SCAFFOLD) — Organized agents (educators, parents, digital rights groups) treating attention capture as a temporary coordination failure with sunset. Digital literacy norms, screen time regulation, platform regulation (DSA, TikTok bans), and attention-aware design alternatives are building exit pathways. Sunset logic: as regulatory frameworks mature and alternative platforms with attention-conservative algorithms emerge, the extraction mechanism loses its enforcement power. Estimated sunset: 15-25 years as norms shift.
constraint_indexing:constraint_classification(attention_economy_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ATTENTION METRICS THEATER (PITON) — The metrics system (engagement metrics, time-on-page, watch time) that purports to measure user satisfaction has become largely performative. Metrics correlate poorly with actual user wellbeing; optimizing for metrics drives the extraction mechanism but doesn't optimize for stated goals. Theater persists through institutional inertia — advertising spend follows engagement metrics despite misalignment with user preference. Piton classification from high theater_ratio (0.68).
constraint_indexing:constraint_classification(attention_economy_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, attention architecture coordinates genuine discovery and communication problems (connecting users to content, enabling creator livelihoods, distributing information at scale) alongside severe extraction (behavioral manipulation, cognitive capture, addiction design). The constraint is neither pure coordination nor pure extraction — it is a hybrid system where the coordination function is real but increasingly secondary to extraction optimization. The engine's computed classification should resolve to Tangled Rope, diagnosing the false natural law fallacy common in platform apologetics.
constraint_indexing:constraint_classification(attention_economy_capture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_economy_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_economy_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_economy_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_economy_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_economy_capture, TR),
    TR >= 0.70.

:- end_tests(attention_economy_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platforms capture attention value (user time, cognitive focus, behavioral data) and monetize through advertising networks without proportional return to users. The extraction increases over the interval (0.32 → 0.58) as algorithmic optimization techniques mature and network effects deepen lock-in. However, extractiveness is not maximal because platforms do provide genuine coordination benefits (content discovery, creator distribution, information access) that users actively value — the extraction is parasitic on real coordination, not pure overhead. Suppression (0.65): High. Multiple layers: network effects (switching to alternative platform loses social graph), switching costs (data portability barriers, UX familiarity), lack of exit alternatives (attention-conservative platforms remain marginal), and internalized capture (users believe they prefer engagement despite stated desires to reduce usage). Suppression is substantial but not total — some users do exit, and regulatory alternatives are emerging. Theater ratio (0.68): High and increasing. The engagement metrics (watch time, interactions, DAU) that justify extraction to advertisers and governance boards have become increasingly decoupled from stated user preference and measured wellbeing. The metrics are performative — they generate social proof for extraction architecture without validating its beneficence.
 *
 * PERSPECTIVAL GAP:
 *   Primary gap: Platform/Beneficiary (Rope) vs. Trapped User (Snare). The platform experiences alignment (engagement maximization); the user experiences contradiction (engagement against stated preference). Secondary gap: Content Creator (Tangled Rope) vs. Trapped User (Snare). Creators benefit from platform distribution; users bear cost of algorithmic amplification of engagement-optimized content. Tertiary gap: Digital Literacy Coalition (Scaffold) vs. Attention Metrics Theater (Piton). The coalition sees the constraint as temporary with emerging exit pathways; the theater sees it as persistent through institutional momentum. All gaps collapse at the analytical level into a single Tangled Rope diagnosis: the constraint is fundamentally hybrid, with both coordination and extraction structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation differs sharply by agent. Trapped users (powerless/trapped/identity_locked): d-value assigned by victimhood status and exit barriers → f(d) ≈ 1.42 → high experienced χ. Content creators (moderate/constrained): d assigned by mixed beneficiary/victim status and constrained exit → f(d) ≈ 0.75 → moderate experienced χ. Advertising industry (institutional/arbitrage): d assigned by beneficiary status and arbitrage exit → f(d) ≈ -0.12 → negative experienced χ (extraction flows toward this agent). Platform (institutional/arbitrage): d assigned by architect-beneficiary status and arbitrage exit → f(d) ≈ -0.12. The scope modifier σ(S) = 1.2 (global scope) amplifies χ for all perspectives — attention capture operates at unprecedented scale. The suppression (0.65) is unscaled: it is a raw structural property of the constraint, not affected by perspective or scope.
 *
 * MANDATROPHY ANALYSIS:
 *   HYBRID RESOLUTION: This constraint resolves the mandatrophy by showing that it is genuinely hybrid — both the coordination function and the extraction are structurally real and causally important. Platforms DO solve coordination problems: content discovery at scale, creator distribution, connection of users to media they seek. These coordination services would not exist without platform infrastructure. But platforms ALSO extract: they engineer behavioral manipulation to maximize monetizable engagement, they suppress awareness of addiction design, they defend the extraction architecture through false natural law narratives ('engagement reflects preference', 'this is just how markets work'). The Tangled Rope classification diagnoses this hybrid: it is coordination enabled by platforms, but coordination that has been systematically corrupted by extraction optimization. The false natural law fallacy is common: treating platform engagement as equilibrium between supply (creators) and demand (users), thereby naturalizing the extraction as inevitable. The mandatrophy reveals that this appearance of natural equilibrium depends on the suppression of alternatives — if users had low-friction exit options, the equilibrium would collapse and true user preference would diverge sharply from engagement metrics. The analytical observer's Tangled Rope classification resolves this: the constraint is neither pure coordination nor pure extraction, but a system where real coordination has been warped to serve extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engagement_as_preference,
    'Does user engagement on attention-optimized platforms correlate with stated user preferences, or do platforms exploit cognitive vulnerabilities to generate engagement that contradicts user values?',
    'Longitudinal studies comparing user-reported wellbeing and time-use preference with actual platform engagement. Exit behavior analysis: when given low-friction alternatives, what proportion of users reduce engagement? Post-platform cognitive self-report.',
    'If engagement ≈ preference: constraint classifies as Rope (coordination) from most perspectives. If engagement contradicts preference: constraint is pure Snare (extraction via cognitive capture). Current evidence suggests partial contradiction — users report preference to reduce usage but cannot execute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engagement_as_preference, empirical, 'Whether engagement metrics reflect user preferences or exploit cognitive vulnerabilities').

omega_variable(
    alternative_architecture_viability,
    'Can attention-conservative platform architectures (chronological feeds, limited notifications, no algorithmic amplification) achieve sufficient user reach and advertiser ROI to compete with extraction-optimized platforms?',
    'Adoption and retention metrics for Bluesky, Mastodon, BeReal, and other non-extractive architectures. Advertiser willingness-to-pay for attention in attention-conservative environments. Longitudinal tracking as these platforms mature.',
    'If viable: scaffold sunset becomes structural reality — extraction mechanism loses competitive advantage as cost of network lock-in declines. If not viable: scaffold is aspirational; extraction will persist because the coordinating platforms have overwhelming network effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_architecture_viability, empirical, 'Whether non-extractive platform architectures can scale').

omega_variable(
    cognitive_lock_in_reversibility,
    'For cohorts exposed to attention-capture architectures during critical developmental windows, is the identity lock reversible with intervention, or does the cognitive capture become part of permanent personality structure?',
    'Longitudinal neuroscience studies on reward system plasticity in users weaned from extraction-optimized platforms. Cognitive rehabilitation outcomes for digital detox programs. Generational comparison as different cohorts age into adulthood.',
    'If reversible: identity_locked perspective is valid but not permanent — the cognitive commons can recover. If irreversible: the damage to child development becomes structurally permanent; mitigation requires radical regulatory intervention to prevent future capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_lock_in_reversibility, empirical, 'Whether identity lock from childhood exposure is reversible').

omega_variable(
    suppression_structural_vs_internalized,
    'Is measured suppression (0.65) primarily structural (network effects, switching costs, no alternatives) or internalized (users believe they prefer engagement despite stated desires to reduce it)?',
    'Field experiments with friction reduction: when switching cost is lowered, how many users actually defect? Cognitive surveys: do users retain beliefs that engagement is optimal even after controlled exposure to alternative architectures?',
    'If structural: suppression persists even post-platform exit; users will be captured by successor platforms unless architecture changes. If internalized: suppression follows users into alternative systems; education and cognitive deprogramming required for meaningful exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_economy_capture, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, attention_economy_capture, theater_ratio, 0, 0.38).
narrative_ontology:measurement(attn_tr_t5, attention_economy_capture, theater_ratio, 5, 0.52).
narrative_ontology:measurement(attn_tr_t10, attention_economy_capture, theater_ratio, 10, 0.68).
narrative_ontology:measurement(attn_tr_t15, attention_economy_capture, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, attention_economy_capture, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(attn_be_t5, attention_economy_capture, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(attn_be_t10, attention_economy_capture, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(attn_be_t15, attention_economy_capture, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_economy_capture, attachment_coordination).
narrative_ontology:boltzmann_floor_override(attention_economy_capture, 0.12).
narrative_ontology:affects_constraint(attention_economy_capture, algorithmic_filter_bubble).
narrative_ontology:affects_constraint(attention_economy_capture, social_media_teen_depression).
narrative_ontology:affects_constraint(attention_economy_capture, advertising_targeting_asymmetry).

% DUAL FORMULATION NOTE:
% The attention economy constraint operates upstream of more specific extraction mechanisms: filter bubbles (algorithmic curation that extracts epistemic autonomy), teen depression linkages (developmental damage from attention capture), and advertising asymmetries (data extraction feeding targeting precision). Each downstream constraint has its own ε value reflecting specific extraction mechanisms; the attention economy captures their common root in behavioral manipulation architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(attention_economy_capture, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
