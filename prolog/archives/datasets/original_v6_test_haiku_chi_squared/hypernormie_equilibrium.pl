% ============================================================================
% CONSTRAINT STORY: hypernormie_equilibrium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hypernormie_equilibrium, []).

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
 *   constraint_id: hypernormie_equilibrium
 *   human_readable: The Algorithmic Mean Trap: Hypernormie Equilibrium
 *   domain: social/technological
 *
 * SUMMARY:
 *   The algorithmic mean trap describes a constraint that emerges from the
 *   convergence of recommendation algorithms, social feedback loops, and
 *   content moderation systems on platforms with billions of users. Rather
 *   than reflecting diverse human preferences, these systems progressively
 *   suppress variance and amplify a narrow band of aesthetically 'safe'
 *   content optimized for engagement and advertiser comfort. The constraint
 *   operates as a tangled rope: it provides genuine coordination benefits
 *   (reduced toxicity, improved UX, lower moderation burden) while
 *   simultaneously extracting value from creators and audiences through
 *   suppression of cultural variance, aesthetic minorities, and outlier
 *   expression. Over a decade, the extractiveness has grown from 0.22 to 0.58
 *   as algorithmic sophistication increased, while theater ratio (the
 *   performative content moderation layer) has risen from 0.35 to 0.64,
 *   reflecting the growing gap between visible content guidelines and
 *   invisible algorithmic suppression. The constraint traps outlier creators
 *   in a no-exit scenario: they can remain on the platform with minimal
 *   reach, or leave entirely, but they cannot be both creative and
 *   discovered. The 'hypernormie' aesthetic—perfectly optimized,
 *   conflict-averse, maximally shareable—is not an organic cultural
 *   equilibrium but a structural artifact of engagement metrics applied at
 *   scale.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — reduces moderation costs, brand-safety risk, and toxicity complaints
 *   - Aesthetic Minorities: Primary victim (powerless/trapped) — artists, subcultures, disability representation, non-conforming creators suppressed by algorithmic mean
 *   - Normie Content Creators: Secondary beneficiary/victim (moderate/constrained) — benefit from algorithmic amplification but constrained to proven formulas
 *   - Alternative Platform Coalition: Organized agents (organized/mobile) — Mastodon, Bluesky, Substack, niche communities building escape pathways with decentralized or non-algorithmic governance
 *   - Content Moderation Bureaucracy: Institutional actor (institutional/arbitrage) — visible enforcement rules maintained as theater while actual suppression happens through invisible ranking algorithms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as neither pure law nor pure extraction but a hybrid system solving real problems while flattening aesthetic possibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hypernormie_equilibrium, 0.58).
domain_priors:suppression_score(hypernormie_equilibrium, 0.68).
domain_priors:theater_ratio(hypernormie_equilibrium, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hypernormie_equilibrium, extractiveness, 0.58).
narrative_ontology:constraint_metric(hypernormie_equilibrium, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hypernormie_equilibrium, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hypernormie_equilibrium, tangled_rope).
narrative_ontology:human_readable(hypernormie_equilibrium, "The Algorithmic Mean Trap: Hypernormie Equilibrium").
narrative_ontology:topic_domain(hypernormie_equilibrium, "social/technological").

domain_priors:requires_active_enforcement(hypernormie_equilibrium).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hypernormie_equilibrium, platform_operators).
narrative_ontology:constraint_beneficiary(hypernormie_equilibrium, engagement_optimizers).
narrative_ontology:constraint_victim(hypernormie_equilibrium, cultural_variance).
narrative_ontology:constraint_victim(hypernormie_equilibrium, outlier_creators).
narrative_ontology:constraint_victim(hypernormie_equilibrium, aesthetic_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AESTHETIC OUTLIER (SNARE) — Creator whose style, identity, or expression falls outside the algorithmic mean. Cannot exit without abandoning platform presence. Algorithmic suppression of non-normative content is total: low reach, demotion, shadowbanning. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(hypernormie_equilibrium, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NORMIE CONTENT CREATOR (TANGLED ROPE) — Creator whose output aligns with algorithmic mean. Benefits from preferential algorithmic amplification and engagement. Also constrained: deviation from proven formula risks algorithmic demotion. d≈0.48, f(d)≈0.61, σ=1.0 → χ≈0.35.
constraint_indexing:constraint_classification(hypernormie_equilibrium, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATORS (ROPE) — Optimize for engagement metrics and advertiser safety. Algorithmic mean maximizes session time and minimizes brand-safety violations. Pure coordination benefit: convergence reduces moderation costs, comment toxicity, and advertiser complaint volume. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.08.
constraint_indexing:constraint_classification(hypernormie_equilibrium, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE PLATFORM COALITION (SCAFFOLD) — Mastodon, Bluesky, Substack, niche Discord communities represent a sunset for algorithmic mean dominance. These platforms enable asymmetric niching—communities can optimize for diversity rather than engagement. d≈0.35, f(d)≈0.31, σ=0.9 → χ≈0.16. Low extraction because coalition has genuine exit pathways and agency.
constraint_indexing:constraint_classification(hypernormie_equilibrium, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CONTENT MODERATION BUREAUCRACY (PITON) — Community guidelines and algorithmic safety rules persist as performative theater. Original function (prevent genuine harm) has been mostly delegated to algorithmic suppression of low-engagement outliers. Theater ratio ≈0.64: moderation teams maintain visible enforcement processes while actual suppression happens invisibly in ranking algorithms. Institutional inertia keeps moderation teams staffed despite algorithmic replacement.
constraint_indexing:constraint_classification(hypernormie_equilibrium, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the algorithmic mean trap exhibits genuine coordination benefits (reduced toxicity, improved UX through personalization) AND asymmetric extraction (suppression of cultural variance, flattening of aesthetic possibility space). The constraint is neither pure law nor pure exploitation—it is a hybrid system that solves real problems while creating new forms of conformity pressure. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.79.
constraint_indexing:constraint_classification(hypernormie_equilibrium, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hypernormie_equilibrium_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hypernormie_equilibrium, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hypernormie_equilibrium, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hypernormie_equilibrium, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hypernormie_equilibrium, TR),
    TR >= 0.70.

:- end_tests(hypernormie_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts cultural variance—creators optimizing for algorithmic reach must suppress aspects of their authentic identity that fall outside the mean. The extraction is not absolute (alternatives exist) but pervasive (leaving platforms means losing audience and income). The growth from 0.22 to 0.58 reflects algorithmic sophistication layering extraction onto coordination. Suppression (0.68): High. Algorithmic demotion of low-engagement content is structural and rarely transparent. Outlier creators face shadowbanning, reduced reach, and invisible ranking penalties. Users are suppressed from seeing diverse content. The suppression mechanism is not bureaucratic (no explicit rules against outliers) but mathematical. Theater ratio (0.64): Moderately high. Content moderation policies present themselves as protecting users from harm, but the actual suppression mechanism is engagement optimization. The gap between policy (broad guidelines) and execution (algorithmic ranking based on engagement) creates theater—visible rules that obscure invisible mechanisms. Claimed type (Tangled Rope): Required because the constraint exhibits BOTH coordination function (reduced toxicity, improved UX, advertiser satisfaction) AND asymmetric extraction (suppression of variance). This is not pure extraction (Snare) and not pure coordination (Rope)—it is a hybrid where the coordination benefits accrue to platforms and normie creators while extraction costs fall on outliers.
 *
 * PERSPECTIVAL GAP:
 *   The aesthetic outlier sees a Snare: trapped, fully extracted, no recourse. The normie creator sees a Tangled Rope: benefits from algorithmic amplification but must conform to proven formulas. Platform operators see a Rope: pure coordination benefit with no sense of extraction—they are solving genuine problems (toxicity, brand safety, UX). The alternative platform coalition sees a Scaffold: believes algorithmic mean dominance is temporary, that decentralized platforms or non-algorithmic curation will provide an exit path. The moderation bureaucracy sees a Piton: the content moderation theater persists from institutional inertia, even though actual suppression is now algorithmic. The analytical observer sees a full Tangled Rope: neither pure law nor pure exploitation, but a genuine hybrid with real coordination benefits AND real extraction costs borne by those outside the mean.
 *
 * DIRECTIONALITY LOGIC:
 *   Aesthetic outliers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction relative to victim status. Cannot exit without abandoning platform. Normie creators: Both beneficiary and victim + constrained → d≈0.48, f(d)≈0.61. Moderate extraction. Benefit from amplification but constrained to formula. Platform operators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Full control over algorithmic parameters. Alternative platform coalition: Mobile exit option + organized → d≈0.35, f(d)≈0.31. Lower extraction because coalition has genuine agency and mobility. Content moderation: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Piton classification comes from high theater ratio (0.64), not from directionality. Analytical observer: analytical + civilizational → d≈0.72, f(d)≈1.15. Full view of the structure; sees genuine hybrid with unequal burden distribution.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the 'algorithmic mean' is NOT a natural law or emergent property of scale—it is a contingent institutional arrangement optimized for engagement metrics and platform risk reduction. The analytical observer might initially naturalize the mean as 'inevitable consequence of user preference at scale,' but the structural data reveals active extraction: the growth of extractiveness from 0.22 to 0.58 over a decade shows that suppression of variance was engineered, not inevitable. The theater ratio (0.64) further reveals that the constraint is maintained through performative content guidelines that obscure algorithmic suppression mechanisms. Mandatrophy is resolved by: (1) distinguishing beneficiaries (platform operators, normie creators) from victims (aesthetic minorities), (2) measuring the asymmetry in exit options (outliers trapped; platforms have full control), and (3) acknowledging genuine coordination benefits (reduced toxicity, improved UX) while refusing to let these justify the suppression of cultural variance. The Tangled Rope classification prevents false summits (naturalizing the mean as inevitable) while resisting the temptation to classify purely as Snare (ignoring genuine safety benefits).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_mean_definition,
    'Is the ''mean'' a mathematical artifact of engagement optimization or a deliberate aesthetic design choice?',
    'Algorithm audit: comparison of engagement-optimized ranking vs. diversity-optimized ranking; analysis of recommendation training objectives',
    'If artifact: constraint is primarily an emergent coordination failure (Rope from more perspectives). If deliberate: constraint is intentional extraction (Snare/Tangled Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_mean_definition, empirical, 'Whether algorithmic mean is emergent or deliberately designed').

omega_variable(
    outlier_detection_sufficiency,
    'Can algorithmic systems distinguish between harmful-and-niche content and merely-unusual-but-valuable content?',
    'Error analysis: false-positive suppression rates of artistic, cultural-minority, and disability-representation content; correlation between suppression and objective quality metrics',
    'If distinguishable: moderation can be refined without suppressing outliers (Rope restoration possible). If indistinguishable: suppression is inevitable cost of safety (Snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(outlier_detection_sufficiency, empirical, 'Whether algorithms can distinguish harmful from merely-unusual content').

omega_variable(
    user_preference_authenticity,
    'Do users genuinely prefer the algorithmic mean, or does repeated exposure to mean content create apparent preference through feedback loop?',
    'Experimental: randomized assignment to diverse-recommendation vs. engagement-optimized recommendations; measurement of preference formation over time and baseline aesthetic diversity',
    'If genuine: constraint reflects authentic user preferences (not extraction). If artifact of feedback loop: constraint is Snare (user preferences are manufactured).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_preference_authenticity, empirical, 'Whether user preference for algorithmic mean is authentic or manufactured').

omega_variable(
    multi_platform_exit_viability,
    'Can alternative platforms (Bluesky, Mastodon, niche communities) sustain creators and audiences at scale, or do they remain permanently marginal?',
    'Longitudinal tracking: creator migration to alternatives; revenue viability studies; user retention and network effects on alternative platforms',
    'If viable at scale: Scaffold perspective confirmed, sunset is real. If permanently marginal: creators face de facto trap despite nominal exit (Snare from creative perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_platform_exit_viability, empirical, 'Whether alternative platforms can sustain creators at scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hypernormie_equilibrium, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hypnorm_tr_t0, hypernormie_equilibrium, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hypnorm_tr_t5, hypernormie_equilibrium, theater_ratio, 5, 0.5).
narrative_ontology:measurement(hypnorm_tr_t10, hypernormie_equilibrium, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(hypnorm_be_t0, hypernormie_equilibrium, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hypnorm_be_t5, hypernormie_equilibrium, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(hypnorm_be_t10, hypernormie_equilibrium, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hypernormie_equilibrium, information_standard).
narrative_ontology:affects_constraint(hypernormie_equilibrium, engagement_metric_capture).
narrative_ontology:affects_constraint(hypernormie_equilibrium, content_moderation_theater).
narrative_ontology:affects_constraint(hypernormie_equilibrium, creator_income_concentration).

% DUAL FORMULATION NOTE:
% The algorithmic mean trap decomposes into three related constraints: (1) engagement_metric_capture (ε≈0.15, Mountain-adjacent): fundamental mathematical property of recommendation systems that low-variance content outperforms diverse content in click-through metrics. (2) content_moderation_theater (ε≈0.42, Tangled Rope): visible moderation rules paired with invisible algorithmic suppression. (3) creator_income_concentration (ε≈0.65, Snare): economic dependency on platform algorithmic favor. The hypernormie_equilibrium story integrates all three, focusing on the perceptual experience of the constraint at the creator and audience level. Network links point downstream to constraints that depend on algorithmic mean dominance for their operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hypernormie_equilibrium, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
