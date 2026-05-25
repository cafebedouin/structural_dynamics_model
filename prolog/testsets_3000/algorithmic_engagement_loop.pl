% ============================================================================
% CONSTRAINT STORY: algorithmic_engagement_loop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_engagement_loop, []).

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
 *   constraint_id: algorithmic_engagement_loop
 *   human_readable: Algorithmic Engagement Loop
 *   domain: digital/social_media/platform_economics
 *
 * SUMMARY:
 *   The algorithmic engagement loop represents a structural constraint where
 *   platform operators optimize recommendation algorithms to maximize user
 *   engagement (time spent, interaction frequency, return visits) rather than
 *   maximize user welfare. This creates a system that coordinates supply
 *   (content creators, advertisers) with demand (users) while simultaneously
 *   extracting user attention and data. The constraint is characterized by
 *   asymmetric extraction where end users experience attention capture
 *   through variable rewards and notifications, content creators face
 *   algorithmic ranking opacity and revenue extraction, the information
 *   ecosystem degrades through engagement-driven sensationalism, and platform
 *   operators benefit from attention monetization. The mechanism persists
 *   through multiple reinforcing cycles: users are psychologically engaged
 *   through dark patterns and notifications, content creators adapt to
 *   algorithmic preferences, advertisers find engaged audiences, and
 *   platforms accumulate data and revenue. Regulatory responses (EU DSA, DMA,
 *   transparency mandates) represent an organized effort to implement a
 *   sunset clause by forcing algorithmic architecture changes. The
 *   constraint's extractiveness has increased over 15 years as algorithmic
 *   sophistication has improved and as the optimization target has shifted
 *   explicitly from 'user benefit' to 'engagement.' The theater ratio
 *   reflects that while platforms continue to frame algorithms as serving
 *   user interests, the actual mechanisms are increasingly transparent as
 *   engagement-optimization systems.
 *
 * KEY AGENTS:
 *   - End Users: Primary victim (powerless/trapped) — bear attention extraction through design and psychological manipulation; network effects and social dependency prevent exit
 *   - Content Creators: Secondary victim (moderate/constrained) — dependent on platform reach but subject to opaque algorithmic ranking; some agency through content experimentation
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — extract attention and data for monetization; experience constraint as coordination mechanism; active enforcement through algorithmic optimization
 *   - Advertisers: Secondary beneficiary (institutional/arbitrage) — benefit from efficient audience targeting; experience constraint as pure coordination tool
 *   - Information Ecosystem: Tertiary victim (powerless/trapped) — bears structural degradation through engagement-driven sensationalism and misinformation amplification; no agency or exit
 *   - Regulatory Coalition: Organized agent (organized/constrained) — building alternative architectures and enforcement mechanisms with sunset timeline; represents constraint resolution pathway
 *   - Analytical Observer: Sees tangled coordination-extraction hybrid; recognizes both genuine coordination function and structural extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_engagement_loop, 0.62).
domain_priors:suppression_score(algorithmic_engagement_loop, 0.68).
domain_priors:theater_ratio(algorithmic_engagement_loop, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_engagement_loop, extractiveness, 0.62).
narrative_ontology:constraint_metric(algorithmic_engagement_loop, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algorithmic_engagement_loop, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_engagement_loop, tangled_rope).
narrative_ontology:human_readable(algorithmic_engagement_loop, "Algorithmic Engagement Loop").
narrative_ontology:topic_domain(algorithmic_engagement_loop, "digital/social_media/platform_economics").

domain_priors:requires_active_enforcement(algorithmic_engagement_loop).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_engagement_loop, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_engagement_loop, advertisers).
narrative_ontology:constraint_victim(algorithmic_engagement_loop, end_users).
narrative_ontology:constraint_victim(algorithmic_engagement_loop, information_ecosystem).
narrative_ontology:constraint_victim(algorithmic_engagement_loop, cognitive_attention).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Users experience maximal extraction with minimal coordination benefit. Algorithmic feed optimization is structurally designed to be inescapable: network effects lock users into platform participation, notification systems hijack attention through variable rewards, and social dependency creates psychological barriers to exit. The constraint extracts attention (monetizable engagement) while suppressing awareness of the extraction mechanism itself through opacity and complexity.
constraint_indexing:constraint_classification(algorithmic_engagement_loop, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONTENT CREATOR (TANGLED ROPE) — Content creators face constrained exit: they depend on platform reach for audience and income but are subject to algorithmic ranking that they cannot control or predict. The system provides genuine coordination (connecting creators to audiences) alongside asymmetric extraction (algorithmic privileging of engagement over creator intent, revenue sharing favoring the platform, algorithmic suppression without explanation). Creators have some agency (they can experiment with content, migrate platforms at cost) but are fundamentally dependent on platform infrastructure.
constraint_indexing:constraint_classification(algorithmic_engagement_loop, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ADVERTISER (ROPE) — Advertisers experience the algorithmic loop as pure coordination: the system solves a genuine collective action problem (matching ad exposure to receptive audiences) with minimal overhead and maximal efficiency. Advertisers have arbitrage options (alternative ad platforms, self-hosting, traditional media) and experience low effective extraction. The constraint is a coordinated tool from their perspective — a solution, not an imposition.
constraint_indexing:constraint_classification(algorithmic_engagement_loop, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INFORMATION ECOSYSTEM (SNARE) — The epistemic commons bears extraction with no agency and no exit. Algorithmic engagement optimization incentivizes sensationalism, polarization, and clickbait over accuracy. Misinformation spreads faster through engagement-optimized feeds than corrections. The ecosystem is structurally trapped: individual users cannot control the aggregate effect of their engagement choices, and the system is designed to exploit cognitive biases rather than compensate for them. Extraction is maximal — the constraint systematically degrades information quality — and suppression is complete: the mechanism is invisible to most users.
constraint_indexing:constraint_classification(algorithmic_engagement_loop, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM OPERATOR (TANGLED ROPE) — Platforms genuinely solve coordination: they connect billions of users and enable communication at scale with minimal infrastructure per transaction. This coordination function is real. Simultaneously, the algorithmic engagement optimization extracts user attention and data for monetization. Platforms experience the constraint as coordination (their internal framing) rather than extraction (the user's experience), enabling them to justify the mechanism as necessary to the service. Enforcement is active and continuous — the algorithm itself is the enforcement mechanism, adapting in real time to user behavior. Exit is constrained by regulatory barriers and network effects that make alternative platforms difficult to maintain.
constraint_indexing:constraint_classification(algorithmic_engagement_loop, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY COALITION (SCAFFOLD) — Regulatory bodies (EU DSA, DMA, US legislative efforts, civil society groups) see the engagement loop as a temporary structural failure that regulatory interventions can address. The coalition has a concrete sunset clause: algorithm transparency requirements, attention-limiting mandates (e.g., disabling infinite scroll), separation of amplification from curation, and algorithmic auditing create alternative architectures. These measures are being implemented with 10-15 year timelines. The constraint is extractive today, but organized agents perceive a path to dismantling it through regulatory enforcement. Suppression is moderate because the mechanism is becoming increasingly visible to this perspective.
constraint_indexing:constraint_classification(algorithmic_engagement_loop, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: DEGRADED COORDINATION (PITON) — Originally, algorithmic curation was genuine coordination: sorting information by relevance and filtering noise for finite attention. The mechanism persists through institutional momentum, but the primary function has atrophied into engagement extraction. Platforms continue to describe algorithms as serving user interests ('helping you find what you care about'), but the actual optimization target is engagement and ad-revenue. The ritual of claiming coordination functions persists (terms of service language, public positioning) while the real mechanism has shifted to extraction. Theater ratio is moderate because the original coordination function is not entirely absent — algorithms do provide information filtering — but the optimization target is now overtly engagement, not user benefit.
constraint_indexing:constraint_classification(algorithmic_engagement_loop, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the algorithmic engagement loop exhibits genuine coordination (matching users with content at scale) alongside structural extraction (optimizing for engagement over wellbeing). The constraint requires active enforcement through continuous algorithmic optimization and network effects. Multiple victims (end users, information ecosystem, cognitive autonomy) and multiple beneficiaries (platforms, advertisers) coexist in a single system. The analytical position sees the structural ambiguity clearly: this is not pure extraction (coordination is real) and not pure coordination (extraction is structural). Classification as Tangled Rope is robust across measurement contexts.
constraint_indexing:constraint_classification(algorithmic_engagement_loop, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_engagement_loop_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_engagement_loop, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_engagement_loop, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_engagement_loop, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_engagement_loop, TR),
    TR >= 0.70.

:- end_tests(algorithmic_engagement_loop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The platform's optimization target is explicitly engagement rather than user welfare. This extracts ~27% of engagement as non-volitional additional engagement beyond what users would choose with perfect information and no dark patterns (omega variable). The measurement interval shows extractiveness increasing from 0.35 to 0.62 as algorithmic sophistication improved and optimization targets shifted explicitly to engagement. Suppression (0.68): High. Multiple suppression mechanisms: (1) algorithmic opacity prevents users from understanding why content is ranked as presented; (2) network effects create switching costs (users are locked in by their social graph); (3) notifications and variable rewards exploit cognitive biases; (4) information asymmetry allows platforms to hide true optimization targets; (5) psychological dependency reduces exit willingness. Theater ratio (0.55): Moderate. The original function (content curation and filtering) persists and is not purely theatrical — users do find content they value. However, the optimization target has shifted to engagement metrics unrelated to user benefit. Platforms maintain the public framing of 'helping you find what matters' while optimizing for 'maximizing your time on platform.' The theater has increased over the interval as this gap has widened.
 *
 * PERSPECTIVAL GAP:
 *   Maximum divergence across all perspectives. The snare perspective (end users, information ecosystem) perceives pure extraction with no coordination benefit. The rope perspective (advertisers, partial platform view) perceives pure coordination with no extraction cost. The tangled rope perspectives (content creators, analytical observer) perceive both functions simultaneously. The scaffold perspective perceives a solvable temporary problem with regulatory intervention. The piton perspective reveals that the original coordination justification persists as theater while the function has atrophied. This gap reflects genuine structural difference: different agents occupy fundamentally different positions in the extraction flow. There is no single 'correct' classification — the presheaf of perspectives IS the constraint structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from power level, exit options, and beneficiary/victim status. End users: low power (powerless), trapped exit → high d (≈0.95) → experience maximum χ. Content creators: moderate power, constrained exit, both beneficiary and victim status → moderate d (≈0.50) → experience moderate χ. Platform operators: high institutional power, arbitrage exit, beneficiary status → low d (≈0.15) → experience low or negative χ (benefit). Advertisers: institutional power, arbitrage exit, beneficiary status → low d (≈0.10) → experience low χ. Information ecosystem: no power, trapped, victim status → maximum d (≈1.0) → experience maximum χ. Regulatory coalition: organized power, constrained exit, both beneficiary (for users) and constrained by enforcement burden → moderate d (≈0.45) → moderate χ from regulatory perspective. No overrides required; the structural data directly maps to perspectival experience.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy through genuine tangled rope structure: the coordination function (matching users to content, connecting creators to audiences) is real and large-scale. The extraction function (optimizing for engagement over welfare) is also real and is the mechanism that enables scale by creating incentives for participation and content production. These are not the same function with different labels. However, the constraint exhibits clear asymmetry: the coordination benefit flows primarily to platform operators and advertisers; the extraction flows primarily from end users and the information ecosystem. Mandatrophy would arise if the 'engagement optimization' label obscured genuine extraction — but here the extraction is structural, not definitional. The regulatory coalition perspective shows how to disable the extraction (algorithm transparency, attention limits, separation of amplification from curation) while potentially preserving the coordination function through alternative architectures. This remains an open empirical question (omega variable), but the distinction between the two functions is structurally clear.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    user_agency_vs_addiction_design,
    'To what degree is the constraint''s suppressive power attributable to user cognitive limitations versus deliberate algorithmic design to exploit those limitations?',
    'Empirical comparison of engagement metrics across platforms with high vs low dark-pattern design; user exit behavior when attention-limiting interventions are implemented; neuroscience evidence of variable reward sensitivity in algorithmic systems',
    'If primarily designed exploitation: constraint is structural snare (requires regulatory intervention). If primarily user cognitive limitation: constraint is less extractive (may be rope with voluntary participation). High impact for policy response.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_agency_vs_addiction_design, empirical, 'Attribution of suppression between design intent and cognitive vulnerability').

omega_variable(
    coordination_function_necessity,
    'Is the coordination function (matching users to relevant content) achievable through alternative architectures that do not optimize for engagement, or is engagement optimization inherent to large-scale content curation?',
    'Comparative study of non-engagement-optimized algorithms (chronological feeds, user-controlled curation, algorithmic transparency with user tuning); measurement of user satisfaction and information quality across architectures',
    'If alternative architectures preserve coordination: tangled rope is decomposable (coordinate and extract are separable, not entangled). If engagement optimization is necessary for scale: tangled rope is structurally indivisible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether engagement optimization is inherent to content curation').

omega_variable(
    regulatory_architecture_feasibility,
    'Can regulatory interventions (algorithm transparency, attention limits, separation of curation from amplification) actually disable the engagement extraction mechanism without destroying the coordination function?',
    'Pilot implementation of regulatory architectures (EU DSA compliance, DMA enforcement); measurement of engagement metrics, user wellbeing, and platform viability under intervention',
    'If feasible: scaffold perspective is correct (sunset is real, extraction is disableable). If infeasible: scaffold is aspirational (extraction is structurally necessary to the service, sunset is false).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_architecture_feasibility, empirical, 'Whether regulation can disable extraction without destroying coordination').

omega_variable(
    attention_externality_measurement,
    'What is the accurate magnitude of attention extraction relative to the user''s informed preferences — i.e., how much additional engagement is generated by the algorithmic loop versus what users would choose if they had perfect information and no dark patterns?',
    'Randomized trials with transparent algorithm parameter disclosure; measurement of engagement changes when users can control optimization targets; comparison to user stated preferences in exit surveys',
    'If large (>50% of engagement is non-volitional): extraction is severe. If small (<20%): constraint may reclassify from snare to rope for significant user segment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attention_externality_measurement, empirical, 'Magnitude of non-volitional attention extraction').

omega_variable(
    identity_lock_mechanism_strength,
    'For users who express dissatisfaction with algorithmic engagement loops but continue participation, is the binding mechanism primarily structural (network effects, social dependency, alternative-access barriers) or primarily identity-fusion (inability to imagine social existence outside the platform)?',
    'Qualitative analysis of user exit narratives and blocking barriers; correlation between network size and exit difficulty; measurement of psychological distress in exit attempts; intervention studies where users disable notifications and curated recommendations',
    'If primarily structural: exit_options should be classified as trapped or constrained. If primarily identity-fusion: exit_options is identity_locked, revealing a cognitive capture mechanism distinct from material barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_strength, empirical, 'Whether user suppression is structural or cognitive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_engagement_loop, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algel_tr_t0, algorithmic_engagement_loop, theater_ratio, 0, 0.4).
narrative_ontology:measurement(algel_tr_t5, algorithmic_engagement_loop, theater_ratio, 5, 0.48).
narrative_ontology:measurement(algel_tr_t10, algorithmic_engagement_loop, theater_ratio, 10, 0.55).
narrative_ontology:measurement(algel_tr_t15, algorithmic_engagement_loop, theater_ratio, 15, 0.61).

% Extraction over time
narrative_ontology:measurement(algel_be_t0, algorithmic_engagement_loop, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algel_be_t5, algorithmic_engagement_loop, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(algel_be_t10, algorithmic_engagement_loop, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(algel_be_t15, algorithmic_engagement_loop, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_engagement_loop, resource_allocation).
narrative_ontology:affects_constraint(algorithmic_engagement_loop, attention_economy_scarcity).
narrative_ontology:affects_constraint(algorithmic_engagement_loop, social_media_misinformation_amplification).
narrative_ontology:affects_constraint(algorithmic_engagement_loop, dopamine_reward_targeting).

% DUAL FORMULATION NOTE:
% The algorithmic engagement loop decomposes into three structurally distinct constraints: (1) resource_allocation_coordination (matching users to content, attention scarcity problem) with distinct ε value; (2) misinformation_amplification (engagement-driven sensationalism) with higher ε; (3) dopamine_targeting (variable reward mechanisms) with its own ε. Each has different regulatory intervention points and different sunset timelines. The unified 'algorithmic engagement loop' story captures the system effect; the decomposed stories capture the mechanistic components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_engagement_loop, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
