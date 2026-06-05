% ============================================================================
% CONSTRAINT STORY: attention_capture_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_capture_asymmetry, []).

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
 *   constraint_id: attention_capture_asymmetry
 *   human_readable: Attention Capture Asymmetry
 *   domain: cognitive_economics/information_systems
 *
 * SUMMARY:
 *   Attention capture asymmetry describes the structural constraint that
 *   emerges when digital platforms optimize for user engagement through
 *   mechanisms that extract attention from users while distributing benefits
 *   to platforms and advertisers. The constraint operates at the intersection
 *   of cognitive science (limited attention), economic incentives
 *   (engagement-based monetization), technological capability (algorithmic
 *   manipulation of information presentation), and human psychology
 *   (vulnerability to intermittent reinforcement and social comparison).
 *   Users face engineered stimuli designed to capture and sustain attention,
 *   while platforms benefit from increased engagement metrics. The constraint
 *   exhibits all eight perspectives because the same structural mechanism —
 *   optimizing algorithmic recommendations to maximize user engagement time —
 *   appears as natural law from some viewpoints, legitimate coordination from
 *   others, and pure extraction from still others. The extractiveness
 *   trajectory (0.35 → 0.62 over 15 years) reflects increasing sophistication
 *   in engagement manipulation techniques: from simple ranking algorithms to
 *   deep neural network recommendations to multi-modal content synthesis to
 *   AI-generated personalized content. The theater ratio (0.40 → 0.68)
 *   reflects the growing performative content: engagement metrics become
 *   increasingly detached from actual user benefit as platforms layer
 *   optimization on top of optimization.
 *
 * KEY AGENTS:
 *   - Attention-Captured Users: Primary victims (powerless/trapped) — bear costs of attention extraction through reduced autonomy, cognitive load, and opportunity cost of alternative activities
 *   - Identity-Locked Creators: Secondary victims (powerless/identity_locked) — professional identity and income fused with platform engagement; theoretically mobile but functionally trapped
 *   - Skeptical Users with Partial Resistance: Moderate victims (moderate/constrained) — partially resist capture through technical measures; experience mixed coordination and extraction
 *   - Algorithmic Platforms: Primary beneficiaries (institutional/arbitrage) — capture engagement, monetize attention, benefit from network effects; perceive constraint as legitimate coordination
 *   - Advertisers: Secondary beneficiaries (powerful/mobile) — access targeted attention; benefit from engagement optimization; have exit options
 *   - Digital Rights Coalition: Organized agents (organized/constrained) — advocacy organizations, regulators, open-source alternatives building exit pathways with regulatory and technological sunset logic
 *   - Engagement Metric Theater: Institutional degradation (institutional/arbitrage) — performative infrastructure around metrics that diverges from actual platform function
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent platform design choices as inherent cognitive limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_capture_asymmetry, 0.62).
domain_priors:suppression_score(attention_capture_asymmetry, 0.58).
domain_priors:theater_ratio(attention_capture_asymmetry, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_capture_asymmetry, extractiveness, 0.62).
narrative_ontology:constraint_metric(attention_capture_asymmetry, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(attention_capture_asymmetry, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_capture_asymmetry, tangled_rope).
narrative_ontology:human_readable(attention_capture_asymmetry, "Attention Capture Asymmetry").
narrative_ontology:topic_domain(attention_capture_asymmetry, "cognitive_economics/information_systems").

domain_priors:requires_active_enforcement(attention_capture_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_capture_asymmetry, attention_extractors).
narrative_ontology:constraint_beneficiary(attention_capture_asymmetry, algorithmic_platforms).
narrative_ontology:constraint_victim(attention_capture_asymmetry, attention_bearers).
narrative_ontology:constraint_victim(attention_capture_asymmetry, epistemic_commons).
narrative_ontology:constraint_victim(attention_capture_asymmetry, autonomous_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATTENTION-CAPTURED USER (SNARE) — Users face engineered stimuli designed to capture and sustain attention. Exit costs are prohibitive: the digital infrastructure is essential for employment, social coordination, and information access. Suppression is structural (no realistic alternative platforms) and psychological (intermittent reinforcement via notifications and engagement metrics). Experienced extraction is maximal — the user cannot exit without material life disruption.
constraint_indexing:constraint_classification(attention_capture_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IDENTITY-LOCKED CREATOR (SNARE) — Content creators, influencers, and platform-dependent workers are trapped by identity fusion with their audience and platform presence. They have structural mobility (could build alternative presence, find other work) but cannot exercise it because their professional identity, income stream, and self-concept are constituted through platform engagement metrics. Suppression is both structural (algorithm-driven visibility) and internalized (identity-based). Classification is snare because extraction is severe despite theoretical mobility.
constraint_indexing:constraint_classification(attention_capture_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: SKEPTICAL USER WITH PARTIAL AWARENESS (TANGLED ROPE) — Users who recognize attention-capture mechanisms and partially resist them (using app timers, disabling notifications, curating feeds) experience the constraint as both coordination and extraction. Coordination value: social connection, information access, community participation. Extraction value: engagement metrics that monetize their attention, algorithmic nudging, reduced autonomy. Exit is constrained (switching platforms is costly but possible; digital detox is possible but high-cost). Mixed classification reflects genuine coordination function alongside asymmetric extraction.
constraint_indexing:constraint_classification(attention_capture_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ALGORITHMIC PLATFORM (ROPE) — Platforms experience the attention-capture mechanism as coordination: matching users to content, enabling creators to reach audiences, monetizing attention through advertising or subscription. From the platform's perspective, engagement optimization is legitimate system function. No perceived extraction — only coordination benefits (liquidity, scale, engagement). Exit options are excellent (platforms can pivot business models, engage or disengage from attention capture). The constraint appears as pure coordination from this vantage.
constraint_indexing:constraint_classification(attention_capture_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ADVERTISER BENEFICIARY (TANGLED ROPE) — Advertisers benefit from attention-capture mechanisms (targeted reach, behavioral targeting, conversion optimization) and also depend on the integrity of the attention market. If attention capture becomes too exploitative, it degrades attention quality (users become ad-blind, engagement becomes hollow). Advertisers have mobility (can shift to different platforms or media) but benefit from the current system. Mixed classification: genuine coordination function (matching buyers with relevant audiences) plus asymmetric extraction (from users' attention, not from advertisers themselves). This perspective experiences low extraction because they are beneficiaries.
constraint_indexing:constraint_classification(attention_capture_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DIGITAL RIGHTS COALITION (SCAFFOLD) — Organized actors (privacy advocates, platform regulations, open-source alternatives, digital literacy initiatives) perceive the attention-capture constraint as a temporary problem with a sunset clause. Regulatory frameworks (GDPR, DMA, state attention laws), open-source social platforms, and growing digital literacy norms are building alternative coordination pathways that reduce asymmetric extraction. Low effective extraction because the coalition has agency and sees an exit path through regulation and technological alternatives. Theater is moderate — regulation can be theater, but some enforcement is real.
constraint_indexing:constraint_classification(attention_capture_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ENGAGEMENT METRIC THEATER (PITON) — From a civilizational view, the performative infrastructure around engagement metrics (likes, shares, watch time, recommendations) is largely theater: these metrics are proxies for user value, not the actual coordination function. The primary function (connecting users and creators) is real; the theater (optimizing engagement metrics as an end in itself) is degraded institutional ritual. Theater ratio is high because engagement optimization often contradicts actual platform goals (user wellbeing, information quality, community health). The piton classification reflects the disconnect between stated and actual function, maintained through corporate inertia.
constraint_indexing:constraint_classification(attention_capture_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational frame, attention capture appears immutable: human attention is inherently limited, salient stimuli naturally capture awareness, and coordinating large information networks requires some filtering mechanism. The constraint looks like an inescapable feature of cognitive architecture and scale. However, the structural data reveals this as false naturalization: the specific mechanisms of capture (notification design, algorithmic recommendations, engagement metrics as KPIs, dark patterns) are contingent institutional choices, not natural laws. The engine's false summit detector identifies this perspective as perspectival capture.
constraint_indexing:constraint_classification(attention_capture_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_capture_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_capture_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_capture_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_capture_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_capture_asymmetry, TR),
    TR >= 0.70.

:- end_tests(attention_capture_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High but not maximal. Users experience significant attention extraction through engineered engagement mechanisms, but the constraint contains genuine coordination value (social connection, content discovery, information access). The extractiveness is not 0.85+ because some users perceive real benefits and some platforms provide actual utility. The trajectory from 0.35 to 0.62 reflects accumulating sophistication in engagement optimization techniques: early platforms had blunt engagement loops; current platforms use deep learning to model individual attention vulnerabilities and target exploitable cognitive patterns. Suppression (0.58): Substantial. Users face structural barriers (digital infrastructure essential for employment, education, social coordination) and psychological barriers (habit formation, social comparison, fear of missing out). Exit costs are high but not prohibitive—genuine alternatives exist but require coordination and sacrifice. Theater ratio (0.65): Moderate-high. Engagement metrics (likes, shares, watch time, recommendation scores) are increasingly theater—proxies for value that have become ends in themselves. Platforms measure and optimize engagement metrics not because engagement equals user benefit, but because engagement is monetizable and easy to measure. The theater increases as engagement optimization layers become more sophisticated and more disconnected from actual user satisfaction.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival disagreement is maximal. Users see snare; platforms see rope; regulators see scaffold; academic observers risk seeing mountain. This gap reveals that indexical classification is not about determining 'what the constraint really is' but about measuring how different structural positions experience the same mechanism. The platform's rope experience is not false—they genuinely coordinate users with content and creators with audiences. The user's snare experience is not false—they genuinely experience extraction with no exit. Both are true from their positions. The gap exists because power, exit options, and benefit distributions are asymmetric. Resolving the gap requires asking: from whose perspective is this constraint legitimate? If legitimacy is measured from the most vulnerable position (powerless trapped users), the classification is snare. If measured from the beneficiary position (platforms), it is rope. If measured from an organized coalition trying to build alternatives, it is scaffold. The engine's task is not to choose which perspective is 'correct' but to measure the gap and report it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from power, exit options, and beneficiary/victim status. Powerless trapped users (d ≈ 0.95) experience maximum extraction; institutional arbitrage beneficiaries (d ≈ 0.05) experience minimal extraction; analytical observers (d ≈ 0.72) have observational distance. The identity-locked creator (d ≈ 0.89) derives high d from victim status + identity_locked exit despite moderate power level—the cognitive binding is structurally equivalent to being trapped. Moderately constrained users (d ≈ 0.65) experience medium extraction because they have some agency and some benefits. The effective extractiveness χ = ε × f(d) × σ(S) scales the base extractiveness (0.62) by the sigmoid function f(d) for each perspective and the scope modifier σ(S). Global scope (σ=1.2) amplifies extraction; national scope (σ=1.0) uses baseline. The directionality derivation shows why the same constraint produces such different classifications: powerless agents with high d experience high χ (snare); beneficiaries with low d experience low or negative χ (rope); moderate agents with medium d and some agency experience medium χ (tangled_rope).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint is not mislabeled as pure extraction (snare) when it contains coordination function, nor mislabeled as pure coordination (rope) when it contains extraction. The claimed type (tangled_rope) correctly identifies the hybrid: genuine coordination (users benefit from content discovery, creators reach audiences, platforms enable social connection) plus asymmetric extraction (users' attention is monetized without equivalent compensation, creators are dependent on algorithmic visibility, information quality is degraded by engagement optimization). The mandatrophy is resolved by acknowledging that the coordination and extraction are structurally coupled—you cannot remove the extraction without removing the coordination, and vice versa. Attempts to 'fix' attention capture by removing engagement optimization degrade platform function (cold-start problem, discovery breakdown, creator reach collapse). This is not mandatrophy but genuine structural coupling: the same mechanism that enables coordination also enables extraction. The constraint is legitimately tangled_rope, not a mislabeled snare or rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attention_quality_vs_quantity_threshold,
    'What threshold distinguishes legitimate attention coordination from exploitative attention capture?',
    'Longitudinal user autonomy studies: measure user-reported agency, volitional attention allocation, and alignment between intended vs actual usage patterns. Compare platforms with different engagement optimization intensities.',
    'If threshold is low (most engagement optimization is exploitation): snare classification predominates. If threshold is high (much optimization is legitimate coordination): tangled_rope becomes primary classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_quality_vs_quantity_threshold, empirical, 'Threshold distinguishing coordination from exploitation in attention mechanisms').

omega_variable(
    identity_lock_prevalence_creators,
    'What proportion of platform-dependent creators are genuinely identity-locked versus constrained by economic dependency alone?',
    'Qualitative interviews with creators who have exited platforms; measurement of identity-fusion strength (how much creator identity remains after platform separation); follow-up analysis of post-exit earnings and psychological adjustment.',
    'If identity-lock is prevalent (>60%): creator perspectives should use identity_locked exit option more broadly. If rare (<20%): most creators are constrained by economics alone, not identity fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_prevalence_creators, empirical, 'Degree of identity-lock in platform-dependent creator populations').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is user suppression primarily structural (no realistic alternatives) or internalized (users believe they cannot exit even when alternatives exist)?',
    'Comparison of users in markets with multiple competing platforms vs single-platform-dominant markets; measurement of perceived exit cost vs actual switch cost; post-switch satisfaction and functionality comparison.',
    'If structural: suppression metric remains high regardless of user awareness. If internalized: suppression decreases with digital literacy and awareness campaigns. Classification shifts from snare toward constrained if internalized suppression is dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether user suppression is structural or internalized').

omega_variable(
    algorithmic_intent_exploitation_vs_coordination,
    'Do platform algorithms optimize for user engagement as a legitimate coordination mechanism (matching users to valued content) or as exploitation mechanism (maximizing capture regardless of user benefit)?',
    'Causal analysis: compare outcomes when engagement optimization is aligned vs misaligned with user-reported satisfaction and autonomy. Measure rates of user regret, unintended usage, and autonomy violation across different algorithmic designs.',
    'If coordination dominant: platform''s rope perspective is accurate. If exploitation dominant: platform should be reclassified from rope toward snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_intent_exploitation_vs_coordination, empirical, 'Whether algorithmic optimization serves coordination or exploitation').

omega_variable(
    regulatory_sunset_feasibility,
    'Are regulatory pathways (GDPR, DMA, state attention laws) actually building functional alternative coordination mechanisms, or is regulation becoming performative theater?',
    'Longitudinal analysis of regulatory enforcement: track whether regulations reduce measured attention extraction (user autonomy metrics) or merely change surface compliance. Monitor emergence and viability of alternative platforms (Mastodon, Bluesky, open-source alternatives).',
    'If regulation is building real alternatives: scaffold classification is accurate and sunset is plausible. If regulation becomes theater: constraint reclassifies toward piton (degraded ritual) despite regulatory appearance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_sunset_feasibility, empirical, 'Whether regulation is building real alternatives or becoming theater').

omega_variable(
    natural_law_vs_contingent_architecture,
    'How much of attention asymmetry is inherent to cognitive architecture versus contingent to platform design choices?',
    'Comparative analysis of platforms with deliberately low engagement optimization (Mastodon, Bluesky, private social networks) vs high-engagement platforms; measurement of user autonomy, voluntary engagement time, and satisfaction. Neuroscientific studies of attention capture under different stimulus design principles.',
    'If majority is contingent: mountain classification is false naturalization. If majority is inherent: some form of mountain or rope classification is justified. Results inform whether attention capture is redesignable or structurally necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_architecture, empirical, 'Proportion of attention capture inherent vs contingent to design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_capture_asymmetry, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attcap_tr_t0, attention_capture_asymmetry, theater_ratio, 0, 0.4).
narrative_ontology:measurement(attcap_tr_t5, attention_capture_asymmetry, theater_ratio, 5, 0.55).
narrative_ontology:measurement(attcap_tr_t10, attention_capture_asymmetry, theater_ratio, 10, 0.65).
narrative_ontology:measurement(attcap_tr_t15, attention_capture_asymmetry, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(attcap_be_t0, attention_capture_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(attcap_be_t5, attention_capture_asymmetry, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(attcap_be_t10, attention_capture_asymmetry, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(attcap_be_t15, attention_capture_asymmetry, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_capture_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(attention_capture_asymmetry, algorithmic_information_filtering).
narrative_ontology:affects_constraint(attention_capture_asymmetry, engagement_metric_amplification).
narrative_ontology:affects_constraint(attention_capture_asymmetry, creator_platform_dependency).

% DUAL FORMULATION NOTE:
% Attention capture asymmetry is upstream of more specific platform constraints. Algorithmic information filtering (which content is visible) has its own ε reflecting the technical specificity of recommendation systems; engagement metric amplification (behavioral feedback loops) has its own ε reflecting the feedback mechanism. These constraints are siblings in a family where attention capture is the parent structural phenomenon. Each sibling has different temporal dynamics, different victim groups, and different regulatory pathways, but all depend on the existence of the parent constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(attention_capture_asymmetry, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
