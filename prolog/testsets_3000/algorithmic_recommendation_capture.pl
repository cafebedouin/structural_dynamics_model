% ============================================================================
% CONSTRAINT STORY: algorithmic_recommendation_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_recommendation_capture, []).

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
 *   constraint_id: algorithmic_recommendation_capture
 *   human_readable: Algorithmic Recommendation Capture
 *   domain: digital_platforms/information_systems
 *
 * SUMMARY:
 *   Algorithmic recommendation systems on digital platforms coordinate the
 *   matching of users to content while simultaneously capturing user
 *   attention and behavioral data through engagement-maximization objectives.
 *   The constraint operates at the intersection of genuine coordination
 *   (solving information retrieval at scale) and asymmetric extraction
 *   (attention harvesting, behavior modification, epistemic autonomy
 *   reduction). The extractiveness trajectory (0.35→0.58) reflects the
 *   increasing sophistication of engagement optimization over the measured
 *   interval. The theater_ratio trajectory (0.42→0.68) reveals the growing
 *   gap between platform statements about recommendation transparency and the
 *   actual opacity/complexity of algorithmic decision-making. This constraint
 *   exhibits Tangled Rope classification from the analytical and ecosystem
 *   perspectives because it combines real coordination function with active
 *   enforcement of extraction mechanisms. Users experience it as Snare
 *   (trapped in engagement feeds). Creators experience it as Tangled Rope
 *   (genuine distribution network with embedded asymmetric capture). Platform
 *   operators experience it as Rope (pure coordination problem solved). The
 *   constraint demonstrates how a structurally hybrid mechanism can have
 *   dramatically different classifications depending on the observer's
 *   structural position and exit options.
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/trapped) — attention is harvested through engagement-maximizing algorithms; exit is blocked by network effects and ubiquity of similar systems
 *   - Content Creators: Secondary beneficiary and victim (moderate/constrained) — benefit from platform distribution reach; constrained by algorithmic preferencing and demonetization mechanisms; can exit but at high cost
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — extract engagement data, attention, and revenue from algorithmic prioritization; experience the system as pure coordination problem solved
 *   - Information Ecosystem: Tertiary victim (organized/constrained) — misinformation and sensationalism amplified at equivalent rates to legitimate information; partial agency through platform visibility controls; suppressed by algorithmic opacity
 *   - Trust & Safety Frameworks: Institutional constraint-maintenance apparatus (institutional/arbitrage) — performs transparency and governance functions while algorithmic extraction continues; maintains institutional legitimacy through theater
 *   - Analytical Observer: Sees structural hybridity (analytical/analytical) — both genuine coordination and genuine extraction are operating simultaneously; the constraint cannot be classified as pure either
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_recommendation_capture, 0.58).
domain_priors:suppression_score(algorithmic_recommendation_capture, 0.65).
domain_priors:theater_ratio(algorithmic_recommendation_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_recommendation_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_recommendation_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(algorithmic_recommendation_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_recommendation_capture, tangled_rope).
narrative_ontology:human_readable(algorithmic_recommendation_capture, "Algorithmic Recommendation Capture").
narrative_ontology:topic_domain(algorithmic_recommendation_capture, "digital_platforms/information_systems").

domain_priors:requires_active_enforcement(algorithmic_recommendation_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_recommendation_capture, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_recommendation_capture, high_engagement_content_producers).
narrative_ontology:constraint_victim(algorithmic_recommendation_capture, end_users_attention).
narrative_ontology:constraint_victim(algorithmic_recommendation_capture, information_ecosystem_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Users are trapped within recommendation feeds designed to maximize engagement, not utility. No meaningful exit: all major platforms use similar algorithms. Suppression is structural (network effects, switching costs, psychological addiction mechanisms). The constraint appears as pure extraction: attention is harvested, behavior is shaped, without substantial coordination benefit to the user.
constraint_indexing:constraint_classification(algorithmic_recommendation_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONTENT CREATOR (TANGLED ROPE) — Creators face genuine coordination benefit (platform distribution reaches audiences) alongside asymmetric extraction (algorithmic preferencing, demonetization risk, opacity of ranking criteria). Exit is costly (requires building independent audience) but possible. They experience both the rope (legitimate distribution network) and the snare (recommendation capture by engagement metrics).
constraint_indexing:constraint_classification(algorithmic_recommendation_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Sees the algorithmic recommendation system as pure coordination: enabling content discovery, solving the information retrieval problem, connecting audiences to creators. From the platform's institutional perspective, the system is a genuine solution to a collective action problem. Net beneficiary through engagement growth and advertiser revenue.
constraint_indexing:constraint_classification(algorithmic_recommendation_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INFORMATION ECOSYSTEM (TANGLED ROPE) — Organized systems (journalism, fact-checking, scientific communication) experience both coordination and extraction. Algorithms can amplify niche information; they also amplify misinformation at equivalent rates. The ecosystem has partial agency (some platforms offer visibility controls) but faces suppression through algorithmic opacity and platform policy changes. Active enforcement of engagement-maximization objectives constrains ecosystem diversity.
constraint_indexing:constraint_classification(algorithmic_recommendation_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRUST & SAFETY FRAMEWORKS (PITON) — Content moderation and recommendation governance structures are substantially performative. High theater_ratio reflects that platforms publish recommendation principles and safety reports while algorithmic prioritization continues to drive engagement through mechanisms that conflict with stated values. The institutional apparatus persists through regulatory pressure and reputation management, not functional verification.
constraint_indexing:constraint_classification(algorithmic_recommendation_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The system genuinely solves information retrieval (coordination function: matching users to relevant content). It simultaneously extracts user attention, behavioral data, and epistemic autonomy through engagement-maximization objectives that conflict with user welfare. Both functions are structurally real — the constraint is hybrid, not pure extraction masked as coordination.
constraint_indexing:constraint_classification(algorithmic_recommendation_capture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_recommendation_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_recommendation_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_recommendation_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_recommendation_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_recommendation_capture, TR),
    TR >= 0.70.

:- end_tests(algorithmic_recommendation_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The engagement-maximization objective extracts user attention, behavioral data, and epistemically-mediated autonomy. However, the extraction is not total because the system does solve a genuine information retrieval problem — users do encounter relevant content. The 58% reflects the asymmetry: the platform captures proportionally more value (behavioral data, monetized engagement) than users extract (information discovery). Suppression (0.65): High. Barriers to exit include network effects (all friends and content are on the same platform), switching costs (identity/history loss), psychological capture (engagement metrics drive dopaminergic feedback loops), and the absence of credible alternatives at scale. Theater_ratio (0.68): High. Platforms publish extensive documentation about recommendation principles, fairness criteria, and transparency commitments. The theater consists of high-level principle statements while actual algorithmic behavior remains opaque. Content moderation and safety teams perform visible governance while engagement-maximization objectives continue to prioritize attention-capturing content. The theater has increased over the interval as platforms face regulatory scrutiny and public criticism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival gap. Platform operators genuinely experience a Rope classification — they are solving a real technical problem (information retrieval at scale) and believe engagement-maximization is a legitimate fitness function for recommendation quality. End users experience a Snare classification — they have no meaningful exit, no transparency into ranking criteria, and no ability to modify objectives that directly affect their information diet. Content creators experience Tangled Rope — they benefit from distribution reach (rope) while facing demotion or demonetization mechanisms (snare). The information ecosystem experiences Tangled Rope — the system both enables niche community formation (rope) and amplifies misinformation proportionally to legitimate information (snare). The gap reveals that the same technical system is genuinely a coordination solution from the platform's perspective and genuinely an extraction mechanism from the user's perspective. The gap is not a measurement error — it is the diagnostic signature of successful capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim status and exit options. End users are trapped victims: high d (d≈0.92). Content creators are constrained beneficiary-victims: d ≈ 0.55 (mixed status with moderate exit cost). Platform operators are beneficiaries with arbitrage exit: d ≈ 0.10 (low extraction experienced). The information ecosystem has organized agency but constrained exit: d ≈ 0.60. Each derives d from their structural relationship to the flow of attention and behavioral data. The scoped extractiveness χ varies by observer: the end user experiences high χ (global scope, high σ); the creator experiences moderate χ (national scope, constrained exit); the platform experiences low/negative χ (beneficiary status + arbitrage). The analytical observer sees χ ≈ 0.48 (moderate scope σ, global surveillance extent, analytical exit producing d ≈ 0.72).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that Tangled Rope classification is correct from the analytical perspective: the coordination function (information retrieval) is genuine, the extraction mechanism (engagement-maximization serving platform interests) is genuine, and active enforcement of the extraction (algorithmic prioritization, opacity, behavioral nudging) is evident. The classification prevents miscategorization as pure Rope (coordination only) by requiring declaration of both beneficiaries and victims with asymmetric treatment. It prevents miscategorization as pure Snare by acknowledging that users do receive genuine utility from recommendation systems — they discover content they value at higher-than-random rates. The mandatrophy is resolved by the structural data: beneficiaries (platforms), victims (users), and active enforcement (engagement-maximization algorithms) are all present and documented.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engagement_metric_legitimacy,
    'Is engagement-maximization a proxy for user welfare, or a fundamentally misaligned objective?',
    'Longitudinal user satisfaction studies controlling for engagement; analysis of engagement-maximizing vs user-preferred recommendations on held-out cohorts; measurement of behavioral outcomes (time spent, information quality consumed, choice satisfaction post-use)',
    'If engagement proxies for welfare: classification reverts toward Rope (legitimate coordination metric with optimization error). If orthogonal/antagonistic: classification remains Tangled Rope (genuine extraction embedded in coordination function). If negatively correlated: classification approaches Snare (engagement-maximization actively harms user welfare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engagement_metric_legitimacy, empirical, 'Whether engagement metrics align with user welfare or represent misaligned extraction').

omega_variable(
    algorithmic_opacity_necessity,
    'Is recommendation algorithm opacity necessary for competitive viability, or a choice that enables extraction?',
    'Comparative analysis of platforms with varying transparency levels; study of recommendation quality and extraction rates under transparency constraints; market structure analysis of whether transparency disadvantages platforms relative to closed-box competitors',
    'If necessary: suppression is structural coordination cost (Rope). If chosen: opacity is active extraction mechanism (Snare/Tangled Rope classification strengthened). If varies by platform: identifies which operators use opacity for extraction vs structural reasons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_opacity_necessity, empirical, 'Whether algorithm opacity is inherent to coordination or enables extraction').

omega_variable(
    attention_hoarding_equilibrium,
    'Would algorithmic capture persist if competing platforms offered non-engagement-maximizing recommendation systems, or does the engagement-maximization race reflect a coordination failure (all platforms trapped in zero-sum extraction competition)?',
    'Market entry of alternative platforms emphasizing different recommendation objectives; comparison of user outcomes and platform stability across recommendation philosophies; analysis of whether monopoly power constrains competitor strategy space',
    'If alternative designs are viable: engagement-maximization is a choice, not a necessity (extraction). If impossible at scale: the constraint is a tragic commons (all platforms forced into extraction to compete), shifting the locus of extraction from platforms to market structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attention_hoarding_equilibrium, empirical, 'Whether engagement-maximization recommendation is inevitable or chosen extraction').

omega_variable(
    user_autonomy_internalization,
    'Do users internalize engagement-maximizing recommendations as their own preferences, or do they recognize the misalignment between algorithmic feed and their own goals?',
    'User interviews and revealed preference studies; measurement of awareness of algorithmic selection; comparison of stated preferences vs recommended content; behavioral analysis before/after transparency interventions',
    'If internalized: exit_options should be identity_locked rather than trapped (users cannot see the constraint even if material barriers fell). If recognized: classification remains trapped/constrained (material suppression dominates). If mixed: different user cohorts experience different exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_autonomy_internalization, empirical, 'Whether users perceive algorithmic capture as constraint or internalize it as preference').

omega_variable(
    creator_capture_asymmetry,
    'Do algorithmic preferences systematically advantage certain creator types (high-engagement content, sensational framing) such that the platform captures a disproportionate share of creator value, or is the distribution relatively fair?',
    'Comparison of creator income distribution to content merit/quality metrics; analysis of algorithmic advantage for viral vs educational/niche content; measurement of income volatility tied to algorithmic changes',
    'If asymmetric advantage: creator capture is active extraction mechanism (snare component strengthened). If fair distribution: extraction primarily flows from users (tangled rope maintained). If random/unpredictable: extraction is compounded by uncertainty (suppression increases).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(creator_capture_asymmetry, empirical, 'Whether algorithms systematically advantage certain creator types and extract creator value').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_recommendation_capture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arc_tr_t0, algorithmic_recommendation_capture, theater_ratio, 0, 0.42).
narrative_ontology:measurement(arc_tr_t5, algorithmic_recommendation_capture, theater_ratio, 5, 0.55).
narrative_ontology:measurement(arc_tr_t10, algorithmic_recommendation_capture, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(arc_be_t0, algorithmic_recommendation_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(arc_be_t5, algorithmic_recommendation_capture, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(arc_be_t10, algorithmic_recommendation_capture, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_recommendation_capture, information_standard).
narrative_ontology:affects_constraint(algorithmic_recommendation_capture, attention_economy_extraction).
narrative_ontology:affects_constraint(algorithmic_recommendation_capture, behavioral_data_capture).
narrative_ontology:affects_constraint(algorithmic_recommendation_capture, information_asymmetry_platform_design).

% DUAL FORMULATION NOTE:
% Algorithmic recommendation capture is upstream of several derivative constraints. The capture mechanism itself (this story) is distinct from the attention economy dynamics it enables and the behavioral data extraction it facilitates. Each has different ε values reflecting different measurement bases: recommendation capture focuses on engagement-maximization misalignment; attention economy focuses on aggregate time-displacement effects; behavioral data capture focuses on privacy and predictive modeling asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_recommendation_capture, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
