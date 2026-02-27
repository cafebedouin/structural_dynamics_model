% ============================================================================
% CONSTRAINT STORY: sm_addictive_design
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sm_addictive_design, []).

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
 *   constraint_id: sm_addictive_design
 *   human_readable: Social Media Addictive Design Features
 *   domain: technological/platform_economics
 *
 * SUMMARY:
 *   Social media addictive design represents a system of coordinated
 *   technological features (notification systems, infinite scroll, variable
 *   reward schedules, social validation metrics, algorithmic amplification)
 *   optimized to maximize user engagement and time-on-platform. This
 *   constraint operates as pure extraction from the end-user perspective but
 *   appears as coordination from the platform operator and advertiser
 *   perspectives. The structural asymmetry is stark: platforms and
 *   advertisers benefit from maximized engagement, while users bear the costs
 *   of attention capture, behavioral manipulation, and in the case of
 *   adolescents, disrupted cognitive development during critical neural
 *   windows. The constraint's extractiveness (0.58) and suppression (0.68)
 *   reflect high coercion and minimal perceived alternatives for individual
 *   users. The theater_ratio (0.35) indicates that while platforms deploy
 *   performative narratives around 'community' and 'connection,' the primary
 *   function is straightforward extraction of attention for monetization —
 *   the theater is lower than in many Tangled Rope constraints because the
 *   coordination function is minimal from the user's perspective.
 *
 * KEY AGENTS:
 *   - End Users (Individual Consumers): Primary victim (powerless/trapped) — bear attention extraction and behavioral manipulation costs; few individual exit options
 *   - Adolescents: Vulnerable victim subclass (powerless/trapped at development stage) — cannot meaningfully consent; exposure during neural development window creates capacity-based extraction
 *   - Attention Commons: Abstract collective victim (powerless/trapped) — public good degradation; reduced societal focus capacity and democratic deliberation quality
 *   - Platform Operators (Meta, Google, ByteDance, etc.): Primary beneficiary (institutional/arbitrage) — extract attention and behavioral data for monetization; have exit options (alternative business models) but benefit from current arrangement
 *   - Advertisers: Secondary beneficiary (institutional/arbitrage) — benefit from engagement-amplified targeting; have alternative platforms available
 *   - Content Creators: Intermediate victim (moderate/constrained) — depend on algorithmic amplification for reach but also constrained by algorithmic opacity and policy changes; mixed extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sm_addictive_design, 0.58).
domain_priors:suppression_score(sm_addictive_design, 0.68).
domain_priors:theater_ratio(sm_addictive_design, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sm_addictive_design, extractiveness, 0.58).
narrative_ontology:constraint_metric(sm_addictive_design, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sm_addictive_design, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sm_addictive_design, snare).
narrative_ontology:human_readable(sm_addictive_design, "Social Media Addictive Design Features").
narrative_ontology:topic_domain(sm_addictive_design, "technological/platform_economics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sm_addictive_design, platform_operators).
narrative_ontology:constraint_beneficiary(sm_addictive_design, advertisers).
narrative_ontology:constraint_victim(sm_addictive_design, end_users).
narrative_ontology:constraint_victim(sm_addictive_design, adolescent_cognitive_development).
narrative_ontology:constraint_victim(sm_addictive_design, attention_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Individual users lack viable alternatives and bear the full extraction cost through attention capture and behavioral manipulation. Notification systems, infinite scroll, variable reward schedules, and social validation metrics create compulsive use patterns. Exit requires coordinated collective action. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(sm_addictive_design, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ADOLESCENT COGNITIVE DEVELOPMENT (SNARE) — Cannot consent to or opt out of exposure during critical neural development windows. Features exploit reward-seeking and social-validation circuits during formation. Supression is structural and near-total in childhood/early adolescence. d≈0.98, f(d)≈1.50, σ=1.2 → χ≈1.02 (capped at theoretical limits by χ formula bounds).
constraint_indexing:constraint_classification(sm_addictive_design, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATORS (ROPE) — Experience addictive design as a coordination solution: aligning user engagement with advertising spend and shareholder returns. The constraint enables information exchange between users and platforms, mediated by algorithmic matching. Operators have arbitrage exit (can shift to alternative revenue models) and see coordination benefits. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.08. Net beneficiary.
constraint_indexing:constraint_classification(sm_addictive_design, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISERS (ROPE) — Experience addictive design as a coordination mechanism: solving the matching problem between their products and attention-captured users. High engagement = high conversion probability. Arbitrage exit (can shift to other platforms) and pure coordination function with minimal extraction from advertiser perspective. d≈0.10, f(d)≈-0.10, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(sm_addictive_design, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONTENT CREATORS (TANGLED ROPE) — Depend on platform algorithms for reach and income, but also benefit from engagement optimization (their content gets amplified). However, they are constrained by algorithmic opacity and platform policy changes. Mixed extraction and coordination: they both exploit and are exploited by the addictive mechanics. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(sm_addictive_design, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational view, addictive design extracts from the shared attention commons — a public good that once degraded, affects collective coordination capacity, democratic deliberation, and long-term social well-being. The constraint is viewed as extraction from a statistical aggregate (society-wide attention and focus capacity). d≈0.85, f(d)≈1.20, σ=1.2 → χ≈0.83.
constraint_indexing:constraint_classification(sm_addictive_design, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sm_addictive_design_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sm_addictive_design, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sm_addictive_design, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sm_addictive_design, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sm_addictive_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Addictive design mechanisms systematically extract attention and behavioral data from users in exchange for access to social features. The extraction is not maximal (0.7+) because users do retain some agency (can reduce usage, switch platforms, use features selectively), but the mechanics are specifically engineered to minimize these exit paths. Measured as: (time extracted / total user time) × (behavioral autonomy loss) × (data monetization value). Suppression (0.68): Moderate-high. Significant suppression of alternatives comes from: (a) network effects (everyone is on the major platforms), (b) lack of transparent alternatives (most people unaware addictive mechanics can be removed), (c) algorithmic opacity (users cannot see why content is amplified), (d) switching costs (social graph re-establishment), and (e) developmental stage (adolescents cannot fully evaluate alternatives). Theater ratio (0.35): Low-moderate. Platforms deploy narrative theater around 'bringing people together,' 'authentic connection,' and 'community,' but this is subordinate to the actual function: monetizing attention through engagement optimization. The theater is present (marketing, community guidelines, fact-checking) but less central than in Tangled Rope or Piton constraints — the primary mechanism is transparent extraction via observable metrics (likes, shares, watch time). The theater has increased over the interval as regulatory pressure mounted, pushing platforms to invest in corporate responsibility narratives.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is maximal and structural. Platform operators and advertisers classify the constraint as Rope (coordination: matching users with content and ads). End users classify it as Snare (extraction: attention capture with minimal exit). Adolescents see an even more extreme Snare (incapacity-based extraction). Content creators occupy a middle position (Tangled Rope) — they benefit from engagement optimization but are also constrained by it. The analytical observer can take multiple views: the attention commons perspective (Snare of civilization-scale attention degradation), or a developmental perspective (Snare of cognitive development extraction). No agent sees Rope except the beneficiaries. No agent sees coordination except from the platform's own narrowly-scoped goal (maximizing engagement). The gap reflects the fundamental misalignment: what the platform calls 'engagement coordination' is what users experience as 'attention extraction.' The metrics (extractiveness=0.58, suppression=0.68) support the user and analytical observer classifications; the platform operators' Rope classification comes from their structural position as beneficiaries with arbitrage exit, not from intrinsic constraint properties.
 *
 * DIRECTIONALITY LOGIC:
 *   End users: Victim + trapped → d≈0.92, f(d)≈1.38. Maximal extraction from individual perspective. Users cannot exit without coordination and lack information about behavioral mechanics. Adolescents: Victim + trapped + incapacity → d≈0.98, f(d)≈1.50. Near-maximal extraction from capacity-based perspective. Developmental stage prevents meaningful consent or exit reasoning. Platform operators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Have alternative revenue models available (subscription, licensing, federation) but current model maximizes revenue. Advertisers: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.10. Net beneficiary. Can shift advertising budgets to alternative platforms. Content creators: Mixed victim/beneficiary + constrained → d≈0.55, f(d)≈0.75. Constrained because they depend on platform algorithms for reach but cannot opt out of algorithmic opacity. Attention commons: Victim (structural aggregate) + no exit → d≈0.85, f(d)≈1.20. High extraction from collective perspective. No entity represents attention commons interests; degradation persists.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy (the conflation of coordination with extraction) by maintaining clear beneficiary/victim boundaries. Platform operators genuinely coordinate users with content (Rope from their perspective), but this coordination is secondary to their primary function: extracting attention for monetization (Snare from user perspective). The constraint does NOT claim that coordination and extraction are the same thing; it recognizes that the same mechanisms serve different functions for different agents. The mandatrophy would arise if an analyst claimed 'addictive design IS coordination for users' — but the metrics (suppression=0.68, extractiveness=0.58) and the structural data (beneficiary/victim split) prevent this confusion. The snare classification is confirmed across the powerless, analytical, and adolescent perspectives, with only the beneficiary perspectives seeing rope. This pattern (snare from victims, rope from beneficiaries) is exactly the structure that resolves mandatrophy: the constraint exhibits both coordination and extraction, but they are structurally asymmetric and directed at different agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_compulsive_boundary,
    'At what threshold does engagement optimization become behavioral addiction rather than normal persuasion?',
    'Neuroscientific studies of dopamine response patterns; comparison of behavioral metrics (time-to-compulsion, withdrawal symptoms, interference with other activities) across user populations',
    'If threshold low (design features trigger measurable addiction): snare classification confirmed for all perspectives. If threshold high (users retain substantial agency): reframe as tangled_rope or piton (degraded coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_compulsive_boundary, empirical, 'Boundary between persuasion and behavioral addiction').

omega_variable(
    alternative_coordination_viability,
    'Could advertising-funded social coordination exist without addictive design mechanics?',
    'A/B testing of non-addictive engagement features on major platforms; market analysis of alternative revenue models (subscription, data marketplace, federation); user behavior under reduced-engagement designs',
    'If viable: addictive design is contingent extraction, not inherent coordination requirement — snare classification strengthened. If not viable: extract is bound to coordination function — tangled_rope from platform operator perspective becomes primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_viability, empirical, 'Whether advertising can be sustained without addictive mechanics').

omega_variable(
    adolescent_consent_capacity,
    'What developmental age marks sufficient prefrontal maturity for meaningful consent to addictive design exposure?',
    'Longitudinal neuroimaging and behavioral studies of impulse control development; correlation between age-of-exposure and long-term behavioral patterns; cross-cultural adoption studies',
    'If development complete by age 16: current adolescent exposure may be recoverable (tangled rope). If development extends past age 21 or requires protective periods: exposure to under-21s is extraction from incapacity (maximal snare). Direct implication for regulation severity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adolescent_consent_capacity, empirical, 'Developmental maturity for addictive design consent').

omega_variable(
    algorithmic_opacity_extractiveness,
    'How much of the extractiveness (0.58) derives from algorithmic opacity vs. from the addictive features themselves?',
    'Comparison of user behavior and satisfaction under transparent (disclosed algorithms) vs. opaque design; regulatory transparency mandates (EU AI Act, digital services acts) and behavioral outcomes; user agency under informed-choice conditions',
    'If primarily opacity: fixing transparency reduces χ to ~0.30, reclassifying toward rope. If primarily mechanics: transparency has minimal effect, snare classification stands. Determines whether regulation focuses on transparency or feature restriction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_opacity_extractiveness, empirical, 'Attribution of extractiveness to opacity vs. design mechanics').

omega_variable(
    collective_action_threshold,
    'At what user-base coordination level do end users transition from powerless to organized?',
    'Historical analysis of platform migration waves (Reddit, Bluesky, Mastodon); critical mass thresholds for alternative adoption; network effects analysis',
    'If threshold low (1-5% coordinated exit): end users may organize collective exit, upgrading to organized/mobile perspective. If threshold very high: coordination deficit persists (powerless/trapped remains dominant). Affects whether snare classification is temporally stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'Collective action threshold for end-user coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sm_addictive_design, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smad_tr_t0, sm_addictive_design, theater_ratio, 0, 0.18).
narrative_ontology:measurement(smad_tr_t7, sm_addictive_design, theater_ratio, 7, 0.27).
narrative_ontology:measurement(smad_tr_t15, sm_addictive_design, theater_ratio, 15, 0.35).

% Extraction over time
narrative_ontology:measurement(smad_be_t0, sm_addictive_design, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(smad_be_t7, sm_addictive_design, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(smad_be_t15, sm_addictive_design, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sm_addictive_design, resource_allocation).
narrative_ontology:affects_constraint(sm_addictive_design, attention_scarcity_market).
narrative_ontology:affects_constraint(sm_addictive_design, adolescent_mental_health_extraction).
narrative_ontology:affects_constraint(sm_addictive_design, data_monetization_apparatus).

% DUAL FORMULATION NOTE:
% Addictive design is downstream of advertising-funded platform economics and upstream of specific mental health outcomes (depression, anxiety, sleep disruption in adolescents). The extractiveness value (0.58) reflects the attention-extraction mechanism; downstream constraints decompose the specific damage pathways (mental health, cognitive development). Upstream constraint (advertising model) has different ε reflecting the market structure choice rather than behavioral mechanics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sm_addictive_design, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
