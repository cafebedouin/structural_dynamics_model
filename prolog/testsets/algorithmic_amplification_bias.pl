% ============================================================================
% CONSTRAINT STORY: algorithmic_amplification_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_amplification_bias, []).

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
 *   constraint_id: algorithmic_amplification_bias
 *   human_readable: Algorithmic Amplification Bias in Digital Content Distribution
 *   domain: technology/information_systems/social_dynamics
 *
 * SUMMARY:
 *   Algorithmic amplification bias is a constraint where platform operators
 *   design and deploy recommendation systems that systematically privilege
 *   content engagement, resulting in differential visibility and distribution
 *   advantages for some demographic groups, content types, and producers over
 *   others. The constraint exhibits core tension between the genuine
 *   coordination function (matching users to relevant content at planetary
 *   scale) and asymmetric extraction (capturing value through engagement
 *   metrics, data, and attention asymmetry). The extractiveness has grown
 *   over the interval (0.35 → 0.58) as competitive pressures intensify
 *   engagement optimization and as algorithmic sophistication enables more
 *   precise targeting of user attention. Theater ratio has also risen (0.42 →
 *   0.64) as platforms implement public transparency measures, algorithmic
 *   explanations, and audit frameworks that perform accountability without
 *   substantially altering the underlying optimization targets. The
 *   constraint is a prime exemplar of Tangled Rope classification: it
 *   simultaneously solves a real coordination problem (information
 *   distribution at scale) and implements asymmetric extraction (visibility
 *   asymmetry, engagement capture, data extraction). The classification
 *   prevents two opposing errors: (1) naive coordination reading that ignores
 *   the bias and extraction components, and (2) pure extraction reading that
 *   ignores the genuine coordination contribution.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture engagement metrics, advertisement revenue, user attention, and dataset value; design and maintain the algorithmic constraint
 *   - Marginalized Demographic Communities: Primary victims (powerless/trapped) — face systematically reduced visibility, engagement, and representation in algorithmic ranking; cannot exit without life-domain costs
 *   - Epistemic Commons: Secondary victim (powerless/trapped) — accuracy and truthfulness are deprioritized relative to engagement; misinformation amplification damages shared knowledge bases
 *   - Content Creator Ecosystem: Mixed beneficiary-victim (moderate/constrained) — some benefit from algorithmic reach while others face visibility penalties based on demographic/topical alignment with engagement metrics
 *   - High-Engagement Content Producers: Secondary beneficiaries (powerful/mobile) — influencers and media companies aligned with engagement optimization thrive; can extract from the system while maintaining mobile exit options
 *   - Content Moderation System: Institutional actor (institutional/constrained) — maintains performative accountability while actual amplification of harmful content continues; constrained by resource limitations and coordination with algorithmic systems
 *   - Regulatory and Advocacy Coalition: Organized agents (organized/constrained) — building alternative governance pathways through transparency mandates, interoperability requirements, and algorithmic auditing; constrained by incumbent platform market power
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent design choices as inherent to information systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_amplification_bias, 0.58).
domain_priors:suppression_score(algorithmic_amplification_bias, 0.68).
domain_priors:theater_ratio(algorithmic_amplification_bias, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_amplification_bias, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_amplification_bias, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algorithmic_amplification_bias, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_amplification_bias, tangled_rope).
narrative_ontology:human_readable(algorithmic_amplification_bias, "Algorithmic Amplification Bias in Digital Content Distribution").
narrative_ontology:topic_domain(algorithmic_amplification_bias, "technology/information_systems/social_dynamics").

domain_priors:requires_active_enforcement(algorithmic_amplification_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_amplification_bias, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_amplification_bias, high_engagement_content_producers).
narrative_ontology:constraint_victim(algorithmic_amplification_bias, marginalized_demographic_representation).
narrative_ontology:constraint_victim(algorithmic_amplification_bias, epistemic_commons_accuracy).
narrative_ontology:constraint_victim(algorithmic_amplification_bias, algorithmic_system_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED USER POPULATION (SNARE) — Users whose content or communities are systematically deprioritized by algorithmic ranking bear extraction with no exit. Digital platforms are essential infrastructure for social/economic participation; users cannot opt out without significant life costs. Algorithm opacity prevents meaningful exit signaling. Maximum experienced extraction — no alternatives, no voice in system design.
constraint_indexing:constraint_classification(algorithmic_amplification_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONTENT CREATOR ECOSYSTEM (TANGLED ROPE) — Creators benefit from algorithmic distribution and audience reach (genuine coordination function) while also suffering from the bias-driven winner-take-all dynamics (asymmetric extraction). High variance in outcomes; some creators thrive while others face visibility penalties for demographic/linguistic/topical reasons. Constrained exit: building alternative platforms is capital-intensive and network effects favor incumbents.
constraint_indexing:constraint_classification(algorithmic_amplification_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Algorithms solve the coordination problem of matching users to relevant content at massive scale. Operators experience the constraint as a technical mechanism enabling their core function. Net beneficiary: engagement metrics, ad revenue, and data extraction all flow toward platforms. Arbitrage exit: can modify algorithms without market penalty because switching costs are high and alternatives are fragmented.
constraint_indexing:constraint_classification(algorithmic_amplification_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONTENT MODERATION SYSTEM (PITON) — Nominally designed to prevent harmful content distribution, but operates largely as theater: rule-based systems are easily gamed, harmful content reaches users anyway (often amplified by engagement algorithms), and moderation resources are grossly insufficient. The system persists through institutional requirement (liability protection) rather than functional effectiveness. Theater ratio high: compliance rituals and public reporting of moderation actions maintain appearance of control while actual harm distribution continues.
constraint_indexing:constraint_classification(algorithmic_amplification_bias, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AND ADVOCACY COALITION (SCAFFOLD) — Organized agents (regulators, civil society, interoperability advocates) treat algorithmic amplification as a temporary coordination failure with policy-driven sunset. EU Digital Services Act, algorithmic transparency mandates, and interoperability requirements create alternative governance pathways. Constraint experiences as reducible through regulatory enforcement. Sunset logic: mandatory transparency, user choice, and auditing gradually shift power away from platform-controlled black-box algorithms. Estimated sunset: 5-15 years as regulations mature and enforcement mechanisms develop.
constraint_indexing:constraint_classification(algorithmic_amplification_bias, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: HIGH-ENGAGEMENT CONTENT PRODUCER ELITE (TANGLED ROPE) — Powerful agents (influencers, media companies, political actors) who align with platform algorithms enjoy genuine coordination benefits (audience reach, monetization) while simultaneously wielding the algorithm to suppress competitors and amplify extraction-favorable narratives. Mobile exit (can move between platforms) but benefits are high enough to stay. Asymmetric extraction flows toward other agents; this group extracts from the system. High extractiveness from their perspective as extractors (not targets).
constraint_indexing:constraint_classification(algorithmic_amplification_bias, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing algorithmic bias as inherent to information-at-scale: 'engagement metrics necessarily favor sensationalism,' 'recommender systems must optimize user retention,' 'some amplification asymmetry is inevitable.' But structural data reveals this is contingent institutional design, not physical law. Engine false summit detector identifies this as naturalization of policy choices (recommendation algorithms could optimize for accuracy, diversity, or democratic participation instead of engagement).
constraint_indexing:constraint_classification(algorithmic_amplification_bias, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_amplification_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_amplification_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_amplification_bias, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_amplification_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_amplification_bias, TR),
    TR >= 0.70.

:- end_tests(algorithmic_amplification_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value through engagement metrics, data collection, and attention asymmetry, but not maximal because platforms genuinely coordinate information distribution — users do benefit from algorithmic matching even while experiencing the extraction. The value reflects that the extraction is embedded within a legitimate coordination function rather than pure rent-seeking. Suppression (0.68): High. Users face structural barriers to exit (network effects, essential infrastructure role, switching costs) and informational barriers to understanding (algorithm opacity, limited algorithmic literacy). Platforms maintain control through technical and institutional means. Theater ratio (0.64): Moderate-high. Platforms implement public-facing transparency measures, algorithmic explanations, and audit frameworks that create appearance of accountability without substantially changing optimization targets. The performative component has grown as regulatory and public pressure increase — more explanations, more audits, same engagement maximization underneath.
 *
 * PERSPECTIVAL GAP:
 *   Strongest gap between platform operators (Rope: they genuinely coordinate information distribution) and marginalized users (Snare: they experience extraction with no exit). Second-strongest gap between high-engagement producers (Tangled Rope as extractors) and marginalized communities (Tangled Rope as targets) — same classification type but opposite directionality and experienced chi. Regulatory perspective (Scaffold) differs from operator perspective (Rope) on whether the constraint is mutable: operators treat algorithmic design as essentially fixed given engagement metrics and competitive dynamics; regulators treat it as contingently designed and modifiable through policy. The mountain perspective (natural law framing) is revealed as false by the structural alternatives (viable algorithms optimizing for accuracy or diversity rather than engagement) — the engine's false summit detector identifies this as naturalization of contingent design choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality derives from their structural position relative to the constraint. Platform operators are beneficiaries with arbitrage exit options — d ≈ 0.05-0.15, yielding negative or minimal f(d), experiencing the constraint as enabling rather than extractive. Marginalized users are victims with trapped exit — d ≈ 0.95, yielding f(d) ≈ 1.42, experiencing maximum extracted chi. Content creators have mixed positions: some are secondary beneficiaries (high-engagement alignment, mobile exit, d ≈ 0.35-0.45, moderate positive chi), others are victims with constrained exit (low-engagement demographics, d ≈ 0.70-0.80, high chi). The regulatory coalition has organized power and constrained exit due to incumbent resistance, producing moderate d and moderate experienced chi — they can act but face resistance. The piton classification of content moderation derives from theater ratio ≥ 0.70, not from high chi — the system shows the characteristics of institutional inertia and performative function maintenance rather than high-extraction extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint avoids mandatrophy by clearly declaring the dual character. Beneficiaries include platform operators (genuine coordination benefit of information distribution) and high-engagement content producers (extraction benefit from visibility asymmetry). Victims include marginalized demographic communities (systematically reduced visibility) and epistemic commons (accuracy deprioritization). Requires active enforcement (platforms must implement and maintain algorithmic ranking). All three Tangled Rope gates are satisfied: coordination function (information distribution), asymmetric extraction (engagement/visibility asymmetry), and active enforcement (algorithmic systems require maintenance and tuning). The constraint is not being mislabeled as pure coordination (Rope) because the asymmetric extraction component is undeniable — marginalized users face real visibility penalties that aren't explained by content quality or relevance. It is also not being mislabeled as pure extraction (Snare) because the information distribution coordination function is genuine and benefits users even while extracting value. The mandatrophy prevents both errors simultaneously. The Piton perspective on content moderation is properly distinguished from the main Tangled Rope classification — moderation is a subsidiary system with high theater ratio that operates within the larger algorithmic constraint but has its own degraded-institution signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engagement_optimization_vs_harm_causation,
    'Does algorithmic amplification bias arise from the optimization target (engagement metrics) or from deployment choices independent of the target?',
    'Comparative analysis of algorithms optimized for different objectives (engagement vs accuracy vs diversity); audit of actual algorithm coefficients vs stated design goals; A/B testing different objective functions in controlled settings',
    'If target-driven: changing optimization function reduces bias (Scaffold sunset is real). If independent: algorithmic designers actively choose to amplify bias despite alternatives (Snare from more perspectives, higher malice component).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engagement_optimization_vs_harm_causation, empirical, 'Whether bias is inherent to engagement optimization or a deliberate design choice').

omega_variable(
    platform_knowledge_of_amplification_bias,
    'Did platform operators know about differential amplification bias in their algorithms prior to public disclosure, and if so, what was their decision rationale?',
    'Internal document discovery; employee testimony; timeline correlation between internal research findings and public statements; comparison with competitor internal findings',
    'If knowingly deployed: extractive intent is clear, snare classification is dominant (intentional harm, not accident). If genuinely unaware: negligent extraction (Tangled Rope remains appropriate). If known but deployment was required by engagement metrics: shifts blame from operator to institutional structure (Tangled Rope justified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_knowledge_of_amplification_bias, empirical, 'Platform operator knowledge and intent regarding amplification bias').

omega_variable(
    alternative_algorithm_feasibility,
    'Are algorithmically feasible alternatives to engagement-optimized ranking (e.g., diversity-optimized, accuracy-optimized, proportional representation) technically implementable at platform scale without major service degradation?',
    'Academic literature review of alternative optimization objectives; feasibility studies from independent researchers; pilot deployments by platforms or researchers; computational complexity comparison',
    'If alternatives are feasible: platform choice to use engagement optimization is contingent (Scaffold and Piton perspectives are strengthened). If alternatives are infeasible: engagement optimization is a structural necessity (Mountain perspective gains plausibility; constraint may be partially natural law).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_algorithm_feasibility, empirical, 'Technical feasibility of alternative ranking algorithms').

omega_variable(
    user_awareness_of_amplification_mechanism,
    'What proportion of platform users understand that they are seeing algorithmic amplification, understand its bias properties, and have meaningful opportunity to opt out or customize?',
    'User surveys on algorithmic literacy; analysis of platform interface transparency (how visible are algorithmic explanations?); measurement of actual opt-out/customization take-up rates; comparison of user understanding before/after transparency disclosures',
    'If awareness is low: suppression is structural and high (trapped perspective justified). If awareness is high: users have constrained rather than trapped exit (Tangled Rope shifts to Rope-like). If customization is widely available: exit options improve (mobile or arbitrage becomes plausible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_awareness_of_amplification_mechanism, empirical, 'User awareness and literacy regarding algorithmic amplification').

omega_variable(
    demographic_representation_measurement_ambiguity,
    'How is ''bias'' in algorithmic amplification measured and operationalized? (Equal representation? Equal engagement rates? Equal visibility in discovery? Proportional to population? Proportional to user base?) Does the measurement choice determine the classification outcome?',
    'Comparative analysis of multiple bias metrics applied to the same algorithm; examination of which metrics show bias and which show parity; determination of whether metric choice determines whether constraint is classified as Snare vs Rope',
    'If metric choice determines classification: the constraint may be partially measurement-dependent (suggests separate constraint stories per metric). If bias signal is robust across metrics: measurement-independent classification is justified. High impact for mandatrophy — different metrics could claim different types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_representation_measurement_ambiguity, empirical, 'Metric choice in measuring algorithmic amplification bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_amplification_bias, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algamp_tr_t0, algorithmic_amplification_bias, theater_ratio, 0, 0.42).
narrative_ontology:measurement(algamp_tr_t5, algorithmic_amplification_bias, theater_ratio, 5, 0.54).
narrative_ontology:measurement(algamp_tr_t10, algorithmic_amplification_bias, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(algamp_be_t0, algorithmic_amplification_bias, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algamp_be_t5, algorithmic_amplification_bias, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(algamp_be_t10, algorithmic_amplification_bias, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_amplification_bias, information_standard).
narrative_ontology:affects_constraint(algorithmic_amplification_bias, information_ecosystem_misinformation_amplification).
narrative_ontology:affects_constraint(algorithmic_amplification_bias, attention_economy_winner_take_all).
narrative_ontology:affects_constraint(algorithmic_amplification_bias, platform_market_concentration).

% DUAL FORMULATION NOTE:
% Algorithmic amplification bias can be decomposed into several structurally distinct constraints: (1) engagement-optimization-driven bias (this story), (2) demographic representation in training data bias (upstream), (3) content moderation failure (downstream theatrical system). Each has different ε and different perspectives. This story focuses on the amplification mechanism itself; decomposition preferred for independent treatment of training-data bias and moderation system degradation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_amplification_bias, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
