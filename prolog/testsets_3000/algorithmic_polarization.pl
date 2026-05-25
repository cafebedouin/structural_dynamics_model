% ============================================================================
% CONSTRAINT STORY: algorithmic_polarization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_polarization, []).

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
 *   constraint_id: algorithmic_polarization
 *   human_readable: Algorithmic Polarization in Recommendation Systems
 *   domain: digital_platforms/information_systems/political_economy
 *
 * SUMMARY:
 *   Algorithmic polarization emerges from the structural tension between
 *   platform operators' engagement-maximization objective and users' need for
 *   shared epistemic commons. Recommendation systems optimized for
 *   click-through, watch-time, and ad-serving naturally amplify content that
 *   triggers strong emotional responses — outrage, fear, tribal identity
 *   affirmation — which correlates with political polarization. This
 *   constraint exhibits tangled rope dynamics: platforms provide genuine
 *   coordination function (content discovery at scale) alongside asymmetric
 *   extraction (engagement converted to advertising value, network
 *   fragmentation imposed on users). The constraint's extractiveness has
 *   risen from 0.35 to 0.58 over six years as algorithmic sophistication
 *   increased and engagement optimization became industry standard. Theater
 *   ratio has risen from 0.25 to 0.48 as platforms deployed performative
 *   solutions (content moderation, fact-checking labels, transparency
 *   reports) that address symptoms without restructuring the underlying
 *   ranking mechanism. The constraint is neither immutable law nor pure
 *   coordination — it is an institutional arrangement with real alternatives
 *   (diversity-optimizing algorithms, subscription models, regulatory
 *   oversight) that platforms can choose but do not.
 *
 * KEY AGENTS:
 *   - Individual Users: Primary victims (powerless/trapped) — atomized by network effects; cannot exit without abandoning platform access; fragmented into information silos
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — extract engagement and advertising value through recommendation amplification; can exit to alternative models but profit-optimize engagement
 *   - Engagement-Maximization Mechanisms: The structural mechanism (no agent but system-level) — algorithmic ranking functions that amplify high-engagement content regardless of epistemic quality
 *   - Organized Civil Society: Secondary actors (moderate/constrained) — advocacy groups, researchers, democratic institutions need platform access but suffer polarization effects on constituencies
 *   - Regulatory Coalitions: Organized agents (organized/constrained) — EU DSA, platform accountability frameworks building alternative verification pathways with sunset logic
 *   - Alternative Platform Operators: Counter-institutional actors (organized/constrained) — Bluesky, Mastodon, community forums coordinating non-engagement-maximizing values while constrained by network effects
 *   - Content Moderation Systems: Institutional ritual (institutional/arbitrage) — performative apparatus that persists through inertia despite low functional correction capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_polarization, 0.58).
domain_priors:suppression_score(algorithmic_polarization, 0.62).
domain_priors:theater_ratio(algorithmic_polarization, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_polarization, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_polarization, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(algorithmic_polarization, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_polarization, tangled_rope).
narrative_ontology:human_readable(algorithmic_polarization, "Algorithmic Polarization in Recommendation Systems").
narrative_ontology:topic_domain(algorithmic_polarization, "digital_platforms/information_systems/political_economy").

domain_priors:requires_active_enforcement(algorithmic_polarization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_polarization, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_polarization, engagement_maximizers).
narrative_ontology:constraint_victim(algorithmic_polarization, users_fragmented_information).
narrative_ontology:constraint_victim(algorithmic_polarization, collective_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATOMIZED USER (SNARE) — Individual users cannot exit algorithmic feeds without abandoning platform access entirely. Trapped by network effects and digital necessity; see fragmented information environment as inevitable. Maximum experienced extraction — algorithmic filtering optimizes for engagement, not understanding, and users bear the cost of polarization without visibility into the mechanism.
constraint_indexing:constraint_classification(algorithmic_polarization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORGANIZED CIVIL SOCIETY (TANGLED ROPE) — Advocacy groups, researchers, and democratic institutions face constrained exit: they need platform access for reach but suffer from polarization effects on their constituencies. They benefit from algorithmic visibility for their messages but also bear costs when the same mechanisms amplify extremes. Intermediate extraction — some agency through collective action, some genuine coordination function, significant asymmetric burden.
constraint_indexing:constraint_classification(algorithmic_polarization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the constraint as coordination: algorithmic ranking solves the genuine problem of content curation at massive scale. Extracts engagement and advertising value through the same mechanism. Net beneficiary with full exit flexibility — can arbitrage to alternative business models (subscription, diversity metrics, hybrid approaches) but chooses engagement-maximization. The constraint appears as pure coordination from this perspective.
constraint_indexing:constraint_classification(algorithmic_polarization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — EU Digital Services Act, platform accountability frameworks, algorithmic transparency mandates represent organized attempts to restructure the constraint with a sunset. These regulations create alternative verification pathways (algorithmic audits, content labeling, recommendation diversity requirements) that bypass pure engagement-maximization. See the polarization constraint as temporary institutional artifact yielding to regulatory pressure. Sunset logic applies: compliance mechanisms are building alternatives.
constraint_indexing:constraint_classification(algorithmic_polarization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONTENT MODERATION APPARATUS (PITON) — Traditional moderation (community guidelines, fact-checking labels, removal policies) is largely performative theater: moderators cannot keep pace with content volume, guidelines are applied inconsistently, and removal generates backlash narratives (censorship claims). The moderation ritual persists through institutional inertia despite low functional correction of polarization. Theater ratio high (0.48 overall reflects mix; moderation alone ~0.65) because the system performs responsibility without addressing the underlying algorithmic mechanism.
constraint_indexing:constraint_classification(algorithmic_polarization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ALTERNATIVE PLATFORM (TANGLED ROPE) — Decentralized, smaller, or niche platforms (Bluesky, Mastodon, community forums) experience the constraint differently: they coordinate users around non-engagement-maximizing values (diversity, local control, reduced algorithmic amplification) while bearing costs of smaller network effects and reduced monetization capacity. Benefits from escape narrative and genuine coordination function; constrained by network effects that make switching costly for users. Mixed extraction.
constraint_indexing:constraint_classification(algorithmic_polarization, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INFORMATION GEOMETRY VIEW (MOUNTAIN) — From an information-theoretic perspective, recommender systems operating under engagement metrics face an immutable constraint: systems that maximize user engagement necessarily amplify content that triggers strong emotional responses (anger, fear, outrage polarize more strongly than moderate positions). This is not a policy choice but a mathematical limit. However, the structural data reveals this as naturalization — the 'mathematical inevitability' frame obscures that engagement-maximization is a choice. Alternative objective functions (diversity, accuracy, serendipity) have different amplification profiles. Engine flags this as false summit.
constraint_indexing:constraint_classification(algorithmic_polarization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_polarization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_polarization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_polarization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_polarization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_polarization, TR),
    TR >= 0.70.

:- end_tests(algorithmic_polarization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and rising. Platform operators extract engagement-to-advertising-value conversion; users bear cost of polarization, information fragmentation, and attention capture. The extraction is not total (platforms provide genuine content discovery) but substantial and systematic. Rising trajectory reflects sophistication of engagement optimization — early platforms (2015) used simpler ranking; current systems employ reinforcement learning, neural ranking, and behavioral targeting to maximize engagement. Suppression (0.62): High. Users face barriers to exit: network effects (social graph locked to platforms), information necessity (news/communication via platforms), switching costs (reputation/followers lost), lack of algorithmic transparency (users cannot see mechanism they're trapped in). Alternative platforms exist but with reduced network effects. Suppression has remained stable even as awareness increased — visibility of the mechanism has not substantially reduced exit costs. Theater ratio (0.48): Moderate and rising. Content moderation and transparency initiatives represent partial theater — they perform responsibility without addressing the ranking mechanism. But theater is not dominant — platforms do actually filter, label, and explain some recommendations. The rising trajectory reflects defensive theater deployment as polarization criticism mounted (2018-2024). Theater is response to external pressure, not primary function.
 *
 * PERSPECTIVAL GAP:
 *   Platform operators' rope perspective requires reframing extraction as 'solving coordination.' Users' snare perspective sees the same mechanism as trapping. The gap reveals that beneficiaries internalize the coordination narrative while victims experience extraction. Organized civil society's tangled rope view is structurally accurate — they both benefit from platform reach and bear costs of polarization. Regulatory view sees sunset (alternative mechanisms being built). Alternative platform view demonstrates that the constraint is not inevitable — systems can be built with different objectives at the cost of network scale. The mountain perspective naturalizes what the alternative platforms disprove.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness chi derives from position in the engagement pipeline. Platform operators (institutional/arbitrage) experience low d (full beneficiaries with exit flexibility) → low chi despite high base ε. Users (powerless/trapped) experience high d (targets with no exit) → high chi. Civil society (moderate/constrained) occupies middle position with some organizational capacity → moderate chi. Regulatory coalitions (organized/constrained) have agency through institutional process → lower chi via path to exit. Alternative platforms (organized/constrained) genuinely coordinate non-extraction values but constrained by network effects → mixed chi. The directionality flow is unidirectional: engagement → platforms, while polarization costs → users. No symmetric coordination appears in base structural data despite rope claims from beneficiary perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the classification depends critically on observable choice: WHAT DO WE MEASURE? If we measure engagement-driven outcomes (clicks, watch-time, shares), the ranking algorithm appears successful (rope — coordinates users to content). If we measure epistemic quality or opinion diversity, the same algorithm fails (snare — fragments commons). If we measure against regulatory standards (transparency, non-discrimination), platforms show partial compliance (piton — performative theater). The underlying structural constraint is the objective function (engagement-maximization), not the algorithm itself. Platforms' choice to maximize engagement is not forced by technology — alternative objectives (diversity, epistemic quality, serendipity, local-first) have different and demonstrably less-polarizing amplification profiles. The mandatrophy resolves by making the design choice visible: platforms can choose non-engagement-maximizing objectives; they profit-optimize engagement instead. This transforms the classification from natural law (mountain) to institutional arrangement (tangled rope with beneficiary choice).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engagement_vs_polarization_causal_direction,
    'Does algorithmic engagement-maximization cause user polarization, or do polarized users self-select into algorithmic feeds that amplify their views?',
    'Randomized intervention studies comparing engagement-maximizing vs diversity-maximizing algorithms on same user cohorts; temporal precedence analysis of recommendation changes vs user belief formation',
    'If causal (algorithm → polarization): snare classification confirmed. If correlational (polarized users seek amplification): constraint becomes coordination problem (Rope). If bidirectional: tangled rope dynamics with feedback loops.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engagement_vs_polarization_causal_direction, empirical, 'Causal direction between engagement optimization and user polarization').

omega_variable(
    platform_business_model_plasticity,
    'Can platforms profitably operate under non-engagement-maximizing objectives (diversity, epistemic quality, serendipity) at scale?',
    'Economic analysis of alternative business models; pilot programs with reduced engagement optimization; user willingness-to-pay for lower-polarization feeds',
    'If profitable alternatives exist: platforms'' choice to maximize engagement is extractive (not a constraint). If engagement-maximization is necessary for platform viability: constraint is more natural (platforms face their own pressures). Determines whether beneficiary position is structural or contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_business_model_plasticity, empirical, 'Whether non-engagement-maximizing models are economically viable at scale').

omega_variable(
    algorithmic_transparency_sufficiency,
    'Does algorithmic transparency (explaining recommendation rankings to users) reduce experienced polarization or create new forms of gaming and manipulation?',
    'A/B testing transparency interventions; user behavior change analysis; gaming attempts (users manipulating systems with knowledge of algorithms); longitudinal tracking of polarization metrics post-transparency',
    'If transparency reduces polarization: scaffold sunset logic strengthened (regulatory requirements can work). If transparency is gamed: constraint persists (informed users still cannot exit). If heterogeneous effects: some users benefit, others harmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transparency_sufficiency, empirical, 'Whether algorithmic transparency reduces polarization or enables gaming').

omega_variable(
    collective_action_fragmentation,
    'Can atomized users coordinate collective action (demand alternative algorithms, migrate platforms, regulatory pressure) before polarization fractures their coalition?',
    'Historical analysis of platform migration waves; coalition formation in regulatory processes; tracking of user organization around algorithmic demands',
    'If users can coordinate before fracture: snare reclassifies toward tangled rope with coalition potential. If polarization prevents coordination: snare is stable; exit options remain trapped. Determines whether dynamic coalition extension applies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_fragmentation, empirical, 'Whether users can coordinate collective action against algorithmic polarization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_polarization, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algpol_tr_t0, algorithmic_polarization, theater_ratio, 0, 0.25).
narrative_ontology:measurement(algpol_tr_t3, algorithmic_polarization, theater_ratio, 3, 0.38).
narrative_ontology:measurement(algpol_tr_t6, algorithmic_polarization, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(algpol_be_t0, algorithmic_polarization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algpol_be_t3, algorithmic_polarization, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(algpol_be_t6, algorithmic_polarization, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_polarization, information_standard).
narrative_ontology:affects_constraint(algorithmic_polarization, epistemic_commons_fragmentation).
narrative_ontology:affects_constraint(algorithmic_polarization, platform_network_effects).
narrative_ontology:affects_constraint(algorithmic_polarization, attention_economy_extraction).

% DUAL FORMULATION NOTE:
% Algorithmic polarization decomposes into three structurally distinct constraints: (1) epistemic_commons_fragmentation (ε≈0.45) — information asymmetry and selective exposure at user level; (2) platform_network_effects (ε≈0.55) — switching costs and lock-in effects; (3) attention_economy_extraction (ε≈0.62) — advertiser extraction of user attention value. This story captures their intersection. The upstream epistemic fragmentation claim has contested empirical status (some studies show algorithmic effects are smaller than self-selection); the downstream attention extraction is high-confidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_polarization, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
