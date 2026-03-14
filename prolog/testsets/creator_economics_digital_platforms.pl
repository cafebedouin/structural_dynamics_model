% ============================================================================
% CONSTRAINT STORY: creator_economics_digital_platforms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creator_economics_digital_platforms, []).

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
 *   constraint_id: creator_economics_digital_platforms
 *   human_readable: Creator Economics on Digital Platforms
 *   domain: digital_economy/labor/platform_governance
 *
 * SUMMARY:
 *   Creator economics on digital platforms represents a structural hybrid
 *   constraint combining genuine coordination gains with asymmetric
 *   extraction. Platforms solve the problem of monetizing distributed content
 *   creation — creators reach global audiences without traditional
 *   gatekeeping. But this coordination is inseparable from platform rent
 *   extraction: mandatory revenue sharing (30-50%), algorithm opacity,
 *   unilateral policy enforcement, and data ownership asymmetry. The
 *   constraint exhibits all six classification types depending on observer
 *   position. Individual trapped creators experience Snare. Organized creator
 *   networks experience Rope and negotiation leverage. Platform operators
 *   experience profitable Snare (from the creator side). Legacy media
 *   structures persist as Piton (broadcast-era contracts degraded). The
 *   analytical observer sees Tangled Rope: genuine coordination (global
 *   reach, automated payments, audience infrastructure) entangled with
 *   asymmetric extraction (rent capture, algorithm control, policy
 *   unilateralism). Base extractiveness has risen from 0.35 (2014-2016, early
 *   YouTube Creator economy emergence) to 0.58 (2024-2026) as platforms have
 *   consolidated market power and refined extraction mechanisms. Theater
 *   ratio has risen from 0.52 (platform transparency claims) to 0.68
 *   (increasingly performative creator support initiatives while core
 *   extraction mechanisms remain opaque).
 *
 * KEY AGENTS:
 *   - Individual Content Creators: Primary victim (powerless/trapped) — structurally dependent on platform for income distribution, audience access, and payment processing; no viable alternatives at comparable scale
 *   - Mid-Tier Creator Networks: Secondary victim (moderate/constrained) — gaining negotiation power through collective action; experience mixed coordination (audience reach) and extraction (algorithm control)
 *   - Creator Guilds / Professional Organizations: Organized actors (organized/arbitrage) — emerging collective actors with multiplatform leverage and direct-to-fan infrastructure options; enable coordination solutions
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture 30-50% revenue share, control algorithm ranking, set policy unilaterally; experience constraint as profitable extraction mechanism
 *   - Venture Capital Investors: Secondary beneficiary (institutional/arbitrage) — benefit from platform growth and valuations; fund extraction-maximizing business models
 *   - Legacy Media / Broadcast Industry: Institutional persistence (institutional/constrained) — broadcast-era talent management and contracts persist despite misalignment with creator economics; carry piton characteristics
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees tangled rope structure: genuine coordination embedded with asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creator_economics_digital_platforms, 0.58).
domain_priors:suppression_score(creator_economics_digital_platforms, 0.65).
domain_priors:theater_ratio(creator_economics_digital_platforms, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creator_economics_digital_platforms, extractiveness, 0.58).
narrative_ontology:constraint_metric(creator_economics_digital_platforms, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(creator_economics_digital_platforms, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creator_economics_digital_platforms, tangled_rope).
narrative_ontology:human_readable(creator_economics_digital_platforms, "Creator Economics on Digital Platforms").
narrative_ontology:topic_domain(creator_economics_digital_platforms, "digital_economy/labor/platform_governance").

domain_priors:requires_active_enforcement(creator_economics_digital_platforms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creator_economics_digital_platforms, platform_operators).
narrative_ontology:constraint_beneficiary(creator_economics_digital_platforms, venture_capital_investors).
narrative_ontology:constraint_victim(creator_economics_digital_platforms, content_creators).
narrative_ontology:constraint_victim(creator_economics_digital_platforms, creator_economic_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL CONTENT CREATOR (SNARE) — Trapped by platform dependency for income distribution and audience access. No alternative exists at comparable scale. High suppression: algorithm opacity, unilateral policy changes, account suspension risk, revenue share fluctuations. Maximum extraction experienced — creator bears all risks while platform captures asymmetric rent.
constraint_indexing:constraint_classification(creator_economics_digital_platforms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER CREATOR NETWORK (TANGLED ROPE) — Constrained by revenue dependency but gaining negotiation power through collective action. Experiences genuine coordination benefit (audience reach, payment infrastructure) alongside extraction (algorithm control, rent capture). Can exit at significant cost (audience loss, revenue disruption) but not zero cost. Moderate suppression and balanced extraction/coordination ratio.
constraint_indexing:constraint_classification(creator_economics_digital_platforms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CREATOR GUILD / PROFESSIONAL ORGANIZATION (ROPE) — Organized actors with arbitrage options (multiplatform distribution, direct-to-fan infrastructure, negotiation leverage). See the constraint as coordination problem solvable through collective standards, revenue floors, and platform accountability. Pure coordination function: enabling better terms through organized pressure. Low extraction experienced by the organization itself.
constraint_indexing:constraint_classification(creator_economics_digital_platforms, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM OPERATOR (SNARE) — From the institutional perspective of the platform itself, the creator-platform relationship is a snare targeting creators. The platform benefits from suppression (algorithm opacity, terms-of-service unilateralism, account control). High extractiveness: platform captures 30-50% of revenue, owns audience data, controls visibility and payment terms. Arbitrage exit available (shift to another platform or model), but platform has zero incentive to exercise it — the constraint is the profit center.
constraint_indexing:constraint_classification(creator_economics_digital_platforms, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY MEDIA / BROADCAST INDUSTRY (PITON) — Traditional media employment (studio contracts, talent management, broadcast revenue sharing) is being displaced by digital platforms. Legacy media structures persist through institutional inertia despite lower functionality — agents still attempt to enforce broadcast-era contracts and revenue models that no longer match creator economics. Theater ratio high (performative agent relationships) while coordination function has atrophied. Exit barriers are cultural and career-path-dependent rather than technical.
constraint_indexing:constraint_classification(creator_economics_digital_platforms, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, digital platforms solve a genuine coordination problem (decentralized content distribution at scale) while simultaneously extracting significant rent through monopolistic control of discovery and monetization mechanisms. The tangled rope classification reflects: (a) genuine coordination achieved (creators reach billions, payments automated), (b) asymmetric extraction embedded (platforms capture 30-50% rent, control algorithm rankings, set policy unilaterally), (c) active enforcement required (terms-of-service, account suspension, payment withholding). The constraint is neither pure coordination nor pure extraction — it is fundamentally hybrid.
constraint_indexing:constraint_classification(creator_economics_digital_platforms, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creator_economics_digital_platforms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(creator_economics_digital_platforms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(creator_economics_digital_platforms, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(creator_economics_digital_platforms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(creator_economics_digital_platforms, TR),
    TR >= 0.70.

:- end_tests(creator_economics_digital_platforms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platforms capture 30-50% revenue share (substantial but not monopolistic rents). Additional extraction mechanisms: algorithm control over visibility, unilateral policy enforcement, data ownership asymmetry, payment term control. But genuine coordination value exists (global audience access, automated payments, infrastructure). The measurement trajectory from 0.35 to 0.58 reflects platforms optimizing extraction mechanisms over time while rhetoric emphasizes creator support (theater). Suppression (0.65): High. Strong barriers to creator exit: (a) network effects — 85% of video creators on YouTube create locked-in audiences; (b) technical barriers — API controls, account policies, payment processing lock-in; (c) information asymmetry — algorithm opacity prevents creators from understanding visibility mechanics; (d) policy unilateralism — creators cannot negotiate terms; (e) revenue unpredictability — algorithm changes alter earnings without notice. Theater ratio (0.68): High. Platforms perform creator support (grants programs, creator funds, support documentation) while core extraction mechanisms remain opaque. Algorithm ranking criteria claimed to be merit-based but function as algorithmic rent extraction. Creator 'partnership' language masks asymmetric power relationships.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates fundamental perspectival divergence. Individual creators experience Snare: trapped by network effects, revenue dependency, and algorithm opacity, with no viable exit options. Creator networks experience Rope: organized collective bargaining and alternative infrastructure (direct-to-fan, multiplatform distribution) reduce extraction and increase agency. Platform operators see their side as profitable extraction (they classify it as Snare aimed at creators) but claim to provide Rope (coordination). Legacy media structures persist as Piton: broadcast-era talent contracts and agent relationships degrade as creators bypass traditional gatekeeping, but institutional inertia maintains the structures. The analytical observer sees Tangled Rope: the constraint simultaneously solves coordination (global content distribution) and extracts asymmetrically (platform rent). No single perspective is 'correct' — the constraint is genuinely hybrid and experienced differently based on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) map directly to structural power and exit capacity relative to the constraint. Platform operators and venture investors are beneficiaries with arbitrage options (can shift business models, exit to other sectors) → low d → negative effective extraction experienced. Individual creators are victims with trapped exit (no alternatives at comparable scale) → high d → maximum effective extraction experienced. Mid-tier networks are victims with constrained exit (can diversify multiplatform but at efficiency loss) → moderate-high d → high extraction. Creator organizations have arbitrage options (multiplatform leverage, direct-to-fan infrastructure, negotiation power) → low-moderate d → moderate extraction. The sigmoid f(d) computes experienced extractiveness from base ε, power level, and exit capacity. Trapped victims with no institutional power experience χ at maximum; organized actors with exit options experience lower χ despite same base ε.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint is classified Tangled Rope because it satisfies all three requirements: (1) COORDINATION FUNCTION: Platforms solve distribution bottleneck — creators reach global audiences without traditional gatekeeping (genuine), (2) ASYMMETRIC EXTRACTION: Platforms capture 30-50% revenue share, control algorithm visibility, set policy unilaterally (genuine), (3) ACTIVE ENFORCEMENT: Terms-of-service enforcement, account suspension policy, payment withholding mechanisms (genuine). The mandatrophy would be resolved by claiming this is pure Rope (coordination without extraction) — but base_extractiveness = 0.58 refutes this. It would be resolved by claiming pure Snare (extraction without coordination) — but the genuine audience reach value refutes this. Tangled Rope is the precise classification: coordination and extraction are structurally inseparable because the platform's monopoly power on discovery (coordination function) IS the mechanism of rent extraction. Removing the algorithm control would remove both the discovery advantage AND the extraction mechanism. The theater ratio (0.68) indicates that platform rhetoric emphasizes creator support and opportunity, performing coordination while suppressing visibility of extraction mechanisms (algorithm opacity, policy unilateralism, data ownership).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creator_dependency_threshold,
    'What percentage of a creator''s total income derived from a single platform constitutes structural dependency vs manageable diversification risk?',
    'Survey data on creator income distribution; correlation between platform concentration and creator exit costs; longitudinal tracking of creator portfolio diversification',
    'If threshold < 40%: most creators are diversified and experience constraint as Rope (coordination). If threshold > 70%: most creators are trapped and experience constraint as Snare. Current evidence suggests 60-80% platform-dependent, supporting Snare classification for median creator.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(creator_dependency_threshold, empirical, 'Income dependency threshold determining structural trap vs manageable risk').

omega_variable(
    algorithm_opacity_mechanism,
    'Is algorithm opacity (suppression mechanism) structural necessity for platform operation or deliberate asymmetric information control?',
    'Comparative analysis of transparency claims vs actual disclosure depth; audit studies measuring impact of algorithm changes on creator revenue; comparison with open-source algorithm transparency models',
    'If structural necessity: suppression reflects legitimate coordination cost, reducing effective extraction. If deliberate control: suppression is pure asymmetric enforcement mechanism, increasing extraction classification. Evidence strongly suggests deliberate control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithm_opacity_mechanism, empirical, 'Whether algorithm opacity serves function or enables extraction').

omega_variable(
    alternative_platform_viability,
    'Do alternative platforms (Patreon, YouTube, TikTok, Rumble, BeReal) provide genuine network effects parity or merely niche alternatives?',
    'Cross-platform audience reach analysis; creator switching costs measurement; network effects saturation analysis; emergence of interoperable creator infrastructure',
    'If parity exists: creators have genuine arbitrage options and constraint shifts toward Rope. If alternatives remain niche: trapped classification stands. Current: YouTube 85% video dominance, TikTok 60% growth, creator arbitrage emerging but incomplete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether alternative platforms provide genuine exit options').

omega_variable(
    revenue_floor_sustainability,
    'Can direct-to-fan revenue infrastructure (Patreon, memberships, merchandise, sponsorships) substitute for platform revenue at comparable scale and stability?',
    'Financial modeling of creator income stability; case studies of successful platform independence; measurement of churn rates in direct relationships vs platform-mediated relationships',
    'If viable substitute: scaffold perspective confirmed — creator infrastructure sunset is real pathway. If unstable: direct-to-fan infrastructure remains niche, platform dependency persists. Evidence shows 10-30% of creators achieve sustainability without platform revenue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revenue_floor_sustainability, empirical, 'Whether direct-to-fan infrastructure provides viable alternative').

omega_variable(
    rent_capture_measurement,
    'What is the true economic rent captured by platforms vs legitimate coordination cost (payment processing, infrastructure, audience access)?',
    'Comparative cost analysis: platform infrastructure cost vs revenue retained; benchmarking against historical publishing/broadcast rent rates; measurement of platform profit margins vs necessary operating expenses',
    'If rent > 40%: extraction is clearly asymmetric (Snare/Tangled Rope). If rent < 15%: most retained value represents legitimate coordination cost (Rope). Current: platforms retain 30-50%, suggesting ε ≈ 0.45-0.60, supporting Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rent_capture_measurement, empirical, 'Decomposition of platform revenue into coordination cost vs extractive rent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creator_economics_digital_platforms, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(creator_tr_t0, creator_economics_digital_platforms, theater_ratio, 0, 0.52).
narrative_ontology:measurement(creator_tr_t5, creator_economics_digital_platforms, theater_ratio, 5, 0.62).
narrative_ontology:measurement(creator_tr_t10, creator_economics_digital_platforms, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(creator_be_t0, creator_economics_digital_platforms, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(creator_be_t5, creator_economics_digital_platforms, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(creator_be_t10, creator_economics_digital_platforms, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creator_economics_digital_platforms, resource_allocation).
narrative_ontology:boltzmann_floor_override(creator_economics_digital_platforms, 0.2).
narrative_ontology:affects_constraint(creator_economics_digital_platforms, attention_economy_competition).
narrative_ontology:affects_constraint(creator_economics_digital_platforms, algorithmic_recommendation_systems).
narrative_ontology:affects_constraint(creator_economics_digital_platforms, digital_labor_commodification).

% DUAL FORMULATION NOTE:
% Creator economics decomposes into three structurally distinct constraints: (1) resource_allocation_coordination (ε ≈ 0.20, primarily Rope) — platforms solve distribution logistics and payment infrastructure, (2) algorithmic_visibility_control (ε ≈ 0.65, primarily Snare) — platform algorithm control over creator visibility and audience reach, (3) labor_commodification (ε ≈ 0.50, primarily Tangled Rope) — platform conversion of creator labor into scalable revenue streams. The unified 'creator economics' story (ε = 0.58) represents the entanglement of all three. Upstream constraints: attention_economy_competition (ε ≈ 0.72, Snare) — creators compete in winner-take-all attention markets enabled by platforms. Downstream constraints: digital_labor_commodification (ε ≈ 0.55, Tangled Rope) — labor becomes platform-mediated service provision.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(creator_economics_digital_platforms, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
