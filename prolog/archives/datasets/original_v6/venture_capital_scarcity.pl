% ============================================================================
% CONSTRAINT STORY: venture_capital_scarcity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_venture_capital_scarcity, []).

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
 *   constraint_id: venture_capital_scarcity
 *   human_readable: Venture Capital Scarcity and Founder Extraction
 *   domain: economic/finance
 *
 * SUMMARY:
 *   Venture capital scarcity creates a structural extraction mechanism where
 *   capital-seeking founders occupy a powerless position facing institutional
 *   investors with concentrated capital and information asymmetry. The
 *   constraint exhibits hybrid coordination (matching capital with
 *   opportunity) and extraction (asymmetric term sheets, governance control,
 *   dilution, and founder network gatekeeping). The scarcity is partly
 *   structural — early-stage ventures are genuinely risky and capital must be
 *   concentrated to evaluate them — but also partly manufactured through
 *   founder mythology (the VC-funded startup as the only legitimate scaling
 *   path), geographic concentration (capital pools in a few tech hubs),
 *   demographic gatekeeping (network access limited by founder background),
 *   and piton-like inertia (startup dream persists despite most successful
 *   companies being bootstrapped). Theater ratio has increased over the
 *   measurement interval as founder energy increasingly dedicates to investor
 *   relations, pitching, and narrative performance rather than product
 *   development. Extractiveness has risen as later-stage VC funding becomes
 *   essential for continuation (first-mover advantage in network creates path
 *   dependence), and alternative funding sources remain nascent.
 *
 * KEY AGENTS:
 *   - Capital-Seeking Founders: Primary victim (powerless/trapped) — must accept non-negotiable terms to access growth capital; face full extraction through dilution, control loss, and governance subordination
 *   - Institutional Investors: Primary beneficiary (institutional/arbitrage) — allocate capital across portfolio with high upside optionality; arbitrage across opportunities minimizes exposure to individual failure
 *   - Excluded Demographics: Secondary victim (moderate/constrained) — women, people of color, and non-traditional founders face network gatekeeping and implicit bias; constrained rather than trapped due to emerging alternative funding sources
 *   - Regional Economic Development Coalitions: Organized agents (organized/constrained) — government-backed venture funds, accelerators, and community development finance building alternative capital pathways with sunset logic
 *   - Startup Dream Mythology: Institutional inertia (institutional/arbitrage) — persistent cultural narrative that VC funding is the primary/legitimate path to scale, even as most successful companies are bootstrapped; maintained through aspirational reinforcement
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent capital concentration as inherent feature of risk evaluation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(venture_capital_scarcity, 0.58).
domain_priors:suppression_score(venture_capital_scarcity, 0.62).
domain_priors:theater_ratio(venture_capital_scarcity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(venture_capital_scarcity, extractiveness, 0.58).
narrative_ontology:constraint_metric(venture_capital_scarcity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(venture_capital_scarcity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(venture_capital_scarcity, tangled_rope).
narrative_ontology:human_readable(venture_capital_scarcity, "Venture Capital Scarcity and Founder Extraction").
narrative_ontology:topic_domain(venture_capital_scarcity, "economic/finance").

domain_priors:requires_active_enforcement(venture_capital_scarcity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(venture_capital_scarcity, institutional_investors).
narrative_ontology:constraint_beneficiary(venture_capital_scarcity, venture_capitalists).
narrative_ontology:constraint_victim(venture_capital_scarcity, early_stage_founders).
narrative_ontology:constraint_victim(venture_capital_scarcity, excluded_demographics).
narrative_ontology:constraint_victim(venture_capital_scarcity, regional_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAPITAL-SEEKING FOUNDER (SNARE) — Trapped in asymmetric negotiation. Without VC funding, cannot scale. VC terms are non-negotiable: dilution, control loss, governance rights, follow-on round pressure. Founder bears full extraction — legal structures (preferred stock, board seats, liquidation preferences) institutionalize the power asymmetry. High suppression through information asymmetry and BATNA destruction (alternative funding sources extremely limited).
constraint_indexing:constraint_classification(venture_capital_scarcity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXCLUDED DEMOGRAPHIC (TANGLED ROPE) — Women, people of color, and non-traditional founders have access to venture capital through mentorship networks and diversity commitments, but face structural barriers (network scarcity, implicit bias in pitch evaluation, lower fund availability). Genuine coordination exists (funding mechanisms, governance frameworks), but asymmetric extraction occurs through disadvantageous terms and network gatekeeping. Constrained exit: alternative funding (bootstrapping, friends/family) available but resource-limited.
constraint_indexing:constraint_classification(venture_capital_scarcity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL INVESTOR (ROPE) — Experiences VC as coordination mechanism. Capital allocation to promising ventures solves the matching problem: identifying founders with viable ideas and providing growth capital. Low perceived extraction — the ecosystem functions as intended from their vantage. Arbitrage exit: can invest across multiple opportunities and geographies; if one fails, capital flows to next bet.
constraint_indexing:constraint_classification(venture_capital_scarcity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL DEVELOPMENT COALITION (SCAFFOLD) — Regional development agencies, venture studios, and community development finance view scarcity-driven consolidation as a temporary coordination failure being addressed through sunset-constrained mechanisms: government-backed venture funds, accelerators, microfinance, and equity crowdfunding. These mechanisms build alternative capital pipelines with explicit sunset logic — as startup ecosystems mature, dependence on centralized VC reduces. Theater ratio moderate: regional funds have stronger local governance and transparency than mega-funds.
constraint_indexing:constraint_classification(venture_capital_scarcity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: STARTUP DREAM NARRATIVE (PITON) — The mythology of the VC-funded startup as the primary path to scale has become largely performative. In reality, most successful companies are bootstrapped or funded through non-VC mechanisms (bank loans, corporate backing, retained earnings). The VC narrative persists through aspirational reinforcement and media visibility, not because it represents the dominant funding pathway for economic growth. Theater ratio high: much founder energy goes to pitching and investor relations rather than product development. The constraint is maintained through institutional inertia and founder internalization of the myth.
constraint_indexing:constraint_classification(venture_capital_scarcity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, capital scarcity may appear as an immutable feature of finance: information asymmetry between capital holders and entrepreneurs creates a natural power gradient; risk concentration requires centralization of capital. This perspective risks naturalizing what is actually a contingent institutional arrangement (limited transparency, restricted founder networks, concentrated fund size, institutional preference for specific founder archetypes). The engine will flag this as a false summit.
constraint_indexing:constraint_classification(venture_capital_scarcity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(venture_capital_scarcity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(venture_capital_scarcity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(venture_capital_scarcity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(venture_capital_scarcity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(venture_capital_scarcity, TR),
    TR >= 0.70.

:- end_tests(venture_capital_scarcity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Founders lose significant equity (30-60% dilution across rounds), control (investor board seats, veto rights on major decisions), and upside (preference stacks, liquidation preferences). The extraction is real and substantial. However, extractiveness is not higher (0.70+) because genuine coordination exists: founders do get capital they could not otherwise access, and the VC mechanism does create incentive alignment around growth. Measurement interval shows rising extractiveness as later-stage rounds become essential for continuation and founder BATNA weakens. Suppression (0.62): High. Barriers to exit include: specialized capital requirements for scaling, information asymmetries in term sheet evaluation, founder cognitive capture by startup mythology, geographic concentration limiting available funds, network gatekeeping by demographic, limited transparency in fund selection criteria, social pressure and status competition around VC backing. Suppression is not total (bootstrap and alternative funding exist) but sufficient to make capital-seeking founders captive. Theater ratio (0.58): Moderate-high and rising. Pitching, investor relations, narrative crafting, and metrics theater consume significant founder bandwidth. Particularly high in later rounds where founder focus shifts from product development to capital raise. Theater ratio is lower than pure piton (the VC mechanism still drives real capital allocation and selection) but meaningfully higher than pure rope (much activity is performative rather than functional).
 *
 * PERSPECTIVAL GAP:
 *   The scarcity constraint illustrates how institutional design creates appearance of natural limits. Founders see scarcity as absolute (snare) because they lack access to deep capital pools and lack knowledge of alternative funding mechanisms. Investors see abundant opportunity (rope) because they have diversified capital sources. Regional coalitions see temporary scarcity with sunset solutions (scaffold). The mythology perspective (piton) shows how founder belief in VC as the only legitimate path maintains the constraint despite evidence that most successful companies use other funding sources. The analytical observer risks treating capital concentration as inherent to entrepreneurship rather than recognizing it as institutional design choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options. Institutional investors with arbitrage exit (can rebalance across portfolio) derive low d values — they are capital holders with optionality. Founders with trapped exit and victim status derive high d values — they are capital seekers with no alternatives. Excluded demographics with constrained exit (alternatives exist but costly) derive moderate d values. Organized regional agents with constrained exit but coordination function derive moderate-low d values. The piton classification derives from high theater ratio (0.58+) rather than from high experienced extraction — the constraint persists through narrative maintenance more than through structural enforcement. The mountain classification at analytical scope is flagged as false summit by the schema — the supposed natural law (risk concentration requires capital pooling) is actually contingent on institutional design (current fund size, founder sourcing mechanisms, network gatekeeping, mythology maintenance).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that scarcity is a hybrid constraint: some components are genuinely structural (early-stage ventures are informationally opaque and risky, requiring concentrated evaluation), others are institutionally manufactured (concentrated fund size, geographic clustering, founder mythology, network gatekeeping by demographic). The tangled_rope classification captures this: genuine coordination exists (capital allocation to promising ventures) alongside asymmetric extraction (founder dilution, control loss, governance subordination). The measurement interval shows rising extractiveness and theater as later-stage VC becomes essential rather than optional, suggesting the manufactured component is strengthening relative to the structural component. Resolution would require breaking the cycle at multiple points: increasing alternative funding availability (maturation of crowdfunding, revenue-based financing, government backing), reducing founder mythology through data on non-VC scaling pathways, dispersing capital geographically, and explicitly addressing network gatekeeping by demographic. The constraint is not immutable (mountain) but neither is it easily coordinated away (rope) — it requires active enforcement of asymmetric terms, making tangled_rope the accurate classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_scarcity_structural_vs_narrative,
    'Is venture capital scarcity a structural feature of growth-stage financing or a manufactured constraint created by institutional gatekeeping and founder mythology?',
    'Longitudinal analysis of capital availability across funding rounds, demographics, and geographies; comparison of effective capital costs between VC-funded and non-VC-funded scaling pathways; measurement of founder network composition and access barriers',
    'If structural: scarcity is inherent to risk evaluation and capital formation — extraction reflects legitimate information asymmetry pricing. If manufactured: scarcity is contingent on institutional design — extraction is rent-seeking layered onto coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_scarcity_structural_vs_narrative, empirical, 'Whether capital scarcity is structural or institutionally manufactured').

omega_variable(
    founder_identity_lock_mechanism,
    'Do founders remain committed to VC funding despite extractive terms due to genuine rationality (VC maximizes expected value) or due to identity fusion with the startup dream mythology?',
    'Qualitative analysis of founder exit decision-making; comparison of outcomes between bootstrapped and VC-funded founders with similar initial conditions; measurement of founder persistence in seeking VC despite failed pitches and extractive term sheets',
    'If rational: founders choose VC because it maximizes expected value for their specific context — classification as constrained rather than identity_locked. If identity-fused: founders pursue VC despite negative expected value to maintain the founder identity — classification shifts to identity_locked, revealing cognitive capture mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_identity_lock_mechanism, conceptual, 'Whether founder commitment to VC is rational or identity-locked').

omega_variable(
    network_gatekeeping_enforceability,
    'What enforces the exclusion of non-traditional founders from VC networks — explicit institutional policy, implicit bias, or structural information asymmetries that make network entry costly?',
    'Analysis of founder sourcing patterns; study of pitch acceptance rates by demographic cohort controlling for business fundamentals; measurement of information access differentials (who knows which investors, which funds are open to which founder types)',
    'If explicit policy: suppression is high and removable through antidiscrimination enforcement. If implicit: suppression is high but harder to target. If structural: suppression is lower in principle but harder to address without reorganizing the entire network.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_gatekeeping_enforceability, empirical, 'Whether network gatekeeping is explicit, implicit, or structural').

omega_variable(
    alternative_funding_pathway_maturity,
    'Are alternative capital sources (crowdfunding, bank loans, government-backed venture funds, corporate backing, revenue-based financing) sufficiently mature to constitute genuine exits from VC dependence?',
    'Comparative analysis of capital availability, founder outcomes, and scaling success rates across funding mechanisms; measurement of founder satisfaction and autonomy across pathways',
    'If mature: scaffold perspective confirmed — founders have real alternative pathways and can exit VC constraint. Suppression lowers, exit options shift from trapped to constrained. If immature: alternative pathways are performative, founders remain trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_pathway_maturity, empirical, 'Whether alternative funding sources constitute genuine alternatives').

omega_variable(
    regional_capital_concentration_trend,
    'Is venture capital consolidating geographically (more capital concentrating in Silicon Valley, major tech hubs) or distributing toward regional ecosystems?',
    'Longitudinal measurement of capital distribution by region; analysis of fund formation patterns; tracking of founder success rates across geographies',
    'If consolidating: regional founders'' suppression increases, constrained exit becomes trapped, regional scaffold expires. If distributing: alternative pathways mature, scaffold perspectives strengthen, suppression decreases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_capital_concentration_trend, empirical, 'Trend in geographical capital concentration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(venture_capital_scarcity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vcscr_tr_t0, venture_capital_scarcity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vcscr_tr_t5, venture_capital_scarcity, theater_ratio, 5, 0.48).
narrative_ontology:measurement(vcscr_tr_t10, venture_capital_scarcity, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(vcscr_be_t0, venture_capital_scarcity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vcscr_be_t5, venture_capital_scarcity, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(vcscr_be_t10, venture_capital_scarcity, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(venture_capital_scarcity, resource_allocation).
narrative_ontology:boltzmann_floor_override(venture_capital_scarcity, 0.18).
narrative_ontology:affects_constraint(venture_capital_scarcity, founder_incentive_misalignment).
narrative_ontology:affects_constraint(venture_capital_scarcity, geographic_startup_inequality).
narrative_ontology:affects_constraint(venture_capital_scarcity, demographic_founder_gatekeeping).

% DUAL FORMULATION NOTE:
% VC scarcity is downstream of capital structure constraints and upstream of specific founder outcomes. Decomposed into separate constraints: (1) scarcity as structural feature of risk evaluation (lower ε), (2) scarcity as manufactured through founder mythology and geographic concentration (higher ε). This story addresses the hybrid tangled_rope classification; decomposition would split into structural rope and manufactured snare components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(venture_capital_scarcity, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
