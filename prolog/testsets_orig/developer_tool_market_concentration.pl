% ============================================================================
% CONSTRAINT STORY: developer_tool_market_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_developer_tool_market_concentration, []).

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
 *   constraint_id: developer_tool_market_concentration
 *   human_readable: Developer Tool Market Concentration and Lock-in
 *   domain: software_economics/platform_dynamics
 *
 * SUMMARY:
 *   Developer tool markets exhibit increasing concentration around dominant
 *   platform vendors (cloud infrastructure providers, IDE ecosystems, version
 *   control platforms). This constraint manifests as a hybrid
 *   coordination-extraction mechanism: platforms provide genuine value
 *   through integration, distribution, and ecosystem support, while
 *   simultaneously extracting rents through lock-in, pricing control, and API
 *   gatekeeping. The structural feature is that independent tool developers
 *   cannot realistically reach their market without platform mediation, yet
 *   platform policies create asymmetric extraction on those who depend on
 *   them. The theater ratio is low (0.48) because the coordination function
 *   is genuinely needed — developers do benefit from integrated toolchains
 *   and distribution channels. But extractiveness is increasing (0.32 → 0.58
 *   over the interval) as platforms consolidate market power and convert
 *   coordination access into rent extraction.
 *
 * KEY AGENTS:
 *   - Independent Tool Developers: Primary victim (powerless/trapped) — cannot reach market without platform access; face career lock-in to platform success
 *   - Mid-Tier Tool Vendors: Secondary victim (moderate/constrained) — benefit from platform APIs and distribution but face systematic extraction through pricing, algorithmic suppression, and policy changes
 *   - Dominant Platform Vendors: Primary beneficiary (institutional/arbitrage) — orchestrate ecosystem; capture rents through integration tax and can exit by building competing tools
 *   - Open-Source Coalition: Organized agents (organized/constrained) — building alternative toolchains with sunset logic; constrained by resource and network effect disadvantages but have real exit pathway
 *   - Legacy Enterprise Customers: Institutional actor (institutional/arbitrage) — locked into platform integrations but can technically exit; inertia maintains constraint more than economic necessity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent architectural and policy choices as inherent properties of platform markets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(developer_tool_market_concentration, 0.58).
domain_priors:suppression_score(developer_tool_market_concentration, 0.62).
domain_priors:theater_ratio(developer_tool_market_concentration, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(developer_tool_market_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(developer_tool_market_concentration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(developer_tool_market_concentration, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(developer_tool_market_concentration, tangled_rope).
narrative_ontology:human_readable(developer_tool_market_concentration, "Developer Tool Market Concentration and Lock-in").
narrative_ontology:topic_domain(developer_tool_market_concentration, "software_economics/platform_dynamics").

domain_priors:requires_active_enforcement(developer_tool_market_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(developer_tool_market_concentration, dominant_platform_vendors).
narrative_ontology:constraint_beneficiary(developer_tool_market_concentration, early_adopter_ecosystem_participants).
narrative_ontology:constraint_victim(developer_tool_market_concentration, independent_tool_developers).
narrative_ontology:constraint_victim(developer_tool_market_concentration, developer_workflow_autonomy).
narrative_ontology:constraint_victim(developer_tool_market_concentration, emerging_alternative_vendors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT TOOL DEVELOPER (SNARE) — Individual and small-team tool creators face insurmountable barriers to distribution, discovery, and monetization outside dominant platforms. Network effects, integrations, and installed base lock developers into ecosystem dependency. Career trajectory locked to platform success; cannot exit without abandoning prior work and reputation investment. Maximum extraction experienced.
constraint_indexing:constraint_classification(developer_tool_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER TOOL VENDOR (TANGLED ROPE) — Moderately-sized vendors benefit from platform distribution and API access (genuine coordination) while facing asymmetric extraction through pricing pressure, API changes, algorithmic suppression, and ecosystem policies. Can technically exit but at significant cost — customer base tied to platform integration. Both coordination function and extraction present simultaneously.
constraint_indexing:constraint_classification(developer_tool_market_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT PLATFORM VENDOR (ROPE) — Primary beneficiary experiencing the constraint as coordination mechanism. Orchestrates ecosystem through APIs, app stores, and developer relations. Arbitrage options (can license, bundle, or create competing tools) insulate from extraction. Net benefit flow is toward this actor; constraint existence serves their coordination needs.
constraint_indexing:constraint_classification(developer_tool_market_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-SOURCE ALTERNATIVE COALITION (SCAFFOLD) — Organized developers (Linux Foundation, open-source projects, community-funded tools) view platform concentration as a solvable coordination problem with clear sunset logic. Build alternative toolchains, package managers, and integrated development environments that bypass proprietary platforms. Low experienced extraction because coalition has genuine agency and growing alternatives; suppression declining as open-source tooling matures.
constraint_indexing:constraint_classification(developer_tool_market_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY INTEGRATION NETWORK (PITON) — Thousands of enterprise integrations and plugin ecosystems built on platform APIs represent inertial mass: existing customer workflows depend on these integrations, but the integrations themselves are largely maintained out of obligation rather than active functionality. Platform vendors retain high theater in maintaining backward compatibility and integration APIs despite their degraded strategic importance as cloud-native alternatives gain adoption. Institutional inertia sustains the constraint beyond its functional necessity.
constraint_indexing:constraint_classification(developer_tool_market_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, network effects and platform switching costs appear as natural laws of software markets: developer tool consolidation seems inherent to how software markets evolve. Once a platform reaches critical mass, integration effects become mathematically self-reinforcing. However, this perspective naturalizes what are actually contingent policy choices (API restrictions, app store gatekeeping, platform licensing terms) and contingent technical architectures (closed vs open APIs). The engine's false summit detector will flag this as misclassification.
constraint_indexing:constraint_classification(developer_tool_market_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(developer_tool_market_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(developer_tool_market_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(developer_tool_market_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(developer_tool_market_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(developer_tool_market_concentration, TR),
    TR >= 0.70.

:- end_tests(developer_tool_market_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The primary extraction mechanism is not price gouging but lock-in: developers invest in platform-specific skills, build customer bases dependent on platform integration, and face switching costs that increase over time. The constraint's extraction value has increased from 0.32 to 0.58 over the interval as platforms have matured their lock-in mechanisms (API proprietary features, ecosystem integration depth, algorithmic discoverability control). Suppression (0.62): Moderate-high. Independent developers face substantial barriers to market access: distribution requires platform app stores, discovery depends on platform algorithms, monetization requires platform payment infrastructure. These barriers are not physical but are enforced through technical architecture and policy. Theater ratio (0.48): Moderate-low. The constraint has a genuine coordination function — integrated toolchains do provide real value. The theater component comes from performative ecosystem governance (developer relations, community theater, open-source claims) that partially masks the extraction mechanism. Theater has declined slightly as platforms become more explicit about monetization and less concerned with community image. Suppression is not scaled by power or scope; it is a raw structural property (0.62) that applies uniformly across agents regardless of their context.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of classification types. Dominant platforms see a coordination mechanism (Rope) — they are solving the genuine problem of ecosystem integration. Independent developers see pure extraction (Snare) — they are locked in with no exit and maximum asymmetry. Mid-tier vendors see mixed coordination and extraction (Tangled Rope) — they both benefit from integration and suffer from extraction. The open-source coalition sees a temporary problem with a sunset (Scaffold) — alternative toolchains are building competitive feature parity over a generational timescale. Legacy integrations see an inertial constraint (Piton) — the constraint persists through institutional momentum rather than active function. The civilizational analytical observer risks seeing natural law (Mountain) — network effects as mathematical inevitability — but the structural data reveals this as naturalization of policy choices (API restrictions, app store gatekeeping, licensing terms).
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant platform vendors derive d ≈ 0.05 (beneficiary + arbitrage exit): they set platform policies, can create competing tools, and experience coordination benefits. The sigmoid f(d) produces negative f(d) ≈ -0.12, which reduces their experienced chi. Independent developers derive d ≈ 0.95 (victim + trapped exit): they depend on platform access, have no alternative distribution channels with comparable reach, and lack arbitrage options. The sigmoid f(d) produces high f(d) ≈ 1.42, which amplifies their experienced chi. Mid-tier vendors occupy d ≈ 0.55 (victim + constrained exit): they can theoretically exit to open-source or alternative platforms but at significant cost. Scope modulation (σ(S) = 1.2 for global) scales all χ values upward: global platforms create globally-locked developers. The open-source coalition experiences low chi (scaffold perspective) because their constrained exit is paired with genuine agency — they are building alternatives, not awaiting rescue.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that extractiveness (0.58) places the constraint above the rope threshold but the theater ratio (0.48) is too low for pure snare classification. The constraint genuinely coordinates ecosystem value — it is not pure extraction. But the asymmetry is real and growing: beneficiaries (platform vendors) experience the constraint as low-cost coordination, while victims (independent developers) experience maximum extraction despite genuine coordination benefits. The tangled_rope classification captures this hybrid: platforms provide coordination value that independent developers genuinely need, while simultaneously extracting rents through lock-in. The false summit at the analytical level (mountain perspective) reveals that naturalizing this as 'inherent to networks' obscures the policy and architectural choices that maintain the lock-in. If platforms adopted truly open APIs, interoperable standards, and neutral algorithmic discovery, extractiveness would fall sharply despite preserved network effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effects_vs_policy_lock,
    'What proportion of the observed market concentration derives from genuine network effects vs. policy-enforced lock-in mechanisms (API restrictions, licensing terms, algorithmic suppression)?',
    'Comparative analysis of market structure across different regulatory regimes (EU vs US vs China); measurement of churn rates when alternatives offer feature parity; natural experiments where platforms remove lock-in mechanisms',
    'If network effects dominate: constraint approaches mountain classification (inherent property of platform markets). If policies dominate: constraint is snare/tangled_rope (designed extraction mechanism). Ratio determines authenticity of ''natural law'' framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_vs_policy_lock, empirical, 'Network effects vs policy-enforced lock-in proportion').

omega_variable(
    open_source_competitive_threshold,
    'At what feature parity level do open-source alternatives become competitive for mainstream developer workflows, and are we approaching or past this threshold?',
    'Developer survey data on feature sufficiency; market share trends for open-source tooling; adoption rates in greenfield projects; correlation between open-source maturity and developer churn from proprietary platforms',
    'If threshold is <2 years away: scaffold perspective is accurate — sunset is structural. If threshold is >10 years away: scaffold is aspirational, and independent developers remain trapped in proprietary ecosystems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_competitive_threshold, empirical, 'Open-source tool competitive threshold timeline').

omega_variable(
    api_stability_as_extraction_signal,
    'Does platform API stability correlate with extortion-like behavior (breaking changes that force upgrades or vendor lock-in) vs genuine ecosystem maintenance?',
    'Historical analysis of API deprecation patterns; measurement of vendor costs for API changes; correlation between API stability claims and actual breaking changes; developer sentiment data on API reliability',
    'If APIs are unstable: clear signal of deliberate extraction (developers forced to continuously re-engineer). If APIs are stable: suggests genuine coordination. Stability pattern is key to distinguishing rope from snare from beneficiary perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(api_stability_as_extraction_signal, empirical, 'API stability as extraction mechanism signal').

omega_variable(
    alternative_ecosystem_viability,
    'Can developers realistically build and monetize tools within alternative ecosystems (open-source, decentralized, niche platforms) with comparable revenue potential to proprietary platform ecosystems?',
    'Economic analysis of developer income by platform; venture capital funding availability for ecosystem-specific startups; profitability data for tools in alternative ecosystems; career mobility data (can developers transition from proprietary to alternative ecosystems)',
    'If viable: independent tool developers have genuine exit options; classification shifts toward constrained rather than trapped. If not viable: lock-in is economic rather than technical; snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_ecosystem_viability, empirical, 'Alternative ecosystem economic viability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(developer_tool_market_concentration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(devtool_tr_t0, developer_tool_market_concentration, theater_ratio, 0, 0.35).
narrative_ontology:measurement(devtool_tr_t5, developer_tool_market_concentration, theater_ratio, 5, 0.42).
narrative_ontology:measurement(devtool_tr_t10, developer_tool_market_concentration, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(devtool_be_t0, developer_tool_market_concentration, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(devtool_be_t5, developer_tool_market_concentration, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(devtool_be_t10, developer_tool_market_concentration, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(developer_tool_market_concentration, global_infrastructure).
narrative_ontology:affects_constraint(developer_tool_market_concentration, cloud_infrastructure_vendor_lock_in).
narrative_ontology:affects_constraint(developer_tool_market_concentration, open_source_sustainability).
narrative_ontology:affects_constraint(developer_tool_market_concentration, app_store_gatekeeping).

% DUAL FORMULATION NOTE:
% Developer tool market concentration should be decomposed into: (1) technical network effects (genuine coordination to prevent ecosystem fragmentation), which classify as rope; (2) policy-enforced lock-in (API restrictions, app store gatekeeping, licensing terms), which classify as snare; (3) ecosystem integrations (middleware dependencies), which classify as tangled_rope. Current story combines all three under tangled_rope because they are structurally coupled — policy lock-in prevents network effects from equilibrating, and integration depth amplifies lock-in effects. Upstream constraints: cloud infrastructure lock-in (affects platform vendor lock-in capacity) and app store gatekeeping (directly implements lock-in mechanism). Downstream constraint: open-source sustainability (absorbs developer switching when proprietary platforms over-extract).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(developer_tool_market_concentration, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
