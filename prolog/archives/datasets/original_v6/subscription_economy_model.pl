% ============================================================================
% CONSTRAINT STORY: subscription_economy_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model, []).

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
 *   constraint_id: subscription_economy_model
 *   human_readable: The Subscription Economy Model
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The subscription economy represents a structural shift from permanent
 *   product ownership to continuous access licensing. Over the past 15 years,
 *   software vendors have systematically migrated from perpetual licenses
 *   (buy once, use forever) to recurring subscription models, with similar
 *   patterns emerging in hardware (Adobe Creative Suite → Creative Cloud),
 *   productivity tools (Microsoft Office → Office 365), entertainment
 *   (Netflix, Spotify), and infrastructure (AWS, Azure). This constraint
 *   exhibits genuine coordination benefits (sustainable funding for feature
 *   development, automatic updates, cloud infrastructure) alongside
 *   significant extraction mechanisms (rising costs without ownership, forced
 *   obsolescence of older versions, data lock-in). Individual consumers
 *   experience this primarily as trap; platform operators experience it as
 *   legitimate coordination; open-source coalitions see a sunset trajectory;
 *   large enterprises retain negotiating power that reduces snare severity.
 *   The theater ratio (0.65) reflects that vendor marketing ('access instead
 *   of ownership', 'continuous innovation') frames extraction as benefit,
 *   with genuine benefits underneath the rhetoric. Extractiveness has risen
 *   from 0.28 (when subscription was marginal, 2014-ish) to 0.52 (current,
 *   2024-ish) as subscriptions became dominant and prices accumulated across
 *   toolsets.
 *
 * KEY AGENTS:
 *   - Individual Consumers: Primary victims (powerless/trapped) — no ownership, perpetual billing, high switching costs
 *   - Small Business Users: Secondary victims (moderate/constrained) — depend on platform ecosystems; face rising costs but have some exit options
 *   - Software Platform Operators: Primary beneficiaries (institutional/arbitrage) — recurring revenue enables sustainable development; capture benefit stream
 *   - Open Source Coalition: Organized alternative (organized/constrained) — Linux, LibreOffice, Blender, Nextcloud providing functional alternatives with sunset logic
 *   - Enterprise Customers: Negotiators (powerful/mobile) — have bargaining power, can maintain legacy systems or self-host alternatives
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional choice as technological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model, 0.52).
domain_priors:suppression_score(subscription_economy_model, 0.58).
domain_priors:theater_ratio(subscription_economy_model, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model, extractiveness, 0.52).
narrative_ontology:constraint_metric(subscription_economy_model, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(subscription_economy_model, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model, "The Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model, software_platform_operators).
narrative_ontology:constraint_beneficiary(subscription_economy_model, digital_service_providers).
narrative_ontology:constraint_victim(subscription_economy_model, individual_consumers).
narrative_ontology:constraint_victim(subscription_economy_model, small_business_users).
narrative_ontology:constraint_victim(subscription_economy_model, consumer_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL CONSUMER (SNARE) — Trapped in perpetual payment cycle. No ownership of software; continuous billing prevents exit. Suppressed alternatives: perpetual licenses discontinued, cracked/open-source variants have high switching costs. Maximum experienced extraction.
constraint_indexing:constraint_classification(subscription_economy_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL BUSINESS USER (TANGLED ROPE) — Partially constrained by dependency on platform ecosystem; coordination benefits (cloud sync, automatic updates, feature stability) exist alongside extraction (rising costs, forced upgrades). Has exit options (open-source, legacy software) but switching has operational costs. Mixed experience.
constraint_indexing:constraint_classification(subscription_economy_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOFTWARE PLATFORM OPERATOR (ROPE) — Primary beneficiary (institutional/arbitrage). Experiences subscription model as coordination solution: recurring revenue enables continuous development, cloud infrastructure maintenance, and feature updates. Sees constraint as solving legitimate coordination problem of funding sustainable software. Net positive extraction flow toward this agent.
constraint_indexing:constraint_classification(subscription_economy_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE COALITION (SCAFFOLD) — Organized alternative pathways (Linux, LibreOffice, Blender, Kubernetes) are building parallel ecosystems with sunset logic for proprietary subscription lock-in. Low effective extraction because coalition agents have documented exit path and are actively devaluing the subscription model through functional alternatives. Sunset: as open-source tools mature, subscription revenue model loses exclusive value.
constraint_indexing:constraint_classification(subscription_economy_model, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ENTERPRISE CUSTOMER (PITON) — Large organizations can negotiate volume discounts, self-host alternatives, or maintain legacy perpetual licenses for mission-critical systems. High mobility; perceived extraction is low. Yet institutional inertia keeps many enterprises locked into subscription billing despite negotiating power. Theater ratio high: contractual 'flexibility' performative while consolidation strategies reduce real alternatives.
constraint_indexing:constraint_classification(subscription_economy_model, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT) — Risks naturalizing subscription extraction as inevitable technological necessity ('cloud computing requires recurring payments'; 'continuous updates demand subscription models'). Structural data contradicts: open-source alternatives (LibreOffice, Nextcloud, self-hosted solutions) provide feature parity without recurring fees. Lock-in is institutional choice, not physical law. Engine detects as false mountain through extractiveness (0.52) and suppression (0.58) exceeding natural law thresholds.
constraint_indexing:constraint_classification(subscription_economy_model, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(subscription_economy_model, TR),
    TR >= 0.70.

:- end_tests(subscription_economy_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting the genuine coordination function (sustainable development, cloud maintenance, feature continuity) combined with real extraction mechanisms (rising costs, forced upgrades, data lock-in). The value has increased from 0.28 to 0.52 over the interval as subscription adoption consolidated and price accumulation across multiple tools became structural. Not a pure snare (0.66+) because users do receive ongoing value; not pure coordination (0.35 or below) because extraction is significant and suppression of alternatives is active. Suppression (0.58): Moderate-high. Perpetual licenses have been discontinued for most major platforms. Open-source and legacy alternatives exist but require technical knowledge and have switching costs. Data portability is often performative rather than functional. Consumers are not physically prevented from exiting but face substantial operational and learning barriers. Theater ratio (0.65): Moderately high. Vendor messaging emphasizes 'continuous innovation' and 'modern access models' while downplaying the loss of ownership and rising lifetime costs. Legitimate benefits exist (automatic updates do reduce security burden) but are intertwined with extraction benefits that accrue to vendors. Enterprise negotiations are performatively flexible while consolidation strategies (feature deprecation, API lock-in) reduce real alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a classic perspectival gap between extractors and extracted. Platform operators (institutional/arbitrage) see legitimate coordination: subscriptions fund R&D, cloud infrastructure, and security. Individual consumers (powerless/trapped) see extraction: they lose ownership, face perpetual billing, and cannot easily exit. Small businesses (moderate/constrained) experience both: they benefit from cloud features but suffer from rising costs and vendor-imposed feature changes. Open-source coalitions (organized/constrained) see a temporary constraint being solved: LibreOffice, Blender, and Nextcloud are demonstrating that sophisticated software can operate on community-supported models with lower theater. Enterprises (powerful/mobile) see a negotiable constraint: their scale gives them leverage to maintain legacy systems or negotiate custom arrangements. The analytical observer risks the false mountain: naturalizing subscription extraction as necessary for modern cloud software, when structural data shows that open-source models sustain complex software without recurring per-user extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the structural position of each agent. Platform operators are beneficiaries with arbitrage options (low d ≈ 0.10-0.20), experiencing negative or minimal effective extraction (chi dampened by their power and mobility). Individual consumers are victims with trapped status (high d ≈ 0.85-0.95), experiencing maximum extraction. Small businesses are partially captured victims with some constrained exit (moderate d ≈ 0.55-0.65), experiencing moderate extraction. Open-source coalition agents have organized power and documented exit paths (moderate d ≈ 0.40-0.50), reducing effective extraction through coalition strength. Enterprise customers have powerful institutional status with mobile options (d ≈ 0.35-0.45), able to negotiate down chi. The scope modifier σ(S) amplifies extractiveness at global scale (σ ≈ 1.2 for subscription model's global reach), since verification of alternatives is difficult across jurisdictions and network effects concentrate power.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies all three tangled rope gates: (1) genuine coordination function — subscription revenue does fund sustainable feature development, automatic updates, and cloud infrastructure maintenance; (2) asymmetric extraction — platform operators capture net benefit stream, users experience rising costs without ownership; (3) requires active enforcement — data portability barriers, API lock-in, and feature deprecation schedules are actively maintained by vendors to suppress exit. The mandatrophy is resolved by showing that the coordination function is real but systematically asymmetric: the same subscription model that benefits operators genuinely burdens consumers. This is not pure extraction (snare) hiding behind coordination rhetoric, nor is it pure coordination (rope) with incidental asymmetry. The coordination is genuine and necessary, but its asymmetric implementation creates extraction. The snare perspective (individual consumer/trapped) and scaffold perspective (open-source/sunset) are both empirically grounded: consumers are genuinely trapped (snare), and open-source alternatives are genuinely emerging (scaffold). The tangled rope classification holds the tension between both truths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_elasticity,
    'At what subscription cost threshold do individual consumers actually migrate to open-source or legacy alternatives rather than accept perpetual billing?',
    'Cross-sectional analysis of churn rates by price tier; panel studies tracking consumer switching behavior when subscription fees increase; correlation between alternative tool adoption and pricing events',
    'If threshold < $100/year: extraction is significantly suppressed by elastic demand, extractiveness should be revised downward to 0.35. If threshold > $500/year: suppression is severe, and extraction is largely uncontested, extractiveness confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_elasticity, empirical, 'Cost elasticity threshold for consumer exit to alternatives').

omega_variable(
    data_portability_sufficiency,
    'Do existing data export formats and cross-platform APIs constitute genuine exit options or merely performative interoperability?',
    'Usability testing of data migration workflows; measurement of data loss and format conversion failure rates; time-cost analysis of switching to competing platform with equivalent feature set',
    'If portable: exit_options upgrade from ''trapped'' to ''constrained'' for consumers, reducing snare classification strength. If non-portable: snare classification is robust; exit is genuinely suppressed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_portability_sufficiency, empirical, 'Whether data portability enables meaningful exit').

omega_variable(
    open_source_feature_parity_timeline,
    'How far behind proprietary subscription tools are open-source alternatives in core feature sets, and what is the estimated timeline to parity?',
    'Feature-by-feature benchmarking matrices; temporal analysis of open-source capability maturation; user satisfaction surveys comparing open-source to proprietary across feature domains',
    'If parity achieved < 5 years: scaffold sunset is empirically grounded, constraints on proprietary models accelerate. If parity > 10 years: open source remains marginal, sunset is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_feature_parity_timeline, empirical, 'Timeline to open-source feature parity with proprietary tools').

omega_variable(
    cloud_infrastructure_cost_pass_through,
    'What fraction of subscription fee increases are genuine infrastructure cost increases vs. profit margin expansion?',
    'Time-series decomposition of subscription price changes and cloud infrastructure costs (AWS, Azure, GCP pricing history); comparative analysis of on-premise vs. cloud deployment cost structures; regression analysis of margin expansion independent of input costs',
    'If > 70% margin expansion: coordination benefit claim is weakened; constraint classifies as pure extraction. If < 30% margin expansion: coordination function is real; tangled rope classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cloud_infrastructure_cost_pass_through, empirical, 'Degree to which price increases are rent-seeking vs. cost-driven').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subsec_tr_t0, subscription_economy_model, theater_ratio, 0, 0.45).
narrative_ontology:measurement(subsec_tr_t5, subscription_economy_model, theater_ratio, 5, 0.55).
narrative_ontology:measurement(subsec_tr_t10, subscription_economy_model, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(subsec_be_t0, subscription_economy_model, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(subsec_be_t5, subscription_economy_model, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(subsec_be_t10, subscription_economy_model, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model, software_vendor_consolidation).
narrative_ontology:affects_constraint(subscription_economy_model, digital_ownership_erosion).
narrative_ontology:affects_constraint(subscription_economy_model, cloud_infrastructure_lock_in).

% DUAL FORMULATION NOTE:
% The subscription economy model decomposes into three structurally related but distinct constraints: (1) subscription_economy_model (this file) — the general business model shift and its extraction mechanisms; (2) software_vendor_consolidation — market concentration enabling subscription imposition; (3) digital_ownership_erosion — the loss of user control over digital assets. Each has distinct empirical profiles and extraction vectors. The subscription model constrains more broadly but is enabled by consolidation and realized through ownership erosion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(subscription_economy_model, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
