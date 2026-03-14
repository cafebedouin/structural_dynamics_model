% ============================================================================
% CONSTRAINT STORY: vendor_lock_in_via_integration_costs
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vendor_lock_in_via_integration_costs, []).

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
 *   constraint_id: vendor_lock_in_via_integration_costs
 *   human_readable: Vendor Lock-In via Integration Costs
 *   domain: economic/technology
 *
 * SUMMARY:
 *   Vendor lock-in via integration costs represents a hybrid constraint
 *   combining genuine coordination benefits with asymmetric extraction. When
 *   a customer adopts an enterprise platform, they invest in integrations:
 *   custom API adapters, data migration pipelines, staff training, and
 *   process redesign around the vendor's specific interfaces. These
 *   integrations produce real coordination value — standardized APIs, managed
 *   evolution, vendor support. However, the vendor controls the boundary:
 *   proprietary API design, versioning choices, deprecation cycles, and
 *   feature bundling all make switching to competitors substantially more
 *   expensive than initially anticipated. The constraint accumulates over
 *   time: as more business logic depends on the vendor's platform, exit costs
 *   rise until customers experience the lock-in as entrapment. Theater ratio
 *   rises as vendors maintain performative complexity in their APIs
 *   (over-parameterization, deprecation theater, version-upgrade churn) that
 *   obscures simpler open-standard alternatives. Open-source standards
 *   (containers, Kubernetes, GraphQL) are gradually reducing switching costs,
 *   but adoption lags for mature legacy systems, creating a multi-decade
 *   window of extraction potential.
 *
 * KEY AGENTS:
 *   - Incumbent Vendor: Primary beneficiary (institutional/arbitrage) — controls API boundary, captures switching cost rents, can adjust pricing and features with minimal customer exit risk
 *   - Locked-In Customer: Primary victim (powerless/trapped) — has sunk capital into proprietary integrations, faces exit costs exceeding annual platform value
 *   - Mid-Market Evaluator: Secondary agent (moderate/constrained) — evaluates lock-in risk but sees genuine coordination benefits; faces genuine trade-off
 *   - Open Standards Coalition: Organized agent (organized/constrained) — Docker, Kubernetes, GraphQL provide alternative standards-based pathways; building sunset for proprietary lock-in
 *   - Legacy Integration Layer: Institutional artifact (institutional/arbitrage) — obsolete custom integrations (SOAP, FTP-based ETL) persist through inertia despite high maintenance cost
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing switching cost asymmetry as an inherent feature of software architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vendor_lock_in_via_integration_costs, 0.58).
domain_priors:suppression_score(vendor_lock_in_via_integration_costs, 0.65).
domain_priors:theater_ratio(vendor_lock_in_via_integration_costs, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vendor_lock_in_via_integration_costs, extractiveness, 0.58).
narrative_ontology:constraint_metric(vendor_lock_in_via_integration_costs, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(vendor_lock_in_via_integration_costs, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vendor_lock_in_via_integration_costs, tangled_rope).
narrative_ontology:human_readable(vendor_lock_in_via_integration_costs, "Vendor Lock-In via Integration Costs").
narrative_ontology:topic_domain(vendor_lock_in_via_integration_costs, "economic/technology").

domain_priors:requires_active_enforcement(vendor_lock_in_via_integration_costs).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vendor_lock_in_via_integration_costs, incumbent_vendor).
narrative_ontology:constraint_victim(vendor_lock_in_via_integration_costs, customer_organizations).
narrative_ontology:constraint_victim(vendor_lock_in_via_integration_costs, downstream_integrators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN CUSTOMER (SNARE) — Customer organization has sunk substantial capital into integration: custom API adaptors, data migration pipelines, staff training, process redesign around vendor's specific interfaces. Exit cost (switching to competitor, rebuilding integrations, staff retraining, data re-extraction) exceeds the annual value delivered by the vendor's platform. No meaningful alternatives exist with comparable feature depth. Maximum extraction — the customer experiences the constraint as pure capture with no coordination benefit.
constraint_indexing:constraint_classification(vendor_lock_in_via_integration_costs, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-MARKET EVALUATOR (TANGLED ROPE) — Organization considering vendor adoption faces genuine coordination benefit (standardized integrations, managed API evolution, vendor support ecosystem) alongside extraction risk (proprietary data formats, API versioning lock-in, deprecation cycles designed to force upgrades). Can exit at moderate cost (18-36 months to migrate, some feature loss), but the integration coordination value is real. Mixed experience: true coordination plus asymmetric extraction.
constraint_indexing:constraint_classification(vendor_lock_in_via_integration_costs, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT VENDOR (ROPE) — Vendor benefits from ecosystem lock-in through integration coordination: standardized APIs enable third-party developers to build complementary tools, creating positive feedback. Vendor experiences the constraint as pure coordination — it solves the real problem of how to connect diverse systems. High arbitrage: vendor can switch strategies (pricing, feature deprecation, API restrictions) with minimal cost.
constraint_indexing:constraint_classification(vendor_lock_in_via_integration_costs, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN STANDARDS COALITION (SCAFFOLD) — Open-source projects (Docker containers, Kubernetes APIs, GraphQL, protocol buffers) provide standardized integration mechanisms that bypass proprietary vendor APIs. This perspective sees vendor lock-in as a temporary coordination failure being resolved through open standards adoption. Sunset clause: as container orchestration and open APIs mature, the cost of switching vendors declines sharply. Estimated horizon: 10-15 years for standardization to reduce integration switching costs by 70%.
constraint_indexing:constraint_classification(vendor_lock_in_via_integration_costs, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY INTEGRATION LAYER (PITON) — Many organizations maintain decades-old custom integrations (SOAP wrappers around REST APIs, FTP-based ETL pipelines, custom parsing for semi-structured data exports) that were built before standardization matured. These integrations persist through institutional inertia rather than actual function — they work but are labor-intensive to maintain. Theater ratio is high: substantial overhead devoted to keeping obsolete integration methods operational. The constraint degrades but persists because replacement requires coordinated migration.
constraint_indexing:constraint_classification(vendor_lock_in_via_integration_costs, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, the integration cost asymmetry appears as an immutable feature of software architectures: any system boundary has switching costs, and vendors who own those boundaries can extract rents. This perspective naturalizes what is actually a contingent design choice (proprietary APIs vs open standards). The engine will flag this as a false summit — the structural data (moderate theater, genuine coordination, strong beneficiary/victim asymmetry) indicates the constraint is contingent, not fundamental.
constraint_indexing:constraint_classification(vendor_lock_in_via_integration_costs, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vendor_lock_in_via_integration_costs_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vendor_lock_in_via_integration_costs, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vendor_lock_in_via_integration_costs, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vendor_lock_in_via_integration_costs, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vendor_lock_in_via_integration_costs, TR),
    TR >= 0.70.

:- end_tests(vendor_lock_in_via_integration_costs_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The vendor captures substantial rents through switching cost asymmetry, but the constraint is not as severe as pure extraction because customers do derive genuine coordination value. The initial extractiveness (0.35) was low because early adoption had no lock-in history. As the platform matures and switching costs accumulate, extractiveness rises to 0.58 — the vendor's ability to increase prices, restrict features, or force upgrades depends on customer lock-in. Suppression (0.65): High. Customers face material barriers to exit: redevelopment costs (12-36 months for large deployments), data re-extraction complexity, staff retraining, business continuity risk during migration. These barriers are high but not total — some customers do switch, particularly when new competitors offer compelling feature advantages. Theater ratio (0.48): Moderate, rising. Early integration work (0.30) is mostly functional — genuine API design, real feature development. As the platform matures, vendors introduce performative complexity: API versioning churns, deprecation cycles designed to force upgrades, feature bundling that obscures simpler alternatives. Theater ratio rises to 0.48 as legacy maintenance consumes increasing effort.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates asymmetric experience across the observation site. The incumbent vendor (Rope) genuinely sees pure coordination — standardized APIs enable ecosystem development, vendor support solves real integration problems. The open standards coalition (Scaffold) sees coordination with a sunset — Kubernetes and GraphQL will eventually make switching cheaper, creating an exit path within 10-15 years. The legacy integration layer (Piton) sees degraded ritual — maintaining old SOAP wrappers and FTP pipelines consumes resources but is increasingly unnecessary (theater ratio 0.48 reflects this). The mid-market evaluator (Tangled Rope) sees genuine tradeoff — real coordination benefits alongside real extraction risk. The locked-in customer (Snare) experiences pure capture — the coordination benefit is no longer salient because exit costs dominate. The analytical observer (Mountain) risks naturalizing the extraction as inherent to software architecture — 'every system has switching costs' — but the structural data reveals this as contingent on API design choices, not fundamental physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across perspectives based on exit options and power level. The locked-in customer (powerless/trapped) experiences maximum d (≈0.95): they are the target, they have no escape path, all extraction flows toward them. The incumbent vendor (institutional/arbitrage) experiences minimum d (≈0.05): they are the beneficiary, they control exit costs, they can walk away from any customer relationship without material damage. The mid-market evaluator (moderate/constrained) experiences intermediate d (≈0.55): they see genuine coordination benefits (lower d) but face real exit costs (higher d). The open standards coalition (organized/constrained) experiences lower d (≈0.40) because they see a clear exit path — open standards reduce switching costs sharply within their time horizon. The directionality pipeline (beneficiary/victim + exit → d) produces the chi values that differentiate perspectives despite identical base properties.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that lock-in is neither pure coordination (Rope) nor pure extraction (Snare) but a genuine hybrid (Tangled Rope). The vendor's platform provides real coordination value — standardized APIs, ecosystem development, managed evolution. But this coordination function co-exists with asymmetric extraction: the vendor controls the boundary, can raise prices by leveraging switching costs, and can force upgrades through deprecation cycles. The mandatrophy is avoided by acknowledging both functions are structurally real. The beneficiary/victim declarations are critical: without identifying locked-in customers as victims, the constraint appears as pure coordination (Rope). Without identifying genuine API coordination benefits, it appears as pure extraction (Snare). The tangled rope classification requires both beneficiaries and victims. The temporal measurement (extractiveness rising from 0.35 to 0.58) shows the hybrid mechanism: early coordination value (0.35) is real, but as lock-in accumulates, extraction dominates (0.58). Theater ratio rising to 0.48 indicates that performative complexity (API versioning, deprecation theater) is increasingly layered onto the genuine coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_cost_magnitude_threshold,
    'At what integration cost level does a vendor platform transition from coordination mechanism to extraction mechanism?',
    'Empirical analysis: compare customer switching rates at different integration cost tiers; identify cost threshold where switching probability drops sharply',
    'If threshold < 10% annual platform cost: most integrations remain mobile (Rope from more perspectives). If threshold > 50% annual cost: many legitimate coordinations misclassified as extraction (Snare from customer perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_cost_magnitude_threshold, empirical, 'Integration cost threshold for lock-in onset').

omega_variable(
    open_standards_adoption_speed,
    'Will container orchestration and open APIs mature fast enough to reduce switching costs before vendor lock-in becomes entrenched in mission-critical systems?',
    'Longitudinal tracking of enterprise API standardization adoption; measurement of switching costs for containerized vs legacy monolithic systems; correlation between standards maturity and actual customer switching behavior',
    'If standards mature within 10 years: scaffold sunset is real, lock-in is temporary. If adoption stalls beyond 15 years: lock-in becomes structural, classification shifts toward persistent snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_standards_adoption_speed, empirical, 'Speed of open standards adoption reducing switching costs').

omega_variable(
    switching_cost_opacity,
    'Are integration switching costs genuinely opaque to customers at the time of vendor selection, or do sophisticated buyers accurately anticipate lock-in?',
    'Analysis of RFP evaluation practices; customer interviews on switching cost expectations; comparison of anticipated vs realized exit costs across vendor transitions',
    'If genuinely opaque: customers are deceived, classifies as snare (victim trapped through information asymmetry). If transparent: customers choose lock-in knowingly, classifies as tangled rope (coordination + extraction both understood).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_opacity, empirical, 'Whether integration switching costs are transparent to customers').

omega_variable(
    proprietary_api_necessity,
    'Do vendor-specific API features provide material functional advantages over open standards, or is proprietary lock-in maintained purely for rent extraction?',
    'Feature parity analysis: compare capability gaps between proprietary vendor APIs and open-standard equivalents; track which proprietary features customers actually use vs maintain for historical reasons',
    'If genuine advantage: lock-in reflects real coordination value, tangled rope classification justified. If purely extractive: classification shifts toward snare, integration coordination is theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_api_necessity, empirical, 'Whether proprietary APIs provide functional advantages over open standards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vendor_lock_in_via_integration_costs, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vlk_tr_t0, vendor_lock_in_via_integration_costs, theater_ratio, 0, 0.3).
narrative_ontology:measurement(vlk_tr_t3, vendor_lock_in_via_integration_costs, theater_ratio, 3, 0.42).
narrative_ontology:measurement(vlk_tr_t6, vendor_lock_in_via_integration_costs, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(vlk_be_t0, vendor_lock_in_via_integration_costs, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vlk_be_t3, vendor_lock_in_via_integration_costs, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(vlk_be_t6, vendor_lock_in_via_integration_costs, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vendor_lock_in_via_integration_costs, resource_allocation).
narrative_ontology:affects_constraint(vendor_lock_in_via_integration_costs, enterprise_software_ecosystem_dependency).
narrative_ontology:affects_constraint(vendor_lock_in_via_integration_costs, open_standards_adoption).

% DUAL FORMULATION NOTE:
% Vendor lock-in via integration costs represents a decomposition of the broader concept of 'platform dependency.' Integration lock-in (this story) focuses on API switching costs and has ε=0.58 (Tangled Rope). Feature lock-in (pricing models, complementary services) would constitute a separate story with different ε. Data lock-in (proprietary formats, export restrictions) is a third story. Together they form the platform dependency family, linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vendor_lock_in_via_integration_costs, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
