% ============================================================================
% CONSTRAINT STORY: platform_vendor_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_vendor_lock_in, []).

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
 *   constraint_id: platform_vendor_lock_in
 *   human_readable: Platform Vendor Lock-In Constraint
 *   domain: technology/economics
 *
 * SUMMARY:
 *   Platform vendor lock-in represents a structural constraint where
 *   providers of critical digital infrastructure create and maintain high
 *   switching costs for dependent businesses and users. The constraint
 *   exhibits genuine coordination benefits (standardized APIs, payment
 *   processing, customer reach, logistics integration) alongside engineered
 *   extraction mechanisms (data enclosure, algorithmic preference opacity,
 *   unilateral policy changes, API deprecation). The constraint has
 *   intensified over the interval as platforms consolidated market power and
 *   as business dependence on platform infrastructure deepened. The theater
 *   ratio reflects how lock-in preservation relies increasingly on
 *   organizational inertia and perception of inevitability rather than
 *   technical barriers — interoperability is more feasible than platforms
 *   suggest, but switching costs remain formidable. The constraint operates
 *   across all DR types depending on observational position: it is trap for
 *   powerless vendors, mixed coordination-extraction for mid-tier businesses,
 *   pure coordination for platforms, a problem with a sunset for regulators,
 *   degraded ritual for legacy systems, and false natural law for
 *   civilizational observers.
 *
 * KEY AGENTS:
 *   - Platform Vendor (AWS, Apple, Google, Meta, Shopify, Stripe): Primary beneficiary (institutional/arbitrage) — captures ecosystem rent, margin compression on dependent businesses, unilateral control over terms
 *   - Dependent Business (small merchant, app developer, hosted SaaS startup): Primary victim (powerless/trapped) — faces insurmountable switching costs, zero negotiation power, vulnerability to unilateral platform changes
 *   - Mid-Tier Enterprise (larger SaaS, multi-channel retailer): Secondary victim/partial beneficiary (moderate/constrained) — experiences both genuine coordination benefits and asymmetric extraction; has exit options but at high cost
 *   - Interoperability Coalition (competing vendors, open-source foundations, regulators): Organized agents (organized/constrained) — building alternatives with sunset logic (Digital Markets Act, data portability mandates, open standards)
 *   - End Users (platform customers): Indirect victims (powerless/trapped) — suffer from reduced choice, higher prices, degraded service quality as platform-dependent businesses compress margins or exit
 *   - Ecosystem Autonomy (abstract structural good): Victim (powerless/trapped) — network effects and switching costs reduce global business diversity and create single points of failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_vendor_lock_in, 0.58).
domain_priors:suppression_score(platform_vendor_lock_in, 0.68).
domain_priors:theater_ratio(platform_vendor_lock_in, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_vendor_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_vendor_lock_in, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(platform_vendor_lock_in, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_vendor_lock_in, tangled_rope).
narrative_ontology:human_readable(platform_vendor_lock_in, "Platform Vendor Lock-In Constraint").
narrative_ontology:topic_domain(platform_vendor_lock_in, "technology/economics").

domain_priors:requires_active_enforcement(platform_vendor_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_vendor_lock_in, platform_vendor).
narrative_ontology:constraint_victim(platform_vendor_lock_in, dependent_businesses).
narrative_ontology:constraint_victim(platform_vendor_lock_in, end_users).
narrative_ontology:constraint_victim(platform_vendor_lock_in, ecosystem_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT BUSINESS (SNARE) — Small merchants relying entirely on platform infrastructure (Amazon seller, Shopify store, App Store developer) face insurmountable barriers to exit. Switching costs include porting customer data, rebuilding supply chain integrations, establishing new payment processing, and replicating platform features at prohibitive cost. Platform can derank, suspend, or change terms with no recourse. Maximum extraction experienced.
constraint_indexing:constraint_classification(platform_vendor_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER ENTERPRISE (TANGLED ROPE) — Larger business with technical capacity to diversify platforms faces high but surmountable exit costs (engineering resources, duplicated infrastructure, operational complexity). Experiences genuine coordination benefits (customer reach, payment processing, logistics integration) alongside asymmetric extraction (margin compression, algorithmic preference opacity, unilateral policy changes). Constrained exit produces mixed classification.
constraint_indexing:constraint_classification(platform_vendor_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM VENDOR (ROPE) — AWS, Apple, Google, Meta experience the constraint as pure coordination: managing ecosystem dependencies, standardizing interfaces, and extracting value from network effects. Experiences the lock-in as beneficial coordination enabling business model scale. Zero exit pressure; can arbitrage to other markets or acquisition targets.
constraint_indexing:constraint_classification(platform_vendor_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTEROPERABILITY COALITION (SCAFFOLD) — Competing vendors (Microsoft, open-source foundations, regulated telecom), alternative platforms (Shopify vs Amazon, Stripe vs PayPal), and antitrust regulators see lock-in as a temporary problem with a sunset clause. Digital Markets Act (EU), data portability mandates (GDPR), and open standards initiatives (WebAssembly, OIDC) are building interoperability pathways. Organized agents have exit tools and see expiration date on extraction mechanism.
constraint_indexing:constraint_classification(platform_vendor_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY LOCK-IN RITUAL (PITON) — Historical vendor lock-in mechanisms (Oracle database, Salesforce CRM, SAP) now operate primarily as theater: switching is more feasible than in 2000, but enterprise inertia, training investments, and integration depth persist despite reduced technical barriers. The lock appears more severe than actual exit constraints warrant. Theater ratio reflects the performative maintenance of lock-in through organizational momentum rather than technical inevitability.
constraint_indexing:constraint_classification(platform_vendor_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some degree of lock-in is inherent to any system with sunk costs: switching costs are a mathematical consequence of specialization and integration depth. The argument suggests vendor lock-in is immutable because specialization is immutable. However, the structural data contradicts this false summit — the constraints are policy and architecture choices (API enclosure, data portability restrictions, switching cost subsidies), not laws of nature.
constraint_indexing:constraint_classification(platform_vendor_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_vendor_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_vendor_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_vendor_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_vendor_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_vendor_lock_in, TR),
    TR >= 0.70.

:- end_tests(platform_vendor_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts via margin compression (platforms take 15-40% of transaction value for small vendors), unilateral policy changes (algorithm changes, API deprecation, term modifications), and option value suppression (dependent businesses cannot credibly threaten exit). However, the extraction is not as severe as pure snare (0.72+) because genuine coordination benefits exist and some dependent businesses remain profitable. The interval trajectory shows increasing extractiveness as platform consolidation deepened (2014-2024) and as platforms refined extraction mechanisms (algorithmic preference, dynamic pricing, bundled services). Suppression (0.68): High. Switching costs include technical (data port, API integration, retraining), economic (duplicated infrastructure, margin loss during transition), and organizational (change management, customer communication). Barriers to competition entry are formidable (network effects, scale economies). However, suppression is not total (0.90+) because open-source alternatives exist, some businesses do successfully multi-home, and interoperability mandates are reducing barriers. Theater ratio (0.55): Moderate. Lock-in is partly genuine technical-economic constraint (specialization and integration create real switching costs) and partly institutional narrative ('platforms are natural monopolies,' 'switching is impossible'). Interoperability is more feasible than industry framing suggests, but the performative insistence on inevitability sustains extraction. Theater has declined from historical levels (early cloud era: 0.75) because technical feasibility of switching is now obvious, but organizational inertia maintains theater at moderate level.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The dependent business (powerless/trapped) sees a snare with no exit: platform control of supply is total. The mid-tier business (moderate/constrained) sees tangled rope: genuine coordination mixed with asymmetric extraction. The platform (institutional/arbitrage) sees rope: managing ecosystem and capturing network value through coordination. The regulator/coalition (organized/constrained) sees scaffold: interoperability mandates and open standards provide a sunset mechanism (5-10 year horizon for DMA compliance). The legacy lock-in (institutional/arbitrage at civilizational scale) sees piton: switching is more feasible than organizational memory suggests. The civilizational analytical observer risks naturalizing contingent architecture as immutable law. This perspectival range from snare to rope is the signature of tangled rope at the primary (moderate) level: extraction exists alongside genuine coordination, with asymmetry between who benefits and who bears cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from beneficiary status (platform vendors) with arbitrage-level exit options and victim status (dependent businesses) with trapped-level exit options. Platform vendors derive d ≈ 0.05 (full beneficiaries + arbitrage exit → low d → negative f(d)): they experience effective extraction as negative (subsidized). Dependent businesses derive d ≈ 0.95 (full victims + trapped exit → high d → high f(d) ≈ 1.42): they experience maximal effective extraction. Mid-tier businesses derive d ≈ 0.55 (partial victim + constrained exit → moderate d → f(d) ≈ 0.75): mixed experience. The regulatory coalition derives d from organized power + constrained exit + partial victim status (regulation is being imposed on them) → d ≈ 0.40 → f(d) ≈ 0.40: they experience moderate extraction from the lock-in constraint they are trying to dismantle. This differentiation across institutional perspectives reveals the true structure: two institutions operating different roles in the same constraint, with different directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED TANGLED ROPE: The constraint avoids mandatrophy by satisfying all three gates: (1) beneficiaries declared (platform_vendor); (2) victims declared (dependent_businesses, end_users, ecosystem_autonomy); (3) requires_active_enforcement = true (platforms actively engineer and maintain lock-in through API enclosure, terms-of-service changes, algorithmic preference, data format restrictions). The tangled rope classification is confirmed by perspectival structure: the beneficiary sees rope (pure coordination), the victim sees snare (pure extraction), the moderate level sees tangled rope (both functions present). The presumption against mandatrophy — that the coordination function is genuine — is met: platforms do provide real value (standardized interfaces, reach, payment processing, logistics). However, the extraction function is also genuine and asymmetric. The classification avoids either romanticizing the coordination or dismissing the extraction. The interval measurements show extractiveness increasing (0.32 → 0.58) while theater_ratio stays moderate (0.35 → 0.55), indicating that extraction is becoming more transparent over time, not more theatrical — lock-in is being engineered more openly as market power consolidates. This pattern is characteristic of tangled rope degradation toward snare: if extractiveness reaches 0.70+ and enforcement becomes total, the classification would degrade to snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_composition,
    'What portion of switching costs are technological necessity vs. deliberate architecture choices?',
    'Audit of data export formats, API documentation, and integration points; comparison of lock-in depth across platforms with different architectures (open-source vs proprietary); measurement of actual vs theoretical switching friction',
    'If primarily necessity: lock-in is rope/scaffold (genuine coordination cost). If primarily deliberate: lock-in is snare (engineered extraction). This determines whether interoperability mandates reduce or fail to reduce effective extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_composition, empirical, 'Composition of switching costs: technological vs deliberate architecture').

omega_variable(
    multi_homing_feasibility,
    'Can dependent businesses operate profitably across multiple platforms simultaneously, or does the economics require single-platform concentration?',
    'Cost analysis of parallel infrastructure operation; survey of business models across platform combinations; measurement of margin compression under multi-homing vs single-platform concentration',
    'If feasible: powerless agents can exercise option of constrained exit (reclassify from trapped to constrained). If infeasible: platform has structural monopoly power independent of lock-in engineering (extraction mechanism is economic not institutional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_homing_feasibility, empirical, 'Whether profitable multi-homing is economically feasible').

omega_variable(
    interoperability_mandate_effectiveness,
    'Do digital markets regulation and data portability mandates actually reduce lock-in extraction or merely transfer it to compliance costs?',
    'Post-DMA measurement of business exit rates, margin recovery, and switching velocity; comparison of interoperability cost to original lock-in cost; analysis of whether regulation creates new lock-in (to compliance infrastructure)',
    'If effective: scaffold sunset is real and extraction declines post-mandate. If ineffective: mandates are theater and lock-in persists; reclassify as piton with higher theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_mandate_effectiveness, empirical, 'Whether interoperability mandates reduce effective lock-in extraction').

omega_variable(
    network_effect_dependency,
    'How much of the lock-in premium comes from genuine network effects (customers follow suppliers to platform) vs. switching cost engineering?',
    'Measurement of customer churn rates under platform switching; analysis of whether customers follow supplier across platforms or require re-acquisition; comparison of platform value to users as function of supplier availability',
    'If network effects dominate: lock-in reflects genuine coordination value (upgrade rope classification). If switching costs dominate: lock-in is engineered extraction (maintain snare/tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_dependency, empirical, 'Attribution of lock-in premium to network effects vs switching costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_vendor_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pvli_tr_t0, platform_vendor_lock_in, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pvli_tr_t5, platform_vendor_lock_in, theater_ratio, 5, 0.45).
narrative_ontology:measurement(pvli_tr_t10, platform_vendor_lock_in, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(pvli_be_t0, platform_vendor_lock_in, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pvli_be_t5, platform_vendor_lock_in, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(pvli_be_t10, platform_vendor_lock_in, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_vendor_lock_in, resource_allocation).
narrative_ontology:affects_constraint(platform_vendor_lock_in, data_portability_barriers).
narrative_ontology:affects_constraint(platform_vendor_lock_in, api_deprecation_cycles).
narrative_ontology:affects_constraint(platform_vendor_lock_in, marketplace_algorithmic_preference).

% DUAL FORMULATION NOTE:
% Platform vendor lock-in is decomposed into three structurally distinct constraints: (1) data_portability_barriers (ε≈0.35, rope) — genuine cost of moving data, partly technical necessity; (2) api_deprecation_cycles (ε≈0.52, tangled rope) — platforms maintain lock-in through deliberate API changes, mixed coordination and extraction; (3) marketplace_algorithmic_preference (ε≈0.65, snare) — algorithmic deranking/promotion operates with minimal coordination benefit and high extraction of merchant visibility. The parent constraint (platform_vendor_lock_in) is the integrated effect across these mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_vendor_lock_in, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
