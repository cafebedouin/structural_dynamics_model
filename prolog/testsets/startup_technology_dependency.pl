% ============================================================================
% CONSTRAINT STORY: startup_technology_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_startup_technology_dependency, []).

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
 *   constraint_id: startup_technology_dependency
 *   human_readable: Startup Technology Dependency and Vendor Lock-in
 *   domain: technology/economics/platform_governance
 *
 * SUMMARY:
 *   Startup technology dependency on cloud platforms represents a hybrid
 *   coordination-extraction mechanism that exhibits all characteristics of
 *   Tangled Rope. Startups depend on cloud infrastructure (AWS, Google Cloud,
 *   Azure) for rapid scaling, geographic distribution, and access to managed
 *   services (databases, ML pipelines, serverless compute). These platforms
 *   genuinely solve the coordination problem of providing reliable, scalable
 *   infrastructure without massive capital investment. However, this
 *   coordination function coexists with systematic extraction through vendor
 *   lock-in: APIs designed to be difficult to migrate from, pricing
 *   structures that reward deeper integration, and operational complexity
 *   that increases the switching cost. The constraint has been intensifying
 *   over 6 years (extractiveness rising from 0.35 to 0.58) as platforms have
 *   added more specialized managed services and startups have built deeper
 *   dependencies. Concurrently, the theater ratio has risen from 0.40 to
 *   0.55, reflecting that vendor marketing (promoting proprietary services
 *   and 'cloud-native' architectures) increasingly substitutes for actual
 *   coordination value. The emergence of containerization (Docker/Kubernetes)
 *   and multi-cloud tooling represents a structural challenge to the lock-in
 *   mechanism, classifying as Scaffold from the perspective of organized
 *   agents who can adopt these technologies.
 *
 * KEY AGENTS:
 *   - Startup Founders: Primary victim (powerless/trapped) — structurally locked into platform choices made at launch; migration becomes progressively more expensive as technical debt accumulates
 *   - Startup Engineering Teams: Secondary victim (moderate/constrained) — benefit from managed services but lose career optionality and face operational complexity from lock-in
 *   - Cloud Platform Providers (AWS, Google Cloud, Azure): Primary beneficiary (institutional/arbitrage) — extract value through lock-in while providing genuine infrastructure coordination; maintain arbitrage options by diversifying customer segments
 *   - Containerization Community (CNCF, Kubernetes ecosystem): Organized agent (organized/mobile) — provides technological exit mechanism through portable infrastructure abstractions; reduces lock-in force over time
 *   - Well-Capitalized Startups: Secondary beneficiary (powerful/mobile) — can negotiate better terms and maintain multi-cloud strategies; extract less because they retain exit options
 *   - Traditional Enterprise IT: Tertiary actor (institutional/constrained) — uses cloud services but maintains hybrid architectures; lock-in mechanism has degraded relative to startup context
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(startup_technology_dependency, 0.58).
domain_priors:suppression_score(startup_technology_dependency, 0.62).
domain_priors:theater_ratio(startup_technology_dependency, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(startup_technology_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(startup_technology_dependency, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(startup_technology_dependency, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(startup_technology_dependency, tangled_rope).
narrative_ontology:human_readable(startup_technology_dependency, "Startup Technology Dependency and Vendor Lock-in").
narrative_ontology:topic_domain(startup_technology_dependency, "technology/economics/platform_governance").

domain_priors:requires_active_enforcement(startup_technology_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(startup_technology_dependency, cloud_platform_providers).
narrative_ontology:constraint_beneficiary(startup_technology_dependency, proprietary_sdks_vendors).
narrative_ontology:constraint_victim(startup_technology_dependency, startup_founders).
narrative_ontology:constraint_victim(startup_technology_dependency, startup_employees).
narrative_ontology:constraint_victim(startup_technology_dependency, product_diversification).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STARTUP FOUNDER (SNARE) — Trapped by accumulated technical debt, customer expectations, and investor pressure to scale on the chosen platform. Exits from AWS/Google Cloud/Azure are theoretically possible but require months of engineering effort, increased operational costs during migration, and risk of service disruption that could kill the business. The founder's technical choices cascade into institutional lock-in: every feature, every deployment script, every customer integration assumes the platform's specific APIs, pricing structure, and availability zones. No meaningful exit option within a career timescale.
constraint_indexing:constraint_classification(startup_technology_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: STARTUP ENGINEERING TEAM (TANGLED ROPE) — Constrained by the need to deliver product velocity and meet deployment deadlines. The platform's managed services (databases, caching, message queues, ML pipelines) do provide genuine coordination value: they reduce the need to build infrastructure, enable rapid scaling, and allow the team to focus on product. But this coordination function coexists with extraction: vendor lock-in prevents switching even when better alternatives exist, and pricing lock-in means the startup captures no benefit from the platform's cost reductions over time. The team's skills become platform-specific, limiting career mobility. Some benefit, significant extraction.
constraint_indexing:constraint_classification(startup_technology_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CLOUD PLATFORM PROVIDER (ROPE) — Experiences the constraint as a coordination mechanism: providing managed infrastructure enables startups to launch quickly and scale globally without capital expenditure on data centers. The platform benefits from this coordination (sustained customer growth, expanding ecosystem) and also extracts through pricing tiers and lock-in mechanisms. From the provider's immediate perspective, this is a win-win coordination mechanism. They have arbitrage options (moving to different business models, competing platforms) that keep their experience of extraction minimal — they can exit the startup market segment entirely if terms degrade.
constraint_indexing:constraint_classification(startup_technology_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONTAINERIZATION AND KUBERNETES MOVEMENT (SCAFFOLD) — Open-source containerization (Docker, Kubernetes, CNCF ecosystem) provides a technological pathway out of vendor lock-in. Startups that adopt these tools early gain portable infrastructure that can run on any cloud provider or on-premises. This is a temporary support structure with an implicit sunset: as containerization becomes standard and multi-cloud tooling matures, the lock-in mechanism loses force. The constraint persists but becomes progressively less binding. This perspective classifies as scaffold because the exit path is real and accelerating, even though adoption is not yet universal.
constraint_indexing:constraint_classification(startup_technology_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL ENTERPRISE IT (PITON) — Large enterprises with legacy on-premises infrastructure still maintain cloud vendor relationships, but the dependency has atrophied. The IT department uses cloud services for specific workloads while maintaining hybrid architectures, reducing the lock-in mechanism's force. What remains is often performative: compliance frameworks and procurement processes that mandate cloud provider certifications even when the actual technical lock-in is weak. The theater persists (compliance documentation, audit trails, vendor management reviews) but the underlying extraction mechanism has degraded as alternative deployment options have matured.
constraint_indexing:constraint_classification(startup_technology_dependency, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: WELL-CAPITALIZED STARTUP (TANGLED ROPE) — Startups with sufficient capital and engineering resources can maintain multi-cloud strategies or migrate between platforms. The constraint still binds (migration is costly, fragmentation adds complexity, operational overhead is high), but mobility increases significantly. These startups can negotiate better terms, build custom infrastructure abstraction layers, and retain exit options. The constraint persists as genuine coordination (infrastructure management) plus extraction (premium pricing for flexibility, operational complexity), but effective extraction is lower due to available agency.
constraint_indexing:constraint_classification(startup_technology_dependency, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some degree of technical standardization and platform concentration appears inevitable: the economies of scale in cloud infrastructure, the coordination costs of polyglot deployments, and the switching costs inherent to distributed systems might be treated as natural laws. This perspective risks naturalizing what is actually a contingent feature of current market structure and regulatory environments. However, the structural data reveals this as a false summit: technological alternatives (containerization, serverless abstractions, edge computing) are reducing lock-in, and regulatory frameworks (cloud interoperability mandates, data portability requirements) are eroding the mechanism.
constraint_indexing:constraint_classification(startup_technology_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(startup_technology_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(startup_technology_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(startup_technology_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(startup_technology_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(startup_technology_dependency, TR),
    TR >= 0.70.

:- end_tests(startup_technology_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): High-moderate. Cloud platforms extract value through lock-in mechanisms (API switching costs, specialized managed services, pricing tiers) while providing legitimate coordination services. The value is not as extreme as pure extraction (Snare would be ≥0.66) because startups do benefit from rapid time-to-market and operational simplicity. However, extractiveness has risen over the interval as platforms have invested in lock-in mechanisms. Suppression (0.62): Moderate-high. Startup founders face significant barriers to exit: technical switching costs (months of engineering effort), operational risk during migration, customer expectations locked into platform-specific features, and capital constraints that make parallel development difficult. However, suppression is not total — containerization and open-source alternatives reduce barriers, and well-capitalized startups can exit. Theater ratio (0.55): Moderate. Platform vendor marketing ('cloud-native development,' proprietary service benefits) increasingly substitutes for actual coordination value. Compliance and best-practice documentation create performative overhead. However, the coordination function remains core — this is not a pure piton (which would be ≥0.70 theater).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how indexical positioning creates radically different classifications from identical structural data. The founder sees Snare; the platform sees Rope; the engineering team sees Tangled Rope; the containerization movement sees Scaffold; and the analytical observer risks a false mountain (naturalizing concentration as inevitable). The gap is not about disagreement on facts but about structural position relative to the extraction flow. Each perspective is correct from its position. The constraint's full logical form is the presheaf over all these positions, not any single perspective. The mandatrophy resolves by recognizing that no single type is 'true' — the truth is the family of observations indexed by position.
 *
 * DIRECTIONALITY LOGIC:
 *   Cloud platform providers derive low directionality (d ≈ 0.15-0.20) from beneficiary status + arbitrage exit options: they can walk away from the startup market segment entirely if terms degrade, and they benefit from the constraint. Startup founders derive high directionality (d ≈ 0.90) from victim status + trapped exit options: they bear the switching costs and have no realistic exit within a biographical timescale (5-10 years at company). Containerization advocates derive moderate directionality (d ≈ 0.55) from victim status (they are reducing a harmful constraint) + mobile exit options (they can develop and deploy alternatives). Engineering teams derive higher directionality (d ≈ 0.70) from mixed victim status (skills degradation, lock-in) + constrained exit options (can migrate platforms but at high cost). The directionality values drive the χ computation via the sigmoid f(d), producing experienced extractiveness that varies from near-zero (platform provider) to maximum (founder).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint demonstrates the critical case where a single structural phenomenon (cloud platform dependency) genuinely exhibits properties of both coordination (Rope) and extraction (Snare) simultaneously. The mandatrophy is resolved not by choosing a single classification but by recognizing that the classification IS position-dependent. The constraint provides genuine coordination value (managed infrastructure, rapid scaling, operational simplicity) while simultaneously extracting value through lock-in mechanisms. For powerless agents (founders), the extraction dominates and the constraint appears as Snare. For institutional agents (platform providers), the coordination dominates and the constraint appears as Rope. For organized agents building alternatives (Kubernetes community), the constraint appears as Scaffold — a temporary coordination problem being solved by technological transition. The Tangled Rope classification is correct from the moderate perspective (engineering teams) and from the global analytical view. The constraint is not mislabeled coordination (which would occur if we called it Rope when it clearly extracts) nor mislabeled extraction (which would occur if we called it Snare when it genuinely coordinates). The Tangled Rope label correctly captures that both mechanisms are present and neither dominates sufficiently to classify as pure Rope (χ ≤ 0.35) or pure Snare (χ ≥ 0.66). The extractiveness rising over the 6-year interval reveals that platform providers are deliberately amplifying the extraction component while maintaining sufficient coordination value to justify the relationship. This is exactly the Tangled Rope dynamic: active enforcement (proprietary service proliferation) maintaining asymmetric extraction alongside genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    containerization_adoption_threshold,
    'At what adoption rate does containerization technology (Docker/Kubernetes) become a sufficient exit mechanism to reclassify startup lock-in from Snare to Tangled Rope?',
    'Market survey of startup containerization adoption rates; measurement of multi-cloud deployment prevalence; analysis of engineering team capacity to manage container orchestration. Industry benchmarks from CNCF surveys.',
    'If adoption exceeds 60%: lock-in mechanism loses force, classification shifts toward Rope or Scaffold for powerless agents. If adoption remains below 40%: trap mechanism persists as Snare. Current estimate: ~45% of startups use containers, placing the boundary near current state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(containerization_adoption_threshold, empirical, 'Containerization adoption threshold for lock-in reduction').

omega_variable(
    platform_pricing_lock_mechanism,
    'Is vendor lock-in driven primarily by technical switching costs or by pricing/contract structures designed to extract value from lock-in?',
    'Comparison of switching costs across different platform components (compute, storage, networking, managed services). Analysis of pricing discounts offered to locked-in customers vs those with multi-cloud architectures. Regulatory intervention analysis (does price transparency or switching assistance reduce extraction?).',
    'If technical switching costs dominate: constraint is unavoidable infrastructure coordination (Rope). If pricing lock-in dominates: constraint is engineered extraction (Snare). If both equally: constraint is genuine Tangled Rope. Current evidence suggests pricing mechanisms amplify technical lock-in by roughly 1.5x.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_pricing_lock_mechanism, empirical, 'Whether lock-in is driven by technical costs or engineered pricing').

omega_variable(
    regulatory_fragmentation_costs,
    'Do emerging data residency and interoperability regulations (GDPR, proposed digital markets acts, cloud interoperability mandates) reduce or increase the effective lock-in by imposing compliance fragmentation costs?',
    'Cost analysis of multi-cloud vs single-cloud deployments under various regulatory regimes. Case studies of startups operating across EU/US with different data residency requirements. Longitudinal measurement of compliance overhead.',
    'If regulations reduce lock-in costs below current levels: Snare → Tangled Rope shift. If regulations increase fragmentation costs: lock-in persists or strengthens despite containerization. Current trajectory: early evidence suggests interoperability mandates will reduce lock-in by ~15-20%.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_fragmentation_costs, empirical, 'Impact of data residency and interoperability regulations on lock-in').

omega_variable(
    startup_failure_rate_correlation,
    'How much of startup failure in the 2-5 year window correlates with lock-in costs during platform migration attempts vs other factors (market fit, competition, team dynamics)?',
    'Post-mortem analysis of failed startup technical decisions; interviews with failed founders about migration attempts; comparison of survival rates between startups that maintained multi-cloud vs single-platform strategies.',
    'If lock-in costs correlate with >30% of failures: constraint is genuinely extractive (Snare). If correlation is <10%: extraction may be overstated (constraint is actual coordination). Current anecdotal evidence: ~15-20% of startup failures cite infrastructure migration or lock-in as contributing factors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(startup_failure_rate_correlation, empirical, 'Correlation between lock-in costs and startup failure rates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(startup_technology_dependency, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(startdep_tr_t0, startup_technology_dependency, theater_ratio, 0, 0.4).
narrative_ontology:measurement(startdep_tr_t3, startup_technology_dependency, theater_ratio, 3, 0.48).
narrative_ontology:measurement(startdep_tr_t6, startup_technology_dependency, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(startdep_be_t0, startup_technology_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(startdep_be_t3, startup_technology_dependency, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(startdep_be_t6, startup_technology_dependency, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(startup_technology_dependency, resource_allocation).
narrative_ontology:affects_constraint(startup_technology_dependency, cloud_market_consolidation).
narrative_ontology:affects_constraint(startup_technology_dependency, startup_capital_dependency).
narrative_ontology:affects_constraint(startup_technology_dependency, technical_debt_accumulation).

% DUAL FORMULATION NOTE:
% Startup technology dependency decomposes into three distinct constraints: (1) infrastructure coordination (genuine Rope, ε≈0.15) — the legitimate problem of providing scalable infrastructure; (2) vendor lock-in extraction (Snare, ε≈0.68) — the deliberate API and pricing mechanisms designed to make switching costly; (3) operational theater (Piton, theater≈0.55) — vendor marketing and compliance documentation. The Tangled Rope classification at ε=0.58 represents the aggregate of all three mechanisms. Decomposing into separate stories would provide finer-grained analysis but would sacrifice the diagnostic value of showing how a single natural-language concept (startup cloud dependency) encompasses multiple structurally distinct constraints that are causally coupled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(startup_technology_dependency, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
