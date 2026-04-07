% ============================================================================
% CONSTRAINT STORY: cloud_infrastructure_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cloud_infrastructure_lock_in, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cloud_infrastructure_lock_in
 *   human_readable: Cloud Infrastructure Lock-In
 *   domain: technology/infrastructure/economic
 *
 * SUMMARY:
 *   Cloud infrastructure lock-in represents a structural extraction mechanism
 *   that operates through accumulated switching costs, proprietary API
 *   ecosystems, and data gravity. Unlike physical infrastructure, cloud
 *   services appear portable by design (code, containers, APIs) but achieve
 *   lock-in through the interaction of technical design, pricing mechanisms,
 *   and organizational inertia. The constraint exhibits a perspectival range
 *   from pure extraction (snare) for powerless enterprises to genuine
 *   coordination (rope) for providers. The multi-cloud movement (Scaffold)
 *   represents an organized attempt to reduce suppression through
 *   containerization and open standards, but faces structural limits from
 *   proprietary service expansion and ecosystem effects. Theater ratio
 *   remains moderate — the lock-in is not primarily performative but
 *   structurally embedded in APIs, data location, and managed services.
 *
 * KEY AGENTS:
 *   - Enterprise customers: Primary victims (powerless/trapped) — bear switching costs that accumulate over time; minimum 5-year horizon for cost recovery makes exit irreversible
 *   - Cloud platform providers (AWS, Azure, GCP): Primary beneficiaries (institutional/arbitrage) — capture rent from lock-in, maintain pricing power, benefit from ecosystem lock
 *   - Multi-cloud consortia (CNCF, OpenStack): Organized agents (organized/constrained) — coordinate portability standards while facing constraint from provider proprietary service expansion
 *   - Container/Kubernetes ecosystem: Powerful agents (powerful/mobile) — provide temporary suppression relief through abstraction layers; represent scaffold sunset pathway
 *   - Cloud-native startups: Mixed position (powerful/arbitrage) — have early arbitrage options but face lock-in once established; shift from mobile to constrained as workloads accumulate
 *   - Legacy data center operators: Institutional agents (institutional/arbitrage) — maintain parallel infrastructure through inertia; piton classification reflects institutional degradation
 *   - Analytical observer: Civilizational perspective — reveals structural snare at global scale despite competitive market appearance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cloud_infrastructure_lock_in, 0.58).
domain_priors:suppression_score(cloud_infrastructure_lock_in, 0.62).
domain_priors:theater_ratio(cloud_infrastructure_lock_in, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cloud_infrastructure_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(cloud_infrastructure_lock_in, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cloud_infrastructure_lock_in, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cloud_infrastructure_lock_in, tangled_rope).
narrative_ontology:human_readable(cloud_infrastructure_lock_in, "Cloud Infrastructure Lock-In").
narrative_ontology:topic_domain(cloud_infrastructure_lock_in, "technology/infrastructure/economic").

domain_priors:requires_active_enforcement(cloud_infrastructure_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cloud_infrastructure_lock_in, cloud_providers).
narrative_ontology:constraint_beneficiary(cloud_infrastructure_lock_in, platform_ecosystem_developers).
narrative_ontology:constraint_victim(cloud_infrastructure_lock_in, enterprise_customers).
narrative_ontology:constraint_victim(cloud_infrastructure_lock_in, competitive_cloud_market).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MIGRATING ENTERPRISE (SNARE) — Organization with 5+ years of workloads, custom integrations, and bespoke tooling in a single cloud provider. Exit costs include data transfer fees (often prohibitive), retraining staff on new platform APIs, rewriting proprietary integrations, and downtime during migration. Switching costs accumulate faster than the enterprise can pay them. Lock-in becomes irreversible at biographical timescale.
constraint_indexing:constraint_classification(cloud_infrastructure_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MULTI-CLOUD COALITION (TANGLED ROPE) — Industry consortia (Linux Foundation, Open Infrastructure Foundation) and cooperative cloud efforts (OpenStack, Apache foundation projects) experience the constraint as both coordination (enable portability standards, containerization, Kubernetes) AND extraction (cloud providers maintain incompatible proprietary services, making true portability impossible). Genuine coordination function exists alongside asymmetric extraction by major providers.
constraint_indexing:constraint_classification(cloud_infrastructure_lock_in, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CLOUD PLATFORM PROVIDER (ROPE) — Experiences lock-in as coordination mechanism: the constraint enables vendor-specific service optimization, proprietary tooling integration, and customer relationship continuity. Provider views the lock-in as solving the problem of building sustainable platform ecosystems. No experienced extraction — the provider is the beneficiary.
constraint_indexing:constraint_classification(cloud_infrastructure_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONTAINERIZATION/KUBERNETES MOVEMENT (SCAFFOLD) — Docker, Kubernetes, and container orchestration represent temporary but declining suppression. Containers enable workload portability across cloud providers by abstracting infrastructure details. Theater is moderate (container management still requires provider-specific optimization). Sunset clause is real: as container standards mature and multi-cloud tooling becomes production-grade, the lock-in mechanism loses potency. Estimated sunset: 5-10 years for multi-cloud orchestration to become commodity.
constraint_indexing:constraint_classification(cloud_infrastructure_lock_in, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CLOUD-NATIVE DEVELOPER ECOSYSTEM (TANGLED ROPE) — Startups and independent developers have arbitrage options (choose provider at inception, negotiate terms, sell to larger entities). But they also experience coordinated extraction: dominant cloud providers set API standards, pricing tiers, and feature availability that lock in early design decisions. Developers face asymmetric extraction through proprietary managed services and pricing lock after initial growth.
constraint_indexing:constraint_classification(cloud_infrastructure_lock_in, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY DATA CENTER MODEL (PITON) — Private infrastructure (on-premises, private cloud) has become vestigially maintained. Enterprises run parallel workloads: mission-critical on cloud, legacy systems in data centers. The data center persists through institutional inertia despite cloud alternatives. Theater ratio is high (compliance audits, regulatory requirements, change management theater). The model is degraded — maintained because exit costs are high, not because it functions better. Represents path-dependent institutional lock rather than structural extraction.
constraint_indexing:constraint_classification(cloud_infrastructure_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, the cloud lock-in mechanism is structural: data gravity (cost of moving petabytes), ecosystem lock (services built on proprietary APIs), and pricing dynamics create an extraction machine that persists despite competitive threats. The analytical view reveals the snare structure: suppression (switching costs) prevents exit; extraction (pricing power, service lock) grows over time; coordination function (platform reliability, managed services) is secondary to lock-in function.
constraint_indexing:constraint_classification(cloud_infrastructure_lock_in, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cloud_infrastructure_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cloud_infrastructure_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cloud_infrastructure_lock_in, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cloud_infrastructure_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cloud_infrastructure_lock_in, TR),
    TR >= 0.70.

:- end_tests(cloud_infrastructure_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High and rising. The constraint demonstrates clear extraction dynamics: cloud providers maintain pricing power despite ostensible competition, proprietary services grow faster than customer demand justifies, switching costs are intentionally maintained high through API incompatibility. The rising trajectory (0.32→0.58) reflects accumulation of proprietary service integration and data gravity over a 10-year period. Initial cloud adoption appeared as coordination (competitive, flexible) but reveals extraction mechanism as workloads mature and data accumulates. Suppression (0.62): High. Exit barriers include: data transfer costs (often 25-50% of annual cloud spend), proprietary API retraining (2-6 months per team), custom integration rewrites (significant engineering cost), regulatory/compliance holdback (depends on data classification), and organizational switching fatigue (multiple failed migration attempts). Switching costs are irreversible once a certain scale is reached — the constraint intentionally crosses a threshold. Theater ratio (0.48): Moderate. Cloud services present as optimized, transparent, and technically justified. The lock-in mechanism is real (data gravity, API incompatibility) but benefits from narrative framing as 'optimization' rather than extraction. Competing explanations (proprietary services improve security/performance vs. proprietary services lock in customers) coexist.
 *
 * PERSPECTIVAL GAP:
 *   The constraint shows maximum disagreement because beneficiary and victim occupy opposite structural positions with respect to the same mechanism. For providers, the proprietary service ecosystem and lock-in dynamics are genuine coordination solutions (managing complexity, ensuring reliability, building sustainable platforms). For customers, the identical mechanism is pure extraction (pricing power, forced upgrade paths, inability to exit). The gap reveals that 'coordination' and 'extraction' are not properties of the mechanism itself but of the agent's structural relationship to it. From the beneficiary's perspective, suppression is acceptable because they are not suppressed. From the victim's perspective, suppression is the entire mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by the agent's beneficiary/victim status and exit options. Cloud providers experience d ≈ 0.05 (beneficiary + arbitrage) → negative f(d) → they see rope. Enterprises with trapped exit experience d ≈ 0.95 (victim + trapped) → high f(d) → they see snare. Organized multi-cloud coalitions experience d ≈ 0.50 (partially victim, partially coordinating) + constrained exit → moderate f(d) → they see tangled rope with lower experienced extraction than powerless agents. The directionality derivation is automatic: the engine computes d from the beneficiary/victim declarations and exit options, then applies the sigmoid to produce experienced extractiveness. No overrides are needed — the beneficiary/victim structure fully explains the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES THROUGH PERSPECTIVAL ANALYSIS: The mandatrophy (is this coordination or extraction?) is resolved by recognizing that BOTH are structurally true. Cloud lock-in genuinely solves coordination problems (platform reliability, service integration, data consistency) — the rope perspective is empirically accurate. Cloud lock-in genuinely extracts from customers (captures pricing power, prevents exit, grows switching costs) — the snare perspective is empirically accurate. The tangled_rope classification captures this hybrid: the constraint requires active enforcement (cloud providers actively expand proprietary services, maintain incompatible APIs), has genuine beneficiaries (providers and their developer partners), has genuine victims (locked-in customers and the competitive market as a whole). The mandatrophy is resolved by rejecting the false binary: the constraint is BOTH coordination AND extraction, operating at different time scales. At immediate/biographical scale, it is extraction (snare). At generational scale with active multi-cloud work, it is coordination with extraction (tangled rope). At civilizational scale from the analytical perspective, it reveals pure extraction (snare) because competitive alternatives have failed to materialize despite decades of effort.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_gravity_quantification,
    'How much of the measured lock-in is due to pure data transfer costs vs. integration costs vs. operational retraining?',
    'Cost decomposition analysis of actual migrations; tracking of failed vs. successful multi-cloud transitions; measurement of exit cost trajectories across customer cohorts',
    'If data gravity dominant (>60%): lock-in is cost-driven, potentially addressable by reduced transfer fees. If integration costs dominant (>50%): lock-in is architecture-driven, requires fundamental API compatibility work. If retraining costs dominant: cultural/organizational factors dominate, harder to address structurally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_gravity_quantification, empirical, 'Decomposition of lock-in costs by type').

omega_variable(
    multi_cloud_adoption_ceiling,
    'What is the structural ceiling on successful multi-cloud adoption? Is it constrained by orchestration maturity, provider incompatibility, or organizational capability?',
    'Longitudinal tracking of enterprises with declared multi-cloud strategies; measurement of workload portability rates; analysis of abandonment trajectories for multi-cloud projects',
    'If ceiling < 30% of workloads portable: multi-cloud is theater, scaffold sunset is aspirational. If ceiling > 70% portable: portability is achievable, true competition becomes viable. Determines whether generational timescale for Scaffold is realistic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_cloud_adoption_ceiling, empirical, 'Effective ceiling on multi-cloud adoption').

omega_variable(
    proprietary_service_expansion_rate,
    'Are cloud providers consciously expanding proprietary (non-standard) managed services faster than open-source alternatives can match?',
    'Comparative growth rates of proprietary vs. open services in cloud provider portfolios; measurement of customer migration toward proprietary services; analysis of feature parity between open and proprietary alternatives',
    'If expansion accelerating: lock-in mechanism is active and structural. If plateauing: lock-in may be entering maintenance/piton phase. Determines whether extraction pressures are growing or stabilizing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proprietary_service_expansion_rate, empirical, 'Rate of proprietary service expansion by cloud providers').

omega_variable(
    regulatory_relief_effectiveness,
    'Can data residency regulations, EU cloud acts, or anti-lock-in legislation meaningfully reduce switching costs or restore market competition?',
    'Comparative analysis of cloud market concentration pre/post-regulation; measurement of switching rates in regulated vs. unregulated jurisdictions; analysis of compliance cost impact on new entrants',
    'If regulations effective: extraction is structural but not immutable — policy can address. If ineffective: lock-in persists despite intervention (suggests deeper technical or economic roots). Determines whether tangled_rope classification persists or shifts to snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_relief_effectiveness, empirical, 'Effectiveness of regulatory intervention in reducing lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cloud_infrastructure_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cloud_tr_t0, cloud_infrastructure_lock_in, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cloud_tr_t5, cloud_infrastructure_lock_in, theater_ratio, 5, 0.42).
narrative_ontology:measurement(cloud_tr_t10, cloud_infrastructure_lock_in, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(cloud_be_t0, cloud_infrastructure_lock_in, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cloud_be_t5, cloud_infrastructure_lock_in, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cloud_be_t10, cloud_infrastructure_lock_in, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cloud_infrastructure_lock_in, global_infrastructure).
narrative_ontology:affects_constraint(cloud_infrastructure_lock_in, vendor_lock_in_software).
narrative_ontology:affects_constraint(cloud_infrastructure_lock_in, data_portability_standards).
narrative_ontology:affects_constraint(cloud_infrastructure_lock_in, multi_cloud_orchestration).

% DUAL FORMULATION NOTE:
% Cloud lock-in decomposes into three structurally distinct constraints: (1) API-level lock-in (proprietary service ecosystems, non-standard implementations), (2) data-level lock-in (data gravity, transfer costs, residency regulations), (3) organizational lock-in (switching costs, knowledge lock, path dependency). This story focuses on the aggregate constraint; the three substories have different ε values and different sunset timelines.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
