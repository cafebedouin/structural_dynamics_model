% ============================================================================
% CONSTRAINT STORY: invisible_infrastructure_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_invisible_infrastructure_dependency, []).

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
 *   constraint_id: invisible_infrastructure_dependency
 *   human_readable: The Submerged Substrate Trap
 *   domain: technological/logistical/economic
 *
 * SUMMARY:
 *   The submerged substrate trap describes the phenomenon wherein a critical
 *   technological or logistical infrastructure becomes so deeply embedded in
 *   the operational foundation of dependent organizations that its presence
 *   is rendered invisible — both literally (it does not appear in
 *   organizational awareness or decision-making) and economically (the cost
 *   of dependence is not transparently charged). Classic examples include
 *   cloud computing platforms (AWS, Google Cloud, Azure), open-source core
 *   libraries (npm, Python packages, Kubernetes), global logistics networks
 *   (shipping containers, port infrastructure, just-in-time supply), payment
 *   systems, and DNS/CDN infrastructure. The trap operates through several
 *   mechanisms: (1) Early coordination benefit — the infrastructure reduces
 *   operational friction and enables rapid scaling; (2) Architectural
 *   embedding — dependent systems integrate the substrate into their core
 *   logic, making reversal architecturally expensive; (3) Visibility collapse
 *   — once the infrastructure works reliably, operators stop perceiving it as
 *   a separate system and treat it as a natural law of their operational
 *   environment; (4) Lock-in emergence — by the time dependency is
 *   recognized, switching costs are prohibitive; (5) Extraction phase — the
 *   infrastructure provider, now insulated by lock-in, can increase prices,
 *   reduce service quality, impose terms of service changes, or extract data
 *   value with minimal customer defection risk. The constraint exhibits all
 *   six DR types from different structural positions, revealing the
 *   perspectival nature of infrastructure dependency.
 *
 * KEY AGENTS:
 *   - Infrastructure Provider: Primary beneficiary (institutional/arbitrage) — captures switching-cost rent and network effect monopoly; experiences constraint as low-cost coordination with arbitrage upside
 *   - Locked-In Service Operators: Primary victims (powerless/trapped) — operational systems depend entirely on substrate; cannot exit without complete rebuild; bear full extraction cost
 *   - End Users: Secondary victims (moderate/trapped) — perceive only service quality, not substrate dependency; experience outages as service failures; have no exit option without data loss
 *   - Large Enterprises with Alternatives: Mixed actors (powerful/constrained) — resources to architect multi-provider solutions; experience genuine benefits but also face extraction; can negotiate but remain locked-in
 *   - Alternative Infrastructure Providers: Excluded competitors (powerless/trapped) — cannot enter market due to network effects and switching costs; trapped outside the lock-in ecosystem
 *   - Regulatory Bodies: Institutional gatekeepers (institutional/constrained) — recognize the trap but enforcement is performative; regulation documents risk without preventing lock-in
 *   - Open Standards Coalition: Organized resistance (organized/mobile) — building technical alternatives (federated systems, open APIs, data portability standards) with genuine exit pathways; experience constraint as temporary problem with sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(invisible_infrastructure_dependency, 0.62).
domain_priors:suppression_score(invisible_infrastructure_dependency, 0.68).
domain_priors:theater_ratio(invisible_infrastructure_dependency, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(invisible_infrastructure_dependency, extractiveness, 0.62).
narrative_ontology:constraint_metric(invisible_infrastructure_dependency, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(invisible_infrastructure_dependency, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(invisible_infrastructure_dependency, tangled_rope).
narrative_ontology:human_readable(invisible_infrastructure_dependency, "The Submerged Substrate Trap").
narrative_ontology:topic_domain(invisible_infrastructure_dependency, "technological/logistical/economic").

domain_priors:requires_active_enforcement(invisible_infrastructure_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(invisible_infrastructure_dependency, infrastructure_provider).
narrative_ontology:constraint_beneficiary(invisible_infrastructure_dependency, ecosystem_dependents_initial).
narrative_ontology:constraint_victim(invisible_infrastructure_dependency, downstream_service_operators).
narrative_ontology:constraint_victim(invisible_infrastructure_dependency, end_users).
narrative_ontology:constraint_victim(invisible_infrastructure_dependency, alternative_providers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN SERVICE OPERATOR (SNARE) — Once dependent on the infrastructure, cannot exit without catastrophic operational failure. The service operator's business model is built on the assumption of substrate availability; switching is economically impossible mid-deployment. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈1.03.
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: END USER (SNARE) — Does not perceive the substrate; outages appear as service failure, not infrastructure dependency. Cannot switch providers without losing accumulated data, configuration, or integration. Suppression is extreme: users have no visibility into substrate risk. d≈0.88, f(d)≈1.32, σ=1.2 → χ≈0.97.
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LARGE ENTERPRISE WITH ALTERNATIVES (TANGLED ROPE) — Powerful organizations with resources to architect multi-provider solutions experience the constraint as tangled: genuine benefits from economies of scale (coordination function) but also locked into specific substrate features (extraction). Exit is expensive but possible; creates negotiating power but requires constant architectural work. d≈0.60, f(d)≈0.73, σ=1.0 → χ≈0.45.
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INFRASTRUCTURE PROVIDER (ROPE) — Sees the substrate as a coordination mechanism: enabling ecosystem growth, reducing operational friction for users, creating network effects. Provider can easily exit (redirect infrastructure, sunset service, migrate users) but chooses not to because lock-in is profitable. Experiences the constraint as low-cost coordination with arbitrage upside. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Negative chi = net beneficiary.
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY BODY (PITON) — Recognizes the substrate trap as a critical risk (market concentration, systemic fragility) but enforcement is performative: compliance testing, auditing, and reporting exist, yet dependencies persist and actually deepen. Regulation maintains the appearance of oversight without disrupting the profitable lock-in. theater_ratio=0.55 reflects that regulatory interventions are substantially theater (they do not prevent lock-in, only document it). d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.36.
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ALTERNATIVE PROVIDERS (SNARE) — New entrants cannot compete because switching costs for ecosystem dependents are prohibitive. Network effects and lock-in create winner-take-most dynamics. Alternative providers are effectively excluded from the market, not through technical superiority but through substrate entrenchment. d≈0.94, f(d)≈1.42, σ=1.2 → χ≈1.05.
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a sufficiently abstracted level, infrastructure lock-in appears to be an inevitable consequence of complex systems: any substrate that works well enough becomes indispensable; dependency is a law of technological progress. However, the high extractiveness (0.62), suppression (0.68), and active enforcement requirement contradict the mountain gates. This is a false summit: what appears natural is actually contingent on policy choices (intellectual property, data portability rules, interoperability standards, antitrust enforcement).
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: OPEN STANDARDS COALITION (SCAFFOLD) — Organized actors (open-source communities, standards bodies, data-portability advocates) experience the substrate trap as a temporary coordination failure with an exit pathway: open APIs, containerization, federated architectures, and data export standards are building alternatives to substrate lock-in. Exit options improve over time as standards mature. theater_ratio would be low for this perspective (~0.35) because the technical mechanisms (containerization, APIs) have genuine function, not just appearance. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.24.
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(invisible_infrastructure_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(invisible_infrastructure_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(invisible_infrastructure_dependency, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(invisible_infrastructure_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(invisible_infrastructure_dependency, TR),
    TR >= 0.70.

:- end_tests(invisible_infrastructure_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High but not extreme. The infrastructure provider captures significant switching-cost rent and can extract through price increases, service degradation, or policy changes. However, extractiveness is moderated by (a) competitive threat from open standards and federation (reducing effective extraction to below 0.70), (b) customer perception of alternatives (even if switching is expensive), and (c) reputational risk if extraction becomes visible. The initial extractiveness (0.35) reflects the honest coordination phase; the rise to 0.62 reflects lock-in accumulation over 10 time periods. Suppression (0.68): High. Operators are systematically prevented from perceiving the substrate as a separate system; visibility collapse is the mechanism. Contractual terms often prevent competitive benchmarking or public disclosure of costs. Technical complexity obscures the true scope of dependency. Switching is suppressed by architecture, data format lock-in, and sunk costs. Theater ratio (0.55): Moderate. Regulatory oversight and industry standards bodies exist (compliance testing, audits, service-level agreements), but these provide the appearance of control without preventing lock-in. SLAs guarantee uptime but not portability. Audits verify provider compliance but not alternative feasibility. The theater has increased from 0.38 to 0.55 as regulatory pressure has grown, forcing providers to adopt performative compliance mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a stark perspectival divide. The infrastructure provider (Rope) experiences genuine coordination benefit and low extraction; they are solving a real problem and collecting modest arbitrage profits. Locked-in operators and end users (Snare) experience pure extraction; they have no perception of the substrate as a separate system and no ability to exit. Large enterprises (Tangled Rope) occupy the middle: they perceive both coordination and extraction but have negotiating power. Regulators (Piton) maintain performative oversight without disrupting the profitable arrangement. Alternative providers (Snare) are excluded entirely by network effects. The open standards coalition (Scaffold) sees the trap as temporary and builds technical exit pathways. The analytical observer risks naturalizing the lock-in as an inevitable law of technology (Mountain), but the high extractiveness and suppression metrics reveal it as contingent on policy and design choices. The perspectival gap is maximal between the beneficiary's rope and the victims' snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Infrastructure provider: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; experiences low-cost coordination. Locked-in service operators: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit options. End users: Victim + trapped → d≈0.88, f(d)≈1.32. High extraction; visibility collapse prevents perception of substrate as separate system. Large enterprises: Mixed + constrained → d≈0.60, f(d)≈0.73. Significant extraction but constrained rather than trapped; have alternative architectures but remain locked-in for operational reasons. Alternative providers: Victim (excluded) + trapped → d≈0.94, f(d)≈1.42. Maximum extraction through exclusion; trapped outside the lock-in ecosystem. Regulatory bodies: Observer + constrained → d≈0.50, f(d)≈0.65. Intermediate; recognize the trap but enforcement is constrained by political and economic pressures. Open standards coalition: Organized + mobile → d≈0.35, f(d)≈0.32. Low effective extraction; mobile exit options and organized agency reduce chi.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy by clarifying the dual nature of infrastructure dependency. (1) The initial coordination benefit is genuine (Rope perspective) — the infrastructure reduces friction and enables scaling that would be impossible without it. (2) The lock-in that emerges is also genuine (Snare perspective) — operators become unable to exit without catastrophic cost. These are not contradictory; they are sequential phases. The constraint is tangled because both coordination and extraction are structurally present. The error would be to classify it as pure Rope (ignoring the extraction trap) or pure Snare (ignoring the legitimate coordination benefit). The tangled rope classification accurately captures that the constraint provides real value while simultaneously imposing real costs that exceed the value provided to trapped operators. The theater ratio (0.55) reveals that regulatory responses are largely performative — they create the appearance of control without enabling actual exit or reducing extraction. The scaffold perspective (open standards coalition) is crucial because it shows that mandatrophy can be resolved by building genuine alternatives: if federated systems, containerization, and data portability standards mature, the constraint's extraction mechanism weakens (effectively becoming a Rope or Scaffold). The mandatrophy is resolved by distinguishing the temporal phases (coordination → lock-in → extraction) and the perspectival phases (provider's coordination, operator's extraction, coalition's exit pathway).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_threshold,
    'At what total switching cost (in USD or operations hours) does a lock-in trap transition from temporary coordination overhead to permanent extraction mechanism?',
    'Economic analysis of historical platform migrations; comparison of post-migration satisfaction and cost reduction across 50+ large-scale transitions; industry benchmarks for break-even switching costs',
    'If threshold < 100k USD: many enterprises could defect but don''t (indicates pure extraction suppression). If threshold > 10M USD: switching is genuinely impossible (indicates structural lock-in, not predatory design).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_threshold, empirical, 'Threshold separating coordination overhead from permanent extraction').

omega_variable(
    data_portability_feasibility,
    'Is true data portability (reversible migration preserving all state and integrations) technically feasible at scale, or does substrate lock-in involve inherent information loss?',
    'Technical case studies of attempted migrations in cloud platforms, SaaS applications, and logistics networks; measurement of data fidelity, integration recreation time, and hidden dependency discovery during migration',
    'If feasible: scaffold perspective is technically valid (exit cost can be reduced to near-zero). If inherently lossy: apparent alternatives are illusory, snare classification is correct for all dependents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_portability_feasibility, empirical, 'Whether true data portability is technically feasible').

omega_variable(
    provider_intentionality_in_lock_in,
    'To what degree is substrate lock-in a deliberate extractive design choice versus an inevitable emergent property of complex systems?',
    'Analysis of design decisions: API versioning practices, data format choices, integration hooks; comparison of intentionally-portable architectures (federated systems, open protocols) vs proprietary lock-in designs; deposition of internal provider documentation revealing design intent',
    'If deliberate: snare classification is correct; extraction is active enforcement. If emergent: tangled rope classification is more accurate; coordination benefit is genuine but creates lock-in as side effect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(provider_intentionality_in_lock_in, conceptual, 'Whether lock-in is intentional design or emergent property').

omega_variable(
    single_point_of_failure_systemic_risk,
    'Does the infrastructure provider''s market concentration create systemic risk sufficient to classify the constraint as a snare even for willing participants?',
    'Network analysis of failure propagation; measurement of economically critical services depending on single provider; historical analysis of provider outages and cascade effects; game-theoretic modeling of provider incentives during systemic crises',
    'If high systemic risk: snare classification justified even for beneficiaries (extraction occurs through risk imposition). If low risk: rope classification more accurate; provider reliability genuinely benefits ecosystem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_point_of_failure_systemic_risk, empirical, 'Whether infrastructure concentration creates systemic risk').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(invisible_infrastructure_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(invinfra_tr_t0, invisible_infrastructure_dependency, theater_ratio, 0, 0.38).
narrative_ontology:measurement(invinfra_tr_t5, invisible_infrastructure_dependency, theater_ratio, 5, 0.47).
narrative_ontology:measurement(invinfra_tr_t10, invisible_infrastructure_dependency, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(invinfra_be_t0, invisible_infrastructure_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(invinfra_be_t5, invisible_infrastructure_dependency, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(invinfra_be_t10, invisible_infrastructure_dependency, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(invisible_infrastructure_dependency, global_infrastructure).
narrative_ontology:affects_constraint(invisible_infrastructure_dependency, cloud_provider_market_concentration).
narrative_ontology:affects_constraint(invisible_infrastructure_dependency, open_source_supply_chain_dependency).
narrative_ontology:affects_constraint(invisible_infrastructure_dependency, just_in_time_logistics_fragility).

% DUAL FORMULATION NOTE:
% The submerged substrate trap decomposes into domain-specific instantiations: cloud infrastructure (AWS/Azure/GCP), core libraries (npm/PyPI), logistics networks, payment systems, and CDN/DNS. Each instantiation has its own ε value reflecting domain-specific exit costs and network effects. Cloud infrastructure has higher ε (~0.65) due to architectural lock-in; open-source libraries have lower ε (~0.45) due to easier forking and alternatives; logistics has higher ε (~0.70) due to sunk infrastructure. All are linked by the common structural mechanism: visibility collapse + switching cost + network effects → lock-in → extraction. This story models the generic mechanism; domain-specific stories detail the ε values for each instantiation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(invisible_infrastructure_dependency, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
