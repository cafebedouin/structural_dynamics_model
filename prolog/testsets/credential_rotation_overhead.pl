% ============================================================================
% CONSTRAINT STORY: credential_rotation_overhead
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credential_rotation_overhead, []).

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
 *   constraint_id: credential_rotation_overhead
 *   human_readable: Credential Rotation Overhead in Security Infrastructure
 *   domain: cybersecurity/systems_operations
 *
 * SUMMARY:
 *   Credential rotation overhead represents a structural constraint embedded
 *   in security operations: the requirement to regularly replace
 *   cryptographic secrets (API keys, passwords, certificates, database
 *   credentials) to maintain assurance bounds. This constraint exhibits the
 *   full range of DR types depending on observational position. From the
 *   security compliance apparatus, rotation is pure coordination (Rope) —
 *   enabling risk quantification and regulatory alignment. From embedded
 *   systems operators, it is extraction (Snare) — forced participation in
 *   rotations that correlate weakly with actual breach prevention. From
 *   platform teams, it is mixed coordination and extraction (Tangled Rope) —
 *   genuine security benefits alongside asymmetric operational burden. From
 *   infrastructure modernization architects, it is temporary (Scaffold) —
 *   zero-trust and hardware-backed secrets will eliminate the requirement
 *   within 5-10 years. From institutional perspective, the standard is
 *   degraded (Piton) — compliance frameworks treat rotation as hygiene
 *   despite ambiguous causation link to breach prevention. From formal
 *   cryptography, it appears as an immutable law (Mountain) — but this
 *   naturalizes a contingent implementation choice. The constraint's
 *   extractiveness has increased over 6 years as organizational scale and
 *   microservice architecture complexity have multiplied rotation
 *   coordination costs while security benefits remain poorly measured.
 *   Theater ratio has risen as compliance requirements drive rotations
 *   disconnected from threat modeling.
 *
 * KEY AGENTS:
 *   - Systems Operators and SREs: Primary victim (powerless/trapped) — absorb manual coordination costs, on-call exposure, deployment disruption; cannot opt out without institutional violation
 *   - Platform and Development Teams: Secondary victim (moderate/constrained) — face deployment coordination costs and test disruption; experience both security benefits and operational overhead
 *   - Security and Compliance Functions: Primary beneficiary (institutional/arbitrage) — capture institutional authority, resource allocation, and regulatory alignment through rotation mandate; experience rotation as coordination, not extraction
 *   - Infrastructure Modernization Coalition: Organized actors (organized/constrained) — zero-trust advocates and hardware security promoters building alternative architectures; see sunset pathway through technical maturity
 *   - Legacy Security Standards (CIS Benchmarks, NIST Guidelines): Institutional framework (institutional/arbitrage) — maintains rotation recommendation; propagates as compliance requirement through governance structures
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent architectural choices as immutable cryptographic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credential_rotation_overhead, 0.38).
domain_priors:suppression_score(credential_rotation_overhead, 0.52).
domain_priors:theater_ratio(credential_rotation_overhead, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credential_rotation_overhead, extractiveness, 0.38).
narrative_ontology:constraint_metric(credential_rotation_overhead, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(credential_rotation_overhead, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credential_rotation_overhead, tangled_rope).
narrative_ontology:human_readable(credential_rotation_overhead, "Credential Rotation Overhead in Security Infrastructure").
narrative_ontology:topic_domain(credential_rotation_overhead, "cybersecurity/systems_operations").

domain_priors:requires_active_enforcement(credential_rotation_overhead).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credential_rotation_overhead, security_apparatus).
narrative_ontology:constraint_beneficiary(credential_rotation_overhead, credential_issuers).
narrative_ontology:constraint_victim(credential_rotation_overhead, operational_efficiency).
narrative_ontology:constraint_victim(credential_rotation_overhead, human_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMBEDDED OPERATOR (SNARE) — Systems administrators and operators cannot exit credential rotation requirements. They bear full operational cost: manual secret updates, downtime windows, on-call exposure, and accumulated cognitive load. No alternative exit exists without institutional violation. The constraint extracts operational surplus through forced participation in rotations that do not correlate with actual breach risk reduction.
constraint_indexing:constraint_classification(credential_rotation_overhead, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PLATFORM TEAM (TANGLED ROPE) — Development teams benefit from credential rotation infrastructure (reduced breach surface, recovery procedures, automated tools), but bear significant costs (deployment disruption, coordinated updates across microservices, test suite failures). Extraction and coordination coexist: genuine security coordination function exists alongside asymmetric operational burden distribution.
constraint_indexing:constraint_classification(credential_rotation_overhead, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SECURITY COMPLIANCE FUNCTION (ROPE) — Compliance and security teams experience credential rotation as pure coordination. Rotations enable risk quantification, audit trails, and regulatory alignment. No extraction experienced — the constraint solves the collective action problem of credential lifecycle management. Net beneficiary through institutional authority and resource allocation.
constraint_indexing:constraint_classification(credential_rotation_overhead, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INFRASTRUCTURE MODERNIZATION COALITION (SCAFFOLD) — Organized technical leaders (zero-trust advocates, hardware security module promoters, certificate automation standardizers) see credential rotation as a temporary coordination failure with a clear sunset. Hardware-backed secrets, mutable cryptographic bindings, and automated certificate renewal are building alternative pathways that reduce manual rotation overhead within 5-10 years. The scaffold has a genuine exit path: infrastructure maturity makes manual rotation obsolete.
constraint_indexing:constraint_classification(credential_rotation_overhead, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY SECURITY STANDARD (PITON) — Credential rotation as a security control is largely performative by civilizational standards. From a zero-trust architecture perspective, credential rotation addresses a problem that better-designed systems (immutable identities, cryptographic binding to hardware, ambient authority) do not generate. The standard persists through institutional inertia: compliance frameworks, audit checklists, and CIS benchmarks treat rotation as hygiene, not as response to specific threat models. Theater ratio reflects the gap between rotation frequency recommendations (90 days) and actual breach causation analysis (most breaches involve compromised credentials aged < 2 weeks or persisted > 180 days).
constraint_indexing:constraint_classification(credential_rotation_overhead, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a formal cryptography perspective, credential rotation addresses an immutable constraint: secrets have finite lifespans due to replay vulnerability, key derivation limits, and backward secrecy. This perspective sees rotation as a law of cryptographic physics — unavoidable overhead of maintaining cryptographic assurance bounds. However, this naturalizes a contingent implementation choice. Hardware-backed secrets and cryptographic binding mechanisms can extend this 'natural' limit substantially, suggesting the mountain classification is false — the overhead is institutional architecture, not physics.
constraint_indexing:constraint_classification(credential_rotation_overhead, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credential_rotation_overhead_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credential_rotation_overhead, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credential_rotation_overhead, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(credential_rotation_overhead, TR),
    TR >= 0.70.

:- end_tests(credential_rotation_overhead_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts operational effort and cognitive load from operators through forced participation in rotations that achieve uncertain security benefit. However, extraction is not severe because genuine security coordination exists (breach surface reduction, recovery procedures) and some rotations do prevent specific attack vectors (credential replay). The value reflects that extractiveness is real but contested — beneficiaries and victims disagree on whether the overhead is justified. Suppression (0.52): Moderate-high. Operators face significant barriers to exit: institutional compliance requirements, integration into audit frameworks, regulatory mandates, and architectural integration into secret management systems. But suppression is not total — operators can constrain rotation frequency and automate portions of the process, reducing (but not eliminating) overhead. Theater ratio (0.58): Moderate-high. Credential rotation as practiced exhibits substantial performative content: 90-day rotation frequency is recommended by compliance standards but lacks strong empirical justification for breach prevention. Rotation audits focus on compliance (did you rotate?) rather than security (did rotation prevent anything?). However, the theater is not dominant — genuine security coordination exists (key versioning, recovery procedures, attack surface reduction).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence driven by institutional position, not by disagreement about facts. All observers acknowledge that credential rotation has both coordination (breach surface reduction) and extraction (operational overhead) components. The disagreement is whether coordination benefit justifies extraction cost. Security compliance sees high coordination value (regulatory alignment, risk quantification). Operators see low coordination value (most breaches exploit non-rotated credentials or vectors unrelated to credential age). Platform teams see mixed value (some benefits, significant costs). The gap is not resolvable by better information — it reflects genuine structural divergence: the beneficiary and victim have different objective functions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (security_apparatus, credential_issuers) occupy institutional power with arbitrage exit options — they can shift rotation costs to operators and maintain regulatory authority. Their directionality is low (d ≈ 0.15-0.20): they benefit from the constraint, experiencing it as coordination. Victims (operational_efficiency, human_operators) occupy powerless or moderate power with trapped or constrained exit options — they absorb coordination costs without institutional authority to reframe or escape. Their directionality is high (d ≈ 0.80-0.90): they experience the constraint as extraction. The asymmetry is structural: compliance functions control the mandate; operators execute it. Extraction runs from operators toward compliance apparatus.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through perspectival specificity. The question 'Is credential rotation a coordination mechanism or extraction?' has no global answer. From the security apparatus position, it is coordination (Rope). From the operator position, it is extraction (Snare). The Tangled Rope classification at the moderate/constrained level correctly captures the hybrid — platform teams genuinely benefit from the coordination function AND genuinely bear extraction costs. The Scaffold classification reveals a structural resolution: infrastructure maturity (zero-trust, hardware-backed secrets) will eliminate the overhead while preserving security benefits, converting extraction into pure coordination. The Piton classification flags that compliance-driven rotation (detached from threat modeling) is performative — the standard persists through inertia, not function. The false Mountain classification at the civilizational level reveals the trap: 'credential rotation is necessary' naturalizes a specific implementation choice (time-based rotation) rather than the underlying security need (cryptographic assurance bounds). The constraint story resolves mandatrophy by showing that all six types are correct — they measure different aspects of the same structural phenomenon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rotation_frequency_threshold,
    'What rotation frequency actually correlates with measured breach prevention? Do 90-day rotations prevent more breaches than 180-day rotations?',
    'Breach causation analysis: identify breaches caused by aged credentials; correlate breach credential age with organization rotation policies; control for detection latency and credential exposure vector',
    'If 90-day rotations prevent measurably more breaches: extraction is justified (current overhead is reasonable cost of security). If correlation is weak or absent: overhead is theater without security justification, reclassifying the constraint as higher suppression + lower actual security benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rotation_frequency_threshold, empirical, 'Correlation between rotation frequency and breach prevention').

omega_variable(
    alternative_architecture_feasibility,
    'Can zero-trust architectures with hardware-backed secrets and immutable identity binding eliminate credential rotation without security degradation?',
    'Technical feasibility analysis of zero-trust migration; cost-benefit comparison of hardware security module deployment vs. rotation overhead; timeline estimation for architectural transition viability',
    'If feasible within 5-10 years: scaffold perspective confirmed, sunset is structural. If not feasible: rotation is a permanent architectural constraint, mountain perspective gains credibility, overhead becomes accepted baseline cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_architecture_feasibility, empirical, 'Feasibility of zero-trust architecture as sunset pathway').

omega_variable(
    compliance_extraction_mechanism,
    'Does credential rotation requirement primarily serve security assurance or does it function as a compliance audit theater to demonstrate due diligence?',
    'Audit log analysis: measure what percentage of discovered security issues trace to aged-credential exploitation vs. other vectors; correlation between rotation compliance scores and actual breach incidence; documentation review of breach post-mortems',
    'If security-driven: extraction is justified, beneficiaries and victims both gain. If compliance-driven: theater ratio increases, constraint reclassifies toward higher piton characteristics, suppression increases (operators forced to rotate for audit appearance, not security effect).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_extraction_mechanism, empirical, 'Whether rotation requirement serves security or compliance audit theater').

omega_variable(
    cognitive_load_accumulation,
    'Does recurring credential rotation create persistent cognitive load that reduces operator effectiveness in other security domains?',
    'Operator survey data on cognitive load; measurement of security incident response time correlation with rotation cycle phase; analysis of operator error rates in security protocols before, during, and after rotation windows',
    'If significant cognitive load: suppression should increase (operators cannot escape mental overhead), extraction increases. If negligible: current suppression estimate (0.52) is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_load_accumulation, empirical, 'Cognitive load persistence of credential rotation cycles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credential_rotation_overhead, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cro_tr_t0, credential_rotation_overhead, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cro_tr_t3, credential_rotation_overhead, theater_ratio, 3, 0.51).
narrative_ontology:measurement(cro_tr_t6, credential_rotation_overhead, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(cro_be_t0, credential_rotation_overhead, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cro_be_t3, credential_rotation_overhead, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(cro_be_t6, credential_rotation_overhead, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credential_rotation_overhead, enforcement_mechanism).
narrative_ontology:affects_constraint(credential_rotation_overhead, secret_management_automation).
narrative_ontology:affects_constraint(credential_rotation_overhead, zero_trust_architecture_adoption).

% DUAL FORMULATION NOTE:
% Credential rotation overhead is downstream of the security assurance requirement but represents a distinct structural constraint. The upstream constraint (cryptographic assurance bounds) has lower extractiveness; the rotation overhead constraint has higher extractiveness reflecting implementation-specific costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
