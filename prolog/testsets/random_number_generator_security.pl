% ============================================================================
% CONSTRAINT STORY: random_number_generator_security
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_random_number_generator_security, []).

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
 *   constraint_id: random_number_generator_security
 *   human_readable: Random Number Generator Security Constraint
 *   domain: cryptography/computer_security
 *
 * SUMMARY:
 *   Random number generator security represents a structural constraint where
 *   dependence on cryptographic randomness creates asymmetric information
 *   between RNG architects (who understand entropy sources, algorithm design,
 *   implementation details) and users (who cannot verify quality without
 *   specialized expertise). The constraint combines elements of coordination
 *   (RNG design genuinely solves the problem of scaling deterministic systems
 *   to produce unpredictable outputs) and extraction (complexity and
 *   information asymmetry enable capture and lock-in). The increasing
 *   extractiveness trajectory reflects: (1) growing dependence on
 *   cryptographic systems across all digital infrastructure, (2)
 *   concentration of RNG expertise in smaller populations, (3) emergence of
 *   new attack surfaces (side-channel attacks, weak entropy sources) that
 *   users cannot detect independently, (4) regulatory capture where standards
 *   bodies incorporate assumptions favoring incumbent suppliers. The theater
 *   ratio indicates that verification activity (statistical testing,
 *   auditing, certification) is partially performative — tests may pass while
 *   cryptographically relevant bias remains undetected.
 *
 * KEY AGENTS:
 *   - Cryptographic System Users: Primary victims (powerless/trapped) — depend on RNG quality without ability to verify; cannot exit without abandoning cryptography
 *   - Information Security Infrastructure: Collective victim (powerless/trapped) — systemic dependence on RNG correctness across all deployed systems; coordinated failure mode
 *   - RNG Implementation Architects: Primary beneficiary (powerful/mobile) — control technical specification; authority asymmetry enables extraction through complexity and lock-in
 *   - Security Standards Bodies: Coordinating institution (organized/constrained) — NIST, ISO provide genuine coordination function with modest extraction
 *   - Post-Quantum Cryptography Transition: Organized exit pathway (organized/constrained) — building alternative RNG designs and entropy sources that may bypass legacy constraints
 *   - Legacy Hardware RNG: Institutional residue (institutional/arbitrage) — older implementations persist through regulatory inertia despite superior alternatives
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent architectural choices as inherent cryptographic limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(random_number_generator_security, 0.62).
domain_priors:suppression_score(random_number_generator_security, 0.68).
domain_priors:theater_ratio(random_number_generator_security, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(random_number_generator_security, extractiveness, 0.62).
narrative_ontology:constraint_metric(random_number_generator_security, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(random_number_generator_security, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(random_number_generator_security, snare).
narrative_ontology:human_readable(random_number_generator_security, "Random Number Generator Security Constraint").
narrative_ontology:topic_domain(random_number_generator_security, "cryptography/computer_security").

domain_priors:requires_active_enforcement(random_number_generator_security).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(random_number_generator_security, rng_implementation_architects).
narrative_ontology:constraint_victim(random_number_generator_security, cryptographic_system_users).
narrative_ontology:constraint_victim(random_number_generator_security, information_security_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CRYPTOGRAPHIC SYSTEM USER (SNARE) — End users depend on RNG quality without ability to verify it. They cannot audit entropy sources, test statistical properties, or detect bias without specialized expertise. Trapped by dependence on upstream security assumptions they cannot inspect or exit. Maximum extraction: their security posture is compromised by RNG flaws they cannot detect or escape.
constraint_indexing:constraint_classification(random_number_generator_security, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INFORMATION SECURITY INFRASTRUCTURE (SNARE) — The entire cryptographic ecosystem rests on RNG quality. Systemic weakness in RNG functions (hardware/software implementation, entropy sources, seeding mechanisms) compromises all dependent systems simultaneously. No alternative exists at infrastructure scale. Trapped in absolute dependence. The collective cannot exit — must trust the RNG implementation or abandon cryptography entirely.
constraint_indexing:constraint_classification(random_number_generator_security, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: RNG IMPLEMENTATION ARCHITECTS (TANGLED ROPE) — Designers of RNG systems benefit from the constraint through: (a) concentration of expertise authority (only specialists can verify correctness), (b) lock-in effects (switching RNG implementations is costly), (c) plausible deniability (RNG failure blamed on entropy sources, not design). Also provide genuine coordination function: designing algorithms that minimize extraction from entropy sources. Asymmetric benefit despite coordination contribution.
constraint_indexing:constraint_classification(random_number_generator_security, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: SECURITY STANDARDS BODIES (ROPE) — NIST, ISO, international bodies provide coordination function (standardizing RNG algorithms, requiring testing procedures, establishing entropy requirements) with minimal extraction. Their role is genuinely coordinating field activity. Exit is constrained by regulatory obligation and institutional legitimacy, but the coordination benefits justify the constraint. Low extractiveness despite institutional power.
constraint_indexing:constraint_classification(random_number_generator_security, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-QUANTUM CRYPTOGRAPHY TRANSITION (SCAFFOLD) — Organized effort to migrate cryptographic infrastructure to quantum-resistant algorithms creates alternative RNG pathways. The constraint is temporary: quantum-resistant standards (lattice-based, hash-based) require different entropy properties and may bypass legacy RNG vulnerabilities. Sunset clause: as post-quantum adoption matures (estimated 10-15 years), legacy RNG constraints lose force. High suppression acceptable because exit path exists and is being built.
constraint_indexing:constraint_classification(random_number_generator_security, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY HARDWARE RNG IMPLEMENTATIONS (PITON) — Older RNG designs (thermal noise-based, oscillator-based) persist in systems long after superior alternatives emerge, maintained through institutional inertia. The original functions are largely degraded: cheaper entropy sources now available, algorithms improved, but older implementations remain embedded in deployed systems. Theater ratio high because the RNG persists for regulatory compliance rather than effectiveness. Exit is low-cost (replace with modern implementation) but rarely executed.
constraint_indexing:constraint_classification(random_number_generator_security, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some degree of entropy verification requirement is inherent to cryptography: any cryptographic system requires indistinguishable randomness, and verifying indistinguishability requires statistical testing against observable patterns. The irreducibility appears as a natural law: cryptographic security fundamentally depends on unpredictability, which cannot be proven, only tested probabilistically. The engine will detect this as a false summit — the 'unprovability' is mathematical, not institutional, but is often naturalized to justify weak empirical verification standards.
constraint_indexing:constraint_classification(random_number_generator_security, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(random_number_generator_security_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(random_number_generator_security, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(random_number_generator_security, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(random_number_generator_security, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(random_number_generator_security, TR),
    TR >= 0.70.

:- end_tests(random_number_generator_security_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. RNG architects benefit from: (a) expertise asymmetry (only specialists understand entropy sources, algorithm design, implementation security), (b) switching costs (replacing RNG implementations requires full system redesign), (c) plausible deniability (failures can be attributed to entropy sources rather than design), (d) regulatory leverage (RNG certification creates lock-in). However, extraction is not total because: (1) RNG design does solve genuine coordination problems (scaling from physical entropy to usable randomness), (2) standards bodies maintain oversight, (3) open-source implementations partially reduce information asymmetry. The trajectory from 0.45 to 0.62 reflects increasing extraction as digital systems scale — broader dependence makes RNG lock-in more valuable. Suppression (0.68): High. Barriers to exit are substantial: (1) no alternative to RNG for cryptographic systems, (2) entropy verification is mathematically non-trivial, (3) implementation flaws are latent (side-channel attacks), (4) expertise barriers prevent independent auditing, (5) regulatory capture (standards favor incumbent suppliers). Theater ratio (0.55): Moderate-high. Verification activity is partly performative: statistical tests (NIST SP 800-22, diehard) may pass while cryptographically relevant bias persists; implementation audits cannot detect all side-channel vulnerabilities; certification processes emphasize procedural compliance over empirical verification. The increasing theater trajectory reflects growing gap between testing procedures and actual cryptanalytic threat models.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how information asymmetry alone can drive maximum perspectival divergence. All perspectives observe the same structural fact: RNG implementation requires unverifiable entropy sources. But their interpretation depends on structural position: (1) Users interpret this as entrapment (Snare) — they cannot verify what they fundamentally depend on. (2) Architects interpret this as legitimate specialization (Tangled Rope) — they provide coordination value for verifying the unverifiable. (3) Standards bodies interpret this as manageable risk (Rope) — coordinated testing and certification reduce verification burden. (4) The transition to post-quantum (Scaffold) — reframes as temporary problem with known exit path. (5) Legacy systems (Piton) — show that the constraint persists through inertia, not necessity. (6) The civilizational view (Mountain) — risks declaring entropy verification fundamentally impossible, naturalizing what is actually an architectural choice. The perspectival gap is not about disagreement on facts but about how structural position determines what facts are salient and how they are weighted.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary identification: RNG architects and their organizations (Intel, Qualcomm, cryptographic library maintainers) are primary beneficiaries — they benefit from expertise asymmetry, lock-in, and regulatory leverage. Victims identification: all users of cryptographic systems and the collective security infrastructure are victims — they depend on RNG quality without ability to verify. Secondary beneficiaries: standards bodies benefit modestly through coordinating role and authority, but their benefit is proportional to genuinely provided coordination value, not extraction. The beneficiary/victim structure drives the directionality computation: beneficiaries with mobile/arbitrage exit options derive low d values (0.15-0.30); victims with trapped exit options derive high d values (0.90+). The result is χ divergence: users experience effective extraction near ceiling; architects experience near-zero or negative effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that RNG security constraint combines genuine coordination function (solving the problem of generating cryptographic-grade randomness from physical entropy) with extractive architecture (concentrating verification authority and creating lock-in). The constraint is NOT purely coordinating (would be Rope) because asymmetric information persists despite standards efforts. It is NOT purely extractive (would be Snare) because RNG design does provide genuine value. The Snare classification from the powerless user perspective is correct — they experience pure extraction. The Tangled Rope classification from architects' perspective is correct — they provide coordination while capturing informational rents. The constraint resolves the mandatrophy by being legitimately different things from different structural positions. The analytical perspective's Mountain view is a false summit — the constraint appears inevitable only if one naturalizes the specific architectural choices (centralized verification, supplier lock-in, opacity of entropy sources) as inherent to cryptography. Alternative architectures (transparent entropy sources, distributed verification, standardized entropy interfaces) could reduce extraction while maintaining coordination, demonstrating that the constraint is not a natural law but a contingent institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    entropy_source_verification_impossibility,
    'Can entropy sources be verified independent of the RNG algorithm, or are entropy and RNG algorithm mutually coupled in verification?',
    'Adversarial testing framework where entropy sources are evaluated in isolation vs integrated with RNG algorithms; identification of algorithm-entropy dependencies',
    'If decoupled: entropy verification is transparent and extraction can be minimized. If coupled: RNG architects can obscure failures through entropy source claims, maintaining high extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(entropy_source_verification_impossibility, empirical, 'Whether entropy sources can be verified independently of RNG algorithm').

omega_variable(
    statistical_test_sufficiency,
    'Do standard statistical tests (NIST SP 800-22, diehard, testU01) actually detect cryptographically relevant bias, or do they miss systematic weaknesses that matter for cryptanalysis?',
    'Post-mortem analysis of RNG failures: correlation between passing statistical tests and actual cryptanalytic exploits; identification of test-resistant bias patterns',
    'If tests are sufficient: RNG security can be empirically verified, reducing asymmetric information extraction. If insufficient: implementation architects maintain privileged knowledge, justifying high suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statistical_test_sufficiency, empirical, 'Whether statistical tests detect cryptographically relevant RNG bias').

omega_variable(
    hardware_entropy_cost_asymmetry,
    'Is the cost differential between high-quality entropy generation and cryptographic-grade RNG design truly required, or do RNG architects inflate complexity to justify lock-in?',
    'Comparative cost analysis of entropy-only systems vs RNG+entropy stacks; implementation of simpler RNG designs with same security guarantees',
    'If costs are truly asymmetric: suppression is structural (legitimate barrier). If cost inflation is architected: suppression includes extractive design choices that could be avoided.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hardware_entropy_cost_asymmetry, empirical, 'Whether RNG complexity cost is necessary or architected for lock-in').

omega_variable(
    side_channel_attack_prevalence,
    'How often do RNG vulnerabilities emerge from algorithmic bias vs timing/power side-channels in implementation?',
    'Catalog of disclosed RNG vulnerabilities by category; prevalence analysis of algorithm flaws vs implementation flaws',
    'If algorithmic: suppliers can claim plausible deniability. If implementation-focused: suppliers bear responsibility for quality control, reducing extraction potential.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(side_channel_attack_prevalence, empirical, 'Prevalence of algorithmic vs implementation RNG vulnerabilities').

omega_variable(
    quantum_resistant_rng_contingency,
    'Do proposed post-quantum RNG designs actually solve the entropy verification problem, or do they displace it to new domains (lattice-based entropy, hash-based state)?',
    'Analysis of post-quantum RNG designs for retained dependencies on unverifiable entropy sources; comparison of verification requirements across quantum and post-quantum landscapes',
    'If problem is solved: scaffold perspective confirmed — post-quantum transition is real exit path. If displaced: constraint persists under new formulation, and scaffold is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_resistant_rng_contingency, empirical, 'Whether post-quantum RNG designs resolve entropy verification problem').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(random_number_generator_security, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rng_sec_tr_t0, random_number_generator_security, theater_ratio, 0, 0.38).
narrative_ontology:measurement(rng_sec_tr_t10, random_number_generator_security, theater_ratio, 10, 0.48).
narrative_ontology:measurement(rng_sec_tr_t20, random_number_generator_security, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(rng_sec_be_t0, random_number_generator_security, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(rng_sec_be_t10, random_number_generator_security, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(rng_sec_be_t20, random_number_generator_security, base_extractiveness, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(random_number_generator_security, enforcement_mechanism).
narrative_ontology:affects_constraint(random_number_generator_security, cryptographic_key_generation).
narrative_ontology:affects_constraint(random_number_generator_security, side_channel_attack_vectors).

% DUAL FORMULATION NOTE:
% RNG security decomposes into: (1) entropy_source_quality (ε ≈ 0.35, Rope) — physical randomness generation, (2) random_number_generator_security (ε ≈ 0.62, Snare) — algorithm design and verification, (3) implementation_timing_attacks (ε ≈ 0.68, Snare) — cryptographic implementation security. The main story focuses on RNG architectural extraction; upstream entropy constraint is separate; downstream timing vulnerabilities form a distinct extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(random_number_generator_security, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
