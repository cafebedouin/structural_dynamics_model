% ============================================================================
% CONSTRAINT STORY: cryptographic_primitive_trust
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cryptographic_primitive_trust, []).

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
 *   constraint_id: cryptographic_primitive_trust
 *   human_readable: Cryptographic Primitive Trust and Security Certification
 *   domain: cybersecurity/cryptography
 *
 * SUMMARY:
 *   Cryptographic primitive trust operates at the intersection of
 *   mathematical security, institutional standardization, and geopolitical
 *   control. The constraint creates a structural asymmetry: emerging
 *   cryptographers and resource-constrained deployments bear extraction costs
 *   (delayed adoption of novel primitives, lock-in to standards optimized for
 *   first-world computational resources) while standardizing bodies,
 *   incumbent vendors, and national security agencies capture benefits
 *   (market concentration, cryptanalytic advantage, backdoor insertion
 *   opportunities). The theater ratio has increased over the interval as the
 *   post-quantum cryptography transition has accelerated, generating
 *   high-profile review processes that create the appearance of rigorous
 *   vetting while masking persistent vulnerabilities in implementation, key
 *   management, and deployment context. The constraint exhibits
 *   characteristics of a Tangled Rope: genuine coordination functions
 *   (ecosystem convergence, interoperable tooling, distributed cryptanalysis)
 *   coexist with systematic extraction mechanisms (barrier elevation for
 *   alternatives, vendor lock-in, state-level backdoor insertion). Multiple
 *   institutional perspectives diverge sharply: standardizing bodies and
 *   incumbent vendors perceive pure coordination (Rope), while emerging
 *   researchers and resource-constrained deployments experience primarily
 *   extraction (Snare). The post-quantum cryptography transition represents a
 *   temporary Scaffold structure with an explicit sunset (2030 migration
 *   target), yet this sunset is potentially illusory if quantum threat
 *   timelines slip or if the transition itself becomes permanent
 *   institutional arrangement.
 *
 * KEY AGENTS:
 *   - Emerging Cryptographers: Primary victims (powerless/trapped) — face systematic barriers to alternative primitive adoption; career incentives flow toward standardized primitives
 *   - Resource-Constrained Deployments: Secondary victims (moderate/constrained) — locked into standards optimized for first-world computational budgets; custom alternatives require prohibitive cryptanalysis certification
 *   - Cryptographic Field Integrity: Collective victim (powerless/trapped) — abstract epistemic commons bearing costs of delayed strong alternatives and premature weak candidates
 *   - Standardizing Bodies (NIST/ISO): Primary beneficiaries (institutional/arbitrage) — coordinate ecosystem convergence, maintain flexibility to shift standards, exercise gatekeeping power
 *   - Incumbent Algorithm Vendors: Beneficiaries (institutional/arbitrage) — gain market concentration through standardization, extend patent-protected periods, capture licensing monopolies
 *   - National Security Agencies: Institutional beneficiaries (organized/constrained) — coordinate security infrastructure, extract through backdoor insertion and vulnerability non-disclosure, constrained by need for industrial compliance
 *   - Post-Quantum Transition Coalition: Organized actors (organized/constrained) — coordinate temporary PQC migration with explicit sunset, constrained by quantum threat uncertainty
 *   - Cryptanalytic Review Ritual: Institutional actor (institutional/arbitrage) — maintains performative certification ceremony; theater persists through publication/hiring requirements despite low functional verification
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (certification bottlenecks) as mathematical necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cryptographic_primitive_trust, 0.58).
domain_priors:suppression_score(cryptographic_primitive_trust, 0.65).
domain_priors:theater_ratio(cryptographic_primitive_trust, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cryptographic_primitive_trust, extractiveness, 0.58).
narrative_ontology:constraint_metric(cryptographic_primitive_trust, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cryptographic_primitive_trust, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cryptographic_primitive_trust, tangled_rope).
narrative_ontology:human_readable(cryptographic_primitive_trust, "Cryptographic Primitive Trust and Security Certification").
narrative_ontology:topic_domain(cryptographic_primitive_trust, "cybersecurity/cryptography").

domain_priors:requires_active_enforcement(cryptographic_primitive_trust).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cryptographic_primitive_trust, standardizing_bodies).
narrative_ontology:constraint_beneficiary(cryptographic_primitive_trust, incumbent_algorithm_vendors).
narrative_ontology:constraint_beneficiary(cryptographic_primitive_trust, national_security_agencies).
narrative_ontology:constraint_victim(cryptographic_primitive_trust, emerging_cryptographers).
narrative_ontology:constraint_victim(cryptographic_primitive_trust, alternative_algorithm_researchers).
narrative_ontology:constraint_victim(cryptographic_primitive_trust, resource_constrained_deployments).
narrative_ontology:constraint_victim(cryptographic_primitive_trust, cryptographic_field_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING CRYPTOGRAPHER (SNARE) — PhD researchers and small labs proposing novel cryptographic primitives face systematic barriers: NIST standards process dominates deployment, peer review gatekeepers (predominantly from incumbent vendors) scrutinize new proposals with hostile burden-of-proof, and years of cryptanalysis are required before any adoption. Cannot exit without abandoning the research direction entirely. Maximum extraction — career opportunities flow toward those who build on approved primitives, not toward those proposing alternatives.
constraint_indexing:constraint_classification(cryptographic_primitive_trust, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD CRYPTANALYTIC INTEGRITY (SNARE) — The abstract collective ability to identify weak primitives before deployment cannot exit the constraint. Delayed standardization of strong candidates (due to bureaucratic caution) and premature deployment of weak candidates (due to agency pressure) both degrade field integrity. The epistemic commons bears costs while beneficiaries capture security theater gains.
constraint_indexing:constraint_classification(cryptographic_primitive_trust, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: RESOURCE-CONSTRAINED DEPLOYMENT (TANGLED ROPE) — IoT devices, embedded systems, and developing-world deployments need lightweight cryptography. They benefit from standardized primitives (reduces implementation effort, improves ecosystem support) but suffer extraction through lock-in to standards that optimize for different constraints (US/EU computational power). Exit is possible but costly: custom primitives require cryptanalysis certification, defeating the resource constraint that motivated them.
constraint_indexing:constraint_classification(cryptographic_primitive_trust, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STANDARDIZING BODY (ROPE) — NIST and ISO gain coordination benefits from centralized primitive standardization: industry converges on common algorithms, ecosystem tools become interoperable, and cryptanalytic effort pools toward vetted candidates. Minimal extraction from this perspective — the constraint is experienced as a legitimate coordination mechanism. High arbitrage: standards bodies maintain flexibility to shift primitives as threats evolve.
constraint_indexing:constraint_classification(cryptographic_primitive_trust, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INCUMBENT ALGORITHM VENDOR (ROPE) — Vendors with established implementations (RSA, AES, SHA-2) experience the constraint as coordination (interoperability, market size, library support). They also benefit from extraction: standardization raises barriers to alternative implementations, extends patent-protected periods, and creates licensing monopolies. But they perceive this as coordination benefit (ecosystem strength) not extraction. High arbitrage: can shift to new standards as market demands, influence standards process.
constraint_indexing:constraint_classification(cryptographic_primitive_trust, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATIONAL SECURITY AGENCY (TANGLED ROPE) — NSAs (US, UK, others) coordinate security infrastructure through standardized primitives but also extract through backdoor insertion, vulnerability non-disclosure, and algorithm selection bias. Benefits from coordination (ecosystem-wide hardening) coexist with extraction benefits (cryptanalytic advantage, surveillance capacity). Constrained exit: cannot fully deploy non-standard cryptography without industrial defection; cannot fully abandon standards without losing coordination benefits.
constraint_indexing:constraint_classification(cryptographic_primitive_trust, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: CRYPTANALYTIC REVIEW RITUAL (PITON) — Peer review of cryptographic proposals follows a performative ceremony: journal papers and conference presentations demonstrate attack vectors, but this ritual often misses deployment-context vulnerabilities, side-channel weaknesses, and implementation errors. Theater ratio is high — the review confirms academic novelty but not practical security. The ritual persists through institutional inertia (publication requirements, hiring criteria) despite low functional verification.
constraint_indexing:constraint_classification(cryptographic_primitive_trust, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: POST-QUANTUM TRANSITION COALITION (SCAFFOLD) — NIST's post-quantum cryptography standardization (PQC process, 2022-2025) is a temporary scaffolding structure coordinating the migration from RSA/ECC to lattice/hash-based primitives. Organized actors (national security agencies, tech companies, standards bodies) see this as a sunset mechanism: temporary suppression of alternative PQC candidates is justified by the transitional urgency. Effective extraction is lower because the coalition has agency and explicit timeline for releasing alternatives.
constraint_indexing:constraint_classification(cryptographic_primitive_trust, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / MATHEMATICAL VIEW (MOUNTAIN) — From a universal/civilizational perspective, cryptographic primitives require computational hardness assumptions (discrete logarithm, factorization, lattice problems). No primitive can be adopted without proof of security against known attacks. This perspective sees the certification bottleneck as an immutable mathematical requirement. However, the structural data reveals this as a false summit: the gap between mathematical security proof and deployment security is a contingent institutional arrangement, not a natural law.
constraint_indexing:constraint_classification(cryptographic_primitive_trust, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cryptographic_primitive_trust_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cryptographic_primitive_trust, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cryptographic_primitive_trust, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cryptographic_primitive_trust, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cryptographic_primitive_trust, TR),
    TR >= 0.70.

:- end_tests(cryptographic_primitive_trust_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The standardization process captures genuine extraction: it raises barriers to alternative primitives through bureaucratic gatekeeping and peer review hostility, extends vendor lock-in periods, and enables state-level backdoor insertion. But the extraction is not total — standardization does provide real coordination benefits (ecosystem convergence, interoperability, distributed cryptanalysis). The value reflects that extraction coexists with legitimate coordination function. Suppression (0.65): High. Significant barriers to alternative cryptography include: NIST standards dominance in government/enterprise procurement, peer review gatekeeping by incumbent-affiliated reviewers, cryptanalysis burden (years of study before adoption is possible), and career disincentives for researchers pursuing non-standard directions. Emerging researchers face real costs to exiting the standardization pathway. Theater ratio (0.68): Moderately high and increasing. Peer review of cryptographic proposals creates the appearance of rigorous mathematical vetting but often misses deployment-context vulnerabilities, side-channel weaknesses, and implementation errors. The review ritual is required for legitimacy but provides limited functional security assurance. Theater has risen from 0.52 to 0.68 over the interval as post-quantum cryptography reviews have become more visible/ceremonial while implementation security problems have multiplied.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a radical divergence between institutional and marginal perspectives. The standardizing body (NIST) and incumbent vendors perceive a pure coordination mechanism (Rope): the constraint solves the genuine problem of algorithm interoperability and ecosystem convergence. But the emerging cryptographer perceives a snare: the same standardization process blocks their research alternatives and extends vendor dominance. The post-quantum coalition perceives a temporary scaffold with explicit sunset — the urgency is justified and the constraint is temporary. The analytical observer risks perceiving a mathematical natural law (Mountain) — cryptographic certification requires proof of security against known attacks, appearing unchangeable. But this natural law appearance collapses when examined structurally: the gap between mathematical security proof and deployment security is filled by contingent institutional arrangements (peer review, vendor gatekeeping, bureaucratic timelines) not mathematical necessity. The perspectival gap arises because different agents occupy different positions in the extraction flow; the same institutional structure that coordinates interoperability (real benefit to users) simultaneously extracts through barrier elevation (real cost to alternatives).
 *
 * DIRECTIONALITY LOGIC:
 *   The standardization process exhibits classic institutional asymmetry: benefits accrue to large incumbents with established implementations; costs fall on emergent researchers and resource-constrained deployments without institutional voice. Standardizing bodies maintain arbitrage flexibility (can shift standards as threats evolve) while alternative researchers face trapped/constrained exit. Incumbent vendors gain from lock-in without incurring the cryptanalysis costs (distributed across research community). National security agencies benefit from state-level backdoor insertion capacity while maintaining plausible deniability through the standardization ritual. The directionality values (d) encode these asymmetries: powerless emerging cryptographers derive d ≈ 0.95 → high f(d) ≈ 1.42 → high χ contribution; institutional standardizing bodies derive d ≈ 0.15 → low f(d) ≈ -0.01 → negligible χ contribution. The same extractiveness value (0.58) produces different experienced extraction (χ) depending on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that Tangled Rope is the only type that captures its true structure. A pure Rope classification (coordination only) misses the systematic extraction benefiting incumbents and harming emergent alternatives. A pure Snare classification (extraction only) misses the genuine coordination functions that standardization provides. Tangled Rope captures both: the constraint genuinely coordinates algorithm convergence (real beneficiary to ecosystem users) AND systematically extracts from emergent researchers and resource-constrained deployments (real cost). The perspectival gap is not ambiguity about the true type — it is divergence in experienced classification. The institutional standardizing body truthfully perceives Rope from its position. The powerless emerging cryptographer truthfully perceives Snare from theirs. These are not false perspectives waiting to be corrected; they are legitimate readings of the same structural data from different observation sites. The mandate clarifies that all perspectives are valid; the classification spectrum (snare at powerless/trapped, rope at institutional/arbitrage, tangled rope at moderate/constrained) reveals the constraint's true structure as a hybrid with asymmetric distribution of coordination benefits and extraction costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    backdoor_detection_capacity,
    'Can cryptanalytic review reliably detect deliberate backdoors inserted by state actors with superior computational resources?',
    'Post-deployment analysis of NSA/GCHQ Algorithm Suite history; comparison of discovered vs undetected backdoors; cryptanalytic team resource constraints relative to adversary resources',
    'If detection rate < 30%: standardization theater is high, extraction via backdoor insertion is severe. If detection rate > 80%: standardization process has genuine verification function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(backdoor_detection_capacity, empirical, 'Whether peer review detects inserted backdoors').

omega_variable(
    alternative_primitive_equivalence,
    'Do non-standard cryptographic primitives (elliptic curves over different fields, hash-based signatures, isogeny-based schemes) provide equivalent or superior security-per-bit compared to standardized alternatives, independent of standardization status?',
    'Comparative cryptanalysis of equivalent-parameter primitives; attack complexity analysis controlling for research investment and adversary resources',
    'If equivalent: non-standard primitives are suppressed by institutional lock-in (high extraction). If inferior: standardization captures genuine cryptanalytic consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_primitive_equivalence, empirical, 'Whether non-standard primitives are cryptographically equivalent').

omega_variable(
    quantum_threat_timeline_urgency,
    'Does the post-quantum cryptography transition timeline (2022-2030) reflect genuine cryptographically-relevant quantum computer threat acceleration or bureaucratic urgency unlinked to technical threat?',
    'Comparison of quantum algorithm development pace vs classical cryptanalysis pace; modeling of practical large-scale quantum computer deployment timeline',
    'If genuine quantum threat: PQC scaffold sunset is technically justified; lower extraction classification. If bureaucratic urgency: scaffold is manufacturing scarcity; higher extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_threat_timeline_urgency, empirical, 'Whether post-quantum transition urgency reflects actual quantum threat').

omega_variable(
    implementation_fidelity_gap,
    'What fraction of cryptographic vulnerabilities in deployed systems arise from weak primitives (standardization domain) vs implementation flaws, side-channel leaks, and key management failures (deployment domain)?',
    'Analysis of CVE database for cryptography-related vulnerabilities; categorization by root cause; comparison of standardization contributions to overall security posture',
    'If primitive weakness causes < 20% of vulnerabilities: standardization process addresses non-dominant threat; extraction via primitive control exceeds utility. If > 50%: standardization has genuine security function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_fidelity_gap, empirical, 'What fraction of cryptographic failures come from weak primitives').

omega_variable(
    resource_constrained_primitive_availability,
    'Are cryptographic primitives optimized for resource-constrained devices (IoT, embedded, developing-world deployments) available through standardization pathways, or do these deployments require custom alternatives?',
    'Survey of IoT/embedded cryptographic choices; analysis of NIST/ISO standard coverage for different computational budgets; cost analysis of custom vs standard implementations',
    'If standards omit resource-efficient options: emerging economies bear extraction through forced over-provisioning or non-standardized alternatives. If standards cover the spectrum: standardization provides genuine equity benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_constrained_primitive_availability, empirical, 'Whether standardization includes resource-constrained options').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cryptographic_primitive_trust, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crypt_tr_t0, cryptographic_primitive_trust, theater_ratio, 0, 0.52).
narrative_ontology:measurement(crypt_tr_t10, cryptographic_primitive_trust, theater_ratio, 10, 0.62).
narrative_ontology:measurement(crypt_tr_t20, cryptographic_primitive_trust, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(crypt_be_t0, cryptographic_primitive_trust, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(crypt_be_t10, cryptographic_primitive_trust, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(crypt_be_t20, cryptographic_primitive_trust, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cryptographic_primitive_trust, information_standard).
narrative_ontology:boltzmann_floor_override(cryptographic_primitive_trust, 0.08).
narrative_ontology:affects_constraint(cryptographic_primitive_trust, post_quantum_cryptography_transition).
narrative_ontology:affects_constraint(cryptographic_primitive_trust, cryptographic_backdoor_insertion).
narrative_ontology:affects_constraint(cryptographic_primitive_trust, implementation_verification_gap).

% DUAL FORMULATION NOTE:
% Cryptographic primitive trust is upstream of deployment-specific constraints (post-quantum transition urgency, backdoor insertion risk, implementation security gaps). Each downstream constraint has its own extractiveness value reflecting its domain-specific extraction mechanisms. The primitive trust constraint represents the institutional standardization layer shared by all downstream cryptographic constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cryptographic_primitive_trust, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
