% ============================================================================
% CONSTRAINT STORY: cryptographic_key_management_scaling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cryptographic_key_management_scaling, []).

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
 *   constraint_id: cryptographic_key_management_scaling
 *   human_readable: Cryptographic Key Management Scaling
 *   domain: cybersecurity/cryptography/infrastructure
 *
 * SUMMARY:
 *   Cryptographic key management scaling represents a constraint on
 *   distributed security infrastructure where the coordination problem —
 *   securing private keys against compromise — has become increasingly
 *   dominated by centralized custodianship and hardware security module
 *   vendors. As organizations scale cryptographic operations across
 *   distributed systems, they face a structural choice: build decentralized
 *   key management infrastructure (high operational overhead, novel security
 *   assumptions) or rely on centralized key custodians (lower operational
 *   complexity but concentrated extraction risk and vendor dependence). The
 *   constraint exhibits tangled_rope structure: genuine coordination value in
 *   managing key material securely coexists with extractive mechanisms
 *   (vendor lock-in, licensing models, operational overhead multiplication).
 *   The theater ratio (0.58) reflects that compliance-driven key rotation
 *   ceremonies and audit procedures consume significant operational effort
 *   while the actual reduction in breach risk remains unclear. Extractiveness
 *   has increased over 15 years as distributed systems have scaled and vendor
 *   consolidation has reduced alternatives.
 *
 * KEY AGENTS:
 *   - Distributed System Operators: Primary victims (powerless/trapped) — dependent on key management infrastructure; face high barriers to building alternatives
 *   - Small Organization Security Teams: Secondary victims (moderate/constrained) — constrained by cost barriers to HSM infrastructure; also benefit from standardized tooling
 *   - Hardware Security Module Vendors: Primary beneficiaries (institutional/arbitrage) — capture licensing revenue and switching costs; benefit from market consolidation
 *   - Centralized Key Custodians: Secondary beneficiary (institutional/constrained) — benefit from market position but also bear security and compliance burden
 *   - Legacy PKI Infrastructure: Institutional actor (institutional/constrained) — persists through inertia; performative compliance processes maintain theater without proportional security gain
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional custodianship as cryptographic necessity when distributed alternatives exist
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cryptographic_key_management_scaling, 0.52).
domain_priors:suppression_score(cryptographic_key_management_scaling, 0.65).
domain_priors:theater_ratio(cryptographic_key_management_scaling, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cryptographic_key_management_scaling, extractiveness, 0.52).
narrative_ontology:constraint_metric(cryptographic_key_management_scaling, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cryptographic_key_management_scaling, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cryptographic_key_management_scaling, tangled_rope).
narrative_ontology:human_readable(cryptographic_key_management_scaling, "Cryptographic Key Management Scaling").
narrative_ontology:topic_domain(cryptographic_key_management_scaling, "cybersecurity/cryptography/infrastructure").

domain_priors:requires_active_enforcement(cryptographic_key_management_scaling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cryptographic_key_management_scaling, centralized_key_custodians).
narrative_ontology:constraint_beneficiary(cryptographic_key_management_scaling, hardware_security_module_vendors).
narrative_ontology:constraint_victim(cryptographic_key_management_scaling, distributed_system_operators).
narrative_ontology:constraint_victim(cryptographic_key_management_scaling, small_organization_security_teams).
narrative_ontology:constraint_victim(cryptographic_key_management_scaling, end_user_privacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISTRIBUTED SYSTEM OPERATOR (SNARE) — Cannot implement decentralized cryptographic infrastructure without centralized key management intermediaries. Trapped by lack of economically viable alternatives and the network effects that lock in incumbent custodians. Bears full cost of key management complexity while beneficiaries capture coordination rent.
constraint_indexing:constraint_classification(cryptographic_key_management_scaling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL ORGANIZATION SECURITY TEAM (TANGLED ROPE) — Constrained by cost barriers to building in-house HSM infrastructure and the overhead of key rotation protocols. Also benefits from coordinated key management standards and tooling that reduce implementation burden. Moderate extraction with genuine coordination function.
constraint_indexing:constraint_classification(cryptographic_key_management_scaling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HARDWARE SECURITY MODULE VENDOR (ROPE) — Net beneficiary from the constraint. Benefits from market demand for HSM products and licensing models. Experiences the constraint as coordination: managing key material securely is a collective action problem that their products solve. Can exit to alternative cryptographic architectures but chooses not to.
constraint_indexing:constraint_classification(cryptographic_key_management_scaling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CENTRALIZED KEY CUSTODIAN (TANGLED ROPE) — Institutional beneficiary that also faces constraints: operational burden of secure key storage, regulatory compliance, and liability for key compromise. Benefits from market position and switching costs but also carries extraction costs — must invest heavily in security infrastructure to maintain trust.
constraint_indexing:constraint_classification(cryptographic_key_management_scaling, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY PKI INFRASTRUCTURE (PITON) — Centralized key hierarchy persists through institutional inertia despite emergence of decentralized alternatives (blockchain-based PKI, threshold cryptography, distributed key generation). Theater ratio reflects performative key rotation ceremonies and compliance audits that are largely symbolic — actual key compromise risk is not substantially reduced. Maintained because alternatives haven't fully displaced it.
constraint_indexing:constraint_classification(cryptographic_key_management_scaling, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a cryptographic complexity perspective, key management fundamentally requires custodianship of some form: the mathematical properties of asymmetric cryptography mean that private keys must be stored somewhere secure. Some centralization or trusted third-party involvement appears mathematically necessary. However, this perspective risks naturalizing a contingent institutional choice as a cryptographic law. Distributed key generation and threshold schemes offer alternative mathematical structures that reduce single-point-of-failure risk.
constraint_indexing:constraint_classification(cryptographic_key_management_scaling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cryptographic_key_management_scaling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cryptographic_key_management_scaling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cryptographic_key_management_scaling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cryptographic_key_management_scaling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cryptographic_key_management_scaling, TR),
    TR >= 0.70.

:- end_tests(cryptographic_key_management_scaling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from distributed system operators and small organizations through licensing costs, operational overhead multiplication, and vendor switching costs. The extraction is not maximal (0.66+) because genuine coordination value exists — secure key management is a real collective action problem. The extractiveness trend (0.38→0.52 over 15 years) reflects vendor consolidation and increased complexity as systems scale. Suppression (0.65): Moderate-high. Significant barriers to exit include: lack of economically viable decentralized alternatives at scale, network effects favoring incumbent vendors, regulatory compliance requirements that mandate specific HSM models, and technical complexity of implementing alternative key management architectures. Theater ratio (0.58): Moderate-high. Key rotation ceremonies, compliance audits, and HSM certifications consume operational effort while actual reduction in cryptographic breach risk is unclear. Many organizations perform these rituals to satisfy compliance requirements rather than for direct security gain. The theater has increased (0.42→0.58) as regulatory pressure has grown.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates divergent classifications from structurally different positions. The distributed operator sees a snare — they are locked into vendor ecosystems with high switching costs and no viable exit. The small organization sees tangled rope — real coordination benefits from standardized tools coexist with extraction via licensing and operational overhead. The HSM vendor sees rope — they solve a genuine collective action problem (secure key storage) and experience the market as coordination. The centralized custodian sees tangled rope from the opposite side — they benefit but must invest heavily in security infrastructure and liability management. The legacy PKI sees itself as degraded (piton) — performing key rotation ceremonies largely for compliance theater, not primary function. The analytical observer risks the false summit of seeing centralized custodianship as cryptographically necessary when distributed key generation and threshold schemes offer mathematical alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: beneficiaries with arbitrage options (HSM vendors, custodians) have low d despite extraction; trapped victims have high d; constrained actors occupy middle ground. The HSM vendor's arbitrage exit option derives from their ability to shift to alternative markets (cloud security, identity management) if key management demand declines, placing them as net beneficiary despite extraction. The distributed operator's trapped status reflects lack of viable alternatives at the scale required for modern infrastructure. The centralized custodian's constrained exit reflects that they bear significant liability for security failures even as they benefit from market position. The piton's theater ratio (0.65) derives from operational rituals that persist despite uncertain security benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy through the distinction between mathematical coordination requirements and institutional extraction mechanisms. Secure key storage IS a genuine collective action problem — some custodianship is necessary. However, the institutional choice to concentrate custodianship in vendor-controlled HSMs creates secondary extraction mechanisms (licensing, lock-in, compliance overhead multiplication). The perspective structure reveals this decomposition: rope-level genuine coordination exists (mathematical necessity), but tangled_rope extraction is layered on top (institutional choice). The piton's performative key rotation reflects Goodhart drift — the measurable compliance metric (rotation frequency) has decoupled from the security outcome (actual breach prevention). Decentralized alternatives (DKG, threshold cryptography) prove that custodianship concentration is not mathematically necessary, making the vendor lock-in mechanism a tangled_rope rather than a mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mathematical_necessity_vs_institutional_choice,
    'Is centralized key custodianship a mathematical necessity of asymmetric cryptography or a contingent institutional choice?',
    'Analysis of threshold cryptography, distributed key generation (DKG), and multi-party computation schemes showing whether private key custodianship can be mathematically distributed. Compare cryptographic properties of centralized vs distributed key storage architectures.',
    'If mathematical necessity: mountain classification confirmed; key management scaling is an inherent limit. If institutional choice: constraint is tangled_rope; scaling barriers are extractive mechanisms that could be redesigned.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mathematical_necessity_vs_institutional_choice, conceptual, 'Whether centralization is cryptographically necessary or institutionally contingent').

omega_variable(
    decentralized_infrastructure_adoption_threshold,
    'At what operational maturity do distributed key management systems (blockchain PKI, threshold cryptography, DKG protocols) become economically competitive with centralized HSM infrastructure?',
    'Cost-benefit analysis of decentralized vs centralized approaches controlling for security level, compliance requirements, and operational overhead. Temporal tracking of adoption rates in high-security domains (financial institutions, government, critical infrastructure).',
    'If adoption threshold is near: scaffold perspective confirmed — current centralization has sunset clause. If threshold is far or unreachable: snare perspective confirmed — distributed operators are locked in for decades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_infrastructure_adoption_threshold, empirical, 'Adoption threshold for decentralized key management systems').

omega_variable(
    key_compromise_causation,
    'What proportion of real-world cryptographic breaches result from key management failures vs other attack vectors (implementation bugs, quantum threats, side-channel attacks)?',
    'Forensic analysis of disclosed breaches categorized by root cause. Longitudinal tracking of breach attribution across financial, government, and critical infrastructure sectors.',
    'If key management is dominant cause: justifies high suppression and centralized custodianship overhead. If key management is minority cause: suggests theater ratio is inflated — compliance theater around key rotation obscures that actual breach risk comes from elsewhere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(key_compromise_causation, empirical, 'What fraction of breaches result from key management failures').

omega_variable(
    vendor_lock_in_mechanism,
    'Is the constraint''s extraction mechanism primarily technical (incompatible key formats), contractual (licensing terms), or network-effect-based (no competing standards)?',
    'Analysis of HSM vendor ecosystems; investigation of key portability between vendors; examination of licensing agreements for switching costs; assessment of whether competing standards exist but lack adoption.',
    'If technical: constraint may be mountain-like (cryptographic incompatibility is hard to overcome). If contractual: snare-like (artificial lock-in that could be reformed). If network-effect: tangled_rope (genuine coordination value but extractive secondary effects).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vendor_lock_in_mechanism, empirical, 'Whether vendor lock-in is technical, contractual, or network-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cryptographic_key_management_scaling, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ckms_tr_t0, cryptographic_key_management_scaling, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ckms_tr_t5, cryptographic_key_management_scaling, theater_ratio, 5, 0.5).
narrative_ontology:measurement(ckms_tr_t10, cryptographic_key_management_scaling, theater_ratio, 10, 0.58).
narrative_ontology:measurement(ckms_tr_t15, cryptographic_key_management_scaling, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(ckms_be_t0, cryptographic_key_management_scaling, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ckms_be_t5, cryptographic_key_management_scaling, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(ckms_be_t10, cryptographic_key_management_scaling, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(ckms_be_t15, cryptographic_key_management_scaling, base_extractiveness, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cryptographic_key_management_scaling, enforcement_mechanism).
narrative_ontology:affects_constraint(cryptographic_key_management_scaling, post_quantum_cryptography_migration).
narrative_ontology:affects_constraint(cryptographic_key_management_scaling, cloud_infrastructure_trust_assumptions).
narrative_ontology:affects_constraint(cryptographic_key_management_scaling, zero_trust_architecture_implementation).

% DUAL FORMULATION NOTE:
% Cryptographic key management scaling decomposes into three structurally distinct constraints: (1) mathematical key custodianship requirement (near-mountain, ε≈0.12), (2) vendor consolidation and lock-in mechanism (snare, ε≈0.68), (3) compliance-driven operational overhead (piton, ε≈0.35, theater≈0.75). This story represents the tangled_rope hybrid — the coordination function plus the institutional extraction that has accumulated. Upstream: post-quantum migration will force key management redesign. Downstream: zero-trust architecture assumes distributed key verification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cryptographic_key_management_scaling, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
