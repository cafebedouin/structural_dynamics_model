% ============================================================================
% CONSTRAINT STORY: cryptographic_group_selection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cryptographic_group_selection, []).

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
 *   constraint_id: cryptographic_group_selection
 *   human_readable: Cryptographic Group Selection Standards
 *   domain: cryptography/standardization/infrastructure
 *
 * SUMMARY:
 *   Cryptographic group selection — the process of choosing which
 *   mathematical structures (ECC, lattice-based, multivariate, hash-based
 *   systems) and specific parameters become standardized for deployment —
 *   exhibits the structural features of a tangled rope constraint. The
 *   selection process coordinates a genuine collective action problem:
 *   without standards, cryptographic software cannot reliably interoperate,
 *   and security decisions become unilaterally fragmented. Simultaneously,
 *   the standardization process extracts value from those who must migrate
 *   away from incumbent algorithms, lengthens adoption timelines for
 *   alternative approaches, and concentrates decision-making power in
 *   specialized standards bodies. The theater ratio (0.65) reflects that
 *   significant portions of the standardization process are performative:
 *   security audits of legacy algorithms despite known quantum vulnerability,
 *   compliance frameworks that document rather than mitigate risk, and
 *   competitive position maintenance disguised as technical evaluation. The
 *   constraint's evolution over the 10-year interval shows rising theater (as
 *   legacy systems accumulate compliance theater) and rising extractiveness
 *   (as the cost of delayed migration becomes clearer). The core tension is
 *   that standardization is both necessary (coordination of interoperability)
 *   and extractive (gatekeeping by incumbent bodies, delays for alternatives,
 *   implicit bias toward established vendors).
 *
 * KEY AGENTS:
 *   - Post-Quantum Alternative Proponents: Primary victims (powerless/trapped) — lattice-based, multivariate, hash-based cryptographers whose algorithms compete for standardization; trapped by slow NIST and IETF cycles
 *   - Incumbent Standards Bodies: Primary beneficiaries (institutional/arbitrage) — NIST, IETF, ISO; coordinate standardization and maintain control over algorithm selection with option to maintain legacy standards indefinitely
 *   - Cryptographic Software Vendors: Secondary beneficiaries (institutional/mobile) — maintain revenue streams from existing implementations; benefit from standardization delays that extend legacy product lifecycles
 *   - Academic Cryptographic Community: Secondary victims (organized/constrained) — must participate in standards processes to gain algorithm legitimacy; constrained by standards timeline and patent licensing requirements
 *   - Quantum-Ready Enterprises: Powerful actors (powerful/mobile) — can implement cryptographic agility during transition; have resources to absorb dual-algorithm costs temporarily
 *   - Resource-Constrained Implementations: Tertiary victims (powerless/constrained) — IoT devices, embedded systems that cannot support multiple cryptographic groups simultaneously; forced to wait for standardization or accept compatibility costs
 *   - Legacy Infrastructure Systems: Institutional actors (institutional/constrained) — maintain RSA/ECC despite quantum vulnerability; trapped by transition costs and institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cryptographic_group_selection, 0.38).
domain_priors:suppression_score(cryptographic_group_selection, 0.52).
domain_priors:theater_ratio(cryptographic_group_selection, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cryptographic_group_selection, extractiveness, 0.38).
narrative_ontology:constraint_metric(cryptographic_group_selection, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(cryptographic_group_selection, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cryptographic_group_selection, tangled_rope).
narrative_ontology:human_readable(cryptographic_group_selection, "Cryptographic Group Selection Standards").
narrative_ontology:topic_domain(cryptographic_group_selection, "cryptography/standardization/infrastructure").

domain_priors:requires_active_enforcement(cryptographic_group_selection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cryptographic_group_selection, incumbent_cryptographic_standards_bodies).
narrative_ontology:constraint_beneficiary(cryptographic_group_selection, cryptographic_software_vendors).
narrative_ontology:constraint_beneficiary(cryptographic_group_selection, surveillance_capable_actors).
narrative_ontology:constraint_victim(cryptographic_group_selection, cryptographic_alternative_proponents).
narrative_ontology:constraint_victim(cryptographic_group_selection, resource_constrained_implementations).
narrative_ontology:constraint_victim(cryptographic_group_selection, users_requiring_quantum_resistance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POST-QUANTUM ALTERNATIVE PROPONENT (SNARE) — Trapped by standardization bodies' slow adoption cycles. Cannot exit from the constraint: deployment of new cryptographic groups requires standards consensus. Bears full cost of selection delays while incumbent groups remain embedded in infrastructure. No organizational power to accelerate selection. Maximum extraction.
constraint_indexing:constraint_classification(cryptographic_group_selection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACADEMIC RESEARCH COMMUNITY (TANGLED ROPE) — Constrained by the requirement to participate in standards processes to gain influence, yet benefits from standardization (legitimacy for their algorithms, funding for standards work). Significant extraction in time investment and patent licensing terms; genuine coordination benefit in collective validation. Organized but not free to exit — abandoning standardization processes means losing voice.
constraint_indexing:constraint_classification(cryptographic_group_selection, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INCUMBENT STANDARDS BODIES (ROPE) — NIST, IETF, and similar bodies benefit from coordinating algorithm selection and deployment. The constraint solves a genuine coordination problem: without standards, software cannot reliably interoperate. Experiences the constraint as enablement rather than extraction. Has arbitrage options (can maintain legacy standards indefinitely while new groups emerge elsewhere). Net beneficiary.
constraint_indexing:constraint_classification(cryptographic_group_selection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: QUANTUM-READY ENTERPRISES (SCAFFOLD) — Powerful actors (major tech firms, financial infrastructure) have the resources to implement cryptographic agility — supporting multiple group types simultaneously during migration. See the constraint as a temporary coordination failure with a visible sunset: post-quantum standards will eventually achieve consensus, and the agile infrastructure built during transition becomes the new normal. Moderate extraction during the transition; low extraction once standards solidify.
constraint_indexing:constraint_classification(cryptographic_group_selection, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY INFRASTRUCTURE (PITON) — RSA, ECC, and SHA families remain deployed despite known quantum vulnerability. Persistence is driven by institutional inertia: transition costs are enormous, and the quantum threat is temporally distant for most systems. Theater ratio is high — many security assessments and compliance audits ritualize legacy algorithm use without addressing quantum risk. The infrastructure sees its own continuation as degraded maintenance of outdated standards.
constraint_indexing:constraint_classification(cryptographic_group_selection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPLEXITY VIEW (MOUNTAIN) — From a theoretical perspective, some cryptographic group selection burden is inevitable: any cryptographic system must choose concrete instantiations of algebraic structures, and the transition from one family to another is computationally and logistically constrained. The perceived mountain classification derives from the theoretical immutability of computational complexity limits. However, the structural data reveals this as false naturalization — the selection burden is largely institutional, not mathematical. True quantum resistance thresholds and standardization timelines are contingent choices.
constraint_indexing:constraint_classification(cryptographic_group_selection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cryptographic_group_selection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cryptographic_group_selection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cryptographic_group_selection, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(cryptographic_group_selection, TR),
    TR >= 0.70.

:- end_tests(cryptographic_group_selection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The standardization process delays migration to quantum-resistant algorithms while incumbent groups remain embedded. Alternative proponents bear opportunity costs and time-to-deployment barriers. However, extractiveness is not extreme because: (1) standards bodies operate with public processes and documented criteria, reducing opacity; (2) incumbent cryptographers have genuine technical legitimacy (RSA/ECC are well-studied), not pure gatekeeping; (3) quantum threat is temporally distant, reducing urgency and making transition timing negotiable. The value reflects that extraction exists alongside coordination benefit. Suppression (0.52): Moderate-high. Significant barriers include: (1) standardization gatekeeping (only NIST-endorsed algorithms become widely deployed); (2) network effects (existing infrastructure creates switching costs); (3) vendor coordination problems (cryptographic libraries move slowly even after standardization); (4) patent licensing uncertainty for alternative algorithms. But suppression is not total — some alternative pathways exist (open-source adoption, blockchain systems without standards bodies). Theater ratio (0.65): Moderate-high. Rising over the interval because: (1) security audits of legacy algorithms create performative compliance without addressing quantum risk; (2) standardization committees produce documentation theater (threat assessments, migration recommendations) without enforcement; (3) vendor migration timelines are announced repeatedly with limited follow-through. The theater rises as the constraint persists — initial standardization work is substantive; later years emphasize compliance narratives rather than actual transition.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a pronounced perspectival gap between beneficiaries and victims. Incumbent standards bodies and vendors experience the constraint as pure coordination (Rope) — they solve the genuine problem of algorithm interoperability and benefit from standardization delays that extend legacy product lifecycles. Their perspective is transparent: standardization is coordination, extraction is minimal. Post-quantum alternatives experience the constraint as pure extraction (Snare) — they are locked out of deployment pathways by gatekeeping and face indefinite delays for algorithm legitimacy. The open science coalition (powerful enterprises) experiences it as a temporary problem with a sunset (Scaffold) — cryptographic agility and decentralization are reducing standardization's necessity over time. The academic community experiences it as mixed coordination and extraction (Tangled Rope) — standardization legitimizes research but extracts time and patent liability. The civilizational observer risks seeing immutable constraints (Mountain) — that cryptographic interoperability necessarily requires centralized standardization — but the structural data reveals this as false naturalization. Alternative pathways (open-source, blockchain, industry consortia) are feasible but suppressed by incumbent preference for centralization.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the selection constraint. Incumbent standards bodies (institutional/arbitrage) have minimal extraction directed at them — they control the process and can delay indefinitely. Their d ≈ 0.1 (weak beneficiary position). Post-quantum alternatives (powerless/trapped) experience maximal extraction — they cannot access deployment pathways without standardization approval. Their d ≈ 0.95 (strong target position). Academic researchers (organized/constrained) experience moderate extraction — they must participate in standards processes to gain legitimacy but can exit by pursuing alternative pathways (open-source, blockchain). Their d ≈ 0.55. Enterprises with agility (powerful/mobile) experience low extraction — they have resources to implement dual stacks and can arbitrage between standards and alternatives. Their d ≈ 0.35. Resource-constrained implementations (powerless/constrained) experience high extraction but with less permanence than trapped alternatives — they must eventually migrate once standards solidify. Their d ≈ 0.75. The pipeline applies f(d) to these values, producing experienced extractiveness chi that varies by perspective while ε and suppression remain fixed.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that standardization performs genuine coordination (multiple stakeholders need to agree on algorithms) while also enforcing extraction (incumbent bodies and vendors benefit from slow migration timelines). The constraint is not mislabeled — it is legitimately Tangled Rope. The beneficiary/victim structure is clear: standards bodies + vendors benefit (lower d, negative χ); post-quantum alternatives suffer (higher d, positive χ); researchers and enterprises occupy intermediate positions. The classification as Tangled Rope is stable across metrics: base extractiveness (0.38) sits between pure coordination (ε ≤ 0.05) and pure extraction (ε ≥ 0.46); suppression (0.52) indicates coercive gatekeeping; theater ratio (0.65) reflects performative compliance. The false summit risk (mountain classification) exists only at the analytical/civilizational level, where centralised standardization appears immutable. The structural data contradicts this: decentralized alternatives and cryptographic agility are reducing standardization's necessity. The mountain perspective is accurate about asymptotic constraints (interoperability across billions of devices requires some coordination mechanism) but inaccurate about the necessity of centralized standards bodies specifically. The constraint persists through institutional inertia and vendor preference, not mathematical immutability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_threat_timeline_ambiguity,
    'What is the actual timeline for cryptographically relevant quantum computers (CRQCs) capable of breaking current public-key cryptography?',
    'Hardware progress tracking; gate fidelity benchmarks; logical qubit demonstrations; extrapolation from current error rates and scaling projections',
    'If timeline < 10 years: standardization delays become extractive (victims face imminent risk while standards linger). If timeline > 50 years: post-quantum migration becomes a coordination burden without corresponding risk mitigation. If timeline permanently recedes as hardware improves: perpetual transition state masquerades as urgency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quantum_threat_timeline_ambiguity, empirical, 'Timeline for cryptographically relevant quantum computer emergence').

omega_variable(
    harvest_now_decrypt_later_threat_materiality,
    'How significant is the ''Harvest Now, Decrypt Later'' threat — the practice of recording encrypted traffic today for decryption once quantum computers arrive?',
    'Empirical assessment of adversary storage and computational capacity; analysis of high-value encrypted data with retrospective utility; timeline for adversary quantum capacity relative to data sensitivity half-life',
    'If materially significant: urgent migration justified for high-value historic data. If low risk: transition timeline becomes negotiable, reducing selection pressure and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harvest_now_decrypt_later_threat_materiality, empirical, 'Significance of Harvest Now Decrypt Later threat').

omega_variable(
    standardization_process_capture_risk,
    'Do incumbent standards bodies (NIST, IETF) systematically bias selection toward cryptographic groups that entrench existing infrastructure or vendor positioning rather than selecting the mathematically most robust options?',
    'Historical analysis of NIST algorithm competition outcomes; comparison of selected algorithms'' robustness profiles against rejected candidates; vendor influence tracking in standards committee participation',
    'If systematic capture confirmed: standardization is an extraction mechanism (Snare from alternatives'' perspective; Tangled Rope from body''s perspective). If capture is absent or corrected: standardization is genuine coordination (Rope from all perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standardization_process_capture_risk, empirical, 'Capture risk in cryptographic standardization bodies').

omega_variable(
    implementation_agility_feasibility,
    'Can practical cryptographic implementations achieve genuine algorithmic agility — the ability to switch between multiple group types at runtime without severe performance penalties or security side-channel exposure?',
    'Benchmarking of cryptographic agility in production systems; analysis of performance/security trade-offs; empirical testing of side-channel attacks under algorithm switching',
    'If agility is feasible: scaffold perspective is correct (powerful actors can survive transition at low cost). If agility is infeasible: extraction increases for all actors except standards bodies (transition becomes an all-or-nothing replacement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_agility_feasibility, empirical, 'Feasibility of cryptographic algorithmic agility').

omega_variable(
    alternative_pathway_viability,
    'Could decentralized, non-standards-body-endorsed cryptographic adoption (via open-source projects, blockchain systems, or alternative protocol stacks) achieve sufficient network effects to bypass standardization altogether?',
    'Analysis of adoption curves for non-standard cryptographic groups; interoperability rates between standard and alternative implementations; economic incentives for deviation from standards',
    'If viable: the constraint''s suppression decreases (exit options improve). If non-viable: standardization remains the only path, and extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_viability, empirical, 'Viability of alternative non-standardized cryptographic pathways').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cryptographic_group_selection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cgs_tr_t0, cryptographic_group_selection, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cgs_tr_t5, cryptographic_group_selection, theater_ratio, 5, 0.58).
narrative_ontology:measurement(cgs_tr_t10, cryptographic_group_selection, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(cgs_be_t0, cryptographic_group_selection, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cgs_be_t5, cryptographic_group_selection, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(cgs_be_t10, cryptographic_group_selection, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cryptographic_group_selection, information_standard).
narrative_ontology:affects_constraint(cryptographic_group_selection, quantum_computing_threat_materiality).
narrative_ontology:affects_constraint(cryptographic_group_selection, cryptographic_software_vendor_lock_in).
narrative_ontology:affects_constraint(cryptographic_group_selection, post_quantum_algorithm_selection_bias).

% DUAL FORMULATION NOTE:
% Cryptographic group selection is upstream of algorithm-specific constraints. The selection process affects which algorithms become standardized and available for deployment. Downstream constraints (quantum threat materiality, vendor lock-in) depend on which groups are selected and how rapidly migration occurs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cryptographic_group_selection, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
