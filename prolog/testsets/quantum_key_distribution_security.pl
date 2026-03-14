% ============================================================================
% CONSTRAINT STORY: quantum_key_distribution_security
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_key_distribution_security, []).

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
 *   constraint_id: quantum_key_distribution_security
 *   human_readable: Quantum Key Distribution Security and Authentication Asymmetry
 *   domain: cryptography/quantum_information/cybersecurity
 *
 * SUMMARY:
 *   Quantum Key Distribution promises cryptographic security by leveraging
 *   the no-cloning theorem of quantum mechanics, but this promise contains a
 *   structural trap: QKD secures only the key exchange phase. It delegates
 *   the hardest remaining problem — authenticating that the quantum channel's
 *   endpoints are actually the intended parties — back to classical
 *   authentication methods. This creates an authentication asymmetry where
 *   end users must solve a classical authentication problem to use a quantum
 *   system, while the classical problem itself has not been solved in
 *   quantum's favor. Simultaneously, QKD vendors and state actors extract
 *   geopolitical and market advantages from the infrastructure, capturing
 *   regulatory authority through standards bodies that are not equipped to
 *   audit quantum optics independently. The constraint exhibits all six types
 *   across perspectives: a natural law (mountain, from the physics limit
 *   view), a coordination mechanism (rope, from vendors), mixed
 *   coordination-extraction (tangled rope, from standards bodies and state
 *   actors), pure extraction (snare, from end users), theatrical ritual
 *   (piton, from classical cryptography standards), and a temporary problem
 *   with a sunset (scaffold, from post-quantum cryptography adoption).
 *
 * KEY AGENTS:
 *   - End Users: Primary victim (powerless/trapped) — bear the authentication burden while being locked into QKD infrastructure; cannot exit without abandoning quantum-safe cryptography
 *   - QKD Vendors (Huawei, Toshiba, ID Quantique, Chinese manufacturers): Primary beneficiary (institutional/arbitrage) — extract procurement revenue, regulatory credibility, and geopolitical advantage from exclusive infrastructure control
 *   - Cryptographic Standards Bodies (NIST, ETSI, ISO): Secondary actor (moderate/constrained) — coordinate authentication protocols but are constrained by opacity of quantum optics domain; extractive asymmetry in credential value
 *   - State Surveillance Apparatus (China, USA, EU): Secondary beneficiary (organized/constrained) — extract geopolitical leverage from QKD infrastructure control; benefit from perceived quantum dominance; cannot exit without intelligence capability loss
 *   - Classical Cryptography Consensus (RSA/ECC): Institutional degradation (institutional/arbitrage) — pre-quantum cryptography persists through inertia; theater has shifted but authentication problem remains unsolved
 *   - Post-Quantum Cryptography Transition (NIST PQC standards): Exit pathway (organized/mobile) — offers classical-secure key exchange without quantum hardware dependencies; represents scaffold with genuine sunset
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing contingent policy choices about quantum infrastructure investment as inherent limits of cryptography
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_key_distribution_security, 0.58).
domain_priors:suppression_score(quantum_key_distribution_security, 0.52).
domain_priors:theater_ratio(quantum_key_distribution_security, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_key_distribution_security, extractiveness, 0.58).
narrative_ontology:constraint_metric(quantum_key_distribution_security, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(quantum_key_distribution_security, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_key_distribution_security, tangled_rope).
narrative_ontology:human_readable(quantum_key_distribution_security, "Quantum Key Distribution Security and Authentication Asymmetry").
narrative_ontology:topic_domain(quantum_key_distribution_security, "cryptography/quantum_information/cybersecurity").

domain_priors:requires_active_enforcement(quantum_key_distribution_security).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_key_distribution_security, qkd_vendors).
narrative_ontology:constraint_beneficiary(quantum_key_distribution_security, state_surveillance_agencies).
narrative_ontology:constraint_victim(quantum_key_distribution_security, end_users_authentication_burden).
narrative_ontology:constraint_victim(quantum_key_distribution_security, field_cryptographic_standards).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Trapped by the authentication paradox: QKD promises unconditional security of the key exchange itself but delegates the hardest problem (authenticating the quantum channel's endpoints) back to classical methods. The user cannot escape classical authentication vulnerabilities while being locked into the QKD infrastructure. Full extraction via theater — the security promise is conditional on the very classical authentication that QKD was supposed to replace.
constraint_indexing:constraint_classification(quantum_key_distribution_security, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CRYPTOGRAPHIC STANDARDS BODY (TANGLED ROPE) — Constrained by the need to validate QKD claims in an opaque quantum optics domain, but also benefits from the perceived advancement of quantum-safe cryptography in public perception. Real coordination function (standardizing authentication protocols) paired with asymmetric extraction (vendors extract credibility via standards certification; users bear implementation burden). Active enforcement required: standards bodies must continuously audit QKD implementations against phantom side-channel threats.
constraint_indexing:constraint_classification(quantum_key_distribution_security, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: QKD VENDOR (ROPE) — Institutional beneficiary with arbitrage options. Experiences constraint as pure coordination: selling quantum-secure infrastructure, certifying implementations, capturing government procurement advantages. Net positive extraction — vendors benefit from regulatory mandates, security theater, and government subsidies without bearing implementation costs. Exit via arbitrage: can shift to next-generation quantum cryptography or post-quantum classical alternatives without material loss.
constraint_indexing:constraint_classification(quantum_key_distribution_security, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE SURVEILLANCE APPARATUS (TANGLED ROPE) — Organized agent experiencing the constraint as mixed coordination and extraction. Genuine coordination: QKD infrastructure enables state-to-state secure communication, reducing risk of diplomatic miscommunication. Asymmetric extraction: states that control QKD networks (China, European Union via EuroQCI) extract geopolitical leverage from infrastructure control. Constrained exit: cannot withdraw without losing intelligence capability advantage. Active enforcement: states maintain QKD networks and extract utility from controlling quantum channels.
constraint_indexing:constraint_classification(quantum_key_distribution_security, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: CLASSICAL CRYPTOGRAPHIC CONSENSUS (PITON) — Degraded institutional position. Pre-quantum cryptography (RSA, elliptic curves) was performative in a different way: security relied on computational hardness assumptions that were always contestable. QKD promised to replace computation with physics. But the promised replacement doesn't actually resolve the authentication bottleneck — the theater has merely shifted. Classical cryptography persists through inertia in most applications; QKD adoption remains limited to government/finance. Theater ratio is high because both systems are partly performative: they promise more certainty than they deliver.
constraint_indexing:constraint_classification(quantum_key_distribution_security, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: POST-QUANTUM CRYPTOGRAPHY TRANSITION (SCAFFOLD) — Organized alternative pathway with sunset logic. NIST post-quantum standards (Crystals-Kyber, Crystals-Dilithium) offer classical-secure key exchange without quantum hardware dependencies. Organizations adopting PQC can exit QKD infrastructure without abandoning quantum-safe cryptography. Mobile exit options available. The constraint has a natural sunset: as PQC becomes standardized and hardware-efficient, QKD's extraction mechanism (exclusive access to 'quantum-safe' communication) loses force. Estimated sunset: 10-15 years for PQC to mature and replace QKD in non-government applications.
constraint_indexing:constraint_classification(quantum_key_distribution_security, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICS LIMIT VIEW (MOUNTAIN) — From a universal/civilizational perspective, some authentication asymmetry is inherent to quantum mechanics: you cannot authenticate a quantum channel without either a pre-shared key (assuming trusted delivery) or a classical side-channel. This appears as a natural law: no cryptographic system solves the pre-authentication problem without assuming something trusted. However, the structural data reveals this as false naturalization — the authentication asymmetry is real, but the extraction layered on top of it is contingent. The mountain classification obscures policy choices about whether to build expensive quantum infrastructure to solve a problem that classical cryptography already handles adequately.
constraint_indexing:constraint_classification(quantum_key_distribution_security, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_key_distribution_security_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quantum_key_distribution_security, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quantum_key_distribution_security, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_key_distribution_security, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quantum_key_distribution_security, TR),
    TR >= 0.70.

:- end_tests(quantum_key_distribution_security_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The authentication asymmetry creates genuine extraction: end users must solve classical authentication to use quantum systems, while vendors extract procurement revenue from this dependency. But extractiveness is not as high as pure rent-seeking (0.70+) because QKD does provide genuine quantum-level security for the key exchange phase. The extraction is mechanism-based (authentication theater) rather than pure scarcity. Suppression (0.52): Moderate-high. Significant barriers include: (1) opacity of quantum optics makes independent verification difficult for standards bodies and end users, (2) regulatory capture of cryptography standards by vendors and states, (3) technical barriers to implementing alternative authentication protocols, (4) government subsidies that make QKD adoption path-dependent. But suppression is not total — post-quantum cryptography offers a genuine exit path, and device-independent QKD reduces classical overhead. Theater ratio (0.65): High. The security narrative emphasizes quantum-level guarantees while understating the classical authentication problem. Both QKD and classical cryptography engage in theater — they promise more certainty than they deliver. QKD's theater has increased as vendors emphasize quantum advantage while minimizing authentication gaps.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint as a policy choice framed as physics. Vendors' rope perspective emphasizes genuine coordination: 'QKD enables secure quantum communication.' End users' snare perspective emphasizes blocked authentication: 'We still must solve classical authentication, now within quantum infrastructure.' Standards bodies' tangled rope perspective emphasizes constrained coordination: 'We validate QKD but lack expertise to audit quantum optics independently.' State actors' tangled rope perspective emphasizes leverage: 'QKD infrastructure provides geopolitical advantage even if authentication problem persists.' Classical cryptography's piton perspective emphasizes degradation: 'Pre-quantum cryptography was adequate but is now dismissed as quantum-unsafe.' Post-quantum cryptography's scaffold perspective emphasizes exit: 'PQC offers equivalent security without quantum infrastructure dependency.' The analytical observer's mountain perspective risks naturalizing: 'Authentication asymmetry is inherent to any cryptographic system.' The empirical reality suggests the mountain is false naturalization — authentication asymmetry exists, but the policy to build QKD infrastructure to solve it is contingent, not necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   QKD vendors derive d ≈ 0.15 (beneficiary + arbitrage) → f(d) ≈ -0.01 → negative effective extraction. They benefit from the constraint and can exit via migration to PQC without loss. End users derive d ≈ 0.95 (victim + trapped) → f(d) ≈ 1.42 → high effective extraction. They bear the authentication burden and cannot exit without abandoning quantum-safe cryptography. Standards bodies derive d ≈ 0.60 (mixed: coordinate standards but extract credibility) → f(d) ≈ 0.85 → moderate effective extraction. State actors derive d ≈ 0.40 (beneficiary with constrained exit) → f(d) ≈ 0.40 → low effective extraction, but the constraint provides geopolitical leverage compensating for mobility barriers. Post-quantum cryptography path derives d ≈ 0.30 (mobile exit option) → f(d) ≈ 0.15 → low effective extraction. The scope modifier σ(global) = 1.2 amplifies effective extraction at global scale: larger scope makes verification harder, enabling more extractive hidden mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via perspectival decomposition: The constraint is simultaneously tangled rope (from standards bodies and state actors), snare (from end users), rope (from vendors), piton (from classical cryptography), and scaffold (from PQC adoption). The mandatrophy dissolves when recognizing that all six types are legitimate readings from different structural positions. End users are genuinely trapped by authentication asymmetry (snare); vendors are genuinely benefiting from infrastructure control (rope); standards bodies are genuinely constrained by technical opacity while extracting credibility (tangled rope); states are genuinely benefiting from geopolitical leverage while constrained by security needs (tangled rope); classical cryptography is genuinely degraded (piton); post-quantum cryptography represents a genuine sunset pathway (scaffold). The false summit (mountain from physics-limit view) is revealed by the structural data showing that authentication asymmetry, while real, is not inherent to cryptography — it is a policy choice to build expensive quantum infrastructure rather than solve classical authentication directly. The mandatrophy analysis confirms: tangled rope is the primary type for the constraint as a whole, with snare (end user perspective) and rope (vendor perspective) as dominant secondary perspectives. The mountain perspective naturalizes what should be treated as contingent institutional choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authentication_asymmetry_necessity,
    'Is the authentication bottleneck inherent to cryptography or contingent to current QKD implementations?',
    'Theoretical analysis of whether any key exchange protocol (classical or quantum) can authenticate endpoints without pre-shared information or trusted side-channels. Comparison with device-independent QKD protocols that claim to reduce classical overhead.',
    'If inherent: mountain classification is justified — all parties face the same authentication burden regardless of system design. If contingent: the snare classification is justified — QKD implementations extract by hiding the authentication problem rather than solving it. Current evidence suggests contingent (device-independent QKD exists but is experimentally challenging).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authentication_asymmetry_necessity, conceptual, 'Whether authentication asymmetry is inherent to cryptography or specific to QKD design').

omega_variable(
    side_channel_reality,
    'What fraction of real-world QKD vulnerabilities arise from authenticated-channel attacks vs actual implementation side-channels (detector efficiency, timing, environmental)?',
    'Empirical analysis of disclosed QKD vulnerabilities (Xu et al., Scarani et al., Lo, Preskill reviews); categorization by attack vector; comparison of attack success rates across different QKD protocols.',
    'If authentication attacks dominant (>60%): extractiveness should increase to 0.70+ and classification shifts to snare. If implementation side-channels dominant: theater ratio may be higher (0.75+) and piton classification becomes stronger. Current evidence: mixed — Trojan horse attacks and detector efficiency exploits are both significant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(side_channel_reality, empirical, 'Relative frequency of authenticated-channel vs implementation side-channel attacks').

omega_variable(
    vendor_market_concentration,
    'Does QKD vendor consolidation (China''s dominance in ground-based QKD, European dominance in satellite QKD) create extractive chokepoints beyond the inherent authentication problem?',
    'Market analysis of QKD infrastructure control; geographic distribution of vendor capabilities; cost comparison of building independent national QKD vs using commercial systems; regulatory capture indicators.',
    'If high concentration: extractiveness increases via geopolitical chokepoint mechanism independent of authentication asymmetry. If distributed: extraction is primarily mechanism-based (authentication theater) rather than market-based. Current evidence: high concentration — China controls most deployed QKD infrastructure globally; EuroQCI is EU-specific.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vendor_market_concentration, empirical, 'Whether vendor consolidation creates extractive chokepoints beyond authentication asymmetry').

omega_variable(
    pqc_migration_credibility,
    'Will post-quantum cryptography actually replace QKD, or is PQC a parallel standard that coexists with QKD indefinitely?',
    '5-year and 10-year adoption forecasts for PQC vs QKD in government, finance, and enterprise sectors. Analysis of regulatory pressure (NIST mandates, EU standards) vs market incentives. Evaluation of whether quantum-advantage claims in QKD will prevent migration.',
    'If PQC dominates by 2036: scaffold sunset is real, extraction window closes. If QKD persists: scaffold perspective is aspirational rather than structural — the constraint becomes permanent tangled rope or snare rather than temporary scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pqc_migration_credibility, preference, 'Whether PQC will actually replace QKD or coexist indefinitely').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_key_distribution_security, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qkd_tr_t0, quantum_key_distribution_security, theater_ratio, 0, 0.45).
narrative_ontology:measurement(qkd_tr_t7, quantum_key_distribution_security, theater_ratio, 7, 0.58).
narrative_ontology:measurement(qkd_tr_t15, quantum_key_distribution_security, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(qkd_be_t0, quantum_key_distribution_security, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(qkd_be_t7, quantum_key_distribution_security, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(qkd_be_t15, quantum_key_distribution_security, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_key_distribution_security, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quantum_key_distribution_security, 0.12).
narrative_ontology:affects_constraint(quantum_key_distribution_security, post_quantum_cryptography_adoption).
narrative_ontology:affects_constraint(quantum_key_distribution_security, quantum_supremacy_claims).
narrative_ontology:affects_constraint(quantum_key_distribution_security, cryptographic_standards_capture).

% DUAL FORMULATION NOTE:
% QKD security consists of multiple structurally distinct constraints: (1) quantum_key_exchange_security (ε ≈ 0.08, Mountain) — the quantum phase is genuinely secure under no-cloning assumptions; (2) quantum_key_distribution_security (ε ≈ 0.58, Tangled Rope) — this story, covering authentication asymmetry and infrastructure extraction; (3) end_user_authentication_burden (ε ≈ 0.72, Snare) — the specific problem users face after QKD deployment. These three constraints are linked: QKD's low extractiveness in pure key exchange becomes high extractiveness when authentication burden is delegated to users. Network decomposition shows why naive QKD analysis understates the extraction: the quantum phase solves one problem (key exchange) while creating another (authentication burden). The apparent security gain is partial — extraction is transferred to users and vendors rather than eliminated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_key_distribution_security, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
