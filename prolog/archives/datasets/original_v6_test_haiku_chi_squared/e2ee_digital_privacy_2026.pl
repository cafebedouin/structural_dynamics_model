% ============================================================================
% CONSTRAINT STORY: e2ee_digital_privacy_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_e2ee_digital_privacy_2026, []).

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
 *   constraint_id: e2ee_digital_privacy_2026
 *   human_readable: End-to-End Encryption (E2EE) as Digital Privacy Constraint
 *   domain: technological/political/social
 *
 * SUMMARY:
 *   End-to-end encryption (E2EE) is a global digital constraint that
 *   simultaneously enables privacy coordination and extracts from
 *   surveillance-dependent institutions. The constraint emerged from
 *   cryptographic research (Diffie-Hellman 1976, RSA 1977) but became
 *   politically charged with mass adoption in consumer platforms (WhatsApp
 *   2014, Signal 2010) and the post-Snowden encryption arms race
 *   (2013-present). E2EE presents as a 'wall' — mathematically elegant,
 *   algorithmically simple, and narratively compelling as a technical
 *   solution to privacy problems. Yet it exhibits hybrid
 *   coordination-extraction properties: it genuinely enables secure
 *   communication (rope-like coordination), simultaneously blocks law
 *   enforcement and state surveillance (snare extraction from their
 *   perspective), relies on degraded narrative marketing (piton theater), and
 *   faces fundamental challenges from quantum computing (scaffold sunset).
 *   The constraint is structurally mutable, not a natural law, making it a
 *   diagnostic case for mandatrophy resolution.
 *
 * KEY AGENTS:
 *   - Individual Users: Primary beneficiaries (powerless/mobile) — gain privacy coordination without constraint. d≈0.15, see E2EE as pure coordination (Rope).
 *   - Privacy Advocates / Civil Libertarians: Secondary beneficiaries (organized/arbitrage) — actively defend and deploy E2EE as counter-surveillance. d≈0.05.
 *   - Law Enforcement Agencies: Primary victims (organized/constrained) — face blocked access to suspect communications; cannot fully exit legal/political constraints on backdoors. d≈0.68, see mixed Tangled Rope.
 *   - Authoritarian State Surveillance Apparatus: Secondary victim (powerful/trapped) — mass surveillance infrastructure rendered useless by E2EE; cannot exit deployment globally. d≈0.95, see pure Snare extraction.
 *   - Platform Companies (Meta, Google, Apple): Mixed actor (institutional/constrained) — deploy E2EE for privacy marketing but lose data-harvesting revenue; constrained by regulation and competition. d≈0.72, see Tangled Rope with significant extraction pressure.
 *   - Cryptographic Community / Standards Bodies (IETF, Signal Protocol, ECC): Organized builder (organized/constrained) — actively developing post-quantum cryptography and formal verification; see clear technical sunset. d≈0.35, see Scaffold.
 *   - Quantum Hardware Researchers: Potential disruptor (powerful/mobile) — advances in quantum computing will mutate constraint; currently have arbitrage (can publish or withhold results). d≈0.20.
 *   - National Security Agencies (NSA, GCHQ): Institutional extractor (institutional/constrained) — massive operational cost from E2EE deployment; cannot mandate backdoors without international backlash. d≈0.80, experience as constrained Snare.
 *   - Tech-Solutionism Narrative Carriers: Institutional theater (institutional/arbitrage) — market E2EE as complete privacy solution, masking metadata leakage and device compromise. d≈0.45, maintain Piton through narrative authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(e2ee_digital_privacy_2026, 0.38).
domain_priors:suppression_score(e2ee_digital_privacy_2026, 0.62).
domain_priors:theater_ratio(e2ee_digital_privacy_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(e2ee_digital_privacy_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(e2ee_digital_privacy_2026, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(e2ee_digital_privacy_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(e2ee_digital_privacy_2026, tangled_rope).
narrative_ontology:human_readable(e2ee_digital_privacy_2026, "End-to-End Encryption (E2EE) as Digital Privacy Constraint").
narrative_ontology:topic_domain(e2ee_digital_privacy_2026, "technological/political/social").

domain_priors:requires_active_enforcement(e2ee_digital_privacy_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(e2ee_digital_privacy_2026, individual_users).
narrative_ontology:constraint_beneficiary(e2ee_digital_privacy_2026, privacy_advocates).
narrative_ontology:constraint_beneficiary(e2ee_digital_privacy_2026, dissidents_and_activists).
narrative_ontology:constraint_beneficiary(e2ee_digital_privacy_2026, journalists_and_whistleblowers).
narrative_ontology:constraint_victim(e2ee_digital_privacy_2026, law_enforcement_agencies).
narrative_ontology:constraint_victim(e2ee_digital_privacy_2026, intelligence_services).
narrative_ontology:constraint_victim(e2ee_digital_privacy_2026, corporate_data_harvesters).
narrative_ontology:constraint_victim(e2ee_digital_privacy_2026, state_surveillance_apparatus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL USER (ROPE) — E2EE is pure coordination: sender and recipient achieve secure communication without intermediary surveillance. User can adopt E2EE or abandon it (mobile exit). Benefits from the standard without being constrained by it. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.005. Negative effective extraction = net coordination benefit.
constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: CRIME VICTIM / INTELLIGENCE TARGET (SNARE) — Individual trapped by E2EE when adversary uses it to hide criminal activity or when state uses it to evade accountability. Cannot exit the constraint — the encryption persists. From this perspective, E2EE is pure extraction: it enables criminal/state actors to hide while victims have no recourse. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.66. High effective extraction.
constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LAW ENFORCEMENT / INTELLIGENCE AGENCIES (TANGLED ROPE) — E2EE both enables and constrains law enforcement. Coordination benefit: agencies can use E2EE for secure internal communications. Extraction cost: criminals and adversaries use E2EE to hide. Agencies are constrained by political/legal limits on backdoors and cannot fully exit. d≈0.68, f(d)≈1.05, σ=1.2 → χ≈0.48. Mixed coordination (internal secure comms) and extraction (blocked access to suspects/adversaries).
constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: AUTHORITARIAN STATE SURVEILLANCE APPARATUS (SNARE) — From the surveillance state's perspective, E2EE is pure extraction: it blocks mass surveillance infrastructure, rendering billions of citizens' communications opaque. The state cannot exit (E2EE is deployed globally). High extraction cost imposed on state power. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.66. Maximum effective extraction against state control.
constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, snare,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM COMPANIES (TANGLED ROPE) — E2EE provides coordination benefit (user privacy, competitive differentiation, reduced liability). But E2EE blocks corporate data harvesting and ad targeting — massive extraction cost. Companies are constrained by privacy regulation and user expectations; cannot fully exit E2EE deployment without losing trust. d≈0.72, f(d)≈1.12, σ=1.2 → χ≈0.52. Significant extraction pressure from blocked data flows, but coordination benefit from privacy marketing.
constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CRYPTOGRAPHIC COMMUNITY / STANDARDS BODIES (SCAFFOLD) — E2EE is temporary coordination mechanism solving mid-21st century privacy crisis. Exit path: quantum-resistant post-quantum cryptography (PQC) and novel threat models may shift the constraint. Organized agents (IETF, ECC, Signal Protocol maintainers) are actively building sunset: PQC migration, formal verification, zero-knowledge proofs reduce reliance on E2EE magic box. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.15. Low effective extraction; clear technical pathway toward resolution.
constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: CRYPTO MYSTIFICATION NARRATIVE (PITON) — E2EE is often presented as a complete solution to digital privacy, masking structural power relationships (corporate ownership of servers, metadata leakage, device compromise, state pressure on intermediaries). The 'encryption solves privacy' narrative is largely performative theater (theater_ratio=0.58). Real surveillance happens via metadata, network traffic analysis, compelled testimony, and device implants — E2EE blocks none of these. d≈0.45, f(d)≈0.47, σ=1.2 → χ≈0.22. Degraded constraint: E2EE persists through narrative authority despite incomplete functional protection.
constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: MATHEMATICAL PERSPECTIVE (MOUNTAIN) — From computational complexity theory, E2EE is NOT a mountain: RSA and ECC are not natural laws; they are contingent on the computational hardness assumptions (integer factorization, discrete log problem). These are not proven axioms — they are empirical observations about current algorithms and quantum vulnerability. The false summit risk here is high: conflating 'computationally hard today' with 'mathematically impossible.' Post-quantum cryptography research proves this is mutable. ε=0.38, suppression=0.62 violate mountain thresholds. Engine correctly classifies as Tangled Rope, not Mountain.
constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational view, E2EE is a hybrid mechanism: it enables privacy coordination (genuine coordination benefit to individuals) but simultaneously extracts from surveillance-dependent institutions and constrains law enforcement. The constraint is structurally mutable (PQC will change the game), hence not a mountain. Requires active enforcement (cryptographic standards bodies maintain the protocol). d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.30. Balanced extraction and coordination.
constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(e2ee_digital_privacy_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(e2ee_digital_privacy_2026, TR),
    TR >= 0.70.

:- end_tests(e2ee_digital_privacy_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. E2EE imposes meaningful costs on surveillance institutions (law enforcement, state actors, corporate data harvesters) but is not maximally extractive because: (1) metadata, network traffic, and device-level compromise provide partial surveillance alternatives; (2) E2EE is voluntarily adopted, not coercively imposed; (3) enforcement requires cryptographic standards bodies rather than state power; (4) the constraint is mutating as quantum threats emerge. If E2EE were mandatory or if metadata alternatives were truly unavailable, extractiveness would be higher. Suppression (0.62): High. Barriers to using alternatives to E2EE include: cryptographic complexity, lack of user-friendly deployment, corporate and state disincentives for key disclosure, and international variance in legal frameworks. Not maximal suppression because open-source alternatives exist and adoption is accelerating. Theater ratio (0.58): Moderate. E2EE narrative frequently oversells privacy protection (end-to-end encryption alone does not protect metadata, device security, or behavioral patterns). The 'E2EE solves privacy' framing is partially performative. But the constraint has genuine functional content (message confidentiality against transport-layer eavesdropping), so theater is not dominant. Rising over time as threat models shift toward metadata and behavioral surveillance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is extreme, spanning nine different classifications. Individual users see pure coordination (Rope) — E2EE enables their core benefit (privacy) without imposing costs. Authoritarian surveillance apparatus sees pure extraction (Snare) — E2EE blocks their core function (mass surveillance) with no compensatory benefit. Law enforcement and platform companies see mixed Tangled Rope — they experience both coordination benefits (internal secure comms, competitive privacy marketing) and extraction costs (blocked suspect access, lost data revenue). The cryptographic community sees Scaffold — a temporary coordination mechanism with a known technical sunset (post-quantum migration). The piton perspective reveals that E2EE's narrative authority (the 'digital wall' myth) masks incomplete protection — metadata and device compromise provide surveillance alternatives. The false mountain perspective (viewing E2EE as a natural law of mathematics) is correctly rejected by the structural data: E2EE is computationally hard TODAY, but post-quantum cryptography research proves it is NOT eternally impossible. This perspectival range — from pure coordination to pure extraction to performative theater to contingent technical constraint — is exactly what Tangled Rope classification is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual users: Beneficiary + mobile → d≈0.15, f(d)≈-0.01. Net beneficiary; E2EE solves their problem without constraining them. Authoritarian state: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction; state cannot exit global E2EE deployment. Law enforcement (organized/constrained): Victim (blocked access) + constrained (cannot mandate backdoors) → d≈0.68, f(d)≈1.05. Asymmetric extraction because agencies have some alternatives (metadata) but face political constraints on backdoors. Platform companies (institutional/constrained): Mixed (beneficiary of privacy marketing + victim of data-harvesting loss) + constrained → d≈0.72, f(d)≈1.12. Extraction pressure from lost data revenue, but coordination benefit from privacy positioning. Cryptographic community (organized/constrained with exit path): Mixed + constrained but with technical exit → d≈0.35, f(d)≈0.32. Low extraction because the exit is visible (PQC migration) and the community has agency. Piton perspective (institutional): Beneficiary of narrative authority + arbitrage → d≈0.45, f(d)≈0.47. Low-moderate extraction because narrative authority is contingent on threat model stability.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY: E2EE exemplifies the 'crypto-as-pure-coordination' false consensus that leads mandatrophy. The temptation to classify E2EE as pure Rope (ε ≤ 0.45, χ ≤ 0.35, no victims) arises from tech-sector narratives that frame E2EE as a collective coordination win. But this classification: (1) ignores the state and law enforcement perspectives where E2EE IS extraction; (2) masks the power asymmetry: civilians gain privacy coordination while states lose surveillance control (not symmetric coordination); (3) oversells protection (metadata and device threats remain). The Tangled Rope classification (ε=0.38, suppression=0.62, requires_active_enforcement=true, beneficiaries=[users, advocates], victims=[law_enforcement, surveillance_apparatus]) captures the real structure: E2EE genuinely solves a coordination problem (secure communication), but SIMULTANEOUSLY extracts from surveillance institutions. This is not 'coordination with side effects' — it is structurally asymmetric. Some agents benefit from the coordination; others are victimized by the extraction. The mandatrophy is resolved by: (1) declaring both beneficiaries and victims explicitly; (2) modeling multiple perspectives that show the perspectival gap; (3) acknowledging that 'pure coordination' is false — the constraint has anti-coordination properties for surveillance institutions; (4) noting the theater ratio (0.58) indicates that narrative ('impenetrable wall') exceeds reality (metadata still leaks). If mandatrophy persists in treating E2EE as Rope, the system misses the geopolitical extraction (surveillance states lose monitoring capability) and the institutional tension (law enforcement vs privacy advocates is not symmetric coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_threat_timeline,
    'When will quantum computers achieve practical cryptanalytic capability against RSA-2048 and elliptic curve cryptography?',
    'Qubit scaling trajectory, error correction rates, and cryptanalytically relevant quantum computer (CRQC) timeline estimates from quantum hardware labs and NIST PQC migration schedules',
    'If within 10 years: E2EE mountain collapses entirely; becomes Piton immediately. If > 30 years: PQC migration is precautionary; E2EE remains stable Tangled Rope. Timeline determines whether constraint mutates vs persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_threat_timeline, empirical, 'Timeline for practical quantum cryptanalysis capability').

omega_variable(
    metadata_extraction_sufficiency,
    'Can law enforcement and state actors achieve sufficient investigative capability through metadata analysis, network traffic patterns, and device-level access without needing to break E2EE?',
    'Empirical study of closed cases where investigators succeeded via metadata-only methods vs cases requiring message content; comparison of conviction rates and investigation speed',
    'If yes: E2EE is Piton (performative, doesn''t actually protect); snare perspective overstates extraction cost. If no: E2EE represents real extraction; snare/tangled rope perspectives validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metadata_extraction_sufficiency, empirical, 'Whether metadata analysis provides sufficient investigative alternative').

omega_variable(
    authoritarian_adoption_paradox,
    'Why do authoritarian states adopt E2EE for internal communications and state apparatus while restricting it for citizens?',
    'Analysis of state deployment patterns (Signal adoption by defense ministries, military crypto standards) vs citizen restrictions; game-theoretic modeling of elite security vs population control',
    'Reveals that authoritarian classification (Snare from state view) is contingent on power asymmetry, not on E2EE itself. If true mutual adoption occurs, constraint becomes Rope-like. Explains ''trap the population, liberate the state'' pattern.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authoritarian_adoption_paradox, conceptual, 'Differential adoption by state apparatus vs population').

omega_variable(
    compelled_decryption_legal_frontier,
    'Will courts and legislatures enforce mandatory key disclosure or backdoor access, or will E2EE source code protection (as speech) hold constitutional ground?',
    'Precedent accumulation in Riley v CA, Carpenter v US, and emerging legislation (GDPR Article 32, UK PCIPA amendments); comparative constitutional analysis across jurisdictions',
    'If backdoors mandated: E2EE becomes Scaffold with enforced sunset. If source code protected: E2EE remains Tangled Rope. If bifurcated (legal variance by jurisdiction): E2EE splinters into multiple constraint stories per region.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compelled_decryption_legal_frontier, preference, 'Constitutional/legislative trajectory for key disclosure mandates').

omega_variable(
    alternative_threat_model_sufficiency,
    'Do emerging threat models (AI-assisted surveillance, biometric identification, behavioral pattern recognition) make E2EE message privacy irrelevant compared to device-level and behavioral-level compromise?',
    'Comparative effectiveness studies: message confidentiality vs device compromise vs behavioral inference; testing whether state surveillance can achieve equivalent investigative outcome without message access',
    'If yes: E2EE becomes Piton (security theater). If no: E2EE remains functionally important. Determines whether constraint mutates from Tangled Rope to Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_threat_model_sufficiency, empirical, 'Relative threat significance of message confidentiality vs alternative vectors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(e2ee_digital_privacy_2026, 1977, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(e2ee_tr_t0, e2ee_digital_privacy_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(e2ee_tr_t13, e2ee_digital_privacy_2026, theater_ratio, 13, 0.48).
narrative_ontology:measurement(e2ee_tr_t26, e2ee_digital_privacy_2026, theater_ratio, 26, 0.58).

% Extraction over time
narrative_ontology:measurement(e2ee_be_t0, e2ee_digital_privacy_2026, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(e2ee_be_t13, e2ee_digital_privacy_2026, base_extractiveness, 13, 0.28).
narrative_ontology:measurement(e2ee_be_t26, e2ee_digital_privacy_2026, base_extractiveness, 26, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(e2ee_digital_privacy_2026, information_standard).
narrative_ontology:affects_constraint(e2ee_digital_privacy_2026, metadata_extraction_dragnet).
narrative_ontology:affects_constraint(e2ee_digital_privacy_2026, quantum_cryptanalysis_capability).
narrative_ontology:affects_constraint(e2ee_digital_privacy_2026, device_level_compromise_resilience).
narrative_ontology:affects_constraint(e2ee_digital_privacy_2026, regulatory_capture_backdoor_mandates).

% DUAL FORMULATION NOTE:
% E2EE as a digital constraint decomposes into at least three structurally distinct claims: (1) Message confidentiality against transport-layer eavesdropping (ε≈0.08, Mountain-like if quantum-resistant; currently Rope); (2) Privacy coordination enabling secure communication (ε≈0.12, Rope); (3) Extraction from surveillance institutions (ε≈0.62, Snare from state perspective). The combined story (ε=0.38, Tangled Rope) is the union. Network edges show upstream dependencies: quantum cryptanalysis capability will mutate E2EE from Tangled Rope to either Mountain (if PQC holds) or Piton (if post-quantum assumptions fail). Device compromise resilience determines whether metadata extraction makes E2EE functionally obsolete (shifting to Piton). Regulatory mandates for backdoors could enforce Scaffold sunset clause (temporary E2EE with enforced transition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(e2ee_digital_privacy_2026, institutional, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
