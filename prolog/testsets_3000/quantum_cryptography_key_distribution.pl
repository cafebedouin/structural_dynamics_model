% ============================================================================
% CONSTRAINT STORY: quantum_cryptography_key_distribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_cryptography_key_distribution, []).

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
 *   constraint_id: quantum_cryptography_key_distribution
 *   human_readable: Quantum Key Distribution Infrastructure Coordination and Extraction
 *   domain: cryptography/quantum_information/infrastructure
 *
 * SUMMARY:
 *   Quantum key distribution represents a structural constraint combining
 *   genuine cryptographic innovation with infrastructure lock-in, vendor
 *   capture, and developing-economy dependency. The constraint exhibits the
 *   full tension between coordination function (solving post-quantum
 *   cryptographic vulnerability) and extraction function (forcing costly
 *   migration, centralizing network architecture, creating vendor
 *   monopolies). QKD's theoretical unconditional security guarantee creates a
 *   powerful legitimacy narrative ('quantum-level' security), but practical
 *   implementations exhibit vulnerabilities (side-channel attacks, detector
 *   loopholes) that undermine claimed advantage over simpler post-quantum
 *   classical cryptography. The constraint's extractiveness has increased
 *   over the measurement interval as deployment mandates have propagated and
 *   alternative quantum communication approaches have been systematically
 *   deprioritized in funding and standards-setting. The theater ratio (0.62)
 *   reflects substantial performative content: security metrics emphasize
 *   deployment kilometers and government contracts rather than demonstrable
 *   security improvement; NIST post-quantum cryptography standardization has
 *   advanced competing classical approaches with equivalent security at lower
 *   complexity, yet QKD dominates policy framing.
 *
 * KEY AGENTS:
 *   - QKD Technology Vendors: Primary beneficiaries (institutional/arbitrage) — capture market dominance and revenue during infrastructure transition; include Chinese, European, and US vendors with different regional lock-in strategies
 *   - Non-Quantum-Secure Users: Primary victims (powerless/trapped) — face mandatory migration costs without meaningful choice as policy mandates QKD adoption
 *   - National Security Agencies: Secondary beneficiary (institutional/constrained) — gain cryptographic mandate (genuine coordination) plus surveillance infrastructure consolidation and vendor control (extraction)
 *   - Developing Economy Telecommunications Sector: Secondary victim (moderate/constrained) — forced to adopt expensive foreign technology without capacity for independent development; dual extraction from cost and dependency
 *   - Legacy Infrastructure Administrators: Moderate victim (moderate/constrained) — experience real coordination benefit (post-quantum security) plus significant transition costs and vendor lock-in
 *   - Post-Quantum Cryptography Standards Body: Organized actor (organized/constrained) — developing classical alternatives with sunset logic; provide countervailing force to QKD monopoly
 *   - Quantum Information Science Funding Apparatus: Institutional maintainer (institutional/arbitrage) — sustains QKD through research funding momentum and narrative framing despite advancing alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_cryptography_key_distribution, 0.38).
domain_priors:suppression_score(quantum_cryptography_key_distribution, 0.48).
domain_priors:theater_ratio(quantum_cryptography_key_distribution, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_cryptography_key_distribution, extractiveness, 0.38).
narrative_ontology:constraint_metric(quantum_cryptography_key_distribution, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(quantum_cryptography_key_distribution, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_cryptography_key_distribution, tangled_rope).
narrative_ontology:human_readable(quantum_cryptography_key_distribution, "Quantum Key Distribution Infrastructure Coordination and Extraction").
narrative_ontology:topic_domain(quantum_cryptography_key_distribution, "cryptography/quantum_information/infrastructure").

domain_priors:requires_active_enforcement(quantum_cryptography_key_distribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_cryptography_key_distribution, qkd_technology_vendors).
narrative_ontology:constraint_beneficiary(quantum_cryptography_key_distribution, national_security_agencies).
narrative_ontology:constraint_beneficiary(quantum_cryptography_key_distribution, early_adopter_institutions).
narrative_ontology:constraint_victim(quantum_cryptography_key_distribution, non_quantum_secure_users).
narrative_ontology:constraint_victim(quantum_cryptography_key_distribution, legacy_infrastructure_administrators).
narrative_ontology:constraint_victim(quantum_cryptography_key_distribution, developing_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-QUANTUM-SECURE USER (SNARE) — Faces mandatory migration pressure without meaningful choice. As quantum computing threat timeline accelerates and policy mandates QKD deployment, users dependent on legacy infrastructure face escalating costs and incompatibility. No exit option: continue using deprecated systems (increasing risk) or pay for costly replacement infrastructure. Maximum suppression through regulatory harmonization pushing toward QKD as the only 'secure' path forward.
constraint_indexing:constraint_classification(quantum_cryptography_key_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LEGACY INFRASTRUCTURE ADMINISTRATOR (TANGLED ROPE) — Experiences both genuine coordination benefit (QKD solves real cryptographic vulnerability) and extraction (high replacement costs, operational complexity, vendor lock-in). Can transition but at significant resource cost and operational disruption. Coordination function is real — QKD addresses post-quantum cryptographic threat — but extraction is embedded in the transition pathway.
constraint_indexing:constraint_classification(quantum_cryptography_key_distribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: QKD TECHNOLOGY VENDOR (ROPE) — Primary beneficiary experiencing the constraint as pure coordination. Vendors solve the key distribution problem while capturing market share and revenue during deployment window. Arbitrage exit option: can pivot to other quantum technologies if QKD adoption slows. Net experience is coordination with favorable position — extraction runs toward this agent through vendor selection and ecosystem lock-in.
constraint_indexing:constraint_classification(quantum_cryptography_key_distribution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NATIONAL SECURITY AGENCY (TANGLED ROPE) — Coordination function: genuinely addresses post-quantum cryptographic threat to classified communications. Extraction function: uses cryptographic mandate to consolidate surveillance infrastructure, standardize telecommunications architecture under security rubric, and create vendor dependencies. High enforcement capability creates coordination-through-coercion dynamic. Constrained exit due to security mandates — cannot simply abandon QKD deployment once committed.
constraint_indexing:constraint_classification(quantum_cryptography_key_distribution, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVELOPING ECONOMY TELECOMMUNICATIONS SECTOR (SNARE) — Faces dual extraction: must adopt QKD infrastructure (high capital cost) to maintain interoperability with developed-economy networks, but lacks resources for independent technology development. Technology procurement forces dependency on vendors from developed economies. Constrained exit: can delay but cannot opt out without network isolation. Theater ratio high — security rhetoric masks infrastructure control and vendor lock-in.
constraint_indexing:constraint_classification(quantum_cryptography_key_distribution, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: POST-QUANTUM CRYPTOGRAPHY STANDARDS BODY (SCAFFOLD) — Organized agents (NIST, ETSI, ISO working groups) see QKD as temporary coordination solution: quantum-resistant classical cryptography standards are advancing as alternative to quantum-dependent key distribution. Sunset logic: as post-quantum classical algorithms mature and deployment accelerates, QKD's monopoly on 'quantum-safe' framing decays. Constrained by NIST post-quantum standardization timeline (2022-2024 algorithm selection). Theater ratio declining as alternatives become visible.
constraint_indexing:constraint_classification(quantum_cryptography_key_distribution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: QUANTUM INFORMATION SCIENCE FUNDING APPARATUS (PITON) — Maintains QKD as flagship quantum technology through substantial research funding, conference prioritization, and narrative framing, despite competing quantum communication approaches (entanglement swapping, measurement-device-independent QKD) showing comparable security at lower cost. Theater ratio high: QKD deployment metrics (kilometers of fiber, government contracts) substitute for genuine security outcome measurement. Institutional inertia maintains funding momentum; functional verification increasingly performative.
constraint_indexing:constraint_classification(quantum_cryptography_key_distribution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / PHYSICAL LAW VIEW (MOUNTAIN) — From universalizable perspective, quantum key distribution solves a fundamental cryptographic problem: key distribution without pre-shared secrets or trusted channels. The Bell inequality violation enables unconditional security guarantee impossible in classical cryptography. This appears as immutable natural law. However, empirical reality contradicts: implementation vulnerabilities (side-channel attacks, detector loopholes), practical complexity, and cost make QKD less secure in real deployment than simpler post-quantum classical methods. The mountain classification reveals false summit — physics naturalizes what is contingent engineering practice.
constraint_indexing:constraint_classification(quantum_cryptography_key_distribution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_cryptography_key_distribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quantum_cryptography_key_distribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quantum_cryptography_key_distribution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(quantum_cryptography_key_distribution, TR),
    TR >= 0.70.

:- end_tests(quantum_cryptography_key_distribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. QKD deployment extracts value through vendor pricing, infrastructure switching costs, and network lock-in, but the extraction is legitimated by genuine post-quantum cryptographic threat. The revised assessment (down from initial 0.45) reflects that the coordination function is structurally real — quantum computing threat requires cryptographic remediation — and post-quantum classical alternatives, while viable, require their own transition costs. The extraction premium over classical approaches is moderate because both paths involve cost; QKD's additional cost is partly justified by security gain, partly by infrastructure control. Suppression (0.48): Moderate-high. Significant barriers to alternative approaches include narrative dominance (QKD framed as 'quantum-safe,' classical as 'transitional'), funding concentration favoring QKD research, and standards-setting bodies prioritizing QKD deployment. But suppression is not total — NIST post-quantum standardization is advancing, alternative quantum communication approaches exist, and some institutions deploy classical post-quantum algorithms. Theater ratio (0.62): Moderate-high. QKD deployment emphasizes security narrative and kilometer-of-fiber metrics while downplaying practical implementation vulnerabilities (side-channel attacks, detector loopholes) and cost comparisons to post-quantum classical methods. The theater has increased as deployment has accelerated — performance claims have become more absolutist while empirical evidence remains contested.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. QKD vendors see pure coordination (Rope) with market opportunity — they are solving a real problem while capturing value. National security agencies see mixed coordination-extraction (Tangled Rope) — genuine cryptographic mandate plus infrastructure control. Developing economies see pure extraction (Snare) — forced adoption without resources or alternatives. Post-quantum standards bodies see temporary coordination with sunset (Scaffold) — classical alternatives are maturing, QKD dominance is contingent on policy choices, not technical necessity. The quantum funding apparatus sees its own infrastructure as essential (Piton) — maintains deployment momentum through research prioritization despite advancing alternatives. The civilizational analytical observer risks seeing physical necessity (Mountain) — quantum mechanics guarantees unconditional security — but empirical reality reveals this as false summit: practical vulnerabilities, classical alternatives providing equivalent real-world security, and deployment urgency disconnected from threat timeline. The perspectival gap reveals that QKD's mandatory framing relies on naturalizing policy choices as physical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation differs significantly across perspectives. QKD vendors (beneficiary + arbitrage) derive low d → negative χ (they experience the constraint as beneficial coordination). Non-quantum-secure users (victim + trapped) derive high d → high χ (maximum extraction experienced). National security agencies derive moderate d (beneficiary via security mandate + constrained exit via deployment commitment) → moderate χ reflecting coordination-with-control. Developing economies derive high d (victim + constrained exit) → high χ reflecting dual extraction from cost and dependency lock-in. Post-quantum standards body derives lower d (organized agents with exit paths through classical alternatives) → moderate χ reflecting agency and sunset mechanism. The analytical observer at civilizational scope risks deriving natural-law classification (quantum physics guarantees unconditional security) but structural data shows false summit: practical vulnerabilities, cost disadvantage vs alternatives, and deployment urgency disconnected from actual threat timeline.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in QKD constraints arises from confusion between theoretical unconditional security (valid) and practical implementation security (contested). The theoretical guarantee suggests mountain — QKD solves a fundamental cryptographic problem quantum mechanics makes impossible classically. But implementation vulnerabilities (detector loopholes, side-channel attacks, environmental noise) undermine practical superiority. Post-quantum classical cryptography provides comparable real-world security at lower complexity, yet QKD dominates policy and funding narratives. The mandatrophy resolves by decomposing: (1) Cryptographic threat from quantum computing: genuine coordination problem, justified urgency. (2) QKD as specific solution: contingent choice among viable alternatives, subject to extraction through vendor lock-in and network effects. The first requires policy response; the second enables extraction. Conflating them ('quantum-safe = QKD') naturalizes what is a contingent technology choice. The scaffold perspective (post-quantum standards body) provides exit path — as classical alternatives mature and deployment accelerates, QKD's monopoly on 'quantum-safe' framing erodes. The snare perspective reveals who bears extraction costs (powerless users, developing economies) while vendors (institutional beneficiaries) capture value. The constraint's true structure is not immutable law but policy choice with asymmetric cost distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    post_quantum_classical_equivalence,
    'Do post-quantum classical cryptographic algorithms provide equivalent or superior security to QKD at lower implementation complexity and cost?',
    'Comparative security analysis of NIST post-quantum finalists vs deployed QKD systems; real-world vulnerability discovery rates; implementation-weighted security metrics accounting for side-channel risks',
    'If classical superior: QKD extraction mechanism becomes apparent, scaffold sunset accelerates, snare classification strengthens for forced adopters. If QKD genuinely superior: coordination function dominates, tangled rope becomes more rope-like, extraction burden justified by security gain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_quantum_classical_equivalence, empirical, 'Whether post-quantum classical cryptography provides equivalent security to QKD').

omega_variable(
    practical_implementation_vulnerability,
    'What proportion of QKD security claims rely on theoretical unconditional security versus practical implementation details (detector efficiency, environmental noise, channel calibration)?',
    'Systematic audit of deployed QKD systems for side-channel attacks, detector vulnerabilities, and implementation gaps; comparison of theory-to-practice security margin for QKD vs classical post-quantum methods',
    'If implementation vulnerabilities significant: theater ratio should be higher, mountain perspective becomes false summit. If theoretical guarantees reliably translate to practice: security extraction becomes legitimate coordination, tangled rope shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_implementation_vulnerability, empirical, 'Vulnerability gap between theoretical QKD security and practical implementation').

omega_variable(
    network_interoperability_lock_in,
    'Is QKD infrastructure lock-in primarily driven by genuine network effects (interoperability standards) or by vendor ecosystem capture and switching costs?',
    'Historical analysis of QKD standard-setting (China dominance in fibers, Europe in networks, US in algorithms); comparative cost of deploying alternative quantum communication approaches; network effect strength vs proprietary lock-in quantification',
    'If network effects dominant: constraints on developing economies are coordination-driven (justified extraction). If vendor lock-in dominant: snare classification strengthens, developing-economy perspective becomes more severe, mandatrophy analysis shifts toward asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_interoperability_lock_in, empirical, 'Whether QKD lock-in is driven by network effects or vendor capture').

omega_variable(
    quantum_computing_threat_timeline,
    'What is the credible timeline for fault-tolerant quantum computers capable of breaking RSA-2048? How does this timeline compare to QKD deployment timescales?',
    'Consensus estimation from quantum computing roadmaps (IBM, Google, IonQ, others); sensitivity analysis on error correction requirements; correlation between threat timeline and current QKD deployment urgency messaging',
    'If threat timeline > 20 years: current QKD deployment urgency is overblown, extraction mechanism becomes apparent, snare classification strengthened. If threat timeline < 10 years: deployment urgency justified, coordination function genuine, tangled rope reflects real security need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_computing_threat_timeline, empirical, 'Timeline for cryptographically relevant quantum computing threat').

omega_variable(
    measurement_device_independent_qkd_viability,
    'Can measurement-device-independent QKD or other alternative quantum communication approaches achieve comparable unconditional security guarantees with simpler infrastructure and lower cost than standard QKD?',
    'Comparative deployment cost analysis; security proof equivalence for MDI-QKD; practical infrastructure requirements (fiber distance, repeaters, detector complexity)',
    'If MDI-QKD viable: current QKD dominance appears contingent, scaffold sunset becomes plausible, theater ratio reflects technology choice rather than necessity. If MDI-QKD inferior: current approach justified, extraction becomes embedded in natural limitation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_device_independent_qkd_viability, empirical, 'Viability of measurement-device-independent alternatives to standard QKD').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_cryptography_key_distribution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qckd_tr_t0, quantum_cryptography_key_distribution, theater_ratio, 0, 0.4).
narrative_ontology:measurement(qckd_tr_t5, quantum_cryptography_key_distribution, theater_ratio, 5, 0.52).
narrative_ontology:measurement(qckd_tr_t10, quantum_cryptography_key_distribution, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(qckd_be_t0, quantum_cryptography_key_distribution, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(qckd_be_t5, quantum_cryptography_key_distribution, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(qckd_be_t10, quantum_cryptography_key_distribution, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_cryptography_key_distribution, global_infrastructure).
narrative_ontology:affects_constraint(quantum_cryptography_key_distribution, post_quantum_cryptography_deployment).
narrative_ontology:affects_constraint(quantum_cryptography_key_distribution, telecommunications_infrastructure_standardization).
narrative_ontology:affects_constraint(quantum_cryptography_key_distribution, national_cryptographic_mandate_systems).

% DUAL FORMULATION NOTE:
% QKD deployment constraint is downstream of quantum computing threat assessment but represents structurally distinct constraint. Upstream: cryptographic vulnerability from quantum computing — genuine coordination problem. Downstream: QKD as specific technological solution — subject to vendor lock-in and developing-economy extraction. These decompose into separate stories: quantum_threat_cryptography_gap (ε=0.12, coordination problem) influences qkd_infrastructure_extraction (ε=0.38, technology-specific constraint). Post-quantum classical alternatives provide parallel pathway with different extraction profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_cryptography_key_distribution, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
