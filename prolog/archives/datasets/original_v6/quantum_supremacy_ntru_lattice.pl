% ============================================================================
% CONSTRAINT STORY: quantum_supremacy_ntru_lattice
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_supremacy_ntru_lattice, []).

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
 *   constraint_id: quantum_supremacy_ntru_lattice
 *   human_readable: Quantum Supremacy via NTRU Lattice Cryptography Claims
 *   domain: quantum_computing/cryptography/post_quantum_security
 *
 * SUMMARY:
 *   The claim that NTRU lattice-based cryptography provides 'quantum
 *   supremacy' creates a structural tension between legitimate post-quantum
 *   standardization needs and vendor extraction via unverified technical
 *   claims. Quantum computers will eventually break RSA/ECC (high
 *   confidence), creating genuine coordination problem: infrastructure
 *   worldwide needs post-quantum alternatives. NTRU lattice cryptography is a
 *   mathematically plausible candidate, but vendor claims of its supremacy
 *   are often framed as marketing rather than peer-reviewed cryptographic
 *   assessment. The constraint exhibits characteristics of tangled rope:
 *   genuine coordination function (need for standardized post-quantum
 *   solutions) coupled with asymmetric extraction (vendors benefit from
 *   urgency and asymmetric verification burden). The theater ratio (0.68)
 *   reflects that much of the 'quantum supremacy' rhetoric consists of white
 *   papers and vendor claims with limited independent cryptanalytic scrutiny
 *   comparable to the decades of attacks on RSA/ECC. Theater has increased
 *   over the measurement interval as vendor marketing has escalated while
 *   independent verification mechanisms have lagged. Extractiveness (0.58)
 *   reflects moderate but non-trivial beneficiary advantage: quantum
 *   computing vendors and lattice cryptography researchers who promoted NTRU
 *   gain career, funding, and commercial advantage during the standardization
 *   window before independent verification catches up.
 *
 * KEY AGENTS:
 *   - Cryptographic Infrastructure Security: Primary victim (powerless/trapped) — must assume quantum threat is real despite unresolved NTRU verification; cannot exit or negotiate terms
 *   - Post-Quantum Standardization Bodies: Institutional actor (organized/constrained) — NIST, ETSI facing genuine coordination problem (need standards) alongside vendor extraction pressure; constrained by urgency and geopolitical factors
 *   - Quantum Computing Vendors: Primary beneficiary (institutional/arbitrage) — capture commercial and research advantage from NTRU adoption; can arbitrage between competing approaches and exit to alternative markets
 *   - Cryptographic Engineers: Secondary victim (moderate/constrained) — must implement and audit NTRU claims; face technical and career risk; constrained by job requirements and organizational decisions
 *   - Academic Cryptanalysis Community: Organized agents (powerful/mobile) — providing temporary verification function (cryptanalysis pressure) with sunset as standards mature and formal security assessment is completed
 *   - Legacy Cryptographic Standards: Institutional (institutional/arbitrage) — persist through inertia despite functional obsolescence; maintain RSA/ECC standards while building post-quantum alternatives
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing quantum threat as immutable law rather than examining institutional mechanisms driving extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_supremacy_ntru_lattice, 0.58).
domain_priors:suppression_score(quantum_supremacy_ntru_lattice, 0.52).
domain_priors:theater_ratio(quantum_supremacy_ntru_lattice, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_supremacy_ntru_lattice, extractiveness, 0.58).
narrative_ontology:constraint_metric(quantum_supremacy_ntru_lattice, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(quantum_supremacy_ntru_lattice, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_supremacy_ntru_lattice, tangled_rope).
narrative_ontology:human_readable(quantum_supremacy_ntru_lattice, "Quantum Supremacy via NTRU Lattice Cryptography Claims").
narrative_ontology:topic_domain(quantum_supremacy_ntru_lattice, "quantum_computing/cryptography/post_quantum_security").

domain_priors:requires_active_enforcement(quantum_supremacy_ntru_lattice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_supremacy_ntru_lattice, quantum_computing_vendors).
narrative_ontology:constraint_beneficiary(quantum_supremacy_ntru_lattice, lattice_cryptography_researchers).
narrative_ontology:constraint_victim(quantum_supremacy_ntru_lattice, cryptographic_infrastructure_security).
narrative_ontology:constraint_victim(quantum_supremacy_ntru_lattice, post_quantum_standardization_process).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CRYPTOGRAPHIC INFRASTRUCTURE SECURITY (SNARE) — Cannot exit the quantum supremacy claim environment. Infrastructure operators must assume worst-case scenarios (quantum computers breaking RSA/ECC) despite lack of independent verification that NTRU lattice claims are legitimate. Bears full cost of premature migration or security degradation. No advocacy mechanism, no exit option. Maximum extraction experienced.
constraint_indexing:constraint_classification(quantum_supremacy_ntru_lattice, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: POST-QUANTUM STANDARDIZATION BODIES (TANGLED ROPE) — NIST, ETSI, and international bodies face coordination problem (genuine need for standardized post-quantum algorithms) alongside extractive pressure from vendors promoting unverified claims. Constrained by geopolitical competition, security urgency, and vendor influence. Experience both coordination benefits and asymmetric extraction.
constraint_indexing:constraint_classification(quantum_supremacy_ntru_lattice, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: QUANTUM COMPUTING VENDORS (ROPE) — Benefit from coordination around post-quantum cryptography as existential infrastructure need. Can arbitrage between NTRU claims, competing lattice approaches, and commercial advantage. Experience the constraint as enabling their market position rather than extractive.
constraint_indexing:constraint_classification(quantum_supremacy_ntru_lattice, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CRYPTOGRAPHIC ENGINEERS (TANGLED ROPE) — Constrained by job requirements to implement post-quantum algorithms and audit NTRU claims. Face technical risk (deploying unverified algorithms) and career risk (maintaining legacy systems vs. migrating). Benefit from coordination around standardized solutions but pay cost of uncertainty and frequent standard revisions.
constraint_indexing:constraint_classification(quantum_supremacy_ntru_lattice, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ACADEMIC CRYPTANALYSIS COMMUNITY (SCAFFOLD) — Powerful agents (universities, independent researchers) with mobile exit options. Can choose research directions and funding. Temporary support function: cryptanalytic pressure on NTRU lattice claims is building independent verification mechanisms. Sunset clause: as formal quantum-resistant cryptography standards mature and deployment begins, the pressure for rapid academic validation decreases. Theater ratio is moderate because cryptanalytic research produces legitimate mathematical results alongside performative security arguments.
constraint_indexing:constraint_classification(quantum_supremacy_ntru_lattice, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY CRYPTOGRAPHIC STANDARDS (PITON) — RSA/ECC standardization bodies persist through institutional inertia despite functional obsolescence under quantum threat. Maintenance of legacy standards is largely performative — governments and corporations continue deploying them while simultaneously building post-quantum infrastructure. The constraint persists due to sunk costs and organizational identity, not functional verification.
constraint_indexing:constraint_classification(quantum_supremacy_ntru_lattice, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational timescale, quantum computers will mathematically break RSA/ECC. This appears as an immutable physical law. However, structural data contradicts mountain classification: NTRU supremacy claims are institutional constructs (vendor marketing, funding incentives, publication bias), not laws of physics. The engine will detect this as false summit — naturalization of contingent technical claims.
constraint_indexing:constraint_classification(quantum_supremacy_ntru_lattice, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_supremacy_ntru_lattice_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quantum_supremacy_ntru_lattice, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quantum_supremacy_ntru_lattice, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_supremacy_ntru_lattice, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quantum_supremacy_ntru_lattice, TR),
    TR >= 0.70.

:- end_tests(quantum_supremacy_ntru_lattice_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Vendors promoting NTRU lattice achieve asymmetric benefit during the standardization window (2018-2025) while cryptanalytic verification is ongoing. The benefit is not maximal (0.72+) because post-quantum standardization is a legitimate infrastructure problem and NTRU is a mathematically credible candidate. However, the extraction is above the pure coordination threshold (0.45) because vendor marketing often claims supremacy without equivalent peer-reviewed cryptanalytic support compared to RSA/ECC. Suppression (0.52): Moderate. Barriers to independent verification include specialized expertise requirements, computational complexity of lattice cryptanalysis, and publication bias favoring positive results. Standardization urgency creates time pressure that suppresses thorough independent assessment. However, suppression is not total — academic cryptanalysis continues and competing approaches are being evaluated. Theater ratio (0.68): High and increasing. Much of the 'quantum supremacy' rhetoric appears in vendor white papers, conference marketing, and funding narratives rather than peer-reviewed cryptographic journals. This represents performative security argument rather than rigorous cryptanalytic assessment. The interval measurement shows theater increasing from 0.42 to 0.68, indicating that marketing intensity has outpaced independent verification.
 *
 * PERSPECTIVAL GAP:
 *   Original research group sees coordination (Rope) — they solve the legitimate problem of providing post-quantum alternatives. Academic cryptanalysis community sees temporary support function (Scaffold) — their scrutiny pressure will eventually produce verified standards. Standardization bodies see mixed coordination and extraction (Tangled Rope) — genuine need for standards coupled with vendor pressure. Cryptographic engineers see constrained extraction (Tangled Rope) — implementing solutions that may not yet be fully verified. Cryptographic infrastructure sees pure extraction (Snare) — must assume threat and migrate without independent verification. The analytical observer risks seeing natural law (Mountain) — quantum computers will break RSA — but the structural data reveals this as conflating physical fact with institutional mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural positions relative to the NTRU supremacy constraint. Vendors with arbitrage options and beneficiary status experience negative effective extraction (they benefit). Standardization bodies with constrained options and mixed beneficiary/victim status experience moderate extraction. Cryptographic infrastructure with trapped status and victim position experiences maximum extraction (d ≈ 0.95). Academic cryptanalysts with powerful status and mobile exit options experience low extraction despite victim classification (they have alternative funding and research directions). The constraint's asymmetry emerges from the temporal mismatch: vendors capture advantage during standardization window while verification lags; after verification completes, advantage equalizes. Beneficiary declarations identify vendors and lattice researchers; victim declarations identify infrastructure security and standardization integrity.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves by distinguishing physical fact from institutional mechanism. Physical fact: quantum computers mathematically break RSA/ECC (Mountain, ε ≤ 0.25). Institutional mechanism: vendor extraction via marketing of unverified NTRU claims (Tangled Rope, ε = 0.58). The mandatrophy dissolves when analyzed separately. Vendors benefit from conflating the two (creating urgency that suppresses verification). The standardization process should treat them separately: assume quantum threat is real → commit to post-quantum migration (necessary), but require independent cryptanalytic verification before standardizing NTRU → reduce extraction window. The current constraint exhibits extraction because standardization timeline creates asymmetry: vendors benefit from urgency, infrastructure bears cost of uncertainty. Resolution path: extend standardization timelines to allow equivalent cryptanalytic scrutiny (reduce suppression), publish vendor claims and competing approaches in peer-reviewed venues (reduce theater), conduct independent verification before mandatory migration (reduce extraction). Post-resolution: constraint should classify as pure coordination (Rope) — NTRU as verified post-quantum option among several equally vetted alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ntru_classical_hardness_assumption,
    'Is NTRU lattice hardness a proven mathematical property or an empirical assumption based on current classical algorithms?',
    'Formal proof of NTRU hardness equivalence to known hard problems (e.g., Shortest Vector Problem). Comparison of quantum-resistant proofs vs classical hardness assumptions.',
    'If proven: NTRU is mathematically rigorous (moves toward Mountain). If empirical assumption: vulnerability to future classical algorithms cannot be ruled out (remains Snare/Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ntru_classical_hardness_assumption, empirical, 'Whether NTRU hardness is proven or empirical assumption').

omega_variable(
    quantum_supremacy_timeline_verification,
    'What is the credible timeline for quantum computers capable of breaking 2048-bit RSA, and how does it compare to cryptographic migration timelines?',
    'Technical analysis of quantum computing roadmaps (gate counts, error rates, scaling laws). Comparison of vendor claims vs. published estimates from independent quantum physics research.',
    'If timeline > 20 years: NTRU migration is contingent coordination problem (Rope/Scaffold). If timeline < 10 years: urgency drives extraction (Snare/Tangled Rope). If timeline is uncertain: entire constraint is predicated on unresolved technical claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_supremacy_timeline_verification, empirical, 'Timeline for quantum computers breaking RSA').

omega_variable(
    lattice_cryptanalysis_state_of_art,
    'Have lattice-based cryptosystems (including NTRU) been subjected to equivalent cryptanalytic pressure as RSA/ECC over equivalent time periods?',
    'Historical comparison of cryptanalytic research effort: person-years spent, publications, attacks found and patched for each system. Temporal adjustment for relative maturity.',
    'If NTRU has equivalent scrutiny: extraction pressure is justified by legitimate standardization need. If NTRU has less scrutiny: vendors benefit from asymmetric verification burden (increases chi). Affects classification of standardization bodies'' perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lattice_cryptanalysis_state_of_art, empirical, 'Whether lattice cryptosystems have equivalent cryptanalytic scrutiny').

omega_variable(
    vendor_supremacy_claim_verification,
    'Are vendor claims of ''quantum supremacy via NTRU'' marketing narratives or mathematically grounded claims with peer-reviewed supporting evidence?',
    'Audit of vendor white papers and patents. Cross-reference with peer-reviewed cryptographic literature. Identification of reviewed vs. unreviewed claims.',
    'If peer-reviewed claims with supporting evidence: extraction is bounded (Tangled Rope). If primarily marketing (unreviewed white papers): pure extraction mechanism (Snare). High impact on theater_ratio and suppression scoring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_supremacy_claim_verification, empirical, 'Vendor quantum supremacy claims vs peer-reviewed evidence').

omega_variable(
    post_quantum_standardization_capture,
    'To what degree has NIST/international standardization been influenced by vendor interests vs. independent cryptographic assessment?',
    'Analysis of NIST Round voting patterns, industry representation on standards committees, patent licensing terms for selected algorithms.',
    'If vendor influence is marginal: standardization is legitimate coordination (Rope). If vendor influence is substantial: standardization process exhibits regulatory capture (increases χ toward Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_quantum_standardization_capture, conceptual, 'Degree of vendor influence on post-quantum standardization process').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_supremacy_ntru_lattice, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qsntru_tr_t0, quantum_supremacy_ntru_lattice, theater_ratio, 0, 0.42).
narrative_ontology:measurement(qsntru_tr_t3, quantum_supremacy_ntru_lattice, theater_ratio, 3, 0.55).
narrative_ontology:measurement(qsntru_tr_t6, quantum_supremacy_ntru_lattice, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(qsntru_be_t0, quantum_supremacy_ntru_lattice, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(qsntru_be_t3, quantum_supremacy_ntru_lattice, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(qsntru_be_t6, quantum_supremacy_ntru_lattice, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_supremacy_ntru_lattice, information_standard).
narrative_ontology:affects_constraint(quantum_supremacy_ntru_lattice, rsa_ecc_cryptographic_dependency).
narrative_ontology:affects_constraint(quantum_supremacy_ntru_lattice, post_quantum_migration_timeline).
narrative_ontology:affects_constraint(quantum_supremacy_ntru_lattice, lattice_cryptanalysis_validation).

% DUAL FORMULATION NOTE:
% The quantum supremacy claim decomposes into two structurally distinct constraints: (1) quantum_threat_to_rsa_ecc (physical/mathematical, Mountain, ε ≤ 0.25) — quantum computers mathematically break current cryptography; (2) ntru_vendor_extraction (institutional, Tangled Rope, ε = 0.58) — vendor marketing of unverified NTRU during standardization window. The quantum threat creates genuine standardization coordination problem (Rope upstream); vendor extraction via unverified supremacy claims creates Tangled Rope (downstream). The constraints interact: vendor extraction is powered by threat legitimacy, but threat does not justify unverified claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_supremacy_ntru_lattice, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
