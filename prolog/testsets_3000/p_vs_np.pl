% ============================================================================
% CONSTRAINT STORY: p_vs_np
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_p_vs_np, []).

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
 *   constraint_id: p_vs_np
 *   human_readable: The P versus NP Problem
 *   domain: technological/mathematics
 *
 * SUMMARY:
 *   The P versus NP problem is a constraint that operates simultaneously as a
 *   mathematical open question, a cryptographic foundation, an economic
 *   bottleneck, and an institutional status symbol. The constraint exhibits
 *   properties of both coordination (shared cryptographic assumptions) and
 *   extraction (those dependent on NP-hardness bear permanent costs). The
 *   unresolved status is itself the constraint: neither proof nor disproof is
 *   available, leaving optimization-dependent industries in a state of
 *   permanent uncertainty, while the cryptography industry builds its
 *   security infrastructure on an unvalidated assumption. The problem's
 *   theater ratio has increased over 55 years as the problem remains unsolved
 *   despite being worth a $1 million Millennium Prize — much institutional
 *   activity is now descriptive (exploring reformulations) rather than
 *   functional (advancing toward solution). Simultaneously, post-quantum
 *   cryptography research has begun architecting an exit pathway with
 *   explicit sunset logic: if the constraint's assumptions (P ≠ NP) prove
 *   false or quantum computing breaks classical crypto, migration protocols
 *   are pre-designed.
 *
 * KEY AGENTS:
 *   - Cryptography Industry: Primary beneficiary (institutional/arbitrage) — builds security models on assumption that P ≠ NP; captures trust value and pricing premium
 *   - Optimization-Dependent Sectors: Primary victim (powerless/trapped) — logistics, manufacturing, drug discovery, circuit design unable to exit exponential-time computation barriers; trapped by unresolved theoretical question
 *   - Computational Verification Infrastructure: Secondary actor (moderate/constrained) — must assume verification (NP) is faster than solving (P) without proof; enables verification-dependent systems but constrains by cryptographic security requirements
 *   - Post-Quantum Cryptography Coalition: Organized agents (organized/constrained) — NIST, security researchers, industry consortia building transition pathways with sunset logic if classical cryptography fails
 *   - Mathematical Logic Establishment: Institutional actor (institutional/arbitrage) — maintains P vs NP as iconic unsolved problem; generates prestige and research funding independent of resolution progress
 *   - Analytical Observer: Civilizational view (analytical/analytical) — questions whether P ≠ NP is a logical/physical law or contingent computational fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(p_vs_np, 0.38).
domain_priors:suppression_score(p_vs_np, 0.42).
domain_priors:theater_ratio(p_vs_np, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(p_vs_np, extractiveness, 0.38).
narrative_ontology:constraint_metric(p_vs_np, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(p_vs_np, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(p_vs_np, tangled_rope).
narrative_ontology:human_readable(p_vs_np, "The P versus NP Problem").
narrative_ontology:topic_domain(p_vs_np, "technological/mathematics").

domain_priors:requires_active_enforcement(p_vs_np).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(p_vs_np, cryptography_industry).
narrative_ontology:constraint_beneficiary(p_vs_np, mathematical_logic_researchers).
narrative_ontology:constraint_victim(p_vs_np, optimization_dependent_sectors).
narrative_ontology:constraint_victim(p_vs_np, computational_verification_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPTIMIZATION-DEPENDENT SECTOR (SNARE) — Industries relying on NP-hard problem solutions (logistics, manufacturing, drug discovery, circuit design) cannot exit the constraint that efficient solutions may not exist. If P ≠ NP, they are permanently trapped in exponential-time computation, bearing the full cost of the unsolved problem. No verification pathway, no escape route, no institutional arbitrage available.
constraint_indexing:constraint_classification(p_vs_np, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPUTATIONAL VERIFICATION INFRASTRUCTURE (TANGLED ROPE) — Must operate under the assumption that verification (NP) is fundamentally faster than solving (P), yet cannot prove this asymmetry. This constraint both enables the infrastructure (fast verification is useful) and constrains it (fast solving would break cryptography). Genuine coordination function coupled with asymmetric cost distribution — those who depend on hardness-as-security suffer if P=NP, while verification-focused systems are stable either way.
constraint_indexing:constraint_classification(p_vs_np, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CRYPTOGRAPHY INDUSTRY (ROPE) — Benefits from the P ≠ NP assumption as a foundation for security protocols. The unresolved problem is treated as a coordination mechanism: all parties agree to assume hardness exists. This is pure coordination with minimal coercive overhead — no enforcement needed beyond academic consensus and engineering practice. Net beneficiary position.
constraint_indexing:constraint_classification(p_vs_np, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POST-QUANTUM CRYPTOGRAPHY COALITION (SCAFFOLD) — Organized actors (NIST, security researchers, industry consortia) recognize that P=NP or quantum algorithms breaking RSA would invalidate current cryptographic assumptions. They are actively building alternative cryptographic pathways (lattice-based, hash-based, multivariate) with explicit sunset logic: if P=NP or quantum computing reaches practical scale, transition protocols are pre-architected. Suppression (transition difficulty) is high, but the coalition has agency and exit visibility.
constraint_indexing:constraint_classification(p_vs_np, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MATHEMATICAL LOGIC ESTABLISHMENT (PITON) — The P vs NP problem is treated as an iconic open problem, generating prestige and research funding. Much of the institutional activity (conferences, journals, prize announcements) is performative: the establishment maintains the problem's iconic status independent of progress toward resolution. Theater ratio is high — most activity is descriptive (exploring equivalent formulations) rather than functional (solving the problem). The research institution persists through inertia and status maintenance, not because resolution is imminent.
constraint_indexing:constraint_classification(p_vs_np, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL LIMIT VIEW (MOUNTAIN) — From a universal analytical perspective, the P vs NP question may represent an irreducible limit on computational expressiveness: a fundamental asymmetry between checking and solving that is inherent to logic itself, not contingent on current algorithms or computing technology. Accessibility to proof or disproof may have an absolute floor due to the structure of formal systems. However, this perspective risks naturalizing what could be a contingent historical fact (current lack of polynomial-time solvers) as an immutable law.
constraint_indexing:constraint_classification(p_vs_np, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(p_vs_np_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(p_vs_np, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(p_vs_np, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(p_vs_np, TR),
    TR >= 0.70.

:- end_tests(p_vs_np_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from optimization-dependent industries who cannot efficiently solve their NP-hard problems, but the extraction is not severe because (1) approximation algorithms and heuristics provide partial solutions, (2) specialized hardware and quantum research offer alternative pathways, (3) the problem is intellectually transparent — no hidden mechanism obscures the cost structure. The extractiveness value reflects sustained but not total subordination to the constraint. Suppression (0.42): Moderate. High barriers include the intrinsic difficulty of NP-hardness (not artificially imposed), but suppression is not extreme because alternative problem-solving methods (approximation, metaheuristics, quantum exploration) reduce exit barriers. The constraint suppresses direct optimization but not all approaches to useful computation. Theater ratio (0.58): Moderate-high. Much of the institutional response to P vs NP is now performative: conferences explore equivalent formulations, journals publish surveys, but new progress toward resolution is infrequent. Theater has risen from 0.42 (1971, early exploration phase) to 0.58 (2026, iconic open problem phase) as the problem's status as unsolvable has crystallized. However, this is not a piton-level theater (≥0.70) because the underlying mathematical question remains genuinely open and consequential.
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives is stark. The beneficiary (cryptography industry) sees pure coordination (Rope) — parties agreeing to assume hardness is sufficient. The victim (optimization-dependent sectors) sees pure extraction (Snare) — trapped by an unresolved theoretical question with no escape. The organized coalition sees a temporary problem with a designed exit (Scaffold) — post-quantum migration protocols are pre-architected. The mathematical establishment sees a degraded iconic problem (Piton) — maintained through prestige and funding allocation, not active solution attempts. The infrastructure sees mixed coordination and extraction (Tangled Rope) — verification is faster than solving (useful) but this asymmetry is unproven and could fail if P=NP or quantum computing breaks it. The analytical observer risks seeing an immutable law (Mountain) — computational limits inherent to logic itself — but this naturalizes what is an open empirical question in complexity theory.
 *
 * DIRECTIONALITY LOGIC:
 *   Cryptography industry benefits from the P ≠ NP assumption and has low exit cost (arbitrage: they could switch cryptographic paradigms if assumptions change). Engine derives low d, making their chi negative (they experience the constraint as enabling, not extractive). Optimization-dependent sectors are victims with no exit (trapped) — they experience high d and high f(d), making chi strongly positive (maximal extraction). Post-quantum coalition has constrained exit (migration is possible but requires infrastructure investment) and moderate power (organized actors) — they experience mid-range d with moderate chi. The mathematical logic establishment benefits from the problem's unsolved status (arbitrage: they could study other problems) but maintains it through institutional inertia. The computational verification infrastructure is locked into the P ≠ NP assumption for cryptographic security (constrained exit) and cannot easily escape the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY EXPOSURE: The constraint resolves the mandatrophy by showing that the P vs NP problem exhibits genuine coordination (shared cryptographic assumptions, global security infrastructure) coupled with asymmetric extraction (optimization sectors remain stuck in exponential time while crypto industry builds on unvalidated assumptions). This is not pure extraction disguised as coordination — both elements are structurally necessary. However, the mandatrophy risk emerges if the problem remains unresolved for another 50+ years: the coordination function (shared assumption of P ≠ NP) may calcify into a piton (institutional maintenance without functional resolution). The theater ratio trajectory (0.42 → 0.58) shows early piton drift, indicating that the institutional response is slowly shifting from problem-solving orientation to iconic problem maintenance. The post-quantum coalition's exit pathway (Scaffold perspective) represents active mandatrophy mitigation: by pre-designing alternatives to current cryptographic assumptions, the coalition prevents the constraint from degrading into pure institutional theater. If post-quantum migration succeeds before P vs NP is resolved, the tangled rope converts to a scaffold with an executed sunset clause, preventing mandatrophy lock-in.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p_equals_np_solvability,
    'Is there a polynomial-time algorithm for NP-complete problems, or is P ≠ NP a fundamental computational law?',
    'Proof of P=NP (constructive algorithm) or proof of P ≠ NP (separation argument from formal complexity theory). Alternatively, resolution via automated theorem proving or meta-mathematical arguments about Gödel incompleteness in ZFC.',
    'If P=NP: cryptography breaks, optimization becomes practical, industries dependent on NP-hardness face disruption. If P ≠ NP: current cryptographic assumptions are validated, optimization barriers persist, computational hierarchy is confirmed. If undecidable in ZFC: the problem may have no mathematical resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(p_equals_np_solvability, empirical, 'Whether P equals NP or they are distinct complexity classes').

omega_variable(
    cryptographic_hardness_sufficiency,
    'Even if P ≠ NP is proven, do polynomial-time worst-case lower bounds guarantee security in practice (average-case cryptographic hardness)?',
    'Analysis of gap between worst-case P ≠ NP and average-case hardness assumptions used in cryptography. Study of potential subexponential algorithms exploiting structure in real cryptographic problems.',
    'If worst-case hardness doesn''t imply cryptographic security: even with P ≠ NP proven, RSA and similar systems could break. If gap exists but is bridgeable: post-quantum migration may be more urgent than mathematical proof suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cryptographic_hardness_sufficiency, empirical, 'Whether worst-case P ≠ NP guarantees practical cryptographic hardness').

omega_variable(
    quantum_polynomial_time_disruption,
    'Can quantum algorithms solve NP-hard problems in polynomial time, effectively making P=NP in the quantum computational model?',
    'Quantum complexity theory analysis (BQP vs NP relationships). Empirical demonstration of quantum advantage on specific NP-hard problems as quantum computers scale. Theoretical proof of quantum polynomial-time solutions or fundamental barriers.',
    'If quantum P=NP: the cryptographic protection from classical P ≠ NP is illusory in a quantum-enabled world. Post-quantum migration becomes urgent independent of classical P vs NP resolution. If quantum cannot solve NP-hard problems: quantum advantage is limited to specific problem classes, and classical cryptography remains robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_polynomial_time_disruption, empirical, 'Whether quantum computing enables polynomial-time NP-hard problem solving').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(p_vs_np, 1971, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pvsnp_tr_t0, p_vs_np, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pvsnp_tr_t25, p_vs_np, theater_ratio, 25, 0.52).
narrative_ontology:measurement(pvsnp_tr_t50, p_vs_np, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(pvsnp_be_t0, p_vs_np, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pvsnp_be_t25, p_vs_np, base_extractiveness, 25, 0.34).
narrative_ontology:measurement(pvsnp_be_t50, p_vs_np, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(p_vs_np, information_standard).
narrative_ontology:affects_constraint(p_vs_np, cryptographic_assumption_validation).
narrative_ontology:affects_constraint(p_vs_np, quantum_supremacy_ntru_lattice).
narrative_ontology:affects_constraint(p_vs_np, optimization_approximation_floors).

% DUAL FORMULATION NOTE:
% The P vs NP problem decomposes into two structurally distinct constraints: (1) The mathematical open question (P=NP? decidable within formal complexity theory), which is a pure mountain from some perspectives and a genuine open problem from others; (2) The cryptographic hardness assumption (P ≠ NP as a security foundation), which is a tangled rope of coordination + extraction. These are linked because the cryptographic assumption depends on the mathematical resolution, but they have different ε values and different structural properties. The cryptographic story is about institutional reliance on an unvalidated assumption (ε ≈ 0.38). The mathematical story might be about irreducible undecidability (potentially ε ≤ 0.25 if proven to be undecidable in ZFC, making it a logical mountain).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(p_vs_np, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
