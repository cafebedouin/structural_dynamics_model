% ============================================================================
% CONSTRAINT STORY: p_vs_np
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: theoretical_computer_science/computational_complexity
 *
 * SUMMARY:
 *   The P versus NP problem occupies a unique structural position: it is
 *   simultaneously a fundamental mathematical question (possibly a natural
 *   law of computation), an unsolved research agenda (benefiting theorists
 *   and security institutions), an extraction mechanism (constraining
 *   optimization-dependent industries), and a temporary vulnerability window
 *   (before post-quantum cryptography standards fully mature). The constraint
 *   exhibits tangled coordination and extraction: the complexity theory
 *   research community coordinates through the open problem while also
 *   benefiting from its openness; cryptographic infrastructure coordinates
 *   around the assumption of hardness while extracting value from
 *   optimization-dependent sectors that lack alternatives; quantum computing
 *   research and post-quantum standardization both represent structured
 *   efforts to resolve or work around the constraint. The theater ratio
 *   (0.65) reflects that much research activity—quantum advantage narratives,
 *   complexity-theoretic paper production, cryptographic positioning—serves
 *   institutional goals (funding, career advancement, competitive
 *   positioning) rather than direct progress on the problem's resolution. The
 *   constraint's extractiveness has increased over 50 years (0.22 → 0.38) as
 *   computational infrastructure became more dependent on cryptographic
 *   assumptions and as the unsolved status enabled rent-seeking behavior in
 *   research funding and security markets.
 *
 * KEY AGENTS:
 *   - Optimization-Dependent Sectors (logistics, manufacturing, drug discovery, finance): Primary victim (powerless/trapped) — no exit from computational complexity constraints; bear full cost of exponential solution times
 *   - Cryptographic Security Infrastructure: Primary beneficiary (institutional/arbitrage) — public-key cryptography assumes P≠NP; gains market value from this assumption's unresolved status
 *   - Complexity Theory Research Community: Secondary beneficiary/coordinator (moderate/constrained) — coordinate through open problem; benefit from its research funding and intellectual prestige; partial disincentive to closure
 *   - Large Technology Companies (quantum/approximation): Institutional actor (powerful/mobile) — derive competitive advantage from practical speedups; maintain piton-like position through heuristic solver research and quantum positioning
 *   - Post-Quantum Cryptography Standards Movement: Organized actors (organized/constrained) — building alternative verification systems; structured sunset logic as quantum threat timeline clarifies
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional lock-in as fundamental computational law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(p_vs_np, 0.38).
domain_priors:suppression_score(p_vs_np, 0.48).
domain_priors:theater_ratio(p_vs_np, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(p_vs_np, extractiveness, 0.38).
narrative_ontology:constraint_metric(p_vs_np, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(p_vs_np, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(p_vs_np, tangled_rope).
narrative_ontology:human_readable(p_vs_np, "The P versus NP Problem").
narrative_ontology:topic_domain(p_vs_np, "theoretical_computer_science/computational_complexity").

domain_priors:requires_active_enforcement(p_vs_np).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(p_vs_np, cryptography_infrastructure).
narrative_ontology:constraint_beneficiary(p_vs_np, complexity_theory_research_community).
narrative_ontology:constraint_victim(p_vs_np, optimization_dependent_industries).
narrative_ontology:constraint_victim(p_vs_np, computational_resource_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPTIMIZATION-DEPENDENT SECTOR (SNARE) — Industries relying on combinatorial optimization (logistics, manufacturing, drug discovery, financial modeling) have no exit from the computational intractability. Trapped by physical law constraints on verification speed vs solution speed. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(p_vs_np, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CRYPTOGRAPHIC SECURITY INFRASTRUCTURE (ROPE) — Benefits structurally from P≠NP assumption. Public-key cryptography (RSA, elliptic curve) relies on the hardness of factorization/discrete-log (NP but not known to be in P). Experiences constraint as coordination mechanism enabling trustworthy digital commerce. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.04. Net beneficiary.
constraint_indexing:constraint_classification(p_vs_np, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPLEXITY THEORY RESEARCH COMMUNITY (TANGLED ROPE) — Coordination function: P vs NP unifies theory-building; researchers collaborate on proof attempts, complexity classes, hardness reductions. Extraction function: solving P vs NP would collapse entire subfields (NP-completeness, approximation theory); career incentives partially align with perpetuating openness rather than closure. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.28.
constraint_indexing:constraint_classification(p_vs_np, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE TECHNOLOGY COMPANIES (PITON) — Maintain substantial research programs in quantum algorithms, approximation algorithms, and heuristic solvers (SAT solvers, constraint programming). Derive competitive advantage from solving hard instances faster in practice, independent of P vs NP's theoretical resolution. theater_ratio=0.65: much published research is performative positioning (quantum advantage narratives) rather than solving the core question. d≈0.35, f(d)≈0.35, σ=1.1 → χ≈0.25.
constraint_indexing:constraint_classification(p_vs_np, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-QUANTUM CRYPTOGRAPHY STANDARDS (SCAFFOLD) — Organized effort (NIST standardization, lattice-based/code-based alternatives) to build verification systems that do NOT rely on P≠NP assumption. Structured as temporary coordination: develop alternatives over 5-10 year horizon while classical cryptography remains viable. d≈0.42, f(d)≈0.42, σ=1.1 → χ≈0.19. Sunset mechanism: as quantum computing matures, lattice-based schemes become standard, reducing reliance on unsolved theoretical question.
constraint_indexing:constraint_classification(p_vs_np, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From civilizational/universal perspective, P vs NP may reflect an intrinsic asymmetry in computation: verification might be fundamentally easier than solution. This perspective naturalizes the constraint as a law of computational physics. However, structural data (ε=0.38, suppression=0.48, theater=0.65) contradicts pure mountain classification — the unsolved status partly reflects institutional rent-seeking (complexity theorists' research agenda) and technological lock-in (cryptographic infrastructure), not just natural law.
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
    constraint_indexing:constraint_classification(p_vs_np, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.38): Moderate. The constraint extracts value from optimization-dependent industries (they cannot escape polynomial verification asymmetry), but the extraction is not severe because practical algorithmic advances (SAT solvers, approximation algorithms, heuristics) provide partial mitigation. The true extractiveness comes from cryptographic infrastructure's reliance on hardness assumptions—but this is partly a *chosen* dependency, not pure victimization. The growth from 0.22 to 0.38 over 50 years reflects increased infrastructure lock-in and security market dependence. Suppression (0.48): Moderate. Significant barriers to alternative approaches (quantum computing not yet scaled, post-quantum standards not yet universal, approximation algorithms have bounded guarantees). But suppression is not total—partial solutions exist, research is ongoing, alternatives are developing. Theater ratio (0.65): Moderately high. Research activity is substantial, but much is performative: quantum advantage narratives (unsupported at scale), complexity-theoretic papers that advance academic prestige more than problem closure, security theater around cryptographic 'strength' despite fundamental unsolved status. The theater has grown as funding and institutional incentives expanded around the problem.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows perspectival divergence based on structural position relative to the unresolved question. Cryptographic infrastructure (institutional/arbitrage) sees a coordination mechanism—the assumption of hardness enables trustworthy commerce—and thus experiences the constraint as Rope. Optimization-dependent sectors (powerless/trapped) see pure extraction—they cannot solve hard instances and have no exit—thus experience Snare. Complexity theorists (moderate/constrained) experience both: coordination through the open problem, but also partial disincentive to closure (if P vs NP were proven, subfields would collapse), yielding Tangled Rope. Large tech companies (powerful/mobile) treat the constraint as a source of competitive advantage through practical algorithms, experiencing it as degraded/piton-like. Post-quantum cryptography builders (organized/constrained) see a solvable temporary problem with structured exit, yielding Scaffold. The analytical observer risks naturalizing this as Mountain—a law of computation—when it partly reflects institutional lock-in and research incentive structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Cryptographic infrastructure: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; experiences low effective extraction (negative χ). Optimization-dependent sectors: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; no exit from computational asymmetry. Complexity theory community: Mixed (beneficiary from research funding + victim from potential closure risk) + constrained → d≈0.55, f(d)≈0.75. Moderate extraction reflecting dual role. Large tech companies: Beneficiary (from competitive advantage) + mobile → d≈0.35, f(d)≈0.35. Low effective extraction; can exit through algorithmic innovation. Post-quantum standards: Organized + constrained → d≈0.42, f(d)≈0.42. Low extraction; coalition has agency. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Risk of false mountain if observer naturalizes contingent institutional dependencies.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy emerges from the observation that P vs NP appears simultaneously as a fundamental natural law (mountain: true asymmetry in computational structure) and as a rent-seeking research agenda (extraction: complexity theorists and security institutions benefit from its unresolved status). The constraint is tangled_rope because it exhibits both genuine coordination (complexity theory unifies around the problem; cryptography coordinates on hardness assumptions) and asymmetric extraction (optimization industries trapped; theorists benefit from perpetual openness). Resolution of the mandatrophy requires distinguishing: (1) the mathematical core (potentially mountain), (2) the institutional lock-in (snare), and (3) the working alternatives (scaffold). If P vs NP is proven true, the constraint becomes a validated natural law (mountain), and extraction mechanisms are vindicated. If P = NP, cryptographic extraction collapses. If the proof remains elusive indefinitely, the constraint degrades into piton—maintained through institutional inertia rather than functional necessity—as post-quantum standards mature and heuristic algorithms improve. Current structural data (ε=0.38, theater=0.65, growth trajectory) suggests the constraint is NOT currently a true mountain but rather a mixed institutional/theoretical hybrid—tangled_rope classification is appropriate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p_equals_np_vs_separation,
    'Does P = NP or P ≠ NP at the fundamental level? Is the apparent asymmetry a law of computation or a contingent feature of polynomial-time Turing machines?',
    'Proof-theoretic breakthrough (constructive proof of algorithm or impossibility result); independence-from-ZFC analysis; exploration of non-standard computational models',
    'If P = NP: cryptography infrastructure collapses; optimization industries gain exponential advantage; complexity theory subfields dissolve. If P ≠ NP (proven): constraint becomes true Mountain; extraction mechanism is validated as natural law. If independent: constraint becomes hybrid framework-dependent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(p_equals_np_vs_separation, conceptual, 'Fundamental nature of P vs NP: law vs contingency').

omega_variable(
    quantum_advantage_realizability,
    'Can quantum computers solve NP-complete problems in subexponential time? Does quantum advantage translate to solving hard practical instances?',
    'Scaled quantum computer demonstrations; hardness certificates for quantum algorithms; comparison of quantum vs classical runtimes on standard NP-complete benchmarks',
    'If quantum solves NP-hard in subexponential: cryptography assumption invalidated immediately; constraint shifts from theoretical to urgent technological extraction. If quantum advantage is limited: constraint remains theoretical; practical optimization depends on heuristics, not fundamental resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_advantage_realizability, empirical, 'Quantum advantage for NP-complete problems').

omega_variable(
    heuristic_solver_efficacy_ceiling,
    'Do SAT solvers, constraint propagation, and approximation algorithms represent a fundamental practical ceiling below exponential time, or can algorithmic improvements sustain linear/polynomial speedups indefinitely?',
    'Historical trend analysis of solver runtime improvements; empirical phase transition studies; competitive solver benchmarking over 20+ year timescale',
    'If ceiling exists: heuristic performance bounds the optimization-dependent sector despite theoretical P vs NP status. If improvements continue indefinitely: practical cryptography remains feasible via algorithm selection, reducing instantaneous extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(heuristic_solver_efficacy_ceiling, empirical, 'Practical limits of classical optimization algorithms').

omega_variable(
    cryptographic_agility_adoption,
    'How rapidly can global cryptographic infrastructure transition to post-quantum standards? Does the transition create a window of vulnerability or extraction opportunity?',
    'Deployment timeline tracking (TLS certificate adoption, hardware upgrade cycles); security audit results for transition implementations; monitoring of quantum threat timelines vs adoption curves',
    'If rapid adoption: scaffold perspective confirmed, extraction window closes. If slow adoption: organizations face extraction risk (classical systems broken before transition); becomes Snare for late-adopting industries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cryptographic_agility_adoption, empirical, 'Post-quantum cryptography adoption rate and transition risk').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(p_vs_np, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pnp_tr_t0, p_vs_np, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pnp_tr_t25, p_vs_np, theater_ratio, 25, 0.54).
narrative_ontology:measurement(pnp_tr_t50, p_vs_np, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(pnp_be_t0, p_vs_np, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(pnp_be_t25, p_vs_np, base_extractiveness, 25, 0.3).
narrative_ontology:measurement(pnp_be_t50, p_vs_np, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(p_vs_np, information_standard).
narrative_ontology:affects_constraint(p_vs_np, cryptographic_hardness_assumption).
narrative_ontology:affects_constraint(p_vs_np, quantum_factorization_algorithm).
narrative_ontology:affects_constraint(p_vs_np, np_complete_approximation_ceiling).

% DUAL FORMULATION NOTE:
% P vs NP decomposes into three structurally distinct constraints: (1) the mathematical question itself (mountain or indeterminate), (2) the cryptographic infrastructure dependency (snare/rope depending on perspective), and (3) the practical optimization ceiling (snare). These are linked because the mathematical resolution would affect both cryptographic validity and optimization feasibility, but they have different ε values reflecting different failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(p_vs_np, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
