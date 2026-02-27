% ============================================================================
% CONSTRAINT STORY: prime_number_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prime_number_theorem, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: prime_number_theorem
 *   human_readable: Prime Number Theorem (Asymptotic Density)
 *   domain: mathematical/analytic_number_theory
 *
 * SUMMARY:
 *   The Prime Number Theorem describes the asymptotic distribution of primes:
 *   the number of primes less than or equal to x, denoted π(x), is
 *   asymptotically equivalent to x/ln(x). First proved by Jacques Hadamard
 *   and Charles-Jean de la Vallée Poussin independently in 1896, the theorem
 *   is a foundational result in analytic number theory. Unlike many
 *   mathematical theorems that emerged from human attempts to solve practical
 *   problems or optimize systems, the PNT emerges from the intrinsic
 *   structure of the positive integers and the properties of the Riemann zeta
 *   function. No agent — computational, mathematical, institutional, or
 *   individual — can negotiate, exit, or extract value by altering this
 *   constraint. It is invariant across all consistent axiomatic systems (at
 *   least all known frameworks). The theorem has zero degrees of freedom for
 *   all indices: it binds powerless mathematicians and powerful computational
 *   systems equally; it applies to immediate calculations and
 *   civilizational-scale mathematics; it cannot be escaped by arbitrage or
 *   mobility; and it holds universally across all domains that depend on
 *   prime distribution.
 *
 * KEY AGENTS:
 *   - Mathematical Community: Analytical observers (analytical/analytical) — perceives the theorem as a self-evident mathematical truth, no extraction
 *   - Computational Mathematicians: Powerful agents (powerful/mobile) — attempt to optimize prime-finding algorithms but cannot escape the density constraint
 *   - Cryptographers: Organized actors (organized/constrained) — must work within the asymptotic structure when designing cryptosystems; the constraint is uncompromisable
 *   - Mathematical Institutions: Institutional actors (institutional/arbitrage) — benefit from the stability of mathematical foundations; no extraction mechanism
 *   - Individual Mathematicians: Moderate agents (moderate/trapped) — must accept the theorem as foundational to their research; cannot negotiate or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prime_number_theorem, 0.08).
domain_priors:suppression_score(prime_number_theorem, 0.02).
domain_priors:theater_ratio(prime_number_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prime_number_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(prime_number_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(prime_number_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(prime_number_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(prime_number_theorem, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prime_number_theorem, mountain).
narrative_ontology:human_readable(prime_number_theorem, "Prime Number Theorem (Asymptotic Density)").
narrative_ontology:topic_domain(prime_number_theorem, "mathematical/analytic_number_theory").

domain_priors:emerges_naturally(prime_number_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Prime Number Theorem emerges as a mathematical necessity from the axioms of analysis. No agent can exit or negotiate the asymptotic density π(x) ~ x/ln(x). The theorem is invariant across all mathematical frameworks consistent with real analysis. d≈0.72, f(d)≈1.15, but the mountain classification overrides chi scaling — this is a natural law of number theory.
constraint_indexing:constraint_classification(prime_number_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Even agents with computational resources cannot escape the prime density constraint. Whether one uses trial division, sieves, or probabilistic tests, the underlying asymptotic structure persists. The constraint is invariant to computational methodology. d≈0.48, f(d)≈0.60, but the mountain classification (ε=0.08, suppression=0.02) indicates no meaningful extraction despite computational power.
constraint_indexing:constraint_classification(prime_number_theorem, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% Cryptographic applications must work within the asymptotic density theorem — they cannot negotiate the spacing distribution of large primes. The constraint binds organized actors designing RSA systems, elliptic curve cryptography, and quantum-resistant algorithms. The theorem is uncompromisable. d≈0.40, f(d)≈0.40, but mountain classification holds: agents experience an immutable property of number theory, not extraction.
constraint_indexing:constraint_classification(prime_number_theorem, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Universities, research institutes, and mathematical societies cannot arbitrage the Prime Number Theorem. It is foundational to the mathematical enterprise itself. Institutional actors benefit from the stability of the theorem, but they cannot extract value by altering it. d≈0.05, f(d)≈-0.12, indicating net benefit from a mountain constraint — institutions are stabilized by mathematical truth.
constraint_indexing:constraint_classification(prime_number_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% A mathematician working on analytic number theory cannot exit the constraint. The asymptotic density is a structural fact they must accept and work within. Career advancement in the field requires accepting the theorem's truth. d≈0.65, f(d)≈1.00, but mountain classification (ε=0.08, suppression=0.02) shows this is an immutable property, not extraction — the constraint binds equally to all agents regardless of power.
constraint_indexing:constraint_classification(prime_number_theorem, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prime_number_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(prime_number_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prime_number_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(prime_number_theorem, ExtMetricName, E),
    domain_priors:suppression_score(prime_number_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(prime_number_theorem),
    narrative_ontology:constraint_metric(prime_number_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(prime_number_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(prime_number_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The PNT describes an invariant property of number theory, not a mechanism for one agent to extract resources from another. No beneficiary or victim can be identified because the theorem constrains all agents symmetrically. The small non-zero value (0.08 rather than 0.00) reflects minor epistemic costs: agents must invest computational effort to verify the theorem's predictions and cannot use the 'alternate distribution' for practical advantage. Suppression (0.02): Minimal. The theorem is not suppressed — it is openly known, widely taught, and essential to modern mathematics and cryptography. The small value reflects the trivial practical cost of learning and internalizing the constraint. Theater ratio (0.15): Minimal. The theorem's proof is functional and necessary — there is no performative aspect. The small non-zero value reflects pedagogical theater: textbooks and lectures present the theorem with examples and context, but this is legitimate mathematical communication, not suppression via obfuscation. Mountain Classification: The constraint is immutable. It follows from the axioms of real analysis and the definition of prime numbers. No alternative formulation, computational method, or mathematical framework can negotiate the asymptotic density. The constraint has zero degrees of freedom.
 *
 * PERSPECTIVAL GAP:
 *   Unlike the verification bottleneck exemplar, the Prime Number Theorem produces NO perspectival gap. All six agent types — from powerless individual mathematicians to institutional mathematical societies to computational powers to analytical observers — perceive the same classification: Mountain. The theorem is invariant. This is a key diagnostic property of true natural laws in mathematics: they bind all agents equally, and the perspectival differences (power, time horizon, exit options, scope) produce zero change in classification. The absence of perspectival gap is the feature that distinguishes a mountain from other constraint types.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation is not applicable to mountain constraints. Mountains have no beneficiaries or victims — the constraint is symmetric and immutable. All agents experience the PNT as a structural fact, not as extraction or coordination. The chi formula χ = ε × f(d) × σ(S) produces trivially small values across all indices because ε is minimal and there is no asymmetric directionality (all d values collapse to the same outcome). The theorem constrains action, but it does not extract from or benefit any agent group.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED BY MOUNTAIN CLASSIFICATION. The mandatrophy (the risk that a constraint be mislabeled as pure extraction when it is actually coordination, or vice versa) does not arise for mountains. The PNT exhibits no coordination function and no extraction mechanism — it is simply an immutable property of number theory. The absence of beneficiaries and victims confirms that this is not a tangled rope masquerading as a mountain or a rope masquerading as immutable law. The mountain classification is the correct final classification, and mandatrophy is resolved by the constraint's inherent purity: it is neither coordination nor extraction, but a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    riemann_hypothesis_dependency,
    'Does the Prime Number Theorem''s mountain status depend on the truth of the Riemann Hypothesis, or is PNT genuinely independent of RH?',
    'Review of historical proof development. The PNT was proved (Hadamard, Vallée Poussin, 1896) before RH was formulated (1859 conjecture, ongoing). Hadamard-Vallée Poussin proof uses only properties of the Riemann zeta function''s poles on Re(s)=1, not RH. Confirm independence by examining proof dependencies.',
    'If PNT depends on RH: constraint remains mountain, but the dependency chain is more complex than initial formulation suggests. If fully independent: mountain status is confirmed without caveat — PNT is a theorem, not a conjecture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(riemann_hypothesis_dependency, empirical, 'Whether PNT is logically independent of the Riemann Hypothesis').

omega_variable(
    constructive_mathematics_invariance,
    'Does the Prime Number Theorem hold in constructive mathematics (intuitionistic logic), or is it a classical-only result?',
    'Survey constructive number theory literature. The Hadamard-Vallée Poussin proof uses classical limit arguments and the intermediate value theorem, which require excluded middle. Determine whether a constructive proof exists or if the theorem is classically false-constructively-undecidable.',
    'If constructive proof exists: mountain status is framework-invariant. If proof is classically dependent: PNT is a mountain in classical mathematics but not in other axiomatic systems — suggesting the constraint is not truly immutable, only immutable-in-classical-ZFC.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructive_mathematics_invariance, conceptual, 'Whether PNT is provable in constructive (intuitionistic) mathematics').

omega_variable(
    alternative_asymptotic_formulations,
    'Are the various asymptotic formulations of prime distribution (π(x) ~ x/ln(x), Li(x) approximation, explicit formulas via zeros of ζ) genuinely equivalent descriptions of the same constraint, or do they represent distinct mathematical constraints with different ε values?',
    'Analyze the historical development and modern equivalences. Check whether each formulation makes the same empirical predictions about prime gaps, density irregularities, and statistical properties. Determine whether a gap exists between formulations under special conditions.',
    'If equivalent: single mountain constraint. If non-equivalent under some conditions: decompose into constraint family per ε-invariance principle — separate stories for density (ε≈0.08), gap structure (ε≈0.15), and explicit formula agreement (ε≈0.20).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_asymptotic_formulations, conceptual, 'Whether alternative asymptotic formulations are equivalent or distinct constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prime_number_theorem, 1896, 2046).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prim_tr_t0, prime_number_theorem, theater_ratio, 0, 0.05).
narrative_ontology:measurement(prim_tr_t75, prime_number_theorem, theater_ratio, 75, 0.1).
narrative_ontology:measurement(prim_tr_t150, prime_number_theorem, theater_ratio, 150, 0.15).

% Extraction over time
narrative_ontology:measurement(prim_be_t0, prime_number_theorem, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(prim_be_t75, prime_number_theorem, base_extractiveness, 75, 0.08).
narrative_ontology:measurement(prim_be_t150, prime_number_theorem, base_extractiveness, 150, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prime_number_theorem, information_standard).
narrative_ontology:affects_constraint(prime_number_theorem, riemann_zeta_critical_strip).
narrative_ontology:affects_constraint(prime_number_theorem, prime_gap_conjecture).
narrative_ontology:affects_constraint(prime_number_theorem, distribution_of_primes_in_arithmetic_progressions).

% DUAL FORMULATION NOTE:
% The Prime Number Theorem is upstream in the mathematical dependency graph. It is cited as evidence for finer-grained conjectures about prime spacing and distribution. The upstream-downstream relationship is causal: PNT (ε=0.08, Mountain) implies constraints on prime gaps (ε≈0.20, Tangled Rope) and the distribution of primes in arithmetic progressions (ε≈0.15, Rope). The family structure reflects how mathematical theorems propagate constraints through inference.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
