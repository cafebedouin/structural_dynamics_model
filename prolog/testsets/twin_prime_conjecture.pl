% ============================================================================
% CONSTRAINT STORY: twin_prime_conjecture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_twin_prime_conjecture, []).

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
 *   constraint_id: twin_prime_conjecture
 *   human_readable: The Unproven Nature of the Twin Prime Conjecture
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   The twin prime conjecture — the claim that there exist infinitely many
 *   pairs of primes differing by 2 — is a paradigmatic example of a
 *   mathematical mountain constraint. The conjecture's status is independent
 *   of observer perspective, institutional resources, historical epoch, or
 *   measurement methodology. Either infinitely many twin primes exist or they
 *   do not. This truth-value is fixed by the structure of natural numbers and
 *   the definition of primality, not by human investigation, conjecture, or
 *   proof. The constraint exhibits zero degrees of freedom: no agent can
 *   escape it, no institutional arrangement can modify it, no alternative
 *   methodology can bypass it. The conjecture has resisted proof attempts for
 *   over 150 years (since Alphonse de Polignac formulated it in 1849),
 *   generating substantial partial results (Hardy-Littlewood conjecture,
 *   Brun's theorem, Zhang's bounded gaps, Maynard's improvements), but the
 *   full resolution remains open. This persistent openness does not render
 *   the constraint contingent or extractive — it merely demonstrates that
 *   humans have not yet discovered the proof or refutation. The constraint's
 *   existence and structure are independent of human knowledge.
 *
 * KEY AGENTS:
 *   - Prime numbers themselves: Not an agent, but the structural ground. Twin primes are mathematical objects whose existence is determined by logical necessity, not by institutional or epistemic arrangements.
 *   - The mathematical community: Distributed agent across centuries; pursues the conjecture but does not determine its truth-value. Has no extractive relationship to the constraint — cannot modify it, negotiate with it, or escape it.
 *   - Computational verification efforts: Methods like distributed searches to 10^18 twin primes do not constitute proof and cannot resolve the existential claim. Theater activity (publishable partial results) but no effective extraction.
 *   - Formalized proof systems (ZFC, etc.): The logical ground in which the conjecture is formulated. The constraint's truth-value may be independent of specific axiom systems, but the constraint itself is not contingent on that independence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(twin_prime_conjecture, 0.12).
domain_priors:suppression_score(twin_prime_conjecture, 0.02).
domain_priors:theater_ratio(twin_prime_conjecture, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(twin_prime_conjecture, extractiveness, 0.12).
narrative_ontology:constraint_metric(twin_prime_conjecture, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(twin_prime_conjecture, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(twin_prime_conjecture, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(twin_prime_conjecture, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(twin_prime_conjecture, mountain).
narrative_ontology:human_readable(twin_prime_conjecture, "The Unproven Nature of the Twin Prime Conjecture").
narrative_ontology:topic_domain(twin_prime_conjecture, "mathematical/logical").

domain_priors:emerges_naturally(twin_prime_conjecture).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNIVERSAL MATHEMATICAL LOGIC (MOUNTAIN) — The twin prime conjecture's truth-value is a fixed fact independent of observer, context, or methodology. Either infinitely many twin primes exist or they do not. The constraint emerges from the structure of natural numbers and prime distribution itself. No degrees of freedom; no coercion; no extraction. Pure logical/mathematical necessity.
constraint_indexing:constraint_classification(twin_prime_conjecture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICIAN WITH RESEARCH RESOURCES (MOUNTAIN) — Even with unlimited computational power, funding, and collaboration networks, the constraint remains immutable. No amount of empirical verification (checking twin primes to 10^18) resolves the existential claim. The conjecture's truth is independent of whether any human investigates it. Mobile exit options and powerful status do not change the classification — this is a true mountain, not a scaffold masquerading as one.
constraint_indexing:constraint_classification(twin_prime_conjecture, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: INDIVIDUAL STUDENT (IMMEDIATE/LOCAL) — A mathematics student learning about the twin prime conjecture confronts the same immutable structure: the conjecture is either true or false, and this fact does not change whether the student has resources, institutional support, or time to work on it. The constraint is identical from every observer position because it is a structural feature of prime number distribution, not a social arrangement.
constraint_indexing:constraint_classification(twin_prime_conjecture, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: MATHEMATICS INSTITUTION (NATIONAL/GENERATIONAL) — Research universities, funding agencies, and mathematics departments all confront the same constraint: the conjecture's truth-value is independent of institutional structure. Arbitrage exit options (reallocating mathematicians to other problems) do not change the mountain classification — the conjecture will remain unproven as long as it is unproven, regardless of how many mathematicians pursue it or how many abandon it for other pursuits.
constraint_indexing:constraint_classification(twin_prime_conjecture, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(twin_prime_conjecture_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(twin_prime_conjecture, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(twin_prime_conjecture, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(twin_prime_conjecture, ExtMetricName, E),
    domain_priors:suppression_score(twin_prime_conjecture, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(twin_prime_conjecture),
    narrative_ontology:constraint_metric(twin_prime_conjecture, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(twin_prime_conjecture, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(twin_prime_conjecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The conjecture has no extractive relationship to any agent. Mathematical truth is not extracted from subjects nor does it extract resources — it simply is. The 0.12 value reflects only the minimal epistemic burden of formulating the conjecture precisely (necessary labor to state the problem clearly). Suppression (0.02): Negligible. There is no coercion, alternative-blocking, or institutional suppression. Any mathematician is free to work on the conjecture or abandon it. The constraint does not prevent exit or threaten penalty — it simply states a claim about prime distribution. Theater ratio (0.15): Very low. Mathematical proof is minimally theatrical. Successful proofs add minimal cosmetic layer over underlying logical content. The 0.15 reflects only the minor performative elements in how conjectures are stated, how partial results are framed for publication, and how research programs are narrated. These are communication structures, not extraction mechanisms. Accessibility collapse (0.92): Very high. The conjecture's statement is maximally degenerate under attempted concealment or obfuscation — it cannot be hidden, reframed as contingent, or presented as negotiable. The truth-value is completely accessible to logical analysis. Resistance (0.08): Very low. No resistance to the mountain classification emerges because the constraint is purely logical, not institutional or socially constructed.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits zero perspectival gap across all contexts. Every perspective produces the same classification (mountain) because the constraint is not perspective-dependent. The truth-value of the twin prime conjecture is invariant across all observers: mathematicians with resources, students without resources, institutions, analytical observers, agents with exit options, and agents trapped in mathematical inquiry all confront the identical immutable claim. This invariance is the defining signature of a true mountain. Unlike the verification bottleneck exemplar (which shows all six types from different perspectives), the twin prime conjecture shows the same type from every perspective. This is not a deficiency in the framework — it is the correct analysis. Some constraints really are invariant; others are perspective-dependent. The framework correctly distinguishes between them.
 *
 * MANDATROPHY ANALYSIS:
 *   The twin prime conjecture resolves mandatrophy trivially: there is no mandatrophy to resolve because the constraint is a pure mountain, not a snare masquerading as coordination. The conjecture does not extract (no beneficiary/victim structure), does not coordinate (no collective action problem), and does not suppress (no alternative-blocking). It simply makes a claim about the structure of primes. The apparent 'unprovenness' might initially be misread as an extractive constraint (researchers extract funding, prestige, publishable partial results from the open conjecture), but this would conflate the constraint with the institutional arrangements around mathematical research. The constraint itself — the mathematical claim — is non-extractive. The institutional theater around conjecture-chasing is separable and could be analyzed in a separate constraint story if needed, but the constraint story presented here focuses on the mathematical claim itself, which is mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decidability_of_twin_primes,
    'Is the twin prime conjecture decidable within ZFC (Zermelo-Fraenkel set theory with choice), or is it independent like the Continuum Hypothesis?',
    'Formal proof of decidability or independence from ZFC; analysis of forced models where the conjecture differs in truth-value',
    'If decidable: truth-value is fixed in all standard models. If independent: truth-value is model-relative, introducing a perspectival degree of freedom. However, independence would not change the mountain classification — it would only reveal that the constraint''s structure is more subtle (the truth-value exists but is not determined by standard axioms).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decidability_of_twin_primes, conceptual, 'Whether the twin prime conjecture is decidable in standard set theory').

omega_variable(
    proof_vs_refutation_asymmetry,
    'Is there a structural asymmetry between proving the twin prime conjecture true versus proving it false (i.e., proving there are only finitely many)?',
    'Analysis of known partial results (bounded gaps, sieve theory limits) and their implications for proof strategies; classification of attempted proofs by their logical structure',
    'If asymmetry exists: one direction may be fundamentally harder, constraining which proof strategies are viable. If symmetric: both directions are equally difficult. This does not change the mountain classification but clarifies whether the constraint''s difficulty is emergent from prime distribution structure or imposed by mathematical methodology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proof_vs_refutation_asymmetry, empirical, 'Structural asymmetry between proof and refutation approaches').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(twin_prime_conjecture, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tpc_tr_t0, twin_prime_conjecture, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tpc_tr_t50, twin_prime_conjecture, theater_ratio, 50, 0.15).
narrative_ontology:measurement(tpc_tr_t100, twin_prime_conjecture, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(tpc_be_t0, twin_prime_conjecture, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(tpc_be_t50, twin_prime_conjecture, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(tpc_be_t100, twin_prime_conjecture, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(twin_prime_conjecture, information_standard).
narrative_ontology:affects_constraint(twin_prime_conjecture, goldbach_conjecture).
narrative_ontology:affects_constraint(twin_prime_conjecture, riemann_hypothesis).
narrative_ontology:affects_constraint(twin_prime_conjecture, collatz_conjecture).

% DUAL FORMULATION NOTE:
% The twin prime conjecture belongs to a family of unproven conjectures about prime distribution (Goldbach, Riemann, Collatz). All are mountains from a logical perspective. However, the *institutional arrangements around conjecture-chasing* (funding allocation, prestige hierarchies, publish-or-perish dynamics) could be analyzed as a separate constraint story with different extractiveness. This story focuses on the mathematical claim itself; institutional critique would require a separate analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
