% ============================================================================
% CONSTRAINT STORY: burali_forte_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_burali_forte_paradox, []).

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
 *   constraint_id: burali_forte_paradox
 *   human_readable: Burali-Forti Paradox: The Ordinal Hierarchy Barrier
 *   domain: mathematical_logic/set_theory
 *
 * SUMMARY:
 *   The Burali-Forti paradox, discovered in 1897, reveals a structural limit
 *   in formal mathematics: any consistent system must distinguish between the
 *   collection of all ordinal numbers and legitimate set-theoretic objects.
 *   The paradox arises from self-reference: if the collection of all ordinals
 *   were itself a set, it would possess an ordinal rank greater than any
 *   ordinal it contains — a logical contradiction. This constraint
 *   exemplifies a natural law classification because it represents an
 *   immutable boundary of mathematical coherence, not a contingent
 *   institutional arrangement or extractive mechanism. Every consistent
 *   foundational system (ZFC, Von Neumann hierarchy, Grothendieck universes,
 *   type theory, categorical foundations) must respect this barrier. The
 *   constraint does not benefit identifiable agents, does not suppress
 *   alternatives through coercion, and emerges naturally from the logical
 *   structure of ordering itself. Low extractiveness (0.12) reflects that the
 *   paradox imposes no asymmetric burden on any mathematical community — it
 *   is a universal structural fact. Low suppression (0.03) reflects that the
 *   barrier is imposed by logical necessity, not institutional enforcement.
 *   Low theater (0.15) reflects that the mathematics is purely functional —
 *   the distinction between sets and collections solves a genuine
 *   coordination problem (how to coherently order ordinals without
 *   self-reference), not a performance problem.
 *
 * KEY AGENTS:
 *   - Informal Ordinal Theorist: Discovers the paradox through naive set construction; experiences the barrier as an irreducible logical wall (powerless/trapped)
 *   - Set Theory Community: Develops axiomatic responses (ZFC, Von Neumann hierarchy); experiences the paradox as a boundary condition that shapes foundational choices (institutional/constrained)
 *   - Logical Analyst: Observes the paradox as evidence of a universal structural limit in mathematics (analytical/analytical)
 *   - Mathematical Practice (Institutional): The day-to-day use of ordinals within ZFC respects the stratification solution; ordinals are tools that work because the paradox forced their precise formalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burali_forte_paradox, 0.12).
domain_priors:suppression_score(burali_forte_paradox, 0.03).
domain_priors:theater_ratio(burali_forte_paradox, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burali_forte_paradox, extractiveness, 0.12).
narrative_ontology:constraint_metric(burali_forte_paradox, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(burali_forte_paradox, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(burali_forte_paradox, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(burali_forte_paradox, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(burali_forte_paradox, mountain).
narrative_ontology:human_readable(burali_forte_paradox, "Burali-Forti Paradox: The Ordinal Hierarchy Barrier").
narrative_ontology:topic_domain(burali_forte_paradox, "mathematical_logic/set_theory").

domain_priors:emerges_naturally(burali_forte_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMAL ORDINAL THEORIST (MOUNTAIN) — An agent attempting to construct ordinal arithmetic without formal axiomatic constraint faces an irreducible barrier: self-reference through the collection-of-all-ordinals cannot be avoided by naive reasoning. The paradox is not a limitation of a particular framework but a structural limit on what informal set construction can coherently express. No exit from this constraint without formal axiomatization.
constraint_indexing:constraint_classification(burali_forte_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: LOGICAL ANALYST (MOUNTAIN) — From a civilizational analytical view, the Burali-Forti paradox instantiates a fundamental logical closure: any consistent formal system must exclude from the domain of legitimate set-theoretic objects the very totality it purports to order. The barrier is not epistemic (we could know more) but structural (the system itself forbids certain constructions). This is a mathematical law.
constraint_indexing:constraint_classification(burali_forte_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: SET THEORY AXIOMATIZER (MOUNTAIN) — Institutional mathematics (ZFC, Von Neumann hierarchy, Grothendieck universes) copes with the paradox through stratification: ordinals are ordered by rank, and the collection of all ordinals cannot belong to any single rank. The constraint remains immutable — the axioms are *chosen* to respect it, not to avoid it. The axiomatizer experiences the paradox as a boundary condition that must be honored, not a problem to be solved.
constraint_indexing:constraint_classification(burali_forte_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burali_forte_paradox_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(burali_forte_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burali_forte_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(burali_forte_paradox, ExtMetricName, E),
    domain_priors:suppression_score(burali_forte_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(burali_forte_paradox),
    narrative_ontology:constraint_metric(burali_forte_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(burali_forte_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(burali_forte_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The Burali-Forti paradox does not extract value from any agent toward any other. It is a structural boundary that all mathematicians must respect equally. There is no beneficiary (no group benefits from the paradox's existence) and no victim (the barrier protects the coherence of the entire system). The low value reflects the absence of asymmetric benefit — this is a pure constraint on what is logically possible, not a mechanism of differential advantage. Suppression (0.03): Minimal. The suppression of alternative approaches (naive ordinal theory) is not coercive or institutional — it flows from logical necessity. An agent cannot 'choose' to escape the paradox; the paradox is not enforced but discovered. Attempts to build coherent ordinal arithmetic without respecting the boundary fail for reasons of logical consistency, not institutional prohibition. Theater ratio (0.15): Low. The mathematics involved is purely functional. The distinction between sets and proper classes solves a real coordination problem: how to coherently order ordinals without infinite regress or contradiction. There is no performative element — the axioms do what they claim. The modest non-zero value reflects that formalization itself involves some interpretive choice (the Von Neumann hierarchy represents one way to stratify, not the only logical solution in principle), but the core mathematics is not theatrical.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives converge on mountain classification. The informal theorist experiences the paradox as an inescapable barrier. The axiomatizer experiences it as a boundary condition that constrains but doesn't oppress. The analytical observer recognizes it as a universal structural limit. Unlike most constraints, the Burali-Forti paradox does not exhibit perspectival disagreement because no agent occupies a position of differential advantage or disadvantage relative to it. The paradox is genuinely universal — it binds all mathematical agents equally, regardless of their position within mathematics. This uniformity is itself diagnostic: when a constraint appears to be mountain-type from all perspectives without exception, the mountain classification is on firmest ground. No hidden extraction mechanism, no naturalization of contingent arrangements — just mathematical structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The Burali-Forti paradox represents a clean resolution of mandatrophy through universal consensus. There is no debate about whether the paradox exists or what it constrains. Mathematical communities do not disagree on the classification or the nature of the barrier. The paradox is not claimed to be natural law by some and construction by others — it is recognized as a structural fact across all foundational systems. The absence of mandatrophy is itself notable and instructive: it shows that genuine mathematical necessities can be identified and distinguished from institutional constraints or extractive mechanisms. The constraint's stability over 125+ years (since 1897) without revision or contestation further confirms the mountain classification. No new data has suggested alternative solutions; no mathematical community has attempted to challenge the fundamental boundary. This is precisely the immutability profile we expect from a true natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    informal_vs_formal_boundary,
    'Is the Burali-Forti paradox a property of informal set construction or a mathematical law that formal systems must respect?',
    'Examine whether every consistent formal system (ZFC, NBG, Morse-Kelley, type theory, category-theoretic foundations) exhibits the same structural barrier or whether the barrier is specific to particular axiomatic choices.',
    'If universal: paradox is a genuine natural law of mathematical structure. If variable: it is a contingent feature of chosen axioms, and the constraint is tangled_rope (axioms enforce a coordination solution that benefits the axiomatizers).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(informal_vs_formal_boundary, empirical, 'Whether Burali-Forti is universal to all consistent systems or contingent to axiomatic choice').

omega_variable(
    collection_vs_set_distinction_grounding,
    'Is the distinction between legitimate sets and improper collections (like the collection of all ordinals) grounded in logic, in chosen axioms, or in a deeper structural necessity?',
    'Trace whether the set/non-set distinction can be motivated from first principles (logical consistency requirements alone) or requires explicit axiomatization. Compare across formalization frameworks (ZFC, category theory, homotopy type theory).',
    'If grounded in logic alone: mountain status is secure. If requires explicit axioms: the constraint may be tangled_rope (axioms embed a choice that benefits certain mathematical practices over others).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collection_vs_set_distinction_grounding, conceptual, 'Grounding of the set/collection distinction in logic vs axioms').

omega_variable(
    rank_stratification_necessity,
    'Does the Von Neumann rank hierarchy represent the only coherent solution to the Burali-Forti barrier, or are alternative stratification schemes possible with different mathematical consequences?',
    'Survey alternative foundational systems (Kelley-Morse class theory, type-theoretic foundations, categorical set theory) and assess whether they all converge on equivalent solutions or permit genuinely distinct approaches.',
    'If unique: the mountain classification is strengthened (nature imposes a single solution). If multiple: the constraint might be tangled_rope (institutional mathematics selects among competing coherent frameworks, each with different coordination properties).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rank_stratification_necessity, empirical, 'Whether Von Neumann rank hierarchy is unique or contingent among solutions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(burali_forte_paradox, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bfp_tr_t0, burali_forte_paradox, theater_ratio, 0, 0.12).
narrative_ontology:measurement(bfp_tr_t50, burali_forte_paradox, theater_ratio, 50, 0.13).
narrative_ontology:measurement(bfp_tr_t100, burali_forte_paradox, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(bfp_be_t0, burali_forte_paradox, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(bfp_be_t50, burali_forte_paradox, base_extractiveness, 50, 0.11).
narrative_ontology:measurement(bfp_be_t100, burali_forte_paradox, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(burali_forte_paradox, information_standard).
narrative_ontology:affects_constraint(burali_forte_paradox, cantor_set_theory_informal).
narrative_ontology:affects_constraint(burali_forte_paradox, zfc_axiom_foundation).
narrative_ontology:affects_constraint(burali_forte_paradox, grothendieck_universe_hierarchy).

% DUAL FORMULATION NOTE:
% The Burali-Forti paradox is the foundational constraint from which formal set theory derives. The paradox itself (this story) is the natural-law recognition. The various axiomatizations (ZFC, Von Neumann hierarchy, etc.) are institutional responses to this constraint — they are not separate constraints but coherent formal systems respecting a single underlying mathematical barrier. The network edges point to downstream constraints that inherit or apply the Burali-Forti structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
