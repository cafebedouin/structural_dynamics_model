% ============================================================================
% CONSTRAINT STORY: hilberts_hotel
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hilberts_hotel, []).

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
 *   constraint_id: hilberts_hotel
 *   human_readable: Hilbert's Hotel Infinite Capacity Accommodation
 *   domain: mathematical_logic/set_theory
 *
 * SUMMARY:
 *   Hilbert's Hotel is a thought experiment demonstrating the properties of
 *   infinite cardinality in formal mathematics. A hotel with countably
 *   infinite rooms, each occupied by a guest, can accommodate any finite
 *   number of additional guests by reassigning existing guests via the
 *   bijection n → n+1 (moving each guest from room n to room n+1, freeing
 *   room 1). This procedure is not a contingent policy or institutional
 *   arrangement but a logical consequence of Cantor's definition of countable
 *   infinity. The constraint exhibits zero degrees of freedom from all
 *   perspectives: the reallocation is immutable, universal, and independent
 *   of observation, enforcement, or preference. No agent can exit the
 *   constraint, because the constraint is not a social structure but a
 *   mathematical property. The thought experiment has become a canonical
 *   pedagogical tool in set theory and model theory, making it one of the
 *   clearest exemplars of a constraint that classifies as Mountain from every
 *   perspective.
 *
 * KEY AGENTS:
 *   - Mathematical logicians: Analytical observers (analytical/analytical) — define and prove the bijection
 *   - Set theorists: Powerful agents (powerful/analytical) — establish foundational axioms
 *   - Physics community: Institutional actors (institutional/analytical) — search for instantiations of infinite capacity
 *   - Students of mathematics: Moderate power (moderate/biographical/analytical) — learn the constraint through proof and counterintuition
 *   - The hotel (abstraction): No agent — the hotel is the constraint itself, not an actor within it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hilberts_hotel, 0.08).
domain_priors:suppression_score(hilberts_hotel, 0.02).
domain_priors:theater_ratio(hilberts_hotel, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hilberts_hotel, extractiveness, 0.08).
narrative_ontology:constraint_metric(hilberts_hotel, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(hilberts_hotel, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hilberts_hotel, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hilberts_hotel, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hilberts_hotel, mountain).
narrative_ontology:human_readable(hilberts_hotel, "Hilbert's Hotel Infinite Capacity Accommodation").
narrative_ontology:topic_domain(hilberts_hotel, "mathematical_logic/set_theory").

domain_priors:emerges_naturally(hilberts_hotel).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The bijective reallocation procedure (n → n+1) is a logical necessity, not a contingent institutional arrangement. Follows from Cantor's definition of infinite cardinality. No alternatives exist; no suppression required.
constraint_indexing:constraint_classification(hilberts_hotel, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Infinite divisibility and infinite regress in field theory exhibit the same structural property. The constraint is universal across formal systems where infinity is well-defined.
constraint_indexing:constraint_classification(hilberts_hotel, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The paradox dissolves once countable infinity is properly understood. The constraint that 'infinite + 1 = infinite' is immutable once Dedekind's definition is accepted.
constraint_indexing:constraint_classification(hilberts_hotel, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(universal))).

% Proof of the hotel's capacity is constructive and mechanically reproducible. No institutional power, funding, or suppression can alter the result. The constraint is enforcement-independent.
constraint_indexing:constraint_classification(hilberts_hotel, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hilberts_hotel_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hilberts_hotel, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hilberts_hotel, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hilberts_hotel, ExtMetricName, E),
    domain_priors:suppression_score(hilberts_hotel, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hilberts_hotel),
    narrative_ontology:constraint_metric(hilberts_hotel, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hilberts_hotel, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hilberts_hotel_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε = 0.08): Minimal. There is no agent within the thought experiment who extracts from another. The bijection is a pure rearrangement, with no asymmetric benefit or cost. The nonzero value reflects the pedagogical 'theater' required to communicate the result (it seems paradoxical until formalized), not actual extraction. Suppression (0.02): Near zero. Once the axioms of set theory and the definition of countable infinity are accepted, no suppression is required. The procedure is purely constructive and transparent. There are no hidden mechanisms, power asymmetries, or coercive enforcement. Theater ratio (0.15): Low. The appearance of paradox (fully occupied hotel accommodates new guests) is resolved by formal proof. The theater is confined to the pedagogical gap between intuition and rigor, not embedded in the structure itself. Accessibility collapse (0.92): Very high. The constraint is inaccessible to agents without formal training in set theory and mathematical logic. Once that barrier is crossed, the constraint is completely transparent. Resistance (0.08): Very low. There is no plausible alternative to the bijection once the axioms are accepted. The constraint is invariant across all formalizations of set theory that include countable infinity.
 *
 * PERSPECTIVAL GAP:
 *   Hilbert's Hotel exhibits zero perspectival gap. All four perspectives produce Mountain classification with identical structure. The mathematical logician, theoretical physicist, student, and institutional observer all agree: the constraint is a logical necessity, universal, enforcement-independent, and invariant across all observation contexts. This unanimity is the defining feature of a pure Mountain. The apparent gap between intuition and formality is pedagogical (theater ratio reflects teaching overhead) but does not create genuine classification disagreement. Even the student who finds the result counterintuitive, once they verify the proof, classifies it identically to the expert.
 *
 * DIRECTIONALITY LOGIC:
 *   The standard directionality chain (beneficiary/victim + exit → d) does not apply to Hilbert's Hotel because there are no beneficiaries or victims. The constraint is a mathematical structure, not a social structure. All agents occupy identical structural positions: they are observers of the constraint, not actors within it. The constraint has no victims (the guests are accommodated), no beneficiaries (no agent gains extraction), and no extractive mechanism. The hotel does not exist; it is a model used to study infinite cardinality. Directionality is undefined for this constraint — it is a property of mathematical structures, not social relationships. The engine will derive d = 0.5 (symmetric) for all agents by default, but this is a null classification: it reflects the absence of asymmetric power, not a balanced power relationship.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    finite_approximation_relevance,
    'Does Hilbert''s Hotel constrain any real-world system, or is it purely a mathematical abstraction with no physical correlates?',
    'Survey applications in physics (quantum field theory, cosmology), computer science (memory allocation), and economics (market scaling). If applications exist and rely on the infinite-case proof, the constraint is instantiated in engineered systems.',
    'If purely abstract: the constraint remains universal but non-instantiated. If instantiated: the constraint has physical consequences for systems approximating infinite capacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(finite_approximation_relevance, conceptual, 'Whether the constraint has instantiation in physical or engineered systems').

omega_variable(
    choice_axiom_dependence,
    'Is the bijection n → n+1 constructive (provable in intuitionistic logic) or does it require the Axiom of Choice?',
    'Review formal proofs in intuitionistic set theory (IZF). If constructive: the constraint is stronger (provable without choice). If it requires choice: the constraint is weaker (depends on unfinished foundations of mathematics).',
    'Constructive: mountain classification is ironclad. Requires choice: the mountain is contingent on foundational assumptions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(choice_axiom_dependence, empirical, 'Constructiveness of the bijection under intuitionistic logic').

omega_variable(
    measurement_incompleteness,
    'Can ''accommodation of guests'' be defined precisely in infinite set theory without smuggling in temporal or epistemic assumptions?',
    'Formal definition of ''accommodation'' in model theory. Check whether the definition requires: (a) a privileged frame of reference, (b) a notion of ''completion'' or ''finished state'', (c) an observer outside the system.',
    'If smuggled assumptions: the constraint dissolves into a definition mismatch (mountain is false). If clean: the mountain is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_incompleteness, conceptual, 'Whether the definition of accommodation requires hidden temporal or epistemic scaffolding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hilberts_hotel, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hilbert_tr_t0, hilberts_hotel, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hilbert_tr_t50, hilberts_hotel, theater_ratio, 50, 0.15).
narrative_ontology:measurement(hilbert_tr_t100, hilberts_hotel, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(hilbert_be_t0, hilberts_hotel, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hilbert_be_t50, hilberts_hotel, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(hilbert_be_t100, hilberts_hotel, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hilberts_hotel, information_standard).
narrative_ontology:affects_constraint(hilberts_hotel, cantors_diagonal_argument).
narrative_ontology:affects_constraint(hilberts_hotel, peano_axioms_recursion).

% DUAL FORMULATION NOTE:
% Hilbert's Hotel is part of the mathematical foundations cluster alongside Cantor's diagonal argument (ε=0.05, Mountain) and Peano axioms (ε=0.03, Mountain). All three are logically independent proofs that infinite sets have properties distinct from finite sets. The network link reflects shared foundational status, not causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
