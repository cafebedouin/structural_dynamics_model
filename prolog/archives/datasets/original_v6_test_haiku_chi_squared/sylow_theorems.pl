% ============================================================================
% CONSTRAINT STORY: sylow_theorems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sylow_theorems, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sylow_theorems
 *   human_readable: Sylow Theorems
 *   domain: mathematics/abstract_algebra
 *
 * SUMMARY:
 *   The Sylow theorems are fundamental results in finite group theory,
 *   stating: (1) For a finite group G and prime p, if p^k divides |G|, then G
 *   contains a subgroup of order p^k (Sylow p-subgroups exist). (2) All Sylow
 *   p-subgroups are conjugate. (3) The number of Sylow p-subgroups divides
 *   |G| and is congruent to 1 modulo p. These theorems are mathematical
 *   truths with zero degrees of freedom. No agent—whether a mathematician,
 *   computational system, cryptographic implementation, or pedagogical
 *   framework—can negotiate their way around these constraints. They are not
 *   enforced by any external authority, nor is their function coordinative.
 *   They simply follow from the axioms of group theory with logical
 *   necessity. The Sylow theorems represent a pure mountain constraint: a
 *   natural law within mathematics.
 *
 * KEY AGENTS:
 *   - Mathematical Community: Analytical observers (analytical/civilizational) — discover and verify the theorems through proof and independent confirmation
 *   - Computational Systems: Analytical actors (analytical/civilizational) — must implement algorithms respecting Sylow structure; no circumvention possible
 *   - Pedagogical Systems: Analytical actors (analytical/civilizational) — teach finite group structure using Sylow framework; constraint is foundational to the curriculum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sylow_theorems, 0.08).
domain_priors:suppression_score(sylow_theorems, 0.02).
domain_priors:theater_ratio(sylow_theorems, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sylow_theorems, extractiveness, 0.08).
narrative_ontology:constraint_metric(sylow_theorems, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(sylow_theorems, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sylow_theorems, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(sylow_theorems, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sylow_theorems, mountain).
narrative_ontology:human_readable(sylow_theorems, "Sylow Theorems").
narrative_ontology:topic_domain(sylow_theorems, "mathematics/abstract_algebra").

domain_priors:emerges_naturally(sylow_theorems).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL COMMUNITY (MOUNTAIN) — The Sylow theorems are mathematical truths independent of proof technique, funding, or institutional preference. No agent can negotiate their way around the necessity of p-Sylow subgroups existing in groups of order divisible by p. The constraint is a logical consequence of group axioms; it emerges naturally from the structure of finite groups themselves.
constraint_indexing:constraint_classification(sylow_theorems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPUTATIONAL ALGEBRA PERSPECTIVE (MOUNTAIN) — Whether computing subgroup structures in GAP, Magma, or Sage, algorithms must respect the Sylow constraints. These are not negotiable facts imposed by a regulatory body or enforced through social pressure — they are structural necessities that any algorithm implementation must account for. The constraint emerges naturally from the mathematical structure being computed.
constraint_indexing:constraint_classification(sylow_theorems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: PROOF THEORY PERSPECTIVE (MOUNTAIN) — Multiple independent proofs of Sylow theorems exist (Cauchy, Lagrange cycle, modern group action arguments). The diversity of proofs demonstrates that the constraint is not proof-dependent or methodologically contingent — it is a logical necessity that multiple pathways all confirm. Accessibility is complete: any mathematician with group theory foundation can verify Sylow existence directly.
constraint_indexing:constraint_classification(sylow_theorems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sylow_theorems_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(sylow_theorems, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sylow_theorems, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sylow_theorems, ExtMetricName, E),
    domain_priors:suppression_score(sylow_theorems, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sylow_theorems),
    narrative_ontology:constraint_metric(sylow_theorems, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sylow_theorems, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sylow_theorems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The Sylow theorems make no extractive demand. They are not mechanisms for one agent to take resources from another. A student learning group theory is not extracted from; a computational system checking subgroups is not extracted from. The theorems constrain what is possible, not who gets what. Suppression (0.02): Negligible. There are no suppressed alternatives to Sylow structure. One cannot propose an alternative group theory where Sylow p-subgroups do not exist without abandoning the group axioms entirely. The theoretical freedom is maximal — all paths to understanding groups lead through Sylow inevitably. Theater ratio (0.15): Very low. The Sylow theorems perform no social function; they make no claims about incentives or status. Proofs are direct logical arguments, not performative rituals. Accessibility collapse (0.92): Very high. Any group theorist can access the full logical content of Sylow theorems. Multiple proofs exist; the shortest is accessible to an advanced undergraduate. No gatekeeping, specialization, or tacit knowledge prevents verification. Resistance (0.08): Very low. The theorems have been proven multiple ways for 170+ years with no counterexamples, modifications, or erosion of confidence. Emergence (natural): True. Sylow theorems follow directly from group axioms using elementary combinatorial arguments — they are not imposed by external authority or contingent social structure, but emerge necessarily from the mathematical system itself.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All three perspectives classify the constraint as Mountain. A mathematician, a computational system, and a teacher all encounter the same logical necessity. The Sylow theorems are invariant across all observables and measurement methods — this is the defining property of a mountain constraint at the universal/civilizational scale. The uniformity of classification is the confirmation that the constraint is a natural law, not a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality applies. Mountain constraints do not have beneficiaries or victims. The Sylow theorems do not extract from anyone nor benefit anyone specifically. They constrain the set of possible finite groups, but this is a logical/mathematical constraint, not a structural power relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by being purely natural law. There is no risk of misclassifying coordination as extraction because the theorems perform no coordinative function — they are logical necessities. There is no risk of false summing (calling a snare a mountain) because the underlying metrics (ε=0.08, suppression=0.02, theater=0.15, accessibility=0.92, resistance=0.08) align perfectly with the mountain gates. The constraint is genuinely a natural law of mathematics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sylow_theorems, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sylow_theorems, information_standard).
narrative_ontology:affects_constraint(sylow_theorems, lagrange_theorem).
narrative_ontology:affects_constraint(sylow_theorems, cauchy_theorem).
narrative_ontology:affects_constraint(sylow_theorems, composition_series).

% DUAL FORMULATION NOTE:
% Sylow theorems are downstream of Lagrange's theorem and upstream of the classification of finite simple groups. They form the bridge between basic group axioms and the structure theory of finite groups.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
