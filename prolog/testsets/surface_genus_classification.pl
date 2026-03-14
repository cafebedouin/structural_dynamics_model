% ============================================================================
% CONSTRAINT STORY: surface_genus_classification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_surface_genus_classification, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: surface_genus_classification
 *   human_readable: Surface Genus Classification (Topological Invariant)
 *   domain: mathematics/topology
 *
 * SUMMARY:
 *   Surface genus classification is a mathematical invariant: the genus
 *   (number of 'holes' in a closed orientable surface) is a topological
 *   property that remains unchanged under any continuous deformation without
 *   tearing or gluing. A sphere has genus 0, a torus has genus 1, and so on.
 *   This is not a constraint imposed by institutional power, scarcity, or
 *   design — it is a consequence of the fundamental structure of topology
 *   itself. The classification emerges naturally from the axioms of topology
 *   and cannot be violated or circumvented. There is no extraction,
 *   suppression, or coordination problem because there is no meaningful agent
 *   relationship to the constraint. All perspectives converge: genus is an
 *   immutable natural law.
 *
 * KEY AGENTS:
 *   - Research Mathematician: Analyst (analytical/analytical) — perceives genus as an immutable truth; uses it as a foundation for further work
 *   - Educational Institution: Institutional actor (institutional/analytical) — must convey genus as invariant; no strategic leverage over the constraint
 *   - Student: Learner (moderate/analytical) — encounters genus as mathematical fact; no exit from the constraint because it is not a relationship constraint
 *   - Mathematical Community: Collective observer (analytical/analytical) — maintains consensus that genus is invariant; no dissent because the property is mathematically proven
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(surface_genus_classification, 0.08).
domain_priors:suppression_score(surface_genus_classification, 0.03).
domain_priors:theater_ratio(surface_genus_classification, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(surface_genus_classification, extractiveness, 0.08).
narrative_ontology:constraint_metric(surface_genus_classification, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(surface_genus_classification, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(surface_genus_classification, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(surface_genus_classification, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(surface_genus_classification, mountain).
narrative_ontology:human_readable(surface_genus_classification, "Surface Genus Classification (Topological Invariant)").
narrative_ontology:topic_domain(surface_genus_classification, "mathematics/topology").

domain_priors:emerges_naturally(surface_genus_classification).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TOPOLOGICAL INVARIANT (MOUNTAIN) — The genus of a closed orientable surface is a topological invariant that cannot be changed by continuous deformation. This is a mathematical truth independent of observer or context. The constraint emerges from the fundamental properties of topological equivalence and homeomorphism. No agent can circumvent or negotiate this classification; it is unchangeable across all time horizons and spatial scopes.
constraint_indexing:constraint_classification(surface_genus_classification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: RESEARCH MATHEMATICIAN (MOUNTAIN) — A mathematician working on surface topology perceives genus classification as an immutable constraint on their work. Whether they study genus 0 (sphere), genus 1 (torus), or higher genera, the genus cannot be changed by topological manipulation. The constraint bounds the space of equivalent representations but does not extract or coordinate — it simply defines what is mathematically possible.
constraint_indexing:constraint_classification(surface_genus_classification, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: EDUCATIONAL INSTITUTION (MOUNTAIN) — Universities teaching topology must convey that genus is an invariant property. This is not a choice or negotiable constraint but a foundational fact. Institutions cannot opt out of teaching this constraint without abandoning topological rigor. The constraint is equally binding whether the institution is elite or resource-constrained.
constraint_indexing:constraint_classification(surface_genus_classification, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: STUDENT LEARNING TOPOLOGY (MOUNTAIN) — For a student learning topology, surface genus classification is presented as an unchangeable mathematical fact. Their access to mathematical truth about genus is asymmetric (they lack expertise), but the constraint itself — that genus is an invariant — is not negotiable or extractive. The pedagogical relationship is not a snare or rope but the transparent teaching of natural law.
constraint_indexing:constraint_classification(surface_genus_classification, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(surface_genus_classification_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(surface_genus_classification, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(surface_genus_classification, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(surface_genus_classification, ExtMetricName, E),
    domain_priors:suppression_score(surface_genus_classification, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(surface_genus_classification),
    narrative_ontology:constraint_metric(surface_genus_classification, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(surface_genus_classification, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(surface_genus_classification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The only small non-zero value reflects the pedagogical asymmetry — students initially lack the mathematical knowledge to understand why genus is invariant, creating a temporary knowledge gap. But this is not extraction in the DR sense; it is the normal learning gradient in specialized domains. Suppression (0.03): Minimal. There are no barriers to learning about genus or computing genera of surfaces. Computational methods are well-documented and freely available. Theater ratio (0.05): Minimal. The presentation of genus classification has very low performative content. Proofs that genus is invariant are direct, based on rigorous mathematical foundations. The constraint is exactly what it appears to be.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All perspectives converge on the same classification: mountain. The research mathematician, the educator, the student, and the analytical observer all perceive genus classification as an immutable natural law. The absence of a gap is itself informative — it indicates that the constraint is not a social construction, institutional design, or power relationship masquerading as natural law. It is genuinely invariant.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality logic applies to this mountain constraint. There are no beneficiaries or victims because there is no extraction or coordination problem. The constraint is not relational — it does not operate between agents with conflicting interests. All agents experience the same constraint equally: genus is invariant, and no one can negotiate around this fact.
 *
 * MANDATROPHY ANALYSIS:
 *   UNIFORM MOUNTAIN: This constraint exhibits no mandatrophy because all perspectives classify identically as mountain. There is no tension between claiming coordination (rope) and exhibiting extraction (snare). The constraint is purely a statement of mathematical fact: genus is a topological invariant. The absence of mandatrophy is a diagnostic signal of genuine natural law — the constraint does not mask extraction as coordination, nor does it oscillate between types depending on framing. It is stable across all observation contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(surface_genus_classification, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
