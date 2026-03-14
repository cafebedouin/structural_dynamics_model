% ============================================================================
% CONSTRAINT STORY: finite_group_classification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_finite_group_classification, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: finite_group_classification
 *   human_readable: The Finite Group Classification Problem
 *   domain: pure_mathematics/group_theory
 *
 * SUMMARY:
 *   The finite group classification is the mathematical theorem that every
 *   finite group is isomorphic to exactly one group in a well-defined,
 *   exhaustive list. This constraint is canonical example of a natural law in
 *   the Deferential Realism framework — it emerges from pure logical
 *   necessity without extraction, coordination burden, or suppression. The
 *   classification reflects the actual mathematical structure of finite
 *   algebraic objects and cannot be negotiated, escaped, or reformulated. All
 *   agents confronting this constraint experience it as an immutable
 *   necessity. There are no beneficiaries or victims — the constraint is a
 *   property of mathematical reality itself.
 *
 * KEY AGENTS:
 *   - Aspiring group theorists (powerless/trapped): must internalize the classification to participate in the field
 *   - Research mathematicians (moderate/trapped): structure their work within the fixed landscape of known finite groups
 *   - Mathematical institutions (institutional/analytical): organize curricula and research around the classification
 *   - Analytical observer (analytical/analytical): recognizes the classification as a logical necessity, not a social arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(finite_group_classification, 0.12).
domain_priors:suppression_score(finite_group_classification, 0.03).
domain_priors:theater_ratio(finite_group_classification, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(finite_group_classification, extractiveness, 0.12).
narrative_ontology:constraint_metric(finite_group_classification, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(finite_group_classification, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(finite_group_classification, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(finite_group_classification, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(finite_group_classification, mountain).
narrative_ontology:human_readable(finite_group_classification, "The Finite Group Classification Problem").
narrative_ontology:topic_domain(finite_group_classification, "pure_mathematics/group_theory").

domain_priors:emerges_naturally(finite_group_classification).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ASPIRING GROUP THEORIST (MOUNTAIN) — Must learn the finite group classification hierarchy to advance in the field. Cannot negotiate with or circumvent the logical structure. The constraint is perceived as immutable because it reflects the actual structure of finite groups, not institutional convention. Zero degrees of freedom.
constraint_indexing:constraint_classification(finite_group_classification, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE RESEARCH MATHEMATICIAN (MOUNTAIN) — At generational timescale, the classification remains unchanged. Every generation must recapitulate the same mathematical structure. The constraint is not a coordination problem or extraction mechanism — it is a logical fact about the nature of finite groups. Cannot exit or reframe.
constraint_indexing:constraint_classification(finite_group_classification, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN) — From the mathematical logic perspective, finite group classification is a fundamental theorem about the structure of finite algebraic objects. The classification emerges necessarily from the axioms of group theory. No alternative framework generates a different classification of the same objects. The constraint is universal and unchanging.
constraint_indexing:constraint_classification(finite_group_classification, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(finite_group_classification_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(finite_group_classification, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(finite_group_classification, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(finite_group_classification, ExtMetricName, E),
    domain_priors:suppression_score(finite_group_classification, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(finite_group_classification),
    narrative_ontology:constraint_metric(finite_group_classification, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(finite_group_classification, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(finite_group_classification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. No agent extracts value from others via this constraint — it is a shared logical necessity. The score reflects only the minimal epistemic cost of learning the classification, which is a coordination overhead not an extraction mechanism. Suppression (0.03): Minimal. No agent faces coercive barriers or lack of alternatives enforced by this constraint. The classification is simply true; there is no alternative that works or that agents could be prevented from seeing. Theater ratio (0.08): Minimal. The entire content of the constraint is functional — proving and stating the classification. There is no performative content, no ritual component, no Goodhart substitution. Emerges naturally (true): The classification emerges necessarily from the axioms of group theory and the definition of 'finite group.' No enforcement mechanism is needed to maintain it. Accessibility collapse (0.92): Very high. The classification is completely inaccessible to being replaced, reformulated, or negotiated. Any alternative would either be equivalent (same classification under different labeling) or false (incorrect statement about finite groups). Resistance (0.08): Very low. There is no structural resistance to accepting the classification — the barrier is not social or political but purely epistemic (understanding the proof). Once understood, there is nothing to resist against. These metrics place the constraint firmly in mountain territory with no borderline cases.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All agents perceive the finite group classification as an immutable mathematical fact, classified as mountain from every structural position. The powerless agent, the moderate mathematician, and the analytical observer all agree on the classification type — not because they share interests (they don't) but because the constraint is genuinely invariant across all positions. This uniformity is itself the diagnostic signature of a mountain: same classification from (powerless, trapped), (moderate, trapped), and (analytical, analytical) perspectives indicates a natural law rather than a social or institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality analysis does not apply to this constraint. There are no beneficiaries or victims. No agent is positioned as a target or extractor relative to the finite group classification. The constraint is not a relationship between agents but a property of mathematical reality. The chi formula does not activate because there is no extraction function f(d) — the constraint lacks the directionality axis entirely. This is by design: mountain constraints at high accessibility_collapse and low resistance do not generate directionality metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY DETECTED. The finite group classification presents no risk of false natural law claims or mislabeled coordination. The constraint is genuinely a mountain — a logical necessity, not a social arrangement. The logical necessity is not being hidden or claimed falsely; it is openly acknowledged as theorem rather than policy. No alternative institutional arrangement could modify the classification of finite groups, so there is no shadowed coordination function. The constraint cleanly separates from any mandatrophy pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    axiom_dependence,
    'Does the finite group classification depend on the specific choice of axioms for group theory, or does it emerge from multiple equivalent axiomatic foundations?',
    'Demonstrate that alternative axiomatic foundations (constructive mathematics, categorical group theory, tropical algebra) either yield the same classification or classify ''finite group'' differently such that the two classifications are not comparable.',
    'If axiom-independent: mountain classification is fully robust. If axiom-dependent: classification becomes a ''mountain within ZFC'' rather than a universal natural law — a subtle but meaningful distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_dependence, conceptual, 'Axiom dependence of the finite group classification').

omega_variable(
    practical_verification_floor,
    'What is the practical limit to direct verification of the finite group classification given computational resources and mathematical proof length?',
    'Formal characterization of proof length for the classification theorem and comparison to computational verification limits. Determine whether any aspect of the classification relies on mathematical results that cannot be practically verified.',
    'If all steps are practically verifiable: the mountain is fully epistemically accessible. If some steps exceed practical verification: a gap emerges between logical necessity and epistemic certainty — the constraint remains a mountain but with an epistemological shadow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_verification_floor, empirical, 'Computational verifiability of the finite group classification').

omega_variable(
    discretionary_indexing_conventions,
    'How much of the finite group classification reflects the intrinsic structure of finite groups, and how much reflects choice of notation, ordering, and presentation conventions?',
    'Separate the invariant structural content (composition series, Sylow subgroups, solvability) from the conventional content (labeling of simple groups, ordering by size, presentation as a list). Formalize both in a framework-neutral representation.',
    'If invariant content fully determines structure: mountain classification applies to the content. If conventions affect what counts as ''classified'': the constraint partially depends on human choice and might blend mountain (invariant structure) with rope (coordinative convention).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discretionary_indexing_conventions, conceptual, 'Role of presentation conventions in the classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(finite_group_classification, 0, 1).

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
