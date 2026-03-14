% ============================================================================
% CONSTRAINT STORY: cantor_cardinality_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cantor_cardinality_hierarchy, []).

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
 *   constraint_id: cantor_cardinality_hierarchy
 *   human_readable: Cantor's Cardinality Hierarchy
 *   domain: mathematics/set_theory
 *
 * SUMMARY:
 *   Cantor's cardinality hierarchy is a foundational mathematical truth
 *   establishing that infinite sets can be ordered by cardinality, with the
 *   power set of any set having strictly greater cardinality than the set
 *   itself. This constraint is the purest exemplar of a Mountain
 *   classification: it is invariant across all coherent mathematical
 *   frameworks, emerges naturally from the definitions of set, cardinality,
 *   and power set, and admits no escape route through any choice of axioms,
 *   interpretation, or agent position. The constraint does not extract value
 *   from any agent, does not suppress alternatives through institutional
 *   power, and does not maintain itself through theatrical activity. It
 *   simply is — a logical necessity.
 *
 * KEY AGENTS:
 *   - All mathematical agents: Collectively, all mathematicians, logicians, and reasoning systems are constrained by this hierarchy with no differential benefit or cost structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cantor_cardinality_hierarchy, 0.12).
domain_priors:suppression_score(cantor_cardinality_hierarchy, 0.02).
domain_priors:theater_ratio(cantor_cardinality_hierarchy, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cantor_cardinality_hierarchy, extractiveness, 0.12).
narrative_ontology:constraint_metric(cantor_cardinality_hierarchy, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(cantor_cardinality_hierarchy, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cantor_cardinality_hierarchy, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(cantor_cardinality_hierarchy, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cantor_cardinality_hierarchy, mountain).
narrative_ontology:human_readable(cantor_cardinality_hierarchy, "Cantor's Cardinality Hierarchy").
narrative_ontology:topic_domain(cantor_cardinality_hierarchy, "mathematics/set_theory").

domain_priors:emerges_naturally(cantor_cardinality_hierarchy).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FINITE MATHEMATICIAN (MOUNTAIN) — No escape from the fact that the real numbers are strictly more numerous than the natural numbers. This is not a choice or institutional arrangement; it is a logical necessity that follows from the definition of cardinality and the properties of infinite sets. The constraint is immutable across all mathematical frameworks.
constraint_indexing:constraint_classification(cantor_cardinality_hierarchy, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: CONSTRUCTIVIST MATHEMATICIAN (MOUNTAIN) — Even within constructivism, which rejects certain classical infinities, the Cantor hierarchy emerges. Constructive cardinality still orders infinite sets. The constraint persists even when epistemology changes; the structure is invariant to the foundational debate.
constraint_indexing:constraint_classification(cantor_cardinality_hierarchy, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: SET THEORIST (MOUNTAIN) — Even with full intellectual mobility and power to define axiom systems, the Cantor hierarchy cannot be avoided within any consistent mathematical framework. Choosing different axioms (ZFC, NBG, category theory) produces isomorphic orderings. The constraint is universal across all coherent mathematical systems.
constraint_indexing:constraint_classification(cantor_cardinality_hierarchy, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From the perspective of mathematical logic, the Cantor hierarchy is a provable fact within any sound axiom system containing basic set-theoretic operations. The constraint emerges from the definitions themselves, not from any institutional or social arrangement. It is a natural law of mathematics.
constraint_indexing:constraint_classification(cantor_cardinality_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cantor_cardinality_hierarchy_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(cantor_cardinality_hierarchy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cantor_cardinality_hierarchy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cantor_cardinality_hierarchy, ExtMetricName, E),
    domain_priors:suppression_score(cantor_cardinality_hierarchy, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(cantor_cardinality_hierarchy),
    narrative_ontology:constraint_metric(cantor_cardinality_hierarchy, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(cantor_cardinality_hierarchy, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(cantor_cardinality_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint imposes no extraction — no group benefits from the constraint while others bear costs. The asymmetry of cardinalities is symmetric across all observers; knowledge of the hierarchy does not advantage any agent structurally. The non-zero value reflects the minimal cost of understanding and working within the constraint, not exploitation. Suppression (0.02): Negligible. There are no suppressed alternatives — all mathematically coherent frameworks must accommodate the hierarchy. Suppression requires some alternative to be forced into invisibility; here, incoherent alternatives simply do not exist. Theater ratio (0.05): Negligible. The constraint maintains itself through pure logical necessity, not through performative activity. Mathematical proofs of the hierarchy are direct and functional, not ritualistic. Accessibility collapse (0.92): Extremely high. The constraint is fully accessible to any agent capable of understanding basic set theory; there is no epistemic barrier to complete knowledge of the structure. Resistance (0.08): Extremely low. Once the definitions are understood, no agent resists the constraint — it is self-evident. The low values of extractiveness, suppression, and theater, combined with high accessibility and low resistance, are the signature of a natural law.
 *
 * PERSPECTIVAL GAP:
 *   There is no meaningful perspectival gap in this constraint. All four perspectives (finite mathematician, constructivist, set theorist, analytical observer) classify the constraint identically as Mountain. This uniformity is precisely what distinguishes a natural law from a contingent institutional arrangement. Different power levels, time horizons, and exit options do not produce different classifications because the constraint does not depend on power dynamics, temporal change, or escape possibilities. The very absence of perspectival variation is diagnostic evidence for natural law status.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not meaningfully computed for this constraint because there are no beneficiaries or victims. The Cantor hierarchy does not structure asymmetric extraction. All agents relate to it identically: as a constraint they cannot escape and which does not differentially advantage any group. This symmetry is characteristic of natural law constraints. The absence of beneficiary/victim structure means the constraint is not indexed by directionality; it is indexed only by its logical truth-value.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuum_hypothesis_status,
    'Does the continuum hypothesis (that there is no set strictly between the natural numbers and the reals in cardinality) determine the cardinality hierarchy, or is the hierarchy independent of CH?',
    'Gödel-Cohen independence proofs show CH is undecidable in ZFC. The Cantor hierarchy (existence of strictly increasing cardinalities) persists regardless of CH status. The hierarchy does not depend on CH.',
    'The hierarchy is fully determined by basic set-theoretic axioms alone. CH is independent but does not affect the core constraint structure. Classification remains Mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuum_hypothesis_status, empirical, 'Whether CH status affects the Cantor hierarchy classification').

omega_variable(
    axiom_choice_dependence,
    'Does the Cantor hierarchy depend on the Axiom of Choice, or does it hold in ZF without AC?',
    'Cantor''s diagonal argument works in ZF without AC. The existence of ℵ₀ < 2^ℵ₀ is proven without AC. The hierarchy is independent of AC.',
    'The constraint is universal even without AC. All standard mathematical frameworks supporting the constraint exist in the ZF base theory. Classification remains Mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_choice_dependence, empirical, 'Whether AC is required for the Cantor hierarchy').

omega_variable(
    alternative_cardinality_orderings,
    'Could alternative notions of cardinality (cardinal ordinals, Dedekind cardinality, other order types) escape the Cantor hierarchy by redefining what ''cardinality'' means?',
    'Any coherent cardinality notion must preserve the ordering relation induced by bijection and injection. Alternative definitions that violate Cantor''s theorem either collapse to classical cardinality or become non-comparative. The constraint is invariant under cardinality definition.',
    'The constraint persists under all coherent generalizations of cardinality. Escaping it requires incoherence. Classification remains Mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_cardinality_orderings, conceptual, 'Whether alternative cardinality definitions escape the hierarchy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cantor_cardinality_hierarchy, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cantor_tr_t0, cantor_cardinality_hierarchy, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cantor_tr_t50, cantor_cardinality_hierarchy, theater_ratio, 50, 0.05).
narrative_ontology:measurement(cantor_tr_t100, cantor_cardinality_hierarchy, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(cantor_be_t0, cantor_cardinality_hierarchy, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cantor_be_t50, cantor_cardinality_hierarchy, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(cantor_be_t100, cantor_cardinality_hierarchy, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cantor_cardinality_hierarchy, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
