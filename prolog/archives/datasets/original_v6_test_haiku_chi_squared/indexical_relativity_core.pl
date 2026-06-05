% ============================================================================
% CONSTRAINT STORY: indexical_relativity_core
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indexical_relativity_core, []).

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
 *   constraint_id: indexical_relativity_core
 *   human_readable: The Law of Indexical Relativity
 *   domain: epistemological/ontological
 *
 * SUMMARY:
 *   The law of indexical relativity is the foundational logical principle of
 *   the Deferential Realism framework: the classification of any constraint
 *   (as mountain, rope, snare, tangled_rope, scaffold, or piton) is not an
 *   intrinsic property of the constraint itself but depends on the observer's
 *   structural position within a four-dimensional index space (P, T, E, S).
 *   This principle states that there is no view from nowhere — every
 *   classification is indexed to a position defined by agent power, time
 *   horizon, exit options, and spatial scope. The same constraint structure
 *   appears as a natural law (mountain) to one observer, pure extraction
 *   (snare) to another, and coordination (rope) to a third. The law of
 *   indexical relativity is itself a mountain: a logical requirement that
 *   cannot be escaped. It does not describe an extractive mechanism; it
 *   describes the structure that all mechanisms (extractive, coordinative,
 *   and structural) must respect.
 *
 * KEY AGENTS:
 *   - The Logical Structure: The constraint itself — the requirement that classification is index-relative. Not an actor; a formal principle.
 *   - The Analytical Observer: Agent holding the civilizational/universal/analytical perspective — can see the constraint's logical necessity.
 *   - The Powerless Agent: Victim of index imprisonment — trapped in a single perspective (e.g., immediate/local/trapped) that prevents access to higher-level views where the constraint might be negotiable.
 *   - The Institutional Actor: Beneficiary of index control — gains power through selecting which index dominates within a domain.
 *   - The Framework Itself: Deferential Realism as a system of measurement and classification — built entirely on the assumption of indexical relativity.
 *   - The Mathematical Formalist: Observer who sees the constraint as a product-space geometry issue, not a power issue.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indexical_relativity_core, 0.08).
domain_priors:suppression_score(indexical_relativity_core, 0.02).
domain_priors:theater_ratio(indexical_relativity_core, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indexical_relativity_core, extractiveness, 0.08).
narrative_ontology:constraint_metric(indexical_relativity_core, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(indexical_relativity_core, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(indexical_relativity_core, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(indexical_relativity_core, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indexical_relativity_core, mountain).
narrative_ontology:human_readable(indexical_relativity_core, "The Law of Indexical Relativity").
narrative_ontology:topic_domain(indexical_relativity_core, "epistemological/ontological").

domain_priors:emerges_naturally(indexical_relativity_core).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOGICAL OBSERVER (MOUNTAIN) — Classification is relative to an index (P,T,E,S). This is not a contingent fact about social power but a logical requirement: the same constraint must be evaluated from some observer position. No observer can escape this requirement. ε=0.08 reflects that the constraint is purely formal — it describes the structure of classification itself, not any extractive mechanism.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL FORMALIST (MOUNTAIN) — The index tuple (P,T,E,S) is a product space; classification is a function of all four axes. You cannot remove an axis and still have meaningful classification. You cannot measure a constraint without specifying where you are in the space. This is a mathematical fact, not a political choice. ε=0.08; suppression=0.02 reflects zero coercion — the constraint is purely structural.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: POWERLESS AGENT UNDER MISCLASSIFICATION (MOUNTAIN) — If a Snare is falsely classified as a Mountain (natural law), the powerless agent cannot exit because escape would require denying the law itself. The indexical relativity principle guarantees that some perspective exists from which the constraint is not a natural law — but the powerless agent trapped in the beneficiary's frame has no access to that perspective. The constraint itself (indexical relativity) remains mountain-class; the false classification is the extraction mechanism.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 4: INSTITUTIONAL ACTOR CONTROLLING INDEX (MOUNTAIN) — If an institution controls which index is used to evaluate constraints within its domain, it gains power not through direct extraction but through index selection. The law of indexical relativity guarantees that every index is legitimate — but choosing which index dominates is a form of power. The institutional actor sees the constraint as pure structure; the powerless agent trapped in an unwanted index experiences this structure as extraction.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 5: THE FRAMEWORK (MOUNTAIN) — Deferential Realism is built on indexical relativity. The six constraint types (mountain, rope, tangled_rope, snare, scaffold, piton) are only defined relative to an index. No constraint has an intrinsic type independent of (P,T,E,S). This is not a limitation of the framework — it is the framework's core claim. The framework is therefore not describing an extractive mechanism; it is describing the logical structure that all extractive mechanisms (and coordination mechanisms) must respect.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indexical_relativity_core_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(indexical_relativity_core, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indexical_relativity_core, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(indexical_relativity_core, ExtMetricName, E),
    domain_priors:suppression_score(indexical_relativity_core, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(indexical_relativity_core),
    narrative_ontology:constraint_metric(indexical_relativity_core, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(indexical_relativity_core, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(indexical_relativity_core_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Extremely low. The constraint is purely formal. It describes how classification works, not how extraction works. It imposes zero coercive cost on any observer — it is not possible to violate the principle. No agent can classify a constraint without using an index; therefore, no agent can refuse indexical relativity. The small non-zero value (0.08 rather than 0.00) reflects that the principle creates asymmetries in who controls which index dominates in practical classification contexts. Suppression (0.02): Negligible. The constraint has no alternatives to suppress. It is not a rule imposed on agents; it is a logical requirement of meaningful classification. Resistance (0.08): Very low. There is no way to resist the principle because violating it produces incoherence, not alternative freedom. Theater ratio (0.05): Minimal. The constraint has no performative content. Its truth is purely structural. Accessibility collapse (0.92): Very high. The constraint is accessible (knowable) from all perspectives — agents at any index can in principle understand that their classification is index-relative. The collapse reflects that understanding the constraint does not require privileged access; it requires only formal thinking.
 *
 * PERSPECTIVAL GAP:
 *   The gap is not between disagreement on classification but between understanding and its practical use. From the analytical observer's perspective, indexical relativity is a neutral principle — all indices are equally valid, and classification varies with index selection. From the powerless agent's perspective, index imprisonment is a form of extraction: they are forced to occupy a single point in the index space and cannot access the perspectives from which the constraint might be differently classified (and potentially renegotiated). From the institutional actor's perspective, controlling which index dominates is a subtle form of power. From the framework's perspective, indexical relativity is the entire foundation — without it, there would be a claim to classify constraints absolutely, which would be false. The gap is not a perspectival disagreement about the constraint's type (all perspectives agree: mountain) but a disagreement about what indexical relativity implies for power and freedom.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain constraint with no beneficiaries or victims in the ordinary sense. All agents occupy indices and benefit from coherent classification. However, the principle creates asymmetries when institutional actors gain power through index selection. Analytical observer: d≈0.50 (symmetric — sees the principle as neutral structure). Powerless agent trapped in narrow index: d≈0.90 (near-victim position — forced into a single perspective). Institutional actor controlling index: d≈0.10 (beneficiary position — gains power through index dominance, not extraction). The framework itself: d≈0.50 (symmetric — indexical relativity is the framework's core, not its victim or beneficiary). No override needed; the derivation captures the structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    index_selection_power,
    'When does index selection become a form of extractive control?',
    'Identification of cases where institutional actors systematically narrow the index space (e.g., restricting time_horizon to immediate, spatial_scope to local) to prevent higher-level perspectives from accessing the constraint. Measurement of institutional gatekeeping on perspectival access.',
    'If index selection is analyzed as a separate constraint (institutional index control), the original constraint remains mountain. If index selection is absorbed into the original constraint''s classification, the constraint appears as Tangled Rope (coordination + extraction). This is a decomposition question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(index_selection_power, conceptual, 'Whether index selection constitutes a distinct extraction mechanism').

omega_variable(
    natural_law_false_summit_detection,
    'Can the principle itself (indexical relativity) be falsified, or is it analytic?',
    'Identify any constraint that exhibits the same classification from ALL possible indices despite varying (P,T,E,S) values. If such a constraint exists, indexical relativity has a counterexample. Current candidates: logical/mathematical limits, physical constants.',
    'If indexical relativity is analytic (true by definition of classification): it remains mountain for all perspectives. If it is empirical (contingent on how the world is): it may be snare (e.g., if index selection is always constrained by material reality, forcing a particular perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_false_summit_detection, conceptual, 'Whether indexical relativity is analytic or empirical').

omega_variable(
    observer_independence,
    'Is there a perspective-independent fact about constraints, or is perspective all the way down?',
    'Attempt to formulate a constraint classification that is invariant across all indices. Compare to actual corpus data from the same constraint classified from multiple perspectives. If invariance is empirically rare, the framework is fundamentally perspectival; if common, there is a deeper structure beneath perspective.',
    'If perspective-independent facts exist: indexical relativity is a map-territory distinction, not a claim that territory itself is perspective-dependent. If not: classification is purely relational; this claim has philosophical implications for realism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(observer_independence, preference, 'Whether perspective-independent facts about constraints exist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indexical_relativity_core, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indrel_tr_t0, indexical_relativity_core, theater_ratio, 0, 0.04).
narrative_ontology:measurement(indrel_tr_t5, indexical_relativity_core, theater_ratio, 5, 0.05).
narrative_ontology:measurement(indrel_tr_t10, indexical_relativity_core, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(indrel_be_t0, indexical_relativity_core, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(indrel_be_t5, indexical_relativity_core, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(indrel_be_t10, indexical_relativity_core, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indexical_relativity_core, information_standard).
narrative_ontology:affects_constraint(indexical_relativity_core, perspectival_gap_classification).
narrative_ontology:affects_constraint(indexical_relativity_core, index_selection_power).
narrative_ontology:affects_constraint(indexical_relativity_core, false_natural_law_detection).

% DUAL FORMULATION NOTE:
% Indexical relativity is the meta-constraint on which all other constraints in the DR framework depend. It has no upstream constraints — it is foundational. Its downstream constraints are all constraints that depend on index-relative classification: any constraint whose type varies with the observer's position in (P,T,E,S) space is downstream of indexical relativity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
