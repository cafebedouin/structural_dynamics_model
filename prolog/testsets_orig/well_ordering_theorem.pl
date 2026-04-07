% ============================================================================
% CONSTRAINT STORY: well_ordering_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_well_ordering_theorem, []).

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
 *   constraint_id: well_ordering_theorem
 *   human_readable: Well Ordering Theorem
 *   domain: mathematical_logic/set_theory
 *
 * SUMMARY:
 *   The Well Ordering Theorem states that every set can be well-ordered — a
 *   property that follows logically from the Axiom of Choice within ZFC set
 *   theory. This constraint exemplifies a mathematical mountain: it is not
 *   enforced by any agent, not extracted from any victim, and not subject to
 *   perspectival variation. The theorem is a logical entailment, not a
 *   negotiated social structure. All mathematicians who accept ZFC axioms
 *   accept the well-ordering theorem as an unavoidable consequence,
 *   regardless of whether they work in constructive logic, category theory,
 *   or applied domains. The constraint is immutable because logical
 *   entailment is immutable. The accessibility collapse (0.92) reflects that
 *   avoiding well-ordering requires abandoning classical set theory entirely
 *   — there is no partial exit, no high-cost workaround that preserves the
 *   coordination function. Resistance (0.08) reflects that mathematicians
 *   have identified the exact axiom (Choice) that generates the constraint,
 *   leaving little room for surprise or hidden mechanisms. This is a pure
 *   natural law: logical structure, not institutional arrangement.
 *
 * KEY AGENTS:
 *   - Constructivist Mathematicians: Cannot exit ZFC frameworks without fundamental commitment cost; must choose between classical logic and alternative foundations
 *   - Working Mathematicians: Experience constraint as background structure; can avoid engaging directly in most applied contexts but cannot escape the logical fact
 *   - Mathematics Discipline: Institutional commitment to ZFC creates collective immobility; no department-level or field-level consensus exists for alternatives
 *   - Computer Scientists: Bear costs of non-constructivity (well-orderings exist but algorithms may not); cannot compute explicit well-orderings for uncountable sets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(well_ordering_theorem, 0.12).
domain_priors:suppression_score(well_ordering_theorem, 0.02).
domain_priors:theater_ratio(well_ordering_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(well_ordering_theorem, extractiveness, 0.12).
narrative_ontology:constraint_metric(well_ordering_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(well_ordering_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(well_ordering_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(well_ordering_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(well_ordering_theorem, mountain).
narrative_ontology:human_readable(well_ordering_theorem, "Well Ordering Theorem").
narrative_ontology:topic_domain(well_ordering_theorem, "mathematical_logic/set_theory").

domain_priors:emerges_naturally(well_ordering_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRUCTIVIST MATHEMATICIAN (MOUNTAIN) — Cannot exit the constraint that every set admits a well-ordering. The claim follows necessarily from ZFC axioms. Even those who reject classical logic cannot construct an alternative foundational system that denies well-ordering without abandoning the entire axiomatic structure. The constraint is immutable regardless of position or preference.
constraint_indexing:constraint_classification(well_ordering_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: WORKING MATHEMATICIAN (MOUNTAIN) — Faces high cost to work outside classical set theory but can avoid engaging with well-ordering in most applied mathematics. The constraint is real but not immediately felt in most subdisciplines. Yet the constraint remains immutable — no mathematical operation changes the fact that ZFC entails well-ordering. Constraints remain mountains even when agents can engineer high-cost avoidance.
constraint_indexing:constraint_classification(well_ordering_theorem, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational, global perspective, the well-ordering theorem emerges necessarily from the Axiom of Choice plus ZFC axioms. No agent, no measurement basis, no observational context changes this logical entailment. The constraint is invariant across all structural positions. This is the definition of a mountain: base extraction ≤ 0.25, suppression ≤ 0.05, emerges naturally, accessibility collapse ≥ 0.85, resistance ≤ 0.15.
constraint_indexing:constraint_classification(well_ordering_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: MATHEMATICS DISCIPLINE (MOUNTAIN) — Mathematics as an institutional practice is globally committed to ZFC as a default foundational system. No institutional agent can escape the well-ordering theorem without abandoning the shared language of set theory. Even those mathematicians who work in alternative foundations (constructivism, category theory, univalent foundations) cannot deny that well-ordering follows in ZFC contexts. The constraint is universal — it holds regardless of institutional position or temporal horizon.
constraint_indexing:constraint_classification(well_ordering_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(well_ordering_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(well_ordering_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(well_ordering_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(well_ordering_theorem, ExtMetricName, E),
    domain_priors:suppression_score(well_ordering_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(well_ordering_theorem),
    narrative_ontology:constraint_metric(well_ordering_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(well_ordering_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(well_ordering_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The well-ordering theorem does not extract resources from any agent. It is a logical fact. No one gains priority, wealth, or power from the constraint — it is not a constraint in the sense of enforcing asymmetry. It is listed here as a constraint only in the formal sense that it bounds what is possible in ZFC. Suppression (0.02): Negligible. The theorem does not suppress alternatives through coercion or control. Alternative foundational systems (constructivism, univalent type theory) exist and are practiced. Agents can exit classical logic entirely if they choose the cost. Suppression is low because the exit option is explicit, not hidden. Theater ratio (0.15): Low. The well-ordering theorem is proven, not performed. The proof is direct and accessible. There is minimal performative content — no ritual, no institutional maintenance beyond citing the axiom and the logical derivation. Accessibility collapse (0.92): Very high. The constraint is logically accessible — the Axiom of Choice is transparent and the derivation is clear. Accessibility collapse measures how much the constraint resists analysis; well-ordering is fully analyzable. Resistance (0.08): Very low. The mechanism is known completely: the Axiom of Choice → Zorn's Lemma → every set is well-orderable. No hidden mechanisms, no opaque causal chains. This is why the constraint is a mountain — no agent could hide or obscure it further.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All perspectives yield mountain classification because the constraint is a logical entailment. The constructivist mathematicians who reject classical logic still acknowledge that well-ordering follows from ZFC — they simply choose to work outside ZFC. The working mathematician who ignores well-ordering does not classify it as rope or snare — they recognize it as mountain and simply engineer high-cost avoidance. The analytical observer and the disciplinary perspective both see the same logical fact. This uniformity is the signature of a true mountain: no structural relationship, no power asymmetry, no exit option gradient produces perspectival variation. The constraint is invariant across all observation positions because logical entailment is position-invariant.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality applies. Directionality (d) measures how much an agent benefits vs bears costs from a constraint. Well-ordering has no beneficiaries and no victims in the structural sense. The constraint does not distribute costs asymmetrically. No agent extracts from another via the well-ordering theorem. Therefore, d is undefined — the constraint is not subject to directionality computation. This is a defining feature of mountains: they have no extractive structure, no asymmetric beneficiary/victim relationship, and thus no directionality value. All perspectives produce chi = 0 effective extraction, not because f(d) is zero, but because the constraint has no extraction mechanism to begin with.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy trivially: it is a mountain from all perspectives, period. There is no risk of mislabeling it as coordination or extraction, because it is neither. It is a logical fact. The mandatrophy in the well-ordering theorem would only arise if someone attempted to interpret the non-constructivity of the proof (the fact that well-orderings exist but cannot always be constructed) as a form of hidden extraction or coordination cost. That interpretation would be a category error — non-constructivity is a feature of classical logic, not an extraction mechanism. However, omega variable (non_constructive_benign_classification) flags the edge case: agents who work in constructive mathematics may experience non-constructivity as a constraint on what they can prove, which could reframe the well-ordering theorem as having a benign extractive cost (the cost of accepting existence claims without algorithms). This reframing would not change the mountain classification but would add nuance to how agents experience the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    choice_axiom_necessity,
    'Is the Axiom of Choice a logical necessity for mathematics or a contingent foundational choice?',
    'Comparative analysis of foundational systems: ZFC vs ZF (without Choice), constructive mathematics (IZF), univalent type theory. Establish whether any alternative foundation achieves equivalent expressive power without well-ordering as a logical consequence.',
    'If Choice is necessary: well-ordering is mountain from all perspectives. If Choice is contingent: well-ordering becomes rope or snare depending on whether agents can choose alternative foundations without losing coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(choice_axiom_necessity, conceptual, 'Whether Axiom of Choice is logically necessary or contingent foundational assumption').

omega_variable(
    countable_vs_uncountable_extraction,
    'Does the well-ordering theorem enforce qualitatively different extraction costs for countable sets vs uncountable sets?',
    'Proof-theoretic complexity analysis: examine whether well-ordering countable sets is constructively realizable (low extraction) while well-ordering uncountable sets requires full classical logic (high extraction). Test whether agent exit options differ between countable and uncountable contexts.',
    'If extraction differs by cardinality: the constraint decomposes into two separate stories with different ε values. If extraction is uniform: the mountain classification is stable across all mathematical domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countable_vs_uncountable_extraction, empirical, 'Whether extraction cost differs between countable and uncountable well-orderings').

omega_variable(
    non_constructive_benign_classification,
    'Is the well-ordering theorem''s non-constructivity a feature that enables mathematical breadth, or a limitation that constrains what mathematicians can assert?',
    'Historical analysis of mathematical results dependent on well-ordering: count results whose proofs are pure existence claims (non-constructive enabled) vs results that fail constructively but hold classically. Assess whether mathematicians experience non-constructivity as liberation or constraint.',
    'If experienced as liberation: the mountain is benign — agents benefit from the constraint''s logical guarantees. If experienced as constraint: the mountain carries hidden extraction — agents cannot prove effective algorithms for well-orderings, creating a class of ''true but not computable'' results.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_constructive_benign_classification, preference, 'Whether non-constructivity of well-ordering is experienced as enabling or constraining').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(well_ordering_theorem, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(well_tr_t0, well_ordering_theorem, theater_ratio, 0, 0.15).
narrative_ontology:measurement(well_tr_t50, well_ordering_theorem, theater_ratio, 50, 0.15).
narrative_ontology:measurement(well_tr_t100, well_ordering_theorem, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(well_be_t0, well_ordering_theorem, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(well_be_t50, well_ordering_theorem, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(well_be_t100, well_ordering_theorem, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(well_ordering_theorem, information_standard).

% DUAL FORMULATION NOTE:
% The Well Ordering Theorem is a foundational constraint but does not decompose into sub-constraints with different epsilon values. Alternative foundational systems (constructivism, category theory, univalent type theory) may create sibling constraints that explore what happens when well-ordering is not available, but those would be separate stories in a constraint family, not sub-components of this story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
