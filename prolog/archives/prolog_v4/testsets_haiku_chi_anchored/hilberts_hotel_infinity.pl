% ============================================================================
% CONSTRAINT STORY: hilberts_hotel_infinity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hilberts_hotel_infinity, []).

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
 *   constraint_id: hilberts_hotel_infinity
 *   human_readable: Hilbert's Paradox of the Grand Hotel
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   Hilbert's Paradox of the Grand Hotel is a mathematical thought experiment
 *   that illustrates the counter-intuitive properties of infinite sets. It
 *   demonstrates that a hotel with infinitely many rooms, all occupied, can
 *   still accommodate new guests by moving existing guests appropriately.
 *   This appears paradoxical only to finite-set intuition; from the
 *   perspective of formal set theory, it is an immutable consequence of the
 *   definition of cardinality for infinite sets. The constraint is the gap
 *   between human intuition and mathematical reality: infinity behaves
 *   fundamentally differently from finite quantities, and this difference is
 *   not negotiable, contextual, or institution-dependent. It is a natural law
 *   of mathematics.
 *
 * KEY AGENTS:
 *   - Mathematical Logic: The structural foundation (analytical/analytical) — ZFC axioms define the space in which the paradox exists
 *   - Human Intuition: The cognitive system (powerless/trapped) — evolved to handle finite sets, cannot escape the false predictions that infinite sets generate
 *   - Formal Set Theory: The descriptive framework (powerful/arbitrage) — provides the tools to understand why intuition fails
 *   - Pedagogical Process: The communication mechanism (organized/constrained) — must bridge the gap between intuition and mathematics without resolving the paradox itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hilberts_hotel_infinity, 0.08).
domain_priors:suppression_score(hilberts_hotel_infinity, 0.02).
domain_priors:theater_ratio(hilberts_hotel_infinity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hilberts_hotel_infinity, extractiveness, 0.08).
narrative_ontology:constraint_metric(hilberts_hotel_infinity, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(hilberts_hotel_infinity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hilberts_hotel_infinity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hilberts_hotel_infinity, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hilberts_hotel_infinity, mountain).
narrative_ontology:human_readable(hilberts_hotel_infinity, "Hilbert's Paradox of the Grand Hotel").
narrative_ontology:topic_domain(hilberts_hotel_infinity, "mathematical/logical").

domain_priors:emerges_naturally(hilberts_hotel_infinity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL LOGICIAN (MOUNTAIN) — From the formal mathematical standpoint, Hilbert's Hotel is an immutable consequence of set theory and the Dedekind-infinite definition. The existence of a bijection between infinite set and its proper subset is not a contingent fact about hotel management; it is a logical necessity that emerges from the axioms of ZFC set theory. ε≤0.08, suppression≤0.02, accessibility_collapse=0.92, resistance=0.08. No extraction, no coercion, only mathematical structure.
constraint_indexing:constraint_classification(hilberts_hotel_infinity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: WORKING MATHEMATICIAN (MOUNTAIN) — Practitioners in functional analysis, topology, and cardinality theory encounter the paradox as an unchangeable structural feature of their domain. The counter-intuitive property (a proper infinite subset has the same cardinality as the whole) is not an obstacle to be overcome or negotiated; it is the foundation upon which their work rests. The constraint is immutable from their perspective because rejecting it would require abandoning the entire axiomatic framework. ε≤0.08, suppression≤0.02.
constraint_indexing:constraint_classification(hilberts_hotel_infinity, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: PEDAGOGICAL OBSERVER (MOUNTAIN) — Teaching the paradox reveals an immutable cognitive barrier: human intuition developed for finite sets generates false predictions about infinite sets. The paradox cannot be 'resolved' pedagogically; it can only be understood by internalizing that infinity has fundamentally different properties. The constraint is the gap between finite-set intuition and actual mathematical structure — it emerges naturally from the architecture of human cognition encountering mathematical truth, not from contingent institutional choices.
constraint_indexing:constraint_classification(hilberts_hotel_infinity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hilberts_hotel_infinity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hilberts_hotel_infinity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hilberts_hotel_infinity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hilberts_hotel_infinity, ExtMetricName, E),
    domain_priors:suppression_score(hilberts_hotel_infinity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hilberts_hotel_infinity),
    narrative_ontology:constraint_metric(hilberts_hotel_infinity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hilberts_hotel_infinity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hilberts_hotel_infinity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Infinitesimal. The paradox extracts nothing from any agent. No resource flows. The hotel manager's apparent problem (accommodating infinitely many new guests) has a solution that redistributes rooms without cost — no room is lost, no guest is harmed. Suppression (0.02): Negligible. There are no coercive barriers to understanding the paradox — only cognitive difficulty. The mathematical machinery is fully transparent. Theater ratio (0.15): Very low. The explanation is direct and functional. Once cardinality is defined, the paradox has no performative content — it is pure mathematical consequence. Accessibility collapse (0.92): Very high. The paradox emerges inevitably from the axioms of set theory. There is no alternative mathematical framework that avoids it while preserving consistency (in the cases tested). Resistance (0.08): Very low. The paradox cannot be resisted or negotiated. All consistent formalizations produce the same outcome.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perfect perspectival invariance — all observers classify it as Mountain. The mathematical logician, the working mathematician, and the pedagogical observer all recognize the same immutable structure. There is no extraction, no beneficiary-victim relationship, no institutional coercion. The only 'gap' is between human finite-set intuition and mathematical reality, which is precisely why the paradox is classified as a Mountain from all perspectives. The impossibility of disagreement IS the evidence of its natural law status.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality analysis is needed for this constraint. It is a natural law with no beneficiaries or victims, no extraction, no suppression. All agents (if the term applies to mathematical entities) experience the same immutable structure. The constraint does not manipulate any agent's opportunity set; it only describes a property of infinite sets that emerges from the foundations of mathematics itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    axiom_system_dependence,
    'Is Hilbert''s Hotel a property of mathematical reality or a consequence of the ZFC axiom choice?',
    'Examination of alternative set theories (constructive mathematics, intuitionistic logic, category-theoretic foundations) and whether they escape or reproduce the paradox',
    'If it emerges in all consistent foundations: true mountain. If dependent on ZFC choice: contingent constraint disguised as mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_system_dependence, conceptual, 'Whether the paradox is axiom-independent or ZFC-dependent').

omega_variable(
    physical_realizability_gap,
    'Does the mathematical existence of infinite bijections imply physical realizability of infinite hotels?',
    'Analysis of the metaphorical mapping: is the thought experiment''s validity as metaphor dependent on physical instantiation, or is it purely a mathematical phenomenon?',
    'If physical realizability required: the constraint is about reality, not mathematics. If purely mathematical: the paradox is a mountain of logic, not of physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_realizability_gap, conceptual, 'Whether physical instantiation is necessary for the paradox''s validity').

omega_variable(
    intuition_vs_proof,
    'Is the ''paradoxicality'' a feature of the mathematical reality or purely a property of human cognitive mismatch?',
    'Analysis of whether the apparent paradox dissolves when the definition of cardinality is understood from first principles; historical examination of whether mathematicians who internalized infinite set theory find it paradoxical',
    'If dissolution is possible: the constraint is cognitive, not mathematical. If it persists: the mountain includes the necessary human limitation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intuition_vs_proof, conceptual, 'Whether the paradox is mathematical or cognitive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hilberts_hotel_infinity, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hilbert_tr_t0, hilberts_hotel_infinity, theater_ratio, 0, 0.12).

% Extraction over time
narrative_ontology:measurement(hilbert_be_t0, hilberts_hotel_infinity, base_extractiveness, 0, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hilberts_hotel_infinity, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
