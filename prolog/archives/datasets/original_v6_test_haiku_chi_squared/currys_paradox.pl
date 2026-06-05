% ============================================================================
% CONSTRAINT STORY: currys_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_currys_paradox, []).

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
 *   constraint_id: currys_paradox
 *   human_readable: Curry's Paradox
 *   domain: analytical/logic
 *
 * SUMMARY:
 *   Curry's Paradox is a logical result demonstrating that from a
 *   self-referential sentence of the form 'If this sentence is true, then X
 *   is true,' any arbitrary conclusion X can be derived, even absurd ones
 *   (e.g., 'the moon is made of cheese'). The paradox does not require
 *   negation like the Liar Paradox ('this sentence is false'), making it a
 *   more fundamental constraint on the design of formal systems. Curry's
 *   Paradox proves that any logic permitting unrestricted self-reference and
 *   a truth predicate will either (a) permit arbitrary derivations, (b)
 *   declare the sentence unprovable, or (c) restrict its axioms. This is not
 *   a contingent institutional problem or a policy choice — it is a
 *   structural necessity emerging from the interaction of self-reference and
 *   truth-functional reasoning. The paradox classifies as a Mountain from all
 *   perspectives because it reveals an immutable boundary of the logical
 *   design space: formal systems cannot simultaneously maintain classical
 *   logic, unrestricted self-reference, and unrestricted truth predicates.
 *   Any two of these can coexist; all three cannot.
 *
 * KEY AGENTS:
 *   - Naive Formal System: The logical structure that permits unrestricted self-reference and unrestricted truth predicates — cannot escape the paradox without architectural restriction
 *   - Formal Logician: Analyst seeking consistent formal systems — constrained by the necessity of choosing among classical logic, self-reference, or truth predicates
 *   - Type Theorist: Designer of stratified logics — uses the paradox as a design requirement driving type hierarchies
 *   - Paraconsistent Logician: Theorist working in non-classical logics — attempts to accommodate true contradictions rather than forbid them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(currys_paradox, 0.12).
domain_priors:suppression_score(currys_paradox, 0.03).
domain_priors:theater_ratio(currys_paradox, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(currys_paradox, extractiveness, 0.12).
narrative_ontology:constraint_metric(currys_paradox, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(currys_paradox, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(currys_paradox, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(currys_paradox, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(currys_paradox, mountain).
narrative_ontology:human_readable(currys_paradox, "Curry's Paradox").
narrative_ontology:topic_domain(currys_paradox, "analytical/logic").

domain_priors:emerges_naturally(currys_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NAIVE FORMAL SYSTEM (MOUNTAIN) — Any system that permits unrestricted self-reference and truth predication cannot avoid Curry's sentence. The paradox is a logical necessity, not a policy choice or institutional arrangement. The system has no exit: it must either accept the paradox or restrict its axioms. d≈0.98, f(d)≈1.42, σ=1.0 → χ≈0.17.
constraint_indexing:constraint_classification(currys_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: LOGICAL ANALYST (MOUNTAIN) — From the perspective of mathematical logic, Curry's Paradox is an immutable structural constraint on the design space of formal systems. Any system rich enough to express self-reference and contain a truth predicate will generate this paradox. The paradox is not contingent on historical factors or institutional choices. d≈0.65, f(d)≈1.03, σ=1.0 → χ≈0.12.
constraint_indexing:constraint_classification(currys_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: PROOF THEORIST (MOUNTAIN) — Curry's Paradox as a metatheoretic result constrains what can be proven in any consistent formal system. It is the logical equivalent of physical law: the constraint cannot be negotiated, only accommodated through architectural choices (type restrictions, typed languages, stratified logics). d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(currys_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: TYPE THEORIST (MOUNTAIN) — From the perspective of type theory and category theory, Curry's Paradox reveals the necessity of type stratification. The paradox is not a defect but a design requirement: the existence of the paradox proves that hierarchical typing is logically necessary. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(currys_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(currys_paradox_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(currys_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(currys_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(currys_paradox, ExtMetricName, E),
    domain_priors:suppression_score(currys_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(currys_paradox),
    narrative_ontology:constraint_metric(currys_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(currys_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(currys_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. Curry's Paradox does not extract value from any agent in a social sense — it is not a constraint on human action or institutional behavior. The low value reflects that this is a pure logical structure, not a coordination or coercion mechanism. The small non-zero value accounts for the constraint's minimal 'cost' in design space: systems must allocate cognitive resources to reasoning about type restrictions or paraconsistency, but this is negligible compared to actual extraction constraints. Suppression (0.03): Minimal. There is no coercive mechanism suppressing alternatives. Any system designer can choose to accept classical logic + unrestricted truth predicates (and thus the paradox), or to restrict axioms to avoid it. The minimal suppression reflects only that the choice is not free once self-reference is introduced — the constraint becomes binding. Theater (0.15): Minimal. There is no performative content in logical structure. The paradox is functionally present or absent; there is no pretense or theater in its operation. Accessibility collapse (0.92): The paradox is accessible only to those trained in formal logic and set theory, but once understood, its necessity is absolute. Resistance (0.08): Extremely low. The paradox cannot be resisted or negotiated — it is a logical consequence of the axioms. Any attempt to work around it requires either accepting it or changing the axioms.
 *
 * PERSPECTIVAL GAP:
 *   Curry's Paradox exhibits minimal perspectival gap because the constraint is universal and structure-independent. All observers — naive systems, logicians, type theorists, paraconsistent logicians — agree on the same underlying fact: unrestricted self-reference + unrestricted truth predication = paradox. The gap is not in the classification (all Mountain) but in the remedy: different agents respond by restricting different components of the system (type theorists restrict self-reference via stratification; paraconsistent logicians restrict classical logic; proof theorists accept the paradox as a boundary condition). This uniformity across perspectives is the defining signature of a pure Mountain constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Because Curry's Paradox is a logical necessity with no social or institutional beneficiaries or victims, directionality is structurally minimal. No agent benefits from the paradox or bears costs from it — the paradox simply defines a boundary of the design space. All agents are constrained equally by the logical structure. d approaches the centroid 0.50 for all perspectives because the constraint is not asymmetric in its operation. The slight variations in d across perspectives reflect only the observer's epistemic position relative to the paradox (a logician familiar with the proof sees it more directly than a naive system that hasn't encountered it), not differences in extraction or benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paraconsistent_escape,
    'Do paraconsistent logics (which permit true contradictions) genuinely escape Curry''s Paradox or merely suppress its consequences?',
    'Formal analysis of paraconsistent systems: can they accommodate both the Curry sentence and its derivation without either explosion or restriction of the truth predicate?',
    'If genuine escape: Curry''s Paradox is contingent on classical logic assumptions (not a pure mountain). If suppression: the paradox persists structurally; paraconsistency is a pragmatic accommodation, not a resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(paraconsistent_escape, empirical, 'Whether paraconsistent logics genuinely resolve Curry''s Paradox or suppress it').

omega_variable(
    self_reference_necessity,
    'Is self-reference in formal systems an irreducible feature or an artifact of how we construct languages?',
    'Metamathematical analysis of formal systems without explicit self-reference primitives; investigation of whether self-reference emerges necessarily from other axioms (Gödel-style)',
    'If irreducible: Curry''s Paradox is a fundamental constraint (Mountain classification confirmed). If artifact: systems without self-reference primitives escape the paradox, suggesting it is not strictly universal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_reference_necessity, conceptual, 'Whether self-reference is necessary in formal systems or a contingent feature').

omega_variable(
    truth_predicate_alternative,
    'Can a formal system express arbitrary propositions while maintaining consistency without any explicit truth predicate?',
    'Construction of systems that achieve expressiveness equivalent to unrestricted truth predication through alternative means (e.g., implicit satisfaction relations, definability without quotation)',
    'If possible: the constraint is not universal (systems can achieve the same function differently). If impossible: truth predicates are logically necessary, confirming the mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(truth_predicate_alternative, empirical, 'Whether formal systems can avoid explicit truth predicates without sacrificing expressiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(currys_paradox, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(curry_tr_t0, currys_paradox, theater_ratio, 0, 0.15).
narrative_ontology:measurement(curry_tr_t5, currys_paradox, theater_ratio, 5, 0.15).
narrative_ontology:measurement(curry_tr_t10, currys_paradox, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(curry_be_t0, currys_paradox, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(curry_be_t5, currys_paradox, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(curry_be_t10, currys_paradox, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(currys_paradox, information_standard).
narrative_ontology:affects_constraint(currys_paradox, godels_incompleteness).
narrative_ontology:affects_constraint(currys_paradox, liar_paradox).
narrative_ontology:affects_constraint(currys_paradox, cantor_diagonal_argument).

% DUAL FORMULATION NOTE:
% Curry's Paradox is structurally upstream of Gödel's Incompleteness Theorem in the logical hierarchy: Gödel uses self-reference and diagonalization to prove that no consistent formal system can prove all truths, but Curry's Paradox proves that even deriving arbitrary conclusions from self-reference is possible without invoking incompleteness. The Liar Paradox is orthogonal — it uses negation; Curry's Paradox does not. The Cantor diagonal argument is a related but distinct use of self-referential construction in set theory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
