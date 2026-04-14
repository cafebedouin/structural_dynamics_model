% ============================================================================
% CONSTRAINT STORY: russells_paradox_self_reference
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_russells_paradox_self_reference, []).

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
 *   constraint_id: russells_paradox_self_reference
 *   human_readable: Russell's Paradox (Naive Set Theory Collapse)
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   Russell's Paradox is a logical impossibility that arises from naive set
 *   comprehension: if R = {x | x ∉ x} is a valid set, then R ∈ R if and only
 *   if R ∉ R, a contradiction. This constraint operates at the level of
 *   logical space itself, not at the level of social policy or institutional
 *   arrangement. It is a theorem in metamathematics — a proof that certain
 *   axiom systems are inconsistent. No agent, no institution, no coalition of
 *   mathematicians can evade or negotiate around the paradox. It is a
 *   necessity, not a choice. The constraint classifies as Mountain from every
 *   perspective because it represents an irreducible logical limit: any
 *   formal system must restrict set formation to avoid the contradiction, and
 *   this restriction is not optional.
 *
 * KEY AGENTS:
 *   - Naive set theorists: Experience the constraint as an absolute logical barrier — reasoning within unrestricted comprehension leads to immediate contradiction
 *   - Formalist mathematicians: Organized response to the constraint — select axioms (ZFC, NBG) that exclude naive comprehension; the constraint persists as the requirement that these restrictions exist
 *   - Mathematical institution: Collective beneficiary of the constraint's resolution — unified axiom systems provide consistency and enable proof theory
 *   - Analytical observer: Sees the constraint as a theorem about logical space itself — a proof that certain structures are impossible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(russells_paradox_self_reference, 0.08).
domain_priors:suppression_score(russells_paradox_self_reference, 0.02).
domain_priors:theater_ratio(russells_paradox_self_reference, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(russells_paradox_self_reference, extractiveness, 0.08).
narrative_ontology:constraint_metric(russells_paradox_self_reference, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(russells_paradox_self_reference, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(russells_paradox_self_reference, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(russells_paradox_self_reference, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(russells_paradox_self_reference, mountain).
narrative_ontology:human_readable(russells_paradox_self_reference, "Russell's Paradox (Naive Set Theory Collapse)").
narrative_ontology:topic_domain(russells_paradox_self_reference, "mathematical/logical").

domain_priors:emerges_naturally(russells_paradox_self_reference).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE NAIVE SET THEORIST (MOUNTAIN) — Any agent reasoning within naive set theory axioms faces an absolute logical barrier. The contradiction 'R ∈ R ↔ R ∉ R' is inescapable once the set R = {x | x ∉ x} is constructed. No exit exists; no appeal, no modification, no workaround within the framework. The constraint is experienced as a hard logical limit.
constraint_indexing:constraint_classification(russells_paradox_self_reference, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE FORMALIST MATHEMATICIAN (MOUNTAIN) — Even organized mathematicians with sophisticated axiom systems (ZFC, NBG) cannot evade the constraint: they must select axioms that exclude naive set comprehension. The constraint persists as an iron requirement: any formal system attempting to avoid Russell's Paradox must restrict set formation. The restriction is not optional; it is a necessary condition for consistency. This is still a mountain — the constraint is that some axioms must be rejected, and no axiom system can include naive comprehension without contradiction.
constraint_indexing:constraint_classification(russells_paradox_self_reference, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER / LOGICAL SPACE (MOUNTAIN) — From a civilizational and universal scope, Russell's Paradox is a theorem about the structure of logical space itself. It proves that naive comprehension is not an axiom any consistent system can adopt. The constraint is the impossibility result, not a social arrangement. Zero degrees of freedom: any system must be designed to avoid the paradox or accept contradiction. This is classification as natural law.
constraint_indexing:constraint_classification(russells_paradox_self_reference, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE MATHEMATICAL INSTITUTION (MOUNTAIN) — Even the collective institution of mathematics cannot negotiate around Russell's Paradox. It cannot lobby for an exception, cannot claim special status, cannot argue that naive comprehension is useful enough to override consistency. The paradox is a mathematical constraint, not a policy constraint. Institutions benefit from the restriction (clear axioms, consistency proofs) but experience the constraint as an inescapable boundary condition, not as a choice.
constraint_indexing:constraint_classification(russells_paradox_self_reference, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(russells_paradox_self_reference_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(russells_paradox_self_reference, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(russells_paradox_self_reference, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(russells_paradox_self_reference, ExtMetricName, E),
    domain_priors:suppression_score(russells_paradox_self_reference, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(russells_paradox_self_reference),
    narrative_ontology:constraint_metric(russells_paradox_self_reference, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(russells_paradox_self_reference, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(russells_paradox_self_reference_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. Russell's Paradox extracts nothing from any agent — it is not a rent-seeking mechanism or a mechanism for asymmetric advantage. It is a logical ceiling, not a drain. The small non-zero value reflects that the constraint does impose costs on formal systems (the cost of restricting comprehension axioms), but this cost is structural, not extractive. Suppression (0.02): Negligible. There are no suppressed alternatives because the alternative — permitting the contradiction — is not actually viable. Suppression measures coercion, and there is no coercion here, only logical necessity. Theater ratio (0.0): Zero. Russell's Paradox is not performative. It is not a ritual, a display, or a theatrical arrangement. It is a pure logical constraint with zero functional slack. The paradox is what it claims to be — a proof of inconsistency — with no hidden function or pretense.
 *
 * PERSPECTIVAL GAP:
 *   Paradoxically, Russell's Paradox exhibits zero perspectival gap — all four perspectives classify identically as Mountain. This is the defining signature of a true natural law constraint: it is observable-independent and perspective-invariant. A naive set theorist, a formalist mathematician, an organized institution, and an analytical observer all see the same constraint: an impossibility result that applies universally. There is no disagreement on classification because there is no room for disagreement. The paradox is a theorem, not a policy, and theorems are not negotiable.
 *
 * DIRECTIONALITY LOGIC:
 *   Mountain constraints have zero directionality differentiation because they bind all agents equally. The self-reference constraint applies universally — there is no beneficiary and no victim, only agents constrained by logical necessity. The paradox does not flow from one agent to another; it is a fixed point in logical space. Directionality derivation is bypassed for mountains; the constraint classifies identically regardless of agent power, exit options, or time horizon.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN EXEMPLAR: Russell's Paradox demonstrates the mandatrophy resolution for Mountain constraints. The constraint cannot be mislabeled as Rope (there is no coordination function), Snare (there is no extraction or suppression), Scaffold (there is no sunset), or Piton (there is no theater). The classification is rock-solid because the underlying metric signature (extractiveness ≤ 0.08, suppression ≤ 0.02, theater_ratio = 0.0) uniquely determines Mountain from all perspectives. The constraint is a mathematical theorem, not a social structure, and theorems are invariant across all indexical contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    type_theory_vs_zfc_equivalence,
    'Are type-theoretic and ZFC-based resolutions of Russell''s Paradox merely notational variants of the same logical constraint, or do they represent genuinely distinct logical structures with different metaphysical implications?',
    'Proof-theoretic comparison: demonstrate whether every model of type theory has a ZFC model and vice versa; compare consistency proofs across frameworks',
    'If equivalent: Russell''s Paradox is a single logical constraint with multiple formalisations (mountain classification robust). If distinct: the paradox may decompose into multiple constraints depending on which logical framework is adopted (mountain becomes context-dependent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(type_theory_vs_zfc_equivalence, conceptual, 'Whether different formal resolutions are notational or structurally distinct').

omega_variable(
    self_reference_necessity,
    'Is the self-referential structure (the set asking whether it contains itself) an essential feature of the logical constraint, or merely an artifact of the naive comprehension axiom?',
    'Proof that self-reference emerges unavoidably from any unrestricted comprehension scheme; analysis of whether restricting self-reference directly (rather than comprehension) avoids the paradox',
    'If self-reference is essential: Russell''s Paradox captures a deep truth about reflexivity in logical systems (stronger mountain classification). If it is an artifact: the constraint is more narrowly about comprehension axioms than about self-reference (mountain becomes narrower in scope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_reference_necessity, conceptual, 'Whether self-reference is essential to the paradox or incidental').

omega_variable(
    paraconsistent_logic_escape,
    'Do paraconsistent logics that permit true contradictions genuinely escape Russell''s Paradox, or do they merely relocate the constraint to a different logical property?',
    'Formal analysis of whether paraconsistent set theories (e.g., GLB, NF with rejection rules) truly satisfy naive comprehension without contradiction or achieve contradiction-tolerance through semantic reinterpretation',
    'If paraconsistent escape is genuine: Russell''s Paradox is specific to classical logic (mountain narrows to classical systems). If it relocates the constraint: the mountain is broader than classical logic (applies to all consistent formal systems).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(paraconsistent_logic_escape, empirical, 'Whether paraconsistent logics genuinely avoid or merely tolerate Russell''s Paradox').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(russells_paradox_self_reference, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(russells_paradox_self_reference, godel_incompleteness_first_theorem).
narrative_ontology:affects_constraint(russells_paradox_self_reference, halting_problem_undecidability).
narrative_ontology:affects_constraint(russells_paradox_self_reference, cantor_diagonal_argument).

% DUAL FORMULATION NOTE:
% Russell's Paradox is a foundational constraint that influences other impossibility results in mathematical logic. The paradox itself does not decompose into multiple constraints — it is a single logical threshold. However, its resolutions (type theory, ZFC restrictions, paraconsistent logics) may be understood as separate constraint stories if they are treated as alternative formal frameworks rather than as solutions to Russell's Paradox. For this story, the constraint is the paradox itself: the irreducibility of naive comprehension and the necessity of axiom restrictions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
