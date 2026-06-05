% ============================================================================
% CONSTRAINT STORY: banach_tarski_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_banach_tarski_paradox, []).

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
 *   constraint_id: banach_tarski_paradox
 *   human_readable: Banach-Tarski Paradox
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   The Banach-Tarski Paradox is a mathematical theorem, proven in 1924,
 *   stating that a solid ball in 3-dimensional Euclidean space can be
 *   decomposed into finitely many disjoint non-measurable subsets that can be
 *   reassembled to form two identical copies of the original ball. The
 *   constraint is not the paradox itself (which is correct mathematics) but
 *   the fundamental limit it imposes on intuitions about decomposition,
 *   volume conservation, and the role of the Axiom of Choice in set theory.
 *   This is a genuine mountain constraint: it emerges naturally from the
 *   axioms of ZFC, exhibits zero degrees of freedom for all observers, and
 *   marks an immutable boundary between what classical continuous geometry
 *   can guarantee (measurability, finite additivity) and what formal logic
 *   permits (non-measurable pathologies). No institutional arrangement, no
 *   social convention, no measurement methodology can negotiate with this
 *   constraint. It is invariant across all perspectives — all observers agree
 *   the theorem holds in ZFC; they may disagree about AC's status, but they
 *   do not dispute the constraint itself.
 *
 * KEY AGENTS:
 *   - Physical Intuition: Powerless agent (civilizational scope) — bears the cost of violated expectations about continuous decomposition and reassembly; cannot exit the constraint that classical intuition fails
 *   - Set-Theoretic Observer: Analytical agent (civilizational scope) — acknowledges the constraint as a logical ceiling on axiomatized systems; has complete intellectual agency but no escape from the theorem's implications within ZFC
 *   - Applied Mathematician: Organized agent (biographical scope) — recognizes the constraint as a boundary marker: practical decomposition stays on the measurable side, the paradox marks where formalism diverges from constructive work
 *   - Mathematical Community: Institutional actor (generational scope) — collectively enforces the constraint through peer review and canonical status; no negotiation possible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(banach_tarski_paradox, 0.08).
domain_priors:suppression_score(banach_tarski_paradox, 0.02).
domain_priors:theater_ratio(banach_tarski_paradox, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(banach_tarski_paradox, extractiveness, 0.08).
narrative_ontology:constraint_metric(banach_tarski_paradox, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(banach_tarski_paradox, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(banach_tarski_paradox, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(banach_tarski_paradox, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(banach_tarski_paradox, mountain).
narrative_ontology:human_readable(banach_tarski_paradox, "Banach-Tarski Paradox").
narrative_ontology:topic_domain(banach_tarski_paradox, "mathematical/logical").

domain_priors:emerges_naturally(banach_tarski_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PHYSICAL INTUITION (MOUNTAIN) — Classical geometric intuition about volume conservation and continuity. Cannot escape or negotiate with the logical limit that non-measurable sets violate finite additivity. The paradox is an immutable ceiling on physical reasoning about discrete decomposition and reassembly.
constraint_indexing:constraint_classification(banach_tarski_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: SET-THEORETIC OBSERVER (MOUNTAIN) — From formal set theory (ZFC), the Banach-Tarski decomposition is a rigorous theorem. The constraint is the dependence on the Axiom of Choice: without AC, the construction fails. With AC, it holds universally. This is a logical bedrock — not a social convention or institutional arrangement, but an irreducible feature of axiomatic mathematics.
constraint_indexing:constraint_classification(banach_tarski_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: APPLIED MATHEMATICIAN (MOUNTAIN) — For practical measurement and engineering, non-measurable sets have zero probability of appearing in constructive work. The paradox is a boundary condition: it marks where real-world decomposition (continuous, measurable) ends and formal-logic pathology begins. Unavoidable because it names the exact limit of classical intuition.
constraint_indexing:constraint_classification(banach_tarski_paradox, mountain,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: MATHEMATICAL COMMUNITY (MOUNTAIN) — Banach-Tarski is a proven theorem; its status is fixed in the mathematical canon. No institutional negotiation, no social convention, no career incentive changes its truth value. Mathematicians collectively accept it as a logical constraint on all theories built on ZFC.
constraint_indexing:constraint_classification(banach_tarski_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(banach_tarski_paradox_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(banach_tarski_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(banach_tarski_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(banach_tarski_paradox, ExtMetricName, E),
    domain_priors:suppression_score(banach_tarski_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(banach_tarski_paradox),
    narrative_ontology:constraint_metric(banach_tarski_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(banach_tarski_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(banach_tarski_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The Banach-Tarski theorem does not extract resources, regulate behavior, or concentrate power. It is a pure logical statement — a ceiling on what can be guaranteed about finite additivity in formal set theory. The small non-zero value (0.08) rather than zero reflects that the theorem imposes a cognitive/epistemic cost: agents must abandon classical geometric intuition and accept that measurability is not automatic. This is an accessibility cost, not an extraction cost. Suppression (0.02): Negligible. The constraint is not coercive in any sense. No agent is forced into silence or compliance. Mathematicians openly discuss the theorem, its assumptions, and its implications. The minimal value reflects only that the theorem is intellectually demanding — it requires learning formal set theory to fully understand. Theater ratio (0.15): Very low. The Banach-Tarski constraint has minimal performative content. The proof is published, peer-reviewed, and understood. There is no rhetorical theater, no institutional ritual, no proxy metrics. The small value reflects only the pedagogical gap — explaining the proof requires extended mathematical training, which creates a minor accessibility barrier.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify the constraint as Mountain. There is no perspectival gap because the constraint is invariant across all structural positions. A powerless agent has no escape; an institutional agent cannot negotiate it away; an analytical observer sees the same logical bedrock; an applied mathematician recognizes the same boundary condition. The uniformity of classification is itself the diagnostic signature of a true mountain. The only potential disagreement is about whether the Axiom of Choice itself is contingent (omega_1), which would affect the interpretation of the constraint's scope, but not its classification within ZFC-based mathematics.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality derivation is required for this constraint because it is a natural law with no beneficiaries or victims. The Banach-Tarski Paradox does not extract from anyone or benefit anyone. It is not an institutional arrangement or a coordination mechanism. It is an immutable logical consequence of axiomatized set theory. The constraint applies with identical force (d = 0.72 for all observers in the analytical context) because all perspectives converge on the same classification type. The sigmoid f(d) maps 0.72 to approximately 1.15, but this is multiplicative noise on an effectively zero base extractiveness — the constraint's 'power' comes entirely from its logical inevitability, not from any agent's structural advantage.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    axiom_of_choice_necessity,
    'Is the Axiom of Choice a fundamental logical truth or a contingent assumption that could be abandoned?',
    'Development of large cardinal axioms, forcing arguments, or alternative set theories (ZF without AC, constructivism); historical analysis of AC''s justification across different mathematical foundations',
    'If AC is contingent: the Banach-Tarski constraint applies only in AC-based systems, reducing it from universal mountain to a framework-specific limit. If AC is fundamental: the constraint is truly universal and invariant across all mathematical contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_of_choice_necessity, conceptual, 'Whether Axiom of Choice is fundamental or contingent').

omega_variable(
    physical_realizability_boundary,
    'Does the non-constructive nature of the Banach-Tarski decomposition imply it is in principle unrealizable in physical space, or does it merely describe a mathematical artifact with no physical content?',
    'Analysis of what ''physical decomposition'' means; investigation of whether continuous physical space permits non-measurable partitions; examination of quantum measurement and discreteness of physical substrate',
    'If physically unrealizable by nature: the paradox is a pure formalism with no physical constraint — classification should degrade toward Piton. If physically impossible but not for simple reasons: classification remains Mountain but the constraint is narrower than apparent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_realizability_boundary, empirical, 'Whether Banach-Tarski is physically impossible or merely non-constructive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(banach_tarski_paradox, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btp_tr_t0, banach_tarski_paradox, theater_ratio, 0, 0.1).
narrative_ontology:measurement(btp_tr_t50, banach_tarski_paradox, theater_ratio, 50, 0.15).
narrative_ontology:measurement(btp_tr_t100, banach_tarski_paradox, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(btp_be_t0, banach_tarski_paradox, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(btp_be_t50, banach_tarski_paradox, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(btp_be_t100, banach_tarski_paradox, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(banach_tarski_paradox, information_standard).
narrative_ontology:affects_constraint(banach_tarski_paradox, axiom_of_choice_consequence).
narrative_ontology:affects_constraint(banach_tarski_paradox, non_measurable_set_existence).
narrative_ontology:affects_constraint(banach_tarski_paradox, finite_additivity_limit).

% DUAL FORMULATION NOTE:
% The Banach-Tarski Paradox is upstream of several derived constraints in set-theoretic foundations. It demonstrates the necessity of the Axiom of Choice (constraint_axiom_of_choice_consequence) and establishes the existence of non-measurable sets (constraint_non_measurable_set_existence). It also marks the boundary of finite additivity (constraint_finite_additivity_limit). All three downstream constraints would classify differently (as Tangled Rope or Snare) if AC were abandoned, making them dependent on the mountain classification of Banach-Tarski within the ZFC framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
