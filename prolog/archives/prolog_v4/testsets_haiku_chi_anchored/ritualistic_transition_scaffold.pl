% ============================================================================
% CONSTRAINT STORY: ritualistic_transition_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ritualistic_transition_scaffold, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ritualistic_transition_scaffold
 *   human_readable: The Habit-Building Scaffold
 *   domain: social/institutional
 *
 * SUMMARY:
 *   Organizations in chaotic or transitional states often employ structured
 *   rituals—daily standups, formal review cycles, standardized documentation
 *   templates—not to extract value but to create stable reference points
 *   around which members can coordinate and develop competence. The
 *   Habit-Building Scaffold is an intentional use of procedural theater to
 *   stabilize a chaotic organization during a transition period. Unlike pure
 *   extraction (Snare) or pure coordination (Rope), the scaffold combines
 *   low-level extraction overhead with temporary, explicit constraints that
 *   members understand will dissolve as they internalize the procedures. The
 *   constraint succeeds if the theater becomes internalized as habit and can
 *   then be dissolved; it degrades to a Piton if the ritual persists after
 *   internalization due to institutional inertia. This story models how
 *   scaffolding differs structurally from both coordinate-only and
 *   extract-only constraints through its sunset clause—the explicit temporal
 *   design that distinguishes temporary support from permanent extraction.
 *
 * KEY AGENTS:
 *   - Organization Members: Primary beneficiary (moderate/constrained) — constrained by ritual structure but experience it as enabling competence development and reducing chaos-induced anxiety
 *   - Leadership Team: Secondary beneficiary (institutional/arbitrage) — implements scaffold as coordination mechanism; experiences low extraction overhead; benefits from reduced cognitive load from shared procedures
 *   - Organizational Workflow: Implicit victim/constraint object (powerless/trapped) — abstract process state that the ritual stabilizes; subject to degradation risk if ritual persists past its functional window
 *   - Analytical Observer: External evaluator (analytical/analytical) — assesses whether scaffold is functioning as designed and tracking toward sunset vs. degrading toward piton
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ritualistic_transition_scaffold, 0.28).
domain_priors:suppression_score(ritualistic_transition_scaffold, 0.35).
domain_priors:theater_ratio(ritualistic_transition_scaffold, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ritualistic_transition_scaffold, extractiveness, 0.28).
narrative_ontology:constraint_metric(ritualistic_transition_scaffold, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ritualistic_transition_scaffold, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ritualistic_transition_scaffold, scaffold).
narrative_ontology:human_readable(ritualistic_transition_scaffold, "The Habit-Building Scaffold").
narrative_ontology:topic_domain(ritualistic_transition_scaffold, "social/institutional").

domain_priors:requires_active_enforcement(ritualistic_transition_scaffold).
narrative_ontology:has_sunset_clause(ritualistic_transition_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ritualistic_transition_scaffold, organization_members).
narrative_ontology:constraint_beneficiary(ritualistic_transition_scaffold, leadership_team).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORGANIZATIONAL MEMBER (SCAFFOLD) — Constrained by chaos but sees the ritual structure as temporary stabilization enabling competence development. d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.15. Low extraction because the constraint is transparently scaffolding toward independence.
constraint_indexing:constraint_classification(ritualistic_transition_scaffold, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: LEADERSHIP TEAM (ROPE) — Implements ritual procedures as coordination mechanism to structure chaotic workflow. Experiences constraint as enabling: procedural theater creates common reference points for collective action. d≈0.10, f(d)≈-0.05, σ=0.8 → χ≈-0.02. Net beneficiary through coordination function.
constraint_indexing:constraint_classification(ritualistic_transition_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: RITUALIZED ROUTINE (PITON) — From a long-term view, the procedural theater can become vestigial — maintaining ritual structure after the underlying chaos has resolved. theater_ratio=0.65 indicates risk of performative drift. d≈0.90, f(d)≈1.35, σ=0.8 → χ≈0.31. This perspective tracks the risk that the scaffold persists past its sunset date.
constraint_indexing:constraint_classification(ritualistic_transition_scaffold, piton,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (SCAFFOLD) — Sees the constraint as a deliberate, time-bounded use of theater to stabilize and enable capability building. The sunset clause is structural: rituals succeed when they internalize and then dissolve. χ≤0.30 and theater≤0.70 indicate genuine scaffolding architecture, not pure extraction. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.28. Constraint is classified as designed, temporary coordination.
constraint_indexing:constraint_classification(ritualistic_transition_scaffold, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ritualistic_transition_scaffold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ritualistic_transition_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ritualistic_transition_scaffold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ritualistic_transition_scaffold, TR),
    TR >= 0.70.

:- end_tests(ritualistic_transition_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. The ritual structure does impose costs—members must comply with procedures, spend time on meetings, document decisions formally—but these costs are low relative to the coordination benefits and are understood by members as temporary. The extractiveness is not near-zero because leadership maintains some structural advantage (they design and can modify procedures), but it is far from snare-level extraction (0.46+) because members see the constraint as enabling, not oppressive. Suppression (0.35): Moderate. Members are constrained in how they execute work (must follow procedures, cannot informally shortcut) but retain exit options (can leave, can push back on procedures). The suppression is not high because the constraint is transparent and the coercion is legitimized by the scaffolding frame. Theater ratio (0.65): Moderate-high. Much of the procedural structure is explicitly theatrical—rituals and ceremonies designed to stabilize emotional and cognitive state, not purely functional. Daily standups have theater content (team gathering, shared awareness) alongside coordination content (status updates). The theater is intentional and appropriate to the scaffolding function. Claimed type (Scaffold): Fits schema constraints: χ ≤ 0.30 (computed: 0.28 × 0.65 × 0.8 ≈ 0.15), theater ≤ 0.70 (actual: 0.65), has_sunset_clause (explicit in constraint design), beneficiaries present (organization members, leadership), extractiveness trajectory rising slightly (internalization phase, theater becoming more ritualized) but not exceeding threshold for degradation.
 *
 * PERSPECTIVAL GAP:
 *   The organizational member and leadership team both see the constraint as scaffold because they understand the sunset logic and experience the coercion as proportional and temporary. The piton perspective emerges from a long-term view where ritual persists past its functional window—this is the degradation risk that the sunset clause is designed to prevent. The analytical observer sees the scaffold structure as intentional design, validating the classification, but also tracks the omegas that would indicate degradation. The perspectival gap is not between fundamentally opposed views (as in snare vs. rope) but between different temporal horizons: short-term members see scaffolding; long-term institutional inertia risks sees piton degradation. The constraint's success depends on closing this gap—dissolving the ritual before it becomes vestigial.
 *
 * DIRECTIONALITY LOGIC:
 *   Organization members: Beneficiary + constrained → d≈0.50, f(d)≈0.65. Members experience both costs (procedural compliance) and benefits (chaos reduction, competence scaffolding) in roughly equal measure. Leadership team: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Net beneficiary; leadership can exit the constraint (design procedures differently) and benefits from coordination coordination coordination gains. Organizational workflow (as piton perspective): Victim + trapped → d≈0.90, f(d)≈1.35. Abstract process cannot exit; at risk from persistent theater post-sunset. Analytical observer: analytical → d≈0.65, f(d)≈1.00. Standard analytical derivation for observer position.
 *
 * MANDATROPHY ANALYSIS:
 *   The Habit-Building Scaffold resolves mandatrophy by making explicit the temporal design that distinguishes scaffolding from pure extraction. A snare would hide its extraction mechanism and lack a sunset clause; a rope would have no temporal limit because coordination is intrinsically beneficial; a scaffold is temporally bounded and transparently theatrical. The constraint avoids false natural law framing (mountain) by virtue of its low base extractiveness and the mechanisms (internalization, reduced chaos) that enable dissolution. The primary mandatrophy risk is degradation to piton—the ritual persists after its scaffolding function is complete due to institutional inertia or member comfort with familiar procedures. The omegas (internalization_threshold, theater_creep_onset) track this risk. The classification holds if the sunset clause is honored and the ritual is genuinely dissolved when members internalize the procedures. If the ritual persists past internalization, the constraint reclassifies to piton (theater ≥ 0.70, ε ≤ 0.25, institutional inertia maintained) and the mandatrophy is unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_threshold,
    'At what point do organizational members internalize the procedural structure such that the external scaffold is no longer necessary?',
    'Measurement of member competence in executing procedures without prompting; assessment of whether decisions improve without ritual structure; member self-report of confidence in autonomous execution',
    'If internalization occurs rapidly (3-6 months): scaffold succeeds and can be formally dissolved. If internalization is slow or incomplete: scaffold becomes piton through inertia rather than success.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_threshold, empirical, 'Timeline and indicators of procedural internalization').

omega_variable(
    theater_creep_onset,
    'Does the procedural theater gradually replace substantive decision-making rather than enabling it?',
    'Tracking of time spent on ritual performance vs. decision content; measurement of whether procedure adherence correlates with outcome quality; detection of when ritual becomes substitution rather than scaffold',
    'If theater creep is detected early: constraint can be modified or dissolved before degradation to piton. If undetected: constraint persists as performative vestige.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_creep_onset, empirical, 'Detection of procedural theater replacing substantive function').

omega_variable(
    organizational_readiness_assessment,
    'How should the sunset clause timing be calibrated to match actual organizational capacity rather than predetermined schedule?',
    'Establishment of readiness criteria (member competence levels, decision quality metrics, process variance reduction); comparison of predetermined sunset with readiness-based sunset timelines',
    'If sunset is too early: organization regresses when scaffold is removed. If sunset is too late: constraint degrades to piton through institutional inertia. Calibration determines whether scaffold succeeds as designed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_readiness_assessment, preference, 'Sunset clause calibration to organizational readiness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ritualistic_transition_scaffold, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ritu_tr_t0, ritualistic_transition_scaffold, theater_ratio, 0, 0.5).
narrative_ontology:measurement(ritu_tr_t6, ritualistic_transition_scaffold, theater_ratio, 6, 0.62).
narrative_ontology:measurement(ritu_tr_t12, ritualistic_transition_scaffold, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(ritu_be_t0, ritualistic_transition_scaffold, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ritu_be_t6, ritualistic_transition_scaffold, base_extractiveness, 6, 0.22).
narrative_ontology:measurement(ritu_be_t12, ritualistic_transition_scaffold, base_extractiveness, 12, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ritualistic_transition_scaffold, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The Habit-Building Scaffold is a standalone constraint with no upstream dependencies. It may affect organizational outcomes (productivity, member satisfaction) but does not functionally depend on other constraints. If the organization experiences multiple chaotic transitions, separate scaffold stories would track each transition; this story models the generic structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
