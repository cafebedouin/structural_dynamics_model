% ============================================================================
% CONSTRAINT STORY: integration_by_parts
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_integration_by_parts, []).

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
 *   constraint_id: integration_by_parts
 *   human_readable: Integration by Parts (Mathematical Identity)
 *   domain: mathematics/calculus
 *
 * SUMMARY:
 *   Integration by parts is a fundamental identity in calculus that follows
 *   logically from the Leibniz product rule for differentiation. The
 *   constraint represents a natural law of mathematics: for integrals of the
 *   form ∫u dv, the technique ∫u dv = uv - ∫v du is not optional but required
 *   by the logical structure of calculus itself. This constraint exhibits the
 *   properties of a pure mountain: zero degrees of freedom for all agents,
 *   emergence from fundamental mathematical structure (the product rule),
 *   minimal suppression (the identity is universally accessible), and zero
 *   theater (the technique is either applied correctly or it is not — no
 *   performative element). Unlike institutional mountains that might degrade
 *   over time, mathematical mountains are temporally invariant. All
 *   perspectives — the learner, the educator, the working mathematician, the
 *   analytical observer — experience the same immutable logical necessity. No
 *   agent benefits from the constraint and no agent bears extraction; the
 *   constraint is simply constitutive of calculus as a mathematical system.
 *
 * KEY AGENTS:
 *   - Student Learner: Encounters integration by parts as an inescapable requirement when solving certain integrals. No exit option except abandoning calculus itself. Trapped within the domain of calculus (powerless/trapped/civilizational).
 *   - Mathematics Educator: Must teach integration by parts because it is logically necessary for solving a broad class of problems. Institutional obligation flows from mathematical necessity, not policy choice (institutional/arbitrage/generational).
 *   - Working Mathematician: Applies integration by parts as a necessary computational tool in research, applied mathematics, and engineering. No substitute exists at the logical level (moderate/constrained/biographical).
 *   - Analytical Observer: Examines the logical derivation of integration by parts from the Leibniz product rule and confirms that the constraint emerges from mathematical structure itself (analytical/analytical/civilizational).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(integration_by_parts, 0.12).
domain_priors:suppression_score(integration_by_parts, 0.02).
domain_priors:theater_ratio(integration_by_parts, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(integration_by_parts, extractiveness, 0.12).
narrative_ontology:constraint_metric(integration_by_parts, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(integration_by_parts, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(integration_by_parts, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(integration_by_parts, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(integration_by_parts, mountain).
narrative_ontology:human_readable(integration_by_parts, "Integration by Parts (Mathematical Identity)").
narrative_ontology:topic_domain(integration_by_parts, "mathematics/calculus").

domain_priors:emerges_naturally(integration_by_parts).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE STUDENT LEARNER (MOUNTAIN) — Cannot negotiate or avoid integration by parts when solving certain integral problems. The constraint emerges as an immutable requirement of calculus itself. No exit options exist — the mathematical structure is invariant regardless of the learner's position or preference.
constraint_indexing:constraint_classification(integration_by_parts, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE ANALYTICAL OBSERVER (MOUNTAIN) — From a universal, civilizational perspective, integration by parts is a fundamental logical consequence of the Leibniz product rule for differentiation. The identity ∫u dv = uv - ∫v du follows necessarily from d(uv) = u dv + v du. This is not a contingent institutional arrangement but a structural feature of calculus itself. The constraint has zero degrees of freedom across all indices.
constraint_indexing:constraint_classification(integration_by_parts, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE MATHEMATICS EDUCATION SYSTEM (MOUNTAIN) — Educational institutions cannot escape teaching integration by parts because it is a necessary tool for solving a broad class of integrals. The pedagogical constraint emerges from the logical structure of calculus, not from institutional preference. All institutions face the same immutable requirement.
constraint_indexing:constraint_classification(integration_by_parts, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE WORKING MATHEMATICIAN (MOUNTAIN) — Practicing mathematicians and applied scientists encounter integration by parts as an inescapable computational tool when evaluating certain integrals or solving differential equations. No institutional substitution exists; the constraint is logically prior to any choice of methodology. Even alternative computational frameworks (symbolic algebra systems, numerical methods) ultimately rest on the same mathematical foundations that make integration by parts necessary.
constraint_indexing:constraint_classification(integration_by_parts, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(integration_by_parts_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(integration_by_parts, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(integration_by_parts, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(integration_by_parts, ExtMetricName, E),
    domain_priors:suppression_score(integration_by_parts, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(integration_by_parts),
    narrative_ontology:constraint_metric(integration_by_parts, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(integration_by_parts, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(integration_by_parts_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): The value reflects that integration by parts, while necessary, is not extractive in any meaningful sense. It does not concentrate resources, create asymmetric benefit, or suppress alternatives through coercion. The value is slightly above zero rather than exactly zero because pedagogical presentation involves some effort/cost for learners (they must invest time to master the technique), but this is coordination cost, not extraction. The technique exists to solve a class of problems; no agent profits from the constraint itself. Suppression (0.02): Minimal. The constraint is globally accessible — all agents with access to calculus can learn and apply integration by parts without restriction. No institutional gatekeeping prevents access. Theater ratio (0.08): Near-zero. The technique either works or does not. There is no performative element or gaming the constraint. Accessibility collapse (0.92): Very high. The constraint is invariant across all observational contexts — symbolic algebra, numerical methods, different domains of application, different educational settings. No observational or measurement methodology changes the fundamental identity. Resistance (0.03): Minimal. Once the Leibniz product rule is understood, integration by parts follows logically. No internal or external forces resist the constraint. The measurements show perfect temporal stability because the constraint is a logical identity, not an institutional arrangement subject to drift.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All four perspectives converge on the mountain classification. This convergence is diagnostic: it confirms that the constraint is a true mountain rather than an institutionalized practice misrepresented as natural law. The learner experiences the constraint as immutable; the educator sees it as logically necessary; the working mathematician applies it as an indispensable tool; the analytical observer derives it from first principles. The absence of perspectival disagreement is the signature of a genuine natural law. If perspectives had diverged (if, for example, an institutional beneficiary saw the constraint as coordinate-able while victims saw it as extractive), the constraint would be unmasked as an institutional arrangement using natural law framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Integration by parts has no directionality because it has no beneficiaries or victims. The constraint is not extractive. All agents have the same structural relationship to it: they are constrained by it equally. The constraint does not scale by power, exit options, or scope — the mathematical identity is invariant. This is the defining feature of a mountain. When d (directionality) has no meaningful value because there is no asymmetric extraction, the constraint is logically prior to all power relations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    logical_vs_pedagogical_necessity,
    'Is integration by parts logically necessary (inherent to calculus as a mathematical structure) or pedagogically necessary (contingent on how we choose to teach it)?',
    'Demonstration that the Leibniz product rule logically entails integration by parts, independent of pedagogical convention. Examination of alternative calculus formulations to show that any coherent differential calculus requires equivalent machinery.',
    'If logically necessary: mountain classification is confirmed across all observables. If pedagogically contingent: the constraint might decompose into separate teaching methodology constraints with different ε values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(logical_vs_pedagogical_necessity, conceptual, 'Whether integration by parts is logically or pedagogically necessary').

omega_variable(
    observational_invariance,
    'Does the constraint''s ε value remain stable across different measurement contexts (numerical vs symbolic evaluation, different application domains, different mathematical frameworks)?',
    'Apply the constraint story to symbolic algebra systems, numerical integration routines, computer algebra implementations, and alternative calculus frameworks. Measure the extractiveness and suppression values in each context.',
    'If ε is invariant: mountain classification holds under all observables. If ε varies significantly: the constraint story may conflate multiple structurally distinct claims and require decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observational_invariance, empirical, 'Whether integration by parts ε is invariant across observational contexts').

omega_variable(
    zero_degrees_of_freedom,
    'Does integration by parts truly have zero degrees of freedom for all agent positions, or do different computational contexts create meaningful variation in how the constraint is experienced?',
    'Systematic enumeration of all possible agent-constraint interaction modes (teaching contexts, research contexts, application domains, computational systems). Demonstration that the logical core (the identity) remains invariant while surface manifestations vary.',
    'If zero degrees of freedom confirmed: mountain classification is maximally robust. If degrees of freedom exist: the constraint might be better decomposed into logical necessity (mountain) plus contingent pedagogical/computational implementations (rope or scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_degrees_of_freedom, conceptual, 'Whether integration by parts exhibits zero degrees of freedom across all contexts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(integration_by_parts, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ibp_tr_t0, integration_by_parts, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ibp_tr_t5, integration_by_parts, theater_ratio, 5, 0.08).
narrative_ontology:measurement(ibp_tr_t10, integration_by_parts, theater_ratio, 10, 0.08).

% Extraction over time
narrative_ontology:measurement(ibp_be_t0, integration_by_parts, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(ibp_be_t5, integration_by_parts, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(ibp_be_t10, integration_by_parts, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(integration_by_parts, information_standard).

% DUAL FORMULATION NOTE:
% Integration by parts is a standalone mathematical identity. It does not decompose into multiple constraints with different ε values under different observables. The same identity holds in symbolic mathematics, numerical analysis, computer algebra systems, and formal logic. No dual formulation required.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
