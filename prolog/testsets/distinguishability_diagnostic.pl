% ============================================================================
% CONSTRAINT STORY: distinguishability_diagnostic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_distinguishability_diagnostic, []).

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
 *   constraint_id: distinguishability_diagnostic
 *   human_readable: Distinguishability Diagnostic for Structural Limits vs Correctable Friction
 *   domain: philosophy_of_mind/systems_theory/phenomenology_of_constraint
 *
 * SUMMARY:
 *   The distinguishability diagnostic is an operational test to determine
 *   whether a perceived constraint is a structural limit (load-bearing,
 *   unchangeable within the system's current configuration) or correctable
 *   friction (effort-responsive, addressable through intervention). The
 *   diagnostic uses two primary observables: (1) effort transparency — does
 *   additional input yield linear improvement (friction) or asymptotic
 *   flattening (structural limit)? (2) system feedback — is resistance
 *   self-reinforcing (structural) or externally imposed (correctable)? This
 *   constraint is downstream of 'limit_as_information' (the philosophical
 *   claim that structural limits carry informational content about system
 *   architecture). The diagnostic operationalizes that claim by providing a
 *   testable procedure. The constraint exhibits low extraction (0.32) because
 *   it primarily coordinates decision-making rather than extracting from any
 *   agent class. The modest extraction reflects the meta-cognitive overhead
 *   required to apply the diagnostic correctly and the risk of misapplication
 *   (premature acceptance of limits or futile effort against structural
 *   constraints). Theater ratio (0.35) is moderate — some practitioners apply
 *   the diagnostic ritualistically without genuine causal analysis, but the
 *   test itself has real discriminatory power when applied correctly.
 *
 * KEY AGENTS:
 *   - System Designers: Primary beneficiaries (institutional/arbitrage) — use the diagnostic to allocate design effort efficiently, avoiding wasted resources on unchangeable constraints
 *   - Intervention Planners: Primary beneficiaries (organized/mobile) — apply the diagnostic to distinguish policy problems requiring structural reform from those addressable incrementally
 *   - Resource Allocators: Primary beneficiaries (powerful/mobile) — use effort transparency to inform budget decisions about system modification vs acceptance
 *   - Frontline Practitioners: Mixed position (moderate/constrained) — benefit from knowing when effort is futile but bear the cost of meta-cognitive overhead and misapplication risk
 *   - Methodological Reformers: Organized agents (organized/mobile) — see the diagnostic as temporary scaffolding while systems literacy matures
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the diagnostic as pure coordination mechanism reducing ambiguity about constraint type
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(distinguishability_diagnostic, 0.32).
domain_priors:suppression_score(distinguishability_diagnostic, 0.28).
domain_priors:theater_ratio(distinguishability_diagnostic, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(distinguishability_diagnostic, extractiveness, 0.32).
narrative_ontology:constraint_metric(distinguishability_diagnostic, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(distinguishability_diagnostic, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(distinguishability_diagnostic, rope).
narrative_ontology:human_readable(distinguishability_diagnostic, "Distinguishability Diagnostic for Structural Limits vs Correctable Friction").
narrative_ontology:topic_domain(distinguishability_diagnostic, "philosophy_of_mind/systems_theory/phenomenology_of_constraint").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(distinguishability_diagnostic, system_designers).
narrative_ontology:constraint_beneficiary(distinguishability_diagnostic, intervention_planners).
narrative_ontology:constraint_beneficiary(distinguishability_diagnostic, resource_allocators).
narrative_ontology:constraint_beneficiary(distinguishability_diagnostic, policy_analysts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYSTEM DESIGNER (ROPE) — Uses the diagnostic as a coordination tool to allocate resources efficiently. Distinguishing structural limits from friction prevents wasted effort on unchangeable constraints and focuses intervention on correctable problems. Net beneficiary — the diagnostic reduces uncertainty and enables better design decisions.
constraint_indexing:constraint_classification(distinguishability_diagnostic, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERVENTION PLANNER (ROPE) — Applies the diagnostic to distinguish policy problems that require structural reform from those addressable through incremental improvement. The test (effort transparency, feedback pattern) provides actionable information. Experiences the constraint as pure coordination — it solves the collective action problem of knowing where to intervene.
constraint_indexing:constraint_classification(distinguishability_diagnostic, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: RESOURCE ALLOCATOR (ROPE) — Uses the diagnostic to decide whether to invest in changing a system or accept its limits. Effort transparency (does additional input yield linear improvement or asymptotic flattening?) directly informs budget decisions. Low extraction — the diagnostic is a decision aid with minimal overhead.
constraint_indexing:constraint_classification(distinguishability_diagnostic, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: FRONTLINE PRACTITIONER (TANGLED ROPE) — Experiences the diagnostic as both helpful (identifies when effort is futile) and extractive (requires meta-cognitive overhead to apply correctly; misapplication can justify abandoning solvable problems as 'structural'). The coordination function (knowing when to stop pushing) coexists with extraction risk (premature acceptance of limits that are actually correctable friction).
constraint_indexing:constraint_classification(distinguishability_diagnostic, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: METHODOLOGICAL REFORMER (SCAFFOLD) — Sees the diagnostic as a temporary tool needed while systems thinking matures. As practitioners internalize the distinction between structural limits and friction through experience, the explicit diagnostic becomes less necessary. The constraint has a sunset: once the phenomenological literacy is widespread, the formal test is redundant. Low extraction with declining necessity over time.
constraint_indexing:constraint_classification(distinguishability_diagnostic, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — The diagnostic is a pure coordination mechanism. It provides a shared operational test (effort transparency, feedback pattern) that enables agents to converge on the same structural assessment without requiring identical priors or values. The test's value is informational — it reduces ambiguity about constraint type, enabling better collective decision-making. No extraction detected at the analytical level.
constraint_indexing:constraint_classification(distinguishability_diagnostic, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(distinguishability_diagnostic_tests).
:- end_tests(distinguishability_diagnostic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Low-moderate. The diagnostic provides genuine coordination value (knowing where to intervene) but requires meta-cognitive overhead to apply correctly. The extraction is not from a specific victim class but distributed across all users as the cost of maintaining the distinction. Misapplication can generate extraction (abandoning solvable problems as 'structural' or wasting effort on unchangeable limits), but this is a failure mode rather than the diagnostic's primary function. Suppression (0.28): Low-moderate. The diagnostic does not coerce — agents can ignore it or apply alternative tests. The suppression reflects the cognitive cost of learning to apply the test correctly and the risk that incorrect application constrains action (either premature acceptance or futile effort). Theater ratio (0.35): Moderate. Some practitioners apply the diagnostic ritualistically (checking boxes without genuine causal analysis), but the test has real discriminatory power when applied with systems literacy. The theater has increased slightly over the interval as the diagnostic has been adopted in contexts where practitioners lack the background to apply it correctly.
 *
 * PERSPECTIVAL GAP:
 *   The diagnostic shows minimal perspectival gap — most agents classify it as Rope (pure coordination). The exception is the frontline practitioner perspective (Tangled Rope), which experiences both coordination and extraction due to the meta-cognitive overhead and misapplication risk. The scaffold perspective (methodological reformer) sees a sunset clause: as systems literacy matures, the explicit diagnostic becomes redundant. The gap between Rope and Scaffold is not a disagreement about current extraction but about future necessity. The gap between Rope and Tangled Rope reflects the difference between agents who have the literacy to apply the diagnostic correctly (low overhead) and those who struggle with it (higher overhead, misapplication risk).
 *
 * DIRECTIONALITY LOGIC:
 *   All primary beneficiaries (system designers, intervention planners, resource allocators) are institutional or organized agents with mobile or arbitrage exit options. They experience the diagnostic as a coordination tool that reduces uncertainty and enables better decisions. The derived directionality values are low (beneficiary status + high exit options → low d → low or negative chi). Frontline practitioners have constrained exit options and experience both coordination (knowing when to stop) and extraction (meta-cognitive overhead, misapplication risk), placing them in a mixed position. The analytical observer sees pure coordination with no extraction. No victims are declared because the diagnostic does not systematically extract from any agent class — the costs are distributed overhead rather than asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The diagnostic resolves mandatrophy by providing an operational test that distinguishes structural limits (which should be accepted or worked around) from correctable friction (which should be addressed through intervention). Without the diagnostic, agents risk two failure modes: (1) treating friction as structural (premature acceptance, wasted potential), or (2) treating structural limits as friction (futile effort, resource waste). The diagnostic's coordination function is precisely to prevent these misclassifications. The modest extraction (0.32) reflects the real cost of maintaining the distinction — the meta-cognitive overhead and the risk of misapplication. This is not extraction in the sense of asymmetric benefit but the inherent cost of the coordination mechanism itself. The Tangled Rope classification from the frontline practitioner perspective captures this: the diagnostic both helps (coordination) and costs (overhead), but the cost is not high enough to dominate the benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effort_measurement_precision,
    'What precision threshold is required to distinguish genuine asymptotic flattening from temporary plateaus in effort-response curves?',
    'Longitudinal tracking of intervention outcomes across multiple domains; statistical analysis of plateau duration vs breakthrough frequency; identification of measurement noise vs signal',
    'If threshold too sensitive: correctable friction misclassified as structural limit, leading to premature abandonment of solvable problems. If threshold too coarse: structural limits misclassified as friction, leading to wasted effort on unchangeable constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effort_measurement_precision, empirical, 'Precision threshold for distinguishing asymptotic flattening from temporary plateaus').

omega_variable(
    feedback_attribution_ambiguity,
    'How do we distinguish self-reinforcing system feedback (structural) from externally imposed resistance (correctable) when both produce similar effort-response patterns?',
    'Causal analysis of feedback loops; intervention experiments that isolate internal vs external resistance sources; comparison of system behavior under different external conditions',
    'If attribution fails: the diagnostic loses discriminatory power — both structural limits and correctable friction appear identical. If attribution succeeds: the diagnostic becomes a reliable operational test.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feedback_attribution_ambiguity, empirical, 'Whether feedback patterns reliably distinguish internal vs external resistance').

omega_variable(
    practitioner_literacy_threshold,
    'What level of systems literacy is required for practitioners to apply the diagnostic correctly without generating false negatives (abandoning solvable problems) or false positives (persisting against structural limits)?',
    'Training effectiveness studies; error rate analysis across practitioner experience levels; identification of common misapplication patterns',
    'If literacy threshold is high: the diagnostic becomes an elite tool inaccessible to frontline practitioners, limiting its coordination value. If literacy threshold is low: widespread adoption but high error rates undermine trust in the diagnostic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practitioner_literacy_threshold, empirical, 'Systems literacy required for correct diagnostic application').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(distinguishability_diagnostic, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(distdiag_tr_t0, distinguishability_diagnostic, theater_ratio, 0, 0.25).
narrative_ontology:measurement(distdiag_tr_t5, distinguishability_diagnostic, theater_ratio, 5, 0.3).
narrative_ontology:measurement(distdiag_tr_t10, distinguishability_diagnostic, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(distdiag_be_t0, distinguishability_diagnostic, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(distdiag_be_t5, distinguishability_diagnostic, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(distdiag_be_t10, distinguishability_diagnostic, base_extractiveness, 10, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(distinguishability_diagnostic, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of 'limit_as_information' (the philosophical claim that structural limits carry informational content). The diagnostic operationalizes that claim by providing a testable procedure to distinguish structural limits from correctable friction. The upstream constraint has low extractiveness (mountain classification) reflecting its status as a conceptual framework; the diagnostic has moderate extractiveness (rope classification) reflecting the overhead of applying the operational test.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
