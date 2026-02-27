% ============================================================================
% CONSTRAINT STORY: second_order_unintended_consequences
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_order_unintended_consequences, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_order_unintended_consequences
 *   human_readable: The Cobra Effect Trap: Second-Order Unintended Consequences
 *   domain: social/economic/technological
 *
 * SUMMARY:
 *   The Cobra Effect Trap is a structural constraint where an incentive Rope
 *   designed to solve problem A creates unexpected secondary effects (problem
 *   B) that worsen systemic state or create new extraction channels. The
 *   classic example is the colonial bounty on cobra skins: paying for dead
 *   cobras incentivized breeding rather than hunting. Modern examples span
 *   automated content filtering (incentivizing divisive content that triggers
 *   engagement metrics), pharmaceutical pricing metrics (incentivizing
 *   narrow-market profitable drugs over high-volume generics), citation
 *   counting (incentivizing citation inflation and gaming over impact), and
 *   AI alignment metrics (rewarding proxy objectives that diverge from
 *   intended behavior). The constraint exhibits high perspectival variance:
 *   the policy implementer sees pure Rope (successful coordination toward a
 *   metric), the end user sees Snare (trapped by the unintended consequence),
 *   the auditor sees Tangled Rope (mixed coordination and coercion), and the
 *   civilizational observer risks seeing a natural law (Goodhart's law as
 *   immutable). The extractiveness trajectory shows the classic cobra
 *   pattern: initial low extraction (the Rope works for its stated goal),
 *   rising extraction (metric gaming begins, secondary effects accumulate),
 *   and high extraction (secondary effects dominate the system state,
 *   creating new winners and losers). Theater ratio remains moderate because
 *   some metric-driven coordination still functions — the system has not
 *   fully degraded to pure theater (unlike a Piton). This classification as
 *   Tangled Rope reflects that the Rope has genuine coordination function (it
 *   does organize behavior) but creates sustained asymmetric extraction
 *   (those optimizing the metric benefit at the cost of those bearing the
 *   secondary effects).
 *
 * KEY AGENTS:
 *   - Policy Implementer / Metric Optimizer: Primary beneficiary (institutional/arbitrage) — designed the Rope, benefits from metric-driven behavior, experiences the constraint as pure coordination; has institutional incentive to claim the Rope is working even if secondary effects emerge
 *   - End User / Systemic Integrity: Primary victim (powerless/trapped) — depends on the system functioning for original purpose; bears full cost of secondary effect; has no exit or voice
 *   - Ground-Truth Observer / Auditor: Secondary actor (moderate/constrained) — detects divergence between metric and reality; benefits from the Rope's coordination function but faces extraction pressure (retaliation, institutional silencing) when reporting secondary effects
 *   - Regulatory / Oversight Agency: Institutional actor (organized/constrained) — benefits from systematic auditing enabled by the Rope but constrained by inability to redesign entire regulatory framework; faces mixed extraction and coordination
 *   - Adaptive Systems Community: Organized actor (organized/constrained) — sees secondary effect as solvable problem with sunset; building alternative multi-objective architectures; represents Scaffold perspective
 *   - Legacy Metric Regime: Institutional infrastructure (institutional/arbitrage) — persists through inertia; sees own measurement systems as degraded but difficult to replace (Piton perspective)
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing the cobra effect as an immutable law rather than recognizing it as contingent on feedback delays, metric opacity, and institutional misalignment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_order_unintended_consequences, 0.58).
domain_priors:suppression_score(second_order_unintended_consequences, 0.62).
domain_priors:theater_ratio(second_order_unintended_consequences, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_order_unintended_consequences, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_order_unintended_consequences, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(second_order_unintended_consequences, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_order_unintended_consequences, tangled_rope).
narrative_ontology:human_readable(second_order_unintended_consequences, "The Cobra Effect Trap: Second-Order Unintended Consequences").
narrative_ontology:topic_domain(second_order_unintended_consequences, "social/economic/technological").

domain_priors:requires_active_enforcement(second_order_unintended_consequences).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_order_unintended_consequences, policy_implementers).
narrative_ontology:constraint_beneficiary(second_order_unintended_consequences, metric_optimizers).
narrative_ontology:constraint_victim(second_order_unintended_consequences, systemic_integrity).
narrative_ontology:constraint_victim(second_order_unintended_consequences, end_users).
narrative_ontology:constraint_victim(second_order_unintended_consequences, ground_truth_tracking).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER / SYSTEMIC INTEGRITY (SNARE) — Those who depend on the system functioning for its original purpose (not the metric) are trapped. The Rope was intended to solve problem A; the secondary effect is problem B. End users have no exit: they cannot opt out of the system or the unintended consequence. The constraint appears as pure extraction because their welfare diverges from the incentive target.
constraint_indexing:constraint_classification(second_order_unintended_consequences, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GROUND-TRUTH OBSERVER / MEASUREMENT AUDITOR (TANGLED ROPE) — Auditors and field-based observers who detect the secondary effect face a mixed experience. They benefit from the Rope's coordination function (shared measurement standards, systematic feedback) but bear the extraction cost of discovering metric gaming and having to report it despite institutional pressure to hide the divergence. Constrained exit: they can speak, but face institutional retaliation.
constraint_indexing:constraint_classification(second_order_unintended_consequences, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POLICY IMPLEMENTER / METRIC OPTIMIZER (ROPE) — The agent who designed or deployed the incentive structure experiences the constraint as coordination: the Rope is solving their primary problem (organizing behavior toward the metric). They benefit from arbitrage (can choose which metrics to optimize, can shift targets if the secondary effect becomes visible). From their perspective, this is pure coordination — the system is working as intended. The fact that unintended consequences arise is an observable-level phenomenon, not part of their structural experience.
constraint_indexing:constraint_classification(second_order_unintended_consequences, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: ADAPTIVE SYSTEMS COMMUNITY (SCAFFOLD) — Organizational learning theorists, system dynamics researchers, and feedback-loop auditors see the cobra effect as a solvable problem with a sunset: multi-objective optimization, Goodhart-aware metric design, adversarial auditing, and causal inference methods are building alternative verification pathways. The constraint is temporary coordination failure, not inherent extraction. Organized agents see an exit: redesigned incentive architectures with feedback loops that detect secondary effects before they compound.
constraint_indexing:constraint_classification(second_order_unintended_consequences, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY METRIC REGIME (PITON) — Traditional performance measurement systems (KPIs, SLAs, quota-based incentives) persist as ritualized theater despite well-documented failure modes. The regime persists not because it works but because changing measurement infrastructure is institutionally costly. The theater ratio is moderate (0.48) because some metric-driven coordination still functions — but the primary function (aligning incentives with system health) has atrophied. Piton classification reflects institutional inertia: the constraint endures through maintenance rituals despite reduced efficacy.
constraint_indexing:constraint_classification(second_order_unintended_consequences, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / UNIVERSAL VIEW (MOUNTAIN) — From a civilizational, universal perspective, some second-order effects are inherent to goal-driven systems: any proxy metric will diverge from ground truth, any incentive structure will create gaming opportunities, and complex systems with feedback delay always exhibit oscillation and overshoot. This perspective risks naturalizing the cobra effect as an immutable law — 'all measurement systems create perverse incentives' — when structural data reveals this as a false summit. The effect is contingent on specific feedback loop delays, metric opacity, and institutional architecture — not universal.
constraint_indexing:constraint_classification(second_order_unintended_consequences, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: REGULATORY / OVERSIGHT AGENCY (TANGLED ROPE) — Institutional regulators (bank supervisors, environmental agencies, safety inspectors) often benefit from the original Rope (standardized reporting, systematic auditing) but bear asymmetric extraction when secondary effects emerge. They have constrained exit: they cannot ignore detected gaming without institutional consequences, but they also cannot easily redesign the entire regulatory framework. Mixed experience of coordination and coercion.
constraint_indexing:constraint_classification(second_order_unintended_consequences, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_order_unintended_consequences_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_order_unintended_consequences, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_order_unintended_consequences, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_order_unintended_consequences, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_order_unintended_consequences, TR),
    TR >= 0.70.

:- end_tests(second_order_unintended_consequences_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The Rope creates sustained asymmetry between those optimizing the metric (policy implementers, metric owners) and those bearing secondary effects (end users, systemic integrity, ground-truth trackers). The extraction is not maximal because the original Rope function does solve its stated problem — the system still coordinates behavior, just toward a proxy that diverges from intended outcomes. The trajectory shows the cobra pattern: extractiveness rises over the interval as metric gaming compounds and secondary effects accumulate. By time point 6, the secondary effect has become a structural feature, not a transient side effect. Suppression (0.62): Moderate-high. Significant barriers prevent visibility and correction of secondary effects: institutional incentives to hide divergence, technical difficulty in detecting metric gaming, career risk for auditors who report problems, and high institutional cost of redesigning systems. But suppression is not total — field-based observers eventually detect divergence, auditors can surface issues, and some institutions do redesign. Theater ratio (0.48): Moderate. The Rope still has functional coordination content — metrics do organize behavior, auditing does happen, feedback systems exist. Theater is not high because the system is not purely performative (unlike a Piton). But the feedback loop's delayed response and metric opacity create moderate performative content (ritual reporting, post-hoc justification of secondary effects as 'unexpected' rather than 'designed for').
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range of a Tangled Rope. The policy implementer sees Rope (pure coordination). The end user sees Snare (pure extraction). The auditor sees Tangled Rope (mixed). The regulatory agency sees Tangled Rope with institutional complexity (coordinating function + constrained exit). The adaptive systems community sees Scaffold (temporary problem with solution in progress). The legacy metric regime sees Piton (degraded institutional ritual). The analytical observer risks seeing Mountain (Goodhart's law as inevitable). The engine's task is to show that the mountain perspective is a false summit — the cobra effect is not inevitable but contingent on specific feedback delays, metric opacity, and institutional misalignment. Redesigned incentive architectures with multi-objective optimization and feedback auditing represent real alternatives, making the Scaffold sunset plausible.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is driven by structural position within the extraction flow created by metric gaming. The policy implementer benefits from the original Rope and has arbitrage exit (can change metrics, can claim secondary effects were unforeseeable); low d. The end user is trapped with no exit and bears full secondary-effect costs; high d. The auditor has moderate power and constrained exit (can speak but faces retaliation); moderate d. The regulatory agency is organized but institutionally constrained (cannot easily redesign frameworks); moderate-high d. The adaptive systems community is organized with exit paths (alternative architectures exist); lower-moderate d. The legacy metric regime has institutional arbitrage (metrics can change) but is increasingly inert; d reflects both historical beneficiary status and contemporary institutional decay.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CHALLENGE: The cobra effect appears to violate the tangled rope classification because the extractiveness seems to exceed what pure coordination should allow. If the Rope is 'just' an incentive structure, why does it create such severe secondary effects? The resolution is that the Rope's coordination function is real and persists — metrics do organize behavior, auditing does happen, feedback systems do function. But the Rope is sustained by suppression of feedback loops (metric opacity, institutional pressure to hide divergence, career risk for auditors). The tangled rope classification holds because both the coordination function (real) and the extraction asymmetry (real) are present. The secondary effect is not inherent to the Rope itself but contingent on the feedback loop structure and institutional incentives to game the metric. Multi-objective optimization and adversarial auditing represent genuine Scaffold alternatives — not perfect solutions but real structural changes that reduce secondary-effect severity. The classification prevents mislabeling the cobra effect as either pure coordination (which would miss the real extraction) or pure extraction (which would miss the real coordination function). The tangled rope classification highlights the hybrid nature: solve problem A, create problem B, sustain the asymmetry through institutional pressure, create the conditions for alternative solutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_divergence_threshold,
    'At what rate of metric-reality divergence does the secondary effect become structurally dominant (when the unintended consequence exceeds the original problem''s severity)?',
    'Comparative measurement of original system state (before Rope) vs metric-optimized state vs ground-truth state; quantification of secondary effect magnitude relative to primary problem',
    'If threshold < 2x original problem: secondary effect triggers rapid policy reversal (Scaffold sunset is real). If threshold > 5x: effect compounds undetected for years (Snare persists longer). Determines whether constraint is temporary (Scaffold) or entrenched (Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_divergence_threshold, empirical, 'Threshold for secondary effect dominance over original problem').

omega_variable(
    feedback_loop_observability,
    'Can the policy implementer and oversight community detect the divergence between metric and ground truth before the secondary effect compounds beyond recovery cost?',
    'Audit trail analysis; comparison of time-to-detection vs time-to-secondary-effect-saturation; analysis of institutional incentives to report divergence vs incentives to hide it',
    'If observable < 1 year: divergence detected before compounding (enables Scaffold exit). If unobservable > 3 years: secondary effect becomes entrenched before visibility (Snare/Piton dynamics dominate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feedback_loop_observability, empirical, 'Observability of metric-reality divergence within institutional timescales').

omega_variable(
    multi_objective_solution_accessibility,
    'Are there technically and institutionally accessible alternatives to single-metric optimization (multi-objective functions, causal inference, feedback auditing) that the policy implementer could adopt without major institutional cost?',
    'Survey of available methods; analysis of implementation barriers (computational cost, institutional complexity, expertise requirements); case study comparison with systems that adopted multi-objective approaches',
    'If accessible: Scaffold sunset is credible (organized actors can redesign). If inaccessible: constraint persists as Snare or Tangled Rope (institutional lock-in dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_objective_solution_accessibility, conceptual, 'Accessibility of multi-objective alternatives to single-metric optimization').

omega_variable(
    accountability_asymmetry,
    'Do institutional actors face differential accountability for the secondary effect vs the original problem? Who bears the cost of failure?',
    'Analysis of career consequences for policy implementers if secondary effect is revealed vs if original problem persists undetected; identification of accountability gaps',
    'If implementer is shielded from secondary-effect costs: extraction persists (Snare/Tangled Rope). If implementer bears costs: pressure for redesign (Scaffold sunset). Determines whether constraint is sustained by institutional misalignment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accountability_asymmetry, preference, 'Asymmetric accountability for primary problem vs secondary effect').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_order_unintended_consequences, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cobra_tr_t0, second_order_unintended_consequences, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cobra_tr_t3, second_order_unintended_consequences, theater_ratio, 3, 0.35).
narrative_ontology:measurement(cobra_tr_t6, second_order_unintended_consequences, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(cobra_be_t0, second_order_unintended_consequences, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cobra_be_t3, second_order_unintended_consequences, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(cobra_be_t6, second_order_unintended_consequences, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_order_unintended_consequences, enforcement_mechanism).
narrative_ontology:affects_constraint(second_order_unintended_consequences, goodharts_law_metric_divergence).
narrative_ontology:affects_constraint(second_order_unintended_consequences, institutional_metric_gaming).
narrative_ontology:affects_constraint(second_order_unintended_consequences, feedback_loop_delay_oscillation).

% DUAL FORMULATION NOTE:
% The Cobra Effect Trap is a family of constraints sharing the structure: Rope designed for problem A creates secondary effects worse than problem A. Each domain (pharma, tech, regulation, academia) has distinct epsilon values reflecting different metric opacity and institutional accountability. The family members are linked by shared causal structure (feedback delay + metric opacity + suppression of divergence reports) but differ in severity and sunset credibility. 'goodharts_law_metric_divergence' is upstream (abstract principle); 'institutional_metric_gaming' and 'feedback_loop_delay_oscillation' are downstream domain-specific manifestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_order_unintended_consequences, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
