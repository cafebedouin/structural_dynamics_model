% ============================================================================
% CONSTRAINT STORY: metric_gaming_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_metric_gaming_dynamics, []).

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
 *   constraint_id: metric_gaming_dynamics
 *   human_readable: Metric Gaming Dynamics: Goodhart's Law as Structural Constraint
 *   domain: organizational_governance/measurement_systems
 *
 * SUMMARY:
 *   Metric gaming dynamics represent a structural constraint arising when
 *   organizational actors are evaluated against quantitative measures.
 *   Goodhart's Law — 'when a measure becomes a target, it ceases to be a good
 *   measure' — describes the empirical phenomenon: agents optimize for metric
 *   values rather than the outcomes the metrics were designed to capture.
 *   This constraint exhibits characteristics of both coordination
 *   (measurement enables organizational alignment and performance tracking)
 *   and extraction (gaming distorts work, subordinates outcomes to metric
 *   optimization, and concentrates benefits on metric optimizers). The
 *   constraint operates through institutional suppression: measured agents
 *   face career consequences for poor metric performance, limited
 *   alternatives to complying with measurement regimes, and cognitive capture
 *   from internalized performance narratives. Theater ratio is high (0.78)
 *   because extensive auditing and metric refinement rituals are performed
 *   while gaming persists — the apparatus appears to address gaming while the
 *   mechanism continues. The trajectory shows measurement theater and gaming
 *   intensity increasing over time as agents become more sophisticated at
 *   optimization and as institutions accumulate more metrics.
 *
 * KEY AGENTS:
 *   - Measured Agents: Primary victims (powerless/trapped) — subject to metric optimization pressure with no exit; bear costs of distorted work and perverse incentives
 *   - Metric Optimizers: Primary beneficiaries (institutional/arbitrage) — capture rewards for gaming; have exit options (ability to shift between metric systems or to coaching positions)
 *   - Measurement System Designers: Secondary beneficiaries (institutional/arbitrage) — design and refine metrics; perceive gaming as a solvable design problem
 *   - Aware Reformers: Secondary victims (moderate/constrained) — recognize gaming dynamics and attempt redesign; constrained by institutional resistance and metric inertia
 *   - Competing Institutions: Organized actors (organized/mobile) — demonstrate alternative institutional designs with lower gaming rates and sunset characteristics
 *   - Outcome Integrity: Structural victim (powerless/trapped) — abstract collective good; bears full cost of gaming through corrupted feedback and distorted organizational learning
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (Goodhart's Law) as immutable laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(metric_gaming_dynamics, 0.58).
domain_priors:suppression_score(metric_gaming_dynamics, 0.65).
domain_priors:theater_ratio(metric_gaming_dynamics, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(metric_gaming_dynamics, extractiveness, 0.58).
narrative_ontology:constraint_metric(metric_gaming_dynamics, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(metric_gaming_dynamics, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(metric_gaming_dynamics, tangled_rope).
narrative_ontology:human_readable(metric_gaming_dynamics, "Metric Gaming Dynamics: Goodhart's Law as Structural Constraint").
narrative_ontology:topic_domain(metric_gaming_dynamics, "organizational_governance/measurement_systems").

domain_priors:requires_active_enforcement(metric_gaming_dynamics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(metric_gaming_dynamics, metric_optimizers).
narrative_ontology:constraint_beneficiary(metric_gaming_dynamics, measurement_beneficiaries).
narrative_ontology:constraint_victim(metric_gaming_dynamics, outcome_integrity).
narrative_ontology:constraint_victim(metric_gaming_dynamics, subordinate_agents).
narrative_ontology:constraint_victim(metric_gaming_dynamics, system_performance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MEASURED AGENT (SNARE) — Subject to metric optimization pressure with no exit. Bears full cost of gaming: distorted work patterns, perverse incentives, impossibility of genuine performance. Cannot reject the measurement regime without career termination. Experiences maximum extraction through metric manipulation.
constraint_indexing:constraint_classification(metric_gaming_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE AWARE REFORMER (TANGLED ROPE) — Recognizes the gaming dynamics and attempts metric redesign or enforcement improvement. Constrained by institutional resistance, metric inertia, and the logical necessity of measurement. Experiences both genuine coordination function (measurement enables comparison and accountability) and extraction (the gaming mechanism persists despite awareness). High suppression from institutional rigidity; genuine effort toward coordination.
constraint_indexing:constraint_classification(metric_gaming_dynamics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MEASUREMENT SYSTEM DESIGNER (ROPE) — Designs and refines the metric system. Has arbitrage exit options (can shift metrics, adjust baselines, redefine success criteria). Experiences the constraint primarily as a coordination problem: metrics are necessary tools for organizational alignment. Benefits from stable measurement infrastructure. Low effective extraction for this agent; sees gaming as a solvable design problem rather than structural trap.
constraint_indexing:constraint_classification(metric_gaming_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPETING INSTITUTION (SCAFFOLD) — An organization that has experimentally bypassed metric gaming through transparent measurement, outcome-based incentives with feedback loops, or outcome-independent compensation. Sees the original gaming constraint as temporary — alternative institutional designs with lower theater ratios are gradually replacing traditional measurement regimes. Has sunset clause characteristics: as alternative institutions prove viability, gaming constraints lose competitive force.
constraint_indexing:constraint_classification(metric_gaming_dynamics, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: LEGACY MEASUREMENT APPARATUS (PITON) — Measurement systems persist through institutional inertia despite widespread recognition that gaming is occurring and that metrics have degraded. Theater ratio high (0.78): elaborate auditing, compliance reporting, and metric refinement rituals are performed while the underlying gaming mechanism persists. The apparatus is known to be degraded but is maintained because alternatives haven't fully displaced it and because the measurement habit is deeply embedded in organizational identity.
constraint_indexing:constraint_classification(metric_gaming_dynamics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, Goodhart's Law appears as an invariant of measurement: any metric used as a target ceases to be a good measure of the outcome it was designed to capture. This perspective treats the gaming constraint as an immutable property of human systems subject to measurement. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit, revealing that the perceived inevitability of gaming naturalizes what is actually a contingent institutional arrangement susceptible to governance redesign.
constraint_indexing:constraint_classification(metric_gaming_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(metric_gaming_dynamics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(metric_gaming_dynamics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(metric_gaming_dynamics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(metric_gaming_dynamics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(metric_gaming_dynamics, TR),
    TR >= 0.70.

:- end_tests(metric_gaming_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The original research team revised this from 0.72 because empirical data shows that gaming is not absolute — many measured agents balance metric optimization with real work, some measurement systems successfully reduce gaming through design improvements, and competing institutions with lower gaming rates demonstrate that alternative arrangements are viable. The remaining 0.58 reflects significant but not total extraction through metric distortion, career risk for non-compliance, and concentration of benefits on metric optimizers. Suppression (0.65): Moderate-high. Measured agents face career consequences for poor metric performance (material suppression), limited alternatives to metric-based evaluation in their domain (structural suppression), and internalization of metric optimization as legitimate success (cognitive suppression). But suppression is not total — some agents resist gaming, alternative institutions exist, and transparency movements are reducing suppression in some sectors. Theater ratio (0.78): High and increasing. Extensive auditing, metric refinement, gaming detection, and compliance reporting rituals are performed while the underlying gaming mechanism persists. The trajectory shows theater increasing over time as institutions accumulate more metrics and monitoring systems.
 *
 * PERSPECTIVAL GAP:
 *   The central perspectival gap is between the beneficiary and victim experiences. The measurement system designer sees coordination: metrics enable organizational alignment and performance tracking (Rope). The metric optimizer sees rewards for gaming with minimal cost (Rope or Arbitrage-level benefits). The measured agent sees a trap: optimization pressure with no exit and distorted work (Snare). The aware reformer sees mixed coordination and extraction: genuine measurement function alongside persistent gaming (Tangled Rope). The competing institution sees a temporary problem with architectural solutions (Scaffold). The civilizational observer risks seeing an immutable natural law (Mountain — Goodhart's Law as inevitable) but the structural data contradicts this: gaming rates vary dramatically across institutional designs, suggesting the mountain classification is a false summit that naturalizes contingent arrangements. The perspectival gaps reveal that classification depends on position (beneficiary vs victim) and time horizon (immediate optimization vs generational institutional design).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position in the metric gaming pipeline. Metric optimizers (institutional power, arbitrage exit) have low d because they benefit from the constraint — the gaming mechanism extracts value toward them. Measured agents (powerless, trapped) have high d because they bear extraction costs and cannot exit. Aware reformers (moderate power, constrained exit) have intermediate d — they experience both coordination function (metrics provide real feedback) and extraction (gaming distorts the feedback). The measurement system designer has low d (institutional power, high arbitrage options) — they can redesign metrics and shift between systems. Outcome integrity is an abstract victim (powerless, trapped) — it has the highest d because it cannot organize or exit but bears full structural cost of gaming. The analytical observer at civilizational scope has intermediate d (observational position, analytical exit) — they see the system as a whole and can shift analytical frames.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that metric gaming dynamics contain genuine coordination function (measurement does enable organizational alignment and performance comparison) alongside genuine extraction (gaming concentrates benefits on optimizers and costs on measured agents). The constraint is neither pure coordination (Rope) nor pure extraction (Snare) but a hybrid that requires active institutional enforcement to maintain the coordination function while suppressing the extraction. The false summit risk is high: Goodhart's Law is often treated as a natural law (Mountain) — an inevitable consequence of measurement itself — but the structural data reveals this as naturalization of contingent institutional arrangements. Organizations with transparent measurement, outcome-independent compensation, or outcome-based feedback loops demonstrate lower gaming rates and lower theater ratios, suggesting that the appearance of inevitability is actually institutional path dependence. The constraint will remain Tangled Rope unless institutional redesign either eliminates the measurement system entirely (shifting to a different coordination mechanism) or successfully decouples agent incentives from metric optimization (reducing the extraction mechanism while preserving the coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_necessity_vs_gaming,
    'Is measurement gaming an inevitable consequence of any measurement system, or is it a pathology of specific institutional designs with high suppression and low transparency?',
    'Comparative institutional analysis: measure gaming rates across organizations with different measurement transparency, feedback frequency, and agent autonomy levels. Test whether organizations with outcome-based or outcome-independent compensation show lower gaming rates.',
    'If inevitable: metric gaming is closer to mountain (unmeasurable without distortion). If contingent: metric gaming is a tangled_rope or snare with governance solutions, and the mountain classification is a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_necessity_vs_gaming, empirical, 'Whether metric gaming is inevitable or contingent on institutional design').

omega_variable(
    real_outcome_recovery,
    'Can real outcome integrity be recovered after metrics have been systematically gamed? Is there a measurement technology that prevents post-hoc gaming detection?',
    'Forensic analysis of organizations that have re-measured after discovering gaming: can true outcome status be reconstructed? Do real-time measurement, distributed verification, or outcome-independent auditing prevent gaming?',
    'If recovery is possible and prevention is feasible: gaming is a temporary institutional failure (scaffold). If gaming is irreversible and undetectable: extractiveness should be higher and suppression should be rated as structural rather than behavioral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_outcome_recovery, empirical, 'Whether real outcome status can be recovered after metric gaming').

omega_variable(
    agent_capacity_vs_gaming_incentive,
    'Do agents game metrics primarily because they lack capacity for genuine performance (gaming is compensatory) or because gaming is rewarded more highly than real performance (gaming is rational extraction)?',
    'Intervention study: increase resources/training to measured agents while holding metric system constant. If gaming decreases, it was compensatory (higher suppression, lower extractiveness). If gaming persists, it is rational extraction (lower suppression, higher extractiveness).',
    'If compensatory: constraint is partially rope (measurement reveals real capacity gaps) and victims are partly beneficiaries of honest feedback. If rational extraction: constraint is pure snare with active obstruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agent_capacity_vs_gaming_incentive, empirical, 'Whether metric gaming is compensatory or rational extraction').

omega_variable(
    transparency_as_prevention,
    'Does revealing the gaming mechanism and its economic incentives to measured agents reduce gaming rates, or does transparency merely shift gaming tactics?',
    'A/B test: measure gaming rates in transparent vs opaque measurement regimes where metric formulas and optimization incentives are either visible or hidden to agents.',
    'If transparency reduces gaming: suppression is cognitive (false consciousness can be overcome), theater is lower in transparent regimes. If transparency does not reduce gaming: suppression is structural (real barriers persist), and the constraint is more entrenched than the current scoring suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transparency_as_prevention, empirical, 'Whether transparency prevents or merely transforms metric gaming').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(metric_gaming_dynamics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(metgam_tr_t0, metric_gaming_dynamics, theater_ratio, 0, 0.55).
narrative_ontology:measurement(metgam_tr_t3, metric_gaming_dynamics, theater_ratio, 3, 0.68).
narrative_ontology:measurement(metgam_tr_t6, metric_gaming_dynamics, theater_ratio, 6, 0.78).
narrative_ontology:measurement(metgam_tr_t9, metric_gaming_dynamics, theater_ratio, 9, 0.81).

% Extraction over time
narrative_ontology:measurement(metgam_be_t0, metric_gaming_dynamics, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(metgam_be_t3, metric_gaming_dynamics, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(metgam_be_t6, metric_gaming_dynamics, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(metgam_be_t9, metric_gaming_dynamics, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(metric_gaming_dynamics, resource_allocation).
narrative_ontology:affects_constraint(metric_gaming_dynamics, organizational_misalignment).
narrative_ontology:affects_constraint(metric_gaming_dynamics, principal_agent_divergence).
narrative_ontology:affects_constraint(metric_gaming_dynamics, institutional_decay_piton_dynamics).

% DUAL FORMULATION NOTE:
% Metric gaming dynamics upstream of specific institutional failures (organizational misalignment, principal-agent problems, institutional decay). The constraint operates through measurement architecture and incentive design; downstream constraints represent specific manifestations of gaming in particular domains. Affects organizational performance measurement, performance-based compensation, and institutional transparency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(metric_gaming_dynamics, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
