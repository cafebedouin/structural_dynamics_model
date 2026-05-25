% ============================================================================
% CONSTRAINT STORY: goodhart_metric_substitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_goodhart_metric_substitution, []).

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
 *   constraint_id: goodhart_metric_substitution
 *   human_readable: Goodhart's Law: Metric Substitution and Goal Displacement
 *   domain: organizational_governance/measurement_systems
 *
 * SUMMARY:
 *   Goodhart's Law — 'When a measure becomes a target, it ceases to be a good
 *   measure' — represents a structural tension in governance systems that
 *   rely on quantified metrics to align principal and agent. The constraint
 *   manifests when organizations establish a proxy metric to measure progress
 *   toward a goal (student test scores as a measure of educational quality;
 *   readmission rates as a measure of healthcare quality; financial turnover
 *   as a measure of business health). Once the metric becomes the evaluation
 *   criterion, agents rationally optimize for the metric rather than the
 *   underlying goal, producing metric substitution and goal displacement. The
 *   constraint exhibits all six DR types from different perspectives. The
 *   original objective (student learning, patient health, environmental
 *   restoration) becomes powerless and trapped once the metric takes
 *   governance primacy. Local agents face genuine coordination requirements
 *   but constrained choices — they must demonstrate measurable performance.
 *   Measurement gatekeepers experience the metric system as pure coordination
 *   infrastructure. Reform coalitions see temporary design problems with
 *   sunset pathways. Degraded performance management systems persist through
 *   institutional inertia. The analytical observer risks naturalizing
 *   contingent institutional design as an immutable law. Theater ratio
 *   progression (0.32 → 0.68) reflects that performance management systems
 *   become increasingly performative as the gap between metric and goal
 *   widens: gaming becomes more visible and elaborate, and justification
 *   narratives proliferate. The extractiveness trajectory (0.28 → 0.58) shows
 *   how the constraint intensifies over time as metric optimization becomes
 *   institutionalized and competing measurement systems are suppressed.
 *
 * KEY AGENTS:
 *   - Original Objective Bearer: Primary victim (powerless/trapped) — the actual goal (learning, health, environmental integrity) loses enforcement once measurement substitutes for it
 *   - Local Agent/Metric Optimizer: Secondary victim and partial beneficiary (moderate/constrained) — faces evaluation against the metric and rationally optimizes; bears costs through knowledge of divergence from true goal
 *   - Measurement Gatekeeper: Primary beneficiary (institutional/arbitrage) — audit departments, evaluation commissions, regulatory bodies that control metric definitions and governance
 *   - Measurement Reform Coalition: Organized agents (organized/mobile) — multi-metric advocates, data science communities, outcome measurement researchers building alternative systems
 *   - Performance Management Ritual: Institutional actor (institutional/arbitrage) — the entire apparatus of performance evaluation, KPIs, and accountability reporting that persists through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating Goodhart's Law as a natural law rather than as a contingent organizational design failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(goodhart_metric_substitution, 0.58).
domain_priors:suppression_score(goodhart_metric_substitution, 0.62).
domain_priors:theater_ratio(goodhart_metric_substitution, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(goodhart_metric_substitution, extractiveness, 0.58).
narrative_ontology:constraint_metric(goodhart_metric_substitution, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(goodhart_metric_substitution, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(goodhart_metric_substitution, tangled_rope).
narrative_ontology:human_readable(goodhart_metric_substitution, "Goodhart's Law: Metric Substitution and Goal Displacement").
narrative_ontology:topic_domain(goodhart_metric_substitution, "organizational_governance/measurement_systems").

domain_priors:requires_active_enforcement(goodhart_metric_substitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(goodhart_metric_substitution, metric_optimizers).
narrative_ontology:constraint_beneficiary(goodhart_metric_substitution, measurement_gatekeepers).
narrative_ontology:constraint_victim(goodhart_metric_substitution, original_objective_bearer).
narrative_ontology:constraint_victim(goodhart_metric_substitution, system_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORIGINAL OBJECTIVE (SNARE) — The actual system goal (patient health, student learning, environmental quality) becomes invisible once measurement governance shifts to proxy metrics. The original objective has no enforcement mechanism and cannot coordinate its own defense. Trapped agents bearing the cost of metric gaming have no exit from systems where the metric is mandated.
constraint_indexing:constraint_classification(goodhart_metric_substitution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL AGENT / METRIC GAMER (TANGLED ROPE) — Faces genuine coordination problem (must demonstrate performance against some measurable criterion) but the metric is a poor proxy for actual goal. Constrained by evaluation requirements and career risk, but retains some discretion in how to optimize the metric. Benefits from metric substitution via improved evaluation scores; bears costs through knowledge that the optimization is hollow.
constraint_indexing:constraint_classification(goodhart_metric_substitution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MEASUREMENT GATEKEEPER (ROPE) — Institutional actor (audit department, evaluation commission, regulatory body) experiences the metric system as pure coordination: it creates alignment between principal and agent via observable proxy. The gatekeeper benefits from metric clarity and governance authority. Extraction runs toward the gatekeeper through increased institutional relevance and control.
constraint_indexing:constraint_classification(goodhart_metric_substitution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEASUREMENT REFORM COALITION (SCAFFOLD) — Organized agents (data science communities, multi-metric frameworks, outcome measurement advocates) see metric substitution as a temporary coordination failure being solved by pluralistic measurement systems, outcome bundles, and real-time feedback mechanisms. Mobile exit path: transition from single-metric governance to multi-dimensional outcome tracking with sunset: 10-15 years for mature frameworks.
constraint_indexing:constraint_classification(goodhart_metric_substitution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PERFORMANCE MANAGEMENT RITUAL (PITON) — Organizational performance evaluation systems persist largely through institutional inertia despite widespread recognition that single metrics corrupt behavior. The ritual persists because the alternative (judgment without measurement) threatens accountability doctrine. Theater ratio of 0.68 reflects that much performance management is ceremonial compliance theater rather than functional governance.
constraint_indexing:constraint_classification(goodhart_metric_substitution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational distance, Goodhart's Law appears as an immutable principle of measurement: any metric chosen as a target becomes a poor measure of what it was chosen to measure. This perspective risks naturalizing what is actually a contingent institutional failure — treating metric substitution as inherent to governance rather than as a solvable design problem.
constraint_indexing:constraint_classification(goodhart_metric_substitution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(goodhart_metric_substitution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(goodhart_metric_substitution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(goodhart_metric_substitution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(goodhart_metric_substitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(goodhart_metric_substitution, TR),
    TR >= 0.70.

:- end_tests(goodhart_metric_substitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant value from the original objective bearer through metric substitution, but the extraction is not absolute because some agents (measurement reformers, aware local agents) maintain partial capacity to pursue true goals alongside metric optimization. Suppression (0.62): High. Structural barriers to pursuing the true goal include: metric-based evaluation governance that penalizes non-conformity, career risk for agents who prioritize goal over metric, organizational momentum around metric systems, and epistemic closure (the metric becomes 'what we're measuring for'). However, suppression is not total — some organizations do maintain plural objectives and some agents do resist pure metric optimization. Theater ratio (0.68): High. Performance management systems are substantially performative. The metrics are presented as objective measures while everyone involved recognizes significant divergence from true goals. The ritual persists because explicit judgment-based evaluation threatens accountability narratives. The upward trajectory of theater ratio reflects increasing performativity as metric gaming becomes more visible and sophisticated. Claimed type Tangled Rope: The constraint has both genuine coordination function (metrics do enable principal-agent alignment around something measurable) and asymmetric extraction (the alignment is to the wrong objective). The presence of active enforcement (metric-based evaluation governance) and both beneficiaries and victims confirms the tangled rope classification.
 *
 * PERSPECTIVAL GAP:
 *   The measurement gatekeeper's rope classification depends on assuming the metric is a reasonable proxy for the goal. If the proxy divergence is large (revealed by comparing metric improvement to goal degradation), the rope dissolves into snare — the 'coordination' was illusory. Local agents' tangled rope classification depends on the metric having some positive correlation with the true goal; if correlation is zero or negative, the rope becomes snare. The analytical observer's mountain classification depends on treating metric substitution as an inherent feature of measurement; if substitution is a contingent design choice (revealed by cases where it's been prevented), the mountain becomes tangled rope. The perspectival gap is diagnostic: if all perspectives agree the constraint is rope or mountain, the metric system may be functional. If perspectives diverge (rope from gatekeeper, snare from objective bearer, scaffold from reformers), metric substitution is happening and goal displacement is occurring.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect agents' structural position relative to the metric substitution flow. Measurement gatekeepers benefit from metric authority and have arbitrage-level exit options — if metric governance fails, they can switch to different metrics, so they experience low d (beneficiary status + arbitrage exit). Local agents are constrained by evaluation requirements but retain some discretion — moderate power, constrained exit, mixed benefit/cost creates intermediate d. The original objective bearer has no exit from mandatory metrics and no coordination benefit — trapped status with victim designation produces high d. Organized reform coalitions have mobile exit (can build alternative measurement systems) and retain agency, so they experience moderate d despite victim status. The metrics themselves encode a directionality: what gets measured gets valued, what is not measured is deprioritized. The constraint's d-flow concentrates extraction toward the goal that can't advocate for itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing between the metric system as coordination mechanism (genuine but contingent) and metric substitution as extraction mechanism (pathological but avoidable). The constraint is not 'do metrics enable governance?' (yes, rope classification applies) but rather 'do metric-based governance systems create goal displacement?' (yes, tangled rope or snare classification applies when substitution occurs). The resolution requires analyzing whether: (a) the metric is sufficiently close to the true goal that optimization toward it serves the goal (Rope, or low-extractiveness Tangled Rope), (b) the metric diverges from the goal in ways that metric optimization exploits (Snare, or high-extractiveness Tangled Rope), or (c) multi-metric or outcome-bundle systems have been implemented that prevent substitution (Scaffold with sunset logic). The mandatrophy is not about the law itself (Goodhart's observation is accurate) but about the governance design choice to enforce single-metric systems, which is contingent and changeable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_proximity_to_goal,
    'How do we distinguish between a proxy metric that is genuinely close to the underlying goal versus one that only appears close before optimization pressure reveals divergence?',
    'Pre-optimization and post-optimization outcome comparison; analysis of cases where metric improved dramatically but goal degraded. Cross-validation against independent goal measurements.',
    'If most metrics have hidden divergence: extractiveness floor rises to 0.65+ (snare classification more common). If divergence is rare: extractiveness drops to 0.35 (rope classification more defensible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_proximity_to_goal, empirical, 'Metric proximity to actual goal before optimization pressure').

omega_variable(
    gaming_impossibility_vs_difficulty,
    'Is metric substitution inevitable (agents will always find ways to game any metric) or merely easy (current metrics are poorly designed)?',
    'Analysis of metric robustness properties; investigation of cases where multidimensional outcome bundles resist gaming; examination of whether adversarial metric design can prevent substitution.',
    'If inevitable: Goodhart''s Law is a mountain (extraction is impossible to prevent). If contingent: it is a tangled rope or scaffold (design and organizational change matter).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gaming_impossibility_vs_difficulty, conceptual, 'Whether metric substitution is inevitable or contingent on design').

omega_variable(
    governance_without_measurement_feasibility,
    'Can complex systems (healthcare, education, regulation) be governed effectively without quantified metrics, and if so, at what cost?',
    'Historical analysis of pre-metric governance systems; case studies of organizations that abandoned metrics for judgment-based evaluation; assessment of whether human judgment without measurement can scale.',
    'If feasible: single-metric systems are chosen (not imposed), and the constraint is opt-in coordination (Rope). If infeasible: measurement is structurally necessary, and gaming is the cost of scale (Snare becomes unavoidable).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(governance_without_measurement_feasibility, preference, 'Whether governance without quantified metrics is feasible at scale').

omega_variable(
    goal_specification_depth,
    'How much do agents know about the true underlying objective before metric substitution occurs? Is substitution a discovery process (revealing what we actually care about) or pure corruption?',
    'Analysis of whether metric gaming reveals preferences that were implicit in goal statements; investigation of whether optimization pressure produces ''revealed preferences'' that stakeholders endorse upon reflection.',
    'If metric gaming reveals previously implicit preferences: substitution has coordination value (Tangled Rope confirmed). If gaming is purely destructive: substitution is extractive (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(goal_specification_depth, empirical, 'Whether agents understood the goal specification before metric substitution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(goodhart_metric_substitution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(goodhart_tr_t0, goodhart_metric_substitution, theater_ratio, 0, 0.32).
narrative_ontology:measurement(goodhart_tr_t5, goodhart_metric_substitution, theater_ratio, 5, 0.55).
narrative_ontology:measurement(goodhart_tr_t10, goodhart_metric_substitution, theater_ratio, 10, 0.68).
narrative_ontology:measurement(goodhart_tr_t2, goodhart_metric_substitution, theater_ratio, 2, 0.41).

% Extraction over time
narrative_ontology:measurement(goodhart_be_t0, goodhart_metric_substitution, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(goodhart_be_t5, goodhart_metric_substitution, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(goodhart_be_t10, goodhart_metric_substitution, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(goodhart_be_t2, goodhart_metric_substitution, base_extractiveness, 2, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(goodhart_metric_substitution, resource_allocation).
narrative_ontology:affects_constraint(goodhart_metric_substitution, goal_displacement_institutional_capture).
narrative_ontology:affects_constraint(goodhart_metric_substitution, measurement_gaming_financial_systems).
narrative_ontology:affects_constraint(goodhart_metric_substitution, educational_metrics_substitution).

% DUAL FORMULATION NOTE:
% Goodhart's Law itself is a mountain (measurement systems do produce substitution under optimization pressure). However, metric substitution as an institutional constraint is a tangled rope or snare (contingent on governance design choices). The family includes domain-specific stories (educational metrics, healthcare metrics, financial metrics) each with their own extractiveness values reflecting domain-specific metric-goal divergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(goodhart_metric_substitution, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
