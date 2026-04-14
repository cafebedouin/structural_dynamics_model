% ============================================================================
% CONSTRAINT STORY: goodhart_metric_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_goodhart_metric_collapse, []).

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
 *   constraint_id: goodhart_metric_collapse
 *   human_readable: Goodhart's Law: Metric Collapse and Perverse Incentives
 *   domain: institutional_governance/measurement_systems
 *
 * SUMMARY:
 *   Goodhart's Law — 'When a measure becomes a target, it ceases to be a good
 *   measure' — describes a structural collapse that occurs when institutions
 *   impose quantitative metrics as incentive targets. The constraint exhibits
 *   high theater_ratio (0.78) and moderate-high extractiveness (0.58)
 *   reflecting a system where performative compliance with metrics displaces
 *   substantive outcome improvement. The collapse mechanism is bidirectional:
 *   (1) agents optimize the measured dimension at expense of unmeasured
 *   dimensions; (2) the metric's predictive validity decays as a side effect
 *   of optimization, becoming a target of gaming rather than an indicator of
 *   underlying quality. The constraint is tangled_rope rather than pure snare
 *   because genuine coordination functions exist (metrics do enable
 *   comparison and delegation), but they are entangled with extractive
 *   mechanisms (practitioners lose autonomy, ground truth distorts, gaming
 *   labor replaces productive work). Theater ratio has risen from 0.42 to
 *   0.78 over the interval, indicating accelerating metric degradation — the
 *   measurement system has become increasingly performative as the gap
 *   between measured performance and actual outcomes widens.
 *
 * KEY AGENTS:
 *   - System's Ground Truth (unmeasured outcome quality): Primary victim (powerless/trapped) — actual student learning, patient health, ecosystem quality degrade as measured metrics artificially improve
 *   - Field Practitioners (teachers, doctors, managers): Secondary victim (moderate/constrained) — face coercive optimization of metrics; labor redirected from productive work to gaming; professional autonomy constrained
 *   - Metric Owners (administrators, auditors, policy designers): Primary beneficiary (institutional/arbitrage) — gain measurement-based control authority; metrics enable delegation and comparison without understanding ground truth
 *   - Measurement Apparatus (formal evaluation systems): Institutional actor (institutional/constrained) — persists through inertia despite recognized collapse; theater increased as gaming accelerates
 *   - Measurement Reform Movement: Organized agents (organized/mobile) — building multi-metric and qualitative assessment alternatives with intentional sunset from single-metric systems
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing metric collapse as immutable law rather than contingent institutional outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(goodhart_metric_collapse, 0.58).
domain_priors:suppression_score(goodhart_metric_collapse, 0.65).
domain_priors:theater_ratio(goodhart_metric_collapse, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(goodhart_metric_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(goodhart_metric_collapse, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(goodhart_metric_collapse, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(goodhart_metric_collapse, tangled_rope).
narrative_ontology:human_readable(goodhart_metric_collapse, "Goodhart's Law: Metric Collapse and Perverse Incentives").
narrative_ontology:topic_domain(goodhart_metric_collapse, "institutional_governance/measurement_systems").

domain_priors:requires_active_enforcement(goodhart_metric_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(goodhart_metric_collapse, metric_managers).
narrative_ontology:constraint_beneficiary(goodhart_metric_collapse, performance_optimizers).
narrative_ontology:constraint_victim(goodhart_metric_collapse, system_integrity).
narrative_ontology:constraint_victim(goodhart_metric_collapse, outcome_quality).
narrative_ontology:constraint_victim(goodhart_metric_collapse, stakeholders_downstream).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYSTEM INTEGRITY (SNARE) — The actual outcome being measured (student learning, patient health, ecosystem quality) cannot exit the metric collapse. As measurement systems are imposed, ground truth becomes systematically distorted. The unmeasured dimensions of quality degrade while measured dimensions artificially improve. Maximum extraction from structural reality itself.
constraint_indexing:constraint_classification(goodhart_metric_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD PRACTITIONERS (TANGLED ROPE) — Teachers, doctors, managers constrained by career risk and institutional pressure to optimize metrics. They coordinate around legitimate outcomes (education, healing, productivity) but the metric system extracts from their judgment autonomy and forces labor reallocation to metric gaming. Significant coercion but also genuine coordination function.
constraint_indexing:constraint_classification(goodhart_metric_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: METRIC OWNERS (ROPE) — Administrators, auditors, and policy designers benefit from metric-based control. They experience the constraint as pure coordination: the metric enables delegation, comparison, and accountability. No perceived extraction from their position — the metric system generates authority and visibility.
constraint_indexing:constraint_classification(goodhart_metric_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MEASUREMENT APPARATUS (PITON) — The formal evaluation system (standardized tests, KPIs, audit protocols) persists despite widespread recognition that it has collapsed into theater. Administrators know the metrics no longer measure what they claim, but alternatives haven't fully replaced them. High theater_ratio (0.78) reflects performative compliance — optimizing for metrics that everyone knows are gamed.
constraint_indexing:constraint_classification(goodhart_metric_collapse, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MEASUREMENT REFORM MOVEMENT (SCAFFOLD) — Organized agents (holistic evaluation designers, mixed-methods researchers, participatory auditors) are building alternative assessment pathways with intentional sunset clauses. Shift toward qualitative + quantitative integration, outcome triangulation, and embedded practitioner judgment. These are temporary bridges away from single-metric collapse, with clear transition plans.
constraint_indexing:constraint_classification(goodhart_metric_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, Goodhart's Law appears to be an immutable constraint on measurement systems: whenever a metric becomes a target, it ceases to be a good metric (Campbell's Law corollary). This perspective treats metric collapse as a fundamental limitation of all incentive-based systems. However, this naturalizes what is structurally contingent — metric collapse requires both a power asymmetry (those who set metrics vs those measured) and suppression of feedback loops (inability to revise metrics in response to gaming).
constraint_indexing:constraint_classification(goodhart_metric_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(goodhart_metric_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(goodhart_metric_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(goodhart_metric_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(goodhart_metric_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(goodhart_metric_collapse, TR),
    TR >= 0.70.

:- end_tests(goodhart_metric_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The metric system extracts from practitioners' autonomy and from ground truth (unmeasured dimensions degrade). Extraction is not maximal because metric systems do solve real coordination problems — comparison and delegation do require some shared measurement. The extraction emerges as an unintended consequence of the coordination function, not as a primary design goal. Suppression (0.65): High. Practitioners cannot easily exit metric optimization — career advancement, funding, employment depend on metric performance. Feedback mechanisms that would reveal metric gaming are suppressed by the power structure (those who set metrics control information about metric validity). Theater ratio (0.78): High and rising. Formal audit and evaluation rituals persist despite widespread recognition that they are performative — everyone knows the metrics are gamed, but alternatives haven't fully replaced them. The high theater reflects that the system's primary function has become legitimacy theater rather than outcome measurement. The rising trajectory (0.42 → 0.78 over 20 time units) indicates accelerating metric degradation as gaming sophistication increases.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence exists between beneficiary (rope) and primary victim (snare) perspectives. Metric owners perceive pure coordination because the system transfers information and enables hierarchical control at low cost to them. Practitioners perceive coercive extraction because the constraint redirects their labor, constrains their judgment, and holds them accountable for unmeasured outcomes. System integrity (ground truth) perceives pure extraction with zero agency. The reform movement perceives a solvable problem (scaffold) because they have both power and exit options to implement alternatives. The analytical observer risks perceiving an immutable law (mountain) because Goodhart-type collapses have occurred across diverse domains (education, healthcare, finance, environmental management), suggesting a universal pattern. However, this naturalizes what is actually a recurring institutional structure: asymmetric measurement power + suppression of feedback loops + optimization incentives = metric collapse. Where these three conditions are absent, metric systems remain valid.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position relative to the metric collapse. Metric owners (beneficiaries with arbitrage options) experience low d → negative effective extraction (they benefit). Practitioners (victims with constrained exit) experience moderate-high d → moderate chi (they bear significant extraction but retain some autonomy). System integrity (victim with zero exit capacity) experiences maximum d → maximum experienced extraction (complete powerlessness). The measurement apparatus (institutional actor with performative function) occupies a paradoxical position: it is simultaneously victim (the system's own validity is collapsing) and beneficiary (its continued existence is justified by its coordination function). This is resolved through the piton classification — the apparatus is a degraded constraint sustained by theater rather than by genuine function. The reform movement's mobile exit status gives them lower d despite victim status — they have the option to implement alternatives, which reduces their experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is: Does the presence of genuine coordination functions (metrics do enable comparison and delegation) mean the constraint is coordination (rope) or does the presence of asymmetric extraction mean it is extraction (snare)? The tangled_rope classification resolves this by declaring both functions genuine. The constraint does coordinate — metrics are not arbitrary — but the coordination is entangled with extraction. The metric system solves the delegation problem (principal-agent) while creating a new extraction problem (metric gaming, outcome distortion, autonomy loss). The classification is tangled_rope rather than snare because (1) beneficiaries genuinely benefit from the coordination function, not just from extraction, (2) practitioners experience both coordination benefits (standardized comparison enables resource access) and extraction (optimization coercion), and (3) the system could be redesigned to maintain coordination while reducing extraction (multi-metric systems, feedback loops, practitioner input). The constraint is not immutable — the mountain perspective is a false summit that naturalizes the power asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalization_vs_contingency,
    'Is Goodhart''s Law an immutable property of measurement systems or a contingent outcome of specific institutional power asymmetries?',
    'Comparison of systems with symmetric vs asymmetric measurement power; analysis of measurement systems with strong feedback loops and practitioner input vs top-down metrics',
    'If immutable: mountain classification confirmed. If contingent: the constraint is tangled_rope or scaffold, and metric collapse is preventable through structural reforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_vs_contingency, conceptual, 'Whether metric collapse is natural law or institutional artifact').

omega_variable(
    feedback_loop_sufficiency,
    'Can continuous metric revision loops (rapid feedback, practitioner input, outcome validation) prevent or significantly delay metric collapse?',
    'Longitudinal study of systems with embedded feedback mechanisms vs static metrics; comparison of metric stability and validity over time',
    'If feedback prevents collapse: suppression value should drop to 0.35-0.45, reclassifying as pure Tangled Rope. If feedback merely delays collapse: current (0.65) suppression value confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_loop_sufficiency, empirical, 'Whether feedback mechanisms prevent metric collapse').

omega_variable(
    multi_dimensional_measurement_feasibility,
    'Is simultaneous measurement of multiple outcome dimensions (unmeasured alongside measured) structurally possible without creating a new Goodhart-prone composite metric?',
    'Field test of explicitly multi-metric systems with orthogonal measurement vectors; analysis of whether composite indices collapse faster than single metrics',
    'If feasible: alternative exists that bypasses the collapse mechanism entirely (restructuring towards rope from tangled_rope). If infeasible: the collapse is harder to escape than reform movements assume.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multi_dimensional_measurement_feasibility, empirical, 'Whether multi-dimensional measurement avoids Goodhart collapse').

omega_variable(
    practitioner_autonomy_interaction,
    'Does preserving meaningful practitioner autonomy in metric interpretation reduce gaming incentives, or do sufficiently powerful optimization forces override autonomy-preserving institutional structures?',
    'Comparative institutional analysis of systems with high vs low practitioner discretion; measurement of gaming rates and metric validity degradation',
    'If autonomy prevents gaming: suppression and extractiveness drop significantly (0.35-0.40 range), reclassifying as scaffold or rope. If gaming overrides autonomy: current tangled_rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(practitioner_autonomy_interaction, empirical, 'Whether practitioner autonomy prevents metric gaming').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(goodhart_metric_collapse, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(goodhart_tr_t0, goodhart_metric_collapse, theater_ratio, 0, 0.42).
narrative_ontology:measurement(goodhart_tr_t10, goodhart_metric_collapse, theater_ratio, 10, 0.62).
narrative_ontology:measurement(goodhart_tr_t20, goodhart_metric_collapse, theater_ratio, 20, 0.78).
narrative_ontology:measurement(goodhart_tr_t5, goodhart_metric_collapse, theater_ratio, 5, 0.52).
narrative_ontology:measurement(goodhart_tr_t15, goodhart_metric_collapse, theater_ratio, 15, 0.7).

% Extraction over time
narrative_ontology:measurement(goodhart_be_t0, goodhart_metric_collapse, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(goodhart_be_t10, goodhart_metric_collapse, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(goodhart_be_t20, goodhart_metric_collapse, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(goodhart_be_t5, goodhart_metric_collapse, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(goodhart_be_t15, goodhart_metric_collapse, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(goodhart_metric_collapse, information_standard).
narrative_ontology:affects_constraint(goodhart_metric_collapse, metric_gaming_labor_extraction).
narrative_ontology:affects_constraint(goodhart_metric_collapse, audit_theater_institutional_inertia).
narrative_ontology:affects_constraint(goodhart_metric_collapse, performative_compliance_systems).

% DUAL FORMULATION NOTE:
% Goodhart's Law describes a family of related constraints. The base constraint (goodhart_metric_collapse) covers the general mechanism. Downstream constraints include: (1) metric_gaming_labor_extraction — the specific redirection of productive labor toward gaming behavior, (2) audit_theater_institutional_inertia — the continuation of measurement systems after validity collapse, (3) performative_compliance_systems — the broader phenomenon of compliance theater that emerges when measurement systems are gamed. Each story has its own ε value reflecting different observable perspectives on the same institutional phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(goodhart_metric_collapse, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
