% ============================================================================
% CONSTRAINT STORY: metric_fixation_goodhart
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_metric_fixation_goodhart, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: metric_fixation_goodhart
 *   human_readable: Metric Fixation and Goodhart's Law
 *   domain: organizational_governance/measurement_systems
 *
 * SUMMARY:
 *   Metric fixation — the subordination of actual outcomes to the metrics
 *   designed to measure them — represents one of the most pervasive and
 *   structurally resistant constraints in modern organizations. Goodhart's
 *   Law ('When a measure becomes a target, it ceases to be a good measure')
 *   describes the mechanism: introducing a metric creates an incentive to
 *   optimize it rather than the underlying outcome it was meant to track.
 *   This constraint exhibits snare characteristics because the primary
 *   victims (frontline workers, end users, systems themselves) have no
 *   structural exit, high suppression (compliance is enforced through
 *   funding, employment, accreditation), and suffer extraction in the form of
 *   diverted effort and degraded outcomes. Yet it also appears as rope
 *   (coordination mechanism) from the metric administrator's perspective,
 *   scaffold (reformable system) from organized advocates' perspective, and
 *   piton (degraded ritual) from legacy institutions. The constraint's
 *   theater ratio has increased dramatically over 20 years as metric
 *   compliance has become increasingly divorced from actual measurement of
 *   what matters. Metric administrators face pressure to introduce
 *   accountability mechanisms, metrics provide an illusion of control and
 *   comparability, gaming becomes endemic, outcomes degrade, metrics are
 *   adjusted or replaced, and the cycle repeats.
 *
 * KEY AGENTS:
 *   - Metric Administrators: Institutional beneficiaries (institutional/arbitrage) — control metric design and interpretation; capture coordination authority through metrics; experience metrics as alignment tool
 *   - Frontline Workers: Primary victims (powerless/trapped) — teachers, physicians, social workers bearing direct extraction cost through compliance overhead and forced gaming
 *   - End Users / Affected Populations: Secondary victims (moderate/constrained) — patients, students, citizens harmed by metric optimization rather than outcome optimization
 *   - Reformist Coalition: Organized agents (organized/constrained) — measurement scholars, open-science advocates, outcome-aligned practitioners building alternative accountability mechanisms
 *   - High-Status Professionals: Mobile partial resisters (powerful/mobile) — sufficient autonomy and reputational capital to opt partially out of metrics while maintaining nominal compliance
 *   - Legacy Institutional Leadership: Piton maintainers (institutional/arbitrage) — recognize metrics are broken but maintain them through inertia; cannot justify switching cost despite visibility into failure
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees metric fixation as structural feature of any measurable-outcome system, not contingent institutional failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(metric_fixation_goodhart, 0.58).
domain_priors:suppression_score(metric_fixation_goodhart, 0.65).
domain_priors:theater_ratio(metric_fixation_goodhart, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(metric_fixation_goodhart, extractiveness, 0.58).
narrative_ontology:constraint_metric(metric_fixation_goodhart, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(metric_fixation_goodhart, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(metric_fixation_goodhart, snare).
narrative_ontology:human_readable(metric_fixation_goodhart, "Metric Fixation and Goodhart's Law").
narrative_ontology:topic_domain(metric_fixation_goodhart, "organizational_governance/measurement_systems").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(metric_fixation_goodhart, metric_administrators).
narrative_ontology:constraint_beneficiary(metric_fixation_goodhart, political_actors_controlling_metrics).
narrative_ontology:constraint_victim(metric_fixation_goodhart, actual_outcome_quality).
narrative_ontology:constraint_victim(metric_fixation_goodhart, local_decision_makers).
narrative_ontology:constraint_victim(metric_fixation_goodhart, end_users_of_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE WORKER (SNARE) — Teachers, nurses, police officers, and case managers face metric targets that are externally imposed, structurally misaligned with their actual work, and inescapable. Exit from the role means losing livelihood. Exit from the metric system is not available — compliance is enforced through performance evaluation and funding allocation. The worker experiences pure extraction: time and cognitive effort diverted from actual client welfare to metric gaming.
constraint_indexing:constraint_classification(metric_fixation_goodhart, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: END USER / AFFECTED POPULATION (SNARE) — Patients in hospitals optimizing for wait-time metrics, students in schools optimized for test scores, citizens in cities optimized for crime statistics. The metric distortion directly harms them — they face worse medical outcomes, weaker learning, or actual crime that metrics claim to have prevented. Exit is constrained: must use the public system; cannot easily switch providers or jurisdictions. Bears the cost of metric gaming without control or visibility into the measurement system.
constraint_indexing:constraint_classification(metric_fixation_goodhart, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: METRIC ADMINISTRATOR (ROPE) — Central authorities (governments, corporate headquarters, accreditation bodies) experience metrics as a coordination mechanism: a way to align distributed agents toward stated objectives. The administrator has arbitrage — can change the metrics, can exit the measurement system entirely, can introduce new reporting schemes. Extraction runs toward this agent (resources flow upward as compliance overhead), but the mechanism is perceived as coordination because the administrator controls the frame.
constraint_indexing:constraint_classification(metric_fixation_goodhart, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORMIST COALITION (SCAFFOLD) — Measurement scholars (Muller, Ioannidis, Wilkinson), open-science advocates, and outcome-aligned practitioners see metric fixation as a temporary institutional failure with a visible sunset. The coalition is building alternative accountability mechanisms: thick evaluation, participatory metrics, outcome transparency, and evidence hierarchies that distinguish correlation from causation. The scaffold classification derives from the organized agents having both (a) a real coordination function (learning what actually works) and (b) a sunset clause (better measurement science replaces metric theater within a generation).
constraint_indexing:constraint_classification(metric_fixation_goodhart, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY ACCOUNTABILITY SYSTEM (PITON) — Organizational leaders and institutional authority actors maintain metric-based accountability through inertia despite widespread recognition that the system is broken. They can see the gaming (Goodhart's Law in action) and the harm, but the cost of switching to alternative accountability mechanisms (thick evaluation, outcome tracking, trust-based management) is perceived as high. The system persists because changing it is harder than maintaining it, not because it functions. Theater ratio is high: compliance theater substitutes for actual accountability.
constraint_indexing:constraint_classification(metric_fixation_goodhart, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HIGH-STATUS PROFESSIONAL (TANGLED ROPE) — Senior physicians, university faculty, and established practitioners have enough reputational capital and autonomy to opt partially out of metric compliance. They can ignore metrics, game them selectively, or maintain both metric performance and actual quality. They experience the constraint as coordination (metrics provide accountability) with asymmetric extraction they can moderate. Mobile exit options allow them to preserve quality while nominally complying — a mixed position.
constraint_indexing:constraint_classification(metric_fixation_goodhart, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, the constraint is the fundamental misalignment between measurable and actual outcomes. Metrics select for what can be quantified (speed, volume, binary success/failure) and against what matters (depth, quality, actual impact). The observer sees that Goodhart's Law is not a temporary institutional problem but a structural feature of any system that privileges measurement over meaning. The dynamics are: metrics are introduced to align incentives → agents optimize the metrics → metrics become decoupled from outcomes → system collapses or loses function → metrics are replaced → cycle repeats. This is snare because no agent has control over the fundamental measurement-meaning gap.
constraint_indexing:constraint_classification(metric_fixation_goodhart, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(metric_fixation_goodhart_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(metric_fixation_goodhart, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(metric_fixation_goodhart, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(metric_fixation_goodhart, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(metric_fixation_goodhart, TR),
    TR >= 0.70.

:- end_tests(metric_fixation_goodhart_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint extracts significant value: administrator authority, compliance overhead, legitimacy theater, and avoidance of harder accountability methods. Frontline workers spend 20-40% of time on metric compliance (varies by sector). End users experience outcome degradation quantifiable as reduced service quality, longer wait times despite shorter wait-time metrics, worse learning outcomes despite higher test scores, and higher actual crime despite lower reported crime. However, extractiveness is not at the maximum (0.72+) because some metric correlation with outcomes persists, and some agents (high-status professionals) can partially escape. Suppression (0.65): High. Frontline workers cannot opt out without losing employment. End users cannot switch systems easily. Metric requirements are enforced through funding allocation, performance evaluation, and regulatory compliance. Alternatives (outcome-based accountability, trust-based management, thick evaluation) are perceived as risky by administrators and unfamiliar to policymakers. Theater ratio (0.78): High and increasing. Metric compliance becomes increasingly divorced from actual measurement of outcomes. Time spent on metrics does not correlate with outcome improvement. Compliance theater substitutes for real accountability. The measurement ritual persists because stakeholders have become invested in its maintenance, not because it functions.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits stark perspectival divergence across power levels and exit options. Metric administrators perceive coordination and legitimacy; frontline workers perceive coercion and distortion. High-status professionals can afford partial evasion; powerless workers cannot. The end-user victim perspective (constrained/moderate) is structurally invisible to administrators (institutional/arbitrage) because user harm is not reported through the metric system — it appears only in outcome data that contradicts the metrics. Reformists see a solvable problem with sunset mechanisms; legacy institutions see inertial maintenance. The analytical observer risks naturalizing the constraint as inherent to any measurement system, but the structural data reveals that the constraint is not inevitable — it arises from specific design choices (top-down metric imposition, enforcement through performance evaluation, disconnection from actual outcome tracking) and can be addressed through alternative institutional designs (participatory metrics, transparent outcome data, thick evaluation, outcome-aligned incentives). The perspectival gap is maximal between the powerless/trapped victim and the institutional/arbitrage administrator.
 *
 * DIRECTIONALITY LOGIC:
 *   Metric administrators extract coordination authority and legitimacy from their control over metrics. Their d value is low (beneficiary position with arbitrage exit) — they perceive the constraint as enabling their work and can change it if they choose. Frontline workers face trapped exit (cannot leave without employment loss) and victim status (metric gaming diverts them from actual work) — their d value is high (0.90+), producing maximum experienced extractiveness. End users are constrained (cannot easily exit the system) and victimized (outcome degradation harms them directly) — their d value is moderate-high (0.70-0.80). High-status professionals have mobile exit options (can opt out or selectively game metrics due to reputational capital) and mixed status (both beneficiary and victim depending on the specific metric) — their d value is moderate (0.45-0.55). Reformists are organized (can mobilize alternative systems) and constrained (building new systems is difficult and slow) — their d value is moderate-low (0.35-0.45). The analytical observer sees the constraint as universal (applies to all measurable-outcome systems) and participatory (observer is embedded in institutions using metrics) — d value is moderate (0.70, canonical for analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is classified as snare because the core structural feature is extraction with minimal coordination benefit. While metric administrators perceive coordination (rope), the actual mechanism is coercion and gaming — agents optimize the metric rather than the outcome. The snare classification is validated by: (1) high suppression (exit is not available to frontline workers and end users), (2) high base extractiveness (20-40% of frontline worker time diverted to compliance), (3) outcome degradation (gaming is causal to actual outcome decline), (4) minimal coordination benefit (metrics diverge from outcomes as gaming accumulates). The rope perspective from administrators is mislabeled due to information asymmetry — they do not see the gaming and outcome degradation. The scaffold perspective from reformists is aspirational but structurally sound — alternative measurement systems are being built and have sunset clauses (as new systems mature, metric-based accountability can be phased out). The piton perspective from legacy institutions is accurate — the system persists through inertia, not function. The snare classification is stable across time horizons and power levels, except for high-status professionals (powerful/mobile) who experience tangled rope due to their ability to partially escape. The mandatrophy is resolved by recognizing that this is a snare from most perspectives, rope only from the beneficiary/administrator perspective due to information asymmetry, and that the reformist coalition's scaffold approach can transform it into genuine coordination if alternative metrics mature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    outcome_measurability_ceiling,
    'Are the most important outcomes inherently unmeasurable, or is the failure to measure them a contingent limitation of current methods?',
    'Longitudinal case studies comparing metric-optimized and metric-free systems (e.g., Waldorf vs conventional schools, relationship-based medicine vs protocolized care, trust-based vs surveillance-heavy organizations) on long-term outcomes; interviews with practitioners on what they perceive as real vs measured success',
    'If inherent: metric fixation is an unsolvable design problem (true snare for all perspectives). If contingent: better measurement science could resolve the constraint into rope (coordination without degradation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outcome_measurability_ceiling, empirical, 'Whether important outcomes are inherently unmeasurable or contingently hard to measure').

omega_variable(
    incentive_realignment_threshold,
    'What proportion of a system must actually optimize the metric (vs gaming it) before metric corruption exceeds coordination benefit?',
    'Cross-organizational analysis: comparison of gaming rates to actual outcome drift; threshold identification below which metrics still track outcomes, above which metrics diverge from outcomes',
    'If threshold is high (>70% compliance): metrics can remain useful even with significant gaming. If threshold is low (<30%): metrics become actively harmful early in the gaming process.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incentive_realignment_threshold, empirical, 'Gaming threshold at which metrics stop tracking outcomes').

omega_variable(
    metric_administrator_capture,
    'Do metric administrators become captured by the metrics they designed — losing ability to recognize metric corruption because they have professional and political identity tied to the system?',
    'Interviews and decision-history analysis of administrators who changed or abolished metrics; documentation of cognitive resistance to evidence of gaming; tracking of administrator career outcomes tied to metric persistence vs reform',
    'If high capture: the metric administrators are themselves trapped/identity_locked despite having arbitrage options structurally. The rope perspective is mislabeled; administrator is actually constrained/identity_locked. If low capture: administrators genuinely choose metrics as coordination tools and can change them rationally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metric_administrator_capture, empirical, 'Whether metric administrators become captured by the systems they design').

omega_variable(
    outcome_quality_correlation,
    'In high-metric-fixation systems, is the observed outcome degradation causally driven by metric gaming or explained by other institutional changes (staffing, funding cuts, policy shifts)?',
    'Causal analysis using synthetic control methods, difference-in-differences estimation, or controlled rollout experiments where metrics are introduced/removed in matched jurisdictions; separation of metric effects from concurrent institutional changes',
    'If gaming is causal: metric fixation is the snare (core problem). If confounded with other changes: outcome degradation may be driven by underlying institutional failure, not metrics. Classification remains snare but etiology changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outcome_quality_correlation, empirical, 'Causal link between metric gaming and outcome degradation').

omega_variable(
    metric_reform_sustainability,
    'When organizations introduce participatory or outcome-aligned metrics, do they avoid Goodhart''s Law drift, or do the new metrics repeat the same corruption cycle?',
    'Longitudinal tracking of organizations that have reformed from traditional metrics to alternatives (thick evaluation, balanced scorecards, outcome hierarchies); documentation of whether the new metrics also degrade over time or maintain alignment with outcomes',
    'If new metrics avoid degradation: the constraint is solvable via better measurement design (scaffold thesis correct). If new metrics repeat the cycle: metric fixation is a deeper structural problem than measurement choice (snare thesis correct).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_reform_sustainability, empirical, 'Whether reformed metric systems avoid Goodhart''s Law degradation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(metric_fixation_goodhart, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(metfix_tr_t0, metric_fixation_goodhart, theater_ratio, 0, 0.35).
narrative_ontology:measurement(metfix_tr_t10, metric_fixation_goodhart, theater_ratio, 10, 0.58).
narrative_ontology:measurement(metfix_tr_t20, metric_fixation_goodhart, theater_ratio, 20, 0.78).
narrative_ontology:measurement(metfix_tr_t5, metric_fixation_goodhart, theater_ratio, 5, 0.46).

% Extraction over time
narrative_ontology:measurement(metfix_be_t0, metric_fixation_goodhart, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(metfix_be_t10, metric_fixation_goodhart, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(metfix_be_t20, metric_fixation_goodhart, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(metfix_be_t5, metric_fixation_goodhart, base_extractiveness, 5, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(metric_fixation_goodhart, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(metric_fixation_goodhart, 0.18).
narrative_ontology:affects_constraint(metric_fixation_goodhart, teacher_performance_pay).
narrative_ontology:affects_constraint(metric_fixation_goodhart, hospital_readmission_penalties).
narrative_ontology:affects_constraint(metric_fixation_goodhart, police_arrest_quotas).
narrative_ontology:affects_constraint(metric_fixation_goodhart, academic_citation_metrics).

% DUAL FORMULATION NOTE:
% Metric fixation is a meta-constraint that affects specific measurement systems in distinct domains. The upstream constraint is the institutional need for accountability mechanisms (coordination problem). The downstream constraints are domain-specific metric systems (teacher evaluation, hospital performance, etc.) that instantiate metric fixation with different ε values. This family should be decomposed: accountability_coordination (ε≈0.15, rope) upstream, with domain-specific metric_fixation variants (ε=0.40-0.65) downstream, linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(metric_fixation_goodhart, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
