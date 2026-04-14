% ============================================================================
% CONSTRAINT STORY: goodharts_law_metric_divergence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_goodharts_law_metric_divergence, []).

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
 *   constraint_id: goodharts_law_metric_divergence
 *   human_readable: Goodhart's Law: Metric Divergence and Institutional Capture
 *   domain: institutional_dynamics/epistemic_constraints
 *
 * SUMMARY:
 *   Goodhart's Law — 'when a measure becomes a target, it ceases to be a good
 *   measure' — describes a structural divergence between metrics and outcomes
 *   that emerges systematically in institutions where performance is
 *   evaluated against quantified targets. The constraint exhibits all six DR
 *   types from different structural positions, making it a diagnostic
 *   exemplar for how measurement systems become extractive under
 *   institutional pressure. From the powerless perspective of the actual
 *   outcome target (student learning, patient health, environmental quality),
 *   the divergence is a snare: the measured quantity decouples from reality
 *   with no escape. From the line-level operator's perspective (teacher,
 *   nurse, administrator), it is tangled rope: genuine coordination through
 *   shared metrics alongside gaming pressure. From the metric administrator's
 *   perspective, it is rope: coordination solved, accountability achieved.
 *   From organized measurement reformers, it is scaffold: a temporary problem
 *   being solved by multi-dimensional frameworks. From the legacy metric
 *   system's perspective, it is piton: a degraded ritual maintained through
 *   institutional inertia. From the analytical observer, it risks appearing
 *   as a natural law — an immutable mathematical consequence of optimization
 *   dynamics — but the structural data reveals contingent institutional
 *   choices underneath the naturalization.
 *
 * KEY AGENTS:
 *   - Actual Outcome Target: Primary victim (powerless/trapped) — student learning, patient health, environmental quality cannot escape metric divergence; bears full cost with no coordination benefit
 *   - Line-Level Operators: Secondary victims (moderate/constrained) — teachers, nurses, social workers face gaming pressure and measurement theater; also benefit from metric coordination
 *   - Metric Administrators: Primary beneficiaries (institutional/arbitrage) — extract visibility and control from metric systems; experience coordination mechanism
 *   - Institutional Leadership: Secondary beneficiaries (powerful/mobile) — metrics provide accountability theater and decision-making legitimacy
 *   - Measurement Reform Coalition: Organized agents (organized/constrained) — data scientists, regulators, epistemologists building multi-dimensional alternatives
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices as laws of information theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(goodharts_law_metric_divergence, 0.58).
domain_priors:suppression_score(goodharts_law_metric_divergence, 0.62).
domain_priors:theater_ratio(goodharts_law_metric_divergence, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(goodharts_law_metric_divergence, extractiveness, 0.58).
narrative_ontology:constraint_metric(goodharts_law_metric_divergence, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(goodharts_law_metric_divergence, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(goodharts_law_metric_divergence, tangled_rope).
narrative_ontology:human_readable(goodharts_law_metric_divergence, "Goodhart's Law: Metric Divergence and Institutional Capture").
narrative_ontology:topic_domain(goodharts_law_metric_divergence, "institutional_dynamics/epistemic_constraints").

domain_priors:requires_active_enforcement(goodharts_law_metric_divergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(goodharts_law_metric_divergence, metric_gaming_agents).
narrative_ontology:constraint_beneficiary(goodharts_law_metric_divergence, institutional_administrators).
narrative_ontology:constraint_victim(goodharts_law_metric_divergence, actual_outcome_targets).
narrative_ontology:constraint_victim(goodharts_law_metric_divergence, measurement_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACTUAL OUTCOME TARGET (SNARE) — What the metric was designed to measure cannot escape the constraint. The real-world phenomenon (student learning, patient health, environmental quality) is increasingly decoupled from the metric's signal. Maximum extraction without coordination benefit — the target bears the cost of metric divergence with no exit option.
constraint_indexing:constraint_classification(goodharts_law_metric_divergence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LINE-LEVEL OPERATOR (TANGLED ROPE) — Teachers, nurses, social workers face genuine coordination through metric frameworks (what to teach, what to measure, what to prioritize) alongside extraction pressure (gaming metrics, gaming targets, hours wasted on measurement theater). Constrained by performance evaluation but also benefits from clarity about institutional expectations. Mixed experience: some coordination function, significant extraction.
constraint_indexing:constraint_classification(goodharts_law_metric_divergence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: METRIC ADMINISTRATOR (ROPE) — Experiences the metric system as pure coordination: tracking outcomes, aggregating data, reporting results, adjusting targets. Solves the legitimate problem of institutional accountability. Net beneficiary through reduced ambiguity and standardized reporting — extraction flows toward this agent, not away from them.
constraint_indexing:constraint_classification(goodharts_law_metric_divergence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MEASUREMENT REFORM COALITION (SCAFFOLD) — Organized agents (auditors, regulators, data scientists, epistemologists) see metric divergence as a temporary structural problem with a sunset: multi-dimensional frameworks (balanced scorecards, outcome triangulation, qualitative integration) are building alternatives to single-metric optimization. The reform has a clear exit pathway — as measurement methodology improves, the pressure to game narrows.
constraint_indexing:constraint_classification(goodharts_law_metric_divergence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY METRIC SYSTEM (PITON) — Once-functional measurement frameworks (standardized testing, performance evaluation metrics, KPI dashboards) have become largely performative. Institutions maintain them through inertia: they provide the appearance of accountability and systematic management, but their actual predictive value has degraded. Theater ratio (0.68) reflects this — much institutional effort goes to maintaining the measurement ritual rather than using measurements to improve outcomes.
constraint_indexing:constraint_classification(goodharts_law_metric_divergence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, Goodhart's Law appears to be a mathematical inevitability: any metric selected for optimization will be gamed because optimization pressure always induces divergence between the metric and the underlying phenomenon. The law is presented as immutable — a property of information systems and incentive structures, not contingent on institutional design. However, the structural data contradicts this naturalization. The constraint requires active enforcement (gaming must be deterred or shaped), beneficiaries exist (metric administrators benefit from the divergence), and alternatives exist (multi-dimensional frameworks reduce gaming pressure). The 'natural law' framing obscures contingent institutional choices.
constraint_indexing:constraint_classification(goodharts_law_metric_divergence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(goodharts_law_metric_divergence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(goodharts_law_metric_divergence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(goodharts_law_metric_divergence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(goodharts_law_metric_divergence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(goodharts_law_metric_divergence, TR),
    TR >= 0.70.

:- end_tests(goodharts_law_metric_divergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, and accelerating. At inception (t=0, ε=0.28), metric systems serve genuine coordination — clarity about targets, standardized measurement, comparable reporting. Over time (t=10, ε=0.58), extractiveness increases as gaming pressure mounts and the metric's gap from reality widens. Agents optimize the metric, pulling resources away from actual outcomes. The trajectory reflects systematic degradation: each round of tightened targets increases gaming incentives. Suppression (0.62): Moderate-high. Barriers to escape include: performance evaluation tied to metrics (career consequences), institutional accountability requirements (legal/regulatory obligation), measurement infrastructure investment (switching costs). But suppression is not total — some organizations successfully implement outcome-focused alternatives (balanced scorecards, qualitative integration). Theater ratio (0.68): High and growing. Measurement collection, reporting, and explanation consume substantial institutional effort. Line-level operators spend time on metric documentation and gaming rather than on actual outcomes. The theater increases as metrics diverge: more gaming visible, more corrective measures attempted, more ritual performed to manage perceived failure.
 *
 * PERSPECTIVAL GAP:
 *   The measurement administrator sees a functional system (rope): metrics provide clarity, standardization, comparability. The outcome target sees a broken system (snare): the metrics have decoupled from reality and there is no escape. The line-level operator sees a mixed system (tangled rope): the metrics do provide real coordination guidance, but gaming pressure also extracts time and compromises actual outcomes. The measurement reform coalition sees a problem with a solution (scaffold): alternatives exist, they are being adopted, the constraint has a sunset. The legacy system sees its own degraded ritual (piton): metrics persist through inertia and accountability theater, not because they are working. The civilizational observer risks seeing a natural law (mountain) — optimization always induces divergence — but this naturalizes what is actually a design choice: whether to accept divergence as inevitable or to invest in outcome triangulation. The perspectival gap is not observational but structural: different agents genuinely experience different constraints depending on their position in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is computed from the agent's structural relationship to the divergence mechanism. Metric administrators occupy the position d ≈ 0.15 (institutional beneficiaries with arbitrage options) — they benefit from metric systems without bearing gaming costs. Line-level operators occupy d ≈ 0.62 (moderate agents facing constrained exit) — they must game metrics to meet targets but cannot escape the system entirely. The outcome target occupies d ≈ 0.95 (powerless agents with trapped exit) — maximum extraction, no coordination benefit. The organized reform coalition occupies d ≈ 0.45 (organized agents with constrained exit and visible alternative pathways). These d values produce the perspectival gap: beneficiaries experience rope, targets experience snare, observers experience different types based on their structural position. The unified base properties (ε=0.58, suppression=0.62) mask the fact that experienced extractiveness (χ) varies by perspective according to f(d) × σ(S).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH STRUCTURAL DECOMPOSITION: Goodhart's Law is often presented as either a mathematical truth (immutable constraint on all measurement systems) or a myth (well-managed metrics do not diverge). The DR framework resolves this by showing that both claims are true from different perspectives. The natural law claim (mountain) is the false summit — it obscures contingent institutional choices. The myth claim misses the real snare-like dynamic for the outcome target. The resolution: Goodhart's Law describes a real structural constraint (tangled rope) that emerges when institutional incentives prioritize metric optimization over outcome optimization. The law is not immutable — it is contingent on the incentive structure. The constraint can be remapped: outcome-focused incentives reduce divergence; multi-dimensional measurement frameworks distribute gaming; qualitative integration triangulates metrics against reality. The measurement reform coalition is not aspirational — it is an actual structural alternative with lower extractiveness. The mandatrophy dissolves when we stop asking 'is Goodhart's Law true?' and start asking 'who benefits from treating metric divergence as inevitable, and who bears the cost?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_threshold,
    'At what point does metric optimization become gaming rather than legitimate performance improvement?',
    'Distinction between optimizing-the-outcome (good performance) and optimizing-the-metric (gaming). Observable: does improved metric score correlate with improved underlying outcome? If correlation collapses, classification shifts from rope to snare.',
    'If threshold is high (strict separation): most metric systems are performing legitimately, Goodhart''s Law is false. If threshold is low (metric and outcome diverge quickly): Goodhart''s Law is a snare on the outcome target, not a coordination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentionality_threshold, empirical, 'Threshold for distinguishing performance improvement from metric gaming').

omega_variable(
    metric_lag_causality,
    'Does metric divergence occur because metrics are inherently incomplete (fundamental epistemic limit) or because institutional incentives create gaming pressure (contingent design choice)?',
    'Historical analysis: do metrics diverge faster in high-stakes accountability systems vs low-stakes information systems? Do the same metrics diverge at different rates across different institutional cultures?',
    'If fundamental limit: mountain classification is correct. If contingent incentive: constraint is tangled_rope or snare, and alternatives exist (lower-stakes metrics, multi-dimensional frameworks, outcome triangulation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_lag_causality, conceptual, 'Whether metric divergence is fundamental or contingent on institutional design').

omega_variable(
    measurement_integrity_recovery,
    'Can measurement integrity be restored once gaming has become institutionalized, or does the divergence become path-dependent (self-reinforcing)?',
    'Case studies of institutional metric system reforms: do reformed metrics show renewed correlation with outcomes, or do new metrics immediately begin diverging? Time-to-divergence comparison across successive measurement framework iterations.',
    'If recoverable: scaffold perspective confirmed — reform coalitions can rebuild functional measurement. If path-dependent: the constraint exhibits snare-like stickiness even after metric replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_integrity_recovery, empirical, 'Whether measurement integrity can be restored after gaming institutionalizes').

omega_variable(
    multi_dimensional_sufficiency,
    'Do multi-dimensional measurement frameworks (balanced scorecards, outcome triangulation) actually reduce gaming pressure, or do they simply distribute gaming across more metrics?',
    'Comparison of gaming patterns in single-metric systems vs multi-dimensional systems; analysis of whether time-to-divergence increases or resource-cost-of-gaming increases with dimensionality.',
    'If sufficiency confirmed: scaffold sunset is real — reform pathways reduce extractiveness. If gaming merely distributes: reformers are chasing a mirage, and the constraint remains structurally snare-like at the outcome target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_dimensional_sufficiency, empirical, 'Whether multi-dimensional frameworks reduce or redistribute gaming pressure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(goodharts_law_metric_divergence, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(goodhart_tr_t0, goodharts_law_metric_divergence, theater_ratio, 0, 0.32).
narrative_ontology:measurement(goodhart_tr_t5, goodharts_law_metric_divergence, theater_ratio, 5, 0.5).
narrative_ontology:measurement(goodhart_tr_t10, goodharts_law_metric_divergence, theater_ratio, 10, 0.68).
narrative_ontology:measurement(goodhart_tr_t2, goodharts_law_metric_divergence, theater_ratio, 2, 0.42).

% Extraction over time
narrative_ontology:measurement(goodhart_be_t0, goodharts_law_metric_divergence, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(goodhart_be_t5, goodharts_law_metric_divergence, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(goodhart_be_t10, goodharts_law_metric_divergence, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(goodhart_be_t2, goodharts_law_metric_divergence, base_extractiveness, 2, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(goodharts_law_metric_divergence, information_standard).
narrative_ontology:affects_constraint(goodharts_law_metric_divergence, metric_gaming_incentive_structure).
narrative_ontology:affects_constraint(goodharts_law_metric_divergence, outcome_measurement_triangulation).
narrative_ontology:affects_constraint(goodharts_law_metric_divergence, accountability_theater_maintenance).

% DUAL FORMULATION NOTE:
% Goodhart's Law describes a general structural pattern (metric-outcome divergence under optimization pressure) that manifests as distinct constraints in specific institutional domains. The upstream constraint is the incentive structure that rewards metric optimization over outcome optimization. The downstream constraints are specific metric systems (testing regimes, performance evaluations, KPI dashboards) where divergence is observed. This story captures the general pattern; domain-specific stories decompose into particular metrics, institutions, and reform pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(goodharts_law_metric_divergence, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
