% ============================================================================
% CONSTRAINT STORY: performance_metric_gaming
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_metric_gaming, []).

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
 *   constraint_id: performance_metric_gaming
 *   human_readable: Performance Metric Gaming
 *   domain: organizational_management/incentive_systems
 *
 * SUMMARY:
 *   Performance metric gaming is the structural phenomenon wherein agents
 *   optimize for measured performance indicators rather than the underlying
 *   objectives those metrics purport to represent. This constraint operates
 *   across organizations, sectors, and domains: healthcare providers
 *   optimizing for diagnosis-related group (DRG) reimbursement rather than
 *   patient outcomes; schools teaching to standardized tests rather than
 *   learning; sales teams cherry-picking easy deals to hit quota targets
 *   rather than building sustainable customer relationships; research groups
 *   pursuing metrics-friendly publications rather than scientific depth. The
 *   constraint exhibits tangled_rope structure at the organizational level:
 *   genuine coordination function (metrics do aggregate information about
 *   performance) combined with asymmetric extraction (agents who game metrics
 *   benefit at the cost of the underlying objective and those who cannot
 *   game). The theater_ratio (0.68) reflects that much of the performance
 *   review apparatus is performative ritual: the annual review meeting, the
 *   numerical rating system, and the comparative ranking format persist
 *   despite documented gaming incentives, maintained through organizational
 *   inertia rather than functional value.
 *
 * KEY AGENTS:
 *   - Underlying Objective Function: Primary victim (powerless/trapped) — true goal (patient outcomes, quality, satisfaction) cannot organize or exit; bears cost of misoptimization
 *   - Metric Achievers: Primary beneficiary (moderate to powerful/mobile) — agents who excel at gaming metrics; capture raises, promotions, and bonuses during the gaming window
 *   - Mid-Level Managers: Secondary victim (moderate/constrained) — caught between gaming incentives imposed by upper management and awareness of misoptimization
 *   - Measurement System Designers: Secondary beneficiary (institutional/arbitrage) — professional authority and prestige maintained through metric systems; can exit by redesigning metrics
 *   - Organizational Reformers: Organized agents (organized/constrained) — OKR advocates, balanced scorecard implementers, stakeholder engagement programs building alternatives with sunset potential
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing Goodhart's Law as immutable when it is partly a design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_metric_gaming, 0.58).
domain_priors:suppression_score(performance_metric_gaming, 0.65).
domain_priors:theater_ratio(performance_metric_gaming, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_metric_gaming, extractiveness, 0.58).
narrative_ontology:constraint_metric(performance_metric_gaming, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(performance_metric_gaming, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_metric_gaming, tangled_rope).
narrative_ontology:human_readable(performance_metric_gaming, "Performance Metric Gaming").
narrative_ontology:topic_domain(performance_metric_gaming, "organizational_management/incentive_systems").

domain_priors:requires_active_enforcement(performance_metric_gaming).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_metric_gaming, metric_achievers).
narrative_ontology:constraint_beneficiary(performance_metric_gaming, measurement_system_designers).
narrative_ontology:constraint_victim(performance_metric_gaming, underlying_objective_function).
narrative_ontology:constraint_victim(performance_metric_gaming, non_optimizable_stakeholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERLYING OBJECTIVE (SNARE) — The true goal (patient outcomes, product quality, customer satisfaction, environmental health) cannot exit the constraint. It bears the full cost of metric gaming while the metric becomes the actual optimization target. Powerless and trapped — has no advocates and no alternative representation in the decision system.
constraint_indexing:constraint_classification(performance_metric_gaming, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-LEVEL MANAGER (TANGLED ROPE) — Experiences genuine coordination problem (metrics do provide some signal about performance) alongside extraction (pressure to game metrics to meet targets that may be misaligned with actual value creation). Constrained by performance review systems and career advancement requirements. Mixed experience: benefits from having clear targets, but those targets incentivize gaming.
constraint_indexing:constraint_classification(performance_metric_gaming, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEASUREMENT SYSTEM DESIGNER (ROPE) — Benefits from the constraint as a coordination mechanism: metrics do aggregate complex information into actionable targets. Can exit (design better metrics, incorporate feedback loops) and often experiences the metric system as a solution to coordination problems rather than as extraction. Net beneficiary through professional prestige and authority.
constraint_indexing:constraint_classification(performance_metric_gaming, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZATIONAL REFORMERS (SCAFFOLD) — Organized agents (OKRs, balanced scorecard advocates, stakeholder feedback systems) see metric gaming as a temporary coordination failure with a sunset. Multi-dimensional metrics, outcome-based rather than activity-based measurement, and participatory goal-setting reduce gaming incentives. Estimated sunset: 10-15 years as practices mature in forward-thinking organizations.
constraint_indexing:constraint_classification(performance_metric_gaming, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY PERFORMANCE REVIEW (PITON) — Annual performance reviews, fixed KPIs, and numerical rankings persist through organizational inertia despite widespread recognition that they incentivize gaming. The ritual continues (theater_ratio = 0.68) because no fully-formed alternative has yet been institutionalized across sectors. Institutions see their own processes as degraded but cannot exit without coordination on replacements.
constraint_indexing:constraint_classification(performance_metric_gaming, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, metric gaming appears as an immutable consequence of Goodhart's Law: 'When a measure becomes a target, it ceases to be a good measure.' This perspective sees metric gaming as an inherent structural feature of any measurement system, not a contingent institutional problem. However, this naturalizes what is partly a design choice — organizations can reduce gaming through multi-metric approaches, outcome vs activity focus, and participatory metric design.
constraint_indexing:constraint_classification(performance_metric_gaming, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_metric_gaming_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(performance_metric_gaming, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(performance_metric_gaming, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_metric_gaming, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(performance_metric_gaming, TR),
    TR >= 0.70.

:- end_tests(performance_metric_gaming_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts value from the underlying objective to the metric achievers. This is not as severe as pure extraction (ε > 0.66) because metrics do provide some legitimate signal about performance — they are not pure theater. But the extractiveness is substantial because gaming creates a real divergence between metric performance and actual value creation. Over the interval, extractiveness increased from 0.32 to 0.58 as organizations added more metrics (proliferation hypothesis: each metric individually makes sense, but their collective burden increases gaming opportunity) and as gaming techniques became more sophisticated. Suppression (0.65): High. Multiple barriers prevent agents from exiting metric gaming: career advancement is contingent on hitting numerical targets; compensation is tied to metric performance; managers face termination if their teams miss metrics regardless of underlying outcomes. But suppression is not absolute (0.90+) because some agents can exit by moving to organizations with less metric-driven cultures or by building reputation outside metric systems. Theater ratio (0.68): High and increasing. Annual performance reviews are largely performative — the numerical ratings, the forced curve, and the calibration meetings consume significant organizational energy but produce little actionable signal beyond what continuous observation would reveal. The theater has increased over the interval as metric systems became more elaborate and as awareness of gaming incentives grew without corresponding structural reform.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals a profound perspectival divergence. The measurement system designer sees coordination (Rope) — metrics genuinely help align behavior with organizational goals. The organizational reformer sees a temporary problem with solutions emerging (Scaffold) — OKRs, multidimensional metrics, and participatory goal-setting reduce gaming incentives. The mid-level manager sees mixed coordination and extraction (Tangled Rope) — metrics are useful but also perverse. The metric achiever (if they acknowledge gaming at all) minimizes it as rational optimization. The underlying objective sees pure extraction (Snare) — its interests are systematically subordinated to metric performance. The civilizational observer risks seeing an immutable law (Mountain) — Goodhart's Law appears to make gaming inevitable. The engine's false summit detector flags this: Goodhart's Law is a design consequence, not a law of nature, and organizations that implement multi-metric systems with outcome focus and participatory design show substantially lower gaming than those relying on single-metric, activity-based, top-down targets.
 *
 * DIRECTIONALITY LOGIC:
 *   The directional value (d) derives from each agent's structural relationship to the constraint. Metric achievers are net beneficiaries with mobile exit (they can leave organizations or find gaming-friendly roles) — low d. Mid-level managers are victims with constrained exit (high cost to leave, some benefit from coordination function) — moderate-to-high d. The underlying objective function is a victim with zero exit capacity (abstract collective) — maximum d. Measurement system designers are beneficiaries with arbitrage options (professional authority, can redesign systems) — low d. The piton classification for legacy review systems derives from high theater, not from beneficiary status: the institutional actor maintains the system through inertia, sees it as degraded, yet cannot exit without coordination on alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that metric gaming is a genuine tangled_rope: the coordination function (aggregating information about performance) is real and valuable, AND the extraction (misoptimization of the underlying objective) is real and substantial. Neither can be eliminated without losing the other in naive formulations. However, the constraint's extractiveness and suppression can be substantially reduced through design choices: multi-metric systems reduce the targeting surface; outcome-based metrics reduce gaming opportunity; participatory metric design incorporates feedback from those who understand the underlying objective; and sunset-oriented reform (legacy metrics phased out as new systems prove themselves) enables organizations to escape the trap. This maps cleanly to the scaffold classification for organizational reformers: the constraint has a genuine sunset, because alternatives that reduce gaming while maintaining coordination function are demonstrably feasible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    goodharts_law_contingency,
    'Is Goodhart''s Law an immutable mathematical truth or a contingent result of how metrics are implemented and incentivized?',
    'Comparative analysis of high-gaming vs low-gaming organizations with similar metric types; identify which design features (multi-metric, outcome-based, participatory) correlate with reduced gaming',
    'If immutable law: metric gaming is mountain-class inevitable. If contingent: gaming is snare-class extractive design choice and can be substantially reduced through better practices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(goodharts_law_contingency, empirical, 'Whether Goodhart''s Law is immutable or contingent on design choices').

omega_variable(
    objective_function_specification,
    'Can the underlying objective (patient outcomes, product quality, customer satisfaction) be specified with sufficient precision that a metric system can capture it without gaming?',
    'Domain-by-domain analysis: healthcare, education, manufacturing, customer service. Identify what fraction of true objective variance is captured by current best-practice metrics.',
    'If objective is unspecifiable: some gaming is structural and unavoidable (mountain shift). If specifiable: gaming is design failure and should be classified as snare (extractive constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objective_function_specification, empirical, 'Whether underlying objectives can be sufficiently specified for metrics to capture them').

omega_variable(
    coordination_vs_extraction_boundary,
    'At what point does metric coordination become metric extraction? Where is the boundary between legitimate performance incentives and gaming-driven distortion?',
    'Threshold analysis: measure the divergence between metric performance and underlying objective across organizations. Where does the curve inflect from ''alignment improving'' to ''gaming accelerating''?',
    'If boundary is sharp: can classify constraints as clearly rope or snare. If boundary is diffuse: tangled_rope classification confirms the hybrid nature (coordination + extraction simultaneously).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'The boundary between legitimate performance coordination and gaming-driven extraction').

omega_variable(
    temporal_gaming_accumulation,
    'Does metric gaming accelerate over time (ratchet effect) or stabilize at an equilibrium level?',
    'Longitudinal measurement: track gaming intensity (proxy: divergence between metric performance and underlying outcome) over 5+ year periods in the same organizations',
    'If accelerates: gaming is extractive rent-seeking with compounding cost (snare classification strengthened). If stabilizes: gaming may be a negotiated equilibrium or coordination mechanism (tangled_rope or rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_gaming_accumulation, empirical, 'Whether metric gaming accelerates or stabilizes over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_metric_gaming, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pmg_tr_t0, performance_metric_gaming, theater_ratio, 0, 0.45).
narrative_ontology:measurement(pmg_tr_t3, performance_metric_gaming, theater_ratio, 3, 0.55).
narrative_ontology:measurement(pmg_tr_t6, performance_metric_gaming, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(pmg_be_t0, performance_metric_gaming, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pmg_be_t3, performance_metric_gaming, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(pmg_be_t6, performance_metric_gaming, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_metric_gaming, resource_allocation).
narrative_ontology:affects_constraint(performance_metric_gaming, goodharts_law_generalization).
narrative_ontology:affects_constraint(performance_metric_gaming, organizational_goal_misalignment).
narrative_ontology:affects_constraint(performance_metric_gaming, standardized_test_teaching_to_test).

% DUAL FORMULATION NOTE:
% Performance metric gaming decomposes into domain-specific constraints with varying ε values. A healthcare system gaming DRG metrics for reimbursement (ε ≈ 0.65, Snare) is structurally distinct from a school gaming standardized test scores (ε ≈ 0.55, Tangled Rope with strong coordination component). These are linked as a constraint family through their common causal mechanism (metric-driven optimization) but have different base properties because the underlying objectives differ in specificity and the exit options differ in cost.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_metric_gaming, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
