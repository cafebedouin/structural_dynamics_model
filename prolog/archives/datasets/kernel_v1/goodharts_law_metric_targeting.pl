% ============================================================================
% CONSTRAINT STORY: goodharts_law_metric_targeting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_goodharts_law_metric_targeting, []).

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
 *   constraint_id: goodharts_law_metric_targeting
 *   human_readable: Goodhart's Law: Metric Targeting and Goal Substitution
 *   domain: institutional_management/measurement_systems
 *
 * SUMMARY:
 *   Goodhart's Law describes a structural constraint that emerges when
 *   institutions rely on centralized performance metrics to coordinate
 *   activity. The law states that 'when a measure becomes a target, it ceases
 *   to be a good measure' — agents optimize for the metric rather than the
 *   underlying goal the metric was intended to track. This constraint
 *   manifests across six distinct classifications depending on the observer's
 *   structural position, time horizon, and relationship to the metric system.
 *   The constraint combines genuine coordination functions (the need for
 *   distributed agents to align on measurable objectives) with extractive
 *   mechanisms (agents gaming metrics to achieve measured performance while
 *   actual goals diverge). The theater ratio of 0.65 reflects that
 *   metric-based management creates substantial performative overhead:
 *   performance review rituals, metric dashboards, compliance reporting all
 *   consume organizational energy. Over the 10-year interval, both theater
 *   ratio and base extractiveness have increased as metric systems have
 *   become more sophisticated and more tightly coupled to career advancement
 *   and compensation. The suppression requirement has also risen as
 *   organizations enforce metric compliance through stronger accountability
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - Honest Performers: Primary victims (powerless/trapped) — career advancement requires metric optimization; genuine work in unmeasured dimensions unrewarded
 *   - Metric Optimizers: Primary beneficiaries (powerful/arbitrage) — exploit gap between metric and goal; have agency to shift between organizations with different metrics
 *   - Pragmatic Managers: Secondary agents (moderate/constrained) — balance genuine coordination with metric reporting; suffer from divergence but have modest escape options
 *   - Measurement Reform Movement: Organized agents (organized/constrained) — building alternative frameworks (OKRs, balanced scorecards, participatory assessment) with explicit sunset logic
 *   - Bureaucratic Compliance Apparatus: Institutional actor (institutional/arbitrage) — enforcement machinery for metric systems; sees own function as degraded but persists through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing metric optimization incentives as inevitable laws of rational agency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(goodharts_law_metric_targeting, 0.58).
domain_priors:suppression_score(goodharts_law_metric_targeting, 0.48).
domain_priors:theater_ratio(goodharts_law_metric_targeting, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(goodharts_law_metric_targeting, extractiveness, 0.58).
narrative_ontology:constraint_metric(goodharts_law_metric_targeting, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(goodharts_law_metric_targeting, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(goodharts_law_metric_targeting, tangled_rope).
narrative_ontology:human_readable(goodharts_law_metric_targeting, "Goodhart's Law: Metric Targeting and Goal Substitution").
narrative_ontology:topic_domain(goodharts_law_metric_targeting, "institutional_management/measurement_systems").

domain_priors:requires_active_enforcement(goodharts_law_metric_targeting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(goodharts_law_metric_targeting, metric_gaming_agents).
narrative_ontology:constraint_beneficiary(goodharts_law_metric_targeting, institutional_administrators).
narrative_ontology:constraint_victim(goodharts_law_metric_targeting, actual_goal_achievement).
narrative_ontology:constraint_victim(goodharts_law_metric_targeting, unmetricized_dimensions).
narrative_ontology:constraint_victim(goodharts_law_metric_targeting, principled_performers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HONEST PERFORMER (SNARE) — Trapped between genuine goal pursuit and metric optimization. Career advancement requires metric targets; genuine work unmeasured. No exit: refusing to game metrics leads to career stagnation. Maximum extraction — the performer bears the cost of metric-goal divergence while beneficiaries capture rewards.
constraint_indexing:constraint_classification(goodharts_law_metric_targeting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: METRIC OPTIMIZER (ROPE) — Powerful agents (middle management, gaming specialists) see pure coordination: the system coordinates around metric targets. They benefit from arbitrage — exploiting the gap between metric and goal. Low suppression from their view: they have substantial agency and can exit by shifting to institutions with different metrics.
constraint_indexing:constraint_classification(goodharts_law_metric_targeting, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRAGMATIC MANAGER (TANGLED ROPE) — Constrained by institutional hierarchy and budget authority. Coordinates genuine work (genuine goal alignment, team development) alongside metric reporting. Benefits from clearer accountability metrics; suffers from the divergence between measured and unmeasured performance. Modest escape costs exist (changing employers, sectors) but are significant.
constraint_indexing:constraint_classification(goodharts_law_metric_targeting, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MEASUREMENT REFORM MOVEMENT (SCAFFOLD) — Organized agents (OKRs, balanced scorecards, stakeholder engagement, participatory budgeting) view Goodhart as a temporary measurement failure with architectural sunset. Alternative frameworks (qualitative assessment, multi-metric dashboards, goal transparency) are building replacements for blunt numerical targeting. Theater low because reforms create genuine alternatives, not just ritual substitution.
constraint_indexing:constraint_classification(goodharts_law_metric_targeting, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BUREAUCRATIC COMPLIANCE APPARATUS (PITON) — The institutional machinery of metric enforcement (audit departments, performance reviews, scorecards) persists through inertia despite widespread recognition that metrics diverge from goals. The apparatus sees its own function as degraded: everyone acknowledges 'we're measuring the wrong thing,' yet the structure remains. High theater ratio (0.65): compliance rituals (performance review meetings, metric dashboards) consume organizational energy with minimal actual goal alignment.
constraint_indexing:constraint_classification(goodharts_law_metric_targeting, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NECESSITY VIEW (MOUNTAIN) — From a universal perspective, Goodhart's Law appears to be an immutable principle: any metric used for feedback will be gamed because agents have incentives to optimize observed performance regardless of metric design. The constraint seems to follow logically from information asymmetry and agent rationality. However, the presence of identifiable beneficiaries (metric_gaming_agents, administrators who use metrics to manage) and alternative measurement systems that achieve higher goal alignment reveals this as a false summit: the 'inevitability' of metric gaming naturalizes institutional choices about which metrics to use and how to enforce them.
constraint_indexing:constraint_classification(goodharts_law_metric_targeting, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(goodharts_law_metric_targeting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(goodharts_law_metric_targeting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(goodharts_law_metric_targeting, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(goodharts_law_metric_targeting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(goodharts_law_metric_targeting, TR),
    TR >= 0.70.

:- end_tests(goodharts_law_metric_targeting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint creates systematic incentive misalignment — agents are rewarded for metric performance regardless of goal achievement. However, this is not maximal extraction because some metric optimization is aligned with actual goals, and some organizations achieve relatively high goal-metric correlation. The extractiveness has grown over the interval from 0.32 to 0.58 as metric systems have expanded in scope and enforcement has intensified. Suppression (0.48): Moderate. Honest performers face significant barriers to exit — career penalties for refusing to optimize metrics, social pressure to align, institutional structures that make metric-independent goal pursuit invisible. However, suppression is not total because some performers can migrate to less metric-intensive organizations or create alternative careers where genuine work is rewarded. Measurement reform alternatives are reducing suppression by providing visible pathways to non-gaming organizational cultures. Theater ratio (0.65): Moderate-high. Metric systems create substantial performative activity: dashboard maintenance, performance review meetings, compliance documentation, target-setting rituals. The rise from 0.35 to 0.65 over the interval reflects increasing sophistication of metric enforcement machinery without corresponding increase in actual goal alignment.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival spectrum. The honest performer, trapped and powerless, experiences pure extraction (Snare) — they bear the cost of metric-goal divergence while reaping no benefit. The metric optimizer, powerful and mobile, experiences pure coordination (Rope) — they solve the problem of aligning distributed agents around targets. The pragmatic manager, constrained but moderate, experiences mixed coordination and extraction (Tangled Rope) — they both genuinely coordinate work and extract value from metric gaps. The measurement reform movement, organized with constrained options, sees this as a temporary architectural problem (Scaffold) — alternative systems are building a sunset path. The bureaucratic apparatus, institutional and immobile, sees its own degradation (Piton) — everyone knows metrics diverge from goals, yet the ritual persists. The civilizational analytical observer risks seeing necessity (Mountain) — all rational agents optimize for observed performance — but the beneficiary declarations reveal this as naturalization: metric_gaming_agents and institutional_administrators actively benefit from the current system, which contradicts the natural-law claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position and relationship to the metric-goal divergence. Honest performers are victims with no exit (trapped) — they experience high d (~0.95) because they bear extraction costs and cannot leave. Metric optimizers are beneficiaries with full exit (arbitrage) — they experience low d (~0.10) because they benefit and can exit. Pragmatic managers are mixed (moderate victims/beneficiaries with constrained exit) — they experience moderate d (~0.55) because they both benefit (clearer accountability, management tools) and suffer (unmeasured work devalued). The reform coalition, organized with some exit capacity, experiences low-moderate d (~0.40) because they have agency to build alternatives. The compliance apparatus, institutional with arbitrage, experiences very low d (~0.05) because they benefit from metric enforcement. The analytical observer without commitment, analyzing the structure, derives d from observer position (~0.72) but is identity-locked into naturalizing the system — their analysis produces a false summit classification that the engine should flag.
 *
 * MANDATROPHY ANALYSIS:
 *   Goodhart's Law resolves the mandatrophy by acknowledging that all six classifications coexist from different structural positions. The constraint is simultaneously: a natural law for observers who assume rational agent optimization under information asymmetry (Mountain); an extractive mechanism for trapped performers (Snare); a coordination mechanism for optimizers (Rope); a mixed coordination-extraction system for pragmatic managers (Tangled Rope); a degraded institution for enforcement apparatus (Piton); and a temporary architectural problem for reformers (Scaffold). The false summit appears at the analytical/civilizational level, where the observer's universalizing perspective risks naturalizing institutional choices. The resolution is recognizing that Goodhart is a meta-level constraint: the law describes what happens when metric optimization becomes the dominant institutional logic, but the logic itself is chosen by institutions and can be redesigned. The honest performer's Snare is not inevitable — it emerges from choices about which metrics to use, how to enforce them, and whether to devalue unmeasured work. The Scaffold perspective demonstrates that alternative institutional logics reduce Goodhart effects. The mountain classification fails because Goodhart's Law itself (the meta-claim about metric behavior) is used to justify the institutional choices that produce it: a self-fulfilling prophecy that benefits metric_gaming_agents and institutional_administrators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_gaming_boundary,
    'What threshold distinguishes legitimate metric optimization (efficient pursuit of stated goals) from pathological gaming (metric-goal decoupling)?',
    'Outcome analysis: if metric improvement correlates with actual goal achievement, optimization is legitimate. If metric improvement anti-correlates with goal achievement, gaming is pathological. Time lag analysis of gaming emergence relative to metric introduction.',
    'If legitimate optimization boundary exists: Goodhart is a coordination problem (Rope/Tangled Rope). If all metric-targeting is inherently gaming: Goodhart is an extraction mechanism (Snare). If boundary is fuzzy/contextual: classification depends on institutional intent and goal transparency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_gaming_boundary, empirical, 'Boundary between legitimate optimization and pathological metric gaming').

omega_variable(
    goal_commensurateness,
    'Are there structural categories of goals that resist metric capture? If so, what organizational designs protect unmeasured goals from displacement?',
    'Comparative analysis of organizations with high vs low metric-goal divergence. Correlation between goal type (output-measurable vs process-dependent vs relational) and gaming prevalence. Study of organizations with mixed quantitative/qualitative assessment systems.',
    'If some goals are intrinsically non-metricizable: Goodhart is a structural limit on centralized coordination (Mountain). If all goals can be measured but aren''t: Goodhart is an institutional choice (Tangled Rope/Snare). If goals are commensurable but measuring them creates perverse incentives: Goodhart is a design problem (Scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(goal_commensurateness, empirical, 'Whether some goal categories resist metric capture').

omega_variable(
    alternative_system_sufficiency,
    'Do alternative measurement systems (OKRs, balanced scorecards, participatory assessment, narrative evaluation) actually reduce Goodhart effects or merely displace gaming to different metrics?',
    'Longitudinal studies of organizations transitioning to alternative frameworks. Measurement of goal-metric divergence before/after system change. Documentation of gaming migration (does gaming shift from financial metrics to strategic metrics, or does it decline overall?).',
    'If alternatives work: Scaffold perspective is confirmed — institutional sunset is achievable via measurement redesign. If gaming persists: Goodhart is a deeper structural feature (Mountain or Snare). If gaming shifts: Scaffold is aspirational but requires continuous metric rotation (sustains Piton theater indefinitely).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_system_sufficiency, empirical, 'Whether alternative measurement systems reduce Goodhart effects').

omega_variable(
    institutional_beneficiary_intentionality,
    'Is metric-goal divergence primarily a side effect of well-intentioned measurement design, or does it actively benefit administrators and creates alignment incentives for metric gaming?',
    'Historical analysis of metric system origins: were goals set before metrics (goal-derived measurement) or metrics set before goals (metrics-first design)? Interview analysis of administrator decision-making about metric scope and enforcement. Comparison of organizations where administrators are evaluated on goal achievement vs metric targets.',
    'If side effect: Goodhart is primarily a coordination failure (Tangled Rope, fixable via redesign). If actively beneficial to administrators: Goodhart is extractive by design (Snare, requires institutional restructuring to resolve).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_beneficiary_intentionality, empirical, 'Whether metric-goal divergence benefits administrators intentionally').

omega_variable(
    false_summit_natural_law_claim,
    'Is Goodhart''s Law a natural law of rational agent behavior under information asymmetry, or does it describe a contingent institutional arrangement that could be prevented through structural redesign?',
    'Comparative analysis of organizations with low Goodhart effects (high goal-metric alignment, low gaming prevalence). Identification of structural features enabling this alignment: goal transparency, multi-dimensional evaluation, agent participation in metric design, long time horizons for assessment, mixed quantitative/qualitative feedback. If multiple such organizations exist with sustained low gaming, Goodhart is not a natural law.',
    'If natural law: Mountain classification confirmed. If contingent: False summit — the analytical observer is naturalizing institutional choices. The engine''s false summit detector should flag metric_gaming_agents and institutional_administrators as beneficiaries, revealing that beneficiary presence contradicts the natural-law claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether Goodhart is a natural law or contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(goodharts_law_metric_targeting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(goodhart_tr_t0, goodharts_law_metric_targeting, theater_ratio, 0, 0.35).
narrative_ontology:measurement(goodhart_tr_t3, goodharts_law_metric_targeting, theater_ratio, 3, 0.48).
narrative_ontology:measurement(goodhart_tr_t6, goodharts_law_metric_targeting, theater_ratio, 6, 0.6).
narrative_ontology:measurement(goodhart_tr_t9, goodharts_law_metric_targeting, theater_ratio, 9, 0.65).

% Extraction over time
narrative_ontology:measurement(goodhart_be_t0, goodharts_law_metric_targeting, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(goodhart_be_t3, goodharts_law_metric_targeting, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(goodhart_be_t6, goodharts_law_metric_targeting, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(goodhart_be_t9, goodharts_law_metric_targeting, base_extractiveness, 9, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(goodhart_su_t0, goodharts_law_metric_targeting, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(goodhart_su_t5, goodharts_law_metric_targeting, suppression_requirement, 5, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(goodharts_law_metric_targeting, resource_allocation).
narrative_ontology:affects_constraint(goodharts_law_metric_targeting, measurement_gaming_in_healthcare).
narrative_ontology:affects_constraint(goodharts_law_metric_targeting, target_culture_in_public_sector).
narrative_ontology:affects_constraint(goodharts_law_metric_targeting, quota_systems_in_sales).
narrative_ontology:affects_constraint(goodharts_law_metric_targeting, standardized_testing_alignment).

% DUAL FORMULATION NOTE:
% Goodhart's Law is a meta-constraint: it describes the family of metric-gaming constraints across domains (healthcare, education, public sector, corporate). Each domain instantiation (measurement_gaming_in_healthcare, etc.) is a separate constraint story with domain-specific beneficiaries, victims, and institutional arrangements. They are all downstream of the same structural principle but have different epsilon values reflecting domain-specific factors: medical measurement systems have different failure modes than educational testing systems. Goodhart's Law as stated here is the abstract structural principle; the domain stories are its instantiations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(goodharts_law_metric_targeting, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
