% ============================================================================
% CONSTRAINT STORY: incentive_surface_warping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incentive_surface_warping, []).

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
 *   constraint_id: incentive_surface_warping
 *   human_readable: The Perverse Optimization Trap (Goodhart's Law)
 *   domain: economic/organizational/technological
 *
 * SUMMARY:
 *   Goodhart's Law — 'when a measure becomes a target, it ceases to be a good
 *   measure' — describes a structural trap in incentive systems. A metric
 *   designed to coordinate behavior toward genuine objectives (Rope function)
 *   generates extractive dynamics when agents optimize for the proxy rather
 *   than the underlying goal. The constraint exhibits a temporal trajectory:
 *   initially, metrics provide real coordination value (low extractiveness,
 *   low theater). Over time, agents learn the measurement surface and
 *   optimize toward it, increasing both extractiveness (metric gaming becomes
 *   profitable) and theater ratio (performative optimization replaces
 *   substantive progress). The perverse optimization trap is not a bug in
 *   incentive design — it is an inherent feature of any system where
 *   measurable proxies diverge from true objectives. The constraint's
 *   evolution tracks the Goodhart lifecycle: coordinating mechanism →
 *   extraction pathway → legacy ritual (piton), depending on observer
 *   position and time scale.
 *
 * KEY AGENTS:
 *   - Metric Designers (Executive, Algorithm Creator): Institutional power, arbitrage exit — benefits from metric authority and simplified reporting. Primary beneficiaries of the constraint structure.
 *   - Frontline Operators (Teacher, Nurse, Analyst): Powerless, trapped — bear full cost of metric-objective divergence. No exit from measurement without career penalty. Primary victims.
 *   - System True Objectives: Abstract victim (immeasurable, unrepresented) — degraded by metric optimization as agents redirect effort toward proxies. Cannot advocate or organize.
 *   - Intermediate Managers: Moderate power, constrained exit — caught between upward metric reporting and downward team welfare. Enforcement overhead falls on this layer.
 *   - Systems Reformers (Unions, Professional Associations, Researchers): Organized agents, constrained but with long-term exit paths — building multi-metric and triangulation alternatives to replace single-proxy systems.
 *   - Legacy Accountability Rituals: Institutional theater — annual reviews, performance evaluations persist through inertia despite decoupling from actual decision-making.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incentive_surface_warping, 0.52).
domain_priors:suppression_score(incentive_surface_warping, 0.48).
domain_priors:theater_ratio(incentive_surface_warping, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incentive_surface_warping, extractiveness, 0.52).
narrative_ontology:constraint_metric(incentive_surface_warping, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(incentive_surface_warping, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incentive_surface_warping, tangled_rope).
narrative_ontology:human_readable(incentive_surface_warping, "The Perverse Optimization Trap (Goodhart's Law)").
narrative_ontology:topic_domain(incentive_surface_warping, "economic/organizational/technological").

domain_priors:requires_active_enforcement(incentive_surface_warping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incentive_surface_warping, metric_designers).
narrative_ontology:constraint_beneficiary(incentive_surface_warping, measurement_proxies_creators).
narrative_ontology:constraint_victim(incentive_surface_warping, system_actual_objectives).
narrative_ontology:constraint_victim(incentive_surface_warping, unincentivized_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE OPERATOR (SNARE) — Trapped in metric optimization loops. When bonuses and evaluations depend on measured outputs (test scores, patient wait times, code coverage), the operator faces asymmetric extraction: they must optimize for the proxy measure or lose income/job security, but the proxy often diverges from actual system objectives. No exit from the metric without career penalty. Maximum suppression — alternatives (pursuing true objectives at cost of metric penalties) are structurally unavailable.
constraint_indexing:constraint_classification(incentive_surface_warping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INTERMEDIATE MANAGER (TANGLED ROPE) — Constrained between upward metric reporting and downward team welfare. Benefits from metric targets (performance looks good, justifies budget allocation) but also bears costs when metric optimization produces perverse behaviors (teaching to the test, gaming wait-time statistics, cutting corners on unmeasured quality). Mixed extraction with real enforcement overhead — must actively monitor and adjust incentive structures.
constraint_indexing:constraint_classification(incentive_surface_warping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: METRIC DESIGNER (ROPE) — Benefits from the constraint structure through coordination and control. Metric designers experience the system as solving a genuine coordination problem: how to align many independent agents toward organizational goals without centralized oversight. They have arbitrage options (choice of metrics, adjustment frequency, measurement scope). Extraction flows toward them through simplified reporting and authority consolidation.
constraint_indexing:constraint_classification(incentive_surface_warping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SYSTEMS REFORMER COALITION (SCAFFOLD) — Organized groups (labor unions, professional associations, organizational behavior researchers) view the perverse optimization trap as a temporary coordination failure with a known sunset. Multi-metric approaches (balanced scorecards), triangulation methods (combining multiple imperfect proxies), and agent feedback loops are building alternative evaluation pathways. The constraint is seen as solvable through institutional redesign — not permanent, but requiring active sunset clauses (metric rotation, measurement diversity, time-bound evaluations).
constraint_indexing:constraint_classification(incentive_surface_warping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY ACCOUNTABILITY SYSTEM (PITON) — Traditional annual performance reviews, once functional as coordination mechanisms, have become largely theatrical. The review ritual persists through institutional inertia despite widespread recognition that single-metric or narrow-scope evaluations are degraded coordination tools. Organizations maintain 'compliance with feedback systems' theater even as real decisions depend on unmeasured factors (relationships, tacit knowledge, reputation). Theater ratio is high because the legitimating function has decoupled from actual personnel decisions.
constraint_indexing:constraint_classification(incentive_surface_warping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, metric-driven incentive structures are genuine coordination mechanisms (solving the incentive alignment problem) that simultaneously enable asymmetric extraction (through metric designer authority and measurement gaming dynamics). The constraint is neither pure coordination nor pure extraction but a hybrid where the coordination benefit partially legitimate the extraction overhead. Observable-dependent: if we measure 'attainment of stated objectives,' the extraction dominates; if we measure 'organizational function relative to manual oversight,' the coordination benefit is real.
constraint_indexing:constraint_classification(incentive_surface_warping, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incentive_surface_warping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incentive_surface_warping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incentive_surface_warping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incentive_surface_warping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(incentive_surface_warping, TR),
    TR >= 0.70.

:- end_tests(incentive_surface_warping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint begins as genuine coordination (metric alignment solves the incentive problem) but evolves into extraction as agents optimize the proxy surface rather than true objectives. The 0.52 value reflects the temporal trajectory — metrics provide ~40% coordination benefit and ~52% extraction cost at steady state. Early in the interval (t=0), extractiveness is low (0.28) because metric-objective alignment is strong. As agents learn and optimize the measurement surface, extractiveness grows. Suppression (0.48): Moderate. Significant barriers to resisting metric optimization include career consequences, performance evaluation dependence, and visibility asymmetry (metrics are measured; true objectives are often tacit). But suppression is not total — agents can exit (change jobs), collective action can force metric revision, and meta-awareness of Goodhart effects creates pressure for reform. Theater ratio (0.65): High and growing. Performance measurement develops substantial theatrical components: agents optimize for visible metrics while true objectives are implicit; measurement rituals (quarterly reviews, leaderboards) become performative compliance rather than actual feedback; metric gaming becomes a visible overhead cost that distorts effort allocation. The theater ratio increases over time as agents become more sophisticated at gaming and designers add measurement complexity to counter gaming.
 *
 * PERSPECTIVAL GAP:
 *   The metric designer sees Rope (coordination mechanism solving incentive alignment) while the frontline operator sees Snare (extractive optimization with no exit). This gap is fundamental: the designer experiences coordination because they set the target and can adjust it; the operator experiences extraction because they must hit the target or lose income. The intermediate manager sees Tangled Rope because they genuinely benefit from metric simplification (easier reporting) while also bearing costs (team pressure, gaming overhead). The reformer coalition sees Scaffold because they recognize the problem as solvable through metric diversity and multi-perspective evaluation. The legacy accountability system sees Piton because the review ritual persists through institutional inertia despite universal recognition that single-metric evaluation is degraded. No perspective is wrong — each captures the real structural experience from that position.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation captures how each agent experiences the constraint relative to its structural position. Metric designers (beneficiary + arbitrage) experience low d: they designed the measurement system and can adjust it, giving them upside capture without downside exposure. Frontline operators (victim + trapped) experience high d: they are measured, cannot exit, and bear the cost of metric-objective divergence. Intermediate managers (victim + constrained) experience moderate d: they feel enforcement overhead and team welfare pressure but have more organizational capital than frontline operators. The analytical observer (symmetric + analytical) experiences mid-range d because the constraint is genuinely mixed coordination-extraction. The reformer coalition (victim + mobile) experiences moderate d because they can eventually exit into alternative system designs, creating a sunset dynamic.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing coordination from extraction via temporal evolution and agent position. Early-stage incentive systems (first 1-2 years) function primarily as Rope — metrics closely track true objectives, gaming is minimal, and agents experience the constraint as helpful alignment. Mid-stage systems (5-10 years) become Tangled Rope — agents have learned the measurement surface, gaming increases, but coordination value remains because metric designers still update based on outcome feedback. Late-stage systems (15+ years) degrade toward Snare or Piton — metric gaming becomes the dominant agent strategy, metric designers lose the ability to adjust faster than agents can game, and the system persists through institutional inertia rather than functional coordination. The constraint is neither pure coordination nor pure extraction but a hybrid system that exhibits different classifications depending on time horizon: immediate (Rope), biographical (Tangled Rope), civilizational (Piton). The theater ratio tracks this evolution: low theater when metrics drive real changes; high theater when measurement becomes performative. The Goodhart lifecycle is the mechanism by which incentive systems move from coordination toward extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_objective_identifiability,
    'Can the actual system objective be specified with sufficient precision that metric designers can distinguish coordination from misalignment?',
    'Formalization of system objectives; comparison of stated metrics to revealed decision-weights; post-hoc analysis of which outcomes were truly valued vs which were proxies for unmeasured factors',
    'If objectives are specifiable: metric designers can construct better proxies, moving toward pure Rope. If objectives are fundamentally tacit or evolving: metric-proxy mismatch is structural, making Snare classification more robust than Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(true_objective_identifiability, conceptual, 'Whether true system objectives can be specified with precision').

omega_variable(
    gaming_detection_threshold,
    'What proportion of agent optimization effort must redirect toward metric gaming (rather than true objective advancement) before the constraint flips from Tangled Rope to Snare?',
    'Measurement of agent time allocation to metric-aligned vs true-objective-aligned activities; comparison across organizations with different metric diversity and measurement rigor',
    'If gaming threshold < 20%: metric systems retain some coordination value even with divergence. If threshold > 50%: most agent effort becomes extractive, and Snare classification dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gaming_detection_threshold, empirical, 'Threshold of gaming effort at which Tangled Rope becomes Snare').

omega_variable(
    multi_metric_sufficiency,
    'Do multi-metric or triangulation approaches actually prevent perverse optimization, or do they merely distribute gaming across more surface dimensions?',
    'Longitudinal comparison of single-metric vs balanced-scorecard organizations; analysis of whether gaming patterns shift rather than diminish; qualitative assessment of organizational time spent on measurement vs output',
    'If multi-metrics prevent gaming: scaffold sunset is real — metric diversity can solve the problem. If gaming distributes: theater increases (agents optimize more dimensions), and the constraint morphs toward higher-extraction Snare or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_metric_sufficiency, empirical, 'Whether multi-metric approaches prevent or redistribute perverse optimization').

omega_variable(
    agent_awareness_and_reflexivity,
    'When agents are aware they are being measured and incentivized, does that awareness enable coordination or accelerate metric gaming?',
    'Comparison of outcomes in transparent-measurement systems (agents know metrics) vs opaque systems (agents don''t); analysis of reflexivity effects where agents adjust behavior knowing they''re measured',
    'If awareness enables better coordination: transparency can reduce Snare classification. If awareness accelerates gaming: transparency increases extraction, and the constraint worsens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agent_awareness_and_reflexivity, empirical, 'Effect of measurement transparency on coordination vs gaming').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incentive_surface_warping, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(incent_tr_t0, incentive_surface_warping, theater_ratio, 0, 0.35).
narrative_ontology:measurement(incent_tr_t5, incentive_surface_warping, theater_ratio, 5, 0.5).
narrative_ontology:measurement(incent_tr_t10, incentive_surface_warping, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(incent_be_t0, incentive_surface_warping, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(incent_be_t5, incentive_surface_warping, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(incent_be_t10, incentive_surface_warping, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incentive_surface_warping, resource_allocation).
narrative_ontology:affects_constraint(incentive_surface_warping, metric_gaming_dynamics).
narrative_ontology:affects_constraint(incentive_surface_warping, performance_evaluation_theater).
narrative_ontology:affects_constraint(incentive_surface_warping, alignment_divergence_accumulation).

% DUAL FORMULATION NOTE:
% The perverse optimization trap decomposes into two structurally distinct constraints: (1) The genuine incentive alignment problem (Rope: how to coordinate agent behavior toward organizational goals) and (2) The metric-objective divergence problem (Tangled Rope → Snare: how metric gaming undermines the original coordination intent). These constraints are linked through the agent learning process — as agents optimize the proxy metric, the first constraint's solution becomes the second constraint's extraction mechanism. The network relationship captures this causal pathway.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(incentive_surface_warping, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
