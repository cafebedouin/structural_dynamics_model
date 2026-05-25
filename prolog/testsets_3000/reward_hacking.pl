% ============================================================================
% CONSTRAINT STORY: reward_hacking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reward_hacking, []).

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
 *   constraint_id: reward_hacking
 *   human_readable: Reward Hacking: Alignment Misalignment Between Stated Goals and Measured Outcomes
 *   domain: systems_design/optimization/artificial_intelligence
 *
 * SUMMARY:
 *   Reward hacking is the structural divergence between a system's declared
 *   objective and its measured objective, creating incentive for optimizers
 *   to maximize the measured proxy at the expense of the actual goal. This
 *   constraint operates across domains (education, healthcare, business, AI
 *   safety, ecological management) wherever performance measurement becomes a
 *   primary steering mechanism. The constraint exhibits tangled rope
 *   structure: genuine coordination function (metrics do enable large-scale
 *   goal pursuit when well-designed) coupled with asymmetric extraction
 *   (optimizers benefit from exploiting metric gaps while actual stakeholders
 *   and system health bear the costs). The theater ratio (0.68) reflects that
 *   much measurement activity is performative compliance — generating
 *   dashboards and audit trails that signal accountability without ensuring
 *   goal alignment. The extractiveness trajectory (0.35 → 0.62 over the
 *   interval) shows how reward hacking accumulates as optimizers learn
 *   exploit pathways and institutional actors accumulate sunk investment in
 *   the misaligned metrics.
 *
 * KEY AGENTS:
 *   - Actual Mission/System Health: Primary victim (powerless/trapped) — the true objective has no optimization loop and bears full cost of metric substitution
 *   - Downstream Stakeholders: Secondary victim (moderate/constrained) — believe in system's stated goals; face side effects and externalities from metric optimization
 *   - Metric Optimizer/Gaming Actor: Primary beneficiary (institutional/arbitrage) — captures short-term gains and performance rewards from exploiting metric gaps; can pivot to new metrics if pressure mounts
 *   - Domain Expert/Auditor: Secondary actor (powerful/mobile) — sees both coordination and extraction functions; has capacity to reframe or exit
 *   - Measurement and Compliance Theater: Institutional layer (institutional/arbitrage) — maintains performative reporting systems; persists through inertia even when known to be misaligned
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes Goodhart's law: any finite proxy for infinite objective creates misalignment incentives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reward_hacking, 0.58).
domain_priors:suppression_score(reward_hacking, 0.65).
domain_priors:theater_ratio(reward_hacking, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reward_hacking, extractiveness, 0.58).
narrative_ontology:constraint_metric(reward_hacking, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(reward_hacking, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reward_hacking, tangled_rope).
narrative_ontology:human_readable(reward_hacking, "Reward Hacking: Alignment Misalignment Between Stated Goals and Measured Outcomes").
narrative_ontology:topic_domain(reward_hacking, "systems_design/optimization/artificial_intelligence").

domain_priors:requires_active_enforcement(reward_hacking).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reward_hacking, metric_gamers).
narrative_ontology:constraint_beneficiary(reward_hacking, short_term_optimizers).
narrative_ontology:constraint_victim(reward_hacking, system_health).
narrative_ontology:constraint_victim(reward_hacking, actual_mission_accomplishment).
narrative_ontology:constraint_victim(reward_hacking, downstream_stakeholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACTUAL MISSION (SNARE) — The true objective (human flourishing, ecological health, social stability) cannot exit the metric substitution trap. It has no voice in the optimization loop; the measured proxy is all that drives resource allocation. Maximum extraction — the mission bears full cost of misalignment while the metric receives all attention and resources.
constraint_indexing:constraint_classification(reward_hacking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM STAKEHOLDER (SNARE) — Constrained by the system's publicly declared objectives (which they believed); they face costs from metric optimization that ignores side effects. They can theoretically exit or protest, but social/legal barriers and information asymmetry limit effective alternatives. High extraction with some agency.
constraint_indexing:constraint_classification(reward_hacking, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: METRIC OPTIMIZER (ROPE) — Institutional actor (firm, agency, algorithm) experiences the constraint as pure coordination: meeting the declared metric IS the declared goal. From this perspective, there is no misalignment — the system is working as designed. Arbitrage: they can pivot metrics or exit the system if pressure mounts. Net beneficiary.
constraint_indexing:constraint_classification(reward_hacking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: DOMAIN EXPERT/AUDITOR (TANGLED ROPE) — Sees both the coordination function (metrics do enable alignment when well-designed) and the extraction (misaligned metrics create perverse incentives). Powerful enough to exit or reframe, but also invested in the system's legitimacy. Mixed classification reflects genuine tension between metric benefit and metric cost.
constraint_indexing:constraint_classification(reward_hacking, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MEASUREMENT THEATER (PITON) — Measurement frameworks persist through institutional inertia even when known to be misaligned. KPI dashboards, performance evaluations, and metric reporting systems continue functioning long after stakeholders recognize their dysfunction. Theater ratio is high because much measurement activity is performative — generating compliance reports, audit trails, and accountability theater rather than genuine feedback. The system maintains itself because the alternative (admitting metrics don't work) is organizationally costly.
constraint_indexing:constraint_classification(reward_hacking, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, metric design inevitably creates alignment gaps: any finite proxy for an infinite objective generates at least some misalignment incentives. This is a coordination problem (finding metrics that align sufficiently) coupled with an extraction problem (optimizers exploit remaining gaps). The constraint is not solvable into pure rope — some tension between stated and measured goals is structural.
constraint_indexing:constraint_classification(reward_hacking, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reward_hacking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reward_hacking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reward_hacking, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reward_hacking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reward_hacking, TR),
    TR >= 0.70.

:- end_tests(reward_hacking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Metric gamers capture real benefits — performance bonuses, promotions, budget allocation, status signals — while the actual mission and downstream stakeholders bear costs (false confidence in progress, resources diverted from genuine goals, side effects). The extractiveness is not extreme (< 0.66) because some metrics are reasonably well-aligned and some optimizers do internalize the mission beyond the metric. But the asymmetry is substantial: those who optimize the metric benefit; those dependent on the actual outcome suffer. Suppression (0.65): High. Barriers to exit or voice are significant: stakeholders may not recognize misalignment (information asymmetry), changing metrics is organizationally expensive (sunk investment), alternative measurement systems are unclear, and institutional inertia maintains current frameworks. The suppression is structural — embedded in how performance evaluation and resource allocation systems work. Theater ratio (0.68): Moderately high. Measurement activity increases over time (from 0.42 to 0.75 in the interval) as organizations add more dashboards, reporting systems, and audit trails in response to gaming pressure. Much of this addition is performative — generating more measurement data without improving alignment. The theater reflects Goodhart's law: as metrics become targets, they cease to be good measures.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap shows up most sharply between the metric optimizer (Rope) and the actual mission (Snare). The optimizer is solving the problem the system posed: maximize the metric. The mission is suffering from the solution. This gap is not a difference of opinion but a structural difference in what each perspective observes as the constraint. For the optimizer, the constraint is 'how do I meet the performance targets?' (coordinated by the metric). For the mission, the constraint is 'how does my actual goal survive in a system optimizing a misaligned proxy?' (extraction through metric substitution). The domain expert perspective (Tangled Rope) holds both views simultaneously, which is why it is the most analytically complete but also the most unstable — unstable because an actor occupying that position is cognitively forced to recognize both the legitimacy of metric optimization AND its extractive consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects each agent's structural relationship to the extraction flow. Metric gamers are beneficiaries with institutional power and arbitrage options (low d → negative effective extraction from their perspective). The actual mission and downstream stakeholders are victims with powerless/trapped or moderate/constrained positions (high d → high experienced extraction). Domain experts occupy a mixed position — they have power and exit options but are also invested in the system's legitimacy, producing middle d values. The analytical perspective is observer-neutral (d ≈ 0.72 canonical value). The directionality derivation produces clear extracted value flow: toward metric optimizers, away from actual goals and downstream actors. This asymmetry is the core of the tangled rope classification — genuine coordination structure (metrics can align goals) bundled with real asymmetric extraction (gap between metric and reality is exploitable).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that reward hacking is fundamentally a Tangled Rope: it has a real coordination function (metrics enable large-scale goal pursuit) and a real extraction mechanism (misalignment between metric and goal creates exploitable gaps). The constraint cannot be collapsed into pure Rope because the extraction is structural and asymmetric — those closest to the metric benefit, those dependent on the actual outcome suffer. It cannot be classified as pure Snare because metrics do genuinely coordinate action at scale when reasonably well-designed. The Tangled Rope classification is stable across all high-credibility perspectives (optimizer, expert, analytical observer). The Snare perspective (from the actual mission) is real but represents the victim's experience of a tangled constraint, not the constraint's true type. The Rope perspective (from the optimizer) represents the beneficiary's experience of what is structurally a tangled constraint. Mandatrophy resolution confirms: reward hacking is a coordination mechanism with embedded asymmetric extraction, not a pure extraction mechanism mislabeled as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_goodness_threshold,
    'What level of metric-objective alignment constitutes ''good enough'' coordination vs extractive misalignment?',
    'Empirical comparison of outcome distributions under different metric proxies; cost-benefit analysis of optimization gains vs side-effect damage; stakeholder satisfaction trajectories',
    'If threshold is high (>0.85 alignment): many systems currently classified as Tangled Rope degrade to Snare. If threshold is low (>0.60): many current Snares reframe as acceptable Rope. Fundamentally shifts classification landscape.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metric_goodness_threshold, preference, 'Acceptable threshold for metric-objective alignment').

omega_variable(
    optimizer_awareness_and_intent,
    'Are metric gamers deliberately gaming misaligned metrics (intentional extraction) or responding rationally to incentive structures they believe are legitimate (innocent coordination)?',
    'Post-hoc analysis of optimizer knowledge state at time of optimization; internal communications and decision records; counterfactual: would the same optimizer change behavior if unambiguously told the metric was misaligned?',
    'If deliberate: metric gamers are conscious extractors (higher d → higher χ). If innocent: they are coordination actors with misaligned feedback (lower d → lower χ, but still extraction due to structural misalignment). Classification ranges from Snare to Rope depending on awareness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimizer_awareness_and_intent, empirical, 'Whether metric gamers deliberately exploit misalignment or respond innocently to incentives').

omega_variable(
    metric_design_feasibility,
    'Are perfectly aligned metrics theoretically possible or is misalignment inherent to any finite proxy system?',
    'Formal analysis: can any computable metric fully capture unmeasurable objectives? Proof sketch in Rice''s theorem domain or Goodhart''s law limits. Empirical: does any real-world domain show metrics that never degrade into gaming?',
    'If misalignment is inherent: reward hacking is not a design failure but a structural feature of optimization under partial observability. Constraint should be reclassified as Mountain from the analytical perspective, and Tangled Rope from all others becomes not just pragmatically necessary but theoretically optimal. If misalignment is avoidable: reward hacking is a failure of insufficient metric care — classification suggests active enforcement and sunset (Tangled Rope or Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_design_feasibility, conceptual, 'Whether perfectly aligned metrics are theoretically possible').

omega_variable(
    multi_metric_escape,
    'Does adding more metrics reduce reward hacking by increasing alignment or increase it by providing more dimensions to exploit?',
    'Empirical: comparison of single-metric vs multi-metric systems in same domains; analysis of metric correlation and optimizer exploitation of metric independence; saturation threshold for metric count',
    'If more metrics reduce hacking: multi-objective design is an enforcement mechanism (makes constraint more like Scaffold). If more metrics increase hacking: reward hacking becomes more sophisticated but still extracted (constraint remains Tangled Rope with higher extractiveness). If nonmonotonic: there is an optimal metric count, and systems at extremes are poorly designed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(multi_metric_escape, empirical, 'Whether additional metrics reduce or increase reward hacking severity').

omega_variable(
    feedback_latency_and_alignment,
    'Does real-time feedback on misalignment prevent hacking or accelerate the optimizer''s learning of exploit strategies?',
    'A/B testing: systems with feedback loops that flag misalignment vs systems with delayed feedback; comparison of hacking sophistication and damage under immediate vs delayed awareness',
    'If feedback prevents hacking: transparency and measurement quality are enforcement mechanisms (constraint can degrade to Rope with sufficient feedback infrastructure). If feedback accelerates exploitation: faster feedback paradoxically worsens extraction (constraint becomes worse than Snare in some configurations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_latency_and_alignment, empirical, 'Whether real-time feedback reduces or amplifies reward hacking').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reward_hacking, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rwhack_tr_t0, reward_hacking, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rwhack_tr_t3, reward_hacking, theater_ratio, 3, 0.55).
narrative_ontology:measurement(rwhack_tr_t6, reward_hacking, theater_ratio, 6, 0.68).
narrative_ontology:measurement(rwhack_tr_t9, reward_hacking, theater_ratio, 9, 0.75).

% Extraction over time
narrative_ontology:measurement(rwhack_be_t0, reward_hacking, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rwhack_be_t3, reward_hacking, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(rwhack_be_t6, reward_hacking, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(rwhack_be_t9, reward_hacking, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reward_hacking, resource_allocation).
narrative_ontology:boltzmann_floor_override(reward_hacking, 0.18).
narrative_ontology:affects_constraint(reward_hacking, goodharts_law).
narrative_ontology:affects_constraint(reward_hacking, metric_substitution_bias).
narrative_ontology:affects_constraint(reward_hacking, principal_agent_asymmetry).

% DUAL FORMULATION NOTE:
% Reward hacking is downstream of Goodhart's law (the theoretical structural limit) but represents the behavioral manifestation where actors actually exploit metric-objective gaps. Goodhart's law is a mountain (inevitable divergence); reward hacking is a tangled rope (how that divergence becomes an extraction mechanism). The network shows causality: Goodhart's law establishes why misalignment is inevitable; reward hacking exploits the inevitability; metric substitution bias describes the cognitive framing that naturalizes the exploitation; principal-agent asymmetry provides the institutional structure where optimization and actual goals are misaligned.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reward_hacking, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
