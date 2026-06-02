% ============================================================================
% CONSTRAINT STORY: planning_fallacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_planning_fallacy, []).

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
 *   constraint_id: planning_fallacy
 *   human_readable: The Planning Fallacy
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The planning fallacy is a structural constraint operating across
 *   economic, organizational, and technological domains where systematic
 *   underestimation of task duration creates cascading resource
 *   misallocations. Unlike pure cognitive illusions, the planning fallacy
 *   exhibits institutional persistence and distributed benefit — some agents
 *   profit from compressed timelines, while others absorb the costs. The
 *   constraint appears simultaneously as coordination mechanism (optimism
 *   enables project initiation), extraction device (resource reallocation
 *   from allocators to planners), cognitive trap (human judgment under
 *   uncertainty), and institutional theater (precise Gantt charts that
 *   everyone knows are false). This indexical plurality makes the planning
 *   fallacy a diagnostic exemplar for how cognitive biases become structural
 *   constraints. The extractiveness has increased over the interval (0.28 to
 *   0.52) as project complexity has outpaced planning methodology —
 *   organizational systems have become more interdependent, making planning
 *   misalignment more costly.
 *
 * KEY AGENTS:
 *   - Optimistic Planners: Primary beneficiary (institutional/arbitrage) — capture psychological satisfaction and organizational visibility from claiming ambitious timelines; can pivot or reframe overruns
 *   - Resource Allocators: Primary victim (powerless/trapped) — must repeatedly distribute resources based on systematically false estimates; cannot exit reallocation decisions
 *   - Project Managers: Secondary victim (moderate/constrained) — experience mixed costs (deadline pressure) and benefits (initial stakeholder enthusiasm); constrained by accountability
 *   - Dependent Projects: Systemic victim (powerless/trapped) — inherit compressed schedules from predecessors; cascading failure chains reduce their time buffers
 *   - Debiasing Researchers and Practitioners: Organized agents (organized/constrained) — promoting reference-class forecasting and probabilistic planning as alternative frameworks; constrained by organizational inertia
 *   - Planning Institutions (PMI, Agile, tool vendors): Institutional beneficiary (institutional/arbitrage) — derive legitimacy and revenue from existence of planning process regardless of accuracy
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing what may be partly institutional as inherent cognitive limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(planning_fallacy, 0.52).
domain_priors:suppression_score(planning_fallacy, 0.48).
domain_priors:theater_ratio(planning_fallacy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(planning_fallacy, extractiveness, 0.52).
narrative_ontology:constraint_metric(planning_fallacy, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(planning_fallacy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(planning_fallacy, tangled_rope).
narrative_ontology:human_readable(planning_fallacy, "The Planning Fallacy").
narrative_ontology:topic_domain(planning_fallacy, "economic/social/technological").

domain_priors:requires_active_enforcement(planning_fallacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(planning_fallacy, optimistic_planners).
narrative_ontology:constraint_beneficiary(planning_fallacy, project_stakeholders_benefiting_from_underestimate).
narrative_ontology:constraint_victim(planning_fallacy, resource_allocators).
narrative_ontology:constraint_victim(planning_fallacy, dependent_projects).
narrative_ontology:constraint_victim(planning_fallacy, deadline_sensitive_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE ALLOCATOR (SNARE) — Cannot exit the fallacy; must repeatedly allocate resources based on systematically false estimates. Trapped by cascading failures: early projects overrun, later projects inherit compressed schedules, resource depletion accelerates. Maximum experienced extraction through resource misallocation and opportunity cost.
constraint_indexing:constraint_classification(planning_fallacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PROJECT MANAGER (TANGLED ROPE) — Experiences both coordination benefit (optimistic estimates enable stakeholder buy-in and team motivation) and extraction cost (pressure to deliver against unrealistic timelines). Constrained by accountability expectations but also benefits from initial optimism that facilitates project launching. Mixed extraction and coordination.
constraint_indexing:constraint_classification(planning_fallacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLANNING INSTITUTION (ROPE) — Institutional frameworks (PMI standards, Agile ceremonies, planning tools) experience the fallacy as a coordination mechanism: the belief in plan-ability itself enables project initiation and team coordination. Can arbitrage by switching methodologies or by exploiting the fallacy for competitive advantage.
constraint_indexing:constraint_classification(planning_fallacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEBIASING MOVEMENT (SCAFFOLD) — Organized agents (behavioral economists, planning researchers, reference-class forecasting advocates) see the fallacy as a temporary coordination failure with sunset logic. Reference-class forecasting, buffer-planning, and statistical debiasing methods represent alternative pathways. Constrained by institutional resistance but organized enough to drive progressive constraint reduction.
constraint_indexing:constraint_classification(planning_fallacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ESTIMATION RITUAL (PITON) — Formal planning ceremonies (kickoff meetings, estimation poker, Gantt chart presentations) persist despite known failure of point estimates. The ritual theater (precisely specified timelines, baseline plans, variance tracking) maintains institutional legitimacy while actual execution deviates systematically. Theater ratio high because estimation artifacts are produced and reviewed regardless of predictive accuracy.
constraint_indexing:constraint_classification(planning_fallacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COGNITIVE LIMIT VIEW (MOUNTAIN) — From civilizational/universal perspective, the planning fallacy may appear as an inherent limit of human cognition: forward planning necessarily involves modeling incomplete information, and optimism bias may be an irreducible feature of how humans represent futures. However, this naturalizes what is partly institutional — structured reference-class forecasting and statistical debiasing demonstrably reduce the bias, suggesting it is not a pure cognitive mountain.
constraint_indexing:constraint_classification(planning_fallacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(planning_fallacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(planning_fallacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planning_fallacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(planning_fallacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(planning_fallacy, TR),
    TR >= 0.70.

:- end_tests(planning_fallacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The planning fallacy systematically transfers resources from allocators to planners. Resource budgets are based on optimistic estimates; overruns consume margins intended for subsequent projects; the constraint compounds over time. But the extraction is not total (0.75+) because accurate re-estimation does occur, and organizations gradually adjust based on historical data. The value reflects that the bias is systematic and costly but not completely inescapable. Suppression (0.48): Moderate. Barriers to accurate estimation include genuine uncertainty in complex tasks, cognitive limitations in probability judgment, and institutional pressure to appear confident. But suppression is not extreme because reference-class forecasting and statistical debiasing demonstrably reduce the bias, suggesting it is not fully suppressed. Theater ratio (0.65): High. Planning rituals (estimation poker, baseline planning, variance tracking) are performed extensively despite known fallacy. Organizations produce detailed Gantt charts and point estimates knowing they will be inaccurate — the theater serves legitimacy functions (appearing planful, showing due diligence) rather than predictive functions.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. Resource allocators experience pure extraction (Snare) — they are trapped by cascading misallocations with no exit. Project managers experience mixed effects (Tangled Rope) — they benefit from initial optimism and stakeholder buy-in, but suffer pressure to deliver against false timelines. Planning institutions experience pure coordination (Rope) — the belief in plan-ability itself enables organizational functioning. Debiasing practitioners experience a solvable problem with a sunset (Scaffold) — reference-class forecasting and statistical methods represent a genuine exit pathway. The planning ritual itself appears as degraded theater (Piton) — Gantt charts and estimates persist through institutional inertia despite systematic failure. The civilizational observer risks seeing an immutable cognitive limit (Mountain) — human brains are optimistic about the future — but structural data reveals this as partly false naturalization: organizations that adopt reference-class forecasting achieve measurably better estimates.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values for each agent derive from their structural position relative to the extraction flow. Optimistic planners (beneficiaries with arbitrage options) have low d — they can reframe failures and move to new projects. Resource allocators (victims with trapped options) have high d — they absorb cascading costs with no exit. Project managers (moderate power, constrained exit) occupy middle d — they experience both benefit (initial enthusiasm) and cost (pressure). Dependent-project teams (powerless, trapped) have maximum d — they inherit compressed schedules with no choice. The debiasing movement (organized, constrained) has moderate d — they have agency but face institutional resistance. The planning institution (arbitrage) has low d — it benefits from process legitimacy regardless of outcome accuracy. These d values propagate through the sigmoid f(d) to produce experienced extractiveness chi for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The planning fallacy resolves mandatrophy by showing that all six types capture real structural features of the constraint. The mandatrophy arises from conflating cognitive bias (apparently an inherent feature of human judgment) with institutional arrangement (a structure that can be redesigned). The mountain perspective (cognitive limit) naturalizes what is partly structural. Once you introduce reference-class forecasting and probabilistic planning, the constraint shifts toward Rope (pure coordination) or Scaffold (temporary problem with sunset). The resource allocator's Snare is real — they are trapped by cascading effects. The planning institution's Rope is real — they do solve a genuine coordination problem (getting diverse teams to commit to common goals). The piton is real — precise Gantt charts that everyone knows are false serve institutional theater functions. No single type is 'correct' — the indexical plurality reveals that the planning fallacy is not ONE constraint but a family of related constraints: the cognitive bias, the institutional theater, the resource extraction mechanism, and the coordination coordination problem. Debiasing is structurally effective because it attacks the institutional and extractive dimensions while leaving legitimate coordination intact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimism_bias_cognitive_origin,
    'Is the planning fallacy fundamentally a cognitive bias (mental representation of uncertainty) or a structural incentive (rewarding optimism in competitive contexts)?',
    'Comparative analysis: cognitive-debiasing interventions (training, reference-class forecasting) vs. structural interventions (incentive realignment, accountability mechanisms). Measure persistence of bias under each.',
    'If primarily cognitive: constraint is partially unavoidable, some suppression and theater are inherent costs. If primarily structural: bias is removable through institutional redesign, suggesting false naturalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimism_bias_cognitive_origin, empirical, 'Whether planning fallacy is cognitive limit or structural incentive').

omega_variable(
    extraction_beneficiary_identity,
    'Who actually benefits from systematic underestimation — optimistic planners, project initiators, or compressed-timeline beneficiaries? Is benefit intentional or epiphenomenal?',
    'Trace incentive flow: do optimistic estimates correlate with individual rewards (promotion, bonus) or organizational success? Do planners persist in optimism when incentives are aligned with accuracy?',
    'If planners benefit intentionally: constraint is deliberate extraction (Snare more likely from planner perspective). If benefit is epiphenomenal: constraint is more coordination-failure than extraction (Rope or Scaffold more likely).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_beneficiary_identity, empirical, 'Whether planning optimism is intentional extraction or unintentional bias').

omega_variable(
    reference_class_forecasting_threshold,
    'At what organizational scale do reference-class forecasting and statistical debiasing methods become cost-effective relative to the gains from reduced schedule overruns?',
    'Cost-benefit analysis across project types and organizational sizes. Track adoption rates of reference-class methods and correlation with schedule accuracy improvement.',
    'If threshold is low (small teams/projects): scaffold sunset is structural and near. If threshold is high (only large programs): sunset is aspirational and distant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reference_class_forecasting_threshold, empirical, 'Cost-effectiveness threshold for debiasing methods').

omega_variable(
    institutional_commitment_to_false_plan,
    'Does the planning fallacy persist because humans are cognitively incapable of accurate estimation, or because institutions derive legitimacy from precise-seeming plans even when they know plans are false?',
    'Interview and observational studies of planning practitioners: do they acknowledge fallacy privately but maintain false estimates publicly for legitimacy? Do orgs with track records of overruns still produce point estimates in high-fidelity Gantt charts?',
    'If persistence is institutional theater: mandatrophy resolves via normalization of probabilistic planning (distributional estimates, uncertainty bounds). If persistence is cognitive: theater cannot be fully eliminated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_commitment_to_false_plan, conceptual, 'Whether planning fallacy persists due to institutional legitimacy needs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(planning_fallacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pf_tr_t0, planning_fallacy, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pf_tr_t5, planning_fallacy, theater_ratio, 5, 0.54).
narrative_ontology:measurement(pf_tr_t10, planning_fallacy, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(pf_be_t0, planning_fallacy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pf_be_t5, planning_fallacy, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(pf_be_t10, planning_fallacy, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(planning_fallacy, resource_allocation).
narrative_ontology:affects_constraint(planning_fallacy, optimism_bias_in_forecasting).
narrative_ontology:affects_constraint(planning_fallacy, cascade_failure_in_interdependent_systems).
narrative_ontology:affects_constraint(planning_fallacy, institutional_legitimacy_through_false_precision).

% DUAL FORMULATION NOTE:
% The planning fallacy decomposes into three structurally distinct constraints: (1) cognitive optimism bias in individual judgment (ε ≈ 0.18, mostly Mountain from analytical view, cognitive origin), (2) institutional theater of precise planning (ε ≈ 0.35, Piton from most views, theater-maintenance origin), (3) resource extraction through systemic misallocation (ε ≈ 0.52, Snare for allocators, extraction-mechanism origin). This story treats the integrated phenomenon; related stories examine each component's independent structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(planning_fallacy, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
