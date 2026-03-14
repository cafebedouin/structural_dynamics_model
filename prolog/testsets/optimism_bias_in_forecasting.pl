% ============================================================================
% CONSTRAINT STORY: optimism_bias_in_forecasting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_optimism_bias_in_forecasting, []).

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
 *   constraint_id: optimism_bias_in_forecasting
 *   human_readable: Optimism Bias in Forecasting Systems
 *   domain: decision_making/institutional_bias
 *
 * SUMMARY:
 *   Optimism bias in forecasting represents a persistent constraint on
 *   institutional decision-making where systematic overestimation of positive
 *   outcomes and underestimation of costs creates a coordinated extraction
 *   mechanism: institutional leadership and initiators benefit from
 *   optimistic framing while resource allocators, project teams, and
 *   downstream stakeholders bear the costs of forecast misses. The constraint
 *   exhibits characteristics of both genuine coordination (organizations
 *   genuinely need shared timeline and budget projections to function) and
 *   asymmetric extraction (the bias is systematically directional, benefiting
 *   optimists and penalizing pessimists). The extractiveness has increased
 *   over the interval as computational tools have become available but
 *   institutional adoption of debiasing methods (reference class forecasting,
 *   prediction markets, probabilistic estimation) remains low. The theater
 *   ratio reflects that formal forecasting processes are maintained despite
 *   widespread acknowledgment that institutional point estimates are
 *   unreliable — the ritual persists because replacing it would require
 *   admitting that planning is fundamentally difficult.
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — uses optimistic forecasts for stakeholder communication and career positioning; can exit constraints by reassigning blame or pivoting initiatives
 *   - Resource Allocators: Primary victim (powerless/trapped) — dependent on available forecasts; bears full cost of systematic overestimates; no exit option from using forecasts
 *   - Project Teams: Secondary victim (moderate/constrained) — pressured to meet optimistic timelines while also benefiting from inflated confidence and resource commitment; can partially exit through negotiation but face career risk
 *   - Forecasting Reform Coalition: Organized agents (organized/mobile) — prediction markets, reference class forecasters, probabilistic modelers building alternative pathways with sunset logic
 *   - Forecasting Ritual: Institutional actor (institutional/arbitrage) — maintains performative point estimation processes despite functional degradation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional incentive structures as inherent cognitive limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(optimism_bias_in_forecasting, 0.52).
domain_priors:suppression_score(optimism_bias_in_forecasting, 0.65).
domain_priors:theater_ratio(optimism_bias_in_forecasting, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(optimism_bias_in_forecasting, extractiveness, 0.52).
narrative_ontology:constraint_metric(optimism_bias_in_forecasting, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(optimism_bias_in_forecasting, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(optimism_bias_in_forecasting, tangled_rope).
narrative_ontology:human_readable(optimism_bias_in_forecasting, "Optimism Bias in Forecasting Systems").
narrative_ontology:topic_domain(optimism_bias_in_forecasting, "decision_making/institutional_bias").

domain_priors:requires_active_enforcement(optimism_bias_in_forecasting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(optimism_bias_in_forecasting, forecast_initiators).
narrative_ontology:constraint_beneficiary(optimism_bias_in_forecasting, optimistic_planners).
narrative_ontology:constraint_beneficiary(optimism_bias_in_forecasting, institutional_leadership).
narrative_ontology:constraint_victim(optimism_bias_in_forecasting, resource_allocators).
narrative_ontology:constraint_victim(optimism_bias_in_forecasting, downstream_stakeholders).
narrative_ontology:constraint_victim(optimism_bias_in_forecasting, forecasting_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE ALLOCATOR (SNARE) — Trapped in dependency on forecasts they cannot independently verify. Bears full cost of optimistic overestimates (budget shortfalls, project failures, opportunity loss). No exit option: must allocate resources based on available forecasts despite knowing they are systematically biased. Maximum extraction.
constraint_indexing:constraint_classification(optimism_bias_in_forecasting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PROJECT TEAM (TANGLED ROPE) — Constrained by pressure to deliver timelines and manage expectations, but also benefits from optimistic forecasting (inflated confidence, easier approval, resource commitment). Experiences genuine coordination (planning requires shared timeline projections) alongside asymmetric extraction (bears cost of missed deadlines while leadership reaps benefits of optimistic initial positioning).
constraint_indexing:constraint_classification(optimism_bias_in_forecasting, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — Benefits from optimistic forecasts without bearing primary consequences. Uses rosy projections for stakeholder communication, funding justification, and career advancement. Experiences the constraint as coordination mechanism: maintaining organizational morale and investor confidence. Net beneficiary with exit options (can reshape expectations, reassign blame, pivot to new initiatives).
constraint_indexing:constraint_classification(optimism_bias_in_forecasting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FORECASTING REFORM COALITION (SCAFFOLD) — Reference class forecasting, prediction markets, and probabilistic modeling communities see optimism bias as a temporary problem with a sunset. Building alternative forecasting mechanisms (prediction markets, base rate anchoring, structured estimation protocols) that bypass the optimism bias endemic to institutional point estimates. As these methods mature and become standard practice, the pressure toward naive optimism dissipates.
constraint_indexing:constraint_classification(optimism_bias_in_forecasting, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: FORECASTING RITUAL (PITON) — Point forecasting and budget estimation are largely performative. Organizations maintain formal forecasting processes despite widespread knowledge that institutional forecasts are systematically optimistic. The ritual persists through inertia: removing it would require admitting that planning processes don't work, so the theater is maintained. Theater ratio is moderate (some forecasts do inform decisions) but the core function has atrophied.
constraint_indexing:constraint_classification(optimism_bias_in_forecasting, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COGNITIVE LIMIT VIEW (MOUNTAIN) — From a civilizational perspective, some degree of optimism bias may be inherent to human prediction: agents with planning horizons have motivational pressure toward optimism, and bounded rationality makes accurate estimation of low-probability high-impact events difficult. This perspective risks naturalizing what is actually a contingent institutional arrangement (incentive structures that reward optimism, selection bias in who becomes a forecaster, absence of feedback loops that would correct systematic bias).
constraint_indexing:constraint_classification(optimism_bias_in_forecasting, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(optimism_bias_in_forecasting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(optimism_bias_in_forecasting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(optimism_bias_in_forecasting, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(optimism_bias_in_forecasting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(optimism_bias_in_forecasting, TR),
    TR >= 0.70.

:- end_tests(optimism_bias_in_forecasting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Institutional forecasts systematically overestimate positive outcomes and underestimate costs by 20-40% across diverse domains (software projects, infrastructure, organizational change). This is not random error — the directionality is consistent and beneficiaries are consistently institutional leaders and initiators. However, the extraction is not complete (0.70+) because some coordination function is genuine: organizations do need shared time and resource projections. The constraint bundles legitimate coordination with asymmetric extraction. Suppression (0.65): High. Multiple mechanisms prevent correction: (1) feedback loops are weak (forecasters don't learn from misses because they leave the organization or move to different projects); (2) alternative methods exist but are not adopted (reference class forecasting and prediction markets reduce bias but are treated as optional); (3) institutional culture selects for and promotes optimistic forecasters; (4) acknowledging systematic bias would delegitimize planning processes. Theater ratio (0.58): Moderate. Formal forecasting processes are maintained and used, but their primary function (informing resource allocation accurately) has partly failed. Some organizations have shifted to scenario planning or probabilistic estimates, reducing theater; others maintain traditional point estimates as ritual. The increasing trend reflects that gap between forecast quality and forecasting complexity has widened.
 *
 * PERSPECTIVAL GAP:
 *   Resource allocators see a snare (trapped dependency on biased information). Project teams see a tangled rope (genuine coordination need mixed with asymmetric pressure). Leadership sees a rope (coordination mechanism that serves legitimate function). The reform coalition sees a scaffold (temporary problem being solved by alternative methods). The forecasting ritual itself is a piton (performative persistence). The civilizational observer risks a false mountain (naturalizing contingent institutional structures). This perspectival spectrum is diagnostic of the constraint's true nature: if it were a pure cognitive limit (mountain), all perspectives would converge. The wide disagreement indicates contingent institutional factors.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is determined by each agent's structural position. Institutional leadership with arbitrage options (ability to reframe expectations, assign blame, pivot initiatives) experience low or negative effective extraction — they benefit from optimism. Resource allocators with no exit option (must use available forecasts) experience maximum extraction — they bear all consequences. Project teams with constrained exits (can negotiate but face career penalties) experience moderate extraction. The reform coalition with mobile options (can choose to use alternative methods) experiences low extraction. The empirical question is whether optimization toward optimism is enforced (punishing accurate-but-pessimistic forecasters) or incentivized (rewarding optimistic narratives) — this determines whether the mechanism is snare-like (requiring external correction) or rope-like (correctable through norm change).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH PERSPECTIVAL SPECTRUM. The apparent mandatrophy (is this coordination or extraction?) dissolves when the constraint is viewed as a presheaf over multiple institutional contexts. From leadership's structural position, optimistic forecasting genuinely coordinates planning. From resource allocators' position, it is pure extraction. From project teams' position, it is mixed. The reform community's scaffold perspective shows that the constraint is neither inherent nor immutable — alternative mechanisms (prediction markets, reference class forecasting) bypass the optimism bias entirely, which would be impossible if the bias were a fundamental cognitive limit. The false mountain perspective at the civilizational level reveals that institutional structures (incentive systems, feedback loop closure, selection bias in forecaster populations) are doing the work that would be necessary only if optimism bias were inherent. The actual resolution is institutional: shift from point forecasting to probabilistic estimation and build feedback loops that penalize systematic bias.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    motivation_versus_cognition,
    'Is optimism bias primarily motivational (incentive-driven overstatement) or primarily cognitive (genuine belief distortion)?',
    'Experimental comparison: forecasters with reputation incentives vs anonymous forecasters; private estimates vs public forecasts; post-hoc analysis of who revised expectations when evidence contradicted forecasts',
    'If primarily motivational: the constraint is enforcement-driven (requires changing incentives). If primarily cognitive: the constraint is harder to solve (requires changing how humans process information or using external decision-support systems).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(motivation_versus_cognition, empirical, 'Whether optimism bias is motivational or cognitive').

omega_variable(
    feedback_loop_closure,
    'Why don''t forecasters learn from repeated forecast misses and correct their systematic bias?',
    'Historical analysis of individual forecaster calibration over decades; institutional analysis of whether feedback on forecast accuracy is actually communicated to forecasters; measurement of selection bias (who leaves forecasting, who stays)',
    'If learning is possible but prevented: constraint is a suppression mechanism (feedback is actively blocked). If learning is blocked by cognitive limits: constraint approaches mountain status (inherent to human prediction). If learning occurs but is overridden by institutional pressure: constraint is enforcement-driven.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feedback_loop_closure, empirical, 'Closure of feedback loops in forecasting practice').

omega_variable(
    alternative_method_adoption,
    'When reference class forecasting, prediction markets, or probabilistic models are available, do organizations actually use them to override institutional point estimates?',
    'Survey of organizations with access to alternative forecasting methods; measurement of when/whether alternative forecasts are consulted; tracking of whether alternative forecasts change final resource allocation decisions',
    'If alternatives are adopted: scaffold perspective confirmed — sunset is real. If alternatives are available but ignored: constraint is stronger than measured (institutional incentives resist correction). If alternatives are unavailable: constraint is partly a capability gap rather than purely a bias mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_method_adoption, empirical, 'Adoption and use of alternative forecasting methods').

omega_variable(
    selection_bias_in_forecaster_population,
    'Do organizational cultures systematically select for, retain, and promote forecasters who are optimistic?',
    'Longitudinal analysis of forecaster career trajectories; correlation between historical forecast accuracy and career advancement; measurement of how organizational culture responds to accurate-but-pessimistic vs optimistic-but-wrong forecasters',
    'If selection bias is strong: the constraint includes an active enforcement mechanism (removing pessimistic forecasters). If weak: the bias is more purely motivational or cognitive. Selection bias amplifies the constraint because it ensures the forecaster population is systematically biased.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selection_bias_in_forecaster_population, empirical, 'Selection bias in forecaster population composition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(optimism_bias_in_forecasting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(optim_tr_t0, optimism_bias_in_forecasting, theater_ratio, 0, 0.48).
narrative_ontology:measurement(optim_tr_t5, optimism_bias_in_forecasting, theater_ratio, 5, 0.53).
narrative_ontology:measurement(optim_tr_t10, optimism_bias_in_forecasting, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(optim_be_t0, optimism_bias_in_forecasting, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(optim_be_t5, optimism_bias_in_forecasting, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(optim_be_t10, optimism_bias_in_forecasting, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(optimism_bias_in_forecasting, information_standard).
narrative_ontology:affects_constraint(optimism_bias_in_forecasting, planning_fallacy_institutional).
narrative_ontology:affects_constraint(optimism_bias_in_forecasting, sunk_cost_escalation).
narrative_ontology:affects_constraint(optimism_bias_in_forecasting, backfire_effect_in_correction).

% DUAL FORMULATION NOTE:
% Optimism bias in forecasting decomposes into structural components: (1) cognitive bias component (irreducible human optimism in estimation); (2) incentive-driven component (institutional pressure toward optimism); (3) selection bias component (organizational structures that select for optimists). Each has distinct extractiveness. This story focuses on the institutional-incentive and selection-bias components (higher ε). The pure cognitive component would be a separate story with lower ε (closer to mountain).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(optimism_bias_in_forecasting, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
