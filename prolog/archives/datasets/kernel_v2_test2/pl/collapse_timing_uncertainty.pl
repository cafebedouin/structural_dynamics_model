% ============================================================================
% CONSTRAINT STORY: collapse_timing_uncertainty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collapse_timing_uncertainty, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: collapse_timing_uncertainty
 *   human_readable: Collapse Timing Uncertainty in World3 Recalibration
 *   domain: system_dynamics/sustainability_science/industrial_ecology
 *
 * SUMMARY:
 *   The 2024-2030 collapse timing window from the World3 recalibration
 *   creates a structural trap for preparedness planning: the forecast is
 *   presented as the model's primary output and drives resource allocation
 *   decisions, yet the authors explicitly state that (a) recalibration
 *   optimized only for rising-edge fit through 2023, (b) the model assumes
 *   constant socio-economic relationships through regime change, and (c)
 *   post-collapse trajectories are not valid. This creates an asymmetry: the
 *   forecast is loud and actionable, the methodological limitations are quiet
 *   and technical. Collapse preparedness planning cannot exit this constraint
 *   — infrastructure investments, institutional design, and resource
 *   allocation all require lead time measured in years to decades, yet the
 *   only integrative model at global scope with 50+ years of empirical
 *   grounding provides a timing estimate that carries its own epistemic
 *   warnings. The constraint extracts through forced reliance: planning
 *   bodies must act on a forecast the model itself declares limited, because
 *   no alternative framework exists at comparable scope. The theater ratio
 *   (0.58) reflects that much of the 'precision' in the 2024-2030 window is
 *   performative: the confidence interval is narrower than the methodological
 *   uncertainties justify, creating an appearance of actionable specificity
 *   that the underlying assumptions cannot support.
 *
 * KEY AGENTS:
 *   - Collapse Preparedness Planning: Primary victim (powerless/trapped) — must act on epistemically compromised forecast because no alternative integrative model exists; bears full cost of timing uncertainty through misallocation risk
 *   - Policy Intervention Design: Secondary victim (moderate/constrained) — constrained by institutional mandates to incorporate long-range forecasts, but benefits from World3's integrative framework; mixed experience of coordination and extraction
 *   - Recalibration Authors: Primary beneficiary (institutional/arbitrage) — captures citation advantage and agenda-setting authority during forecast window; experiences constraint as transparent coordination with stated limitations
 *   - Business-as-Usual Advocacy: Secondary beneficiary (institutional/arbitrage) — benefits from timing uncertainty as deferral mechanism; wide confidence interval and post-collapse invalidity create rhetorical space to question urgency
 *   - Resource Allocation Bodies: Organized victims (organized/constrained) — must justify long-term investments with quantitative forecasts; timing uncertainty extracts through misallocation risk but model provides genuine coordination value
 *   - Analytical Observer: Sees structural extraction (analytical/analytical) — methodological limitations are stated but suppressed by forecast's rhetorical weight; asymmetry between loud forecast and quiet disclaimers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collapse_timing_uncertainty, 0.68).
domain_priors:suppression_score(collapse_timing_uncertainty, 0.72).
domain_priors:theater_ratio(collapse_timing_uncertainty, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collapse_timing_uncertainty, extractiveness, 0.68).
narrative_ontology:constraint_metric(collapse_timing_uncertainty, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(collapse_timing_uncertainty, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(collapse_timing_uncertainty, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(collapse_timing_uncertainty, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collapse_timing_uncertainty, snare).
narrative_ontology:human_readable(collapse_timing_uncertainty, "Collapse Timing Uncertainty in World3 Recalibration").
narrative_ontology:topic_domain(collapse_timing_uncertainty, "system_dynamics/sustainability_science/industrial_ecology").

domain_priors:requires_active_enforcement(collapse_timing_uncertainty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collapse_timing_uncertainty, recalibration_authors).
narrative_ontology:constraint_beneficiary(collapse_timing_uncertainty, business_as_usual_advocacy).
narrative_ontology:constraint_victim(collapse_timing_uncertainty, collapse_preparedness_planning).
narrative_ontology:constraint_victim(collapse_timing_uncertainty, policy_intervention_design).
narrative_ontology:constraint_victim(collapse_timing_uncertainty, resource_allocation_bodies).
narrative_ontology:constraint_vindicates(collapse_timing_uncertainty, limits_to_growth_framework_validity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COLLAPSE PREPAREDNESS PLANNING (SNARE) — Trapped by the need to act on model outputs despite epistemic uncertainty. Cannot exit the planning requirement (infrastructure, resource allocation, institutional design all require lead time), yet the 2024-2030 window carries methodological warnings the model itself declares invalid post-collapse. Maximum extraction: forced to treat an epistemically compromised forecast as actionable intelligence because no alternative integrative model exists at comparable scope.
constraint_indexing:constraint_classification(collapse_timing_uncertainty, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: POLICY INTERVENTION DESIGN (TANGLED ROPE) — Constrained by institutional mandates to incorporate long-range forecasts into policy design, but also benefits from the model's integrative framework for cross-sectoral analysis. The timing uncertainty extracts credibility cost (interventions designed for 2024-2030 that arrive too early or too late lose political support), but the World3 structure provides genuine coordination value for understanding feedback loops. Mixed experience: real coordination function contaminated by timing extraction.
constraint_indexing:constraint_classification(collapse_timing_uncertainty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RECALIBRATION AUTHORS (ROPE) — Benefits from citation advantage, methodological priority, and agenda-setting authority during the 2024-2030 window. Experiences the constraint as coordination: the recalibration communicates updated parameter estimates and extends the model's empirical grounding through 2023. The epistemic limitations are explicitly stated in the paper, so from this perspective the constraint is transparent coordination with known bounds. Net beneficiary: extraction runs toward this agent through citation accumulation and intellectual priority.
constraint_indexing:constraint_classification(collapse_timing_uncertainty, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BUSINESS-AS-USUAL ADVOCACY (ROPE) — Benefits from timing uncertainty as a deferrral mechanism. The wide confidence interval and explicit post-collapse invalidity create rhetorical space to argue that collapse is either not imminent or not certain, justifying delay of costly interventions. Experiences the constraint as coordination: the model's methodological transparency provides legitimate grounds to question urgency. Net beneficiary: the uncertainty extracts credibility from preparedness planning and redirects it toward wait-and-see positions.
constraint_indexing:constraint_classification(collapse_timing_uncertainty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RESOURCE ALLOCATION BODIES (TANGLED ROPE) — Organized institutional actors (national planning agencies, multilateral development banks, climate adaptation funds) that must allocate resources across time horizons. Constrained by the need to justify long-term investments with quantitative forecasts, but also benefit from the World3 framework's integrative structure for cross-sectoral resource planning. The timing uncertainty extracts through misallocation risk (resources committed to 2024-2030 preparedness that may be premature or late), but the model provides genuine coordination value for understanding resource interdependencies. Mixed experience: coordination function present but contaminated by timing extraction.
constraint_indexing:constraint_classification(collapse_timing_uncertainty, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From the analytical position, the constraint is a snare: the recalibration optimized only for rising-edge fit (1970-2023), assumes constant socio-economic relationships through regime change, and explicitly disclaims validity for post-collapse trajectories, yet the 2024-2030 window is presented as the model's primary forecast output. The methodological limitations are stated but structurally suppressed by the forecast's rhetorical weight. The analytical observer sees extraction: the timing window functions as a claim-staking mechanism that captures attention and resources during the forecast window, while the epistemic limitations prevent accountability when the window closes. The suppression is in the asymmetry: the forecast is loud, the disclaimers are quiet.
constraint_indexing:constraint_classification(collapse_timing_uncertainty, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collapse_timing_uncertainty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collapse_timing_uncertainty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collapse_timing_uncertainty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(collapse_timing_uncertainty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(collapse_timing_uncertainty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts through forced reliance on an epistemically compromised forecast. Preparedness planning cannot exit (infrastructure and institutional design require multi-year lead times), yet the 2024-2030 window carries methodological warnings the model declares invalid for post-collapse trajectories. The extraction is in the asymmetry: the forecast drives resource allocation, but the epistemic limitations prevent accountability when the window closes. The value reflects that this is not pure extraction (the model provides genuine integrative value and the limitations are stated), but the structural position of planning bodies creates a trap. Suppression (0.72): High. Suppression operates through three mechanisms: (1) no alternative integrative model at comparable scope exists, so exit requires abandoning quantitative long-range planning entirely; (2) the technical nature of the methodological limitations (rising-edge optimization, constant-relationship assumption, post-collapse invalidity) suppresses their salience relative to the headline forecast; (3) institutional mandates require quantitative forecasts for resource allocation, creating structural lock-in. The suppression has intensified over the interval as the 2024-2030 window has approached and planning urgency has increased. Theater ratio (0.58): Moderate-high. The precision of the 2024-2030 window is partly performative: the confidence interval is narrower than the methodological uncertainties justify. Rising-edge-only optimization, constant-relationship assumptions through regime change, and explicit post-collapse invalidity all suggest wider uncertainty than the published bounds indicate. The theater is in presenting a point forecast with narrow confidence interval when the structural assumptions support only a broad qualitative claim (collapse likely within decades, not within a specific 6-year window). The theater has increased over the interval as the forecast has been communicated to policy audiences with progressively less methodological context. Accessibility collapse (0.35): Low-moderate. Alternatives to World3-based forecasting do exist: other integrated assessment models, earth system models with socio-economic coupling, qualitative scenario analysis, and adaptive planning frameworks that don't depend on point forecasts. The accessibility collapse is partial: World3 is the most established and empirically grounded integrative model at global scope, so alternatives lack comparable legitimacy, but they are not foreclosed. The value reflects that planning bodies could exit to alternative frameworks, but at significant institutional cost (loss of quantitative precision, reduced political legitimacy, departure from established practice). Resistance (0.62): Moderate-high. The constraint meets substantial resistance from multiple directions: (1) methodological critics who argue the recalibration's rising-edge-only optimization and constant-relationship assumptions are unjustified; (2) planning bodies frustrated by the timing uncertainty and post-collapse invalidity; (3) alternative modeling communities (IAM, ESM, agent-based) who contest World3's structural assumptions; (4) business-as-usual advocates who use the uncertainty to argue against urgent intervention. The resistance is real and organized, but has not dislodged World3's agenda-setting authority. The value reflects that this is a contested constraint, not an accepted one.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is diagnostic. Collapse preparedness planning sees a snare: trapped by the need to act on a forecast the model declares limited, with no exit and no alternative. Policy intervention design sees tangled rope: genuine coordination value (integrative framework for cross-sectoral analysis) contaminated by timing extraction (credibility cost of interventions designed for a window that may be wrong). The recalibration authors see rope: transparent coordination with stated limitations, where the epistemic warnings are part of the communication. Business-as-usual advocacy also sees rope: the uncertainty provides legitimate grounds to question urgency. Resource allocation bodies see tangled rope: must allocate resources across time horizons using a forecast with known limitations, experiencing both coordination value and extraction. The analytical observer sees snare: the methodological limitations are stated but structurally suppressed by the forecast's rhetorical weight, creating an asymmetry that extracts from planning bodies while protecting the authors from accountability. The gap reveals that the constraint's type depends on structural position: beneficiaries experience coordination, victims experience extraction, and the analytical observer sees the asymmetry that creates the trap.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the timing uncertainty. Collapse preparedness planning is a full victim: trapped by the need to act on the forecast, bears the full cost of timing uncertainty through misallocation risk, and has no exit option because no alternative integrative model exists. The engine derives high d (toward 1.0) from victim status + trapped exit, producing high effective extraction. Policy intervention design is a partial victim: constrained by institutional mandates but also benefits from the integrative framework, so d is moderate (around 0.5-0.6), producing moderate effective extraction. The recalibration authors are beneficiaries: capture citation advantage and agenda-setting authority, with arbitrage exit options (can pivot to other research if the forecast is invalidated), so d is low (toward 0.0), producing low or negative effective extraction (they experience net benefit). Business-as-usual advocacy is also a beneficiary: the timing uncertainty provides rhetorical ammunition for deferral, with arbitrage exit (can shift to other arguments if the window closes without collapse), so d is low, producing net benefit. Resource allocation bodies are partial victims: organized institutional actors with some agency, but constrained by the need to justify investments with quantitative forecasts, so d is moderate, producing moderate effective extraction. The analytical observer's d is derived from the victim structure (the constraint extracts from planning bodies) but modulated by analytical exit options, producing moderate effective extraction. The directionality pattern shows extraction flowing from trapped planning bodies toward beneficiaries with arbitrage options (authors, business-as-usual advocates), with organized institutional actors experiencing mixed effects.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the snare classification is perspectival, not absolute. From the position of collapse preparedness planning, the constraint is a snare: forced reliance on an epistemically compromised forecast with no exit. From the position of the recalibration authors, the constraint is rope: transparent coordination with stated limitations. From the position of business-as-usual advocacy, the constraint is also rope: the uncertainty provides legitimate grounds to question urgency. The mandatrophy is not 'is this a snare or a rope?' but 'from which structural position are you measuring?' The analytical observer sees the asymmetry that creates the trap: the forecast is loud, the disclaimers are quiet, and the structural position of planning bodies (trapped by lead-time requirements, no alternative integrative model) converts methodological transparency into extraction. The constraint is a snare for those who cannot exit, rope for those who can, and tangled rope for those in between. The classification is indexical: it depends on power, exit options, and time horizon. The mandate (provide integrative long-range forecasts for global sustainability planning) has not outlived its function, but the execution (rising-edge-only optimization, constant-relationship assumptions, post-collapse invalidity) creates extraction for those structurally required to act on the output.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parameter_sensitivity_threshold,
    'What magnitude of parameter perturbation within NRMSD-equivalent range would shift the collapse window outside 2024-2030?',
    'Systematic sensitivity analysis: Monte Carlo sampling of parameter space within empirical error bounds; identification of parameter combinations that shift peak timing by >5 years',
    'If threshold is low (small perturbations shift timing substantially): the 2024-2030 window is fragile, and the snare classification is confirmed. If threshold is high (large perturbations required): the window is robust, and the constraint is closer to tangled_rope (genuine forecast contaminated by methodological limitations rather than pure extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parameter_sensitivity_threshold, empirical, 'Sensitivity of collapse timing to parameter perturbations').

omega_variable(
    regime_change_detection_lag,
    'How long after a structural regime change (e.g., degrowth transition, technological breakthrough, geopolitical fragmentation) would World3''s constant-relationship assumption produce detectably invalid forecasts?',
    'Historical backtesting: apply World3 structure to past regime changes (1970s oil shocks, 1990s Soviet collapse, 2008 financial crisis) and measure forecast error accumulation post-transition',
    'If detection lag < 2 years: the model''s post-collapse invalidity is a fundamental limit, and any forecast through regime change is extractive. If detection lag > 5 years: the model has some structural robustness, and the constraint is closer to scaffold (temporary limitation being addressed by model development).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_change_detection_lag, empirical, 'Time lag for regime change to invalidate constant-relationship models').

omega_variable(
    alternative_model_availability,
    'Do alternative integrative models with comparable scope and empirical grounding exist that provide tighter confidence intervals or explicit regime-change handling?',
    'Systematic review of global integrated assessment models (IAMs), earth system models with socio-economic coupling, and agent-based models at comparable scale; comparison of timing uncertainty and structural assumptions',
    'If alternatives exist with tighter bounds: the World3 timing uncertainty is a methodological choice, and the snare classification is confirmed (suppression of alternatives). If no alternatives exist: the constraint is closer to mountain (inherent epistemic limit of integrative modeling at this scale).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_model_availability, empirical, 'Existence of alternative models with lower timing uncertainty').

omega_variable(
    rising_edge_overfitting_risk,
    'Does optimizing only for rising-edge fit (1970-2023) create systematic bias toward earlier collapse dates by fitting to acceleration rather than equilibrium dynamics?',
    'Comparison of recalibration results using different objective functions: rising-edge only vs. full historical period vs. weighted by data quality; assessment of whether rising-edge optimization systematically shifts collapse timing forward',
    'If systematic bias exists: the 2024-2030 window is an artifact of methodological choice, and the snare classification is confirmed. If no bias: the rising-edge focus is methodologically justified, and the constraint is closer to tangled_rope (legitimate methodological choice with known limitations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rising_edge_overfitting_risk, empirical, 'Whether rising-edge optimization biases collapse timing estimates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collapse_timing_uncertainty, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_2014, collapse_timing_uncertainty, theater_ratio, 0, 0.42).
narrative_ontology:measurement(theater_2017, collapse_timing_uncertainty, theater_ratio, 3, 0.48).
narrative_ontology:measurement(theater_2020, collapse_timing_uncertainty, theater_ratio, 6, 0.53).
narrative_ontology:measurement(theater_2023, collapse_timing_uncertainty, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(extractiveness_2014, collapse_timing_uncertainty, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(extractiveness_2017, collapse_timing_uncertainty, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(extractiveness_2020, collapse_timing_uncertainty, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(extractiveness_2023, collapse_timing_uncertainty, base_extractiveness, 9, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(suppression_2014, collapse_timing_uncertainty, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(suppression_2017, collapse_timing_uncertainty, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(suppression_2020, collapse_timing_uncertainty, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(suppression_2023, collapse_timing_uncertainty, suppression_requirement, 9, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collapse_timing_uncertainty, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of three structural dependencies: proxy_measurement_validity (mountain — the empirical adequacy of World3's aggregate proxies), recalibration_interpretive_validity (tangled_rope — whether rising-edge-only optimization is methodologically justified), and regime_change_structural_break (piton — the degraded assumption that socio-economic relationships remain constant through regime change). Each upstream constraint has its own extractiveness reflecting its specific epistemic status; collapse_timing_uncertainty has its own extractiveness reflecting the forced reliance on a forecast with stated limitations. The timing uncertainty is not reducible to the upstream constraints — it is a distinct structural phenomenon that emerges from their combination with the institutional requirement for quantitative long-range planning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
