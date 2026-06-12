% ============================================================================
% CONSTRAINT STORY: optimization_artifact_risk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_optimization_artifact_risk, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: optimization_artifact_risk
 *   human_readable: Optimization Artifact Risk in Multi-Parameter System Dynamics Models
 *   domain: system_dynamics/ecological_economics/computational_modeling
 *
 * SUMMARY:
 *   The optimization artifact risk in multi-parameter system dynamics models
 *   creates a structural tension between the computational tractability of
 *   automated parameter fitting and the epistemic requirement that model
 *   parameters represent genuine system properties rather than mathematical
 *   artifacts. When a 35-parameter World3 model is optimized against
 *   imperfect proxies (e.g., using GDP as a proxy for human welfare), the
 *   optimization process can find local minima that fit the proxy well but
 *   distort the underlying system structure. Sensitivity analysis revealing
 *   different optimal parameter sets for different initial conditions, and
 *   extreme parameter changes like a 662% increase in industrial capital
 *   lifetime, are diagnostic signals that the optimization may be fitting
 *   noise rather than signal. This constraint exhibits rising extraction over
 *   time (0.45 → 0.68) as model complexity has increased faster than
 *   validation capacity, rising suppression (0.50 → 0.72) as institutional
 *   mandates and publication norms have locked in optimization-first
 *   workflows, and rising theater ratio (0.35 → 0.58) as the ritual of
 *   reporting goodness-of-fit statistics has increasingly substituted for
 *   genuine validation of parameter interpretability. The constraint is
 *   downstream of proxy_measurement_validity — if the proxies were perfect,
 *   optimization artifacts would not arise.
 *
 * KEY AGENTS:
 *   - Model Interpretability: Primary victim (powerless/trapped) — abstract epistemic commons that cannot exit or organize; bears full cost when optimization produces uninterpretable parameters
 *   - Downstream Policy Users: Secondary victim (moderate/constrained) — inherit optimized models through institutional mandates; face career risk in questioning published frameworks; high exit cost but not insurmountable
 *   - Model Developers: Mixed position (powerful/mobile) — experience genuine coordination (systematic parameter exploration) alongside extraction (career incentives reward fit metrics over interpretability); can exit but institutional incentives suppress that choice
 *   - Optimization Framework Developers: Primary beneficiary (institutional/arbitrage) — solve legitimate coordination problem of high-dimensional parameter search; benefit from tool adoption; minimal extraction; can pivot to alternatives
 *   - Open Validation Coalition: Organized agents (organized/constrained) — advocate for ensemble modeling and uncertainty quantification; see optimization artifact risk as temporary with sunset as norms mature
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function (reproducible calibration) embedded with substantial extraction (systematic bias toward proxy-fitting over structural validity)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(optimization_artifact_risk, 0.68).
domain_priors:suppression_score(optimization_artifact_risk, 0.72).
domain_priors:theater_ratio(optimization_artifact_risk, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(optimization_artifact_risk, extractiveness, 0.68).
narrative_ontology:constraint_metric(optimization_artifact_risk, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(optimization_artifact_risk, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(optimization_artifact_risk, snare).
narrative_ontology:human_readable(optimization_artifact_risk, "Optimization Artifact Risk in Multi-Parameter System Dynamics Models").
narrative_ontology:topic_domain(optimization_artifact_risk, "system_dynamics/ecological_economics/computational_modeling").

domain_priors:requires_active_enforcement(optimization_artifact_risk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(optimization_artifact_risk, optimization_framework_developers).
narrative_ontology:constraint_beneficiary(optimization_artifact_risk, model_publication_incentives).
narrative_ontology:constraint_victim(optimization_artifact_risk, model_interpretability).
narrative_ontology:constraint_victim(optimization_artifact_risk, downstream_policy_users).
narrative_ontology:constraint_victim(optimization_artifact_risk, epistemic_reliability_of_field).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(optimization_artifact_risk, model_developers).
narrative_ontology:constraint_victim(optimization_artifact_risk, model_developers).
narrative_ontology:constraint_vindicates(optimization_artifact_risk, computational_optimization_sufficiency).
narrative_ontology:constraint_vindicates(optimization_artifact_risk, parameter_space_completeness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Software developers and methodologists who create optimization tools for system dynamics models. They set the technical agenda by defining what 'good' parameter fitting means (convergence criteria, goodness-of-fit metrics, search algorithms). They benefit from tool adoption through citations, funding, and professional reputation. They face minimal extraction — if optimization proves inadequate, they can pivot to alternative methods (Bayesian inference, ensemble modeling, agent-based approaches). They experience the constraint as pure coordination: their frameworks solve the legitimate problem of exploring high-dimensional parameter spaces.
narrative_ontology:constraint_stakeholder(optimization_artifact_risk, optimization_framework_developers, agenda_setter,
    institutional, immediate, arbitrage, global).

% Researchers who develop and publish system dynamics models using optimization frameworks. They benefit from career incentives: publications in high-impact journals, citations, funding, professional recognition. They also pay a cost: the optimization process may produce uninterpretable parameters, and they face reputational risk if the model's predictions fail. They can exit by choosing alternative modeling approaches (qualitative system dynamics, agent-based models, Bayesian calibration), but institutional incentives (publication bias, peer review standards, funding priorities) suppress that choice. They experience genuine coordination (the optimization framework enables systematic parameter exploration) alongside extraction (career rewards privilege goodness-of-fit over interpretability).
narrative_ontology:constraint_stakeholder(optimization_artifact_risk, model_developers, beneficiary,
    powerful, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(optimization_artifact_risk, model_developers, payer).

% Policy analysts, government agencies, and decision-makers who inherit optimized system dynamics models for policy evaluation. They bear the cost: policy recommendations rest on parameters that may be optimization artifacts rather than system properties, and they face career risk if policies fail due to model inadequacy. They face high exit costs: institutional mandates require using 'state-of-art' models, no alternative models exist at comparable scope, and questioning published frameworks carries professional risk. Exit is possible (they could commission alternative models or use simpler heuristics) but costly. They experience severe extraction — they inherit uninterpretable models and bear the risk of policy failures.
narrative_ontology:constraint_stakeholder(optimization_artifact_risk, downstream_policy_users, payer,
    moderate, biographical, constrained, national).

% Organized groups advocating for ensemble modeling, sensitivity analysis, and parameter uncertainty quantification in system dynamics. They include methodologists, open-science advocates, and researchers building alternative validation frameworks. They neither collect from the optimization regime nor pay its costs directly — they observe the artifact risk and work to build alternatives. They face resource barriers (ensemble methods are computationally expensive) and norm barriers (publication bias favors optimized point estimates), but they have agency to build alternatives and see a sunset: as computational resources grow and validation norms mature, the field will adopt multi-model ensembles and Bayesian parameter distributions.
narrative_ontology:constraint_stakeholder(optimization_artifact_risk, open_validation_coalition, observer,
    organized, generational, constrained, global).

% The abstract collective good of model interpretability — the epistemic commons that model parameters should represent genuine system properties rather than mathematical artifacts. This is not an agent but a non-agent entity kept for narrative completeness. It bears the full cost when optimization produces uninterpretable parameters (e.g., 662% increase in industrial capital lifetime with no mechanistic justification), and it cannot exit or organize. It is excluded from directionality derivation per the agent-hood gate.
narrative_ontology:constraint_stakeholder(optimization_artifact_risk, model_interpretability, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(optimization_artifact_risk, model_interpretability).

% The abstract collective good of epistemic reliability in system dynamics and ecological economics — the field's capacity to produce models whose parameters and predictions are trustworthy. This is not an agent but a non-agent entity. It bears the cost when optimization artifacts contaminate the literature and mislead policy, and it cannot exit or organize. It is excluded from directionality derivation per the agent-hood gate.
narrative_ontology:constraint_stakeholder(optimization_artifact_risk, epistemic_reliability_of_field, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(optimization_artifact_risk, epistemic_reliability_of_field).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The optimization framework solves the genuine coordination problem of exploring high-dimensional parameter spaces in complex system dynamics models. Without automated optimization, 35-parameter models would be intractable — manual parameter tuning cannot systematically search the space, and reproducible calibration would be impossible. The coordination function is real and valuable.
% TRANSFER_FUNCTION: The optimization process transfers interpretability from the model to the developers' career capital. Parameters that should represent physical or social system properties become free variables adjusted to fit imperfect proxies. Model developers capture career benefits (publications, citations, funding) during the window before validation failures surface. Downstream policy users and the epistemic commons bear the cost of uninterpretable models whose parameters may be mathematical artifacts rather than system properties.
% ABSENT_VOICES: The absent voices are future policy-makers who will inherit models whose parameters are artifacts, and the epistemic commons of model interpretability which has no institutional advocate. Also absent: researchers who attempted sensitivity analysis or ensemble approaches but could not publish because journals favor optimized point estimates. The optimization framework's dominance arose partly because dissenting methodological voices (Bayesian skeptics, qualitative system dynamicists, complexity theorists warning about parameter identifiability) were not in the room when publication norms and institutional standards were set.
% DISAPPEARANCE_RATIONALE: If the optimization artifact risk disappeared overnight (if optimization always found parameters that genuinely represent system dynamics rather than fitting proxies), the world would rearrange substantially: policy recommendations would become more reliable, model developers would face different career incentives (interpretability would matter as much as fit), downstream users would trust model parameters, and the epistemic commons would be healthier. The constraint's presence shapes institutional arrangements (publication norms, peer review standards, funding priorities) and career structures (what counts as a 'good' model). Its disappearance would require rearrangement.
% FOUNDING_PROBLEM: The founding problem was computational intractability of manual parameter tuning in high-dimensional system dynamics models. Early system dynamics models (1970s-1980s) had fewer parameters and were calibrated through expert judgment and iterative refinement. As models grew in complexity (World3 with 35+ parameters, integrated assessment models with 100+ parameters), manual tuning became impossible. Automated optimization was introduced to solve this genuine coordination problem: systematic parameter search, reproducible calibration, and computational tractability.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (computational intractability of manual tuning) is still live and is corroborated by methodologists outside the beneficiary set: Bayesian modelers acknowledge that high-dimensional parameter spaces require computational search methods; complexity theorists confirm that manual tuning cannot systematically explore the space; even critics of optimization (ensemble modeling advocates, sensitivity analysis proponents) agree that some form of automated parameter search is necessary for complex models. The problem is real. What has changed is that the solution (point-estimate optimization) has been captured by career incentives that reward goodness-of-fit over interpretability, and the optimization process now systematically produces artifacts alongside genuine parameter estimates.
narrative_ontology:disappearance_verdict(optimization_artifact_risk, world_rearranges).
narrative_ontology:founding_problem_status(optimization_artifact_risk, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MODEL INTERPRETABILITY (SNARE) — The abstract collective good of model interpretability cannot exit the optimization regime and bears full extraction cost. When optimization finds local minima that fit proxies rather than dynamics, the model's explanatory power degrades but the interpretability commons has no advocate and no escape mechanism. Maximum experienced extraction.
constraint_indexing:constraint_classification(optimization_artifact_risk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM POLICY USERS (SNARE) — Policy analysts and decision-makers who inherit optimized models face high exit costs: institutional mandates require using 'state-of-art' models, career risk attaches to questioning published frameworks, and no alternative models exist at comparable scope. Constrained rather than trapped because exit is possible at high professional cost, but the extraction is severe — policy recommendations rest on parameters that may be optimization artifacts rather than system properties.
constraint_indexing:constraint_classification(optimization_artifact_risk, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MODEL DEVELOPERS (TANGLED ROPE) — Researchers developing and publishing optimized models experience genuine coordination (the optimization framework enables systematic parameter exploration and model comparison) alongside extraction (career incentives reward goodness-of-fit metrics over interpretability; publication bias favors optimized models over sensitivity analysis). Mobile exit because researchers can choose alternative modeling approaches, but institutional incentives suppress that choice.
constraint_indexing:constraint_classification(optimization_artifact_risk, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: OPTIMIZATION FRAMEWORK DEVELOPERS (ROPE) — Software developers and methodologists who create optimization tools experience the constraint as pure coordination: their frameworks solve the legitimate problem of exploring high-dimensional parameter spaces. They benefit from adoption (citations, funding, tool usage) and face minimal extraction — they can pivot to alternative methods if optimization proves inadequate. Net beneficiaries with arbitrage-level exit.
constraint_indexing:constraint_classification(optimization_artifact_risk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN VALIDATION COALITION (SCAFFOLD) — Organized groups advocating for sensitivity analysis, ensemble modeling, and parameter uncertainty quantification see the optimization artifact risk as a temporary coordination failure with a sunset: as computational resources grow and validation norms mature, the field will adopt multi-model ensembles and Bayesian parameter distributions rather than single optimized point estimates. Estimated sunset: 15-25 years for norms to shift in ecological economics and integrated assessment modeling.
constraint_indexing:constraint_classification(optimization_artifact_risk, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, optimization in complex system models serves a genuine coordination function (systematic parameter exploration, reproducible calibration) but embeds substantial extraction: the optimization process systematically privileges goodness-of-fit to imperfect proxies over structural validity, and the resulting parameter sets (e.g., 662% increase in industrial capital lifetime) may be mathematical artifacts rather than system properties. The constraint requires active enforcement through publication norms, peer review standards, and institutional adoption mandates.
constraint_indexing:constraint_classification(optimization_artifact_risk, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(optimization_artifact_risk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(optimization_artifact_risk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(optimization_artifact_risk, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(optimization_artifact_risk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(optimization_artifact_risk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The optimization process systematically extracts interpretability from the model: parameters that should represent physical or social system properties (capital lifetime, resource depletion rates, pollution absorption capacity) become free variables adjusted to fit imperfect proxies. The 662% increase in industrial capital lifetime is not a discovered fact about industrial systems — it is a mathematical compensation for proxy measurement error. Model developers capture career benefits (publications, citations, funding) during the window before validation failures surface, while downstream policy users and the epistemic commons bear the cost of uninterpretable models. The extraction is not total (0.68 rather than 0.85+) because some optimization genuinely improves model fit to real dynamics, and sensitivity analysis can reveal artifacts when performed. Suppression (0.72): High. Significant barriers prevent escape from optimization-first workflows: institutional mandates require using 'state-of-art' optimized models, publication bias strongly favors papers reporting optimized parameters over papers reporting sensitivity analysis or parameter uncertainty, peer review standards treat goodness-of-fit as sufficient validation, computational cost of ensemble approaches creates resource barriers, and career risk attaches to questioning established optimization frameworks. Suppression has increased over the interval as these institutional locks have hardened. Theater ratio (0.58): Moderate-high. Substantial performative content: reporting R-squared and goodness-of-fit statistics has become a ritual that substitutes for genuine validation of parameter interpretability; optimization convergence is treated as evidence of model validity rather than as a mathematical property of the search algorithm; extreme parameter values are often reported without mechanistic justification or plausibility checks; sensitivity analysis is performed pro forma but rarely changes model structure or parameter bounds. The theater has increased as model complexity has outpaced validation capacity.
 *
 * PERSPECTIVAL GAP:
 *   The optimization framework developers see pure coordination (Rope) — they are solving the legitimate problem of exploring high-dimensional parameter spaces, and they benefit from tool adoption with minimal extraction. Model developers see mixed coordination and extraction (Tangled Rope) — the optimization framework enables systematic work but career incentives reward goodness-of-fit over interpretability. Downstream policy users see severe extraction (Snare) — they inherit models whose parameters may be artifacts, face high exit costs due to institutional mandates, and bear the risk of policy failures. Model interpretability as an abstract commons sees maximum extraction (Snare) — it cannot exit, cannot organize, and bears the full cost of optimization artifacts contaminating the literature. The open validation coalition sees a temporary problem with a sunset (Scaffold) — ensemble methods and uncertainty quantification will eventually replace point-estimate optimization as norms mature. The analytical observer sees the full structure (Tangled Rope) — genuine coordination function embedded with substantial extraction that requires active enforcement through publication norms and institutional mandates.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Optimization framework developers are primary beneficiaries with arbitrage exit — they collect from tool adoption and can pivot to alternatives, yielding low d and negative or low chi (experienced as coordination). Model developers are in a mixed position — they benefit from career incentives but also bear some interpretability cost; powerful with mobile exit yields moderate d and moderate chi (experienced as tangled rope). Downstream policy users are victims with constrained exit — they inherit uninterpretable models and face high exit costs; moderate power with constrained exit yields high d and high chi (experienced as snare). Model interpretability is a victim with trapped exit — the abstract commons cannot escape and bears maximum extraction; powerless with trapped exit yields maximum d and maximum chi (experienced as snare). The open validation coalition is organized with constrained exit — they have agency to build alternatives but face resource and norm barriers; organized power with constrained exit yields moderate d and moderate chi, but the sunset logic shifts the classification to scaffold. The analytical observer has analytical power and exit — they see the full structure without being trapped in any position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that optimization in system dynamics models serves a genuine coordination function (systematic parameter exploration, reproducible calibration, computational tractability) that has been captured by an extraction mechanism (career incentives reward proxy-fitting over structural validity, publication bias suppresses sensitivity analysis, institutional mandates lock in optimization-first workflows). The coordination function is real — without optimization, 35-parameter models would be intractable. The extraction is also real — optimization systematically privileges goodness-of-fit to imperfect proxies over parameter interpretability, and the resulting artifacts (extreme parameter values, initial-condition sensitivity, convergence to local minima) contaminate the literature and mislead policy. The tangled rope classification at the analytical level captures this dual structure. The snare classification from the victim perspectives (model interpretability, downstream policy users) captures their structural reality — they bear extraction costs with limited exit. The rope classification from the beneficiary perspective (optimization framework developers) captures their genuine coordination experience. The scaffold classification from the organized coalition captures the real sunset logic — ensemble methods and uncertainty quantification are maturing alternatives. No single type is 'the' answer — the presheaf over observation sites is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    local_minima_vs_global_structure,
    'Do different optimal parameter sets found from different initial conditions represent genuine structural uncertainty in the system, or are they optimization artifacts reflecting the proxy measurement''s inadequacy?',
    'Out-of-sample validation on held-out time periods; comparison of parameter sets'' physical plausibility; convergence analysis across multiple optimization algorithms with different search strategies',
    'If genuine uncertainty: the optimization is revealing real ambiguity and the constraint is coordination (Rope from more perspectives). If artifacts: the optimization is generating spurious precision and the constraint is extraction (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_minima_vs_global_structure, empirical, 'Whether parameter variation reflects system uncertainty or optimization artifacts').

omega_variable(
    parameter_magnitude_interpretability,
    'When optimization produces extreme parameter changes (e.g., 662% increase in industrial capital lifetime), does this reflect a genuine discovery about system behavior or a mathematical compensation for proxy measurement error?',
    'Independent empirical validation of extreme parameter values against historical data; mechanistic analysis of whether the parameter change has a plausible causal story; sensitivity of extreme values to proxy choice',
    'If genuine discovery: optimization is revealing non-obvious system properties. If compensation: optimization is fitting noise and the interpretability cost is severe.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parameter_magnitude_interpretability, empirical, 'Whether extreme optimized parameters reflect system properties or proxy fitting').

omega_variable(
    ensemble_vs_point_estimate_sufficiency,
    'Is a single optimized parameter set ever sufficient for policy-relevant system dynamics models, or does the optimization artifact risk require ensemble approaches by default?',
    'Comparison of policy recommendation robustness: single optimized model vs. ensemble of models with parameter distributions; historical analysis of policy failures traceable to point-estimate overconfidence',
    'If single estimates sufficient: optimization artifact risk is overstated and the constraint is weaker. If ensembles required: current practice is systematically inadequate and the extraction is more severe than base metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ensemble_vs_point_estimate_sufficiency, conceptual, 'Whether point estimates are ever adequate for policy-relevant models').

omega_variable(
    proxy_goodness_of_fit_threshold,
    'What threshold of goodness-of-fit to imperfect proxies justifies confidence in optimized parameters, and how does this threshold vary with model complexity and parameter count?',
    'Cross-validation analysis correlating fit quality with out-of-sample prediction accuracy; information-theoretic analysis of parameter identifiability given data quality; comparison across model complexity classes',
    'If threshold is high and well-defined: optimization can be used safely with appropriate guardrails. If threshold is low or undefined: current practice lacks epistemic foundation and the snare is deeper than recognized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proxy_goodness_of_fit_threshold, empirical, 'Goodness-of-fit threshold for parameter confidence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(optimization_artifact_risk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_early_optimization, optimization_artifact_risk, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_mid_optimization, optimization_artifact_risk, theater_ratio, 3, 0.42).
narrative_ontology:measurement(theater_late_optimization, optimization_artifact_risk, theater_ratio, 6, 0.51).
narrative_ontology:measurement(theater_current_optimization, optimization_artifact_risk, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(extract_early_optimization, optimization_artifact_risk, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(extract_mid_optimization, optimization_artifact_risk, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(extract_late_optimization, optimization_artifact_risk, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(extract_current_optimization, optimization_artifact_risk, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(suppress_early_optimization, optimization_artifact_risk, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(suppress_mid_optimization, optimization_artifact_risk, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(suppress_late_optimization, optimization_artifact_risk, suppression_requirement, 6, 0.66).
narrative_ontology:measurement(suppress_current_optimization, optimization_artifact_risk, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(optimization_artifact_risk, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of proxy_measurement_validity. If the proxies were perfect measures of the target quantities (human welfare, environmental health, resource availability), optimization would find parameters that genuinely represent system dynamics rather than fitting measurement error. The optimization artifact risk is the amplification mechanism through which proxy imperfection propagates into model structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
