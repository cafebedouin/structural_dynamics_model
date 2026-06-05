% ============================================================================
% CONSTRAINT STORY: institutional_metrics_gaming
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_metrics_gaming, []).

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
 *   constraint_id: institutional_metrics_gaming
 *   human_readable: Institutional Metrics Gaming and Performance Measurement Distortion
 *   domain: organizational_governance/incentive_systems
 *
 * SUMMARY:
 *   Institutional metrics gaming emerges when organizations adopt
 *   quantitative performance measures to coordinate effort and allocate
 *   resources, but agents rationally optimize the metrics rather than the
 *   underlying mission functions. The constraint creates a structural tension
 *   between measurable outputs (which drive funding and career advancement)
 *   and unmeasured mission functions (mentorship, exploratory work,
 *   institutional culture, stakeholder trust) that generate value but resist
 *   quantification. Theater increases over time as institutions become more
 *   sophisticated at measurement and more dependent on metrics for legitimacy
 *   — the performative apparatus grows even as the correlation between
 *   metrics and actual institutional health declines. This constraint
 *   exhibits tangled rope structure: genuine coordination function
 *   (transparency, comparable measurement) coexists with asymmetric
 *   extraction (resources flow toward easily-measured functions, away from
 *   unmeasured ones) and suppresses alternatives (institutions without
 *   metric-dependent budgeting are harder to justify to funders and
 *   overseers). The empirical trajectory shows metrics gaming intensifying
 *   over 10-15 years: theater rises as measurement infrastructure expands;
 *   extractiveness rises as institutional dependence on metrics deepens;
 *   suppression stabilizes at high levels because the barriers to
 *   unmeasured-function advocacy are structural, not dependent on any
 *   particular measurement system.
 *
 * KEY AGENTS:
 *   - Measured Institution: Primary beneficiary (institutional/arbitrage) — captures resource concentration and legitimacy during metrics-optimized budget cycles; can shift measurement focus
 *   - Metrics Designers: Secondary beneficiary (institutional/arbitrage) — architects of measurement systems benefit from their adoption; design metrics to align with institutional priorities
 *   - Unmeasured Mission Functions: Primary victim (powerless/trapped) — mentorship, exploratory work, community trust cannot exit institution or find alternative scale; defunded as metrics consolidate budgets
 *   - External Stakeholders: Secondary victim (moderate/constrained) — served by unmeasured functions but have limited visibility into metrics distortion; constrained by reliance on institutional services
 *   - Field Epistemic Validity: Systemic victim (powerless/trapped) — when research institutions game publication metrics or teaching metrics, field-level validity suffers; collective good with no advocate
 *   - Frontline Workers: Mixed position (moderate/constrained) — benefit from some metric feedback, harmed by metrics-driven workload and resource reallocation; career-path dependent, cannot fully exit
 *   - Accountability Reform Movement: Organized agents (organized/constrained) — believe multi-dimensional and participatory assessment can provide sunset for metrics gaming; constrained by institutional resistance to reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_metrics_gaming, 0.58).
domain_priors:suppression_score(institutional_metrics_gaming, 0.65).
domain_priors:theater_ratio(institutional_metrics_gaming, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_metrics_gaming, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_metrics_gaming, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_metrics_gaming, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_metrics_gaming, tangled_rope).
narrative_ontology:human_readable(institutional_metrics_gaming, "Institutional Metrics Gaming and Performance Measurement Distortion").
narrative_ontology:topic_domain(institutional_metrics_gaming, "organizational_governance/incentive_systems").

domain_priors:requires_active_enforcement(institutional_metrics_gaming).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_metrics_gaming, measured_institution).
narrative_ontology:constraint_beneficiary(institutional_metrics_gaming, metrics_designers).
narrative_ontology:constraint_victim(institutional_metrics_gaming, unmeasured_mission_functions).
narrative_ontology:constraint_victim(institutional_metrics_gaming, external_stakeholders).
narrative_ontology:constraint_victim(institutional_metrics_gaming, field_epistemic_validity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNMEASURED MISSION FUNCTION (SNARE) — Functions that cannot be easily quantified (mentorship, exploratory research, institutional culture, community trust) face structural defunding as metrics-optimized budgets consolidate resources toward measurable outputs. These functions are trapped: they cannot exit the institution, cannot be performed elsewhere at scale, and have no advocate in the metrics system. Maximum extraction with high suppression.
constraint_indexing:constraint_classification(institutional_metrics_gaming, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FRONTLINE WORKER (TANGLED ROPE) — Teachers, nurses, social workers, and researchers experience metrics as both enabling and extractive. Quantitative feedback improves some practices; metrics also incentivize documenting work over doing work, generating compliance labor. Constrained by career path dependence and licensing — cannot fully exit but can moderate compliance. Mixed coordination-extraction.
constraint_indexing:constraint_classification(institutional_metrics_gaming, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: METRICS-GAMING ADMINISTRATOR (ROPE) — Institutional managers experience metrics as a coordination mechanism enabling transparent comparison and resource allocation. Metrics provide cover for resource decisions ('the numbers made us do it'). Beneficiary with full exit: can change institutions, redesign metrics, or shift focus. Net beneficiary with arbitrage.
constraint_indexing:constraint_classification(institutional_metrics_gaming, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ACCOUNTABILITY REFORM MOVEMENT (SCAFFOLD) — Organized actors (auditors, oversight bodies, public advocates) frame metrics gaming as a temporary coordination failure with a sunset: multi-dimensional assessment, participatory evaluation, and outcome measurement are building alternative accountability pathways. These agents see constrained exit through reform but believe the extraction mechanism has finite lifespan. Theater high initially, declining with reform adoption.
constraint_indexing:constraint_classification(institutional_metrics_gaming, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: METRICS INFRASTRUCTURE (PITON) — The apparatus of measurement itself (data systems, compliance reporting, audit trails) persists through institutional inertia. Most organizations know their metrics are gamed, that outputs diverge from outcomes, and that measurement theater consumes resources — yet continue because alternatives aren't fully deployed and changing the system is disruptive. High theater, low functional verification. Extractiveness ≤ 0.25 but theater ≥ 0.70.
constraint_indexing:constraint_classification(institutional_metrics_gaming, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GOODHART'S LAW VIEW (MOUNTAIN) — From a universalizing perspective, metrics gaming appears as a natural law: 'When a measure becomes a target, it ceases to be a good measure' (Goodhart's Law). This perspective sees the constraint as an immutable feature of incentive design itself — all quantitative systems must eventually be gamed because agents rationally optimize what they're measured on. However, the structural data reveals this as false naturalization: the extraction is contingent on specific institutional choices, not inherent to measurement per se.
constraint_indexing:constraint_classification(institutional_metrics_gaming, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_metrics_gaming_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_metrics_gaming, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_metrics_gaming, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_metrics_gaming, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_metrics_gaming, TR),
    TR >= 0.70.

:- end_tests(institutional_metrics_gaming_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Metrics gaming creates genuine extraction: resources diverted from unmeasured functions toward measured ones, and effort spent on compliance and gaming represents lost productive capacity. But extraction is not maximal (snare-level) because some coordination benefit exists — metrics do enable comparison and allocation in large institutions. The measurement of extractiveness reflects both the real cost (defunded mentorship, foregone exploration) and the real benefit (improved transparency, reduced nepotism in some allocation decisions). Suppression (0.65): High. The barriers to unmeasured-function advocacy are substantial: they cannot produce scorable outputs, lack champions in metrics-dependent budgeting, and face the presumption that 'if it mattered, we'd measure it.' Alternative institutional forms (peer governance, mission-driven budgeting, stakeholder participation) exist but require institutional change that is itself suppressed by metrics-dependent funders. Theater ratio (0.81): Very high and increasing. Measurement infrastructure is substantially performative: institutions maintain elaborate reporting systems that bear little correlation to actual institutional health; metrics are gamed by everyone involved; the appearance of measurement discipline matters more than actual measurement validity. The trajectory shows theater increasing as institutions deploy more sophisticated gaming techniques while maintaining measurement legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The measured institution (beneficiary) sees metrics as a coordination mechanism enabling transparent comparison and rational resource allocation — they experience it as rope. The frontline worker sees mixed coordination (feedback improves some practices) and extraction (compliance labor, resources diverted from core mission) — they experience it as tangled rope. Unmeasured functions have no perspective within the metrics system; they experience pure extraction (snare). The reform movement sees a contingent institutional arrangement with a sunset (scaffold) — they believe multi-dimensional assessment can dissolve the extraction mechanism. The metrics infrastructure itself is a piton: most organizations acknowledge that their metrics are gamed and that measurement theater consumes resources, yet persist because alternative accountability systems haven't fully replaced them. The analytical observer risks seeing Goodhart's Law as a natural law — metrics gaming is 'inevitable' — but the structural data shows this as naturalization: the extraction is contingent on specific institutional choices (single-metric optimization, metrics-dependent budgeting, suppression of alternatives).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality reflects each agent's position in the extraction flow. Measured institutions and metrics designers are beneficiaries (low d) with arbitrage exit — they can change metrics, shift institutional focus, or move to different organizations. Their experience of the constraint is as coordination (rope) because they benefit from the transparency and resource concentration. Unmeasured functions are trapped victims (high d) with no exit — the functions are core to institutional mission but cannot produce metrics; they experience snare. Frontline workers are constrained-exit victims (moderate d) — they cannot fully exit but can moderate compliance; they experience mixed extraction and benefit. Reformers are organized agents (moderate d) with constrained exit — they see an exit path (metric reform) but require institutional change to reach it. The piton perspective reflects that the metrics infrastructure itself has low experienced extraction (it's not extracting in the agent_power:institutional sense) but very high theater (the measurement legitimacy persists despite low functional validity). The mountain perspective risks false naturalization by attributing extraction to Goodhart's Law rather than institutional design choices.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (ε=0.58): The tangled rope classification resolves the mandatrophy by showing that metrics gaming simultaneously coordinates (legitimate transparency/allocation function) and extracts (diverts resources from unmeasured functions, suppresses alternatives). The constraint is not pure coordination (rope) because the extraction is asymmetric and sustained by suppression — institutions rationally continue gaming because metrics-dependent funders reward it. The constraint is not pure extraction (snare) because genuine coordination benefit exists — metrics do improve allocation in large organizations and do reduce some forms of nepotism. The mandatrophy is resolved by the requirement that tangled rope must have BOTH a coordination function (yes: transparent measurement) AND asymmetric extraction (yes: resource diversion away from unmeasured functions) AND active enforcement (yes: institutional budgeting that rewards measured outputs). The classification prevents mislabeling metrics gaming as either pure coordination (oversimplifying) or pure extraction (ignoring real benefits). The perspectival gap between beneficiary (rope) and victim (snare) is diagnostic: it reveals that the constraint's extraction is not inherent to measurement per se, but contingent on institutional structure that could be changed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    goodhart_inevitability_threshold,
    'At what metric complexity and agent diversity does gaming become inevitable vs. preventable?',
    'Comparative institutional analysis: organizations with multi-stakeholder metrics governance vs. single-objective metrics; longitudinal tracking of gaming emergence; controlled intervention studies with nested metrics systems',
    'If inevitable: mountain classification warranted for some metric systems. If preventable: contingent design choices matter more than structural limits; snare/tangled_rope classifications more appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(goodhart_inevitability_threshold, empirical, 'Threshold for inevitability of metrics gaming').

omega_variable(
    extraction_mechanism_specificity,
    'Is gaming extraction (diversion of effort from unmeasured functions) intentional institutional strategy or emergent organizational behavior?',
    'Analysis of metrics design documents, budget reallocation patterns, and institutional incentive structures; interviews with architects of measurement systems; comparison of explicit vs implicit extraction mechanisms',
    'If intentional strategy: snare/tangled_rope with deliberate suppression. If emergent behavior: snare/rope with unintended consequences; suppression shifts from structural to behavioral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_specificity, empirical, 'Whether metrics gaming extraction is intentional or emergent').

omega_variable(
    multidimensional_assessment_feasibility,
    'Do participatory and multi-dimensional assessment systems actually reduce gaming or simply displace it to harder-to-measure dimensions?',
    'Comparison of institutions using multi-dimensional metrics vs single-metric systems; analysis of gaming patterns under different assessment frameworks; tracking of unintended consequences in reform implementations',
    'If genuine reduction: scaffold sunset is real. If displacement: gaming persists; scaffold is piton (theatrical reform that doesn''t change extraction structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multidimensional_assessment_feasibility, empirical, 'Whether multi-dimensional assessment reduces or displaces gaming').

omega_variable(
    suppression_mechanism_structural_vs_behavioral,
    'Is the suppression of unmeasured functions structural (impossible to measure well) or behavioral (measured-function bias in resource allocation)?',
    'Experimental allocation of resources independent of metrics; analysis of measurement technology advances and whether they reduce gaming; cross-cultural comparison of measurement-dependent vs measurement-resistant institutions',
    'If structural: suppression ≥ 0.65 reflects true measurement limits. If behavioral: suppression could be reduced by alternative governance structures; current high suppression is contingent on metrics-dependent budgeting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_behavioral, empirical, 'Whether suppression is inherent to measurement or contingent on institutional structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_metrics_gaming, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(metrx_tr_t0, institutional_metrics_gaming, theater_ratio, 0, 0.55).
narrative_ontology:measurement(metrx_tr_t5, institutional_metrics_gaming, theater_ratio, 5, 0.68).
narrative_ontology:measurement(metrx_tr_t10, institutional_metrics_gaming, theater_ratio, 10, 0.81).
narrative_ontology:measurement(metrx_tr_t15, institutional_metrics_gaming, theater_ratio, 15, 0.79).

% Extraction over time
narrative_ontology:measurement(metrx_be_t0, institutional_metrics_gaming, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(metrx_be_t5, institutional_metrics_gaming, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(metrx_be_t10, institutional_metrics_gaming, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(metrx_be_t15, institutional_metrics_gaming, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_metrics_gaming, resource_allocation).
narrative_ontology:boltzmann_floor_override(institutional_metrics_gaming, 0.18).
narrative_ontology:affects_constraint(institutional_metrics_gaming, research_publication_gaming).
narrative_ontology:affects_constraint(institutional_metrics_gaming, educational_grade_inflation).
narrative_ontology:affects_constraint(institutional_metrics_gaming, healthcare_diagnostic_coding_optimism).
narrative_ontology:affects_constraint(institutional_metrics_gaming, performance_rating_ceiling_effects).

% DUAL FORMULATION NOTE:
% Institutional metrics gaming is a meta-constraint that affects domain-specific gaming in research metrics, teaching metrics, diagnostic metrics, and performance rating systems. Each domain story has its own ε value reflecting the specific measurement context; this story models the generic structural pattern across all domains. The network edges indicate how gaming in one domain influences gaming in others (e.g., research metrics gaming incentivizes researchers to game teaching metrics; healthcare diagnostic gaming creates broader institutional culture of metric optimization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_metrics_gaming, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
