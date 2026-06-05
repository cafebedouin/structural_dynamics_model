% ============================================================================
% CONSTRAINT STORY: finnish_ubi_experiment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_finnish_ubi_experiment, []).

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
 *   constraint_id: finnish_ubi_experiment
 *   human_readable: Finnish Basic Income Experiment (2017-2018)
 *   domain: economic/social
 *
 * SUMMARY:
 *   Finland's 2017-2018 basic income experiment represents a hybrid
 *   constraint combining genuine social policy coordination (reducing
 *   bureaucratic overhead, providing income security) with extractive
 *   data-gathering and political legitimacy operations. The constraint
 *   exhibits multiple classifications depending on the observer's structural
 *   position: the precarious worker trapped in the experiment sees extraction
 *   (snare); the state administration sees a low-cost data and legitimacy
 *   mechanism (rope); progressive reformers see a temporary pilot with sunset
 *   logic (scaffold); international policy elites see a performative ritual
 *   (piton); the welfare bureaucracy sees an institutional threat that it
 *   paradoxically maintains (piton); and the analytical observer risks
 *   naturalizing the experiment's design choices as inherent to
 *   redistribution policy (false mountain). The constraint's theater ratio
 *   increased over the 2-year interval from 0.55 to 0.68, reflecting rising
 *   focus on evaluation performance relative to actual income transfer
 *   function — by the final year, the experiment's primary function was
 *   producing publishable findings rather than testing policy feasibility.
 *   This lifecycle drift is typical of tangled-rope constraints where
 *   evaluation procedures gradually displace the primary coordination
 *   function.
 *
 * KEY AGENTS:
 *   - Finnish State Administration: Primary beneficiary (institutional/arbitrage) — extracts data, political legitimacy, and policy options via sunset structure
 *   - Low-income workers and chronically unemployed: Primary victim (powerless/trapped) — participate without exit option; provide behavioral data; face policy uncertainty
 *   - Employed middle-income taxpayers: Secondary victim (moderate/constrained) — fund the experiment; cannot fully exit without emigration; fund transfer without policy certainty
 *   - Progressive reform coalition: Organized beneficiary (organized/constrained) — use experiment to advance longer-term UBI agenda; have constrained exit (policy change is slow)
 *   - Finnish welfare bureaucracy: Institutional actor (institutional/arbitrage) — threatened by UBI bypass logic but maintains administrative legitimacy through inertia
 *   - International UBI advocacy movement: Organized beneficiary (organized/mobile) — extract moral authority and policy momentum from Finnish experiment; can adapt findings to advocacy narrative
 *   - Global technocratic policy infrastructure (OECD, World Bank, academic networks): Institutional observer (institutional/arbitrage) — consume experiment as evidence of rational governance; perform evaluation procedures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(finnish_ubi_experiment, 0.38).
domain_priors:suppression_score(finnish_ubi_experiment, 0.52).
domain_priors:theater_ratio(finnish_ubi_experiment, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(finnish_ubi_experiment, extractiveness, 0.38).
narrative_ontology:constraint_metric(finnish_ubi_experiment, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(finnish_ubi_experiment, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(finnish_ubi_experiment, tangled_rope).
narrative_ontology:human_readable(finnish_ubi_experiment, "Finnish Basic Income Experiment (2017-2018)").
narrative_ontology:topic_domain(finnish_ubi_experiment, "economic/social").

domain_priors:requires_active_enforcement(finnish_ubi_experiment).
narrative_ontology:has_sunset_clause(finnish_ubi_experiment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(finnish_ubi_experiment, finnish_state_apparatus).
narrative_ontology:constraint_beneficiary(finnish_ubi_experiment, labor_market_reformers).
narrative_ontology:constraint_victim(finnish_ubi_experiment, low_income_workers).
narrative_ontology:constraint_victim(finnish_ubi_experiment, welfare_system_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Low-income workers and the chronically unemployed cannot exit the experiment; they bear the full cost of policy uncertainty. The constraint extracts behavioral compliance data (work patterns, consumption, health outcomes) while providing conditional income support that can be terminated. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.53. Classifies as pure extraction despite nominal income benefit.
constraint_indexing:constraint_classification(finnish_ubi_experiment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMPLOYED MIDDLE-INCOME TAXPAYERS (TANGLED ROPE) — Constrained by taxation burden and policy uncertainty; experience both coordination function (social stability, reduced bureaucratic overhead) and asymmetric extraction (funding transfer without guarantees of return). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.40. Mixed experience of genuine coordination and extraction.
constraint_indexing:constraint_classification(finnish_ubi_experiment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINNISH STATE ADMINISTRATION (ROPE) — Benefits from the experiment as a data-gathering and legitimacy-building mechanism. Can pivot findings to justify policy direction (expanded UBI or reversion to means-testing). Extracts behavioral data and political cover with low cost via the 2-year sunset. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.04. Net beneficiary; experiences constraint as coordination tool.
constraint_indexing:constraint_classification(finnish_ubi_experiment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROGRESSIVE REFORM COALITION (SCAFFOLD) — Organized actors (labor unions, social policy NGOs, progressive parties) see the experiment as a sunset mechanism: a temporary unconditional income transfer that gathers evidence for longer-term policy change. The 2-year timeline functions as a legitimate sunset clause. Theater is moderate (performance for evaluation) but declining as real behavioral data accumulates. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.13. Low effective extraction because the coalition perceives agency and a genuine exit path.
constraint_indexing:constraint_classification(finnish_ubi_experiment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GLOBAL TECHNOCRATIC EVALUATION INFRASTRUCTURE (PITON) — International policy communities (OECD, World Bank, academic networks) treat the Finnish experiment as a ritual of evidence-gathering and legitimacy performance. The experiment serves primarily to signal that Finland is rational, data-driven, and responsive to global best practices — the actual findings matter less than the fact that a careful evaluation occurred. theater_ratio≈0.68 reflects the ritualistic nature of large-scale policy experiments and their consumption as policy theater. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(finnish_ubi_experiment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: UBI ADVOCACY MOVEMENT (TANGLED ROPE) — International UBI advocates experience both coordination and extraction. The Finnish experiment coordinates a global conversation about unconditional income and provides evidence for advocacy. However, advocates also extract moral authority and policy momentum from the experiment's framing — the results are filtered through advocacy goals, creating asymmetric benefit (advocacy movement gains legitimacy; Finnish policy space loses control of interpretation). d≈0.48, f(d)≈0.58, σ=1.2 → χ≈0.27.
constraint_indexing:constraint_classification(finnish_ubi_experiment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: FINNISH WELFARE BUREAUCRACY (PITON) — The experiment bypasses the traditional means-testing apparatus (Kela social security administration), creating an institutional threat. However, the welfare bureaucracy persists through inertia despite functional displacement — the experiment doesn't eliminate Kela, it supplements it, and Kela maintains administrative legitimacy even as its effectiveness is questioned. theater_ratio≈0.68 reflects the performative distinction between 'UBI experiment' and 'continuing welfare administration.' d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.16.
constraint_indexing:constraint_classification(finnish_ubi_experiment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears immutable: any cash transfer system must extract data about recipient behavior to justify continued funding; tradeoffs between universality and targeting are intrinsic to redistribution; the experiment's sunset is necessitated by the irreducible uncertainty in extrapolating 2-year localized results to national policy. However, ε=0.38 and suppression=0.52 contradict the mountain classification — the engine detects a false summit, revealing that the 'inherent tradeoff' framing naturalizes contingent design choices.
constraint_indexing:constraint_classification(finnish_ubi_experiment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(finnish_ubi_experiment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(finnish_ubi_experiment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(finnish_ubi_experiment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(finnish_ubi_experiment, TR),
    TR >= 0.70.

:- end_tests(finnish_ubi_experiment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The experiment provides real income support to participants (genuine coordination benefit) but also extracts behavioral compliance data, political legitimacy, and policy options via its sunset structure. The state benefits from the ability to gather evidence without committing to a permanent program, creating asymmetry: participants bear risk of termination; state gains analytical leverage. The value of 0.38 reflects that extraction is significant but not predatory — the income transfer is real, not a facade. Suppression (0.52): Moderate-high. Barriers include: (1) precarious workers cannot exit the experiment; (2) the 2-year sunset creates uncertainty that suppresses long-term decision-making; (3) political debate around 'welfare dependency' creates reputational suppression for claimants; (4) the experiment bypasses the normal democratic process (no parliament vote on UBI, just executive pilot). Theater ratio (0.68): High, and increasing. Over the 2-year interval, theater increased from 0.55 to 0.68 as evaluation activities came to dominate implementation. The experiment's public presentation shifted from 'testing if UBI works' to 'producing evidence for evidence-based policy,' a characteristic Goodhart drift where the metric (publishable findings) becomes the goal, displacing the substantive goal (testing feasibility). The increase reflects rising institutional investment in producing findings.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_habituation_vs_measurement_artifact,
    'Are observed behavioral changes (reduced work hours, improved health outcomes) genuine responses to unconditional income or artifacts of the experimental setup and temporary knowledge of sunset?',
    'Long-term follow-up of treatment and control groups after program termination; comparison with permanent UBI programs in other jurisdictions (Kenya, Stockton CA) with different sunset structures',
    'If genuine: UBI extracts behavioral adaptation data; the mechanism is real. If artifact: the experiment measures political theater rather than policy effect; extractiveness should be lower (ε≈0.25). Classification could shift from tangled_rope to piton for all perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_habituation_vs_measurement_artifact, empirical, 'Whether behavioral changes reflect genuine UBI response or experimental setup artifacts').

omega_variable(
    political_interpretation_capture,
    'Will Finnish policymakers use the experiment''s results to justify their preferred policy direction regardless of findings (toward full UBI or back to means-testing)?',
    'Content analysis of political statements pre- and post-findings; tracking of how different political parties cite the experiment; comparison with comparable policy experiments in other Nordic countries',
    'If true: the experiment extracts legitimacy from appearing evidence-based while serving primarily as political theater (theater_ratio should increase to 0.75+). If false: findings genuinely constrain political choice. Changes mandatrophy classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_interpretation_capture, conceptual, 'Whether political interpretation of results is predetermined despite empirical findings').

omega_variable(
    welfare_system_integrity_degradation,
    'Does the experiment''s existence (and the public debate around it) degrade the legitimacy and perceived fairness of the underlying welfare system for those not in the treatment group?',
    'Survey data on welfare system trust and perceived fairness pre-, during, and post-experiment; analysis of policy changes in means-tested benefits; tracking of welfare fraud rates and bureaucratic appeals during experiment period',
    'If yes: the experiment extracts social cohesion and system legitimacy even if individual recipients benefit. The victim classification ''welfare_system_integrity'' is empirically grounded. If no: the integrity concern is overstated. Affects snare classification for precarious workers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_system_integrity_degradation, empirical, 'Whether UBI experiment undermines welfare system legitimacy for non-participants').

omega_variable(
    global_policy_diffusion_capture,
    'Are Finnish experiment results used selectively by international organizations and advocacy networks to support predetermined policy positions rather than genuinely informing policy debate?',
    'Citation analysis of experiment findings in OECD reports, think tank publications, and academic papers; tracking of how different stakeholders cite same findings for opposite conclusions',
    'If selective: the advocacy movement perspective (tangled_rope) should be reclassified as snare — pure extraction of legitimacy. If balanced: advocacy movement perspective is correctly tangled_rope. Affects interpretation of global institutional perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_policy_diffusion_capture, empirical, 'Whether global policy adoption of experiment results is selective and predetermined').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(finnish_ubi_experiment, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ubi_tr_t0, finnish_ubi_experiment, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ubi_tr_t1, finnish_ubi_experiment, theater_ratio, 1, 0.62).
narrative_ontology:measurement(ubi_tr_t2, finnish_ubi_experiment, theater_ratio, 2, 0.68).

% Extraction over time
narrative_ontology:measurement(ubi_be_t0, finnish_ubi_experiment, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ubi_be_t1, finnish_ubi_experiment, base_extractiveness, 1, 0.33).
narrative_ontology:measurement(ubi_be_t2, finnish_ubi_experiment, base_extractiveness, 2, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(finnish_ubi_experiment, resource_allocation).
narrative_ontology:affects_constraint(finnish_ubi_experiment, nordic_welfare_system_path_dependency).
narrative_ontology:affects_constraint(finnish_ubi_experiment, global_ubi_legitimacy_claims).

% DUAL FORMULATION NOTE:
% The Finnish UBI experiment is downstream of broader Nordic welfare system design and upstream of global UBI policy diffusion. It serves as a local instantiation of the tension between universal and means-tested redistribution, but the experiment itself is a distinct constraint because it extracts behavioral data and political legitimacy while providing temporary income support.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(finnish_ubi_experiment, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
