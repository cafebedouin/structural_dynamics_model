% ============================================================================
% CONSTRAINT STORY: training_volume_dose_response
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_training_volume_dose_response, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: training_volume_dose_response
 *   human_readable: Training Volume Dose-Response Relationship for Mortality Risk Reduction
 *   domain: exercise_physiology/gerontology/preventive_medicine
 *
 * SUMMARY:
 *   The training volume dose-response relationship for mortality risk
 *   reduction is a coordination mechanism that translates biological
 *   adaptation to aerobic exercise into actionable public health and
 *   individual optimization information. Large-scale cohort studies (Arem et
 *   al. 2015, Mandsager et al. 2018) demonstrate that all-cause mortality
 *   risk continues to decline beyond the public health minimum of 150 minutes
 *   per week of moderate-intensity aerobic activity, with additional benefit
 *   observed up to 450-750 minutes per week before marginal returns diminish
 *   substantially. This constraint coordinates decision-making across
 *   multiple scales: individuals allocating discretionary time, public health
 *   agencies calibrating recommendations, and researchers prioritizing
 *   investigation of mechanisms and boundary conditions. The relationship
 *   exhibits minimal extractive overhead — the information is accessible
 *   through multiple channels (peer-reviewed literature, public health
 *   guidelines, popular media), requires no institutional gatekeeping to act
 *   upon, and imposes no structural barriers beyond the time and
 *   physiological capacity required for the activity itself. The constraint's
 *   low theater ratio (0.15) reflects that the dose-response relationship is
 *   grounded in robust epidemiological data with minimal performative
 *   mediation: the hazard ratios are directly measurable, replicable across
 *   cohorts, and mechanistically plausible given known cardiovascular and
 *   metabolic adaptations to endurance training.
 *
 * KEY AGENTS:
 *   - High-Volume Exercisers: Primary beneficiary (moderate/mobile) — gain mortality risk reduction and optimization information; can adjust volume based on evidence
 *   - Public Health Agencies: Institutional beneficiary (institutional/arbitrage) — use dose-response data to calibrate population-level recommendations and resource allocation
 *   - Exercise Science Researchers: Organized beneficiary (organized/mobile) — coordinate research priorities and public communication around dose-response relationships
 *   - Time-Constrained Individuals: Moderate beneficiary (moderate/constrained) — gain actionable information for rational time allocation despite limited discretionary time
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as a coordination mechanism translating biological reality into information with minimal extractive overhead
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(training_volume_dose_response, 0.08).
domain_priors:suppression_score(training_volume_dose_response, 0.12).
domain_priors:theater_ratio(training_volume_dose_response, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(training_volume_dose_response, extractiveness, 0.08).
narrative_ontology:constraint_metric(training_volume_dose_response, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(training_volume_dose_response, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(training_volume_dose_response, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(training_volume_dose_response, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(training_volume_dose_response, rope).
narrative_ontology:human_readable(training_volume_dose_response, "Training Volume Dose-Response Relationship for Mortality Risk Reduction").
narrative_ontology:topic_domain(training_volume_dose_response, "exercise_physiology/gerontology/preventive_medicine").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(training_volume_dose_response, high_volume_exercisers).
narrative_ontology:constraint_beneficiary(training_volume_dose_response, public_health_agencies).
narrative_ontology:constraint_beneficiary(training_volume_dose_response, exercise_science_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIGH-VOLUME EXERCISER (ROPE) — Mobile agent who can adjust training volume based on evidence. Experiences the dose-response relationship as pure coordination: the constraint communicates that additional volume beyond public health minimums continues to reduce mortality risk, enabling informed decision-making about time allocation. Minimal extraction — the information enables optimization without imposing costs.
constraint_indexing:constraint_classification(training_volume_dose_response, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC HEALTH AGENCY (ROPE) — Institutional beneficiary that uses dose-response data to calibrate recommendations. The constraint coordinates population-level messaging: 150 min/week as achievable minimum, with acknowledgment that higher volumes confer additional benefit. Low extraction — the relationship enables evidence-based guideline setting without imposing structural costs on the agency.
constraint_indexing:constraint_classification(training_volume_dose_response, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: EXERCISE SCIENCE RESEARCH COMMUNITY (ROPE) — Organized agents who study and communicate dose-response relationships. The constraint coordinates research priorities and public communication: identifying the volume range where marginal benefits diminish enables efficient allocation of research resources and clear messaging. Minimal extraction — the relationship is a coordination tool for the field.
constraint_indexing:constraint_classification(training_volume_dose_response, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: TIME-CONSTRAINED INDIVIDUAL (ROPE) — Moderate power agent with constrained exit (limited discretionary time due to work/family obligations). The dose-response relationship coordinates their decision: knowing that 150 min/week captures substantial mortality benefit while 450+ min/week captures additional but diminishing benefit enables rational time allocation. The constraint is experienced as coordination despite exit constraints — it provides actionable information without imposing additional costs.
constraint_indexing:constraint_classification(training_volume_dose_response, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — The dose-response relationship is a coordination mechanism that translates biological reality (cardiovascular adaptation, metabolic efficiency, inflammatory modulation) into actionable information. The constraint has minimal extractive overhead: the relationship exists independently of institutional mediation, is accessible through multiple information channels, and imposes no structural barriers to acting on the information. Classification: rope across all perspectives.
constraint_indexing:constraint_classification(training_volume_dose_response, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(training_volume_dose_response_tests).
:- end_tests(training_volume_dose_response_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The dose-response relationship imposes minimal structural costs beyond the time and effort required for the activity itself. The information is freely accessible, requires no institutional mediation to act upon, and creates no asymmetric extraction between agents. The slight non-zero value reflects minor information asymmetries (access to scientific literature, health literacy required to interpret guidelines) and the opportunity cost of time spent exercising rather than in other activities, but these are coordination costs rather than extractive rents. Suppression (0.12): Very low. Barriers to acting on the dose-response information are primarily individual (time availability, baseline fitness, injury risk) rather than structural. No institutional actor suppresses access to the information or prevents individuals from adjusting training volume. The non-zero value reflects that time scarcity and baseline physiological capacity create real constraints, but these are not imposed by the constraint itself — they are background conditions the constraint helps agents navigate. Theater ratio (0.15): Very low. The dose-response relationship is grounded in direct epidemiological measurement (hazard ratios from large cohort studies) with minimal performative mediation. Public health guidelines communicate the relationship with some simplification (150 min/week as a round-number minimum) but the underlying data is transparent and the simplification serves coordination rather than obfuscation. The slight increase over the interval (0.10 to 0.15) reflects growing commercialization of fitness tracking and wellness programs that add performative layers (gamification, social comparison, branded training plans) on top of the core dose-response information, but the relationship itself remains empirically grounded.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all agents classify it as rope because the dose-response relationship functions as pure coordination across all structural positions. The high-volume exerciser, the public health agency, the research community, the time-constrained individual, and the analytical observer all experience the constraint as information that enables optimization without imposing structural costs. The uniformity of classification reflects the constraint's low extractiveness and suppression: there are no trapped agents bearing asymmetric costs, no institutional gatekeepers capturing rents, and no performative mediation obscuring the underlying relationship. The constraint is a diagnostic exemplar of a rope that remains rope across all perspectives — a coordination mechanism with minimal extractive overhead regardless of the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents in this constraint are beneficiaries — the dose-response relationship provides coordination information without imposing asymmetric extraction. High-volume exercisers benefit from knowing that additional training confers additional mortality benefit. Public health agencies benefit from evidence-based calibration of recommendations. Researchers benefit from a clear empirical relationship to investigate mechanistically. Time-constrained individuals benefit from knowing where marginal returns diminish, enabling rational allocation of limited discretionary time. The constraint has no victims because the information itself imposes no costs — agents are free to act on it or ignore it based on their own preferences and constraints. The directionality values are uniformly low (beneficiaries with mobile or arbitrage exit options) or moderate (beneficiaries with constrained exit due to time scarcity), producing low effective extraction across all perspectives. The constraint classifies as rope from all perspectives because it coordinates decision-making without extracting rents.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that rope classification can be robust across all perspectives when extractiveness and suppression are genuinely low. The dose-response relationship is not mislabeled coordination masking hidden extraction — it is coordination with minimal overhead. The constraint's victims array is empty because no agent bears asymmetric costs: the information is freely accessible, acting on it is voluntary, and the relationship itself imposes no structural barriers. The omega variables identify genuine empirical uncertainties (individual variation, injury risk, socioeconomic confounding) that could change the constraint's extractiveness if resolved in particular ways, but current evidence supports the rope classification. The constraint is a coordination mechanism that translates biological reality into actionable information, and the classification reflects this structural function accurately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_variation_threshold,
    'At what point does individual genetic/physiological variation make population-level dose-response curves misleading for individual optimization?',
    'Genomic and metabolomic profiling to identify responder vs non-responder phenotypes; longitudinal tracking of individual dose-response curves vs population means',
    'If individual variation is high: the constraint''s coordination function weakens for personalized optimization, though population-level guidance remains valid. If variation is low: the constraint''s rope classification is robust across individual and population scales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_variation_threshold, empirical, 'Whether individual variation undermines population dose-response coordination').

omega_variable(
    injury_risk_crossover_point,
    'Does injury risk at high training volumes (>750 min/week) create a net mortality cost that offsets cardiovascular benefit?',
    'Longitudinal cohort studies tracking injury incidence, severity, and downstream mortality effects across volume ranges; comparison of all-cause mortality in high-volume exercisers vs moderate-volume controls accounting for injury-related deaths',
    'If injury risk creates net mortality cost above threshold: the dose-response relationship has an optimal ceiling, changing the constraint''s shape but not its rope classification. If injury risk is negligible: the relationship remains monotonically beneficial across the measured range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(injury_risk_crossover_point, empirical, 'Whether injury risk at high volumes offsets mortality benefit').

omega_variable(
    socioeconomic_access_bias,
    'Do the observed dose-response curves reflect biological reality or socioeconomic confounding (high-volume exercisers have better healthcare access, nutrition, stress profiles)?',
    'Stratified analysis controlling for income, education, healthcare access, occupational stress; randomized controlled trials (where feasible) comparing mortality outcomes at different prescribed volumes within matched cohorts',
    'If confounding is substantial: the constraint''s extractiveness increases — the dose-response relationship becomes a marker of socioeconomic privilege rather than pure biological coordination. If confounding is minimal: rope classification is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(socioeconomic_access_bias, empirical, 'Whether dose-response reflects biology or socioeconomic confounding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(training_volume_dose_response, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tvdr_tr_t0, training_volume_dose_response, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tvdr_tr_t10, training_volume_dose_response, theater_ratio, 10, 0.12).
narrative_ontology:measurement(tvdr_tr_t20, training_volume_dose_response, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(tvdr_be_t0, training_volume_dose_response, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(tvdr_be_t10, training_volume_dose_response, base_extractiveness, 10, 0.07).
narrative_ontology:measurement(tvdr_be_t20, training_volume_dose_response, base_extractiveness, 20, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(training_volume_dose_response, information_standard).
narrative_ontology:affects_constraint(training_volume_dose_response, age_related_capacity_erosion).

% DUAL FORMULATION NOTE:
% The training volume dose-response relationship is downstream of age-related capacity erosion (a mountain constraint reflecting the biological inevitability of declining VO2max and muscle mass with age). The dose-response relationship coordinates how individuals and populations can modulate the rate of decline through training volume, but it does not eliminate the underlying erosion. The two constraints form a family: the mountain sets the boundary conditions (aging is inevitable), and the rope provides the coordination mechanism (training volume modulates the trajectory within those boundaries).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
