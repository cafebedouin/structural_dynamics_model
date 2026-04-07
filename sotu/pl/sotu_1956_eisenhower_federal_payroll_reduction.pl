% ============================================================================
% CONSTRAINT STORY: sotu_1956_eisenhower_federal_payroll_reduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1956_eisenhower_federal_payroll_reduction, []).

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
 *   constraint_id: sotu_1956_eisenhower_federal_payroll_reduction
 *   human_readable: Federal Government Workforce Contraction (1956 Eisenhower Payroll Reduction)
 *   domain: governance/labor_policy/budgetary_constraint
 *
 * SUMMARY:
 *   The federal workforce contraction of 1956 represents a deliberate
 *   structural reduction in government employment, eliminating approximately
 *   297,000 positions while claiming to maintain operational capacity and
 *   fiscal discipline. The constraint embeds a presumption against federal
 *   expansion and operationalizes an ideology of minimal government. It
 *   benefits taxpayers (through lower federal spending) and balanced-budget
 *   advocates by creating a visible payroll reduction; it costs displaced
 *   federal workers (trapped with no equivalent alternative) and constrains
 *   state capacity for new programs (federal co-funding declines).
 *   Structurally, the constraint combines genuine coordination (reducing
 *   deficit spending while attempting to maintain core functions) with
 *   asymmetric extraction (concentrated costs on workers; diffuse benefits
 *   across taxpayer base). The theater ratio rises over time as the reduction
 *   persists — initial claims of 'efficiency improvement' and 'elimination of
 *   wasteful positions' face reality testing as service backlogs accumulate
 *   and deferred work becomes visible. This is a diagnostic case for how
 *   coordination functions can embed substantial extraction when the
 *   distributional consequences are concentrated on a powerless population.
 *
 * KEY AGENTS:
 *   - Displaced Federal Workers: Primary victims (powerless/trapped) — 297,000 individuals with no exit option; bear concentrated costs of reduced wages, lost benefits, retraining burden, geographic relocation
 *   - Balanced Budget Coalition: Primary beneficiary (institutional/arbitrage) — fiscal conservatives, congressional budget hawks, taxpayer advocacy groups; achieve deficit reduction goal
 *   - Taxpayers (especially middle/upper income): Secondary beneficiary (powerful/mobile) — aggregate population experiencing reduced federal income taxes through lower spending
 *   - Private Sector Employers: Secondary beneficiary (organized/arbitrage) — benefit from labor supply increase and reduced wage pressure; can recruit federal workers
 *   - State Governments: Victim (moderate/constrained) — federal co-funding declines; space created for state-level solutions but constrained by loss of federal spending in state economies
 *   - Federal Service Continuity: Victim (powerless/trapped) — abstract institutional capacity facing degradation through reduced staffing; cannot exit or organize resistance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1956_eisenhower_federal_payroll_reduction, 0.52).
domain_priors:suppression_score(sotu_1956_eisenhower_federal_payroll_reduction, 0.68).
domain_priors:theater_ratio(sotu_1956_eisenhower_federal_payroll_reduction, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1956_eisenhower_federal_payroll_reduction, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1956_eisenhower_federal_payroll_reduction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sotu_1956_eisenhower_federal_payroll_reduction, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1956_eisenhower_federal_payroll_reduction, tangled_rope).
narrative_ontology:human_readable(sotu_1956_eisenhower_federal_payroll_reduction, "Federal Government Workforce Contraction (1956 Eisenhower Payroll Reduction)").
narrative_ontology:topic_domain(sotu_1956_eisenhower_federal_payroll_reduction, "governance/labor_policy/budgetary_constraint").

domain_priors:requires_active_enforcement(sotu_1956_eisenhower_federal_payroll_reduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1956_eisenhower_federal_payroll_reduction, taxpayers_middle_upper_income).
narrative_ontology:constraint_beneficiary(sotu_1956_eisenhower_federal_payroll_reduction, balanced_budget_coalition).
narrative_ontology:constraint_beneficiary(sotu_1956_eisenhower_federal_payroll_reduction, private_sector_employers).
narrative_ontology:constraint_victim(sotu_1956_eisenhower_federal_payroll_reduction, federal_workers_displaced).
narrative_ontology:constraint_victim(sotu_1956_eisenhower_federal_payroll_reduction, state_capacity_for_programs).
narrative_ontology:constraint_victim(sotu_1956_eisenhower_federal_payroll_reduction, federal_service_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED FEDERAL WORKER (SNARE) — Trapped by loss of secure employment with no alternative pathway at equivalent salary/benefits. No agency in the reduction decision; bears full extraction cost. Career disruption, retraining burden, geographic relocation. Maximum suppression: employment options are constrained by specialization, age, regional labor market conditions, and loss of pension vesting in some cases.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_federal_payroll_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE GOVERNMENT CAPACITY (TANGLED ROPE) — Experiences mixed effects. Federal contraction constrains state capacity for new programs (reduced federal co-funding, reduced federal payroll spending in state economies). But also faces coordination benefit: federal withdrawal creates space for state-level solutions and local adaptation. Constrained exit — states cannot unilaterally expand federal employment, but can implement alternative programs. Moderate extraction with genuine coordination element.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_federal_payroll_reduction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BALANCED BUDGET COALITION (ROPE) — Primary beneficiary (institutional actors: fiscal conservatives, congressional budget hawks, taxpayer advocacy groups). Achieves coordination goal: reducing deficit spending while maintaining core federal functions. Arbitrage position — can exit the constraint by supporting federal expansion, but chooses payroll reduction as preferred pathway. Net beneficiary.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_federal_payroll_reduction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIVATE SECTOR EMPLOYERS (ROPE) — Secondary beneficiary (organized institutional actors). Federal workforce reduction increases labor supply to private sector; reduces wage pressure; reduces government competition for talent. Coordination function: efficient labor reallocation from public to private use. Arbitrage position — can hire federal workers released by contraction or absorb their skills. Net beneficiary.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_federal_payroll_reduction, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE TAXPAYER / ANTI-GOVERNMENT IDEOLOGY (MOUNTAIN) — From the ideological perspective (widely distributed institutional/powerful agents), federal payroll reduction appears as a natural law: government spending must be constrained by fiscal discipline; bloated bureaucracies are inefficient and must be pruned; minimal government is a structural principle, not a contingent choice. This perspective risks false summit classification — the 'naturalization' of anti-government ideology as immutable law.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_federal_payroll_reduction, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Cross-position analysis reveals the constraint as embedded coordination (maintaining core functions while reducing payroll) with structural asymmetry (workers bear concentrated costs; benefits diffuse across taxpayer base). The constraint coordinates deficit reduction but extracts from a concentrated, trapped population. The mountain perspective (natural law) is perspectival, not objective.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_federal_payroll_reduction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1956_eisenhower_federal_payroll_reduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1956_eisenhower_federal_payroll_reduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1956_eisenhower_federal_payroll_reduction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1956_eisenhower_federal_payroll_reduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_1956_eisenhower_federal_payroll_reduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The reduction produces real extraction from displaced workers (wage loss, benefit loss, career disruption) but also achieves genuine coordination benefit (deficit reduction, maintained core services with lower payroll). The value of 0.52 reflects the mixed nature — this is not pure extraction (which would be >0.66 snare threshold) but substantial asymmetric extraction embedded in coordination. Suppression (0.68): High. Workers face severe suppression: employment alternatives are constrained by skill specialization and regional labor market conditions; pension vesting creates lock-in (losing years of service if forced out); age discrimination affects older workers; relocation burden is high. Suppression is partially structural (labor market scarcity) and partially imposed (government choice to cut rather than reallocate). Theater ratio (0.38 rising to 0.42): Moderate, increasing. Initial reduction is presented as efficiency improvement (elimination of wasteful positions) with relatively low performative content — decisions appear technical/budgetary. Theater increases over time as backlogs accumulate and deferred work becomes visible, suggesting the 'efficiency' framing is not fully supported by outcomes. The constraint's claimed_type of tangled_rope is correct — genuine coordination (deficit control) is inseparable from asymmetric extraction (worker displacement).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps are stark and reveal the constraint's extractive logic. The displaced worker sees pure snare (trapped, maximum extraction). The beneficiary coalition sees pure rope (coordination, deficit reduction achieved). The state sees tangled rope (coordination benefit from federal withdrawal mixed with extraction of state capacity). The taxpayer ideology sees mountain (natural law of fiscal discipline) which the engine identifies as false summit. The analytical observer sees tangled rope — genuine coordination with concentrated extraction. The gap between beneficiary perspective (rope) and victim perspective (snare) is the diagnostic signature of asymmetric extraction masquerading as coordination. If the constraint were pure coordination, all perspectives should converge — some would see rope, some would see more efficient rope, but none would see snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural relationships. Displaced workers: d ≈ 0.95 (trapped victims, no exit → maximum f(d)). Balanced budget coalition: d ≈ 0.05 (beneficiary with arbitrage exit → minimum f(d)). Taxpayers: d ≈ 0.10 (beneficiary with mobile exit → near-minimum f(d)). Private employers: d ≈ 0.15 (beneficiary with arbitrage exit → low f(d)). State governments: d ≈ 0.70 (mixed victim-beneficiary status with constrained exit; coordination benefit from federal withdrawal mixed with extraction of capacity). Federal service continuity: d ≈ 1.0 (powerless victim, trapped, cannot exit). The directive asymmetry — beneficiaries cluster at d ≈ 0.05–0.15, victims cluster at d ≈ 0.70–1.0 — is the structural hallmark of extraction embedded in coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the tangled rope classification: it is genuinely both coordination (reduces federal spending, maintains core service delivery in theory) and extraction (concentrates costs on trapped workers). The mandatrophy would arise if the analyst tried to classify this as 'pure coordination' (rope) — that classification would be false, masking asymmetric extraction. The mountain perspective ('fiscal discipline is natural law') is correctly identified by the engine as false summit: beneficiaries exist (balanced budget coalition, taxpayers), ideology is naturalized, and the constraint is contingent on political choice. The analytical observer's tangled rope classification captures the true structure: coordination function + asymmetric extraction + active enforcement (government power to terminate employment). Mandatrophy resolves when the analysis refuses to collapse this into either 'good coordination' or 'bad extraction' and instead models it as both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_function_substitution,
    'Can 297,000 positions be eliminated while maintaining equivalent federal service capacity, or does service degradation follow the reduction?',
    'Comparative analysis of federal service metrics (processing times, error rates, program reach) before and after reduction; measurement of task consolidation vs service loss; tracking of deferred maintenance and backlog accumulation',
    'If service maintained: constraint is pure coordination (lower chi). If service degrades: constraint masks service extraction wrapped in efficiency language (higher chi, reclassify toward snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_function_substitution, empirical, 'Degree to which service capacity is maintained vs degraded by payroll reduction').

omega_variable(
    private_sector_absorption_capacity,
    'Can private sector actually absorb 297,000 federal workers at equivalent wages and working conditions, or are many absorbed at lower salary/security?',
    'Longitudinal tracking of displaced federal workers'' employment trajectories; wage comparison (federal vs private sector post-placement); benefits comparison; sector distribution; geographic matching analysis',
    'If absorbed at equivalent: extraction is lower, constraint appears more coordinated. If absorbed at lower wages/conditions: extraction is higher, worker victimization is worse, classify toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(private_sector_absorption_capacity, empirical, 'Degree of wage/benefits loss for absorbed federal workers').

omega_variable(
    ideological_naturalization,
    'Is federal payroll reduction presented as natural law (fiscal discipline, government efficiency) rather than as a contingent policy choice with distributional consequences?',
    'Content analysis of political rhetoric surrounding the reduction; examination of whether alternatives (progressive taxation, programmatic efficiency, federal expansion with cost offsets) are presented as serious options or dismissed as infeasible; tracking of false summit detection by engine (beneficiary presence on mountain classification)',
    'If heavily naturalized: constraint gains ideological immunity from challenge; false summit detection fires (engine reclassifies mountain → tangled rope). If presented as choice: political accountability is clearer; distributional debate is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_naturalization, conceptual, 'Degree to which payroll reduction is naturalized as law vs presented as policy choice').

omega_variable(
    regional_vulnerability_concentration,
    'Are federal jobs concentrated in specific regions, creating regional extraction while other regions benefit from tax reduction?',
    'Geographic analysis of federal employment density; mapping of job losses by region; correlation with regional economic indicators; tracking of regional migration post-reduction',
    'If concentrated regionally: some regions experience snare (trapped workers, economic collapse); others experience rope (tax benefits without cost). Constraint is spatially heterogeneous — may require separate stories by region.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_vulnerability_concentration, empirical, 'Geographic concentration of federal job losses vs tax benefits').

omega_variable(
    pension_vesting_asymmetry,
    'For workers near vesting (5-10 years from pension), does forced reduction cause loss of pension rights, creating extraction beyond wage loss?',
    'Analysis of pension law at the time; tracking of vesting status of displaced workers; measurement of pension loss by vesting proximity; comparison to forced retirement packages',
    'If pension loss is significant: suppression increases dramatically (workers lose both current wage and future security). If pension transfers or buyouts protect vesting: suppression is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pension_vesting_asymmetry, empirical, 'Degree to which payroll reduction causes pension vesting loss').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1956_eisenhower_federal_payroll_reduction, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu56_theater_1953, sotu_1956_eisenhower_federal_payroll_reduction, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sotu56_theater_1956, sotu_1956_eisenhower_federal_payroll_reduction, theater_ratio, 3, 0.38).
narrative_ontology:measurement(sotu56_theater_1960, sotu_1956_eisenhower_federal_payroll_reduction, theater_ratio, 7, 0.42).

% Extraction over time
narrative_ontology:measurement(sotu56_extractiveness_1953, sotu_1956_eisenhower_federal_payroll_reduction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu56_extractiveness_1956, sotu_1956_eisenhower_federal_payroll_reduction, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(sotu56_extractiveness_1960, sotu_1956_eisenhower_federal_payroll_reduction, base_extractiveness, 7, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1956_eisenhower_federal_payroll_reduction, resource_allocation).
narrative_ontology:affects_constraint(sotu_1956_eisenhower_federal_payroll_reduction, federal_civil_service_mobility).
narrative_ontology:affects_constraint(sotu_1956_eisenhower_federal_payroll_reduction, state_federal_fiscal_federalism).
narrative_ontology:affects_constraint(sotu_1956_eisenhower_federal_payroll_reduction, government_workforce_loyalty_programs).

% DUAL FORMULATION NOTE:
% The payroll reduction operates at two structural levels: (1) the coordination of deficit reduction (federal budget balance) with (2) the extraction mechanism targeting federal workers. These could decompose into separate stories if the empirical question is whether service capacity actually survives the reduction unchanged. If service degradation follows, a second story (federal_service_degradation) would have higher extractiveness (ε ≈ 0.65) and would classify as snare from analytical perspective. Current story assumes service maintenance; decompose if service loss is empirically documented.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1956_eisenhower_federal_payroll_reduction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
