% ============================================================================
% CONSTRAINT STORY: uk_welfare_state_efficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_welfare_state_efficiency, []).

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
 *   constraint_id: uk_welfare_state_efficiency
 *   human_readable: UK Welfare State Efficiency Constraint
 *   domain: social_policy/economic_governance
 *
 * SUMMARY:
 *   The UK welfare state efficiency constraint represents a structural
 *   tension between two legitimate policy objectives: targeting limited
 *   resources to those most in need (means-testing), and maintaining
 *   universal access to subsistence provision as a right. This constraint
 *   exhibits the full range of DR classification types from different
 *   perspectives, making it a diagnostic exemplar for how institutional power
 *   structures embed themselves in ostensibly technical efficiency
 *   mechanisms. The same structural phenomenon — means-testing as a
 *   coordination mechanism for welfare distribution — appears as an immutable
 *   fiscal law (mountain), a coordination solution (rope), a mixed system
 *   with genuine coordination function alongside extraction (tangled rope), a
 *   degraded administrative ritual (piton), temporary coordination failure
 *   with a sunset path (scaffold), or pure extraction from the powerless
 *   (snare), depending on the observer's structural position and relationship
 *   to the welfare mechanism. The theater_ratio trajectory (0.38 → 0.58)
 *   reflects that administrative complexity and conditionality requirements
 *   have increased over the interval as means-testing verification procedures
 *   have become more intricate. The extractiveness trajectory (0.32 → 0.52)
 *   shows that the system's extraction mechanisms have accumulated: initial
 *   targeting logic has been layered with work conditionality, sanctions
 *   regimes, digital exclusion, and behavioral paternalism. The constraint is
 *   in degradation — the coordination function (targeting to need) is
 *   increasingly obscured by extraction mechanisms (gatekeeping, verification
 *   theater, normalized precarity).
 *
 * KEY AGENTS:
 *   - Low-Income Benefit Claimants: Primary victims (powerless/trapped) — bear full cost of means-testing verification, administrative burden, surveillance, and conditionality; cannot organize or exit
 *   - Welfare-Dependent Workers: Secondary victims (moderate/constrained) — face in-work poverty, means-testing disincentives, work conditionality, and partial stigma; constrained exit via sustained income above thresholds
 *   - Fiscal Administrators (DWP, Treasury): Primary beneficiaries (institutional/arbitrage) — experience constraint as enabling coordination; can restructure means-testing to achieve policy goals; benefit from budgetary control and performance measurability
 *   - Conservative Political Regime: Institutional beneficiary (institutional/constrained) — benefits from work incentive narratives and fiscal discipline rhetoric; constrained by path dependency and electoral politics; cannot easily shift to universalism without ideological reframing
 *   - Welfare Access Universality: Trapped collective good (powerless/trapped) — abstract principle that welfare is a right; cannot organize or exit; bears cost of erosion of universal entitlement norms
 *   - UBI Coalition: Organized reformers (organized/constrained) — perceive means-testing as temporary failure with sunset; constrained by institutional resistance; building alternative verification pathways
 *   - Means-Testing Bureaucracy: Institutional actor (institutional/arbitrage) — persists through self-preservation and professional identity; maintains performative verification procedures despite recognized inefficiency
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing political choice (means-testing vs universalism) as inherent fiscal constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_welfare_state_efficiency, 0.52).
domain_priors:suppression_score(uk_welfare_state_efficiency, 0.65).
domain_priors:theater_ratio(uk_welfare_state_efficiency, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_welfare_state_efficiency, extractiveness, 0.52).
narrative_ontology:constraint_metric(uk_welfare_state_efficiency, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(uk_welfare_state_efficiency, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_welfare_state_efficiency, tangled_rope).
narrative_ontology:human_readable(uk_welfare_state_efficiency, "UK Welfare State Efficiency Constraint").
narrative_ontology:topic_domain(uk_welfare_state_efficiency, "social_policy/economic_governance").

domain_priors:requires_active_enforcement(uk_welfare_state_efficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_welfare_state_efficiency, fiscal_administrators).
narrative_ontology:constraint_beneficiary(uk_welfare_state_efficiency, means_testing_apparatus).
narrative_ontology:constraint_beneficiary(uk_welfare_state_efficiency, conservative_political_actors).
narrative_ontology:constraint_victim(uk_welfare_state_efficiency, low_income_claimants).
narrative_ontology:constraint_victim(uk_welfare_state_efficiency, welfare_access_universality).
narrative_ontology:constraint_victim(uk_welfare_state_efficiency, administrative_burden_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BENEFIT CLAIMANT (SNARE) — Trapped by economic necessity and high barriers to exit. Faces invasive means-testing, frequent recertification, surveillance of household finances, conditionality requirements, and administrative gatekeeping. The constraint extracts dignity and time while providing minimal coordination benefit. Suppression is high: claimants cannot organize collectively due to stigma and fragmentation; alternative systems are unavailable; legal recourse is costly.
constraint_indexing:constraint_classification(uk_welfare_state_efficiency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WELFARE-DEPENDENT WORKER (TANGLED ROPE) — Constrained by in-work poverty and wage stagnation. The constraint both coordinates (provides income floor, enables labor force participation) and extracts (work conditionality, means-testing disincentives, stigma). Exit requires sustained income above means-testing thresholds — difficult but not impossible. Experiences high suppression but not total entrapment.
constraint_indexing:constraint_classification(uk_welfare_state_efficiency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FISCAL ADMINISTRATOR (ROPE) — Benefits from the constraint as a coordination mechanism: means-testing concentrates resources, means-tested delivery enables budgetary control, administrative overhead becomes measurable and rationalizable. Experiences the constraint as enabling rather than extractive. Has arbitrage options: can restructure means-testing formulas, adjust thresholds, shift between benefits. Net beneficiary.
constraint_indexing:constraint_classification(uk_welfare_state_efficiency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSERVATIVE POLITICAL REGIME (TANGLED ROPE) — Constrained by path dependency and electoral politics. The constraint serves coordination (targets limited budgets to poorest) and extraction (maintains work incentive narrative, contains entitlement costs, enables means-testing theater). Cannot fully exit without ideological reframing; cannot fully accept without admitting moral costs. High suppression of redistributive alternatives through rhetorical dominance and institutional embedding.
constraint_indexing:constraint_classification(uk_welfare_state_efficiency, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WELFARE ACCESS UNIVERSALITY (SNARE) — Abstract collective good representing the principle that welfare is a right, not a privilege. Trapped by institutional design that privileges targeting over universalism. Cannot organize or exit. Bears the cost of means-testing's coordination function. Extraction manifests as erosion of universal entitlement norms and replacement with conditionality.
constraint_indexing:constraint_classification(uk_welfare_state_efficiency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: UBI COALITION (SCAFFOLD) — Organized actors (think tanks, labor unions, poverty campaigners) perceive the means-testing system as a temporary coordination failure with a sunset clause. Universal Basic Income represents an alternative verification pathway that bypasses means-testing theater entirely. Coalition sees the extraction as structurally correctable via policy reform. Constrained by institutional resistance and fiscal orthodoxy, but agency and exit path are present. Sunset window: estimated 15-25 years as pilot programs mature and cost assumptions shift.
constraint_indexing:constraint_classification(uk_welfare_state_efficiency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: MEANS-TESTING BUREAUCRACY (PITON) — The administrative apparatus persists through institutional inertia despite recognized dysfunction. High theater ratio (0.58) reflects that much means-testing activity is performative verification rather than functional resource allocation. The bureaucracy maintains itself through organizational self-preservation and professional identity, not because means-testing is optimal. Administrative overhead consumes 15-25% of benefit budgets, producing theater: complex calculations, verification procedures, recertification cycles. The constraint is degraded — actors within it recognize its inefficiency but lack coordinated exit.
constraint_indexing:constraint_classification(uk_welfare_state_efficiency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FISCAL CONSTRAINT VIEW (MOUNTAIN) — From a civilizational timescale, some degree of welfare gatekeeping appears inherent to public finance: scarcity requires allocation, allocation requires mechanisms to distinguish need from preference. This perspective risks naturalizing what is actually a contingent institutional choice (means-testing vs universalism). The engine will compute this as a false summit, revealing that 'fiscal constraints' narratives obscure political choices about redistribution.
constraint_indexing:constraint_classification(uk_welfare_state_efficiency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_welfare_state_efficiency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_welfare_state_efficiency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_welfare_state_efficiency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_welfare_state_efficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_welfare_state_efficiency, TR),
    TR >= 0.70.

:- end_tests(uk_welfare_state_efficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts in multiple dimensions: time burden (application, recertification, compliance), dignity (surveillance, conditionality, stigma), and foregone income (means-testing disincentives discouraging work or undeclared earnings). However, extraction is not maximal because the system does deliver subsistence support and some degree of genuine need-targeting occurs. The extractiveness increase over the 30-year interval (0.32 → 0.52) reflects layering of conditionality (work requirements), sanctions regimes (financial penalties for non-compliance), and behavioral paternalism (requirements to attend job-seeking activities, parenting classes) onto the original means-testing logic. Suppression (0.65): High. Multiple barriers prevent exit or resistance: economic necessity (claimants must remain in welfare to survive), administrative complexity (prevents understanding of rules and alternatives), organizational fragmentation (claimants lack collective identity), stigma and shame (internalized suppression of welfare-seeking), and legal barriers (statutory framework that makes benefits conditional rather than unconditional). Theater ratio (0.58): Moderate-high. Significant proportion of means-testing activity is performative: recertification cycles that produce no new information, verification procedures that duplicate existing data (benefit claimants are already in government databases), compliance checking that punishes failure to perform rather than failure to meet need. However, some theater is lower than pure Piton threshold because genuine resource allocation does occur — the theater is layered onto real coordination function, not replacing it entirely.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the fiscal administrator's Rope classification and the claimant's Snare classification is diagnostic. Both are seeing the same constraint; they disagree on whether it is primarily coordination or extraction. The gap reveals that means-testing DOES provide coordination benefit (it targets resources to need more efficiently than untargeted universal payments would) AND creates extraction mechanisms (it creates incentives to hide income, undergoes surveillance, imposes behavioral conditions). Both are true simultaneously. The Tangled Rope classifications from moderate power and institutional constrained positions reflect this mixed reality: coordination and extraction are genuinely present in the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the extraction flow. Fiscal administrators (beneficiaries with arbitrage exit options) derive low d → negative f(d) → negative χ (they experience the constraint as enabling, not extractive). Low-income claimants (victims with trapped exit) derive high d → high f(d) → high χ (they experience maximum extraction). Welfare-dependent workers (victims with constrained exit) derive moderate d → moderate f(d) → moderate χ (they experience significant extraction but not maximal). The UBI coalition (organized agents with constrained but visible exit path) derives lower d than their powerless/trapped peers because they can see the policy sunset and have leverage to work toward it. The conservative political regime (beneficiary constrained by path dependency) derives higher d than pure beneficiaries because political constraints reduce their arbitrage freedom. The welfare access universality principle (powerless/trapped collective) derives maximum d — it has no exit option and no leverage.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint demonstrates that mandatrophy is resolved not by selecting one type as 'correct' but by acknowledging the legitimacy of multiple perspectival readings while preserving the structural claim that extraction is asymmetric. Snare and Rope both describe real phenomena: the system does coordinate (real need-targeting exists), and it does extract (real burdens fall on the trapped). The classification gap is not error; it is structural reality. The Tangled Rope classifications from constrained and institutional perspectives are the 'truth': there genuinely is both coordination and extraction. The Scaffold and Piton perspectives add temporal dimension: the system is degrading toward pure extraction (theater increasing from 0.38 to 0.58) while simultaneously enabling real alternatives (UBI pilots, simplified universal credits) that constitute a real sunset path. The Mountain perspective is the false summit: 'fiscal constraints are inherent' naturalizes what is actually a political choice about redistribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    means_testing_coordination_function,
    'Does means-testing genuinely coordinate resource allocation to maximize need coverage, or does it primarily serve to reduce total benefit expenditure by creating friction that discourages legitimate claims?',
    'Comparative analysis: benefit take-up rates under means-testing vs universal schemes; cost-benefit analysis of administrative overhead vs savings from reduced eligibility; international data on targeting efficiency across welfare regimes',
    'If genuine coordination: extractiveness ≤ 0.35, classification shifts toward Rope. If primarily fiscal gatekeeping: extractiveness ≥ 0.55, classification confirmed as Tangled Rope/Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(means_testing_coordination_function, empirical, 'Whether means-testing coordinates allocation or serves primarily to reduce expenditure').

omega_variable(
    suppression_internalization_dynamic,
    'Is suppression of welfare take-up primarily structural (administrative barriers, insufficient publicity) or internalized (shame, stigma, normalization of precarity)?',
    'Post-reform longitudinal analysis: if administrative barriers are removed (simplified claims, auto-enrolment) and take-up rates surge, suppression was structural. If they remain flat despite barrier reduction, suppression is internalized through shame/identity.',
    'If structural: suppression can be reduced by policy design changes. If internalized: removal of barriers alone is insufficient; requires cultural reframing of welfare as right rather than shame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_dynamic, empirical, 'Whether welfare suppression is structural or internalized').

omega_variable(
    universal_benefit_affordability,
    'Is universal welfare (flat, non-means-tested) fiscally unaffordable under current UK tax rates and economic structure, or is the fiscal constraint rhetorical?',
    'Dynamic fiscal modeling: scenarios of funded universalism under alternative tax structures (wealth tax, financial transaction tax, land value tax, inheritance tax increases). Comparison with other high-income democracies that maintain higher benefit universalism.',
    'If genuinely unaffordable: means-testing becomes a legitimate coordination mechanism, classification becomes pure Rope. If affordable under redistributive taxation: means-testing is a political choice, not a constraint, and the snare perspective is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_benefit_affordability, preference, 'Whether universal welfare is fiscally feasible').

omega_variable(
    administrative_theater_measurement,
    'What proportion of means-testing administrative activity produces genuine resource allocation improvements vs produces theater (verification rituals, recertification cycles, compliance documentation)?',
    'Administrative process audit: trace claims from application to first payment, measuring time spent on verification vs time spent on need assessment; compare benefit allocation variance under detailed means-testing vs simpler proxy measures',
    'If theater ≥ 60%: theater_ratio adjustment upward, classification shifts toward Piton. If theater ≤ 30%: means-testing function is defensible, piton perspective loses force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(administrative_theater_measurement, empirical, 'Proportion of means-testing activity that is performative vs functional').

omega_variable(
    claimant_coalition_capacity,
    'Could benefit claimants (individually powerless) organize into a collective power base sufficient to negotiate welfare system redesign?',
    'Comparative historical analysis: cases where welfare recipients successfully organized (Nordic unions, disability rights coalitions). Measurement of current UK claimant organization and potential for solidarity across benefit types.',
    'If coalition capacity exists but suppressed: snare classification is correct and reveals extractive mechanism targeting powerless agents. If coalition capacity is genuinely absent: snare classification reflects true powerlessness, not extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(claimant_coalition_capacity, empirical, 'Whether welfare claimants can develop coalition power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_welfare_state_efficiency, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukwse_tr_t0, uk_welfare_state_efficiency, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ukwse_tr_t10, uk_welfare_state_efficiency, theater_ratio, 10, 0.52).
narrative_ontology:measurement(ukwse_tr_t20, uk_welfare_state_efficiency, theater_ratio, 20, 0.58).
narrative_ontology:measurement(ukwse_tr_t30, uk_welfare_state_efficiency, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(ukwse_be_t0, uk_welfare_state_efficiency, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ukwse_be_t10, uk_welfare_state_efficiency, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(ukwse_be_t20, uk_welfare_state_efficiency, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(ukwse_be_t30, uk_welfare_state_efficiency, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_welfare_state_efficiency, resource_allocation).
narrative_ontology:boltzmann_floor_override(uk_welfare_state_efficiency, 0.18).
narrative_ontology:affects_constraint(uk_welfare_state_efficiency, uk_labor_market_precarity).
narrative_ontology:affects_constraint(uk_welfare_state_efficiency, universal_basic_income_viability).
narrative_ontology:affects_constraint(uk_welfare_state_efficiency, stigma_welfare_claiming).

% DUAL FORMULATION NOTE:
% Means-testing efficiency is downstream of political choice about redistribution philosophy (universalism vs targeting) and upstream of outcomes in labor market precarity, welfare claiming suppression, and institutional degradation. Related constraints exist for each benefit type (housing benefit, universal credit, disability allowance) with their own extractiveness values reflecting type-specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_welfare_state_efficiency, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
