% ============================================================================
% CONSTRAINT STORY: vesting_cliff_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vesting_cliff_trap, []).

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
 *   constraint_id: vesting_cliff_trap
 *   human_readable: Vesting Cliff Trap in Equity Compensation
 *   domain: economic/labor/compensation
 *
 * SUMMARY:
 *   The vesting cliff trap is a structural mechanism embedded in equity
 *   compensation that creates extreme incentive asymmetry: employees receive
 *   deferred compensation tied to a discrete cliff date (typically 4 years
 *   for US tech startups) after which unvested equity vanishes if they leave.
 *   This constraint extracts labor concession by making exit catastrophically
 *   expensive at the cliff edge, while the employer maintains narrative
 *   framing of equity as 'shared ownership' and 'alignment.' The base
 *   extractiveness (0.62) reflects that cliff vesting concentrates the
 *   majority of equity gain into a narrow time window, creating high
 *   suppression (0.75) through the permanent loss mechanism. Suppression is
 *   structural: once past the cliff date, the employee cannot recover
 *   unvested equity no matter how long they continue; the door closes
 *   permanently. The theater ratio (0.35) indicates the mechanism still
 *   performs a genuine coordination function (retaining employees during
 *   critical growth phases) but increasingly as a byproduct of historical
 *   practice rather than deliberate optimization. Linear vesting, cash
 *   grants, and portable equity accounts demonstrate that retention goals can
 *   be achieved with lower suppression costs, suggesting the cliff is
 *   maintained through institutional inertia rather than performance
 *   necessity.
 *
 * KEY AGENTS:
 *   - Early-Career Trapped Employee: Primary victim (powerless/trapped) — has invested years at below-market compensation on deferred equity promise; cliff edge creates immobility trap
 *   - Mid-Career Mobile Professional: Secondary victim (moderate/constrained) — can switch employers but loses accumulated unvested equity; real but costly exit option
 *   - Employer Organization: Primary beneficiary (institutional/arbitrage) — uses cliff structure to reduce voluntary turnover, extend cliff-period labor lockup, maintain low salary bases justified by equity kicker
 *   - Labor Advocacy Coalition: Organized challenger (organized/mobile) — pushing regulatory sunset through cliff-flattening mandates, portable equity, accelerated vesting schedules
 *   - Compensation Design Establishment: Institutional actor (institutional/arbitrage) — maintains cliff structures through recruiting industry standards, consultant recommendations, board compensation committee norms; sees as coordination but lacks pressure to optimize
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing cliff as inherent to equity rather than design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vesting_cliff_trap, 0.62).
domain_priors:suppression_score(vesting_cliff_trap, 0.75).
domain_priors:theater_ratio(vesting_cliff_trap, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vesting_cliff_trap, extractiveness, 0.62).
narrative_ontology:constraint_metric(vesting_cliff_trap, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vesting_cliff_trap, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vesting_cliff_trap, snare).
narrative_ontology:human_readable(vesting_cliff_trap, "Vesting Cliff Trap in Equity Compensation").
narrative_ontology:topic_domain(vesting_cliff_trap, "economic/labor/compensation").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vesting_cliff_trap, employer_capital_preservation).
narrative_ontology:constraint_victim(vesting_cliff_trap, employee_mobility).
narrative_ontology:constraint_victim(vesting_cliff_trap, early_career_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMPLOYEE AT CLIFF EDGE (SNARE) — Powerless agent facing imminent vesting cliff. Structurally trapped: cannot exit without massive foregone equity. The cliff mechanism deliberately creates artificial scarcity of exit options. Suppression is extreme because the employee has invested years of above-market-rate labor on deferred compensation premise. Maximum experienced extraction.
constraint_indexing:constraint_classification(vesting_cliff_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER MOBILE EMPLOYEE (TANGLED ROPE) — Moderate power agent with some exit capacity but high costs. Can switch employers but loses accumulated unvested equity. Experiences genuine coordination (equity aligns incentives with company success) alongside asymmetric extraction (cliff structure captures disproportionate labor concession during vesting window). Constrained by financial dependency and opportunity costs.
constraint_indexing:constraint_classification(vesting_cliff_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EMPLOYER ORGANIZATION (ROPE) — Institutional beneficiary experiencing the constraint as coordination mechanism: equity vesting aligns employee effort with long-term company performance and reduces voluntary turnover during critical periods. Net beneficiary with arbitrage exit options (can adjust vesting schedules, grant sizes, or equity composition across labor market). The structure benefits the organization without requiring coercive enforcement.
constraint_indexing:constraint_classification(vesting_cliff_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR REGULATION COALITION (SCAFFOLD) — Organized agents (labor unions, regulatory bodies, worker advocacy groups) see the cliff as a temporary institutional problem being addressed through legislative sunset: cliff-flattening mandates, accelerated vesting schedules, and portable equity accounts are gradually constraining the mechanism. Mobile due to political advocacy leverage. Extraction is low because the coalition has agency and sees a real regulatory exit path.
constraint_indexing:constraint_classification(vesting_cliff_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: EQUITY GRANT RITUAL (PITON) — Traditional equity compensation structures are substantially performative theater: the cliff mechanism persists through corporate inertia despite alternative designs (truly linear vesting, cash-equivalent grants, portable accounts) demonstrating superior retention outcomes. Theater ratio (0.35) reflects that operational function (retention via incentive alignment) is still present but degraded — companies maintain cliff structures because they've always done so, not because they're optimal. Piton classification indicates the structure is maintained by institutional momentum rather than active optimization.
constraint_indexing:constraint_classification(vesting_cliff_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some incentive timing asymmetry is structurally necessary to capital accumulation: all equity-based compensation involves timing mismatch between effort and reward crystallization. This perspective risks naturalizing what is actually a contingent institutional design choice (the cliff mechanism) as an inherent feature of how equity works. The false summit detector reveals that portraying vesting cliffs as 'natural to equity' masks deliberate choice in schedule design.
constraint_indexing:constraint_classification(vesting_cliff_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vesting_cliff_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vesting_cliff_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vesting_cliff_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vesting_cliff_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vesting_cliff_trap, TR),
    TR >= 0.70.

:- end_tests(vesting_cliff_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The cliff mechanism extracts significant labor concession by making exit impossibly expensive during vesting window. An employee who quits one day before cliff loses 100% of unvested equity — the discrete threshold creates maximum extraction at cliff edge. However, extractiveness is not as extreme as full snares (0.70+) because the employer's claimed coordination function (retention incentive) is partially genuine: equity does align some employee interests with company long-term success. The 0.62 value reflects that the extraction is real but embedded within a coordination mechanism. Suppression (0.75): Very high. The permanent loss of unvested equity on departure creates structural suppression — employees cannot walk away without forfeiting years of deferred compensation. This suppression operates through deliberate mechanism design (the cliff itself) rather than through external barriers. The employee has the legal right to leave but the economic penalty is catastrophic. Theater ratio (0.35): Moderate-low. Vesting schedules still serve a genuine retention coordination function, so the constraint hasn't degraded to pure theater. However, alternative designs (linear vesting, cash) achieve comparable retention with lower suppression, suggesting the cliff persists through institutional habit rather than optimization pressure.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows stark perspectival divergence. The employer describes vesting cliffs as 'aligning incentives' and 'sharing in company success' (rope framing: coordination). The trapped employee at year 3.99 sees pure extraction: leave tomorrow and lose everything, or stay locked in (snare framing). The mid-career employee weighs the coordination benefits (equity upside) against the extraction cost (cliff lock-in) and finds mixed experience (tangled rope framing). The labor coalition sees a temporary problem being solved through regulation (scaffold framing with sunset). The institutional compensation establishment sees the mechanism as part of the normal landscape (piton framing: degraded ritual). The analytical observer risks naturalizing cliff as inherent to equity (false summit mountain framing). The gap is structural: a beneficiary with exit options sees coordination; a victim without exit options sees extraction. The same mechanism appears as rope, snare, tangled rope, scaffold, piton, and mountain depending on the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for trapped employee: d ≈ 0.95 (near-full victim with trapped exit), f(d) ≈ 1.40, experience maximum extraction. Directionality for moderate constrained employee: d ≈ 0.65 (mixed victim/beneficiary with constrained exit), f(d) ≈ 1.00, experience moderate extraction balanced by coordination benefit. Directionality for institutional employer: d ≈ 0.10 (beneficiary with arbitrage exit), f(d) ≈ -0.05, experience coordination benefit with negative extraction (subsidy). Directionality for organized coalition: d ≈ 0.50 (symmetric between status quo and reform), f(d) ≈ 0.65, experience moderate effective extraction because they have agency and see exit path. No overrides needed: the derivation chain from beneficiary/victim + exit options produces accurate directionality for all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED: The vesting cliff trap's snare classification is robust across empirical tests. The snare gate requires ε ≥ 0.46 (✓ 0.62), suppression ≥ 0.60 (✓ 0.75), and χ ≥ 0.66 from the primary victim perspective. Computing chi for trapped employee at powerless/biographical horizon: χ = 0.62 × 1.40 (f(d) for trapped) × 1.0 (scope modifier for global) ≈ 0.87, which exceeds 0.66. The snare classification is not threatened by mandatrophy misclassification as pure coordination (rope) because the structural data shows victims (employee mobility, early-career workers) alongside beneficiaries (employer capital preservation), suppression is structural and extreme (permanent equity loss), and coordination function is degraded relative to alternatives (linear vesting achieves same retention with lower suppression). The constraint genuinely extracts. However, the perspectival gap reveals that from the employer's view, the mechanism appears as rope (genuine coordination with retained labor). The mandatrophy resolution requires showing that snare and rope are legitimate perspectival readings of the SAME structural mechanism, not that one is 'correct.' The analytical observer's mountain framing (cliff as inherent to equity) is a false summit: cliff scheduling is a deliberate design choice, not a natural law. Vesting schedule design is contested territory (linear vesting, cliff-flattening, portable accounts, cash-equivalent grants are real alternatives), which disqualifies mountain classification regardless of how natural the framing feels.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equity_value_realization_uncertainty,
    'What proportion of trapped employees actually realize financial gain from cliff vesting, versus those losing equity to company failure, dilution, or forgone gains from switching employers?',
    'Longitudinal tracking of cohorts: actual realized gains on cliff-vested equity vs alternative outcomes if employee had switched at cliff edge. Cohort analysis across public companies with measurable stock outcomes.',
    'If >60% realize positive gains: cliff mechanism is a retention device with real payoff. If <40% realize gains: cliff is primarily extraction mechanism disguised as incentive alignment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equity_value_realization_uncertainty, empirical, 'Proportion of cliff-vested employees realizing positive financial outcomes').

omega_variable(
    retention_rate_without_cliff,
    'Would linearly-vesting equity (no cliff) or cash compensation achieve comparable retention rates at lower suppression costs to employees?',
    'Controlled comparison: employers implementing cliff-flattening or linear vesting; turnover rate changes; survey of retention drivers among departing employees citing cliff structures.',
    'If linear vesting achieves 80%+ of cliff retention: cliff adds extraction without functional benefit. If cliff achieves materially higher retention: cliff has legitimate coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retention_rate_without_cliff, empirical, 'Whether linear vesting achieves comparable retention rates').

omega_variable(
    demographic_distribution_of_cliff_trap,
    'Are vesting cliffs distributed equally across cohorts, or do they disproportionately trap workers with lower mobility (family dependents, visa holders, minority workers with constrained networks)?',
    'Demographic analysis of who leaves at cliff edge vs who stays; correlation between demographic group and cliff-triggered departure; wage gap analysis for those who leave early.',
    'If distributed equally: snare classification applies uniformly. If disproportionately affects less-mobile groups: cliff mechanism is a secondary extraction leveraging pre-existing power differentials.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_distribution_of_cliff_trap, empirical, 'Demographic distribution of vesting cliff impacts').

omega_variable(
    cliff_scheduling_optimization,
    'Are cliff schedules designed to maximize retention incentives, or to maximize lock-in at company-vulnerable moments (pre-IPO, funding round, acquisition)?',
    'Historical analysis of cliff timing: correlation between cliff cliff-placement and corporate event calendars; survey of HR compensation design practices; comparison of cliff-placement across mature vs hypergrowth firms.',
    'If cliff-timing correlates with company vulnerability windows: mechanism is explicitly extractive timing lock. If random or at maturity: mechanism is retention-optimization neutral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cliff_scheduling_optimization, empirical, 'Whether cliff schedules are optimized for lock-in timing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vesting_cliff_trap, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vest_tr_t0, vesting_cliff_trap, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vest_tr_t2, vesting_cliff_trap, theater_ratio, 2, 0.3).
narrative_ontology:measurement(vest_tr_t4, vesting_cliff_trap, theater_ratio, 4, 0.35).

% Extraction over time
narrative_ontology:measurement(vest_be_t0, vesting_cliff_trap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vest_be_t2, vesting_cliff_trap, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(vest_be_t4, vesting_cliff_trap, base_extractiveness, 4, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vesting_cliff_trap, resource_allocation).
narrative_ontology:affects_constraint(vesting_cliff_trap, startup_founder_dilution).
narrative_ontology:affects_constraint(vesting_cliff_trap, tech_industry_wage_compression).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
