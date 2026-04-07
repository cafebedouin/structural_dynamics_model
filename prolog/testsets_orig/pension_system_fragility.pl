% ============================================================================
% CONSTRAINT STORY: pension_system_fragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pension_system_fragility, []).

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
 *   constraint_id: pension_system_fragility
 *   human_readable: Pension System Fragility and Intergenerational Extraction
 *   domain: economic/social/financial
 *
 * SUMMARY:
 *   Pension system fragility emerges from a structural mismatch between
 *   demographic assumptions embedded at system inception and realized
 *   demographic trajectories. In most OECD nations, post-WWII pension systems
 *   were designed with fertility rates of 2.1+ children per woman and life
 *   expectancy at retirement of 15-20 years. Contemporary conditions show
 *   fertility at 1.3-1.8 and life expectancy at retirement of 25-30 years.
 *   This creates a cascading extraction mechanism where current cohorts
 *   (retirees and near-retirees) receive benefits calculated on old
 *   assumptions, funded by declining numbers of workers facing higher
 *   contribution rates or delayed retirement. The constraint exhibits genuine
 *   coordination function (pooled longevity risk, forced savings discipline,
 *   income smoothing) coupled with asymmetric extraction (older cohorts
 *   subsidized by younger). Active enforcement through tax policy and legal
 *   mandates keeps the system functional despite inherent unsustainability.
 *   The theater ratio reflects extensive actuarial legitimation and
 *   regulatory reporting that sustains political consensus despite mounting
 *   technical evidence of structural deficiency.
 *
 * KEY AGENTS:
 *   - Current Retirees: Primary beneficiary (institutional/arbitrage) — receive benefits based on historical formulae; have low exit friction from system
 *   - Future Workers: Primary victim (powerless/trapped) — legally mandated participation with underfunded liabilities; cannot opt out
 *   - Current Workers: Secondary actor (moderate/constrained) — face contribution rate increases and retirement age increases; mixed coordination-extraction experience
 *   - Financial Intermediaries: Beneficiary (institutional/constrained) — extract fees while system remains operational; constrained by regulatory oversight
 *   - Pension Reform Advocates: Organized actor (organized/constrained) — seeking to build sunset clauses and structural transition pathways
 *   - Pension System Administration: Institutional actor (institutional/arbitrage) — maintains apparatus through inertia; benefits from continued operation
 *   - Analytical Observer: System-level perspective (analytical/analytical) — detects tangled rope structure across multiple dimensions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pension_system_fragility, 0.58).
domain_priors:suppression_score(pension_system_fragility, 0.65).
domain_priors:theater_ratio(pension_system_fragility, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pension_system_fragility, extractiveness, 0.58).
narrative_ontology:constraint_metric(pension_system_fragility, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(pension_system_fragility, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pension_system_fragility, tangled_rope).
narrative_ontology:human_readable(pension_system_fragility, "Pension System Fragility and Intergenerational Extraction").
narrative_ontology:topic_domain(pension_system_fragility, "economic/social/financial").

domain_priors:requires_active_enforcement(pension_system_fragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pension_system_fragility, current_retirees).
narrative_ontology:constraint_beneficiary(pension_system_fragility, financial_intermediaries).
narrative_ontology:constraint_victim(pension_system_fragility, future_workers).
narrative_ontology:constraint_victim(pension_system_fragility, future_retirees).
narrative_ontology:constraint_victim(pension_system_fragility, pension_fund_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE WORKERS (SNARE) — Legally mandated participation in pension systems with underfunded liabilities. Cannot exit; bear actuarial deficits through higher contribution rates and later retirement. Trapped by demographic structure and legal obligation. Maximum experienced extraction from perspective of those who will pay taxes to cover shortfalls.
constraint_indexing:constraint_classification(pension_system_fragility, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CURRENT RETIREES (ROPE) — Receive promised benefits funded by current workers. Experience constraint as coordination mechanism: political consensus maintains system integrity. Net beneficiaries with low exit friction. System appears as stable commitment rather than extraction.
constraint_indexing:constraint_classification(pension_system_fragility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: CURRENT WORKERS (TANGLED ROPE) — Face mandatory contributions with uncertain future benefits. System provides some coordination function (pooled longevity risk, forced savings) while extracting through underfunded liability transfer. Constrained by legal obligation and limited alternatives. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(pension_system_fragility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FINANCIAL INTERMEDIARIES (TANGLED ROPE) — Extract fees from pension fund management while providing coordination service (asset allocation, risk management). Constrained by regulatory oversight but benefit from mandatory flow of capital. Active enforcement required to maintain fee structures despite performance questions.
constraint_indexing:constraint_classification(pension_system_fragility, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PENSION REFORM ADVOCATES (SCAFFOLD) — Organized actors promoting transition to sustainable models (increased retirement age, means-testing, hybrid public-private). See system fragility as a temporary problem requiring sunset clause on current structure. Constrained by political inertia but possess agency. Theater ratio declining as reform debate becomes technical.
constraint_indexing:constraint_classification(pension_system_fragility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PENSION SYSTEM ADMINISTRATION (PITON) — Maintains the bureaucratic apparatus of benefit calculation, contribution collection, and payment processing. Primary function (risk pooling, longevity insurance) has largely atrophied; system persists through institutional inertia and political pressure. High theater ratio reflects extensive actuarial projections and regulatory reporting that sustain legitimacy despite structural unsustainability.
constraint_indexing:constraint_classification(pension_system_fragility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination function (pooled longevity risk, forced savings discipline, income smoothing across generations) coupled with asymmetric extraction (current generation subsidized by future demographic structure). System requires active enforcement to function; collapse without intervention. Global scope reveals pattern across OECD nations with similar demographic-fiscal misalignment.
constraint_indexing:constraint_classification(pension_system_fragility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pension_system_fragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pension_system_fragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pension_system_fragility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pension_system_fragility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pension_system_fragility, TR),
    TR >= 0.70.

:- end_tests(pension_system_fragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The system transfers actuarial risk from current retirees to future workers through demographic asymmetry. Current workers see 35% at t=0 (before crisis awareness) rising to 58% at t=20 as dependency ratios worsen. This is neither pure extraction nor pure coordination — the system provides genuine longevity insurance and forced savings benefit alongside the transfer. The increase over time reflects mounting unfunded liabilities rather than new extraction mechanism. Suppression (0.65): High. Barriers to exit include legal mandate (participation required), political lock-in (difficult to modify benefit promises), demographic inevitability (cannot change birth rates retroactively), and cognitive framing (system presented as solidarity rather than extraction). Suppression is not absolute — reform advocates possess agency — but structural barriers are substantial. Theater ratio (0.68): Moderately high and increasing. System maintenance requires actuarial projections, regulatory compliance reports, and legitimation narratives emphasizing solidarity and intergenerational contracts. These serve genuine administrative functions but also mask the extraction mechanism — political consensus rests partly on theater rather than on transparent acknowledgment of who benefits and who bears costs.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap separates current beneficiaries (retirees, financial intermediaries) who experience rope or piton classifications from future victims (workers, future retirees) who experience snare or tangled rope. Current retirees see the system as a earned-right coordination mechanism because their contribution history aligns with the original design assumptions. Future workers see legal mandate to fund obligations they did not create. This perspectival gap is not bridgeable by information alone — it reflects genuine structural asymmetry in who benefits and who bears cost. The organized reformers (scaffold perspective) are attempting to reframe the constraint from inevitable-extraction to fixable-problem, but their agency is constrained by political lock-in and the power of current beneficiary coalitions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim structural relationships. Current retirees as beneficiaries with arbitrage options (can shift to private systems or simply enjoy earned benefits without exposure to future changes) get low d values. Future workers as victims with trapped exit (mandatory participation, no demographic escape) get high d values near 1.0. Current workers as both partial beneficiaries (will receive some pension) and partial victims (contributions rising, benefits uncertain) occupy mid-range d values around 0.55-0.70. Financial intermediaries as partial beneficiaries (fees from asset management) with constrained exit (must operate within regulatory framework) get d values around 0.35-0.45. The engine's sigmoid f(d) maps these to experienced extractiveness values that explain why younger cohorts perceive higher chi than older cohorts perceiving the same system.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that pension system fragility is a genuine tangled rope — not pure extraction masquerading as coordination, nor pure coordination falsely labeled as extraction. The system possesses both: (1) authentic coordination function addressing real market failures (inability to contract longevity risk, forced savings discipline for myopic agents, risk pooling economies) and (2) authentic extraction mechanism (demographic asymmetry transferring cost to future cohorts). The tangled rope classification is robust across multiple observables: whether measured by unfunded liability growth, contribution rate increases, retirement age changes, or intergenerational fairness surveys, the same structure appears — genuine coordination plus asymmetric extraction. The mandatrophy becomes apparent when comparing snare vs scaffold perspectives: both agree on the extraction; they disagree on reversibility. Scaffold advocates argue demographic transition has a finite window and that timely reform creates exit pathways (sunset clause). Snare perspective argues that political lock-in on benefit promises makes demographic transition irreversible (no actual sunset). The empirical question — is reform actually feasible or has political lock-in created a permanent snare? — is precisely what the mandatrophy framework enables systematic investigation of.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_window_closure,
    'At what point does adverse demographic transition make the system mathematically unsustainable regardless of policy intervention?',
    'Actuarial modeling of contribution-to-benefit ratio under various reform scenarios; identification of irreversible threshold in dependency ratio',
    'If threshold already passed: system is snare for all future participants. If threshold 10+ years away: scaffold perspective remains viable. If threshold depends on unrealistic assumptions: naturalizes contingent political choices as demographic inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_window_closure, empirical, 'Point of mathematical unsustainability in demographic structure').

omega_variable(
    moral_hazard_legitimacy_gap,
    'Is the extraction mechanism justified as intergenerational risk-sharing (legitimate coordination cost) or illegitimate free-riding by current cohorts?',
    'Historical analysis of initial actuarial assumptions vs realized demographics; comparative study of fairness perception across generations in different pension regimes',
    'If risk-sharing narrative: tangled rope classification defensible. If free-riding narrative: snare classification more accurate. If gap itself drives institutional instability: affects theater_ratio and piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_legitimacy_gap, conceptual, 'Legitimacy of extraction as intergenerational risk-sharing vs free-riding').

omega_variable(
    reform_reversibility_trap,
    'Do benefit reductions or contribution increases become politically locked-in once implemented, even if economic conditions improve?',
    'Historical tracking of pension reforms that were reversed or relaxed post-implementation; political economy analysis of constituency coalitions supporting locked-in changes',
    'If reforms easily reversed: scaffold sunset clause is credible. If reforms create new constituencies defending new baseline: system locks into extraction mechanism and snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_reversibility_trap, empirical, 'Whether pension reforms create reversible or locked-in extraction').

omega_variable(
    alternative_coordination_sufficiency,
    'Can private savings, portfolio diversification, and voluntary pooling mechanisms replace public pension coordination at equivalent cost and reliability?',
    'Comparative analysis of transaction costs, longevity risk pricing, administrative overhead between public and private systems; behavioral economics evidence on savings discipline',
    'If alternatives effective: current system appears extractive (forcing suboptimal coordination). If alternatives insufficient: system represents genuine coordination need, tangled_rope classification more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_sufficiency, empirical, 'Whether private mechanisms can replace public pension coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pension_system_fragility, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psf_tr_t0, pension_system_fragility, theater_ratio, 0, 0.55).
narrative_ontology:measurement(psf_tr_t10, pension_system_fragility, theater_ratio, 10, 0.62).
narrative_ontology:measurement(psf_tr_t20, pension_system_fragility, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(psf_be_t0, pension_system_fragility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(psf_be_t10, pension_system_fragility, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(psf_be_t20, pension_system_fragility, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pension_system_fragility, resource_allocation).
narrative_ontology:boltzmann_floor_override(pension_system_fragility, 0.18).
narrative_ontology:affects_constraint(pension_system_fragility, intergenerational_implicit_contract).
narrative_ontology:affects_constraint(pension_system_fragility, demographic_transition_lock).
narrative_ontology:affects_constraint(pension_system_fragility, financial_intermediary_capture).

% DUAL FORMULATION NOTE:
% Pension system fragility decomposes into three structurally distinct constraints: (1) the intergenerational implicit contract (ε=0.42, Tangled Rope) — the original coordination mechanism, (2) demographic transition lock (ε=0.72, Snare) — the extraction mechanism created by realized demographic change, and (3) financial intermediary capture (ε=0.51, Tangled Rope) — fee extraction layered onto the system. This story focuses on the composite system-level constraint. Each component story has its own ε value and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pension_system_fragility, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
