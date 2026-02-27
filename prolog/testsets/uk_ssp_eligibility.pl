% ============================================================================
% CONSTRAINT STORY: uk_ssp_eligibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_ssp_eligibility, []).

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
 *   constraint_id: uk_ssp_eligibility
 *   human_readable: UK Statutory Sick Pay (SSP) Eligibility and Rate
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK's Statutory Sick Pay (SSP) system establishes a minimum income
 *   replacement for employees unable to work due to illness. However,
 *   eligibility is conditioned on earning above the Lower Earnings Limit
 *   (LEL), currently £120 per week (2024). This threshold excludes an
 *   estimated 1.5–2 million low-wage, part-time, and gig economy workers from
 *   statutory protection. The constraint exhibits structural characteristics
 *   of a snare: it provides a legitimate coordination function for workers
 *   above the LEL (reducing moral hazard, stabilizing sickness absence
 *   patterns) while simultaneously extracting from those below it through
 *   enforced deprivation. The tension manifests across perspectives: workers
 *   below the LEL experience pure extraction with no exit; salaried employers
 *   above the threshold see coordination; the Treasury experiences it as
 *   fiscal arbitrage; unions see mixed extraction and coordination; and the
 *   historical framework appears as institutional inertia (piton). The
 *   analytical observer, comparing UK SSP to equivalent systems in other OECD
 *   nations with lower thresholds or universal coverage, identifies the LEL
 *   as a contingent policy choice that naturalizes extraction as
 *   'affordability' when alternative designs are feasible.
 *
 * KEY AGENTS:
 *   - Low-wage workers (below LEL): Primary victims (powerless/trapped) — earn too little to qualify; bear full cost of illness through lost income
 *   - Part-time workers: Primary victims (powerless/trapped) — multiple part-time jobs may sum above LEL individually but are counted separately; eligibility fragmented across employers
 *   - Gig economy workers (self-employed/zero-hours): Primary victims (powerless/trapped) — technically self-employed or contractor status; fall outside employee definition; no SSP even if earning below poverty threshold
 *   - Salaried employers (above LEL threshold): Primary beneficiary (institutional/arbitrage) — SSP enables workforce stability at lower total cost; coordination mechanism benefits this group
 *   - Treasury / Department for Work and Pensions: Secondary beneficiary (institutional/arbitrage) — SSP design transfers sickness-related cost from state to low-wage workers; achieves fiscal discipline
 *   - Trade unions / worker advocacy: Organized actor (organized/constrained) — partially protect members above LEL; constrained from reforming system unilaterally; see mixed extraction and coordination
 *   - Public health infrastructure: Affected by constraint (institutional/constrained) — excluded workers present risk of presenteeism (working while sick) or informal unpaid absence; contagion cost borne by NHS
 *   - Analytical observer: Civilizational context (analytical/analytical) — recognizes LEL as contingent policy, not inevitable fiscal necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_ssp_eligibility, 0.52).
domain_priors:suppression_score(uk_ssp_eligibility, 0.68).
domain_priors:theater_ratio(uk_ssp_eligibility, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_ssp_eligibility, extractiveness, 0.52).
narrative_ontology:constraint_metric(uk_ssp_eligibility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(uk_ssp_eligibility, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_ssp_eligibility, snare).
narrative_ontology:human_readable(uk_ssp_eligibility, "UK Statutory Sick Pay (SSP) Eligibility and Rate").
narrative_ontology:topic_domain(uk_ssp_eligibility, "economic/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_ssp_eligibility, employers_salaried_bracket).
narrative_ontology:constraint_beneficiary(uk_ssp_eligibility, treasury_exchequer).
narrative_ontology:constraint_victim(uk_ssp_eligibility, low_wage_workers).
narrative_ontology:constraint_victim(uk_ssp_eligibility, part_time_workers).
narrative_ontology:constraint_victim(uk_ssp_eligibility, gig_economy_workers).
narrative_ontology:constraint_victim(uk_ssp_eligibility, zero_hours_contract_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-WAGE WORKER BELOW LEL (SNARE) — Worker earning below the Lower Earnings Limit (£120/week threshold as of 2024) is structurally excluded from SSP eligibility. Cannot exit through wage negotiation without leaving workforce entirely. Bears full cost of illness: lost income with no statutory replacement. Maximum experienced extraction — no alternatives, no coordination benefit, pure coercion through deprivation.
constraint_indexing:constraint_classification(uk_ssp_eligibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GIG ECONOMY WORKER (SNARE) — Self-employed or zero-hours contractors fall below or barely above LEL; even if technically eligible, the administrative burden of proving continuous engagement exceeds the SSP value. Exit options are zero: switching to salaried employment is impossible for many sectors (e.g., food delivery, rideshare). Experiences pure extraction — the system collects tax revenue from gig work but provides no social insurance coverage in return.
constraint_indexing:constraint_classification(uk_ssp_eligibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SALARIED EMPLOYER (ROPE) — For employees above the LEL, SSP is a coordination mechanism: it reduces moral hazard in sickness absence and prevents wage collapse during illness, stabilizing workforce productivity. The employer benefits from the constraint's predictability (statutory minimum prevents wage-cutting races). Low suppression from this agent's view — they experience SSP as enabling, not coercive. Net beneficiary through arbitrage (can retain workers at lower total cost).
constraint_indexing:constraint_classification(uk_ssp_eligibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRADE UNION / WORKER ADVOCACY (TANGLED ROPE) — Organized labor sees the LEL as both a coordination achievement (SSP exists; it establishes principle of state-backed sick pay) and an extraction mechanism (LEL excludes the most vulnerable members). Unions have constrained exit: they can campaign for LEL removal but cannot unilaterally restructure SSP. Experience mixed extraction and coordination benefit — the system partially protects their membership while abandoning the precariat. Active enforcement and asymmetric extraction both present.
constraint_indexing:constraint_classification(uk_ssp_eligibility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TREASURY / PUBLIC HEALTH (ROPE) — SSP design coordinates public health (workers stay home when sick) with fiscal constraint (low SSP rates, eligibility exclusions reduce immediate cost to exchequer). From a public health lens, the LEL is a coordination failure (contagion risk for excluded workers). From a fiscal lens, it is a successful arbitrage (transfers sick leave cost to low-wage workers). Experience this as primarily coordination with some beneficial extraction.
constraint_indexing:constraint_classification(uk_ssp_eligibility, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: HISTORICAL FRAMEWORK (PITON) — The SSP system was designed in 1983 with the LEL threshold set to exclude high-churn, low-wage sectors (predicted cost reduction). Three decades on, the framework persists through institutional inertia despite economic changes: gig economy, zero-hours contracts, and wage stagnation have made the LEL exclusion far more widespread than originally intended. The theater ratio (0.45) reflects that the LEL is now largely performative — it achieves the original cost reduction goal, but the broader labor market has evolved around it. The constraint persists because formal reform is administratively and politically costly, not because it is functionally optimal.
constraint_indexing:constraint_classification(uk_ssp_eligibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — Comparative analysis across OECD nations reveals SSP design choices that are contingent, not inevitable. Germany, France, and Scandinavia provide higher replacement rates and lower (or no) earnings thresholds. The UK's LEL-based system is a policy choice, not a natural limit. From this analytical vantage, the constraint appears as a snare: structurally designed to transfer sickness-related income loss from employers and the state to low-wage workers. The exclusion is not a natural consequence of 'affording' sick pay; it is a deliberate allocation of fiscal burden downward.
constraint_indexing:constraint_classification(uk_ssp_eligibility, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_ssp_eligibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_ssp_eligibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_ssp_eligibility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_ssp_eligibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_ssp_eligibility, TR),
    TR >= 0.70.

:- end_tests(uk_ssp_eligibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The LEL excludes ~8% of the workforce below the threshold directly, with a secondary effect on part-time workers across multiple employment relationships. The extraction is structural and enforced through the statutory definition of 'employee' — those below the threshold receive zero SSP regardless of hours or contribution history. However, extraction is not at the maximum (0.70+) because some excluded workers have access to occupational sick pay schemes or employer discretion. The value reflects that the LEL is a hard filter producing clear inclusion/exclusion, not a graduated clawback that permits some mitigation. Suppression (0.68): High. Workers below the LEL have no formal exit options and no formal appeal mechanism. They cannot negotiate their way above the threshold without leaving employment or taking on additional risk. The administrative burden of proving continuous engagement (required for some gig workers to claim eligibility) is high relative to the SSP value, effectively suppressing claims even for the formally eligible. Public health infrastructure has constraints but not total suppression — they can (and do) provide care, but the cost is transferred. Theater ratio (0.45): Moderate. SSP framing emphasizes 'affordability' and 'protecting businesses from excessive burden,' but the primary mechanism is straightforward fiscal transfer: the LEL simply excludes those with lowest ability to weather income loss. The performative element is moderate because the eligibility rule is transparent and functional (it successfully excludes) — there is little theatrical pretense that the LEL represents a true test of ability to claim.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiaries and victims is stark. The salaried employer above the LEL sees SSP as coordination — it enables predictable workforce management and prevents wage-cutting races for sick leave. The worker below the LEL sees the same system as pure extraction: they contribute the same payroll taxes but receive zero statutory replacement. The analytical observer identifies this gap as a choice: OECD comparators show that universal or lower-threshold SSP is feasible, meaning the LEL is not an inevitable trade-off between 'affordability' and coverage but a deliberate allocation of risk downward. The trade union perspective bridges these: unions partially defend the principle of statutory sick pay (coordination achievement) while acknowledging that the LEL implementation abandons the precariat (extraction mechanism). The piton perspective (historical framework) suggests the gap has grown over time: the LEL was set in 1983 expecting to exclude a small proportion of high-churn, low-wage workers; three decades of wage stagnation and gig economy growth have made it exclude a much larger cohort than originally intended, but the threshold persists through institutional inertia rather than deliberate design choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by the agent's structural position relative to SSP. Workers below the LEL are full targets (d ≈ 0.95): they bear the full cost of the constraint (lost income during sickness) with no offsetting benefit. They are trapped (no exit option except leaving the workforce entirely). Their d value applies the sigmoid f(d) to maximum effect: high f(d) ≈ 1.42, which when scaled by scope (σ=1.0 for national) produces the high effective extraction χ experienced by this group. Salaried employers above the LEL are beneficiaries (d ≈ 0.10): they benefit from workforce stability at lower total cost and have arbitrage options (can move operations, renegotiate contracts). Their low d produces negative f(d) ≈ -0.01, reducing or inverting the extraction they experience. The Treasury is a beneficiary (d ≈ 0.05): it transfers sickness-related cost from public budget to individual workers, achieving fiscal goals. The analytical observer (d ≈ 0.73) is positioned as an external analyst: they see the full structure including the counterfactual (other OECD designs), which increases their perceived extraction relative to what is 'necessary.' The union (d ≈ 0.50) experiences symmetric extraction and coordination: they defend SSP principle but are unable to remove LEL, so they extract partial wins (higher rates for members above threshold, campaigns for reform) while allowing continued extraction of non-members.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy analysis reveals that SSP is NOT a coordinate-only mechanism masquerading as extraction (or vice versa). The system exhibits genuine dual structure: FOR THOSE ABOVE THE LEL, SSP is authentic coordination (reduces moral hazard, enables workforce stability, produces mutual benefit). FOR THOSE BELOW THE LEL, SSP is pure extraction (enforced deprivation with no offsetting benefit). The snare classification at the victim level (powerless/trapped) is appropriate: workers below the LEL face pure extraction with no coordination function. The rope classification at the beneficiary level (institutional/arbitrage) is also appropriate: employers experience coordination. The tangled rope classification at the union level (organized/constrained) is appropriate: the union defends coordination principle (SSP exists, sets sickness-absence standard) while acknowledging extraction (LEL excludes members). The constraint does not fail mandatrophy because it displays real asymmetry: genuine coordination for in-group, genuine extraction for out-group. The piton classification at the historical level reflects that the original coordination function (prevent wage-cutting races among competing employers) has been partially replaced by fiscal inertia (LEL threshold persists because reform is administratively costly, not because it remains functionally optimal). The analytical observer's identification of the LEL as contingent (not inevitable) confirms that mandatrophy is resolved: the constraint is not a false mountain (natural law) — alternative policy designs exist in other nations, proving the LEL is a choice, not physics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_kel_removal,
    'If the LEL were removed and SSP extended to all employees regardless of earnings, what would be the true fiscal and employment-behavioral consequences?',
    'Pilot programs in specific regions (e.g., Scotland or Wales) removing LEL for defined cohorts; comparison of sickness absence rates, employer hiring patterns, and net exchequer cost before/after',
    'If fiscal cost is minimal (< £500M/year): LEL is primarily extraction with little efficiency gain — removal would be net welfare improvement. If fiscal cost is high (> £2B/year) and reduces hiring: LEL represents genuine coordination trade-off — removal creates new distortions. Current estimates vary 10-fold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_kel_removal, empirical, 'Fiscal and behavioral cost of removing the LEL eligibility threshold').

omega_variable(
    substitution_behavior_measurement,
    'Do excluded workers substitute formal sick leave with informal unpaid leave, presenteeism (working while sick), or exit from workforce?',
    'Longitudinal survey of workers below LEL tracking sickness absence patterns, work-while-sick incidence, and workforce exit rates; comparison with matched cohort above LEL',
    'If substitution toward presenteeism is dominant: extraction cost is borne by public health (contagion). If substitution toward exit is dominant: extraction cost is labor supply loss. If informal leave is substitution: true cost is invisible but real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_behavior_measurement, empirical, 'How excluded workers adapt to lack of statutory sick pay').

omega_variable(
    gig_economy_definitional_boundary,
    'Are self-employed / gig workers who earn below LEL victims of this constraint or operating in a different contract regime entirely?',
    'Legal analysis of employment classification doctrine; comparison of self-employed access to alternative income protection (insurance, savings, family support) vs salaried workers; analysis of whether SSP was ever intended to cover self-employed',
    'If self-employed are in a different regime: snare classification is overstated — measure extraction only among employees. If self-employed are workers trapped in false self-employment: snare classification is understated — extraction is worse than visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gig_economy_definitional_boundary, conceptual, 'Whether self-employed / gig workers are within the scope of SSP extraction or outside it').

omega_variable(
    historical_intent_vs_drift,
    'Was the LEL threshold deliberately designed to exclude low-wage workers, or was it set at a value expected to be rarely breached but has since been overtaken by wage stagnation?',
    'Historical legislative record (Hansard debates, policy documents from 1982-1983 setting); analysis of wage distribution at LEL threshold then vs now; interviews with original policy designers if available',
    'If deliberately exclusionary: constraint is intentional snare — extraction by design. If threshold drift: constraint is snare through inertia — extraction by neglect. Both are snares, but the first suggests strong political will to maintain exclusion; the second suggests opportunity for reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_intent_vs_drift, empirical, 'Whether LEL threshold was deliberately set to exclude low-wage workers or has been eroded by wage stagnation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_ssp_eligibility, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ssp_tr_t0, uk_ssp_eligibility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ssp_tr_t15, uk_ssp_eligibility, theater_ratio, 15, 0.4).
narrative_ontology:measurement(ssp_tr_t30, uk_ssp_eligibility, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(ssp_be_t0, uk_ssp_eligibility, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ssp_be_t15, uk_ssp_eligibility, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(ssp_be_t30, uk_ssp_eligibility, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_ssp_eligibility, enforcement_mechanism).
narrative_ontology:affects_constraint(uk_ssp_eligibility, uk_minimum_wage_coverage).
narrative_ontology:affects_constraint(uk_ssp_eligibility, gig_economy_worker_protections).
narrative_ontology:affects_constraint(uk_ssp_eligibility, occupational_pension_eligibility).

% DUAL FORMULATION NOTE:
% SSP is a composite constraint that decomposes into two structural claims: (1) SSP as coordination mechanism for salaried employment (reduces moral hazard, stabilizes absence patterns) — this is genuine rope. (2) SSP as fiscal extraction through LEL exclusion (transfers sickness cost from state/employers to low-wage workers) — this is snare. The snare analysis focuses on the extractive mechanism (LEL threshold and its distributional consequences); the rope analysis would focus on the coordination function for employees above threshold. This story emphasizes the snare decomposition because the LEL is the constraint's defining feature and source of asymmetry. The rope function is subordinate to and undermined by the snare mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_ssp_eligibility, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
