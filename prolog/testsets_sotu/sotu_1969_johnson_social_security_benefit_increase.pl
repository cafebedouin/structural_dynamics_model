% ============================================================================
% CONSTRAINT STORY: sotu_1969_johnson_social_security_benefit_increase
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1969_johnson_social_security_benefit_increase, []).

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
 *   constraint_id: sotu_1969_johnson_social_security_benefit_increase
 *   human_readable: Social Security 13% Benefit Increase (1969)
 *   domain: social_policy/federal_entitlements
 *
 * SUMMARY:
 *   The 1969 Social Security benefit increase represents a straightforward
 *   expansion of an existing coordination mechanism: the federal social
 *   insurance system that pools lifetime payroll contributions into
 *   retirement income. The 13% increase raises the minimum monthly benefit
 *   from $55 to $80, directly addressing elderly poverty at the lower end of
 *   the income distribution. This constraint demonstrates the Rope
 *   classification in its purest form — a mechanism for coordinating
 *   retirement security across generations with minimal extraction overhead.
 *   The increase strengthens the social expectation that benefit adequacy is
 *   a federal obligation subject to periodic revision based on cost of
 *   living, creating institutional path dependency that shapes future
 *   policymaking. The constraint operates within the Democratic majority of
 *   the 89th Congress and reflects postwar consensus that Social Security is
 *   a universal entitlement program, not a means-tested poverty program. The
 *   theater ratio (0.35) reflects modest performative content: the benefit
 *   increase includes ceremonial congressional debate about 'strengthening'
 *   the safety net, but the substance is straightforward — higher transfer
 *   payments with transparent funding mechanisms (payroll taxes and general
 *   revenue contributions).
 *
 * KEY AGENTS:
 *   - Elderly Minimum Beneficiaries: Primary beneficiary (powerless/constrained) — receive direct income increase; constrained by lack of alternative retirement income sources
 *   - Disabled Workers and Surviving Spouses: Secondary beneficiary (powerless/constrained) — some eligible under Social Security Disability Insurance and survivor benefit provisions; benefit from the same minimum increase
 *   - Senior Advocacy Groups: Organized beneficiary (organized/constrained) — AARP, senior unions, senior centers; mobilized political pressure for increase; have agency within the advocacy ecosystem
 *   - Federal Government / SSA: Institutional administrator (institutional/constrained) — manages benefit distribution, collects payroll taxes, coordinates intergenerational transfers; constrained by demographic trends and fiscal politics
 *   - Workers and Employers: Tax base (powerful/mobile) — bear increased payroll tax burden; have mobile alternatives (labor mobility, wage negotiation, capital flight) but cannot exit the system entirely
 *   - Congress: Legislative authority (institutional/arbitrage) — sets benefit levels and tax rates; has arbitage options (can adjust taxes, benefits, funding mechanisms); captures political credit for expansion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1969_johnson_social_security_benefit_increase, 0.18).
domain_priors:suppression_score(sotu_1969_johnson_social_security_benefit_increase, 0.25).
domain_priors:theater_ratio(sotu_1969_johnson_social_security_benefit_increase, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1969_johnson_social_security_benefit_increase, extractiveness, 0.18).
narrative_ontology:constraint_metric(sotu_1969_johnson_social_security_benefit_increase, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(sotu_1969_johnson_social_security_benefit_increase, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1969_johnson_social_security_benefit_increase, rope).
narrative_ontology:human_readable(sotu_1969_johnson_social_security_benefit_increase, "Social Security 13% Benefit Increase (1969)").
narrative_ontology:topic_domain(sotu_1969_johnson_social_security_benefit_increase, "social_policy/federal_entitlements").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1969_johnson_social_security_benefit_increase, elderly_minimum_beneficiaries).
narrative_ontology:constraint_beneficiary(sotu_1969_johnson_social_security_benefit_increase, disabled_workers).
narrative_ontology:constraint_beneficiary(sotu_1969_johnson_social_security_benefit_increase, surviving_spouses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELDERLY MINIMUM BENEFICIARY (ROPE) — The constraint solves a genuine coordination problem: translating workforce contributions into reliable income at retirement. From this agent's position, the 13% increase represents genuine coordination benefit with minimal extraction overhead. The beneficiary is constrained (cannot exit the social insurance system) but perceives the system as fundamentally fair — a social contract that delivers promised benefits with periodic adjustments for cost of living.
constraint_indexing:constraint_classification(sotu_1969_johnson_social_security_benefit_increase, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: SENIOR ADVOCACY GROUPS (ROPE) — Organized agents (AARP, senior unions) see the benefit increase as a victory for coordinated pressure on the political system. They have agency — they negotiated this increase through advocacy. The constraint they operate within (federal entitlements bureaucracy) is fundamentally a coordination mechanism with low extraction. They perceive genuine benefit and their organized power is acknowledged by the policymaking process.
constraint_indexing:constraint_classification(sotu_1969_johnson_social_security_benefit_increase, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL GOVERNMENT / SSA (TANGLED ROPE) — The SSA experiences the constraint as mixed coordination and asymmetric extraction. Genuinely coordinating retirement income (coordination benefit) while also managing the fiscal burden of expanded entitlements (extraction cost borne by general revenue and payroll taxes). The agency is constrained by political pressure and demographic trends but also captures administrative authority. Mandated to administer benefits accurately but also pressured to contain costs.
constraint_indexing:constraint_classification(sotu_1969_johnson_social_security_benefit_increase, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WORKERS AND EMPLOYERS (TANGLED ROPE) — Face increased payroll tax burden to fund expanded benefits, but also benefit from the coordination function of a stable retirement system that reduces pressure for family support and emergency poor relief. They are powerful enough to negotiate terms (through labor unions and business lobbies) but cannot exit the system. Mixed extraction and coordination benefit — they bear costs but also benefit from the social stability the system provides.
constraint_indexing:constraint_classification(sotu_1969_johnson_social_security_benefit_increase, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, Social Security represents a pure coordination mechanism: pooling lifetime earnings into a collective insurance fund against poverty in old age. The 1969 benefit increase is a straightforward adjustment to preserve purchasing power — a maintenance update to the coordination system. Low extractiveness because the system's primary function (spreading risk across generations and across the income distribution) is genuine, not extractive theater.
constraint_indexing:constraint_classification(sotu_1969_johnson_social_security_benefit_increase, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1969_johnson_social_security_benefit_increase_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1969_johnson_social_security_benefit_increase, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1969_johnson_social_security_benefit_increase, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(sotu_1969_johnson_social_security_benefit_increase_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low-moderate. The constraint's primary function is coordinating retirement income — pooling payroll contributions and distributing them according to a legislated formula. The 13% increase is a maintenance adjustment to the coordination mechanism, not an extractive overlay. However, extractiveness is not zero because the system creates fiscal pressure on general revenues and payroll taxes, and the benefit formula produces unequal returns on contributions (lower-wage workers receive higher replacement rates, higher-wage workers lower returns). This redistribution is intentional but counts as a form of extraction within the economic sense. Suppression (0.25): Low-moderate. The constraint operates through law and administrative procedure with transparency — beneficiaries, employers, and workers all know the tax rates and benefit formulas. However, suppression exists in the structural sense that workers have limited choice about whether to participate, elderly beneficiaries have limited alternatives to Social Security income, and the payroll tax is withheld automatically. These are not coercive in the snare sense but constitute structural constraints on exit. Theater ratio (0.35): Low. The benefit increase is announced plainly — no elaborate justification theater required. Congress debates the increase in terms of cost-of-living adjustment and poverty reduction, which are substantive arguments rather than performative ones. The theater comes from ceremonial framing ('strengthening the safety net') but the core transaction is transparent: higher transfers funded by higher taxes.
 *
 * PERSPECTIVAL GAP:
 *   The elderly beneficiary and senior advocacy groups see pure coordination (Rope) — the system delivers reliable income and the increase proves it is working. The payroll tax base sees mixed extraction and coordination (Tangled Rope) — they bear cost but also benefit from social stability. Congress and the SSA see institutional management of a complex system (Tangled Rope at the administrative level, Rope at the policy intent level). The analytical observer sees coordination logic (Rope) — the system's fundamental purpose is pooling risk across generations, which is genuine. No agent perceives this as extraction-dominated (Snare) in 1969, because the system's solvency is not yet in question and the demographic tail winds are favorable (worker-to-beneficiary ratio is still healthy). The perspectival gap is narrow — all perspectives classify as Rope or Tangled Rope, indicating broad consensus that the constraint serves a genuine coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive positive d (low extraction experienced) because the constraint delivers promised benefits with periodic adjustment for adequacy. The elderly minimum beneficiary is a full beneficiary of the increase (derives d ≈ 0.10–0.20), so experiences low effective extraction. Organized senior groups have agency in the political process (exit_options: constrained but with mobilization capacity), yielding d ≈ 0.25–0.35. Workers and employers face cost without direct benefit (are not elderly, not disabled, not survivors eligible for benefits), deriving higher d ≈ 0.55–0.65 — they bear extraction. However, workers also benefit from the coordination function (stable social order, reduced family support obligations), which moderates their d value. The institutional perspectives (SSA, Congress) operate in a mixed mode: they benefit from the administrative function (arbitrage exit — they can adjust terms) but also bear fiscal constraints (constrained exit — they cannot simply raise taxes or cut benefits without political cost). Overall extractiveness is low because the coordination function is genuine and primary; the extraction is secondary and transparent.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by remaining solidly in the coordination camp across all perspectives. No perspective sees this as pure extraction or coercive theater. The beneficiaries see genuine benefit; the tax base sees both cost and benefit; the administrators see both function and constraint. The lack of mandatrophy — the absence of a gap between 'this is extraction being labeled coordination' or vice versa — indicates the 1969 consensus was accurate: Social Security is fundamentally a coordination mechanism, not an extraction mechanism. The extraction risk lies in future uncertainty (demographic aging, wage stagnation, political inability to raise taxes) rather than in the present institutional structure. The theatrical language ('strengthening the safety net') is modest and not covering a functional void. This is a clean Rope story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    payroll_tax_incidence_ambiguity,
    'Who ultimately bears the payroll tax increase: workers (reduced wages), employers (reduced profits), or consumers (higher prices)?',
    'Labor economics analysis of wage responsiveness to payroll tax changes; empirical incidence studies comparing wage growth in high-tax vs low-tax states before and after tax increases',
    'If burden falls on workers: extraction cost for worker perspective rises, Tangled Rope → Snare. If burden falls on employers/consumers: distribution is diffuse, supports Rope classification. If mixed: Tangled Rope classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(payroll_tax_incidence_ambiguity, empirical, 'Incidence of payroll tax increase among workers, employers, consumers').

omega_variable(
    benefit_adequacy_floor_setting,
    'Does establishing a minimum benefit level ($80/month) create a permanent obligation to adjust for inflation, or can future policymakers allow real purchasing power to erode?',
    'Historical analysis of subsequent benefit adjustments; legislative record documenting whether decision-makers perceived a binding floor commitment or a one-time political win',
    'If binding obligation: the constraint is creating institutional path dependency (increases future extraction risk if inflation outpaces wage growth). If one-time win: constraint is simpler coordination mechanism with no long-term extraction accumulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_adequacy_floor_setting, conceptual, 'Whether minimum benefit creates binding inflation-adjustment obligation').

omega_variable(
    demographic_sustainability,
    'Is the payroll tax structure sustainable under shifting worker-to-beneficiary ratios as the population ages?',
    'Actuarial projections of the Social Security trust fund under various demographic scenarios; comparative analysis with pay-as-you-go systems in other nations experiencing population aging',
    'If unsustainable: the constraint will eventually accumulate extraction (current beneficiaries gain; future taxpayers pay — potential future Snare classification). If sustainable at current rates: the Rope classification is durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_sustainability, empirical, 'Long-term financial sustainability under demographic aging').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1969_johnson_social_security_benefit_increase, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ss69_tr_t0, sotu_1969_johnson_social_security_benefit_increase, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ss69_tr_t2, sotu_1969_johnson_social_security_benefit_increase, theater_ratio, 2, 0.3).
narrative_ontology:measurement(ss69_tr_t5, sotu_1969_johnson_social_security_benefit_increase, theater_ratio, 5, 0.35).

% Extraction over time
narrative_ontology:measurement(ss69_be_t0, sotu_1969_johnson_social_security_benefit_increase, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(ss69_be_t2, sotu_1969_johnson_social_security_benefit_increase, base_extractiveness, 2, 0.12).
narrative_ontology:measurement(ss69_be_t5, sotu_1969_johnson_social_security_benefit_increase, base_extractiveness, 5, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1969_johnson_social_security_benefit_increase, resource_allocation).
narrative_ontology:affects_constraint(sotu_1969_johnson_social_security_benefit_increase, payroll_tax_ceiling_emergence).
narrative_ontology:affects_constraint(sotu_1969_johnson_social_security_benefit_increase, entitlement_spending_path_dependency).

% DUAL FORMULATION NOTE:
% The 1969 benefit increase is a policy adjustment within the existing Social Security institutional structure. It represents expansion of coordination function rather than creation of new constraints. Related upstream constraints include the original Social Security program structure (1935) and the OASI/DI/SSI framework (1956–1972). Downstream constraints emerge from the path dependency this increase creates — future policymakers face a baseline expectation of benefit adequacy and regular cost-of-living adjustment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
