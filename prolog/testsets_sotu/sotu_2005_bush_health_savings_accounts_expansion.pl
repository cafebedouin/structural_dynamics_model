% ============================================================================
% CONSTRAINT STORY: sotu_2005_bush_health_savings_accounts_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2005_bush_health_savings_accounts_expansion, []).

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
 *   constraint_id: sotu_2005_bush_health_savings_accounts_expansion
 *   human_readable: HSA Expansion and Consumer-Directed Care Cost-Shifting
 *   domain: healthcare/economic_policy
 *
 * SUMMARY:
 *   The 2005 Health Savings Account expansion, promoted in the State of the
 *   Union as consumer-directed care reform, represents a fundamental shift in
 *   healthcare cost allocation from pooled employer-insurer risk to
 *   individual consumer burden. HSAs combine high-deductible health insurance
 *   with tax-advantaged savings accounts that allow workers to pre-fund
 *   medical expenses. For high-income workers with discretionary savings
 *   capacity, this creates genuine coordination benefits: tax-free savings,
 *   investment growth, and incentives for cost-conscious purchasing align
 *   personal interest with system efficiency. For low-income and chronically
 *   ill populations, the same mechanism creates pure cost-shifting: unable to
 *   pre-fund deductibles, they face maximum out-of-pocket exposure at point
 *   of care. The constraint exhibits all structural hallmarks of a tangled
 *   rope: genuine coordination function (price signals can reduce unnecessary
 *   care, individual savings accumulation is real), paired with asymmetric
 *   extraction (high-income workers capture tax benefits while low-income
 *   workers bear uncompensated cost increases). The beneficiaries
 *   (high-income workers, health insurers through reduced pooled risk,
 *   financial services sector through HSA management fees) are clearly
 *   identifiable. The victims (low-income workers forced into cost-sharing,
 *   chronically ill populations whose risk pools deteriorate as healthy
 *   workers self-select into HDHP+HSA models, healthcare cost distribution
 *   equity) are equally clear. Active enforcement is required: plan design,
 *   contribution limits, eligibility rules, and insurer cream-skimming via
 *   network and formulary restrictions all actively maintain the extraction
 *   mechanism.
 *
 * KEY AGENTS:
 *   - High-income workers: Primary beneficiaries (institutional/arbitrage) — capture tax-advantaged savings, can accumulate reserves, have exit options through employer plan choices
 *   - Low-income workers: Primary victims (powerless/trapped) — lack discretionary income to fund HSA contributions, forced into uncompensated deductible exposure, trapped in plan design choices
 *   - Chronically ill populations: Secondary victims (powerless/trapped) — face deteriorating risk pools as healthy workers self-select into HDHP+HSA; inelastic demand means cost-shifting hits them hardest
 *   - Health insurers: Primary beneficiaries (institutional/arbitrage) — reduce pooled risk through cream-skimming, lower HDHP premiums, profit from HSA administration and investment management
 *   - Employers: Secondary beneficiaries/coordinators (institutional/arbitrage) — reduce healthcare cost growth, transfer actuarial risk to employees, can design plans to influence workforce composition
 *   - Financial services sector: Tertiary beneficiary (institutional/arbitrage) — HSA account management fees, investment management on accumulated balances
 *   - Policy reformers: Organized agents (organized/constrained) — identify political pressure from cost-shifting failures, build alternative risk-pooling models with sunset logic
 *   - Historical risk-pooling norm: Institutional actor (institutional/arbitrage) — maintains degraded ritual function while normative power erodes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2005_bush_health_savings_accounts_expansion, 0.58).
domain_priors:suppression_score(sotu_2005_bush_health_savings_accounts_expansion, 0.62).
domain_priors:theater_ratio(sotu_2005_bush_health_savings_accounts_expansion, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2005_bush_health_savings_accounts_expansion, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_2005_bush_health_savings_accounts_expansion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sotu_2005_bush_health_savings_accounts_expansion, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2005_bush_health_savings_accounts_expansion, tangled_rope).
narrative_ontology:human_readable(sotu_2005_bush_health_savings_accounts_expansion, "HSA Expansion and Consumer-Directed Care Cost-Shifting").
narrative_ontology:topic_domain(sotu_2005_bush_health_savings_accounts_expansion, "healthcare/economic_policy").

domain_priors:requires_active_enforcement(sotu_2005_bush_health_savings_accounts_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2005_bush_health_savings_accounts_expansion, high_income_workers).
narrative_ontology:constraint_beneficiary(sotu_2005_bush_health_savings_accounts_expansion, health_insurers).
narrative_ontology:constraint_beneficiary(sotu_2005_bush_health_savings_accounts_expansion, financial_services_sector).
narrative_ontology:constraint_victim(sotu_2005_bush_health_savings_accounts_expansion, low_income_workers).
narrative_ontology:constraint_victim(sotu_2005_bush_health_savings_accounts_expansion, chronically_ill_populations).
narrative_ontology:constraint_victim(sotu_2005_bush_health_savings_accounts_expansion, healthcare_cost_distribution_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME WORKER (SNARE) — Lacks discretionary income to pre-fund HSA contributions. Trapped in high-deductible plans with no tax advantage and no reserve fund. Forced into true cost-sharing at point of care with full barrier to access. Cannot exit: employer plan design is dictated; HSA cannot be funded on low wages; alternative coverage paths narrow as employer plans migrate to HDHP+HSA models. Maximum experienced extraction.
constraint_indexing:constraint_classification(sotu_2005_bush_health_savings_accounts_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CHRONICALLY ILL POPULATION (SNARE) — Pre-existing conditions mean high annual costs; HSA-attached high-deductible plans transfer maximum cost burden to those with inelastic demand. Risk pool deteriorates as healthy workers self-select into HDHP+HSA (accumulating tax savings) while sick workers remain in higher-cost options or go uninsured. Trapped by medical necessity and erosion of risk pooling. No exit from illness-driven costs; exit from plan = loss of continuity of care.
constraint_indexing:constraint_classification(sotu_2005_bush_health_savings_accounts_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-INCOME WORKER (ROPE) — Experiences HSA as pure coordination: tax-advantaged savings mechanism that aligns personal interest (building medical reserve) with system goal (cost consciousness). Can fund HSA easily; accumulates tax-free dollars; can defer withdrawals indefinitely (after-tax retirement savings). Exits are abundant: negotiates plan at work, maintains independent HMO option, or uses HSA funds for other health expenses. Net coordinator rather than coercee.
constraint_indexing:constraint_classification(sotu_2005_bush_health_savings_accounts_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HEALTH INSURER (TANGLED ROPE) — Benefits from cost-shifting (premiums lower for HDHP; cream-skimming via plan design attracts healthy workers; reduced claims burden). Also provides genuine coordination function: manages network, administers benefits, negotiates provider rates. The coordination is real; the extraction via risk pool selection is equally real. Active enforcement required: plan design choices, network narrowing, formulary restrictions, prior authorization thresholds all actively channel costs onto specific populations.
constraint_indexing:constraint_classification(sotu_2005_bush_health_savings_accounts_expansion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EMPLOYER (TANGLED ROPE) — Experiences genuine coordination benefit (reducing healthcare cost growth, transferring actuarial burden to employees) alongside extraction (can design plans to encourage profitable self-selection, retain younger/healthier workforces, reduce disabled worker enrollment through plan complexity). Active enforcement is required: must choose which plan options to offer, set employer contribution levels, communicate (or obscure) plan differences. Large employers have arbitrage; small employers constrained by insurer offerings.
constraint_indexing:constraint_classification(sotu_2005_bush_health_savings_accounts_expansion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: POLICY REFORMERS / ALTERNATIVE RISK POOLS (SCAFFOLD) — Organized agents (medical homes, cost-transparency initiatives, alternative payment models, public option proposals) see HDHP+HSA expansion as temporary extraction mechanism that creates political pressure for reform. Rising deductibles drive demand for price transparency and bundled payments. High-deductible plan failures (cost avoidance, delayed care, bankruptcies) create opening for cooperative risk-pooling and negotiated pricing alternatives. Sunset implicit in political backlash dynamics.
constraint_indexing:constraint_classification(sotu_2005_bush_health_savings_accounts_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: HISTORICAL RISK-POOLING NORM (PITON) — For 60+ years, employer health insurance operated on principle of age/health risk pooling and premium averaging. HDHP+HSA inverts this: explicit self-selection, transparent cost differentiation, individualized accumulation vs. mutual aid. The old pooling mechanism persists in background (administrative functions, network negotiation) but its normative power has eroded. HSA-indexed policies maintain ritual of 'individual responsibility' language while administrative pooling continues hidden. Theater ratio reflects this degradation: the rhetoric is consumer sovereignty; the practice is managed risk selection.
constraint_indexing:constraint_classification(sotu_2005_bush_health_savings_accounts_expansion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL ECONOMICS VIEW (MOUNTAIN) — From a civilizational lens, market economics teaches that price signals improve allocation and consumer incentives reduce excess consumption. HSA expansion appears as inevitable application of market discipline to healthcare's third-party payer problem. The constraint seems to emerge naturally from rational economic principles. However, structural data reveals this as a false summit: identifiable beneficiaries exist (high-income workers, insurers, financial services), extraction is measurable (risk pool deterioration, uncompensated cost shifts), and active enforcement is required (plan design, contribution limits, eligibility rules). The 'natural law' framing naturalizes a policy choice that concentrates gains and losses.
constraint_indexing:constraint_classification(sotu_2005_bush_health_savings_accounts_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2005_bush_health_savings_accounts_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2005_bush_health_savings_accounts_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2005_bush_health_savings_accounts_expansion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2005_bush_health_savings_accounts_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2005_bush_health_savings_accounts_expansion, TR),
    TR >= 0.70.

:- end_tests(sotu_2005_bush_health_savings_accounts_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The policy creates measurable extraction via risk pool deterioration, uncompensated cost shifts, and tax subsidy concentration. However, this is not maximal extraction — the coordination function is genuine (price signals do reduce some unnecessary care, HSA savings mechanism works as designed for those who can fund it), and the extraction is not survival-threatening (unlike predatory lending or labor trafficking). The trajectory shows extractiveness rising from 0.28 (baseline employer health insurance landscape in 2005) to 0.58 (mature HDHP+HSA market circa 2019) as cream-skimming accelerates and risk pools deteriorate. Suppression (0.62): Moderate-high. Low-income workers cannot easily exit HDHP+HSA plans when employers adopt them; they lack discretionary savings to pre-fund deductibles; alternative coverage paths narrow. However, suppression is not total — some states have expanded Medicaid, some employers maintain traditional plan options, and workers can technically switch employers or go uninsured. The suppression operates through limited alternatives rather than absolute coercion. Theater ratio (0.48): Moderate. The constraint operates with substantial functional content (actual cost-shifting does occur, actual savings accumulation by high-income workers does happen) but significant performative overlay ('consumer-directed care' rhetoric naturalizes extraction as individual responsibility; 'empowerment' narrative obscures asymmetry; risk pooling continues administratively while being narratively erased). The theater ratio is not high because the extraction mechanism is real and direct, not primarily maintained through symbolic performance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark and diagnostic. The high-income worker sees a coordination mechanism (Rope): policy aligns individual incentive (building reserves) with system goal (cost consciousness), generates real tax savings, enables autonomous choices. The low-income worker sees pure extraction (Snare): uncompensated cost-shifting with no escape path, deteriorating risk pools, asymmetric burden. The health insurer sees mixed coordination and extraction (Tangled Rope): genuinely reduced pooled risk exposure alongside active cream-skimming via plan design and network narrowing — both coordination and extraction are enforced, and they are inseparable. The employer sees tangled rope with less enforcement burden (Tangled Rope) than the insurer: real cost control benefits alongside real ability to influence workforce selection; some employers active in design, others passive. The policy reformer sees temporary extraction with a sunset clause (Scaffold): high-deductible plan failures (bankruptcies, delayed care) and public backlash are generating political pressure for alternative risk-pooling models; the HDHP+HSA phase is recognizable as temporary, not permanent. The historical risk-pooling norm sees a degraded version of itself (Piton): mutual aid principle persists in administrative functions but normative power has eroded; ritual of individualized pricing continues while background pooling remains. The analytical observer risks naturalizing the constraint as market discipline (Mountain, false summit) — the structural data contradicts this via identifiable beneficiaries, measurable extraction, and required active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from agents' structural positions relative to the extraction flow. High-income workers with arbitrage options (can choose plans, can fund HSA, have alternative coverage paths) derive low d values (0.15–0.30) — the extraction mechanism runs toward them, not away. The engine calculates negative or minimal χ from their perspective because they are beneficiaries with exit options. Low-income workers with trapped exit status (no discretionary savings, limited plan choices, medical necessity) derive high d values (0.85–0.95) — maximum experienced extraction. Health insurers with arbitrage options but whose core function is cream-skimming derive moderate d values (0.45–0.55) — they benefit from the extraction but also actively maintain it, so directionality is ambiguous between beneficiary and active extractor. Employers fall in between: some have powerful plan design choices (d ≈ 0.40), others are constrained by insurer offerings (d ≈ 0.65). The policy reformers derive d from their organized status and constrained but potentially increasing exit capacity (alternative models emerging), yielding d ≈ 0.35–0.45. The analytical observer at civilizational scale risks d ≈ 0.50 (symmetric view) but the false summit detector flags the mountain classification as naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by clarifying the extractive mechanism: HSA expansion is not a coordination problem with extraction as side effect, but an asymmetric policy choice that uses coordination language ('consumer-directed care,' 'individual responsibility') to justify extraction ('cost-shifting,' 'risk selection'). The tangled rope classification captures both functions: risk pool coordination is genuine (high-income workers accumulating reserves does reduce certain healthcare costs), and extraction is equally genuine (low-income workers bearing uncompensated deductible exposure while high-income workers capture tax benefits). The constraint cannot be simplified to pure extraction (Snare) because the coordination function is real — taxing high-income HSA accumulations without eliminating the savings mechanism would destroy coordination value. The constraint cannot be simplified to pure coordination (Rope) because the extraction function is real — the same high-deductible plan structure that incentivizes cost consciousness for high-income workers creates uncompensated barriers to care for low-income workers. Mandatrophy is resolved by accepting that the constraint simultaneously solves a real coordination problem (reducing unnecessary care, aligning incentives) and creates a real extraction mechanism (concentrating risk, shifting costs from insurable to individual burden). The policy question is not 'eliminate the coordination' or 'ignore the extraction,' but 'can the coordination be decoupled from the extraction?' — alternative risk-pooling models (medical homes, bundled payments, cost-transparency with maintained risk pools) suggest the answer is yes: cost consciousness and risk pooling are not mutually exclusive. This decoupling pathway constitutes the scaffold sunset mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    risk_pool_selection_mechanism,
    'Does HSA expansion primarily cause cream-skimming (healthy workers self-selecting into HDHP+HSA, leaving sicker workers in higher-cost plans) or does it reflect pre-existing selection patterns in employer plan offerings?',
    'Comparative analysis of risk profiles in HDHP+HSA vs. traditional plans before and after HSA expansion; worker demographic and health status stratification across plan types over time; regression analysis of switching behavior',
    'If cream-skimming is primary: risk pool deterioration is direct causal effect of HSA policy (snare classification strengthened). If pre-existing: HSA expansion accelerates but did not initiate selection (extraction mechanism already present but now amplified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_pool_selection_mechanism, empirical, 'Whether HSA expansion causes cream-skimming or accelerates pre-existing selection').

omega_variable(
    low_income_hsa_funding_capacity,
    'What proportion of low-income workers can actually fund HSA contributions in amounts sufficient to cover deductible obligations?',
    'Survey data on HSA funding rates by income quintile; analysis of HSA account balances and year-to-year rollovers; comparison of funding capacity vs. average deductible amounts; emergency savings adequacy studies',
    'If funding capacity < 20%: HSA is aspirational tool for most low-income workers, reducing to pure cost-shifting (snare strengthened). If > 50%: HSA functions as intended coordination mechanism for broader population (tangled_rope maintained).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(low_income_hsa_funding_capacity, empirical, 'Low-income worker capacity to fund HSA contributions').

omega_variable(
    cost_avoidance_medical_outcomes,
    'Does cost-shifting via HDHP+HSA create measurable health outcome deterioration through delayed care, reduced medication adherence, or avoidance of preventive services?',
    'Longitudinal health outcomes analysis comparing HDHP+HSA enrollees to traditional plan enrollees, controlling for baseline health status; hospitalizations for ambulatory-sensitive conditions; medication non-adherence rates; preventive care utilization; mortality/morbidity by deductible tier',
    'If outcome deterioration is substantial: extraction mechanism is creating health damage (snare/tangled_rope severity increased; suggests mandatrophy unresolved). If outcomes are neutral or improved: efficiency gains may justify cost-shifting (tangled_rope interpretation plausible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_avoidance_medical_outcomes, empirical, 'Health outcome impacts of cost-shifting through high-deductible plans').

omega_variable(
    tax_subsidy_distributional_impact,
    'Do tax advantages of HSA contributions disproportionately benefit high-income workers due to marginal tax rate differences and funding capacity disparities?',
    'Tax expenditure analysis of HSA tax subsidies; distributional incidence by income level; comparison of tax-free savings value across tax brackets; analysis of actual utilization patterns by income decile',
    'If high-income concentration > 70%: tax mechanism is explicitly extractive redistribution mechanism (snare for low-income cohort strengthened). If more distributed: tax design achieves neutral incidence (tangled_rope assessment holds).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tax_subsidy_distributional_impact, empirical, 'Distributional impact of HSA tax advantages across income levels').

omega_variable(
    alternative_risk_pooling_emergence,
    'Are organized alternatives to HDHP+HSA (medical homes, direct primary care, alternative payment models, cost-transparency initiatives, public options) materializing at sufficient scale and political momentum to constitute a real sunset mechanism?',
    'Enrollment and coverage trends in alternative payment models; political pressure analysis (legislative proposals, state-level initiatives); insurer and employer migration toward alternatives; longitudinal tracking of policy proposals',
    'If alternatives reach 20%+ of eligible population within 10 years: scaffold sunset is real (HDHP+HSA extraction phase is temporary). If alternatives remain marginal: sunset is aspirational rather than structural (scaffold classification weakened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_risk_pooling_emergence, empirical, 'Emergence and scale of alternative risk-pooling mechanisms').

omega_variable(
    employer_versus_insurer_extraction_locus,
    'Does the extraction mechanism reside primarily in employer plan-design choices (employer actively cream-skimming and cost-shifting) or in insurer premium pricing and network design (insurers profiting from risk selection)?',
    'Analysis of employer plan design strategy by size and market power; insurer premium pricing practices for HDHP vs. traditional products; administrative cost analysis; insurer profitability trends; employer self-insurance adoption rates',
    'If insurer-primary: extraction mechanism is concentrated in health insurance industry (easier to regulate/tax). If employer-primary: extraction is dispersed across thousands of employers (harder to address via single policy lever).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_versus_insurer_extraction_locus, empirical, 'Locus of extraction mechanism: employer plan design vs. insurer pricing').

omega_variable(
    cultural_narrative_naturalization,
    'To what extent does the ''consumer-directed care'' and ''individual responsibility'' narrative prevent political mobilization against cost-shifting by framing it as inevitable market discipline?',
    'Survey research on public attitudes toward HSA/HDHP; framing experiment analysis; longitudinal tracking of political discourse on healthcare cost-sharing; analysis of beneficiary vs. victim awareness of extraction mechanisms',
    'If narrative naturalization is high: constraint is sustained through performative coordination language (theater ratio higher, extraction harder to challenge). If public perception accurately reflects asymmetry: pressure for reform emerges (scaffold sunset accelerates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_narrative_naturalization, conceptual, 'Extent of narrative naturalization of cost-shifting as individual responsibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2005_bush_health_savings_accounts_expansion, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hsa_expansion_tr_t0, sotu_2005_bush_health_savings_accounts_expansion, theater_ratio, 0, 0.32).
narrative_ontology:measurement(hsa_expansion_tr_t7, sotu_2005_bush_health_savings_accounts_expansion, theater_ratio, 7, 0.4).
narrative_ontology:measurement(hsa_expansion_tr_t14, sotu_2005_bush_health_savings_accounts_expansion, theater_ratio, 14, 0.48).

% Extraction over time
narrative_ontology:measurement(hsa_expansion_be_t0, sotu_2005_bush_health_savings_accounts_expansion, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hsa_expansion_be_t7, sotu_2005_bush_health_savings_accounts_expansion, base_extractiveness, 7, 0.42).
narrative_ontology:measurement(hsa_expansion_be_t14, sotu_2005_bush_health_savings_accounts_expansion, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2005_bush_health_savings_accounts_expansion, resource_allocation).
narrative_ontology:affects_constraint(sotu_2005_bush_health_savings_accounts_expansion, employer_health_insurance_risk_selection).
narrative_ontology:affects_constraint(sotu_2005_bush_health_savings_accounts_expansion, medicaid_coverage_gaps).
narrative_ontology:affects_constraint(sotu_2005_bush_health_savings_accounts_expansion, medical_bankruptcy_epidemic).

% DUAL FORMULATION NOTE:
% HSA expansion is downstream of employer health insurance restructuring (risk selection trends) and upstream of medical bankruptcy and Medicaid gap outcomes. The constraint family represents a policy-induced shift in healthcare cost distribution: the upstream constraint involves employer motivation for cost control; the HSA expansion constraint involves the specific policy mechanism chosen; the downstream constraints involve the measurable health and financial outcomes of that mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2005_bush_health_savings_accounts_expansion, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
