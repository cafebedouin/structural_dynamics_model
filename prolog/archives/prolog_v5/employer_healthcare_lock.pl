% ============================================================================
% CONSTRAINT STORY: employer_healthcare_lock
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employer_healthcare_lock, []).

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
 *   constraint_id: employer_healthcare_lock
 *   human_readable: Employer-Mediated Healthcare Lock in the United States
 *   domain: economic/labor/healthcare
 *
 * SUMMARY:
 *   The employer-mediated healthcare system in the United States emerged from
 *   a historical accident: wage controls during World War II made
 *   employer-provided health insurance an untaxed form of compensation,
 *   creating a structural incentive to couple employment and healthcare
 *   access. This constraint now affects over 160 million Americans and
 *   creates a sophisticated extraction mechanism where workers become trapped
 *   by the convergence of employment dependency and healthcare necessity. The
 *   system performs genuine coordination (large employer group pools reduce
 *   adverse selection and lower individual premium burden) while
 *   simultaneously extracting significant value through labor mobility
 *   suppression, wage stagnation, and unequal access based on employment
 *   status. The extractiveness value has declined from 0.72 (pre-ACA, when
 *   pre-existing condition exclusions and medical underwriting created
 *   maximal extraction) to 0.58 (post-ACA, when coverage mandates and
 *   subsidies created partial alternatives). However, the suppression has
 *   remained high (0.65) because even with ACA protections, individual market
 *   premiums and deductibles remain substantially higher than employer-group
 *   rates, maintaining structural barriers to exit. The constraint exhibits
 *   all characteristics of a Snare: high base extractiveness (0.58), high
 *   suppression (0.65), minimal coordination benefit for vulnerable
 *   populations, and absence of genuine exit options for workers with health
 *   conditions or families.
 *
 * KEY AGENTS:
 *   - Employees with Chronic Conditions: Primary victims (powerless/trapped) — medical necessity and fear of coverage loss prevent exit; face catastrophic costs if employment is lost
 *   - Early-Career Workers and Families: Secondary victims (powerless/trapped) — dependent healthcare creates employment lock; family coverage continuity prevents job mobility
 *   - Healthy Mobile Workers: Mixed agents (moderate/constrained) — experience some coordination benefit but face substantial switching costs; could exit but at high cost
 *   - Large Employers: Primary beneficiaries (institutional/arbitrage) — capture retention value from healthcare lock without providing competitive compensation; group coverage solves employee coordination problem on employer terms
 *   - Health Insurance Carriers: Secondary beneficiaries (institutional/arbitrage) — group markets provide predictable member populations and reduce adverse selection; employer contracts are their core business model
 *   - Independent Contractors and Self-Employed: Victims by exclusion (powerless/trapped) — entirely excluded from group purchasing power; face individual market premiums 1.5-3x employer rates
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees constraint as path-dependent institutional artifact, not medical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employer_healthcare_lock, 0.58).
domain_priors:suppression_score(employer_healthcare_lock, 0.65).
domain_priors:theater_ratio(employer_healthcare_lock, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employer_healthcare_lock, extractiveness, 0.58).
narrative_ontology:constraint_metric(employer_healthcare_lock, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(employer_healthcare_lock, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employer_healthcare_lock, snare).
narrative_ontology:human_readable(employer_healthcare_lock, "Employer-Mediated Healthcare Lock in the United States").
narrative_ontology:topic_domain(employer_healthcare_lock, "economic/labor/healthcare").

domain_priors:requires_active_enforcement(employer_healthcare_lock).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employer_healthcare_lock, large_employers).
narrative_ontology:constraint_beneficiary(employer_healthcare_lock, health_insurance_carriers).
narrative_ontology:constraint_beneficiary(employer_healthcare_lock, pharmaceutical_manufacturers).
narrative_ontology:constraint_victim(employer_healthcare_lock, employees_with_health_conditions).
narrative_ontology:constraint_victim(employer_healthcare_lock, early_career_workers).
narrative_ontology:constraint_victim(employer_healthcare_lock, independent_contractors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMPLOYEE WITH CHRONIC CONDITION (SNARE) — Faces material barriers to exit: losing employer coverage means losing continuity of care, facing pre-existing condition exclusions (pre-ACA context) or premium hikes, and bearing catastrophic out-of-pocket costs. Cannot leave the job without risking medical bankruptcy. Suppression is structural — the coupling of employment and healthcare coverage creates a direct financial penalty for exit. Maximum experienced extraction.
constraint_indexing:constraint_classification(employer_healthcare_lock, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EARLY-CAREER WORKER (SNARE) — Family health insurance depends on maintaining employment. Job mobility is severely constrained — changing employers means coverage gaps, requalification delays, and exposure. No meaningful alternative (individual market is prohibitively expensive for young families). Trapped by structural dependency, not personal choice. The constraint extracts career mobility options.
constraint_indexing:constraint_classification(employer_healthcare_lock, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: HEALTHY MOBILE WORKER (TANGLED ROPE) — Experiences genuine coordination (employer provides group purchasing power that reduces individual premium burden) alongside asymmetric extraction (employer captures surplus from group rate negotiation, conditions benefits on continued employment). High exit costs (loses group rate access) but not catastrophic (personal health risk is lower). Constrained by switching costs, not trapped by medical necessity. Some coordination benefit genuine.
constraint_indexing:constraint_classification(employer_healthcare_lock, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE EMPLOYER (ROPE) — Benefits substantially from lock-in: employee retention without requiring competitive wages, workforce stability, reduced turnover costs. Experiences the constraint as pure coordination — providing group coverage solves collective action problem for healthcare access. No pressure to exit; can arbitrage to alternative workforce strategies. Minimal experienced extraction.
constraint_indexing:constraint_classification(employer_healthcare_lock, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HEALTH INSURANCE CARRIER (ROPE) — Employer group markets are their primary revenue and risk distribution model. Group plans bundle diverse health risks, reducing adverse selection. The constraint solves a genuine market coordination problem (adverse selection in individual markets). Benefits from lock-in through predictable member populations. Experiences as coordination mechanism.
constraint_indexing:constraint_classification(employer_healthcare_lock, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (GENERATIONAL VIEW) — From a comparative health systems perspective, the employer-mediated model is a historical artifact of US wage controls during WWII. Other wealthy democracies decouple healthcare from employment (Germany, Canada, Nordic countries). The constraint reflects path dependence and regulatory capture, not medical necessity. The model generates genuine coordination problems it purports to solve (adverse selection) while creating extraction inefficiencies (labor mobility reduction, wage suppression, unequal access). Classification: Tangled Rope because the constraint does coordinate healthcare access but extracts significant value through labor mobility restriction.
constraint_indexing:constraint_classification(employer_healthcare_lock, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employer_healthcare_lock_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(employer_healthcare_lock, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(employer_healthcare_lock, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(employer_healthcare_lock, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employer_healthcare_lock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximal. The employer-healthcare system extracts substantial value through labor mobility suppression and wage suppression (workers accept lower wages in exchange for healthcare; employers retain workers who would otherwise quit). However, extraction is not at the maximum (0.72) because: (1) ACA coverage mandates and subsidies created partial alternatives to employer coverage, (2) some employers genuinely invest in employee welfare (not pure extraction), and (3) healthy workers with minimal healthcare needs experience lower extraction. The 0.58 value reflects the average across populations with different health statuses and employment options. Suppression (0.65): High. Structural barriers to exit include: medical necessity (people need continuous coverage), financial barriers (individual premiums and out-of-pocket costs far exceed employer-sponsored costs), regulatory barriers (pre-existing condition restrictions, though mitigated post-ACA), informational barriers (complexity of individual market navigation), and psychological barriers (fear of coverage gaps). Suppression is lower than the maximum (0.85) because some pathways exist (ACA exchanges, state programs, spousal coverage) and some workers can exit (healthy individuals, high-income workers, those with partner coverage). Theater ratio (0.48): Moderate-low. The constraint relies less on performative legitimation than on structural financial barriers. The system is legitimized through: (1) framing as employee benefit rather than lock-in mechanism, (2) employer rhetoric about 'competitive benefits packages', (3) narrative of healthcare as complex coordination problem requiring employer mediation. But the theater is not high because the extraction mechanism is primarily material (expensive alternatives) rather than ideological. If the constraint relied on narrative alone (theater high), it would degrade faster as alternatives became available.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence: trapped employees see pure extraction (Snare); healthy mobile workers see mixed coordination and extraction (Tangled Rope); employers see pure coordination (Rope). This gap reveals that the constraint operates differently across populations based on health status and exit options. An employee with diabetes classified as 'powerless/trapped' experiences ε→1.0 extraction; a young healthy employee classified as 'moderate/constrained' experiences ε→0.40 extraction from the same institutional structure. The suppression mechanism is structural (expensive alternatives) but experienced differently based on medical necessity. The analytical observer's generational perspective reveals that the constraint is not inherent to healthcare coordination — all other wealthy democracies coordinate healthcare without employment coupling. This is a false summit trap: the system legitimizes itself by naturalizing employment-healthcare coupling as necessary for risk pooling, when in fact alternative models (state pooling, multi-employer pools, professional association pools) solve the same coordination problem without extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent derives from their beneficiary/victim status and exit options. Large employers and insurance carriers: d ≈ 0.05 (full beneficiaries with arbitrage options). This produces low f(d) ≈ -0.12, making χ negative — they experience subsidy, not extraction. Healthy mobile workers: d ≈ 0.55 (mixed: benefit from group rates but pay switching costs). This produces f(d) ≈ 0.75, making χ moderate. Early-career workers with families: d ≈ 0.85 (victims with constrained exit). This produces f(d) ≈ 1.15, making χ high. Employees with chronic illness: d ≈ 0.95 (victims with trapped exit). This produces f(d) ≈ 1.42, making χ maximal. The directionality pipeline correctly captures that the same constraint extracts different amounts from different agents based on medical necessity and exit capacity. No overrides are required; the beneficiary/victim declarations and exit_options produce accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION REQUIRED: The employer-healthcare system should decompose into 2-3 linked constraints with different ε values, because the coordination function varies by observable: (1) Group-based adverse selection pooling (ε≈0.15, Rope) — genuine coordination benefit that could not be achieved through individual markets. (2) Labor mobility suppression (ε≈0.58, Snare) — extraction mechanism that coupling employment to healthcare enables. (3) Wage suppression (ε≈0.40, Tangled Rope) — mixed coordination (employers coordinate compensation) and extraction (workers accept lower wages). These are structurally distinct constraints with different beneficiaries, victims, and reform pathways. The current single-story representation conflates them; a decomposed family would enable more precise policy analysis. For now, the 0.58 Snare classification reflects dominance of the labor mobility suppression mechanism for vulnerable populations. Mandatrophy is resolved by recognizing that the constraint is not 'is the employer system a coordination mechanism or an extraction mechanism?' but 'for which populations is it which?' The answer depends on health status, family structure, and employment alternatives — these are perspectival variables, not universal properties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pre_aca_post_aca_extraction_magnitude,
    'Has the Affordable Care Act (2010) reduced the extractiveness of the employer healthcare lock, or merely shifted its mechanism?',
    'Comparative analysis of voluntary job-leaving rates before/after ACA; measurement of premium differentials between group and individual markets post-ACA; analysis of pre-existing condition waiver prevalence before/after coverage mandate',
    'If ACA substantially reduced extraction: constraint may downgrade to Tangled Rope baseline with declining theater. If ACA merely shifted mechanism (group rates remain advantageous, individual market still expensive, medical underwriting shifted to employer selection): suppression remains high and extraction persists with different surface.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pre_aca_post_aca_extraction_magnitude, empirical, 'ACA impact on employer healthcare lock extraction magnitude').

omega_variable(
    exit_cost_individual_market_sufficiency,
    'Are individual market healthcare costs (post-ACA exchanges/subsidies) genuinely affordable alternatives, or do they remain prohibitively expensive for working families?',
    'Analysis of total cost of ownership (premium + deductible + out-of-pocket max) for family coverage: employer-sponsored vs individual market at various income levels; tracking of actual enrollment in individual market vs employer group plans among voluntary job-changers',
    'If individual market is genuinely affordable: many workers perceive exit option as mobile rather than trapped — constraint downgrades toward Tangled Rope. If individual market remains expensive or covers fewer providers: suppression remains high, exit remains trapped, Snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_individual_market_sufficiency, empirical, 'Individual market affordability as genuine exit option').

omega_variable(
    voluntary_versus_coercive_retention,
    'What fraction of employer-mediated healthcare lock is voluntary (employees value the coordination benefit and accept employment coupling) versus coercive (employees stay trapped despite preferring alternatives)?',
    'Surveys of employee preferences with hypothetical scenarios; analysis of job-switching decisions in cohorts with and without dependent care; comparison of stated reasons for staying vs leaving jobs in populations with different health status',
    'If predominantly voluntary: constraint may be Rope for most agents (coordination benefit is genuine and valued). If predominantly coercive: Snare classification stands and theater may increase (lock is legitimized through narrative of employee choice).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_versus_coercive_retention, empirical, 'Proportion of lock that is voluntary vs coercive').

omega_variable(
    employment_wage_suppression_coupling,
    'Does employer-provided healthcare suppress wage growth because employers substitute health benefits for salary, reducing workers'' ability to exit?',
    'Wage regression analysis: comparison of total compensation (wages + health insurance value) vs wages-only in sectors with different healthcare models; analysis of wage stagnation acceleration post-employer-healthcare-expansion; international comparison of wage growth in employer-mediated vs state-mediated healthcare systems',
    'If coupled: healthcare lock amplifies wage suppression, increasing effective extraction. If decoupled: healthcare lock is primarily a mobility trap, not a wage suppression mechanism. Changes interpretation of beneficiary-victim relationship (employers benefit from both lock-in and wage suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_wage_suppression_coupling, empirical, 'Healthcare coupling to wage suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employer_healthcare_lock, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ehcl_tr_t0, employer_healthcare_lock, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ehcl_tr_t10, employer_healthcare_lock, theater_ratio, 10, 0.42).
narrative_ontology:measurement(ehcl_tr_t20, employer_healthcare_lock, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(ehcl_be_t0, employer_healthcare_lock, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(ehcl_be_t10, employer_healthcare_lock, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(ehcl_be_t20, employer_healthcare_lock, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employer_healthcare_lock, resource_allocation).
narrative_ontology:affects_constraint(employer_healthcare_lock, wage_suppression_coupling).
narrative_ontology:affects_constraint(employer_healthcare_lock, healthcare_adverse_selection_pooling).
narrative_ontology:affects_constraint(employer_healthcare_lock, labor_mobility_restriction).

% DUAL FORMULATION NOTE:
% Employer-mediated healthcare lock is downstream of three distinct structural constraints: (1) adverse selection in healthcare markets (why pooling is needed), (2) tax code incentive structure (why coupling employment to healthcare made sense post-WWII), and (3) labor market competition for retention (how employers extracted value from health benefits). The ε=0.58 value reflects dominance of labor mobility extraction in the current regime. Decomposition into separate stories would enable precision analysis of reform pathways: breaking the employment coupling (ε↓), expanding multi-employer pools (coordination ↑), or decoupling wages from benefits (extraction mechanism reduction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
