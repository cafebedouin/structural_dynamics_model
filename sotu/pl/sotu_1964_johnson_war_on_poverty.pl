% ============================================================================
% CONSTRAINT STORY: sotu_1964_johnson_war_on_poverty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1964_johnson_war_on_poverty, []).

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
 *   constraint_id: sotu_1964_johnson_war_on_poverty
 *   human_readable: Federal-Local Cooperative Framework for Antipoverty Programs (1964-present)
 *   domain: social_policy/federal_local_coordination
 *
 * SUMMARY:
 *   The War on Poverty framework (launched in 1964) established a
 *   federal-local cooperative structure that combines federal funding and
 *   goal-setting with state and local implementation. This constraint
 *   demonstrates the central tension of American federalism: national
 *   problems (poverty, education quality, health disparities) require
 *   centralized resources, but their solution depends on decentralized
 *   institutional capacity. The constraint exhibits Tangled Rope
 *   characteristics because it serves a genuine coordination function
 *   (pooling federal resources to address problems exceeding local capacity)
 *   while simultaneously enabling asymmetric extraction in multiple
 *   directions: federal agencies expand their mandate and budgetary
 *   authority; local governments become dependent on federal conditions and
 *   compliance requirements; vulnerable populations receive benefits
 *   alongside paternalistic governance and stigmatizing application
 *   procedures. The theater ratio has increased from 0.35 (initial decade of
 *   genuine program innovation and delivery) to 0.58 (current era of
 *   entrenched bureaucratic performance metrics rather than poverty outcome
 *   measures). The base extractiveness has risen from 0.22 to 0.38 as federal
 *   compliance requirements have accumulated without corresponding increases
 *   in local capacity or program flexibility.
 *
 * KEY AGENTS:
 *   - Economically Disadvantaged Populations: Primary beneficiary (powerless/trapped in low-capacity regions; moderate/constrained in organized urban settings) — receive education, health, job training, and housing assistance but experience paternalistic governance and uneven access across jurisdictions
 *   - Federal Implementing Agencies: Secondary beneficiary (institutional/arbitrage) — capture expanded mandate, budgetary authority, institutional growth, and policy influence through the framework
 *   - Federal Taxpayer Base: Primary victim (powerful/mobile in high-income brackets; moderate/trapped in low-income brackets) — bear costs through progressive taxation and opportunity costs of foregone alternative spending
 *   - State and Local Governments: Mixed agent (organized/constrained) — coordinate service delivery while constrained by unfunded mandates, matching requirements, and loss of program autonomy
 *   - Program Implementation Fidelity: Structural victim (abstract, powerless) — no agent advocates for optimal benefit reach; administrative complexity drains resources that could flow to target populations
 *   - Reform Movement: Organized challenger (organized/mobile) — advocates for alternative mechanisms (direct federal benefits, UBI, program devolution) with explicit sunset logic for current framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1964_johnson_war_on_poverty, 0.38).
domain_priors:suppression_score(sotu_1964_johnson_war_on_poverty, 0.48).
domain_priors:theater_ratio(sotu_1964_johnson_war_on_poverty, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1964_johnson_war_on_poverty, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1964_johnson_war_on_poverty, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1964_johnson_war_on_poverty, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1964_johnson_war_on_poverty, tangled_rope).
narrative_ontology:human_readable(sotu_1964_johnson_war_on_poverty, "Federal-Local Cooperative Framework for Antipoverty Programs (1964-present)").
narrative_ontology:topic_domain(sotu_1964_johnson_war_on_poverty, "social_policy/federal_local_coordination").

domain_priors:requires_active_enforcement(sotu_1964_johnson_war_on_poverty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1964_johnson_war_on_poverty, economically_disadvantaged_populations).
narrative_ontology:constraint_beneficiary(sotu_1964_johnson_war_on_poverty, federal_implementing_agencies).
narrative_ontology:constraint_victim(sotu_1964_johnson_war_on_poverty, federal_taxpayer_base).
narrative_ontology:constraint_victim(sotu_1964_johnson_war_on_poverty, program_implementation_fidelity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL POOR / LOW-CAPACITY LOCALITIES (SNARE) — Trapped by geography, lack of alternative opportunities, and dependence on federal transfers that require local administrative capacity they lack. Suppression is structural: cannot relocate without severing community ties and losing informal support networks. Federal-local framework becomes an extraction mechanism when implementation burden falls on localities without capacity, reducing actual benefit reach.
constraint_indexing:constraint_classification(sotu_1964_johnson_war_on_poverty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: URBAN MINORITY COMMUNITY / ORGANIZED BENEFICIARY (TANGLED ROPE) — Benefits from coordinated federal-local programs (education, job training, health services) while constrained by uneven implementation quality across jurisdictions. Genuine coordination function exists (pooled federal resources address problems larger than local capacity); asymmetric extraction through selective implementation, paternalistic governance, and conditions on benefit access.
constraint_indexing:constraint_classification(sotu_1964_johnson_war_on_poverty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL IMPLEMENTING AGENCIES (ROPE) — Primary beneficiary of the framework through expanded mandate, budgetary authority, and institutional growth. Experiences the constraint as coordination: delegating authority to state/local partners enables scaling while preserving federal oversight. Net beneficiary — extraction runs toward federal agencies through budget expansion and mission creep.
constraint_indexing:constraint_classification(sotu_1964_johnson_war_on_poverty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE/LOCAL GOVERNMENT PARTNERS (TANGLED ROPE) — Coordinated function: federal funding enables local initiatives beyond local revenue capacity. Asymmetric extraction: unfunded mandates, compliance burden, matching requirements that drain local budgets, and loss of autonomy over program design. Exit constrained by federal funding dependence; genuine coordination benefit exists but embedded in asymmetric power dynamic.
constraint_indexing:constraint_classification(sotu_1964_johnson_war_on_poverty, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL LEGACY / THEATER VIEW (PITON) — The War on Poverty framework has persisted through institutional inertia despite changing economic conditions, demographic shifts, and program effectiveness debates. Much of the current architecture is theatrical: Head Start, Community Action Programs, and Job Corps persist as performative symbols of antipoverty commitment while their actual impact on poverty reduction is contested. Theater ratio has risen as programs became entrenched policy rather than responsive delivery mechanisms.
constraint_indexing:constraint_classification(sotu_1964_johnson_war_on_poverty, piton,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REFORM MOVEMENT / SUNSET LOGIC (SCAFFOLD) — Organized advocates view the federal-local framework as a temporary institutional structure requiring replacement with direct federal benefits (UBI, guaranteed income, vouchers) or complete devolution to market mechanisms. This perspective sees the War on Poverty framework as transitional coordination with an explicit sunset: as antipoverty policy matures, decentralized delivery through local bureaucracies will be replaced by programmatic mechanisms (direct payments, portable benefits) that don't require local administrative capacity.
constraint_indexing:constraint_classification(sotu_1964_johnson_war_on_poverty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FEDERALISM NATURAL LAW (MOUNTAIN) — From a civilizational perspective, federal-local coordination represents an immutable structural constraint of American governance: centralized problems require decentralized solutions, but decentralized capacity gaps require centralized resources. The tension between federal authority and local autonomy is built into the constitutional structure. However, this appears to be a false summit: the beneficiary structure (federal agencies gain mandate; organized poor gain access; taxpayers lose resources) reveals this as a contingent institutional arrangement, not a law of governance.
constraint_indexing:constraint_classification(sotu_1964_johnson_war_on_poverty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1964_johnson_war_on_poverty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1964_johnson_war_on_poverty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1964_johnson_war_on_poverty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1964_johnson_war_on_poverty, TR),
    TR >= 0.70.

:- end_tests(sotu_1964_johnson_war_on_poverty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The framework pools federal resources and establishes binding commitments to poverty reduction — genuine coordination value. But extractiveness rises above pure rope (0.45 threshold) because: (1) federal agency mission creep enables extraction of benefits toward expanding bureaucracy; (2) local government dependence on federal matching and compliance costs creates rent-seeking opportunities; (3) means-testing and targeting add administrative burden that reduces benefit reach. The value reflects that benefits do flow to intended populations, but with significant overhead and asymmetric power. Suppression (0.48): Moderate. Structural barriers include: geographic immobility of rural poor, local capacity gaps creating implementation barriers, paternalistic administration reducing dignity of benefit receipt, and political stigma attached to poverty programs. But suppression is not total — urban organized groups can mobilize, federal authority constrains the worst local practices, and program access is legally mandated. Theater ratio (0.52): Moderate-high. The framework has accumulated substantial theatrical elements: performance metrics on program completion rather than poverty outcomes, visibility of federal agency mission statements over actual service quality, and compliance theater (documentation and reporting requirements) that substitute for effectiveness measures. Theater has risen as programs matured from innovative delivery mechanisms to entrenched bureaucratic structures.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a sharp perspectival gap between institutional beneficiaries and trapped populations. Federal agencies and organized beneficiaries perceive coordination (Rope from agency perspective: benefits from expanded authority; Tangled Rope from organized community perspective: genuine benefit amid extraction). Rural and low-capacity populations perceive extraction (Snare: trapped with no local alternatives and fragmented federal programs). State/local governments perceive mixed coordination and constraint (Tangled Rope: benefits from federal funding offset by compliance burden and lost autonomy). The reform movement perceives a temporary institution with a sunset path (Scaffold: direct benefits or devolution will replace federal-local framework). The civilizational observer risks naturalizing this arrangement as federalism's immutable structure (Mountain), but the beneficiary distribution reveals it as a contingent institutional design—false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs across agent populations. Economically disadvantaged populations show d ≈ 0.85-0.95 (trapped/constrained victims), yielding high f(d) and experienced extraction chi. Federal agencies show d ≈ 0.10-0.20 (beneficiaries with institutional authority), yielding low/negative f(d) and net benefit chi. State/local governments show d ≈ 0.55-0.65 (mixed agents with constrained exit), yielding moderate f(d) and mid-range extraction. The perspectives reflect these directionality positions: powerless perspectives classify as snare (high d), institutional beneficiaries as rope (low d), constrained mixed agents as tangled rope (moderate d). The analytical observer's mountain perspective masks this directionality structure by naturalizing federalism as an immutable constraint—a false summit revealing how political-institutional arrangements disguise themselves as natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   The War on Poverty framework resolves mandatrophy by showing that federal-local coordination genuinely addresses a real problem (poverty exceeds local government capacity to solve) while simultaneously creating asymmetric extraction flows. The mandate is NOT a mislabeled snare (pure extraction): genuine benefits reach intended populations through pooled resources. But it is NOT pure rope (coordination without asymmetry): federal agency mandate expansion, local government compliance burden, and administrative overhead all extract value away from target populations. The constraint is tangled rope because both coordination and extraction are structural features—removing either feature (pure federalism without coordination or pure federal delivery without local autonomy) would worsen outcomes but in different ways. The rising theater ratio (0.35→0.58) indicates Goodhart drift: as programs aged, performance metrics (program completion, enrollment numbers) replaced outcome metrics (poverty reduction), substituting visible activity for effectiveness. This is the piton mechanism: institutional machinery persists through inertia even as its functional effectiveness declines.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    program_implementation_fidelity_threshold,
    'What implementation fidelity threshold distinguishes genuine poverty reduction from extractive bureaucratic overhead?',
    'Longitudinal tracking of benefit reach: percentage of federal poverty reduction budget that flows to target populations vs administrative costs; comparison of outcomes in high-capacity vs low-capacity jurisdictions',
    'If fidelity > 0.70: framework is tangled rope with significant benefit component. If fidelity < 0.40: framework approaches snare (extraction masquerading as benefit). If fidelity varies by region: amplifies the victimhood of low-capacity localities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(program_implementation_fidelity_threshold, empirical, 'Implementation fidelity threshold distinguishing benefit from overhead').

omega_variable(
    local_capacity_gap_structural,
    'Is the federal-local gap a structural feature of federalism or a remediable capacity deficit?',
    'Historical analysis of capacity building: did federal investment in local administrative infrastructure narrow the gap over time? Comparative analysis across nations with different federal structures.',
    'If structural (irreducible): the snare classification dominates low-capacity regions permanently. If remediable: capacity building is an exit path for trapped agents — constraint could shift to rope as local infrastructure matures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_capacity_gap_structural, empirical, 'Whether federal-local capacity gap is structural or remediable').

omega_variable(
    political_extraction_vs_bureaucratic_extraction,
    'Is the asymmetry in the federal-local framework driven by political extraction (federal agencies capturing benefits) or bureaucratic overhead (compliance costs)?',
    'Structural analysis: compare outcomes when federal agencies have partisan incentive to show results vs when agencies operate under merit-based civil service; track whether partisan cycles correlate with program fidelity changes.',
    'If political: changing administration changes extraction direction and magnitude — constraint becomes unstable. If bureaucratic: extraction is more stable and system becomes optimizable through administrative reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_extraction_vs_bureaucratic_extraction, empirical, 'Whether extraction is politically or bureaucratically driven').

omega_variable(
    targeted_vs_universalist_mechanism_conflict,
    'Does means-testing and targeting (poverty-specific programs) create extraction through stigma and complexity, compared to universal benefits reaching the same populations?',
    'Comparative effectiveness: outcomes and uptake rates in universal programs (SNAP, Social Security) vs targeted programs (TANF, housing assistance); cost per beneficiary reached.',
    'If targeting increases extraction: the tangled rope could shift toward snare as administrative burden grows. If universal outreach proves infeasible: targeting remains necessary despite extraction costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(targeted_vs_universalist_mechanism_conflict, empirical, 'Whether targeted programs extract more than universal alternatives').

omega_variable(
    federal_agency_mandate_expansion,
    'Does the federal-local framework enable federal agencies to expand missions beyond congressional intent (bureaucratic drift)?',
    'Institutional analysis: track program budget allocations and scope expansion compared to original legislation; measure mission creep through decade-by-decade comparison of authorized vs actual program roles.',
    'If significant drift: beneficiary extraction toward federal agencies is greater than base model suggests. If minimal: federal benefit is genuinely limited to coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_agency_mandate_expansion, empirical, 'Magnitude of federal agency mandate expansion over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1964_johnson_war_on_poverty, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wop_tr_t0, sotu_1964_johnson_war_on_poverty, theater_ratio, 0, 0.35).
narrative_ontology:measurement(wop_tr_t15, sotu_1964_johnson_war_on_poverty, theater_ratio, 15, 0.48).
narrative_ontology:measurement(wop_tr_t30, sotu_1964_johnson_war_on_poverty, theater_ratio, 30, 0.52).
narrative_ontology:measurement(wop_tr_t60, sotu_1964_johnson_war_on_poverty, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(wop_be_t0, sotu_1964_johnson_war_on_poverty, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(wop_be_t15, sotu_1964_johnson_war_on_poverty, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(wop_be_t30, sotu_1964_johnson_war_on_poverty, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(wop_be_t60, sotu_1964_johnson_war_on_poverty, base_extractiveness, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1964_johnson_war_on_poverty, resource_allocation).
narrative_ontology:affects_constraint(sotu_1964_johnson_war_on_poverty, public_education_quality_variation).
narrative_ontology:affects_constraint(sotu_1964_johnson_war_on_poverty, healthcare_access_disparity).
narrative_ontology:affects_constraint(sotu_1964_johnson_war_on_poverty, housing_market_segregation).
narrative_ontology:affects_constraint(sotu_1964_johnson_war_on_poverty, job_training_program_effectiveness).

% DUAL FORMULATION NOTE:
% The War on Poverty framework coordinates across multiple sectoral constraints (education, health, housing, employment). Each sector has its own ε value reflecting empirical success rates in that domain. The framework constraint itself captures the coordination function and extraction overhead that applies across all sectoral programs, independent of individual program efficacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1964_johnson_war_on_poverty, powerless, 0.9).
constraint_indexing:directionality_override(sotu_1964_johnson_war_on_poverty, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
