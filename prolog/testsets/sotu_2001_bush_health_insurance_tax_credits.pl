% ============================================================================
% CONSTRAINT STORY: sotu_2001_bush_health_insurance_tax_credits
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2001_bush_health_insurance_tax_credits, []).

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
 *   constraint_id: sotu_2001_bush_health_insurance_tax_credits
 *   human_readable: Refundable Tax Credits for Health Insurance Purchase (2001 Bush Proposal)
 *   domain: healthcare/economic_policy
 *
 * SUMMARY:
 *   The refundable tax credit mechanism for health insurance purchase
 *   represents a market-based policy approach to expanding coverage among
 *   uninsured working Americans. Rather than direct government provision
 *   (single-payer), employer mandate, or public option, the mechanism uses
 *   federal tax policy to subsidize private insurance purchases by reducing
 *   out-of-pocket costs. The constraint exhibits simultaneous coordination
 *   (enabling uninsured workers to purchase coverage) and extraction
 *   (benefiting insurance companies through expanded subsidized demand while
 *   federal treasury bears cost through foregone revenue). The mechanism
 *   preserves private insurance market choice while maintaining
 *   employment-based coverage as the primary pathway—structural inertia
 *   embodied in policy. This creates a hybrid Tangled Rope classification
 *   from the analytical perspective, with radically different classifications
 *   from beneficiaries (insurance industry sees Rope), victims (chronically
 *   ill see Snare), and stakeholder coalitions (public health advocates see
 *   Scaffold). The theater ratio rises over the interval as the mechanism's
 *   performative function becomes apparent—it appears to solve access
 *   problems while structural barriers (pre-existing exclusions, deductible
 *   structures, geographic cost variation) remain unchanged.
 *
 * KEY AGENTS:
 *   - Uninsured working Americans: Primary stated beneficiary (moderate/constrained) — experience mixed coordination (credit reduces cost) and extraction (inadequate for full coverage, regional variation)
 *   - Private insurance industry: Primary unstated beneficiary (institutional/arbitrage) — captures demand expansion and subsidy without price competition, benefits from expanded insurable population
 *   - Federal treasury: Victim (institutional/constrained) — bears cost through foregone tax revenue; constrained to defend deficit rather than expand program
 *   - Chronically ill uninsured: Structural victim (powerless/trapped) — credit insufficient due to pre-existing exclusion and risk-pool mechanics; no exit from constraint
 *   - Employers avoiding direct provision: Secondary beneficiary (institutional/arbitrage) — credit enables shift of coverage cost to federal subsidy rather than employer benefit
 *   - High-cost geographic regions: Structural victim (moderate/constrained) — uniform federal credit inadequate; regional cost variation creates inequality
 *   - Public health advocates: Organized stakeholder (organized/constrained) — see mechanism as temporary scaffold pending comprehensive reform but constrained by political feasibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2001_bush_health_insurance_tax_credits, 0.52).
domain_priors:suppression_score(sotu_2001_bush_health_insurance_tax_credits, 0.48).
domain_priors:theater_ratio(sotu_2001_bush_health_insurance_tax_credits, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2001_bush_health_insurance_tax_credits, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_2001_bush_health_insurance_tax_credits, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_2001_bush_health_insurance_tax_credits, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2001_bush_health_insurance_tax_credits, tangled_rope).
narrative_ontology:human_readable(sotu_2001_bush_health_insurance_tax_credits, "Refundable Tax Credits for Health Insurance Purchase (2001 Bush Proposal)").
narrative_ontology:topic_domain(sotu_2001_bush_health_insurance_tax_credits, "healthcare/economic_policy").

domain_priors:requires_active_enforcement(sotu_2001_bush_health_insurance_tax_credits).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2001_bush_health_insurance_tax_credits, private_insurance_companies).
narrative_ontology:constraint_beneficiary(sotu_2001_bush_health_insurance_tax_credits, uninsured_working_americans).
narrative_ontology:constraint_beneficiary(sotu_2001_bush_health_insurance_tax_credits, employers_avoiding_direct_provision).
narrative_ontology:constraint_victim(sotu_2001_bush_health_insurance_tax_credits, federal_treasury).
narrative_ontology:constraint_victim(sotu_2001_bush_health_insurance_tax_credits, high_cost_geographic_regions).
narrative_ontology:constraint_victim(sotu_2001_bush_health_insurance_tax_credits, chronically_ill_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHRONICALLY ILL UNINSURED WORKER (SNARE) — Tax credit insufficient to purchase coverage in high-risk pools or with high deductibles. Pre-existing condition exclusions (legal until 2014) trap this agent despite subsidy. Cannot exit the constraint without abandoning healthcare access or spending beyond the credit. Maximum extraction: subsidy creates illusion of access while maintaining structural exclusion.
constraint_indexing:constraint_classification(sotu_2001_bush_health_insurance_tax_credits, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HEALTHY UNINSURED WORKER IN HIGH-COST REGION (TANGLED ROPE) — Tax credit enables some insurance purchase (coordination benefit) but deductibles and cost-sharing remain prohibitive. Regional variation in premium costs means credit value differs dramatically by location. Constrained: can purchase insurance but practical coverage remains out of reach in expensive markets. Mixed extraction and coordination.
constraint_indexing:constraint_classification(sotu_2001_bush_health_insurance_tax_credits, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRIVATE INSURANCE INDUSTRY (ROPE) — Tax credits represent pure demand expansion without price controls or direct regulation. Insurers experience this as coordination (market mechanism enabling customer access) with significant extraction benefit (expanded insurable population at federal subsidy). Can arbitrage between premium-setting and subsidy capture. Low effective extraction from their perspective — the mechanism funds their business expansion.
constraint_indexing:constraint_classification(sotu_2001_bush_health_insurance_tax_credits, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH ADVOCATES AND EMPLOYER COALITION (SCAFFOLD) — Sees tax credits as temporary mechanism pending more comprehensive reform. Coordination function: expands coverage incrementally while allowing private market to mature. Sunset logic implicit: credits are scaffolding until political consensus reaches universal coverage or public option. Theater ratio moderate — mechanism performs market-efficiency narrative while structural barriers remain.
constraint_indexing:constraint_classification(sotu_2001_bush_health_insurance_tax_credits, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EMPLOYMENT-BASED INSURANCE SYSTEM (PITON) — Tax credits preserve and entrench employer-sponsored insurance as the primary coverage pathway despite structural degradation. System increasingly fails to cover part-time workers, gig economy participants, and small-business employees. Credits patch the failure (performative solution) while institutional inertia prevents fundamental restructuring. Theater ratio high: mechanism maintains appearance of employer-based universal access despite growing gaps.
constraint_indexing:constraint_classification(sotu_2001_bush_health_insurance_tax_credits, piton,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Market efficiency doctrine treats private insurance with subsidy as natural equilibrium: consumers choose, insurers compete, prices adjust. From this perspective, tax credits are coordination mechanism enabling Pareto improvement with no coercion. However, structural data contradicts mountain classification — significant beneficiaries (insurance industry, employers avoiding direct provision) and victims (chronically ill, high-cost regions) reveal this as naturalizing policy choice. False summit candidate.
constraint_indexing:constraint_classification(sotu_2001_bush_health_insurance_tax_credits, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2001_bush_health_insurance_tax_credits_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2001_bush_health_insurance_tax_credits, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2001_bush_health_insurance_tax_credits, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2001_bush_health_insurance_tax_credits, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2001_bush_health_insurance_tax_credits, TR),
    TR >= 0.70.

:- end_tests(sotu_2001_bush_health_insurance_tax_credits_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The mechanism extracts from the federal treasury and accepts higher insurance industry profits through subsidized demand expansion without price constraints. Initial extractiveness lower (0.35) because the mechanism could plausibly coordinate access. It increases to 0.52 as structural barriers prevent the coordination function from materializing—credits subsidize inadequate coverage for high-risk and high-cost populations. The rise reflects discovery that extraction mechanism (subsidizing insurance company profits via expanded enrollee base) dominates coordination mechanism (enabling uninsured access). Suppression (0.48): Moderate. Barriers to exit are substantial but not insurmountable. Workers can purchase coverage with credit but face high deductibles, cost-sharing, and exclusions. Pre-existing condition exclusions (legal until 2014) create effective traps. Geographic cost variation means some regions' suppression is higher. Theater ratio (0.55): Moderate-high, rising over interval. Mechanism performs the narrative of market-based access expansion while preserving employment-based system inertia. Rising theater reflects increasing performative function as access gaps persist despite credits.
 *
 * PERSPECTIVAL GAP:
 *   Insurance industry sees pure coordination and benefit (Rope): mechanism enables market growth and customer base expansion through subsidy. Healthy uninsured in low-cost regions see mixed benefit and cost (Tangled Rope or Rope): credit enables coverage purchase; they coordinate the market and benefit from access. Chronically ill uninsured see extraction traps (Snare): credit is insufficient and pre-existing exclusions prevent coverage. High-cost region residents see regional Snare: uniform credit inadequate. Public health advocates see temporary scaffolding (Scaffold): mechanism coordinates access incrementally pending comprehensive reform with sunset as universal coverage approaches. Employment-based insurance system sees preservation of institutional inertia (Piton): credit patches gaps in employment linkage while theatrical maintenance prevents fundamental restructuring. Analytical observer risks seeing natural market equilibrium (Mountain): tax credits enable Pareto improvement through subsidy. False summit: structural data reveals beneficiaries (insurance industry, employers) and victims (chronically ill, high-cost regions), contradicting natural law classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by agent structural position. Insurance industry (institutional/arbitrage/beneficiary) experiences low d (~0.15), producing negative χ—the mechanism subsidizes their expansion at federal cost. Uninsured workers (moderate/constrained) experience high d (~0.65-0.75 depending on health risk and region), producing moderate-high χ—they benefit from subsidized access but face non-subsidized costs and exclusions. Chronically ill uninsured (powerless/trapped) experience maximum d (~0.95), producing maximum χ—credit irrelevant due to structural exclusion, full extraction. Federal treasury (institutional/constrained) experiences moderate d (~0.55), constrained by political inability to limit subsidy magnitude despite rising costs. Geographic cost variation creates differentiated d within the uninsured worker category: high-cost regions higher d (trapped by inadequate credit), low-cost regions lower d (credit more effective). Inter-agent directionality conflict is core to Tangled Rope classification—beneficiaries and victims are structurally enmeshed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination (expanding access for some populations) from extraction masquerading as coordination (subsidizing insurance industry profit expansion while creating performative appearance of access). The mechanism simultaneously coordinates (enables uninsured purchase) and extracts (from federal treasury and chronically ill populations). The Tangled Rope classification is correct at analytical perspective: active enforcement required (IRS administration), beneficiaries exist (insurance industry, some uninsured workers), victims exist (chronically ill, high-cost regions). The perspectival gap shows that simpler types (pure Rope or pure Snare) fail from specific vantage points. Insurance industry genuinely experiences Rope; chronically ill genuinely experience Snare; public health advocates genuinely experience Scaffold as sunset logic. No single type is 'the' answer—the constraint is a tangled hybrid revealing how policy mechanisms can simultaneously coordinate some populations and extract from others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_capture_by_premium_expansion,
    'Do tax credits expand genuine insurance access or primarily fund premium increases by insurers?',
    'Pre/post-credit analysis: correlation between credit implementation and average premiums in individual market; comparison of actual coverage uptake vs. model predictions assuming no premium adjustment',
    'If premiums rise to capture credits: mechanism is pure extraction (χ rises, classify as Snare from multiple perspectives). If premiums stable and uptake high: coordination logic holds and Tangled Rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidy_capture_by_premium_expansion, empirical, 'Whether tax credits fund genuine access or are captured by premium increases').

omega_variable(
    risk_pool_fragmentation_externality,
    'Do tax credits subsidizing individual market purchase fragment the insurance pool, raising costs for non-subsidized enrollees and creating negative externality?',
    'Risk profile analysis: health risk distribution in individual vs employer-sponsored markets; medical loss ratio trends post-credit; premium trajectory comparison between subsidized and non-subsidized populations',
    'If fragmentation occurs: hidden victim group emerges (non-subsidized individual market enrollees), extractiveness reassessed upward, institutional coordination failure more severe. If pools remain stable: subsidy redistributes rather than distorts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_pool_fragmentation_externality, empirical, 'Whether credits fragment insurance pools and create negative externalities').

omega_variable(
    employment_lock_vs_labor_mobility,
    'Do tax credits for individual purchase increase labor market mobility for workers trapped in employer coverage, or do they primarily serve as supplement for workers already outside employer plans?',
    'Longitudinal employment data: job switching rates, self-employment formation, part-time to full-time transitions before and after credit availability; exit-from-employer-coverage rates by credit eligibility',
    'If credits enable labor mobility: they unlock coordination benefit and suppress perceived extraction. If credits primarily serve non-employer populations: they coordinate access but don''t change fundamental employment-insurance linkage (Scaffold or Piton classification more robust).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_lock_vs_labor_mobility, empirical, 'Whether credits enable labor mobility or serve non-employer populations').

omega_variable(
    geographic_cost_variance_and_credit_adequacy,
    'Is credit magnitude calibrated to reflect geographic variation in insurance costs, or is a uniform federal credit inadequate in high-cost regions?',
    'Regional premium analysis: credit-to-minimum-premium ratio by metropolitan area and rural region; coverage gaps in high-cost markets; migration or insurance avoidance patterns correlated with credit inadequacy',
    'If uniform credit inadequate in high-cost regions: creates structural inequality (geographic victims emerge), extractiveness locally higher, Snare classification justified for high-cost populations. If regionally adjusted or adequate: Tangled Rope perspective more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_cost_variance_and_credit_adequacy, empirical, 'Whether credit magnitude reflects geographic cost variation').

omega_variable(
    moral_hazard_vs_actuarial_selection,
    'Does the credit mechanism create moral hazard (reduced preventive care due to cost-sharing) or actuarial selection (healthier individuals enroll due to affordability)?',
    'Enrollment demographic analysis: health profile of credit-subsidized enrollees vs baseline population; preventive care utilization rates; adverse selection indicators in subsequent years',
    'If moral hazard dominates: credits subsidize underutilization (Snare from public health perspective). If selection dominates: credits expand access for healthy uninsured (Rope from public health perspective). If balanced: Tangled Rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_vs_actuarial_selection, empirical, 'Whether credits create moral hazard or actuarial selection effects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2001_bush_health_insurance_tax_credits, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hc_tax_theater_t0, sotu_2001_bush_health_insurance_tax_credits, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hc_tax_theater_t5, sotu_2001_bush_health_insurance_tax_credits, theater_ratio, 5, 0.5).
narrative_ontology:measurement(hc_tax_theater_t10, sotu_2001_bush_health_insurance_tax_credits, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(hc_tax_extract_t0, sotu_2001_bush_health_insurance_tax_credits, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hc_tax_extract_t5, sotu_2001_bush_health_insurance_tax_credits, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(hc_tax_extract_t10, sotu_2001_bush_health_insurance_tax_credits, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2001_bush_health_insurance_tax_credits, resource_allocation).
narrative_ontology:affects_constraint(sotu_2001_bush_health_insurance_tax_credits, employer_sponsored_insurance_lock_in).
narrative_ontology:affects_constraint(sotu_2001_bush_health_insurance_tax_credits, healthcare_risk_pool_fragmentation).
narrative_ontology:affects_constraint(sotu_2001_bush_health_insurance_tax_credits, medicaid_coverage_gap).

% DUAL FORMULATION NOTE:
% Tax credits coordinate access (Rope logic) for some populations but extract from others (Snare logic). Decomposition into separate stories by risk profile would show: (a) healthy_uninsured_tax_credit_access (ε=0.35, Rope), (b) chronically_ill_tax_credit_exclusion (ε=0.75, Snare), (c) insurance_industry_subsidy_capture (ε=0.45, Tangled Rope). Single story maintains because they are inextricably linked—the subsidy extraction and access coordination are simultaneous properties of the same mechanism, not separable constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2001_bush_health_insurance_tax_credits, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
