% ============================================================================
% CONSTRAINT STORY: sotu_1967_johnson_medicare_establishment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1967_johnson_medicare_establishment, []).

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
 *   constraint_id: sotu_1967_johnson_medicare_establishment
 *   human_readable: Medicare Establishment as Federal Healthcare Entitlement (1967)
 *   domain: healthcare/social_policy/political_economy
 *
 * SUMMARY:
 *   Medicare, established in 1965 and implemented in 1967, represents a
 *   structural shift in elderly healthcare provision from
 *   private/familial/charitable responsibility to federal entitlement. The
 *   constraint creates a permanent institutional obligation funded through
 *   mandatory payroll taxation (2.9% on earnings) and general revenue
 *   appropriations. Beneficiaries are Americans aged 65+ (approximately 68+
 *   million as of 2024); costs are distributed across working-age taxpayers
 *   and the federal budget. Structurally, Medicare abolished medical
 *   bankruptcy as a primary economic outcome for the elderly, converting
 *   catastrophic healthcare costs into a managed collective risk pool.
 *   However, the constraint simultaneously creates extraction mechanisms:
 *   moral hazard from unlimited reimbursement, hospital sector dependence on
 *   Medicare payment rates, federal budget pressure, and age-based wealth
 *   transfer from younger to older cohorts. The constraint exhibits all six
 *   DR types from different structural positions, making it a diagnostic
 *   exemplar for how a single large institutional arrangement can be
 *   simultaneously experienced as pure coordination (elderly beneficiary),
 *   mixed extraction-coordination (working-age taxpayer), cost-control
 *   constraint (hospital system), intergenerational commitment (federal
 *   government), systemic contradiction (healthcare system itself), and
 *   unsustainable scaffold (analytical observer). The extractiveness has
 *   increased from 0.28 (1967, when elderly poverty was the dominant problem)
 *   to 0.52 (2017, when healthcare cost inflation dominates), revealing how a
 *   coordination mechanism can accumulate extraction over time as its primary
 *   problem-solving function succeeds and secondary extraction mechanisms
 *   emerge.
 *
 * KEY AGENTS:
 *   - Elderly Americans 65+: Primary beneficiary (powerless/trapped at age 65+, but liberated from prior constraint of medical bankruptcy). Experiences constraint as Rope (pure coordination/liberation).
 *   - Working-age Taxpayers: Secondary victim (moderate/constrained by mandatory payroll taxation with no exit option). Experiences constraint as Tangled Rope (insurance premium + age-based extraction).
 *   - Hospital Systems and Medical Device Manufacturers: Apparent beneficiary, structural victim (powerful/arbitrage exit but locked into dependency on Medicare revenue). Experiences constraint as Snare (guaranteed revenue + price control).
 *   - Federal Government: Institutional actor (institutional/constrained by political commitment once entitlement established). Experiences constraint as Tangled Rope (coordination obligation + budget extraction).
 *   - Healthcare System (aggregate): Organized collective (organized/constrained). Experiences constraint as Tangled Rope (demand coordination + moral hazard inflation).
 *   - Analytical Observer: Systemic perspective (analytical/analytical). Sees constraint as Scaffold (temporary solution to mid-20th-century problem, approaching sunset as demographics shift).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1967_johnson_medicare_establishment, 0.52).
domain_priors:suppression_score(sotu_1967_johnson_medicare_establishment, 0.35).
domain_priors:theater_ratio(sotu_1967_johnson_medicare_establishment, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1967_johnson_medicare_establishment, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1967_johnson_medicare_establishment, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sotu_1967_johnson_medicare_establishment, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1967_johnson_medicare_establishment, tangled_rope).
narrative_ontology:human_readable(sotu_1967_johnson_medicare_establishment, "Medicare Establishment as Federal Healthcare Entitlement (1967)").
narrative_ontology:topic_domain(sotu_1967_johnson_medicare_establishment, "healthcare/social_policy/political_economy").

domain_priors:requires_active_enforcement(sotu_1967_johnson_medicare_establishment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1967_johnson_medicare_establishment, elderly_americans_65_plus).
narrative_ontology:constraint_beneficiary(sotu_1967_johnson_medicare_establishment, low_income_seniors).
narrative_ontology:constraint_beneficiary(sotu_1967_johnson_medicare_establishment, hospital_systems).
narrative_ontology:constraint_beneficiary(sotu_1967_johnson_medicare_establishment, medical_device_manufacturers).
narrative_ontology:constraint_victim(sotu_1967_johnson_medicare_establishment, working_age_taxpayers).
narrative_ontology:constraint_victim(sotu_1967_johnson_medicare_establishment, federal_budget_capacity).
narrative_ontology:constraint_victim(sotu_1967_johnson_medicare_establishment, healthcare_cost_control_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELDERLY BENEFICIARY (ROPE) — From the senior's structural position, Medicare is experienced as pure coordination: the constraint solves the catastrophic-cost problem through collective risk pooling. Beneficiaries are trapped in the system by age, but they see it as liberation from the prior constraint (uninsured medical bankruptcy). The extraction they perceive is minimal — they pay premiums and deductibles, but the constraint protects them from infinite medical debt. Coordination function is genuine and dominant.
constraint_indexing:constraint_classification(sotu_1967_johnson_medicare_establishment, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING-AGE TAXPAYER (TANGLED ROPE) — Constrained by mandatory payroll taxation (2.9% income-based, no cap as of 2020s) with no exit option and limited political voice. Experiences genuine extraction: a portion of wages flows to seniors' healthcare regardless of individual choice. However, also benefits from coordination: when they reach 65, the same mechanism protects them. The constraint is hybrid — mandatory collective insurance (coordination) with age-based wealth transfer (extraction from younger to older). Suppression is moderate: technically mobile to other countries, but exit costs are high (losing accumulated benefits, social ties).
constraint_indexing:constraint_classification(sotu_1967_johnson_medicare_establishment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HOSPITAL/PHARMACEUTICAL SECTOR (SNARE) — Appears as beneficiary but inverts to victim under behavioral scrutiny. Hospitals and device manufacturers initially benefit from guaranteed Medicare reimbursement and predictable demand. However, the constraint becomes snare-like when viewed from their incentive structure: Medicare payment rates are administratively set, not market-determined. Hospitals capture extraction from the constraint (guaranteed revenue) but are caught in the mechanism itself — they cannot refuse Medicare patients, cannot set prices freely, and face regulatory overhead. The sector develops organizational dependencies on Medicare revenue, making exit (shifting to all-private-pay) structurally infeasible. Effective extraction is high because the sector is locked into the system by prior investments in scale and capacity.
constraint_indexing:constraint_classification(sotu_1967_johnson_medicare_establishment, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL GOVERNMENT (TANGLED ROPE) — Institutionally constrained by political commitment to the entitlement once established. Genuine coordination function: pooling elderly healthcare costs prevents catastrophic family bankruptcies and reduces demand for state/local charity care. But also genuine extraction: program costs grow with population aging, medical inflation, and political inability to cut benefits or enforce cost controls. The constraint enforces coordination (universal coverage, prevention of medical poverty) while simultaneously extracting from the federal budget with declining elasticity. Institutional exit is politically impossible — reducing Medicare is a third-rail issue — making the government's exit_options effectively constrained.
constraint_indexing:constraint_classification(sotu_1967_johnson_medicare_establishment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HEALTHCARE SYSTEM AS ORGANIZED AGENT (TANGLED ROPE) — From the perspective of the healthcare system itself (as a complex adaptive entity), Medicare creates both coordination and extraction. It coordinates demand (seniors access care) and supply (hospitals receive payment), but also extracts by creating moral hazard: unlimited reimbursement for procedures reduces cost sensitivity for both providers and patients. This generates the inflation spiral — higher costs trigger higher extraction. The system is constrained because Medicare funding is insufficient to cover demand (deductibles, premiums rise) but sufficient to lock in institutional dependencies (hospitals, providers, pharma depend on Medicare scale). The constraint simultaneously solves the access problem and creates the cost problem.
constraint_indexing:constraint_classification(sotu_1967_johnson_medicare_establishment, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SCAFFOLD) — From a long-term structural perspective, Medicare appears as a temporary institutional solution to a mid-20th-century coordination problem (catastrophic elderly healthcare costs in a pre-welfare society). The constraint exhibits scaffold properties: it solved the acute problem (medical poverty for seniors) but contains the seeds of its own unsustainability (cost inflation, population aging, budget constraints). The sunset is implicit in the demographics: as life expectancy increases and the ratio of retirees to workers rises, the payroll-tax-based funding model faces exponential pressure. The scaffold's function is gradually transferring from solving access to managing cost, a phase transition that looks like structural failure but is actually the constraint reaching its sunset. Theater_ratio is moderate (0.42) because while Medicare administrative burden exists, the core coordination function (universal coverage) remains genuine.
constraint_indexing:constraint_classification(sotu_1967_johnson_medicare_establishment, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1967_johnson_medicare_establishment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1967_johnson_medicare_establishment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1967_johnson_medicare_establishment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1967_johnson_medicare_establishment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_1967_johnson_medicare_establishment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint has accumulated extraction over 50+ years. Initial extractiveness was lower (0.28 in 1967) because the dominant problem was elderly medical poverty and access — Medicare's primary function was genuinely solving an acute coordination failure. As elderly healthcare access improved and medical technology expanded, the constraint accumulated secondary extraction mechanisms: unlimited fee-for-service reimbursement incentivizes procedure inflation; hospital dependence on Medicare creates monopoly-like pricing power; aging population increases per-capita costs beyond inflation; younger cohorts' payroll tax burden rises exponentially as the ratio of retirees to workers increases. The current 0.52 value reflects that extraction (cost inflation, intergenerational transfer, federal budget pressure) is now comparable to coordination benefit (universal coverage). Suppression (0.35): Moderate. Working-age taxpayers face suppression through mandatory payroll taxation with no exit option and limited political voice (voting against Medicare is electorally impossible). Elderly beneficiaries face suppression in the form of deductibles, copayments, and coverage gaps. However, suppression is not total — alternative insurance (Medigap, Medicare Advantage) provides some exit pathways, and the constraint is politically transparent (not hidden). Theater ratio (0.42): Moderate-low, increasing slightly over time. Medicare administrative requirements (prior authorization, billing codes, appeals processes) constitute theater, but the core coordination function (universal coverage, pooled risk) remains substantive. The constraint's theater has increased from 0.25 (1967, when administrative simplicity was higher) to 0.42 (2017, as compliance complexity has grown), but theater has not dominated function yet (would require >0.70 for Piton classification). This trajectory suggests the constraint is approaching the phase transition from Tangled Rope to Scaffold as cost controls and coverage restrictions tighten.
 *
 * PERSPECTIVAL GAP:
 *   All six perspectives produce different classifications despite identical base metrics. This is NOT a measurement error but a demonstration of how indexical position determines experienced constraint type. The elderly see Rope (pure coordination) because they are structurally mobile (exit option: become uninsured, move to private insurance) but experience the constraint as liberatory. The working-age see Tangled Rope because they experience both coordination (future protection) and extraction (current mandatory cost). The hospital system sees Snare because while beneficiary on paper, they are functionally locked into the constraint. The federal government sees Tangled Rope because institutional commitment creates constrained exit despite policy agency. The healthcare system sees Tangled Rope because demand coordination creates inflation extraction. The analytical observer sees Scaffold because the constraint's sunset condition (demographic unsustainability) is structurally embedded. These are not disagreements about facts — they are structural disagreements about which effects dominate from which position. The perspectival gap is the constraint's true signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from agent power, exit options, and beneficiary/victim declarations. Elderly beneficiaries have low d (0.15–0.25) because they benefit and are trapped, making them net receivers of value extraction in their direction (negative effective extraction for them). Working-age taxpayers have moderate-high d (0.60–0.75) because they bear costs (mandatory taxation) with constrained exit, making them targets of extraction. Hospital systems have ambiguous d (0.30–0.50) because they are formal beneficiaries (guaranteed revenue) but structural victims (locked into dependency and price control). The federal government has high d (0.70–0.85) because it bears the budget extraction while unable to exit (politically constrained). The analytical observer has moderate d (0.70) derived from standard canonical fallback for analytical position. These directionality values drive the chi calculations: elderly beneficiaries experience low/negative chi (they are net beneficiaries); working-age taxpayers experience high chi (they bear extraction); hospital systems experience moderate chi (mixed benefit and constraint); federal government experiences high chi (budget extraction). The perspectival gap in chi is large because the indexical tuple (P, T, E, S) varies dramatically across perspectives — power ranges from powerless to institutional, exit ranges from trapped to arbitrage, and this variation drives both the classification differences and the directionality differences.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing coordination function (genuine: universal elderly healthcare access) from extraction function (accumulated: cost inflation, intergenerational transfer, federal budget pressure). The constraint is legitimately Tangled Rope — it solves a real coordination problem while simultaneously extracting. The mandatrophy is resolved by the structural data: beneficiaries exist (elderly Americans, working-age future beneficiaries), victims exist (working-age taxpayers, federal budget, healthcare cost control), and active enforcement exists (mandatory payroll taxation, federal administration). The apparent contradiction between 'elderly see Rope' and 'analytical observer sees Scaffold' is not contradiction but structural fact — the constraint simultaneously IS a coordination mechanism AND is approaching unsustainability. The six perspectives are not competing hypotheses but different cuts through a complex institutional reality. The mandatrophy failure would be to claim the constraint is ONLY extraction (missing the genuine coordination) or ONLY coordination (missing the accumulated extraction). The DR framework resolves this by making both true simultaneously through perspectival indexing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_insurance_premium_ambiguity,
    'Is Medicare''s extraction from working-age taxpayers genuine redistribution, or is it a legitimate insurance premium that they will collect in old age, making the net extraction zero across the lifespan?',
    'Longitudinal lifetime fiscal analysis: track cohorts from working age through retirement; calculate whether average payroll tax paid equals average benefits received. Intergenerational accounting: assess whether younger cohorts are subsidizing older cohorts (extraction) or paying actuarially fair premiums (insurance).',
    'If net-zero across lifespan: constraint reclassifies as Rope (pure coordination) from working-age perspective. If younger cohorts systematically underfunded: constraint remains Tangled Rope (age-based extraction layered on coordination). This determines whether the constraint is an intergenerational Ponzi or a legitimate insurance mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_insurance_premium_ambiguity, empirical, 'Lifetime fiscal impact of Medicare taxation: insurance premium vs. redistribution').

omega_variable(
    hospital_sector_extraction_direction,
    'Do hospitals experience Medicare as a beneficial guaranteed-revenue mechanism or as a constraining price-control mechanism that extracts surplus they would capture in a free market?',
    'Comparative analysis of hospital profitability and reinvestment in Medicare-heavy vs. private-pay-heavy markets. Historical pricing data: charting hospital price trajectories before and after Medicare introduction. Sector capital allocation: whether hospitals withdraw from areas with high Medicare concentration.',
    'If hospitals benefit net: reclassify hospital perspective from Snare to Rope or Tangled Rope (genuine mixed benefit). If hospitals experience extraction: classification stands. This determines whether the constraint extracts from medical providers or merely coordinates them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hospital_sector_extraction_direction, empirical, 'Net directional impact of Medicare pricing on hospital sector profitability').

omega_variable(
    cost_inflation_driver_attribution,
    'To what degree is healthcare cost inflation in the Medicare era attributable to: (a) moral hazard from unlimited reimbursement; (b) technological advancement increasing treatment costs; (c) aging population requiring more intensive care; (d) market power of pharmaceutical and device manufacturers?',
    'Decomposition analysis isolating each factor''s contribution via regression, international comparison (countries with different cost-control mechanisms), and historical counterfactuals (what would per-capita costs be without Medicare''s moral hazard effect?).',
    'If moral hazard dominates (>50%): suppression classification rises (the constraint is sustaining inefficient behavior). If technology/aging dominates: suppression classification holds (cost growth is inevitable). If pharma/device sector dominates: reclassify hospital/pharma perspective as Snare (they are extracting from the system). Attribution determines whether extraction is endogenous to the constraint or exogenous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_inflation_driver_attribution, empirical, 'Cost inflation attribution in Medicare era').

omega_variable(
    political_exit_feasibility,
    'Under what political and demographic conditions could Medicare be structurally dismantled or fundamentally restructured, and is the observed political immobility a structural feature or a contingent political equilibrium?',
    'Political economy analysis of coalition durability: seniors as voting bloc, beneficiary industries as lobbying power, rhetorical entrenchment (''earned benefit'' vs. ''entitlement''). Demographic stress testing: as healthcare costs reach crisis thresholds, does political coalescence for fundamental restructuring become possible?',
    'If dismantling is structurally infeasible: government''s exit_options remain ''constrained'' and perspective remains Tangled Rope. If restructuring becomes politically feasible under stress: exit_options shift to ''mobile'' and perspective shifts to Rope or Scaffold (institutional agency emerges). This determines whether the constraint''s persistence is structural or contingent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_exit_feasibility, preference, 'Political structural limits to Medicare restructuring or dissolution').

omega_variable(
    coordination_function_genuine_vs_rhetorical,
    'Is the coordination function of Medicare (universal elderly healthcare access) structural and irreplaceable, or could alternative mechanisms (private insurance with subsidies, negative income tax, etc.) solve the same coordination problem with lower extraction?',
    'Comparative institutional analysis: countries with different elderly healthcare models and their outcomes on access, cost, equity. Counterfactual historical analysis: what would have been the outcome of targeted subsidies vs. universal Medicare?',
    'If coordination is genuinely irreplaceable: Tangled Rope classification stands (coordination function is real, extraction is endemic cost). If alternatives exist with lower extraction: reclassify as Snare (the constraint is chosen form of extraction, not inevitable coordination mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_genuine_vs_rhetorical, conceptual, 'Whether Medicare''s coordination function is unique or replaceable by lower-extraction alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1967_johnson_medicare_establishment, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1967, sotu_1967_johnson_medicare_establishment, theater_ratio, 0, 0.25).
narrative_ontology:measurement(theater_1992, sotu_1967_johnson_medicare_establishment, theater_ratio, 25, 0.38).
narrative_ontology:measurement(theater_2017, sotu_1967_johnson_medicare_establishment, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(extractiveness_1967, sotu_1967_johnson_medicare_establishment, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(extractiveness_1992, sotu_1967_johnson_medicare_establishment, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(extractiveness_2017, sotu_1967_johnson_medicare_establishment, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1967_johnson_medicare_establishment, resource_allocation).
narrative_ontology:affects_constraint(sotu_1967_johnson_medicare_establishment, medicaid_expansion_1967).
narrative_ontology:affects_constraint(sotu_1967_johnson_medicare_establishment, healthcare_cost_inflation_mechanism).
narrative_ontology:affects_constraint(sotu_1967_johnson_medicare_establishment, pharmaceutical_pricing_power).
narrative_ontology:affects_constraint(sotu_1967_johnson_medicare_establishment, hospital_sector_consolidation).
narrative_ontology:affects_constraint(sotu_1967_johnson_medicare_establishment, payroll_tax_intergenerational_transfer).

% DUAL FORMULATION NOTE:
% Medicare can be decomposed into multiple distinct constraints with different ε values: (1) elderly_healthcare_access (ε=0.15, Rope) — pure coordination of access; (2) healthcare_cost_inflation (ε=0.62, Snare) — unlimited reimbursement creates moral hazard; (3) payroll_tax_system (ε=0.48, Tangled Rope) — mandatory wealth transfer with coordination function; (4) federal_budget_entitlement (ε=0.55, Tangled Rope) — institutional commitment to unsustainable growth. This story treats Medicare as a single macro constraint with ε=0.52, balancing these sub-constraints. The decomposition is available for finer-grained analysis but requires separate constraint files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1967_johnson_medicare_establishment, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
