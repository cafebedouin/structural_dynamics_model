% ============================================================================
% CONSTRAINT STORY: oncology_resource_allocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_oncology_resource_allocation, []).

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
 *   constraint_id: oncology_resource_allocation
 *   human_readable: Oncology Resource Allocation Constraint
 *   domain: healthcare/oncology/resource_distribution
 *
 * SUMMARY:
 *   Oncology resource allocation is a constraint that operationalizes the
 *   tension between market-driven healthcare (profit maximization) and equity
 *   imperatives (equal access). The constraint works through multiple
 *   interlocking mechanisms: pharmaceutical companies prioritize
 *   high-incidence, high-revenue cancers; insurance systems reimburse based
 *   on market prices rather than need; specialized cancer centers concentrate
 *   in urban, high-SES regions; clinical trials recruit from accessible
 *   populations; and patient support infrastructure follows investment
 *   capital. The extractiveness has increased over the measurement interval
 *   (0.35 → 0.58) as concentration in oncology care has grown and drug prices
 *   have risen despite efficacy plateaus. Theater ratio (0.55) reflects that
 *   insurance prior authorization, value-based care metrics, and equity
 *   initiatives are increasingly performative — policies announce equity
 *   while actual allocation remains market-driven. This constraint exhibits
 *   all major types from different structural perspectives: pure extraction
 *   (Snare) for uninsured and rural patients, mixed coordination-extraction
 *   (Tangled Rope) for insured patients with rare cancers and for community
 *   providers, pure coordination (Rope) for pharmaceutical manufacturers and
 *   major academic centers, degraded institutional ritual (Piton) for
 *   reimbursement systems, and emerging alternatives (Scaffold) in
 *   value-based care initiatives. The analytical observer risks naturalizing
 *   market-driven allocation as an immutable scarcity law, when it is
 *   actually a contingent policy choice.
 *
 * KEY AGENTS:
 *   - Powerless/Trapped Patients: Uninsured, rural, and low-income patients facing maximum resource barriers. Primary victims bearing full extraction cost.
 *   - Moderate/Constrained Patients: Insured patients with rare cancers or geographic constraints. Experience mixed coordination and extraction.
 *   - Pharmaceutical Manufacturers: Institutional/Arbitrage beneficiaries. Experience constraint as coordination that aligns incentives with R&D priorities.
 *   - Major Academic Medical Centers: Institutional/Arbitrage beneficiaries. Urban research hospitals concentrating resources and patient volume.
 *   - Community Hospitals: Powerful/Constrained actors experiencing extraction through resource concentration while coordinating referrals.
 *   - Patient Advocacy Organizations: Organized/Constrained agents mobilizing collective action to redistribute resources and challenge extraction mechanisms.
 *   - Insurance and CMS Systems: Institutional/Arbitrage actors maintaining performative equity controls while preserving market-driven allocation.
 *   - Value-Based Care Initiatives: Organized/Constrained scaffold agents building alternative payment and allocation models with sunset logic.
 *   - Analytical Observer: Risks naturalizing market structure as scarcity law rather than policy choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(oncology_resource_allocation, 0.58).
domain_priors:suppression_score(oncology_resource_allocation, 0.68).
domain_priors:theater_ratio(oncology_resource_allocation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(oncology_resource_allocation, extractiveness, 0.58).
narrative_ontology:constraint_metric(oncology_resource_allocation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(oncology_resource_allocation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(oncology_resource_allocation, tangled_rope).
narrative_ontology:human_readable(oncology_resource_allocation, "Oncology Resource Allocation Constraint").
narrative_ontology:topic_domain(oncology_resource_allocation, "healthcare/oncology/resource_distribution").

domain_priors:requires_active_enforcement(oncology_resource_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(oncology_resource_allocation, wealthy_patients).
narrative_ontology:constraint_beneficiary(oncology_resource_allocation, high_revenue_cancer_types).
narrative_ontology:constraint_beneficiary(oncology_resource_allocation, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(oncology_resource_allocation, institutional_hospitals).
narrative_ontology:constraint_victim(oncology_resource_allocation, low_income_patients).
narrative_ontology:constraint_victim(oncology_resource_allocation, rare_cancer_populations).
narrative_ontology:constraint_victim(oncology_resource_allocation, rural_access_communities).
narrative_ontology:constraint_victim(oncology_resource_allocation, healthcare_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED CANCER PATIENT (SNARE) — Trapped by economic barriers to treatment access. Cannot exit the constraint through insurance pathways, treatment delays, or geographic mobility. Experiences maximum extraction: resource allocation systematically excludes them from treatment options. No coordination benefit perceived — only exclusion and delayed care.
constraint_indexing:constraint_classification(oncology_resource_allocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RURAL CANCER PATIENT (SNARE) — Trapped by geographic isolation and absence of specialized oncology infrastructure in rural regions. Cannot access treatment facilities without relocation or extended travel. Bears full cost of resource allocation bias toward urban centers. No alternative pathways; suppression is structural and total.
constraint_indexing:constraint_classification(oncology_resource_allocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSURED PATIENT WITH RARE CANCER (TANGLED ROPE) — Partially benefits from insurance and healthcare access but faces constraints: drug development and clinical trial prioritization favor high-incidence cancers with larger markets. Coordination benefit exists (insurance system, basic infrastructure) but is substantially extracted away through underinvestment in rare cancer research and treatment protocols.
constraint_indexing:constraint_classification(oncology_resource_allocation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL MANUFACTURERS (ROPE) — Experience the constraint as pure coordination: oncology resource allocation ensures market incentives align with drug development priorities. They benefit from the system that channels R&D toward high-revenue cancer types. Can arbitrage globally — shift production, pricing, and market access based on profitability. Net beneficiary.
constraint_indexing:constraint_classification(oncology_resource_allocation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MAJOR ACADEMIC MEDICAL CENTERS (ROPE) — Urban research hospitals benefit from concentrated resources, patient volume for clinical trials, and funding concentration. Can arbitrage through patient selection (higher-SES populations, common cancers). Coordination is genuine: the system allocates resources to centers with capacity to utilize them. Network effects amplify this benefit.
constraint_indexing:constraint_classification(oncology_resource_allocation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: COMMUNITY HOSPITALS AND RURAL PROVIDERS (TANGLED ROPE) — Face dual pressure: resource allocation system extracts through concentration of funding in academic centers, yet they also coordinate patients toward specialized care. Constrained by referral requirements and limited capital for oncology infrastructure. Some coordination benefit (patient referral networks) alongside substantial extraction (resource deprivation).
constraint_indexing:constraint_classification(oncology_resource_allocation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: INSURANCE AND CMS REIMBURSEMENT SYSTEMS (PITON) — The resource allocation mechanism is substantially performative: coding systems, prior authorization theater, and 'coverage with evidence development' create appearance of equity control while actual allocation follows profit/volume incentives. Theater ratio elevated (0.55) as reimbursement policies perform equity while preserving market-driven resource concentration. The system is maintained through regulatory inertia despite recognized inequities.
constraint_indexing:constraint_classification(oncology_resource_allocation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: PATIENT ADVOCACY AND EQUITY ORGANIZATIONS (TANGLED ROPE) — Organized agents perceive genuine coordination function (resource standards, treatment protocols, patient support networks) but also identify extraction mechanisms: resource gaps by race, insurance status, and geography. Constrained by institutional dependencies but increasingly mobilizing collective action to redistribute resources and create parallel support pathways.
constraint_indexing:constraint_classification(oncology_resource_allocation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 9: VALUE-BASED CARE INITIATIVES (SCAFFOLD) — Emerging alternative models (bundled payments, capitation, value-based care networks) represent sunset mechanisms for fee-for-service extraction. Organized agents (health systems, payers) see restructuring of incentives as achievable within 10-15 years. Scaffolding has limited enforcement now but growing structural support. Theater is declining as accountability metrics replace prior authorization ritual.
constraint_indexing:constraint_classification(oncology_resource_allocation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, cancer treatment is inherently scarce: expertise, specialized facilities, and high-cost drugs are necessarily limited. Some resource inequality is inevitable — an immutable feature of medicine under scarcity. However, the base properties contradict mountain classification. The constraint is contingent on market structure, insurance design, and policy choices, not on scarcity itself. False summit: naturalizes institutional choices as laws of nature.
constraint_indexing:constraint_classification(oncology_resource_allocation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(oncology_resource_allocation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(oncology_resource_allocation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(oncology_resource_allocation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(oncology_resource_allocation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(oncology_resource_allocation, TR),
    TR >= 0.70.

:- end_tests(oncology_resource_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated and increasing. The constraint extracts through multiple mechanisms: (1) pharmaceutical price concentration in high-revenue cancer types (80% of oncology R&D for ~20% of cancers by prevalence), (2) insurance reimbursement following market prices rather than need or efficacy, (3) geographic concentration of specialized care, (4) clinical trial recruitment from accessible populations, (5) systemic delays in access for uninsured patients. The growth from 0.35 to 0.58 over 14 years reflects consolidation in oncology care (fewer, larger centers), increasing drug prices, and widening outcome disparities by SES and insurance status. Suppression (0.68): High and structural. Barriers include insurance eligibility, out-of-pocket costs, geographic distance to specialists, information asymmetries, and clinical trial eligibility criteria. Suppression operates at multiple levels: economic (cost barriers), informational (knowledge gaps), structural (facility availability), and regulatory (insurance prior authorization). Theater ratio (0.55): Moderate and increasing. Prior authorization processes, value-based care metrics, health equity initiatives, and diversity programs in oncology create performative appearance of equity while actual allocation remains driven by profit incentives and market concentration. The theater increased from 0.42 to 0.55 as regulatory and social pressure generated more policy theater without proportional structural change.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximal perspectival divergence. Powerless trapped agents (Snare) experience the constraint as pure coercion with no coordination benefit — delayed or denied treatment, no exit options. Beneficiary institutions (Rope) experience it as coordination that works: resources flow to them, incentives align, networks function. Moderate constrained agents (Tangled Rope) experience both: the system provides some benefit (insurance coverage, clinical trials) while systematically extracting away resources. Community providers experience extraction through concentration while maintaining coordination function (referral networks). Patient advocacy (organized Tangled Rope) sees the same structural extraction as powerless agents but with agency and collective power to challenge it. Reimbursement systems (Piton) see their own processes as degraded — prior authorization theater without matching outcomes. Value-based care (Scaffold) sees a structural pathway out: payment model reform with a 10-15 year sunset. The analytical observer (Mountain) risks naturalizing all of this as inevitable scarcity, misidentifying a policy structure as a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's structural position relative to the extraction flow. Uninsured/rural patients: d ≈ 0.95 (full victims, trapped exit) → high f(d) ≈ 1.42 → maximum experienced extraction. Insured rare-cancer patients: d ≈ 0.60 (partial victims, constrained exit) → moderate f(d) ≈ 0.90 → moderate extraction. Pharmaceutical manufacturers: d ≈ 0.05 (beneficiaries, arbitrage exit) → f(d) ≈ -0.12 → negative extraction (subsidized). Major academic centers: d ≈ 0.10 (beneficiaries, arbitrage exit) → f(d) ≈ -0.05 → minimal extraction. Community providers: d ≈ 0.65 (partial victims, constrained exit) → f(d) ≈ 1.00 → high extraction. Patient advocacy: d ≈ 0.55 (organized victims) → f(d) ≈ 0.75 → moderate extraction. Insurance systems: d ≈ 0.15 (beneficiaries, arbitrage exit) → f(d) ≈ -0.01 → minimal extraction. Scope modifier σ(national) = 1.0 applies uniformly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through explicit institutional causation. The constraint is NOT 'cancer treatment is inherently scarce' (which would be mountain). It is 'pharmaceutical and healthcare institutional structures create artificial scarcity and concentrate resources.' These are categorically different. The mountain perspective commits a category error: it naturalizes institutional choices (profit-driven R&D, market-based reimbursement, geographic consolidation) as inevitable limits of medical science. The actual structural data reveals that oncology resource allocation is a contingent policy choice: countries with different healthcare systems (NHS, universal single-payer, multi-payer with equity mandates) show different resource distributions, different extraction patterns, and different outcome equity. The constraint is Tangled Rope at the institutional level because it simultaneously (a) coordinates care delivery (genuine clinical networks, referral systems, treatment protocols) AND (b) extracts asymmetrically through market mechanisms. The Snare classification from powerless perspectives is accurate — they experience pure extraction with no perceived benefit. The Rope classification from beneficiary perspectives is accurate — they experience coordination with net benefit. The scaffold classification is real — value-based care and alternative payment models represent genuine structural alternatives that would shift the constraint's character if adopted. The mandatrophy is resolved by refusing the false mountain: this is a policy-embedded constraint, not a law of nature, and its classification depends on institutional structure, not on physical or logical limits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equity_metric_ambiguity,
    'What baseline defines equitable oncology resource allocation — equal access to all treatments, equal outcomes across groups, or proportional resource allocation by disease prevalence?',
    'International comparison of healthcare systems with different equity baselines; analysis of how metrics change when baseline shifts from access to outcomes to prevalence-adjusted distribution',
    'Different baselines yield different extraction measurements. Equal access baseline may show lower extraction than outcomes baseline. Constraint type may shift from Snare to Tangled Rope depending on which equity model is adopted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_metric_ambiguity, preference, 'Ambiguity in defining baseline for equitable resource allocation').

omega_variable(
    profit_driven_extraction_structural,
    'Is pharmaceutical market concentration (high-revenue cancers receive 80% of R&D) structural extraction or efficient market response to risk and prevalence?',
    'Comparative analysis of public-sector R&D allocation vs market allocation; measurement of therapeutic gaps between market priority and disease burden; evaluation of counter-factual: how would allocation differ under non-profit research paradigm',
    'If structural extraction: suppression metric is justified at 0.68; constraint is genuinely Snare for powerless agents. If efficient market response: suppression and extraction scores should be lower; constraint reclassifies toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(profit_driven_extraction_structural, empirical, 'Whether profit-driven R&D concentration is extraction or efficiency').

omega_variable(
    coordination_benefit_distribution,
    'Do powerless agents perceive any genuine coordination benefit from centralized oncology resource systems, or is the entire system experienced as pure extraction?',
    'Qualitative research with patients in low-resource settings; measurement of treatment access trajectories pre/post insurance enrollment; analysis of patient outcomes from decentralized vs centralized oncology networks',
    'If powerless agents perceive coordination benefit: constraint is Tangled Rope from all perspectives, suppression may be lower. If purely extraction: strengthens Snare classification; supports suppression ≥ 0.68.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_benefit_distribution, empirical, 'Whether marginalized populations experience coordination benefits from centralized systems').

omega_variable(
    alternative_supply_feasibility,
    'Can decentralized or community-based oncology capacity genuinely replace centralized high-cost infrastructure, or do high-incidence cancers inherently require specialized centers?',
    'Analysis of treatment outcomes by care setting; measurement of complication and mortality rates for distributed vs concentrated care; evaluation of training capacity for community oncologists',
    'If decentralized care is feasible: scaffold perspective is strengthened; exit options for trapped agents shift from ''trapped'' to ''constrained'' or ''mobile''. If impossible: mountain perspective gains credibility; extraction concentration becomes justified by necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_supply_feasibility, empirical, 'Feasibility of distributed oncology care as alternative to centralization').

omega_variable(
    insurance_reform_causal_pathway,
    'Would shifting from fee-for-service to capitated payment models actually reduce resource extraction to low-income populations, or would extraction simply take different forms (rationing, access restrictions)?',
    'Longitudinal analysis of health systems that have transitioned payment models; measurement of resource allocation equity before/after transitions; analysis of capitated systems showing persistent equity gaps',
    'If payment model change is sufficient: scaffold sunset logic is viable; constraint will naturally degrade as incentives align. If extraction persists: current institutional structure may not be the binding constraint; extraction rooted in deeper structural factors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_reform_causal_pathway, empirical, 'Whether payment model reform addresses root extraction mechanisms').

omega_variable(
    suppression_internalization_mechanism,
    'To what extent is the suppression (0.68) structural (external barriers: insurance, geography, facility access) vs internalized (patients believe treatment is unavailable, don''t seek care, accept delays as inevitable)?',
    'Pre/post intervention studies measuring care-seeking behavior when barriers are removed; qualitative analysis of internalized vs structural barriers in different populations; measurement of suppression persistence after structural interventions',
    'If predominantly structural: barriers can be removed; exit options shift from ''trapped'' toward ''constrained'' or ''mobile''. If predominantly internalized: suppression is deeper; constraint persists even when formal barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Extent of structural vs internalized suppression mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(oncology_resource_allocation, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(onc_res_tr_t0, oncology_resource_allocation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(onc_res_tr_t7, oncology_resource_allocation, theater_ratio, 7, 0.5).
narrative_ontology:measurement(onc_res_tr_t14, oncology_resource_allocation, theater_ratio, 14, 0.55).

% Extraction over time
narrative_ontology:measurement(onc_res_be_t0, oncology_resource_allocation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(onc_res_be_t7, oncology_resource_allocation, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(onc_res_be_t14, oncology_resource_allocation, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(oncology_resource_allocation, resource_allocation).
narrative_ontology:affects_constraint(oncology_resource_allocation, pharmaceutical_pricing_constraint).
narrative_ontology:affects_constraint(oncology_resource_allocation, clinical_trial_recruitment_bias).
narrative_ontology:affects_constraint(oncology_resource_allocation, healthcare_insurance_design).
narrative_ontology:affects_constraint(oncology_resource_allocation, cancer_research_prioritization).

% DUAL FORMULATION NOTE:
% Oncology resource allocation is upstream of specific treatment access constraints and downstream of healthcare financing structure. The extractiveness of resource allocation (0.58) depends on the broader insurance design and pharmaceutical pricing mechanisms that feed into it. Decomposition into separate constraint stories is appropriate for (a) pharmaceutical pricing as a distinct extractive mechanism with higher ε, (b) clinical trial recruitment bias as a distinct epistemic constraint, and (c) insurance design as the policy substrate enabling allocation extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(oncology_resource_allocation, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
