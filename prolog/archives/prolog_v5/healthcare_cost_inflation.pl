% ============================================================================
% CONSTRAINT STORY: healthcare_cost_inflation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_healthcare_cost_inflation, []).

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
 *   constraint_id: healthcare_cost_inflation
 *   human_readable: Healthcare Cost Inflation and Rent Extraction Through Pricing Opacity
 *   domain: healthcare/economics/regulatory
 *
 * SUMMARY:
 *   Healthcare cost inflation in the United States represents a structural
 *   constraint where genuine coordination (pooling risk, financing
 *   innovation, centralizing expertise) is tightly coupled with systematic
 *   rent extraction through pricing opacity, market consolidation, and
 *   cost-shifting. Between 1995 and 2015, US healthcare spending as a
 *   percentage of GDP rose from 13% to 17%, while comparable OECD nations
 *   maintained 9-12% with equivalent or superior health outcomes. This gap
 *   reflects not faster disease progression or aging in the US but structural
 *   institutional choices: physician consolidation into large health systems,
 *   limited price regulation, opaque pricing mechanisms, aggressive patenting
 *   and brand-name drug protection, and administrative overhead that exceeds
 *   comparable nations by 10-15 percentage points. The constraint exhibits
 *   all six DR types from different vantage points. For the uninsured, it
 *   appears as pure extraction (Snare) with no coordination benefit and no
 *   exit. For the insured via employer, it coordinates care while extracting
 *   through cost-shifting and wage suppression (Tangled Rope). For hospital
 *   systems and pharmaceutical manufacturers, it appears as beneficial
 *   coordination with extractive leverage (Rope). For public authorities, it
 *   is a mixed coordination-extraction hybrid with constrained policy options
 *   (Tangled Rope). For insurance corporations, complex pricing and prior
 *   authorization systems provide administrative theater (Piton). For the
 *   analytical observer at civilizational scale, there is a risk of
 *   naturalizing the arrangement as an inherent consequence of medical
 *   complexity rather than recognizing it as contingent institutional design.
 *
 * KEY AGENTS:
 *   - Uninsured Patients: Primary victim (powerless/trapped) — face full sticker pricing with no negotiation power, no insurance subsidy, no exit option except medical debt and bankruptcy
 *   - Underinsured Patients: Secondary victim (moderate/constrained) — have insurance but high deductibles, coinsurance, and out-of-network costs; constrained by health status and employment lock
 *   - Hospital Systems: Primary beneficiary (institutional/arbitrage) — coordinate care while leveraging market consolidation and pricing opacity to extract rents; can shift costs between payer types and services
 *   - Pharmaceutical Manufacturers: Primary beneficiary (institutional/arbitrage) — benefit from patent protection and pricing opacity; can charge monopoly prices for drugs with no close substitutes; arbitrage across international markets
 *   - Insurance Corporations: Mixed actor (institutional/arbitrage) — theoretically coordinate risk pooling but extract through administrative complexity, prior authorization theater, and margin extraction; pass costs to employers and employees
 *   - Medicare/CMS (Public Health Authority): Constrained actor (organized/constrained) — coordinate access for elderly and disabled but politically unable to implement unilateral cost controls; captured by provider lobbying; exit constrained by democratic accountability
 *   - Employers Providing Insurance: Cost-bearer (organized/constrained) — coordinate employee access to care but face rising premiums that suppress wage growth; exit constrained by employee expectations and talent competition
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent to medical complexity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(healthcare_cost_inflation, 0.58).
domain_priors:suppression_score(healthcare_cost_inflation, 0.65).
domain_priors:theater_ratio(healthcare_cost_inflation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(healthcare_cost_inflation, extractiveness, 0.58).
narrative_ontology:constraint_metric(healthcare_cost_inflation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(healthcare_cost_inflation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(healthcare_cost_inflation, tangled_rope).
narrative_ontology:human_readable(healthcare_cost_inflation, "Healthcare Cost Inflation and Rent Extraction Through Pricing Opacity").
narrative_ontology:topic_domain(healthcare_cost_inflation, "healthcare/economics/regulatory").

domain_priors:requires_active_enforcement(healthcare_cost_inflation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(healthcare_cost_inflation, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(healthcare_cost_inflation, hospital_systems).
narrative_ontology:constraint_beneficiary(healthcare_cost_inflation, medical_device_manufacturers).
narrative_ontology:constraint_beneficiary(healthcare_cost_inflation, insurance_corporations).
narrative_ontology:constraint_victim(healthcare_cost_inflation, uninsured_patients).
narrative_ontology:constraint_victim(healthcare_cost_inflation, underinsured_patients).
narrative_ontology:constraint_victim(healthcare_cost_inflation, healthcare_system_efficiency).
narrative_ontology:constraint_victim(healthcare_cost_inflation, public_fiscal_health).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED PATIENT (SNARE) — Faces full sticker price with no negotiating power, no exit option, no subsidy. Trapped by medical necessity and financial barriers. Experiences maximum extraction: hospital bills become debt, debt becomes medical bankruptcy. The constraint is not coordinating their care — it is extracting from their future earnings and credit. Suppression is structural: emergency services cannot be refused; bankruptcy law is designed to protect creditors, not medical debtors.
constraint_indexing:constraint_classification(healthcare_cost_inflation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSURED PATIENT VIA EMPLOYER (TANGLED ROPE) — Genuinely benefits from insurance coordination: risk pooling, negotiated rates, access to care. But also bears costs: rising premiums, higher deductibles, coinsurance, non-coverage for specific treatments. The system coordinates care while extracting rents through cost-shifting to workers (lower wages to pay premiums) and cost-avoidance (denial of expensive treatments). Exit is constrained by health status and employment lock: changing jobs risks losing coverage continuity, especially pre-existing conditions (though ACA reduced this barrier).
constraint_indexing:constraint_classification(healthcare_cost_inflation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HOSPITAL SYSTEM (ROPE) — Primary beneficiary. The constraint coordinates: centralized treatment facilities, standardized protocols, pooled expertise. But hospital systems also benefit from pricing opacity, market consolidation, and cost-shifting from uninsured to insured patients. Arbitrage exit: hospitals can shift revenue sources, cross-subsidize between profitable and unprofitable services, leverage their market power. The rope classification reflects that hospitals experience the constraint as beneficial coordination with extractive overlay.
constraint_indexing:constraint_classification(healthcare_cost_inflation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL MANUFACTURER (ROPE) — Benefits from patent protection, pricing opacity, and limited price regulation in the US market. Can arbitrage: charge different prices in different countries, restrict access to establish scarcity, lobby for regulatory barriers. Experiences the constraint as beneficial coordination (ensuring R&D funding) with extractive leverage (setting monopoly prices). Exit is arbitrage: can shift manufacturing, licensing, or market focus.
constraint_indexing:constraint_classification(healthcare_cost_inflation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH AUTHORITY (TANGLED ROPE) — Coordination function: CMS and Medicare coordinate access to care for elderly and disabled populations, redistribute risk across large pools. But also captured by provider and pharmaceutical lobbying: price caps are politically difficult to enact, cost-control mechanisms are weakened through regulatory exemptions and exceptions. Exit is constrained by democratic accountability and political economy: cannot simply refuse to pay for expensive treatments without political backlash, cannot unilaterally cap prices without industry legal challenge. Moderate extraction: system functions but costs grow faster than revenue.
constraint_indexing:constraint_classification(healthcare_cost_inflation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INSURANCE CORPORATION PRICING MODELS (PITON) — High theater ratio. Insurance underwriting and actuarial models appear to serve coordination (risk assessment, premium calculation) but have largely degraded into performative complexity. Insurance corporations engage in extensive cost containment theater: prior authorization, utilization review, formulary management. These processes consume resources but serve primarily to delay claims and exclude expensive treatments rather than to identify genuinely unnecessary care. The theater persists because it generates revenue (through administrative margin) and shifts costs off-books through bureaucratic denial rather than explicit refusal. The underlying pricing coordination function (matching premiums to risk) is obscured by complexity.
constraint_indexing:constraint_classification(healthcare_cost_inflation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, healthcare cost inflation appears immutable: aging population, advanced technology, chronic disease prevalence, and the nature of medical knowledge production all create inherent pressure for cost growth. This perspective risks naturalizing what is actually a contingent institutional arrangement. The extractive overlay (pricing opacity, monopoly pricing, cost-shifting) is presented as inseparable from the coordination function (ensuring access, funding innovation, supporting infrastructure). However, international comparisons reveal that other developed nations achieve comparable or superior health outcomes at 40-50% of US costs, indicating that cost inflation is not a natural law but a product of specific policy and institutional choices.
constraint_indexing:constraint_classification(healthcare_cost_inflation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(healthcare_cost_inflation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(healthcare_cost_inflation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(healthcare_cost_inflation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(healthcare_cost_inflation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(healthcare_cost_inflation, TR),
    TR >= 0.70.

:- end_tests(healthcare_cost_inflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the genuine coordination function (risk pooling, research financing, centralized expertise) alongside systematic rent extraction through multiple mechanisms: pricing opacity (no transparent price discovery), market consolidation (hospitals and insurers operate as regional monopolies), cost-shifting (uninsured subsidize insured through higher charge-master prices), and administrative extraction (insurance overhead that exceeds international norms). The value increased from 0.32 (1995) to 0.58 (2015) as consolidation deepened and price opacity hardened. Suppression (0.65): Moderate-high. Medical necessity creates inelastic demand; bankruptcy law protects creditors more than debtors; prior authorization creates bureaucratic barriers to expensive treatments; price opacity prevents informed consumer choice; employer-based insurance creates lock-in effects; pharmaceutical patents prevent generic competition. Suppression is not absolute (public insurance exists, some transparency is emerging, generic competition functions in transparent markets), but barriers are substantial. Theater ratio (0.58): Moderate. Prior authorization and utilization review consume resources but serve primarily to delay claims and shift costs through bureaucratic denial rather than to identify genuinely unnecessary care. Administrative overhead (now 25% of spending) vastly exceeds levels in countries with centralized negotiation. However, some administrative complexity is genuine (coordinating millions of beneficiaries, processing claims, managing drug formularies). The theater has grown as pricing has become less transparent — more complexity is required to obscure pricing extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint splits into opposing experienced realities. The uninsured person sees a Snare: pricing extraction with no care coordination. The insured employee sees Tangled Rope: coordination benefits mixed with cost-shifting. The hospital sees Rope: coordination of care with legitimate margin. The pharmaceutical company sees Rope: coordination of R&D funding with justified pricing. The public health authority sees Tangled Rope with capture: coordination mandate constrained by political economy. The insurance corporation sees Piton: performative complexity. The civilization-scale observer risks Mountain: 'this is just how complex systems work.' These are not measurement disputes but genuine structural differences in what the constraint is *for* from each position.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation is transparent in the base_properties declarations. Uninsured patients are declared as victims — they bear the costs of pricing extraction with no offsetting benefit. Their exit options are trapped: medical necessity forces engagement; financial barriers prevent exit; no alternative system exists. The engine derives d from victim status + trapped exit, producing maximum f(d). Hospital systems are declared as beneficiaries — they receive financial benefit from the pricing and consolidation mechanisms. Their exit options are arbitrage: they can shift service mix, consolidate or divest, change pricing strategy. The engine derives d from beneficiary status + arbitrage exit, producing low or negative f(d), which means negative effective extraction (they are not being extracted from; they are extracting). The insured patient occupies an intermediate position: partially benefits from coordination, partially bears cost through premiums and deductibles. Constrained exit (health status, employment lock) produces moderate d, moderate f(d). Pharmaceutical manufacturers are beneficiaries with arbitrage exit (shift markets, licensing, focus), producing low d. Public health authorities are constrained (political accountability) despite organizational power, producing moderate d. The pattern shows that beneficiary status and exit options together fully determine experienced extraction — this is the engine's directionality calculation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    administrative_overhead_vs_necessary_coordination_cost,
    'What proportion of healthcare administrative overhead (currently ~25% of total spending) is necessary coordination cost vs. rent extraction through complexity?',
    'International comparative analysis of administrative spending and health outcomes across OECD nations; cost accounting by function (genuinely necessary processing vs. cost containment theater); time-motion studies of prior authorization and appeals processes',
    'If >60% of overhead is rent extraction: constraint should be reclassified upward in extractiveness, confirming Snare perspective for powerless agents. If <40% extraction: constraint is more purely coordination-driven (stronger Rope/Tangled Rope readings).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(administrative_overhead_vs_necessary_coordination_cost, empirical, 'What proportion of administrative overhead is rent extraction vs coordination cost').

omega_variable(
    price_sensitivity_and_true_demand_elasticity,
    'To what degree does healthcare price inflation reflect genuine supply scarcity vs. monopolistic pricing in a market with inelastic demand (people cannot choose not to get sick)?',
    'Cross-country price and quantity analysis; investigation of price variations for identical procedures within US markets; measurement of demand elasticity when prices are transparent vs. opaque; analysis of competition effects in markets with genuine price transparency',
    'If demand is truly inelastic: pricing power is pure extraction, independent of supply constraints. Snare classification is correct. If demand shows elasticity when prices are transparent: some price inflation reflects coordination on value rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_sensitivity_and_true_demand_elasticity, empirical, 'Whether price inflation reflects scarcity or monopolistic pricing in inelastic demand').

omega_variable(
    innovation_funding_attribution,
    'How much of pharmaceutical R&D funding actually comes from monopoly pricing rents vs. public funding (NIH grants, university research, international public health institutions) and venture capital?',
    'Detailed accounting of R&D funding sources for major therapeutic breakthroughs; attribution of public sector contribution to drug development; comparison of innovation rates in countries with price regulation vs. price freedom',
    'If public/VC funding > 50% of real innovation: monopoly pricing is not necessary for innovation, Snare classification is strengthened. If monopoly pricing is genuinely necessary: coordination function becomes more defensible, Tangled Rope reading gains strength.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(innovation_funding_attribution, empirical, 'Attribution of pharmaceutical innovation funding sources').

omega_variable(
    cost_shifting_measurement,
    'What is the actual magnitude of cost-shifting from uninsured/underinsured to insured patients, and what fraction of insured premium increases is attributable to this mechanism vs. genuine cost growth?',
    'Hospital cost accounting by payer type; measurement of charge-master price variations by insurance status; analysis of bad debt and charity care offsetting; premium trend decomposition',
    'If cost-shifting accounts for >30% of premium growth: the beneficiary group (insured) is partially funding extraction from the victim group (uninsured), confirming Tangled Rope with asymmetric extraction. If <15%: premium growth reflects genuine cost inflation rather than redistribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_shifting_measurement, empirical, 'Magnitude of cost-shifting from uninsured to insured populations').

omega_variable(
    sunset_pathway_feasibility,
    'Is there a credible policy pathway (regulatory reform, price negotiation expansion, transparency mandates) that could reduce extractiveness while maintaining coordination function? What is the timeline?',
    'Policy analysis of existing price regulation models (Medicare negotiation expansion, drug-price negotiation authority, price transparency mandates); measurement of outcomes in jurisdictions with stronger regulation; stakeholder assessment of political feasibility',
    'If credible sunset pathway exists with <15 year timeline: Scaffold reclassification is warranted. If no pathway or timeline >25 years: constraint is structural Snare/Tangled Rope with no managed exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_pathway_feasibility, empirical, 'Feasibility of policy sunset pathway for rent extraction component').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(healthcare_cost_inflation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hci_tr_t0, healthcare_cost_inflation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hci_tr_t10, healthcare_cost_inflation, theater_ratio, 10, 0.48).
narrative_ontology:measurement(hci_tr_t20, healthcare_cost_inflation, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(hci_be_t0, healthcare_cost_inflation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(hci_be_t10, healthcare_cost_inflation, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(hci_be_t20, healthcare_cost_inflation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(healthcare_cost_inflation, resource_allocation).
narrative_ontology:boltzmann_floor_override(healthcare_cost_inflation, 0.2).
narrative_ontology:affects_constraint(healthcare_cost_inflation, pharmaceutical_patent_system).
narrative_ontology:affects_constraint(healthcare_cost_inflation, employer_based_insurance_lock).
narrative_ontology:affects_constraint(healthcare_cost_inflation, hospital_market_consolidation).
narrative_ontology:affects_constraint(healthcare_cost_inflation, medical_bankruptcy_spiral).

% DUAL FORMULATION NOTE:
% Healthcare cost inflation decomposes into multiple structurally distinct constraints: pharmaceutical pricing (ε≈0.60, Snare), hospital consolidation (ε≈0.55, Tangled Rope), insurance administrative overhead (ε≈0.50, Piton), and medical bankruptcy (ε≈0.75, Snare). This story treats the system-level coordination-and-extraction hybrid; each component has its own story with different ε, beneficiaries, victims, and exit options. The network links show how failures in one component cascade to others: pharmaceutical monopoly pricing drives insurance premiums; high premiums increase the uninsured population; uninsured patients face cost-shifting. All are downstream of the fundamental opacity in pricing negotiation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(healthcare_cost_inflation, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
