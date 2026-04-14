% ============================================================================
% CONSTRAINT STORY: plastic_asphalt_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plastic_asphalt_mandate, []).

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
 *   constraint_id: plastic_asphalt_mandate
 *   human_readable: Government Mandate for Plastic-Infused Asphalt
 *   domain: economic/environmental/political
 *
 * SUMMARY:
 *   The plastic-infused asphalt mandate is a government-imposed requirement
 *   that all new road construction and significant repairs incorporate
 *   recycled plastic at a mandated percentage (typically 5-15% by weight).
 *   The constraint exhibits the hallmark tension between claimed
 *   environmental coordination and actual extraction mechanisms. The
 *   mandate's stated purpose is to address dual crises: plastic waste
 *   accumulation in landfills and deteriorating road infrastructure funding.
 *   However, the structural analysis reveals a complex hybrid: genuine
 *   coordination problem (waste plastic lacks market demand) combined with
 *   asymmetric benefit distribution (plastic processors gain guaranteed
 *   market, municipalities bear cost premium, road users bear
 *   durability/maintenance risk). The extractiveness value (0.58) reflects
 *   that the mandate produces measurable transfers of value to beneficiary
 *   groups without proportional reciprocal benefit to victims. The
 *   suppression value (0.62) reflects legal coercion: municipalities cannot
 *   opt out without regulatory sanctions, and traditional asphalt suppliers
 *   cannot refuse to adapt. The theater ratio (0.68) reflects that compliance
 *   verification (testing that asphalt contains plastic, that plastic is
 *   'recycled,' that environmental claims hold) is substantially performative
 *   — actual performance data on durability and lifecycle costs are
 *   contested.
 *
 * KEY AGENTS:
 *   - Plastic Waste Processors: Primary beneficiary (institutional/arbitrage) — mandate guarantees market for previously low-value waste streams; can easily exit to alternative customers
 *   - Budget-Constrained Municipalities: Primary victim (powerless/trapped) — bear cost premium (15-25% markup) without proportional benefit; legally cannot refuse mandate
 *   - Traditional Asphalt Suppliers: Secondary victim (moderate/constrained) — face forced market restructuring; exit requires capital-intensive pivot to plastic-processing capability
 *   - Large Construction Contractors: Mixed position (powerful/mobile) — can comply but absorb supply-chain complexity; benefit from environmental branding but face price volatility
 *   - Environmental Coalition: Advocate (organized/constrained) — sees constraint as solving waste crisis; embedded in regulatory system and subject to capture by extraction mechanisms
 *   - Government Environmental Agencies: Beneficiary (institutional/arbitrage) — gains regulatory authority and funding justification; can exit easily by modifying/revoking mandate
 *   - Road Users: Diffuse victim (analytical/analytical) — bear risks of untested durability, maintenance cost surprises, and lifecycle uncertainty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plastic_asphalt_mandate, 0.58).
domain_priors:suppression_score(plastic_asphalt_mandate, 0.62).
domain_priors:theater_ratio(plastic_asphalt_mandate, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plastic_asphalt_mandate, extractiveness, 0.58).
narrative_ontology:constraint_metric(plastic_asphalt_mandate, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(plastic_asphalt_mandate, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plastic_asphalt_mandate, tangled_rope).
narrative_ontology:human_readable(plastic_asphalt_mandate, "Government Mandate for Plastic-Infused Asphalt").
narrative_ontology:topic_domain(plastic_asphalt_mandate, "economic/environmental/political").

domain_priors:requires_active_enforcement(plastic_asphalt_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plastic_asphalt_mandate, plastic_waste_processors).
narrative_ontology:constraint_beneficiary(plastic_asphalt_mandate, government_environmental_agencies).
narrative_ontology:constraint_beneficiary(plastic_asphalt_mandate, asphalt_manufacturers_with_plastic_capacity).
narrative_ontology:constraint_victim(plastic_asphalt_mandate, traditional_asphalt_suppliers).
narrative_ontology:constraint_victim(plastic_asphalt_mandate, road_users_bearing_lifecycle_costs).
narrative_ontology:constraint_victim(plastic_asphalt_mandate, municipalities_with_limited_budgets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BUDGET-CONSTRAINED MUNICIPALITY (SNARE) — Legally bound to use plastic-infused asphalt regardless of cost premium (15-25% higher than conventional). No exit: federal/state mandate is legally binding. Bears extraction as higher maintenance budgets are diverted from other infrastructure. Cannot refuse without legal sanctions.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRADITIONAL ASPHALT SUPPLIER (SNARE) — Faces mandatory market disruption. Can exit through acquisition by plastic-processing firms or investment in plastic-integration capability, but barriers are high (capital requirements, supply chain restructuring). Constrained exit produces high experienced extraction as established market position erodes under mandate pressure.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLASTIC WASTE PROCESSOR (ROPE) — Primary beneficiary. Mandate creates guaranteed market for waste plastic streams that previously had limited commercial value. Can exit easily (supply to asphalt OR to other users), giving high arbitrage optionality. Experiences constraint as pure coordination: mandate solves the market-failure problem of plastic disposal by guaranteeing demand.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE CONSTRUCTION CONTRACTOR (TANGLED ROPE) — Can comply with mandate (has scale to source certified plastic-infused asphalt) but absorbs procurement complexity and price volatility. Mobile exit: can bid selectively on projects where plastic-asphalt sourcing is established. Experiences both coordination benefit (access to differentiated material, environmental branding) and extraction (supply chain dependency, regulatory compliance overhead).
constraint_indexing:constraint_classification(plastic_asphalt_mandate, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ENVIRONMENTAL COALITION (TANGLED ROPE) — Primary advocate for mandate. Sees constraint as solving plastic waste crisis (coordination function: moving waste from landfills to productive reuse). Also subject to extraction: coalition's success enables government capture of environmental authority without deeper system changes (recycling theater). Constrained exit: embedded in regulatory system, cannot easily pivot to alternative waste solutions.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ENVIRONMENTAL COMPLIANCE BUREAUCRACY (PITON) — Mandate creates regulatory enforcement infrastructure (testing labs, certification protocols, monitoring checkpoints). Theater ratio is high: verification that asphalt actually contains specified plastic percentage, that plastic is 'recycled' (definition ambiguous), and that lifecycle emissions are lower than conventional asphalt all require performative measurement. The bureaucracy persists through institutional inertia even as evidence on plastic-asphalt durability accumulates (mixed results: some studies show premature failure, others show extension).
constraint_indexing:constraint_classification(plastic_asphalt_mandate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT RISK) — Risk of naturalizing the mandate as an environmental inevitability ('plastic waste must go somewhere'). From civilizational scope, it appears that containing plastic waste in roads is an immutable constraint on any industrialized society. However, the structural data reveals this as contingent: the mandate exists because of specific political coalitions, not because of physical necessity. Alternative disposal pathways (advanced recycling, chemical breakdown, waste reduction at source) are suppressed by the mandate's extraction structure.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plastic_asphalt_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(plastic_asphalt_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(plastic_asphalt_mandate, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(plastic_asphalt_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(plastic_asphalt_mandate, TR),
    TR >= 0.70.

:- end_tests(plastic_asphalt_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The mandate creates measurable value transfer to plastic processors (guaranteed market at premium pricing) without corresponding benefit to municipalities (who absorb 15-25% cost markup). Initial extractiveness was lower (0.32) when mandate was new and environmental benefits were assumed; as durability questions emerged and compliance testing infrastructure expanded, extractiveness increased. The value reflects that extraction is not maximal (municipalities derive some environmental benefit, construction contractors gain branding value) but is significant and one-directional. Suppression (0.62): High. The mandate is legally binding; municipalities cannot opt out without federal/state sanctions. Traditional asphalt suppliers face coercive market restructuring. But suppression is not total: large actors can adapt (contractors can source plastic-asphalt), and the mandate does solve a real problem (plastic disposal). Theater ratio (0.68): High. Compliance verification is substantially performative: testing that plastic is present requires certification protocols, testing that it is 'recycled' (vs virgin plastic or contaminated waste) requires definition work, and testing that lifecycle emissions are lower than conventional asphalt requires assumptions about use patterns and disposal. The theater has increased over the interval as the gap between mandate claims and empirical durability data has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits dramatic perspectival disagreement. Plastic processors see pure coordination (Rope) — the mandate solves the market-failure problem of matching waste supply with demand. Environmental coalition advocates also see coordination but with some extraction component (Tangled Rope) — they have achieved their policy goal but now see that government has captured the solution and uses it to avoid deeper system changes. Municipalities and traditional suppliers see extraction disguised as environmental policy (Snare) — they bear costs without corresponding benefit and cannot exit. Large contractors see mixed benefit and burden (Tangled Rope) — they gain environmental branding and access to differentiated materials but absorb supply-chain complexity. The compliance bureaucracy sees its own degraded ritual (Piton) — verification protocols persist even as durability evidence remains contested. The civilizational analyst risks seeing an immutable law (Mountain) — 'plastic waste must be managed somehow' — but this naturalizes what is actually a contingent political settlement favoring plastic processors and government agencies over municipalities and waste-reduction innovators.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each agent is derived from their structural position. Plastic processors are beneficiaries with arbitrage optionality (d ≈ 0.05-0.15 → f(d) ≈ -0.12 to -0.01 → negative χ). Municipalities are victims with trapped exit (d ≈ 0.95 → f(d) ≈ 1.42 → maximum χ for their power level). Traditional suppliers are victims with constrained exit (d ≈ 0.80 → f(d) ≈ 1.20 → high χ). Large contractors are balanced actors with mobile exit (d ≈ 0.50 → f(d) ≈ 0.65 → moderate χ). Environmental coalition members are organized advocates with constrained exit due to regulatory capture (d ≈ 0.60 → f(d) ≈ 0.85, but reduced by beneficiary status). The scopal modifier σ(S) = 1.0 (national scope) — verification difficulty is not amplified by global complexity, as mandate is jurisdictionally bounded.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing between the genuine coordination function (plastic waste lacks market value; roads need capital repair) and the extraction mechanism (cost premium allocation, supply-chain coercion, theater of compliance). The mandate is correctly classified as Tangled Rope: it solves a real coordination problem (matching waste supply with demand) AND it creates asymmetric extraction (plastic processors benefit from guaranteed market while municipalities bear costs). The mandatrophy is NOT 'is this coordination or extraction?' but rather 'how much of the constraint is coordination function vs how much is extraction mechanism?' The answer: approximately 40% genuine coordination (municipalities do get environmental benefit, waste does leave landfills) and 60% extraction (cost markup, regulatory coercion, theater). The false summit risk (Mountain perspective) naturalizes the coordination component and ignores the extraction component. The piton classification correctly identifies that compliance verification has become performative while durability evidence remains contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    plastic_asphalt_durability_empirical,
    'Does plastic-infused asphalt actually show extended pavement life and lower lifecycle costs compared to conventional asphalt?',
    'Long-term field testing (10+ year studies) on pavement performance, crack propagation rates, maintenance frequency, and total cost of ownership across climate zones',
    'If true: constraint is genuinely coordination-enabling (Rope/Scaffold). If false: constraint is pure extraction disguised as environmental policy (Snare/Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plastic_asphalt_durability_empirical, empirical, 'Empirical durability and lifecycle cost comparison of plastic-asphalt vs conventional asphalt').

omega_variable(
    plastic_definition_quality,
    'What constitutes ''recycled plastic'' in asphalt specifications? Is it post-consumer waste or industrial waste? Does the mandate incentivize waste reduction or waste displacement?',
    'Audit of plastic sourcing practices; tracking of where plastic otherwise would have gone (landfill vs incineration vs export); measurement of total plastic waste stream volume before/after mandate',
    'If high-quality post-consumer waste sourcing: coordination function is real. If mandate just redirects industrial waste or imports cheap plastic: mandate is extraction mechanism for waste exporters.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(plastic_definition_quality, empirical, 'Definition and sourcing criteria for ''recycled plastic'' in mandate specifications').

omega_variable(
    alternative_waste_pathways,
    'Are alternative high-value uses for waste plastic (chemical recycling, 3D-printable feedstock, textile fibers) being suppressed by the asphalt mandate''s market guarantee?',
    'Comparative economic analysis of returns for plastic-to-asphalt vs plastic-to-chemistry; investment trends in alternative recycling technologies; licensing and regulatory barriers to non-asphalt uses',
    'If alternatives suppressed: mandate is extracting rents by forcing allocation to low-value use (Snare). If asphalt is genuinely highest-value: mandate solves true coordination problem (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_waste_pathways, empirical, 'Whether mandate suppresses higher-value alternative uses for waste plastic').

omega_variable(
    municipal_cost_distribution,
    'Does the cost premium for plastic-asphalt fall on those who benefit from waste reduction, or is it cross-subsidized by general taxation and municipal budgets?',
    'Audit of funding sources; comparison of plastic-asphalt procurement costs to conventional alternatives; measurement of whether waste-processing savings accrue to municipalities or to plastic processors',
    'If cost borne by users/municipalities while benefits accrue to processors: extraction is severe (high d → high χ). If costs and benefits are aligned: coordination function is more genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(municipal_cost_distribution, empirical, 'Distribution of cost premium and processing benefit across stakeholders').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plastic_asphalt_mandate, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pam_tr_t0, plastic_asphalt_mandate, theater_ratio, 0, 0.45).
narrative_ontology:measurement(pam_tr_t3, plastic_asphalt_mandate, theater_ratio, 3, 0.58).
narrative_ontology:measurement(pam_tr_t6, plastic_asphalt_mandate, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(pam_be_t0, plastic_asphalt_mandate, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pam_be_t3, plastic_asphalt_mandate, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(pam_be_t6, plastic_asphalt_mandate, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plastic_asphalt_mandate, resource_allocation).
narrative_ontology:affects_constraint(plastic_asphalt_mandate, municipal_infrastructure_funding).
narrative_ontology:affects_constraint(plastic_asphalt_mandate, plastic_waste_market_structure).
narrative_ontology:affects_constraint(plastic_asphalt_mandate, road_pavement_lifecycle_standards).

% DUAL FORMULATION NOTE:
% Plastic-asphalt mandate is downstream of two distinct structural problems: (1) municipal infrastructure funding shortfall (upstream constraint: municipal_infrastructure_funding) and (2) plastic waste disposal market failure (upstream constraint: plastic_waste_market_structure). The mandate couples these two problems into a single solution mechanism but the solution exhibits extraction characteristics because cost allocation favors plastic processors over municipalities. Decomposition into separate upstream constraints would show how each upstream problem could be solved independently without the extraction component.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plastic_asphalt_mandate, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
