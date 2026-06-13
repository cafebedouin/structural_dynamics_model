% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__mitigation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__mitigation_priority_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_imperative__mitigation_priority_reading
 *   human_readable: Climate Response as Mitigation-First via Innovation and Markets
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested
 *   climate-response kernel: the claim that climate change is solvable
 *   primarily through technological innovation and market mechanisms (carbon
 *   pricing, clean energy deployment, carbon capture, offset trading), with
 *   adaptation as a residual concern for those regions unable to mitigate
 *   fast enough. This reading is dominant in UNFCCC negotiations, World Bank
 *   climate finance, G7/G20 climate policy, and tech-sector climate
 *   commitments. The kernel contest involves three structurally distinct
 *   readings with different ε values, beneficiary/victim structures, and
 *   institutional bases. This story captures the mitigation-priority
 *   reading's structure: who benefits, who pays, what coordination function
 *   it serves, and what costs are deferred.
 *
 * KEY AGENTS:
 *   - Global North innovation sectors — set agenda, capture IP rents, benefit from sustained technology-focus
 *   - Climate-vulnerable regions (small islands, sub-Saharan Africa, South Asia) — structurally targeted payers, trapped exit, immediate harm, marginalized voice
 *   - Future generations — intergenerational payers, no voice in present decisions, inherit adaptation deficits if mitigation fails
 *   - Carbon offset market operators — extract transaction rents, benefit from continued focus on tradeable reductions
 *   - Wealthy consuming nations — benefit from deferred structural change, maintain high-consumption patterns under innovation cover
 *   - Adaptation advocates — excluded from mainstream UNFCCC architecture, undersourced relative to mitigation finance
 *   - Degrowth advocates — structurally excluded, identity-locked opposition, propose incompatible constraint frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__mitigation_priority_reading, 0.72).
domain_priors:theater_ratio(climate_response_imperative__mitigation_priority_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__mitigation_priority_reading, "Climate Response as Mitigation-First via Innovation and Markets").
narrative_ontology:topic_domain(climate_response_imperative__mitigation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__mitigation_priority_reading, '420b5b19-9745-4be0-b956-6f3a163eaff9').
narrative_ontology:cs_kernel_codification('420b5b19-9745-4be0-b956-6f3a163eaff9', distributed).
narrative_ontology:cs_authority_grounding('420b5b19-9745-4be0-b956-6f3a163eaff9', extraction).
narrative_ontology:cs_interpretation_layer_present('420b5b19-9745-4be0-b956-6f3a163eaff9').
narrative_ontology:cs_reading_relation('420b5b19-9745-4be0-b956-6f3a163eaff9', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('420b5b19-9745-4be0-b956-6f3a163eaff9', climate_response_imperative__degrowth_reading, influences).
narrative_ontology:cs_axiom('420b5b19-9745-4be0-b956-6f3a163eaff9', foundational, technological_innovation_sufficient_for_mitigation).
narrative_ontology:cs_axiom_status(technological_innovation_sufficient_for_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('420b5b19-9745-4be0-b956-6f3a163eaff9', technological_innovation_sufficient_for_mitigation, empirically_contingent).
narrative_ontology:cs_axiom('420b5b19-9745-4be0-b956-6f3a163eaff9', foundational, market_mechanisms_allocate_emissions_reductions_efficiently).
narrative_ontology:cs_axiom_status(market_mechanisms_allocate_emissions_reductions_efficiently, holdable).
narrative_ontology:cs_axiom_grounding('420b5b19-9745-4be0-b956-6f3a163eaff9', market_mechanisms_allocate_emissions_reductions_efficiently, empirically_contingent).
narrative_ontology:cs_axiom('420b5b19-9745-4be0-b956-6f3a163eaff9', secondary, adaptation_is_residual_to_rapid_mitigation).
narrative_ontology:cs_axiom_status(adaptation_is_residual_to_rapid_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('420b5b19-9745-4be0-b956-6f3a163eaff9', adaptation_is_residual_to_rapid_mitigation, instrumental).
narrative_ontology:cs_reference_frame('420b5b19-9745-4be0-b956-6f3a163eaff9', global_mitigation_imperative_2015).
narrative_ontology:cs_drift_state('420b5b19-9745-4be0-b956-6f3a163eaff9', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('420b5b19-9745-4be0-b956-6f3a163eaff9', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__mitigation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_offset_market_operators).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, technology_licensing_institutions).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, climate_vulnerable_regions).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, subsistence_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, wealthy_consuming_nations).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, market_efficiency_in_carbon_allocation).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, technological_solutionism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Green technology industries, carbon capture research consortia, renewable energy manufacturers, and battery companies in wealthy nations profit directly from sustained policy focus on technological solutions. They shape climate discourse through industry associations, lobby on UNFCCC delegations, control research funding pipelines, and influence international climate agreements. Their business models and institutional growth depend on mitigation-via-innovation remaining the dominant framing. They can exit if policy shifts by redirecting toward other sectors.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors, agenda_setter).

% Small island nations (Maldives, Tuvalu, Kiribati), sub-Saharan Africa (Sahel, Horn of Africa), South Asia (Bangladesh, Pakistan), and low-lying coastal zones already experience accelerating climate impacts: sea-level rise, drought, crop failure, flooding, cyclones. They pay through territory loss, resource depletion, displacement, and mortality. Adaptation finance is framed as climate aid rather than obligation, and is allocated far below actual needs (~$30B/year vs. $300B+ estimated need). Exit is impossible; they cannot leave their territories, and they have minimal leverage in international forums.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_vulnerable_regions, payer,
    powerless, immediate, trapped, regional).

% Cannot participate in present climate policy decisions but will inherit the outcomes: cumulative climate damages, unproven carbon removal technologies, constrained adaptation options if present mitigation targets are missed. The mitigation-priority framing defers their adaptation burden by assuming innovation will solve emissions fast enough; if innovation fails or CDR technologies do not scale, they inherit compounded risk. They have zero present choice and zero present representation.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Pastoralists, small-scale farmers, indigenous communities, and fishing populations dependent on specific climate regimes face livelihood collapse as precipitation, temperature, and seasonal patterns shift. Their adaptation needs are immediate and locally-specific (water management, crop switching, migration). They are trapped by land tenure, cultural identity, and lack of capital mobility. The mitigation-priority framing marginalizes their adaptation as residual problem rather than core climate response.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, subsistence_populations, payer,
    powerless, biographical, trapped, local).

% Financial institutions, carbon trading platforms, credit ratings agencies, and certification bodies operate and profit from the offset and carbon credit markets. The mitigation-via-markets framing legitimizes their role as essential climate infrastructure; they extract rents from every transaction, from standard-setting authority, and from the opacity of many offset verification schemes. Their business model depends on sustained policy focus on tradeable emissions reductions rather than structural economic change.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, carbon_offset_market_operators, beneficiary,
    institutional, biographical, arbitrage, global).

% Universities, research consortia, multinational tech companies, and energy corporations in the Global North hold IP on solar, wind, carbon capture, advanced batteries, and hydrogen technologies. They benefit from technology-transfer agreements, licensing fees, and control of the innovation pipeline. The mitigation-priority reading sustains their economic model and geopolitical leverage—developing nations must acquire their technology to meet climate targets, creating dependency relationships.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, technology_licensing_institutions, beneficiary,
    powerful, biographical, arbitrage, global).

% NGOs focused on development and resilience, economists studying adaptation costs, loss-and-damage scholars, and AOSIS (Alliance of Small Island States) are systematically undersourced and marginalized in international climate architecture. They advocate that adaptation is not residual but primary responsibility, that vulnerable regions deserve robust loss-and-damage mechanisms (reparations for climate harm), and that adaptation finance should exceed mitigation finance. They have voice but no power; they are excluded from meaningful agenda-setting in UNFCCC and World Bank climate boards.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, adaptation_finance_advocates, excluded,
    moderate, biographical, constrained, global).

% Scholars, activists, and policy advocates arguing for structural economic transformation—reduced consumption and production in wealthy nations, wealth redistribution, post-growth institutions—are positioned entirely outside mainstream climate policy. They would contest the entire framing of climate response as solvable within existing market and consumption structures. They are identity-locked into critique and oppositional stance; they have zero seats at UNFCCC, World Bank, or G7 tables and no pathway to mainstream influence without abandoning their core claim.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, degrowth_and_structural_transformation_advocates, excluded,
    powerless, civilizational, identity_locked, global).

% Global North governments (US, EU, Australia, Japan, Canada) benefit from the mitigation-priority framing because it defers structural changes to consumption and production patterns. Technology investment, carbon pricing, and offsetting allow high-consumption lifestyles to persist while appearing to address climate risk. They control policy direction through G7/G20 forums, UNFCCC voting power, climate finance architecture, and media/expert institutions. They frame the constraint, enforce it through international agreements, and capture the narrative.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, wealthy_consuming_nations, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, wealthy_consuming_nations, agenda_setter).

% IPCC, climate research institutions, and atmospheric scientists provide empirical evidence on emissions trajectories, warming pathways, and climate impacts. They are positioned as neutral observers providing the factual grounding for the constraint. They observe and report the divergence between IPCC findings (requiring BOTH rapid mitigation AND substantial adaptation) and actual policy focus (nearly exclusive mitigation emphasis, minimal adaptation investment). Their role is epistemically central but politically marginal.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_science_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:fixing_cost_class(climate_response_imperative__mitigation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes global climate response around technological solutions and market mechanisms (carbon pricing, offset markets, clean energy deployment, carbon capture). Enables coordination of emissions reductions across national boundaries without requiring structural changes to production and consumption patterns. Provides a common metric (tons of CO2 equivalent) for comparing and trading mitigation efforts.
% TRANSFER_FUNCTION: Transfers resources from wealthy nations and institutions toward innovation-sector profits, IP rents, and offset-market operators. Simultaneously defers adaptation and loss-and-damage obligations from high-emission historical polluters to vulnerable regions and future generations, who bear cumulative climate impacts while waiting for technological solutions to reduce future emissions.
% ABSENT_VOICES: Adaptation-focused institutions and scholars; developing nations' development finance ministers (who see adaptation as a poverty issue, not just climate risk); indigenous communities and subsistence populations most immediately harmed; climate-vulnerable small island states advocating for loss-and-damage mechanisms. Degrowth advocates are structurally excluded from UNFCCC architecture—they would argue the entire framing is a false solution to an inherently structural problem.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority constraint disappeared, climate finance would reallocate from technology innovation toward direct adaptation and loss-and-damage support in vulnerable regions; consumption patterns in the Global North would face immediate regulatory and social pressure for reduction; carbon offset markets would collapse; and the geopolitical leverage of technology-holding institutions would diminish. The global economy would reorganize around different problem framings and different beneficiary sets.
% FOUNDING_PROBLEM: Global emissions from industrial production and consumption are rising, and their consequences (warming, extreme weather, sea-level rise) will intensify unless emissions are rapidly reduced. Early climate science (1980s-1990s) established that atmospheric CO2 was the binding constraint; the framing emerged that if emissions are reduced fast enough via technological transition, warming can be limited to manageable levels, and adaptation needs can be contained. Wealthy nations and innovation sectors adopted this framing because it deferred demands for consumption reduction and structural redistribution.
% FOUNDING_PROBLEM_CORROBORATION: The IPCC reports confirm that global mitigation is necessary and urgent (AR6 confirms 1.5°C and 2°C pathways require rapid emissions reduction). However, IPCC also emphasizes that mitigation ALONE is insufficient—all pathways require substantial simultaneous adaptation, that residual impacts will occur regardless of mitigation success, and that vulnerable regions require immediate adaptation investment and loss-and-damage finance. The founding-problem status is contested: mitigation advocates say the problem is urgent and solvable via innovation; adaptation advocates say the founding problem is outdated—we are now in a dual-imperative phase where both are primary, not sequential.
narrative_ontology:disappearance_verdict(climate_response_imperative__mitigation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__mitigation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__mitigation_priority_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__mitigation_priority_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__mitigation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__mitigation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.48 → 0.68) as the divergence between global emissions and climate targets widens, and as the constraint's reliance on unproven carbon removal technologies (Direct Air Capture, enhanced weathering) becomes more explicit. Theater ratio rises as well (0.22 → 0.43), indicating that as the founding problem's insufficiency becomes clearer, more effort goes into defending the framing against alternatives rather than into functional mitigation. Suppression requirement is stable-to-rising (0.58 → 0.72) because the constraint must actively exclude and marginalize competing framings (adaptation-first, degrowth) to maintain legitimacy. All metrics share one time grid so the measurement series are coherent. The constraint is claimed as tangled_rope because it possesses both genuine coordination (organizing emissions reductions) AND asymmetric extraction (deferring adaptation costs to powerless victims and future generations) backed by enforcement (gatekeeping of climate finance toward innovation, suppression of alternative policy framings).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (Global North innovation sectors, wealthy consuming nations, offset-market operators) experience this as genuine coordination—they see climate risk, fund solutions, and maintain institutional stability. From the payer seats (vulnerable regions, future generations, subsistence populations), the same structure operates as enforced extraction—they experience the constraint as deferring adaptation obligations and concentrating mitigation finance on technologies that may never materialize, while their immediate survival needs go unfunded. The agenda-setter seats (wealthy nations, innovation institutions) control the narrative and policy levers; powerless victim seats have voice but no power (small island advocacy, adaptation NGO testimony) or no present standing at all (future generations). The engine computes different directionality and type from each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North innovation sectors and wealthy consuming nations sit at the beneficiary end (d ~0.10–0.20): they collect gains (IP rents, profit, sustained consumption) with minimal cost to themselves and strong exit options (arbitrage—can shift to other sectors if climate policy changes). Climate-vulnerable regions sit at the target end (d ~0.85–0.95): they bear immediate, accelerating costs (flooding, drought, displacement), cannot exit (trapped by geography and poverty), and have minimal influence on the constraint's rules. Future generations are the most extreme target (d → 1.0): they have zero present choice, zero present power, zero present voice, and will inherit compounded climate damages if mitigation fails. Adaptation advocates and degrowth advocates sit at the excluded end (d ~0.70–0.80): they would benefit from reshaping the constraint but are systematically kept out of agenda-setting. This radical asymmetry in directionality is the story's core structural claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The mitigation-priority framing emerged in the 1990s as a legitimate response to the binding constraint: global emissions were rising and atmospheric CO2 was the measurable driver. That founding problem remains live. However, the constraint now carries a secondary function that was not present at founding: maintaining the legitimacy of Global North consumption patterns and deferring adaptation obligations. Theater ratio rising (0.22 → 0.43) while actual emissions reductions stagnate and adaptation deficits widen is the signal. The constraint's architects can point to real mitigation efforts (renewable capacity, efficiency gains, some carbon pricing) to justify continued focus on innovation and markets; but the actual trajectory shows emissions flat-lining while climate damages accelerate, and adaptation finance remains less than 10% of global climate finance. This gap between founding problem (rapid mitigation is essential) and current operation (rich nations maintain consumption; vulnerable regions remain underadapted) is the mandatrophy signal: the constraint began in genuine functional necessity and has become partly a cover story for deferring the harder redistribution and consumption-change questions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cdr_technology_viability,
    'Will carbon dioxide removal technologies (Direct Air Capture, enhanced weathering, bioenergy with CCS) reach gigatonne-scale deployment and permanent sequestration at economically viable cost within the timeframes required (2050-2100)?',
    'Real-world CDR deployment costs and volumes; breakthrough in permanent sequestration storage; manufacturing scale-up curves for DAC and BECCS infrastructure.',
    'If CDR viability fails (high cost, low deployment, leakage), the mitigation-priority reading''s pathway to limiting warming collapses, and adaptation becomes structurally necessary rather than residual. Victims (vulnerable regions, future generations) become recognized as such; the constraint reclassifies from tangled_rope to snare. If CDR succeeds, the reading''s framing holds, but extraction burden remains deferred rather than eliminated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cdr_technology_viability, empirical, 'Whether carbon removal technologies can deliver at the scale and cost the mitigation-priority pathway assumes.').

omega_variable(
    mitigation_adaptation_sequencing,
    'Is it structurally possible to achieve 1.5–2°C warming limits while maintaining currently-dominant consumption patterns in the Global North, with adaptation only for residual impacts in vulnerable regions?',
    'Emissions pathway analysis; climate impact projections under mitigation-only scenarios; adaptation cost and feasibility studies in vulnerable regions.',
    'If the sequencing is infeasible (mitigation insufficient, adaptation needs exceed capacity), the adaptation-priority and degrowth readings become functionally necessary. If feasible, the mitigation-priority framing holds, but credibility requires rapid, verifiable emissions reductions (not currently observed) and near-zero risk of CDR failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_adaptation_sequencing, empirical, 'Whether the mitigation-priority reading''s core assumption—that rapid mitigation can be achieved while adaptation remains secondary—is achievable.').

omega_variable(
    consumption_reduction_necessity,
    'Can global emissions be reduced to net-zero within the 2050–2080 window while maintaining or increasing per-capita material consumption in wealthy nations?',
    'Life-cycle analysis of consumption-compatible decarbonization pathways; energy intensity trends; rebound effects from efficiency gains; implicit carbon content of services economy.',
    'If consumption reduction is necessary (cannot be decoupled from emissions reduction at required speed), the degrowth reading becomes a structural requirement, and the mitigation-priority framing is revealed as deferring a harder problem. If decoupling is achievable, the mitigation-priority framing''s promise of ''no sacrifice'' holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consumption_reduction_necessity, empirical, 'Whether technological decoupling of emissions from consumption is achievable at the scale and speed required.').

omega_variable(
    intergenerational_extraction_legitimacy,
    'Is it legitimate for present generations to defer adaptation obligations to future generations, betting on unproven CDR technologies and hoping mitigation succeeds faster than current trajectories suggest?',
    'Ethical framework applied to intergenerational climate justice; observed climate damage and adaptation deficit trends; actual vs. projected mitigation progress.',
    'If deferral is deemed illegitimate (by international courts, ethical consensus, or future-generation grievance mechanisms), the constraint''s legitimacy collapses, and adaptation-priority or degrowth framings become dominant. If deferral is sustained as necessary (given mitigation urgency), extraction continues.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_extraction_legitimacy, preference, 'The normative legitimacy of intergenerational climate-adaptation deferral under uncertainty.').

omega_variable(
    reading_foreclosure_risk,
    'If climate impacts accelerate beyond adaptation capacity in vulnerable regions (multiple simultaneous crop failures, mass displacement, ecosystem collapse), does the mitigation-priority reading''s framing remain credible, or do the adaptation-priority and degrowth readings become mandatory alternatives?',
    'Tracking of realized climate impacts, adaptation capacity, and observed policy responses in vulnerable regions; shifts in international climate finance allocation; emergence of new institutional frameworks for loss-and-damage.',
    'A severe adaptation-failure cascade would functionally foreclose the mitigation-priority reading—the founding problem (rapid mitigation is sufficient) would be empirically falsified, and the constraint would be reclassified as snare or mandate restructuring via the adaptation-priority or degrowth readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_risk, empirical, 'Whether observed climate impacts will force abandonment of the mitigation-priority reading in favor of structurally different response framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__mitigation_priority_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__mitigation_priority_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t8, climate_response_imperative__mitigation_priority_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(clim_tr_t8, observed).
narrative_ontology:measurement(clim_tr_t16, climate_response_imperative__mitigation_priority_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement_basis(clim_tr_t16, observed).
narrative_ontology:measurement(clim_tr_t25, climate_response_imperative__mitigation_priority_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement_basis(clim_tr_t25, observed).
narrative_ontology:measurement(clim_tr_t35, climate_response_imperative__mitigation_priority_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(clim_tr_t35, observed).
narrative_ontology:measurement(clim_tr_t50, climate_response_imperative__mitigation_priority_reading, theater_ratio, 50, 0.43).
narrative_ontology:measurement_basis(clim_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t8, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(clim_be_t8, observed).
narrative_ontology:measurement(clim_be_t16, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement_basis(clim_be_t16, observed).
narrative_ontology:measurement(clim_be_t25, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(clim_be_t25, observed).
narrative_ontology:measurement(clim_be_t35, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(clim_be_t35, observed).
narrative_ontology:measurement(clim_be_t50, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(clim_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t8, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(clim_su_t8, observed).
narrative_ontology:measurement(clim_su_t16, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement_basis(clim_su_t16, observed).
narrative_ontology:measurement(clim_su_t25, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(clim_su_t25, observed).
narrative_ontology:measurement(clim_su_t35, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(clim_su_t35, observed).
narrative_ontology:measurement(clim_su_t50, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(clim_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__mitigation_priority_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__mitigation_priority_reading, 0.22).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, carbon_offset_market_legitimacy).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, technology_transfer_agreements).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, intergenerational_justice_asymmetry).

% DUAL FORMULATION NOTE:
% This constraint is ONE reading of the climate-response-imperative kernel. The mitigation-priority reading treats adaptation and structural change as residual; the adaptation-priority reading inverts priorities; the degrowth reading claims both are impossible without structural economic transformation. The three readings have different ε values (this reading: 0.68; adaptation-priority: ~0.50; degrowth: ~0.75), different victim/beneficiary structures, and incommensurable solutions. They are linked not as alternative measurements of one constraint but as three structurally distinct constraints instantiating three readings of the same kernel. The network affects field routes to all three; the reader follows the three stories for a full account of the kernel's contested space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__mitigation_priority_reading, powerless, 0.92).
constraint_indexing:directionality_override(climate_response_imperative__mitigation_priority_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
