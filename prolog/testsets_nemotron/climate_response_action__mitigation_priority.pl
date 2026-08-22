% ============================================================================
% CONSTRAINT STORY: climate_response_action__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__mitigation_priority, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_action__mitigation_priority
 *   human_readable: Climate Response: Mitigation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation_priority reading of the climate_response_action kernel
 *   frames climate response as achieving <2°C through emissions reductions
 *   driven by technological innovation and carbon markets, while maintaining
 *   GDP growth. This reading became dominant post-UNFCCC (1992), cemented in
 *   Kyoto (1997) and Paris (2015). Its structural operation: concentrates
 *   near-term mitigation costs on high-emitting sectors (fossil energy, heavy
 *   industry, transport) in industrialized nations; creates revenue streams
 *   for carbon market intermediaries and clean tech sectors; assumes carbon
 *   removal technologies will mature to handle residual emissions; defers
 *   adaptation costs to vulnerable regions (Global South) who lack innovation
 *   capacity; shifts the burden of unproven CDR deployment to future
 *   generations. The reading is structurally a tangled_rope: it coordinates
 *   global mitigation effort (genuine coordination function) while extracting
 *   asymmetrically from high-emitting sectors, vulnerable regions, and future
 *   generations (asymmetric extraction). Requires active enforcement (NDCs,
 *   carbon market rules, technology transfer commitments). The claimed_type
 *   is tangled_rope; metrics describe substantial and rising extraction
 *   (0.35→0.68), moderate suppression (0.25→0.55), and growing theater
 *   (0.15→0.42) as the gap between pledged pathways and implemented policy
 *   widens.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_response_action__mitigation_priority, 0.55).
domain_priors:theater_ratio(climate_response_action__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__mitigation_priority, "Climate Response: Mitigation Priority Reading").
narrative_ontology:topic_domain(climate_response_action__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__mitigation_priority, 'c0047578-c229-426f-a643-8160a55ad9f8').
narrative_ontology:cs_kernel_codification('c0047578-c229-426f-a643-8160a55ad9f8', formalized).
narrative_ontology:cs_authority_grounding('c0047578-c229-426f-a643-8160a55ad9f8', extraction).
narrative_ontology:cs_interpretation_layer_present('c0047578-c229-426f-a643-8160a55ad9f8').
narrative_ontology:cs_reading_relation('c0047578-c229-426f-a643-8160a55ad9f8', climate_response_action__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('c0047578-c229-426f-a643-8160a55ad9f8', climate_response_action__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('c0047578-c229-426f-a643-8160a55ad9f8', foundational, growth_compatible_deep_decarbonization_feasible).
narrative_ontology:cs_axiom_status(growth_compatible_deep_decarbonization_feasible, holdable).
narrative_ontology:cs_axiom_grounding('c0047578-c229-426f-a643-8160a55ad9f8', growth_compatible_deep_decarbonization_feasible, empirically_contingent).
narrative_ontology:cs_axiom('c0047578-c229-426f-a643-8160a55ad9f8', foundational, carbon_removal_at_gigatonne_scale_achievable).
narrative_ontology:cs_axiom_status(carbon_removal_at_gigatonne_scale_achievable, holdable).
narrative_ontology:cs_axiom_grounding('c0047578-c229-426f-a643-8160a55ad9f8', carbon_removal_at_gigatonne_scale_achievable, empirically_contingent).
narrative_ontology:cs_axiom('c0047578-c229-426f-a643-8160a55ad9f8', secondary, market_mechanisms_sufficient_for_global_coordination).
narrative_ontology:cs_axiom_status(market_mechanisms_sufficient_for_global_coordination, holdable).
narrative_ontology:cs_axiom_grounding('c0047578-c229-426f-a643-8160a55ad9f8', market_mechanisms_sufficient_for_global_coordination, conventional).
narrative_ontology:cs_reference_frame('c0047578-c229-426f-a643-8160a55ad9f8', unfccc_1992_stabilization_mandate).
narrative_ontology:cs_drift_state('c0047578-c229-426f-a643-8160a55ad9f8', post_paris_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c0047578-c229-426f-a643-8160a55ad9f8', '').
narrative_ontology:cs_kernel_id(climate_response_action__mitigation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, high_innovation_capacity_nations).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, clean_technology_sectors).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, financial_institutions_carbon_finance).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, high_emitting_industrial_sectors).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, vulnerable_global_south_regions).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, fossil_fuel_dependent_workers).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, low_innovation_capacity_nations).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, technological_optimism_carbon_removal_feasibility).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, green_growth_narrative).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, market_mechanism_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the mitigation agenda through UNFCCC/Paris governance, IPCC authorship, and climate finance control. Capture technology rents from clean energy, carbon removal, and efficiency innovations. Their innovation capacity lets them meet NDCs while growing GDP — they arbitrage the constraint by turning compliance into competitive advantage. Exit is arbitrage-grade: they can relocate production, shift supply chains, or buy offsets.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, high_innovation_capacity_nations, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, high_innovation_capacity_nations, agenda_setter).

% Operate the Article 6 and voluntary carbon market infrastructure: registries, verification, brokering, financial structuring. Extract fees from every tonne traded. The constraint's enforcement machinery (NDC accounting, carbon market rules) creates their market. Mobile exit: they can shift to other environmental asset classes if carbon markets contract.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, carbon_market_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Receive massive subsidy flows (IRA, EU Green Deal, carbon contracts for difference) and guaranteed market creation via regulation. Their growth is policy-dependent but they hold market power in key technologies (solar, batteries, electrolyzers). Mobile exit: they can pivot to adjacent markets or jurisdictions if policy support shifts.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, clean_technology_sectors, beneficiary,
    powerful, biographical, mobile, global).

% Structure carbon-linked financial products (transition bonds, sustainability-linked loans, CDR offtake agreements). Capture the financialization rents of the mitigation pathway. Arbitrage exit: they control capital allocation across the entire energy system and can reprice risk faster than policy adjusts.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, financial_institutions_carbon_finance, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the concentrated costs of emissions reductions: stranded assets (coal plants, oil reserves, blast furnaces), carbon pricing compliance, mandated technology switching. Their political power delays but cannot escape the constraint — assets are geography-bound, workforces are specialized, and regulators hold permitting authority. Constrained exit: they can lobby for slower phase-out or free allocation, but structural decline is locked in by the mitigation pathway.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, high_emitting_industrial_sectors, payer,
    powerful, biographical, constrained, global).

% Pay the adaptation costs the mitigation_priority reading defers: sea-level rise, extreme heat, agricultural collapse, climate migration. Lack innovation capacity to capture clean tech rents; face technology access barriers (IP, finance, skills). The $100B climate finance pledge remains unmet. Constrained exit: they can advocate in negotiations (G77, AOSIS) but structural dependency on emitters' mitigation and financiers' terms limits leverage. Excluded from agenda-setting: the mitigation pathway is designed by and for high-innovation nations.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, vulnerable_global_south_regions, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, vulnerable_global_south_regions, excluded).

% Inherit the CDR deployment burden (gigatonnes/year assumed post-2050), residual climate damages from overshoot risk, and pathway dependency locked in by today's infrastructure choices. Zero voice in current negotiations, zero exit from the planetary system. The mitigation_priority reading's growth mandate makes their burden structural: every year of delayed absolute emissions reduction increases their CDR requirement non-linearly.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Bear concentrated transition costs: job loss, community decline, skill obsolescence in coal, oil, gas regions. Just transition rhetoric exists but funded transition programs are minimal. Constrained exit: retraining is partial, geographic mobility is limited by community ties and housing markets, age discrimination limits re-entry. They are payers within high-emitting nations — the constraint extracts from their livelihoods to fund the mitigation pathway.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, fossil_fuel_dependent_workers, payer,
    moderate, biographical, constrained, national).

% Depend on technology transfer and finance from high-innovation nations to meet NDCs. Pay monopoly pricing for patented clean tech, lack domestic R&D capacity to adapt solutions. Constrained exit: they can form technology transfer coalitions (African Group, LDCs) but WTO TRIPS and bilateral investment treaties lock in IP regimes. They are payers in the technology access dimension of the constraint.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, low_innovation_capacity_nations, payer,
    moderate, generational, constrained, global).

% Advocate for adaptation_priority and degrowth_transformation readings. Excluded from formal negotiation text-drafting (observer status only). Their mobilization creates political pressure but does not shift the mitigation_priority reading's structural parameters. Mobile exit: they can shift advocacy to national courts, shareholder actions, direct action — but these are peripheral to the UNFCCC constraint.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, climate_justice_civil_society, excluded,
    organized, biographical, mobile, global).

% Produces the assessment reports that define the <2°C pathway, CDR feasibility assumptions, and mitigation cost curves. Their scenarios (SSPs, IAMs) structurally encode the mitigation_priority reading's assumptions. Analytical exit: they can refine scenarios but the kernel framing (growth-compatible mitigation) is baked into the request from governments.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, ipcc_scientific_assessment_body, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__mitigation_priority, carbon_market_intermediaries).
narrative_ontology:fixing_cost_class(climate_response_action__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global emissions reductions toward a collective temperature target (<2°C) through a common metric (CO2e), a ratchet mechanism (NDCs), and market-based cost allocation (carbon markets, Article 6). Solves the free-rider problem of atmospheric commons by creating a unified accounting and incentive framework.
% TRANSFER_FUNCTION: Moves mitigation costs from the global atmosphere (avoided damages) to high-emitting sectors (compliance costs, stranded assets) and vulnerable regions (adaptation costs, residual damages). Moves financial rents to carbon market intermediaries, clean tech sectors, and financial institutions. Moves CDR deployment burden to future generations. Moves geopolitical leverage to high-innovation-capacity nations.
% ABSENT_VOICES: Future generations (structurally excluded — no voice, maximal stake). Indigenous peoples and local communities in Global South (marginalized in NDC design, their territories host CDR/offset projects). Degrowth and climate justice advocates (excluded from formal text, present only as observers). Fossil fuel dependent communities in Global South (no just transition framework for their contexts).
% DISAPPEARANCE_RATIONALE: If the mitigation_priority reading vanished overnight: NDCs and carbon markets would collapse; high-emitting sectors would face no coordinated phase-out; clean tech subsidy flows would stop; Global South adaptation finance would lose its institutional home; future generations would lose the (fragile) CDR deployment pathway. The world would rearrange into either uncoordinated national responses (likely higher emissions) or a shift to adaptation_priority/degrowth_transformation framings — but the institutional architecture, financial flows, and technology deployment trajectories would fundamentally restructure.
% FOUNDING_PROBLEM: The UNFCCC (1992) was built to solve: 'How to stabilize greenhouse gas concentrations at a level that prevents dangerous anthropogenic interference with the climate system, while enabling sustainable economic development for all nations.' The mitigation_priority reading narrowed this to: 'How to achieve <2°C through emissions reductions compatible with continued GDP growth, using markets and innovation.'
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (stabilization) is corroborated by IPCC Working Group I (physical science) — the problem is live and worsening. The mitigation_priority reading's SPECIFIC framing (growth-compatible, market-mediated, innovation-dependent) is contested: high_innovation_capacity_nations and carbon_market_intermediaries attest it remains viable; vulnerable_global_south_regions (via G77 statements, loss_and_damage negotiations) and climate_justice_civil_society attest it has shifted function to extraction; future_generations (via youth litigation, intergenerational equity scholarship) attest it shifts burden to them. No corroboration from outside the beneficiary set for the growth-compatible claim — independent economic analyses (Hickel, Kallis, Jackson, Timmer) find absolute decoupling insufficient at required pace.
narrative_ontology:disappearance_verdict(climate_response_action__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_response_action__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__mitigation_priority, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1992, UNFCCC framework — broad coordination, low immediate cost) to 0.68 (2026, post-Paris — binding NDCs, carbon markets operational, CDR assumed at scale, growth-maintenance mandate locks in high mitigation burden on emitters). Suppression rises from 0.25 to 0.55: early framework was voluntary; Paris introduced ratchet mechanism, transparency framework, and Article 6 carbon markets that actively exclude non-participants. Theater rises from 0.15 to 0.42: the gap between collective NDC ambition and 2°C pathway widens each stocktake; performative pledges substitute for structural transformation. Accessibility_collapse=0.58: alternative framings (adaptation_priority, degrowth_transformation) are marginalized in formal negotiations but persist in civil society and Global South advocacy — not fully collapsed. Resistance=0.62: significant pushback from fossil sectors, Global South (climate justice), and degrowth advocates; but resistance is fragmented across seats. The metrics and claim are independent: claimed_type=tangled_rope acknowledges the genuine coordination function; metrics capture the extraction that has accumulated atop it.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is structural: high_innovation_capacity_nations and carbon_market_intermediaries experience this as rope/beneficiary (d low, χ negative) — they gain technology rents, market fees, and geopolitical leverage. High_emitting_industrial_sectors experience as snare/target (d high, χ high) — they bear stranded asset risk, compliance costs, and transition mandates with constrained exit. Vulnerable_global_south_regions experience as snare/target (d high, χ high) — they pay adaptation costs the reading defers, lack innovation capacity to capture benefits, and face locked-in impacts from others' emissions. Future_generations experience as snare/target (d=1.0 structural, χ maximal) — they inherit CDR deployment burden, residual damages, and pathway dependency with zero exit. Fossil_fuel_dependent_workers experience as tangled_rope/payer (d moderate-high) — they bear concentrated transition costs with constrained exit (skills, geography). The engine computes these from beneficiary/victim declarations + power/exit. No overrides needed — structural derivation is accurate.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: high_innovation_capacity_nations (institutional power, arbitrage exit — capture technology rents and set rules), carbon_market_intermediaries (organized power, mobile exit — extract fees from every transaction), clean_technology_sectors (powerful, mobile — capture subsidy flows and market share), financial_institutions_carbon_finance (institutional, arbitrage — create asset classes). Victims declared: high_emitting_industrial_sectors (powerful but constrained exit — stranded assets, regulatory lock-in), vulnerable_global_south_regions (moderate power, constrained exit — adaptation costs, technology access barriers), future_generations (powerless, trapped — zero exit, maximal inheritance), fossil_fuel_dependent_workers (moderate, constrained — skills/geography lock-in), low_innovation_capacity_nations (moderate, constrained — technology dependency). Vindicated propositions are NOT beneficiaries — they are doctrines the reading's operation validates (technological_optimism_carbon_removal_feasibility, green_growth_narrative, market_mechanism_sufficiency).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stabilizing climate at safe temperature) remains live — but the mitigation_priority reading's specific coordination mechanism (growth-compatible, market-mediated, innovation-dependent deep decarbonization) shows mandatrophy signals: the growth-maintenance mandate has become the binding constraint, not the temperature target. CDR assumption papers over the gap. Theater rise (0.15→0.42) indicates performative substitution. The reading persists because beneficiaries (innovation nations, carbon finance) capture concentrated gains; victims (Global South, future generations) lack coalition power to force revision. This is not a degraded piton — the coordination function is still actively pursued — but the extraction has accumulated atop it, making it a tangibly extractive tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is this constraint a single reading of the contested climate_response_action kernel, or does it capture the kernel''s totality?',
    'Compare structural profiles across all declared readings (mitigation_priority, adaptation_priority, degrowth_transformation). If each reading has distinct ε, beneficiary/victim structures, and coordination/extraction balances, they are separate constraints from one kernel. Document the structural delta this reading instantiates.',
    'If this is one reading, its ε=0.68 and claimed_type=tangled_rope apply ONLY to the mitigation_priority reading. The sibling readings would instantiate different constraints with different classifications. The kernel itself is not classified — only its readings are.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether this constraint is a kernel reading requiring decomposition discipline (DP-001 ε-invariance).').

omega_variable(
    carbon_removal_feasibility_assumption,
    'Does the mitigation_priority reading''s structural viability depend on the assumption that large-scale carbon removal will become technologically and economically feasible?',
    'Track empirical progress of carbon dioxide removal (CDR) technologies against the scale assumed in integrated assessment models (IAMs) that underpin the <2°C pathway. If CDR deployment falls short of assumed gigatonnes/year, the reading''s coordination function collapses and its extraction shifts from tangled_rope toward snare.',
    'If CDR feasibility fails, the mitigation_priority reading''s coordination story (technological innovation enabling emissions reductions while maintaining growth) is falsified — the constraint becomes predominantly extractive (shifting burden to future generations who must bear unmet removal burden). This would reclassify from tangled_rope to snare for the future_generations seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_removal_feasibility_assumption, empirical, 'Structural dependency on unproven carbon removal at scale.').

omega_variable(
    growth_emissions_decoupling_evidence,
    'Is absolute decoupling of GDP growth from emissions reductions occurring at the pace and scale the mitigation_priority reading assumes?',
    'Longitudinal analysis of territorial and consumption-based emissions vs. GDP across major economies. If decoupling is only relative (emissions intensity falls but absolute emissions rise with growth) or requires offshoring, the reading''s coordination function is empirically unsupported.',
    'If decoupling fails, the green_growth_narrative vindicated_proposition is falsified. The constraint''s coordination function collapses; extraction concentrates on high_emitting_sectors without the promised innovation offset. Reclassifies toward snare for those sectors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_emissions_decoupling_evidence, empirical, 'Whether the reading''s core coordination claim (growth-compatible deep decarbonization) holds empirically.').

omega_variable(
    intergenerational_beneficiary_asymmetry,
    'Does the mitigation_priority reading structurally extract from future generations by deferring the hardest mitigation (carbon removal) to them while claiming present benefits?',
    'Intergenerational accounting of mitigation burden: compare present-cost emissions reductions vs. future-cost CDR deployment assumed in IAM pathways. If >50% of cumulative mitigation burden falls post-2050, future_generations are net payers, not beneficiaries.',
    'Confirms future_generations as structural victims (high directionality) rather than beneficiaries. The reading''s claimed coordination function obscures an intergenerational transfer. This is the core structural delta from adaptation_priority (which front-loads protection costs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_beneficiary_asymmetry, conceptual, 'Whether the reading''s temporal burden distribution makes future generations net extractees.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__mitigation_priority, 1992, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_response_action__mitigation_priority_tr_t1992, climate_response_action__mitigation_priority, theater_ratio, 1992, 0.15).
narrative_ontology:measurement(climate_response_action__mitigation_priority_tr_t1997, climate_response_action__mitigation_priority, theater_ratio, 1997, 0.18).
narrative_ontology:measurement(climate_response_action__mitigation_priority_tr_t2005, climate_response_action__mitigation_priority, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(climate_response_action__mitigation_priority_tr_t2009, climate_response_action__mitigation_priority, theater_ratio, 2009, 0.28).
narrative_ontology:measurement(climate_response_action__mitigation_priority_tr_t2015, climate_response_action__mitigation_priority, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(climate_response_action__mitigation_priority_tr_t2021, climate_response_action__mitigation_priority, theater_ratio, 2021, 0.39).
narrative_ontology:measurement(climate_response_action__mitigation_priority_tr_t2026, climate_response_action__mitigation_priority, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(climate_response_action__mitigation_priority_be_t1992, climate_response_action__mitigation_priority, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(climate_response_action__mitigation_priority_be_t1997, climate_response_action__mitigation_priority, base_extractiveness, 1997, 0.38).
narrative_ontology:measurement(climate_response_action__mitigation_priority_be_t2005, climate_response_action__mitigation_priority, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(climate_response_action__mitigation_priority_be_t2009, climate_response_action__mitigation_priority, base_extractiveness, 2009, 0.48).
narrative_ontology:measurement(climate_response_action__mitigation_priority_be_t2015, climate_response_action__mitigation_priority, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(climate_response_action__mitigation_priority_be_t2021, climate_response_action__mitigation_priority, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement(climate_response_action__mitigation_priority_be_t2026, climate_response_action__mitigation_priority, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(climate_response_action__mitigation_priority_su_t1992, climate_response_action__mitigation_priority, suppression_requirement, 1992, 0.25).
narrative_ontology:measurement(climate_response_action__mitigation_priority_su_t1997, climate_response_action__mitigation_priority, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(climate_response_action__mitigation_priority_su_t2005, climate_response_action__mitigation_priority, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(climate_response_action__mitigation_priority_su_t2009, climate_response_action__mitigation_priority, suppression_requirement, 2009, 0.42).
narrative_ontology:measurement(climate_response_action__mitigation_priority_su_t2015, climate_response_action__mitigation_priority, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(climate_response_action__mitigation_priority_su_t2021, climate_response_action__mitigation_priority, suppression_requirement, 2021, 0.52).
narrative_ontology:measurement(climate_response_action__mitigation_priority_su_t2026, climate_response_action__mitigation_priority, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__mitigation_priority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(climate_response_action__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, carbon_market_architecture).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, cdr_deployment_governance).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_finance_obligations).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_action kernel. The mitigation_priority reading (this file) frames response as growth-compatible technological mitigation; adaptation_priority frames it as resilience-first acceptance; degrowth_transformation frames it as structural economic transformation. The three readings have mutually incompatible beneficiary/victim structures and coordination/extraction balances — they are distinct constraints linked by network.affects_constraints. The mitigation_priority reading influences both siblings: it draws financial/technological resources toward mitigation and away from adaptation (resource competition), and its growth mandate structurally forecloses degrowth_transformation within formal governance frameworks (though degrowth persists as civil society position).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
