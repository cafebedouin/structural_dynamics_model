% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__velocity_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__velocity_primacy_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: technology_legitimacy_kernel__velocity_primacy_reading
 *   human_readable: Velocity Primacy Legitimacy Gate for Climate Mitigation Technologies
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The velocity primacy reading of technology legitimacy for climate
 *   mitigation asserts that the only valid criterion for technology
 *   eligibility is deployment speed against the remaining carbon budget. This
 *   reading became dominant in Western climate policy after Paris 2015,
 *   encoded in the EU Taxonomy's initial exclusion of nuclear, the IRA's
 *   technology-neutral-but-velocity-weighted credits, and the IPCC's emphasis
 *   on 2030 emissions peaks. It structurally benefits wind, solar, batteries,
 *   and transmission — technologies with 2-4 year deployment cycles — while
 *   excluding nuclear (10-15 year lead times), CCS, and long-duration
 *   storage. The constraint operates as a tangled rope: it solves a genuine
 *   coordination problem (mobilizing capital at speed) while extracting from
 *   grid operators (who bear integration costs), nuclear communities (who
 *   lose policy support), and industrial users (who face reliability and cost
 *   risks). The claim/metric independence is deliberate: the reading CLAIMS
 *   rope/coordination framing while the metrics reveal substantial extraction
 *   and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.68).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.72).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Velocity Primacy Legitimacy Gate for Climate Mitigation Technologies").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, 'd7858ea4-7016-4433-bd81-c6e9870b917c').
narrative_ontology:cs_kernel_codification('d7858ea4-7016-4433-bd81-c6e9870b917c', formalized).
narrative_ontology:cs_authority_grounding('d7858ea4-7016-4433-bd81-c6e9870b917c', extraction).
narrative_ontology:cs_interpretation_layer_present('d7858ea4-7016-4433-bd81-c6e9870b917c').
narrative_ontology:cs_reading_relation('d7858ea4-7016-4433-bd81-c6e9870b917c', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7858ea4-7016-4433-bd81-c6e9870b917c', technology_legitimacy_kernel__reliability_primacy_reading, influences).
narrative_ontology:cs_axiom('d7858ea4-7016-4433-bd81-c6e9870b917c', foundational, deployment_velocity_as_sole_legitimacy_criterion).
narrative_ontology:cs_axiom_status(deployment_velocity_as_sole_legitimacy_criterion, holdable).
narrative_ontology:cs_axiom_grounding('d7858ea4-7016-4433-bd81-c6e9870b917c', deployment_velocity_as_sole_legitimacy_criterion, empirically_contingent).
narrative_ontology:cs_axiom('d7858ea4-7016-4433-bd81-c6e9870b917c', foundational, carbon_budget_exhaustion_justifies_exclusion).
narrative_ontology:cs_axiom_status(carbon_budget_exhaustion_justifies_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('d7858ea4-7016-4433-bd81-c6e9870b917c', carbon_budget_exhaustion_justifies_exclusion, instrumental).
narrative_ontology:cs_reference_frame('d7858ea4-7016-4433-bd81-c6e9870b917c', paris_agreement_carbon_budget_framing).
narrative_ontology:cs_drift_state('d7858ea4-7016-4433-bd81-c6e9870b917c', post_global_stocktake_2023, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d7858ea4-7016-4433-bd81-c6e9870b917c', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, wind_solar_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, battery_storage_companies).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, transmission_builders).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, climate_policy_architects).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, green_finance_institutions).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_industry).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, industrial_heat_users).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, energy_intensive_manufacturing).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, rural_electric_cooperatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their technology portfolios align perfectly with the velocity primacy criterion. They receive preferential permitting, tax credits, and grid interconnection priority. Their business models scale on 2-4 year deployment cycles that fit the 2030 timeline. Exit is easy: capital redeploys to wherever policy incentives flow.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, wind_solar_developers, beneficiary,
    powerful, biographical, arbitrage, global).

% Provide the 'solution' to intermittency that makes the velocity reading internally coherent. They shape the technical standards for grid integration and benefit from mandates that treat storage as the necessary complement to variable renewables. Their exit options are wide — multiple grid services markets, behind-the-meter, utility-scale.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, battery_storage_companies, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, battery_storage_companies, agenda_setter).

% Existing plants provide baseload but new builds cannot meet 2030/2050 deployment velocity thresholds. Licensing, supply chains, and construction timelines of 10-15 years disqualify them under this reading. They bear sunk costs of maintaining expertise and supply chains while watching policy support shift to faster-deploying alternatives. Exit means abandoning specialized workforce and regulatory frameworks built over decades.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_industry, payer,
    organized, generational, constrained, national).

% Must physically balance a grid with increasing variable generation while being held to reliability standards that assume dispatchable resources. They absorb the integration costs — curtailment, ancillary services procurement, transmission upgrades — without proportional revenue recovery. Cannot exit: franchise territories, regulatory mandates, and physical infrastructure lock them in. The velocity reading externalizes intermittency management onto their balance sheets.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators, payer,
    institutional, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators, payer).

% Require high-temperature process heat (cement, steel, chemicals) that variable electricity cannot directly provide without massive overbuild and storage. The velocity reading treats electrification as the universal pathway, but their capital stock turns over on 20-30 year cycles. They face stranded asset risk if forced to electrify prematurely, or carbon pricing that makes current operations uneconomic before alternatives exist at scale.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, industrial_heat_users, payer,
    moderate, biographical, constrained, national).

% Compete internationally against jurisdictions with different legitimacy criteria. If domestic policy adopts velocity primacy exclusively, they face higher firm-level energy costs and reliability risks that foreign competitors (operating under reliability or precautionary readings) do not. Relocation is possible but carries enormous capital cost, workforce disruption, and supply chain reconstruction.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, energy_intensive_manufacturing, payer,
    moderate, biographical, constrained, global).

% Serve low-density territories where transmission build-out for remote wind/solar is cost-prohibitive per customer. They lack the rate base to absorb integration costs that urban utilities socialize. The velocity reading's transmission-heavy paradigm systematically disadvantages their topology. Cannot exit their service territories; must petition for exemptions that the reading's logic treats as obstruction.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, rural_electric_cooperatives, payer,
    moderate, biographical, constrained, regional).

% Design the NDCs, carbon budgets, and technology eligibility criteria that encode velocity primacy into law. They benefit intellectually and institutionally from a clear, measurable, near-term metric (deployment velocity) that displaces harder questions about system adequacy. Their authority derives from the carbon budget framing itself — challenging the reading challenges their mandate.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_policy_architects, agenda_setter,
    institutional, generational, analytical, global).

% Deploy capital into taxonomy-aligned assets. Velocity primacy creates a clean, investable universe: wind, solar, batteries, transmission. Nuclear, CCS, and long-duration storage fall outside or into 'transition' categories with higher risk weights. Their exit is trivial — capital flows to whatever the taxonomy rewards. They gain from the reading's simplicity and the policy certainty it appears to provide.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, green_finance_institutions, beneficiary,
    powerful, biographical, arbitrage, global).

% Argue that velocity primacy ignores tail risks: mineral extraction impacts, land use conflicts, waste streams from short-lived assets, lock-in to technologies with unresolved end-of-life problems. Their reading (precautionary) would impose slower, more deliberate deployment with full lifecycle accounting. They are structurally excluded because the carbon budget timeline treats precaution as a luxury the climate cannot afford.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, precautionary_advocates, excluded,
    organized, generational, constrained, global).

% Argue that a grid without sufficient dispatchable capacity will fail catastrophically, imposing costs that dwarf the carbon budget calculus. Their reading (reliability primacy) would require firm capacity adequacy as a legitimacy gate. They are excluded because the velocity reading treats reliability as an engineering problem solvable by overbuild and storage — a premise that remains empirically unproven at continental scale.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, reliability_advocates, excluded,
    organized, generational, constrained, global).

% Model whole-system outcomes under different legitimacy criteria. They see the velocity reading's blind spots: the integration cost curve steepens nonlinearly past ~60-70% variable penetration; the mineral intensity of overbuild+storage creates new geopolitical dependencies; the land use footprint triggers social resistance that slows deployment. They have no stake in the outcome but their analyses are systematically deprioritized in policy venues because they complicate the velocity narrative.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, system_integrators, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns global capital and policy around a single, measurable, near-term metric (deployment velocity against carbon budget) that enables rapid mobilization of renewable energy at historically unprecedented rates. Solves the coordination problem of 'which technologies get priority?' by making speed the sole criterion.
% TRANSFER_FUNCTION: Moves policy support, financing, permitting priority, and grid access from technologies with long lead times (nuclear, CCS, long-duration storage) to technologies with short lead times (wind, solar, batteries, transmission). Transfers integration costs (curtailment, ancillary services, transmission upgrades, reliability risk) from developers to grid operators and ultimately to ratepayers. Transfers industrial competitiveness risk from jurisdictions adopting broader legitimacy criteria to those adopting velocity primacy exclusively.
% ABSENT_VOICES: Communities facing mineral extraction for battery supply chains (Global South, Indigenous territories). Future generations who inherit waste streams from 15-20 year asset lifecycles. Grid reliability engineers whose professional judgment is overridden by deployment targets. Nuclear-adjacent workforces and supply chains in regions with no renewable resource adequacy. These voices are structurally excluded because the velocity reading's timeline treats their concerns as secondary to the 2030/2050 deployment imperative.
% DISAPPEARANCE_RATIONALE: If velocity primacy vanished overnight, nuclear new-build would re-enter eligibility in major taxonomies (EU, US IRA guidance), long-duration storage and CCS would receive parity in permitting and finance, grid operators would gain authority to require firm capacity contributions from all generators, and industrial heat decarbonization pathways would diversify beyond electrification. The entire policy and finance architecture built around the 2030 deployment sprint would reorganize around system adequacy and lifecycle metrics.
% FOUNDING_PROBLEM: After decades of insufficient mitigation, the remaining carbon budget for 1.5°C/2°C is so small that only technologies deployable at gigawatt-scale within 5-10 years can materially contribute. Long-lead-time technologies, whatever their theoretical merits, cannot be built fast enough to matter. The founding problem is temporal: the clock has run out on slow options.
% FOUNDING_PROBLEM_CORROBORATION: The velocity reading's founding problem is attested by IPCC AR6 WGIII (deployment speed as critical), IEA Net Zero by 2050 roadmap (2030 milestones), and the UNFCCC Global Stocktake (implementation gap). However, the status is contested because: (1) nuclear advocates (WNA, IAEA) attest that serial factory-built SMRs could achieve relevant deployment rates if policy supported them; (2) grid reliability organizations (NERC, ENTSO-E) attest that velocity without adequacy creates systemic risk that could cause cascading failures, undermining public support for the transition itself; (3) mineral supply analysts (IEA, USGS, academic) attest that the velocity pathway's mineral intensity creates new bottlenecks that may slow deployment below the required rate. Corroboration from outside the beneficiary set exists but points in multiple directions — the founding problem's parameters are genuinely disputed.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the systematic transfer of policy and financial resources from excluded technologies to velocity-aligned ones, plus the externalization of integration costs onto grid operators and ratepayers. Suppression (0.72) captures the active exclusion of nuclear from taxonomies, the permitting barriers for non-velocity technologies, and the narrative enforcement that frames reliability concerns as 'delay tactics.' Theater ratio (0.41) is substantial: the coordination function (rapid decarbonization) is real, but a growing share of enforcement activity defends the velocity boundary itself rather than achieving emissions outcomes — e.g., opposing nuclear license extensions that would preserve existing zero-carbon generation. Accessibility collapse (0.63) is moderate-high: once the carbon budget framing is accepted, alternatives appear mathematically impossible, yet resistance (0.58) remains significant from excluded stakeholders and system integrators.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (developers, finance), this is a rope: a genuine coordination mechanism that solved the 'which technology?' paralysis and unleashed record deployment. From the victim seats (grid operators, nuclear, industry), this is a snare: a criterion designed to exclude their assets while externalizing costs onto them. From the agenda_setter seat, it is a scaffold: a temporary measure justified by the 2030 deadline that they expect to evolve. The engine computes this divergence from the structural data — the authored claim (tangled_rope) captures the hybrid nature but does not adjudicate the seat-level experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Wind/solar developers, battery companies, and green finance are structural beneficiaries (d near 0.0-0.2): they collect policy rents, have arbitrage-grade exit, and their business models align with the constraint's metric. Nuclear industry, grid operators, and industrial users are structural targets (d near 0.7-0.9): they bear costs, have constrained or trapped exit, and the constraint's logic directly disqualifies their assets or business models. Climate policy architects are agenda_setters with analytical exit — they designed the metric and their authority depends on it. Precautionary and reliability advocates are excluded: their readings would change the legitimacy criteria, so they are kept out of the decision venues where velocity primacy is operationalized.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (temporal urgency of carbon budget) is live but contested. The reading's coordination function (mobilizing speed) remains partially valid — wind/solar/battery deployment has accelerated dramatically. But the extraction function has grown: integration costs now exceed LCOE advantages in many grids, reliability incidents are rising, and mineral bottlenecks are materializing. The mandate has not atrophied (the carbon budget is real) but the reading's monopoly on legitimacy has become extractive — it now suppresses technologies that could contribute to the same carbon budget on different timelines (nuclear lifetime extensions, CCS for industrial heat). The mandatrophy is unresolved: the constraint still serves its founding purpose but has accumulated extraction that the founding problem does not justify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    velocity_reliability_tradeoff,
    'At what variable penetration level does the velocity reading''s integration cost curve exceed the carbon benefit of additional deployment, making the constraint net-extractive even on its own carbon terms?',
    'Empirical observation of grids approaching 70-80% variable penetration (ERCOT, CAISO, South Australia, Germany) — tracking curtailment rates, ancillary service costs, reliability events, and marginal emissions displacement.',
    'If integration costs exceed marginal carbon benefits at achievable penetration levels, the velocity reading becomes a snare on its own terms — it extracts more carbon (via gas peakers, efficiency losses, curtailment) than it saves. This would flip the claimed_type from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(velocity_reliability_tradeoff, empirical, 'Whether the velocity reading''s coordination function remains carbon-effective at system scale.').

omega_variable(
    nuclear_velocity_potential,
    'Could serial factory-built SMRs or advanced nuclear achieve deployment velocities that satisfy the 2030/2050 carbon budget timeline, if policy support matched that given to wind/solar?',
    'Track SMR licensing timelines (NRC Part 53, Canadian CNSC VDR), supply chain maturation (BWX, Nuscale, TerraPower, Rolls-Royce), and first-of-a-kind to nth-of-a-kind cost curves. Compare to wind/solar learning rates under equivalent policy support.',
    'If nuclear can meet velocity thresholds, the victim set shrinks dramatically — the constraint''s exclusion of nuclear becomes arbitrary rather than structurally necessary. This would reduce extractiveness and suppression, potentially reclassifying toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_velocity_potential, empirical, 'Whether the nuclear victim set is structurally necessary or policy-constructed.').

omega_variable(
    suppression_mechanism_intermittency,
    'Is the suppression of reliability_primacy and precautionary readings structural (policy design, taxonomy rules, permitting barriers) or internalized (professional consensus that ''reliability concerns are fossil fuel talking points'')?',
    'Survey grid engineers, reliability coordinators, and system planners anonymously vs. publicly. Track citation networks in policy documents: are reliability studies engaged with or dismissed categorically?',
    'If suppression is internalized, the constraint''s effective suppression is higher than structural measures suggest — the target communities carry the suppression cognitively. This would increase effective extraction for grid operators and reliability advocates beyond what base_properties.suppression captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_intermittency, conceptual, 'Structural vs. internalized suppression of alternative legitimacy criteria.').

omega_variable(
    kernel_reading_boundary,
    'Does the velocity_primacy_reading logically foreclose the precautionary_reading and reliability_primacy_reading, or do they coexist as competing policy frameworks?',
    'Analyze whether any jurisdiction has formally adopted velocity primacy as the EXCLUSIVE legitimacy criterion (legally foreclosing other criteria) versus jurisdictions where multiple criteria operate in parallel (taxonomy carve-outs, reliability standards, precautionary regulations).',
    'If forecloses, the kernel has a single dominant reading and sibling readings are structurally excluded — this constraint''s classification governs the field. If coexists_with, the constraint family exhibits genuine pluralism and each reading''s extraction is modulated by the others'' presence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between velocity_primacy_reading and sibling readings of technology_legitimacy_kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t2015, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(tech_tr_t2018, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2018, 0.28).
narrative_ontology:measurement(tech_tr_t2021, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2021, 0.33).
narrative_ontology:measurement(tech_tr_t2024, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2024, 0.37).
narrative_ontology:measurement(tech_tr_t2027, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2027, 0.41).
narrative_ontology:measurement(tech_tr_t2030, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2030, 0.41).
narrative_ontology:measurement(tech_tr_t2035, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2035, 0.41).

% Extraction over time
narrative_ontology:measurement(tech_be_t2015, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(tech_be_t2018, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2018, 0.48).
narrative_ontology:measurement(tech_be_t2021, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2021, 0.56).
narrative_ontology:measurement(tech_be_t2024, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2024, 0.63).
narrative_ontology:measurement(tech_be_t2027, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2027, 0.68).
narrative_ontology:measurement(tech_be_t2030, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement(tech_be_t2035, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2035, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t2015, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(tech_su_t2018, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2018, 0.52).
narrative_ontology:measurement(tech_su_t2021, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2021, 0.61).
narrative_ontology:measurement(tech_su_t2024, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2024, 0.68).
narrative_ontology:measurement(tech_su_t2027, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2027, 0.72).
narrative_ontology:measurement(tech_su_t2030, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2030, 0.72).
narrative_ontology:measurement(tech_su_t2035, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2035, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__velocity_primacy_reading, 0.18).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel__precautionary_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, eu_taxonomy_nuclear_exclusion).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, ira_technology_neutral_credits).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, grid_reliability_standards_nerc).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, industrial_heat_decarbonization_policy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the technology_legitimacy_kernel. The velocity_primacy_reading makes deployment speed the sole legitimacy gate, benefiting fast-deploying renewables and storage while extracting from nuclear, grid operators, and industrial heat users. The precautionary_reading makes bounded reversibility the gate, which would exclude short-lived assets with unresolved waste streams. The reliability_primacy_reading makes firm capacity the gate, which would exclude variable generators without adequacy commitments. All three readings contest the same policy venues and financial taxonomies. The ε values differ substantially: velocity reading ε=0.68 (high extraction from excluded technologies and externalized integration costs), precautionary reading ε≈0.35 (moderate extraction from slower deployment), reliability reading ε≈0.45 (moderate extraction from overbuild requirements). They are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__velocity_primacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(technology_legitimacy_kernel__velocity_primacy_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
