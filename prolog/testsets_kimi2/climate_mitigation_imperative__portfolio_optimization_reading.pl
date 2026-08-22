% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__portfolio_optimization_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Climate Mitigation Imperative â Portfolio Optimization Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The portfolio optimization reading of the climate mitigation imperative
 *   treats decarbonization as an energy-systems coordination problem
 *   requiring the simultaneous maximization of all low-carbon sources, with
 *   nuclear power designated as necessary for reliable baseload. This
 *   constraint operates through technology-neutral carbon intensity standards
 *   that formally include all low-carbon options while structurally
 *   channeling public subsidy and grid priority to nuclear and renewable
 *   developers. Fossil fuel incumbents bear the costs of displacement and
 *   stranded assets. The constraint is a contested reading of the broader
 *   mitigation kernel; two sibling readings dispute its empirical and
 *   normative premises. This story authors the structural data for the
 *   portfolio optimization reading alone, with metrics and claim authored
 *   independently.
 *
 * KEY AGENTS:
 *   - nuclear_industry: Primary beneficiary (powerful/constrained) â receives subsidy and baseload guarantees.
 *   - fossil_fuel_incumbents: Primary target/payer (institutional/constrained) â bears stranded asset extraction and regulatory exclusion.
 *   - renewable_energy_sector: Secondary beneficiary (organized/mobile) â gains from low-carbon mandate but competes with nuclear.
 *   - energy_regulators: Agenda setter (institutional/constrained) â administers carbon standards and procurement rules.
 *   - climate_science_institutions: Analytical observer (institutional/analytical) â supplies the scenarios that justify the imperative.
 *   - distributed_energy_advocates: Excluded voice (moderate/constrained) â absent from centralized baseload planning.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.62).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Climate Mitigation Imperative â Portfolio Optimization Reading").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, 'a39a2e1d-4416-4585-bbbb-437b3259df4d').
narrative_ontology:cs_kernel_codification('a39a2e1d-4416-4585-bbbb-437b3259df4d', formalized).
narrative_ontology:cs_authority_grounding('a39a2e1d-4416-4585-bbbb-437b3259df4d', expertise).
narrative_ontology:cs_interpretation_layer_present('a39a2e1d-4416-4585-bbbb-437b3259df4d').
narrative_ontology:cs_reading_relation('a39a2e1d-4416-4585-bbbb-437b3259df4d', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('a39a2e1d-4416-4585-bbbb-437b3259df4d', climate_mitigation_imperative__systems_transition_reading, influences).
narrative_ontology:cs_axiom('a39a2e1d-4416-4585-bbbb-437b3259df4d', foundational, maximize_all_low_carbon_sources).
narrative_ontology:cs_axiom_status(maximize_all_low_carbon_sources, holdable).
narrative_ontology:cs_axiom_grounding('a39a2e1d-4416-4585-bbbb-437b3259df4d', maximize_all_low_carbon_sources, instrumental).
narrative_ontology:cs_axiom('a39a2e1d-4416-4585-bbbb-437b3259df4d', foundational, nuclear_baseload_necessity).
narrative_ontology:cs_axiom_status(nuclear_baseload_necessity, holdable).
narrative_ontology:cs_axiom_grounding('a39a2e1d-4416-4585-bbbb-437b3259df4d', nuclear_baseload_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('a39a2e1d-4416-4585-bbbb-437b3259df4d', technology_neutral_low_carbon_optimization).
narrative_ontology:cs_drift_state('a39a2e1d-4416-4585-bbbb-437b3259df4d', contemporary_grid_modernization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a39a2e1d-4416-4585-bbbb-437b3259df4d', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_sector).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_incumbents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives direct public subsidy, long-term power purchase agreements, and regulatory baseload guarantees justified by the mitigation imperative. Its capital-intensive reactors require decades of policy stability to amortize, making exit from the nuclear technology path extremely costly.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry, beneficiary,
    powerful, generational, constrained, global).

% Existing coal and gas generation fleets face stranded asset risk, carbon pricing, and exclusion from future baseload provision under technology-neutral carbon standards. Exit to low-carbon operation is capital-intensive and competitively disadvantaged against subsidized nuclear and priority grid access for renewables.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_incumbents, payer,
    institutional, generational, constrained, global).

% Gains from the mandate to maximize all low-carbon sources, securing grid access, green tariffs, and investment certainty. However, competes with nuclear for subsidy pools and policy priority in integrated resource plans that treat baseload as a system requirement.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_sector, beneficiary,
    organized, biographical, mobile, global).

% Designs and enforces carbon-intensity standards, capacity markets, and technology-neutral procurement rules that operationalize the mitigation imperative. Politically and legally bound to net-zero commitments and IPCC-aligned decarbonization pathways.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, energy_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Produces the integrated assessment models and emissions budgets that frame the mitigation imperative. Does not directly benefit from the technology choice but supplies the authoritative scenarios used to justify portfolio optimization including nuclear baseload.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_science_institutions, observer,
    institutional, civilizational, analytical, global).

% Would argue for decentralized, community-owned renewable systems and demand-side flexibility as an alternative to centralized baseload nuclear, but are structurally absent from the integrated resource plans that treat grid stability as requiring large-scale generation.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, distributed_energy_advocates, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__portfolio_optimization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global energy investment toward decarbonization by establishing a technology-neutral carbon intensity floor, ensuring that all low-carbon generation sources are mobilized simultaneously to meet aggregate mitigation targets.
% TRANSFER_FUNCTION: Moves public subsidy, regulatory priority, and grid access from fossil fuel incumbents to low-carbon generators, with a disproportionate share of baseload guarantees and capital support flowing to nuclear as the designated reliable backbone.
% ABSENT_VOICES: Distributed energy advocates and community-owned renewable developers who would argue for decentralized transition are marginalized by the baseload-necessity framing; fossil fuel workers and frontline communities facing stranded assets are present in discourse but structurally excluded from technology choice.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, fossil fuel incumbents would regain baseload standing and stranded asset risk would abate, nuclear projects would lose subsidy and regulatory guarantees causing cancellations and write-downs, renewable developers would face a less certain investment climate without the carbon mandate, and national grid planners would revert to least-cost dispatch without the decarbonization imperative â the global energy mix and capital allocation would reorganize.
% FOUNDING_PROBLEM: Climate change driven by anthropogenic greenhouse gas emissions from fossil fuel combustion, requiring rapid decarbonization of the electricity sector to limit global warming.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports, national climate advisory bodies, and independent atmospheric science attest the problem from outside the energy-industry beneficiary set; fossil fuel incumbents contest the urgency but do not dispute the underlying greenhouse effect mechanism.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the policy systematically transfers resources from fossil incumbents to low-carbon generators, especially nuclear. Suppression (0.62) reflects active regulatory exclusion of high-carbon alternatives and the marginalization of non-nuclear pathways that challenge baseload necessity. Theater ratio is relatively low (0.25) because the decarbonization function is substantive, though some performative technology-neutrality masks nuclear privilege. Accessibility collapse (0.48) is incomplete: fossil alternatives collapse under carbon standards, but renewable-only alternatives remain visible. Resistance (0.60) is strong from fossil incumbents and anti-nuclear movements. The measurement series share a single time grid so all metrics are authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The nuclear industry and renewable sector experience the constraint as beneficial coordination that solves investment risk and climate compliance. Fossil fuel incumbents experience it as extractive displacement that destroys asset value. Regulators experience it as mandatory implementation of externally corroborated climate commitments. The engine should compute low directionality for beneficiaries and high directionality for fossil fuel payers, producing divergent per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries â nuclear_industry and renewable_energy_sector â receive low directionality because the constraint subsidizes, guarantees markets for, and prioritizes them. Fossil_fuel_incumbents receive high directionality because the constraint extracts via carbon pricing, phase-out mandates, and stranded asset creation. Energy_regulators sit near symmetric because they both enforce and are bound by the constraint. Excluded distributed advocates would compute high directionality because the constraint suppresses their alternative pathway.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â anthropogenic climate change â is live and externally corroborated, so the constraint is not a piton. The genuine coordination function (decarbonization) prevents mislabeling as pure extraction. However, if empirical evidence continues to challenge nuclear necessity while the policy persists unchanged, the constraint would drift toward snare: the coordination justification for nuclear support would erode while the extraction of public subsidy and fossil displacement remained. The temporal measurements show rising extractiveness but low theater, arguing against pure piton and supporting tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nuclear_necessity_empirical_status,
    'Is nuclear power actually necessary for reliable baseload in a high-renewable grid, or have storage, demand-side flexibility, and grid advances made it empirically optional?',
    'Grid-scale modeling and operational data from high-renewable power systems (e.g., 80%+ variable renewable penetration) coupled with cost trajectories for battery and demand-response deployment.',
    'If nuclear is not necessary, the constraint''s coordination justification for concentrated nuclear support collapses, and the beneficiary structure shifts toward pure extraction (snare dynamics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_necessity_empirical_status, empirical, 'Empirical contingency of the nuclear baseload necessity claim').

omega_variable(
    technology_neutrality_asymmetry,
    'Does the technology-neutral carbon intensity standard asymmetrically benefit nuclear despite its formal neutrality?',
    'Comparative subsidy flows, capacity market design, baseload remuneration rules, and regulatory treatment of intermittency across jurisdictions adopting the portfolio optimization reading.',
    'If asymmetric, the coordination function is cover for concentrated extraction to the nuclear industry; if symmetric, the portfolio optimization remains a genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_neutrality_asymmetry, conceptual, 'Whether formal technology neutrality masks structural nuclear privilege').

omega_variable(
    carbon_intensity_sufficiency,
    'Does optimizing for carbon intensity alone capture the full mitigation requirement, or does it systematically exclude transition criteria such as decentralization, democratic control, and community ownership?',
    'Comparative policy analysis of emissions outcomes versus governance and distributional outcomes in jurisdictions with centralized versus decentralized energy strategies.',
    'If carbon intensity is insufficient, the constraint is under-specified and the systems_transition reading gains structural validity; if sufficient, the portfolio boundary is legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_intensity_sufficiency, preference, 'Whether carbon intensity is the sole legitimate optimization target').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, systems_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_mitigation_imperative kernel. It shares the same referent (climate mitigation policy) with its siblings but differs in empirical premises and beneficiary structure. The Îµ-invariance principle requires separate stories because the sibling readings assign different Îµ values and different victim/beneficiary sets to the same natural-language label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
