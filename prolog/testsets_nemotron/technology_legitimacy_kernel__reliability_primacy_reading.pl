% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__reliability_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Reliability Primacy Legitimacy Constraint on Climate Mitigation Technologies
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The reliability-primacy reading of the technology legitimacy kernel
 *   asserts that only dispatchable, baseload-capable generation qualifies as
 *   legitimate climate mitigation. This reading dominates grid planning,
 *   capacity markets, and international energy finance. It creates a
 *   structural beneficiary set (nuclear, gas, existing baseload) and a victim
 *   set (ratepayers, VRE developers, developing-country grids) by defining
 *   legitimacy through a technical criterion that correlates with incumbent
 *   asset characteristics. The constraint is a tangled rope: it solves a
 *   genuine coordination problem (resource adequacy) while extracting rents
 *   for incumbents and imposing costly qualification barriers on challengers.
 *   Its persistence requires active enforcement through capacity market
 *   rules, interconnection standards, and finance conditionality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.72).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.68).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Reliability Primacy Legitimacy Constraint on Climate Mitigation Technologies").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, '9614f461-9b09-4d43-af5c-e0ce5753ebf6').
narrative_ontology:cs_kernel_codification('9614f461-9b09-4d43-af5c-e0ce5753ebf6', implicit).
narrative_ontology:cs_authority_grounding('9614f461-9b09-4d43-af5c-e0ce5753ebf6', practice).
narrative_ontology:cs_interpretation_layer_present('9614f461-9b09-4d43-af5c-e0ce5753ebf6').
narrative_ontology:cs_reading_relation('9614f461-9b09-4d43-af5c-e0ce5753ebf6', technology_legitimacy_kernel__velocity_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('9614f461-9b09-4d43-af5c-e0ce5753ebf6', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('9614f461-9b09-4d43-af5c-e0ce5753ebf6', foundational, dispatchability_as_necessary_condition_for_legitimacy).
narrative_ontology:cs_axiom_status(dispatchability_as_necessary_condition_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9614f461-9b09-4d43-af5c-e0ce5753ebf6', dispatchability_as_necessary_condition_for_legitimacy, conventional).
narrative_ontology:cs_axiom('9614f461-9b09-4d43-af5c-e0ce5753ebf6', foundational, grid_stability_as_supreme_mitigation_criterion).
narrative_ontology:cs_axiom_status(grid_stability_as_supreme_mitigation_criterion, holdable).
narrative_ontology:cs_axiom_grounding('9614f461-9b09-4d43-af5c-e0ce5753ebf6', grid_stability_as_supreme_mitigation_criterion, instrumental).
narrative_ontology:cs_reference_frame('9614f461-9b09-4d43-af5c-e0ce5753ebf6', firm_capacity_paradigm).
narrative_ontology:cs_drift_state('9614f461-9b09-4d43-af5c-e0ce5753ebf6', post_inverter_based_resource_maturity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9614f461-9b09-4d43-af5c-e0ce5753ebf6', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, gas_turbine_manufacturers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, baseload_plant_owners).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, solar_wind_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, storage_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, developing_country_grids).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, storage_developers).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, grid_stability_requires_dispatchable_capacity).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, intermittency_penalty_justifies_baseload_preference).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, reliability_as_supreme_public_good).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nuclear plants provide high-capacity-factor, dispatchable generation that automatically satisfies the legitimacy criterion. The constraint channels policy support, subsidies, and streamlined licensing toward nuclear new-build and life extensions, creating a structural revenue advantage over non-qualifying technologies.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Combined-cycle and peaker gas turbines provide dispatchable capacity that qualifies under the constraint. Manufacturers benefit from sustained demand for flexible fossil capacity as a 'bridge' or 'firming' complement to renewables, locking in fossil infrastructure.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, gas_turbine_manufacturers, beneficiary,
    powerful, biographical, mobile, global).

% System operators (ISOs, TSOs) define and enforce reliability standards (LOLE, SAIDI, reserve margins) that operationalize the constraint. They control interconnection queues, capacity markets, and resource adequacy filings — effectively deciding which technologies count as 'legitimate' for meeting reliability obligations.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators, agenda_setter,
    institutional, generational, constrained, regional).

% Owners of existing coal, nuclear, and hydro baseload plants receive capacity payments and reliability-must-run designations that subsidize uneconomic units. The constraint transforms reliability requirements into a revenue floor for incumbent assets.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, baseload_plant_owners, beneficiary,
    organized, biographical, constrained, national).

% Residential, commercial, and industrial electricity customers bear the full cost of reliability procurement (capacity payments, ancillary services, uplift charges) through retail rates. They have no meaningful exit from the grid and no voice in reliability standard-setting; cost increases are passed through automatically.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers, payer,
    powerless, biographical, trapped, local).

% Developers of variable renewable energy (VRE) must pair projects with storage or firming contracts to qualify as 'legitimate' under the constraint, adding $20–40/MWh to levelized cost. They face interconnection delays, curtailment risk, and market designs that undervalue energy-only contributions — the constraint structurally disadvantages their core product.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, solar_wind_developers, payer,
    moderate, biographical, constrained, global).

% Battery and long-duration storage developers face a dual position: the constraint creates demand for their product (beneficiary) but only as a costly adder to make VRE 'legitimate' (payer). They are locked into a derivative market whose size is dictated by the reliability standard, not by standalone economics.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, storage_developers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__reliability_primacy_reading, storage_developers, beneficiary).

% Grids in emerging economies face the constraint's strictest form: international finance (World Bank, MDBs) ties funding to 'reliable' generation, which in practice means fossil or large hydro. They cannot afford the storage overbuild required to make VRE qualify, locking them into capital-intensive, high-emission pathways.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, developing_country_grids, payer,
    powerless, generational, trapped, regional).

% Analysts model system costs under different legitimacy criteria. They observe that the reliability-primacy framing produces higher cumulative system cost and slower decarbonization than velocity-primacy or precautionary framings, but their analysis is advisory — they do not set standards.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures electricity system reliability by defining a clear, enforceable standard for what counts as 'firm' capacity, solving the coordination problem of resource adequacy across generators, grids, and regulators.
% TRANSFER_FUNCTION: Transfers wealth from ratepayers and VRE/storage developers to nuclear, gas, and baseload plant owners via capacity payments, reliability-must-run contracts, and the implicit subsidy of qualifying technologies in interconnection queues and market designs.
% ABSENT_VOICES: Energy-poor households in developing countries (would object to capital-cost barriers), future generations (bear climate risk from delayed decarbonization), distributed energy resource aggregators (excluded from capacity markets), and communities near fossil infrastructure (bear local pollution from 'reliable' generation).
% DISAPPEARANCE_RATIONALE: If the constraint vanished, capacity markets would shift to technology-neutral reliability metrics (e.g., ELCC, LOLE contributions), VRE+storage would compete directly with thermal firm capacity on cost, interconnection queues would clear faster, and developing-country finance would unlock for least-cost decarbonization portfolios. The entire institutional architecture of 'firm capacity' qualification would reorganize.
% FOUNDING_PROBLEM: Post-1970s grid reliability crises (Northeast blackout 1965, New York 1977) created a regulatory paradigm where 'firm capacity' was the sole metric of system adequacy. The constraint was built to prevent blackouts by ensuring sufficient dispatchable generation online at peak.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and reliability councils (NERC, regional ISOs) attest the problem remains live — extreme weather and load growth create new adequacy risks. Independent system analysts (NREL, IEA, academic centers) and climate finance institutions attest the founding problem has mutated: reliability is now achievable with inverter-based resources + storage + demand response, and the original paradigm now impedes the deeper problem (decarbonization). No corroborating source outside the beneficiary set supports the claim that the *original* framing remains adequate.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint channels vast financial flows (capacity payments, subsidized licensing, finance access) to qualifying technologies while imposing qualification costs ($20-40/MWh storage adder, interconnection delays) on non-qualifying ones. Suppression (0.68) is substantial because the constraint is enforced through mandatory reliability standards, FERC/NERC rules, and MDB loan conditions — alternatives (VRE-only portfolios) are not merely discouraged but structurally excluded from 'legitimate' status. Theater ratio (0.42) is moderate and rising: reliability coordination is real, but a growing share of enforcement defends the *specific technical form* of firm capacity rather than the reliability outcome itself. Accessibility collapse (0.65) reflects that once the reliability-primacy frame is accepted, alternative framings (ELCC, weather-dependent capacity) appear technically incoherent to practitioners. Resistance (0.58) is significant from VRE industry, consumer advocates, and climate finance reformers, but fragmented across jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (grid operators) experiences the constraint as necessary coordination — without firm capacity definitions, resource adequacy cannot be enforced. Beneficiaries (nuclear, gas) experience it as justified value recognition — their technical attributes *are* reliability. Payers (ratepayers, VRE developers, developing grids) experience it as extraction — they pay for a reliability standard that could be met more cheaply with technology-neutral metrics. The engine will compute this divergence from the structural data; the claimed_type (tangled_rope) reflects the author's assessment that both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Grid operators (agenda_setter, institutional) sit near d=0.2 — they administer the constraint and benefit from its authority but also bear operational accountability. Nuclear/gas/baseload owners (beneficiaries, institutional/organized) sit near d=0.1-0.15 — they collect concentrated rents. Ratepayers (payer, powerless, trapped) sit near d=0.95 — they pay all costs with zero exit. VRE/storage developers (payer, moderate, constrained) sit near d=0.7 — they bear qualification costs but retain some market access. Developing-country grids (payer, powerless, trapped) sit near d=0.9 — they face the constraint as an external conditionality with no negotiating power. Analysts (observer, analytical) sit at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing blackouts via firm capacity) was live in 1970. By 2020, inverter-based resources + storage + demand response can provide equivalent reliability at lower cost and emissions — the founding problem is *technically* solved but *institutionally* alive. The constraint persists because the institutional architecture (capacity markets, reliability standards, finance rules) was built around the firm-capacity paradigm and its beneficiaries (incumbents, operators) control the revision process. This is mandatrophy: the mandate (reliability) has outlived its original technical justification (only thermal/hydro can provide it) but the constraint persists through institutional inertia and beneficiary capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reliability_metric_adequacy,
    'Do current reliability metrics (LOLE, SAIDI, reserve margins) adequately capture the reliability contribution of inverter-based resources, storage, and demand response, or do they systematically undervalue non-synchronous resources?',
    'Empirical validation through grid-forming inverter demonstrations, ELCC studies for VRE+storage hybrids, and operational data from high-VRE grids (South Australia, California, Germany).',
    'If metrics are adequate, the constraint''s coordination function is genuine and its extraction is the price of a necessary standard. If metrics systematically undervalue non-synchronous resources, the constraint''s coordination story is a cover for extracting rents from incumbent technical forms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliability_metric_adequacy, empirical, 'Whether the constraint''s coordination metric is technically sound or structurally biased').

omega_variable(
    committer_frame_disagreement_location,
    'The kernel''s three readings disagree on which structural element defines legitimacy: dispatchability (this reading), deployment speed (velocity), or failure reversibility (precautionary). Where exactly is the disagreement located in the commitment structure?',
    'Trace each reading''s axioms to their grounding: this reading grounds in grid physics + institutional continuity (conventional), velocity in carbon budget arithmetic (empirically_contingent), precautionary in intergenerational ethics (deontological). The disagreement is located in the authority_grounding layer — each reading invokes a different warrant for the kernel.',
    'If the disagreement is at the grounding layer, no amount of technical evidence (ELCC studies, cost curves) can resolve it — the readings operate in different epistemic registers. This makes the kernel a permanent site of contestation, not a resolvable technical dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_disagreement_location, conceptual, 'Structural location of the kernel''s reading-level disagreement').

omega_variable(
    developing_country_lockin_mechanism,
    'Is the constraint''s application to developing-country finance (MDB ''reliable generation'' conditionality) a necessary safeguard or an extractive barrier that transfers wealth from poor grids to rich-country technology exporters?',
    'Counterfactual analysis: compare least-cost decarbonization pathways for 10+ developing grids under current MDB rules vs. technology-neutral reliability metrics. Track capital cost differential and emissions outcomes.',
    'If the constraint adds >15% to system cost and delays decarbonization >5 years for majority of analyzed grids, it functions as a wealth transfer from ratepayers in poor countries to nuclear/gas exporters in rich countries — strengthening the snare characterization for that stakeholder subset.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(developing_country_lockin_mechanism, empirical, 'Whether the constraint''s international finance dimension is coordination or extraction').

omega_variable(
    storage_derivative_market_capture,
    'Does the constraint create a genuine market for storage, or does it capture storage as a derivative adjunct to VRE — preventing storage from competing as standalone firm capacity?',
    'Analyze capacity market clearing prices for standalone storage vs. hybrid VRE+storage. Track whether storage developers can access reliability revenue without a VRE pairing.',
    'If storage only qualifies as ''legitimate'' when paired with VRE, the constraint extracts from storage developers twice: they pay the VRE integration cost AND their standalone value is suppressed. This would shift storage_developers from dual-role toward pure payer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_derivative_market_capture, empirical, 'Whether storage qualifies independently or only as VRE enabler').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 1970, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1970, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(tech_tr_t1985, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(tech_tr_t2000, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(tech_tr_t2010, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(tech_tr_t2020, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(tech_tr_t2035, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 2035, 0.42).

% Extraction over time
narrative_ontology:measurement(tech_be_t1970, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(tech_be_t1985, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(tech_be_t2000, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(tech_be_t2010, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(tech_be_t2020, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(tech_be_t2035, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 2035, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1970, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(tech_su_t1985, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 1985, 0.4).
narrative_ontology:measurement(tech_su_t2000, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(tech_su_t2010, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(tech_su_t2020, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 2020, 0.63).
narrative_ontology:measurement(tech_su_t2035, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 2035, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__reliability_primacy_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__precautionary_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, capacity_market_design).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, mdb_energy_finance_conditionality).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, interconnection_queue_rules).

% DUAL FORMULATION NOTE:
% The technology_legitimacy_kernel decomposes into three readings with distinct ε values: reliability_primacy (ε=0.72, tangled_rope), velocity_primacy (ε≈0.35, scaffold→rope transition), precautionary (ε≈0.45, rope). This reading's high ε reflects the extraction embedded in the firm-capacity qualification regime; velocity's lower ε reflects its transitional justification (carbon budget deadline); precautionary's intermediate ε reflects its ethical grounding with some institutional enforcement. All three share the kernel_id and are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__reliability_primacy_reading, institutional, 0.18).
constraint_indexing:directionality_override(technology_legitimacy_kernel__reliability_primacy_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
