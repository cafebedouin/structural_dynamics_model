% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Power Necessity for Decarbonization
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'baseload necessity' reading of climate
 *   mitigation legitimacy, arguing that reliable decarbonization requires
 *   dispatchable baseload power that renewables alone cannot provide at
 *   scale. This framing prioritizes nuclear power and fossil fuels with
 *   carbon capture and storage (CCS) as essential components of the energy
 *   transition, often at the expense of accelerated renewable energy
 *   deployment. The constraint is claimed as a Tangled Rope because it
 *   presents a genuine coordination problem (grid stability) but also enables
 *   significant extraction by industries that benefit from this specific
 *   technological pathway.
 *
 * KEY AGENTS:
 *   - nuclear_industry: Primary beneficiary (institutional/constrained)
 *   - fossil_fuel_industry_with_ccs: Secondary beneficiary (institutional/constrained)
 *   - grid_operators: Agenda setter (institutional/constrained)
 *   - renewable_energy_advocates: Primary payer (organized/constrained)
 *   - environmental_activists: Secondary payer (organized/identity_locked)
 *   - taxpayers: Diffuse payer (powerless/trapped)
 *   - climate_scientists: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.7).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Power Necessity for Decarbonization").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, 'bfa87e19-ae37-4735-a3d3-accad1834cfa').
narrative_ontology:cs_kernel_codification('bfa87e19-ae37-4735-a3d3-accad1834cfa', distributed).
narrative_ontology:cs_authority_grounding('bfa87e19-ae37-4735-a3d3-accad1834cfa', extraction).
narrative_ontology:cs_interpretation_layer_present('bfa87e19-ae37-4735-a3d3-accad1834cfa').
narrative_ontology:cs_reading_relation('bfa87e19-ae37-4735-a3d3-accad1834cfa', climate_mitigation_legitimacy__renewable_primacy_reading, influences).
narrative_ontology:cs_reading_relation('bfa87e19-ae37-4735-a3d3-accad1834cfa', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_reading_relation('bfa87e19-ae37-4735-a3d3-accad1834cfa', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('bfa87e19-ae37-4735-a3d3-accad1834cfa', foundational, grid_stability_requires_dispatchable_baseload).
narrative_ontology:cs_axiom_status(grid_stability_requires_dispatchable_baseload, holdable).
narrative_ontology:cs_axiom_grounding('bfa87e19-ae37-4735-a3d3-accad1834cfa', grid_stability_requires_dispatchable_baseload, empirically_contingent).
narrative_ontology:cs_axiom('bfa87e19-ae37-4735-a3d3-accad1834cfa', foundational, renewables_alone_cannot_meet_scale_and_reliability).
narrative_ontology:cs_axiom_status(renewables_alone_cannot_meet_scale_and_reliability, holdable).
narrative_ontology:cs_axiom_grounding('bfa87e19-ae37-4735-a3d3-accad1834cfa', renewables_alone_cannot_meet_scale_and_reliability, empirically_contingent).
narrative_ontology:cs_reference_frame('bfa87e19-ae37-4735-a3d3-accad1834cfa', traditional_grid_stability_paradigm).
narrative_ontology:cs_drift_state('bfa87e19-ae37-4735-a3d3-accad1834cfa', contemporary_renewable_advances_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bfa87e19-ae37-4735-a3d3-accad1834cfa', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, fossil_fuel_industry_with_ccs).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, environmental_activists).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the framing that nuclear power is indispensable for reliable decarbonization, securing investment and policy support for long-lived, capital-intensive projects. Their continued operation and expansion are justified by this necessity.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry, beneficiary,
    institutional, generational, constrained, global).

% Benefits by extending the lifespan of fossil fuel assets through carbon capture and storage (CCS) technologies, which are presented as a dispatchable baseload solution. This framing delays a full transition to renewables.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, fossil_fuel_industry_with_ccs, beneficiary,
    institutional, generational, constrained, global).

% Are responsible for grid stability and reliability. They advocate for dispatchable baseload power, often favoring existing or proven technologies, due to the perceived challenges of integrating high levels of intermittent renewables without significant storage or backup.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the cost of policy and investment diverted from renewable energy and storage solutions. Their proposed pathways are often dismissed as insufficient or unreliable, leading to slower deployment and reduced market share for renewables.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_advocates, payer,
    organized, generational, constrained, global).

% Oppose nuclear power due to waste and safety concerns, and CCS as a false solution that prolongs fossil fuel use. They bear the cost of policy decisions that prioritize these technologies over a rapid, full transition to renewables.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, environmental_activists, payer,
    organized, generational, identity_locked, global).

% Subsidize the construction and maintenance of large-scale baseload power plants (nuclear, CCS-equipped fossil fuels) through government funding, loan guarantees, and other incentives, often without direct input on technology choices.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, taxpayers, payer,
    powerless, biographical, trapped, national).

% Provide scientific assessments of climate change and decarbonization pathways. Their role is to analyze the technical feasibility and implications of various energy mixes, often finding themselves in a position of explaining complex trade-offs to policymakers.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national and global energy policy around a perceived requirement for stable, dispatchable power sources to ensure grid reliability during decarbonization, preventing blackouts and economic disruption.
% TRANSFER_FUNCTION: Transfers public and private investment towards capital-intensive, long-lived baseload power infrastructure (nuclear, fossil fuels with CCS) and away from potentially faster-deploying, but intermittent, renewable energy sources.
% ABSENT_VOICES: Advocates for decentralized energy systems, microgrids, and demand-side management are often marginalized in discussions dominated by large-scale generation. They would argue for a more flexible, distributed approach to grid stability.
% DISAPPEARANCE_RATIONALE: If the belief in baseload necessity vanished, energy policy would rapidly shift towards accelerated renewable deployment and storage solutions. Investment in nuclear and CCS would halt, and grid planning would be fundamentally reoriented, leading to a significant rearrangement of energy markets and infrastructure.
% FOUNDING_PROBLEM: The challenge of maintaining grid stability and energy security while transitioning away from fossil fuels, given the intermittency of many renewable sources.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and energy security agencies consistently attest to the live nature of the problem, citing technical challenges and geopolitical risks. While renewable advocates propose solutions, the perceived risk of instability remains a widely corroborated concern among those responsible for grid management.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is driven by the high capital costs and long lead times of baseload projects, which often require public subsidies and create long-term lock-in. Suppression (0.70) is high due to the active political and regulatory efforts to promote these technologies and to downplay the potential of renewable-plus-storage solutions. The theater ratio (0.20) is relatively low, as the concern for grid stability is genuine, but there's a performative aspect in consistently framing renewables as inherently insufficient without acknowledging rapid technological advancements. The metrics show a trend of increasing extractiveness and suppression as the debate intensifies and more capital is committed to baseload solutions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the nuclear and fossil fuel industries, this constraint is a necessary Rope, ensuring energy security and a stable transition. For renewable advocates and environmental activists, it operates as a Snare, diverting resources and legitimizing technologies they view as problematic or unnecessary. Grid operators see it as a pragmatic necessity for their mandate. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear and fossil fuel industries are clear beneficiaries, as the constraint directs investment and policy support towards their technologies. Grid operators, while having a genuine coordination function, also benefit from maintaining a familiar, dispatchable energy mix. Renewable advocates, environmental activists, and taxpayers are payers, bearing the costs of delayed renewable deployment, environmental risks, and subsidies for baseload projects. The 'identity_locked' exit for environmental activists reflects their deep ideological commitment against nuclear and fossil fuels, making exit from the debate unthinkable.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (as claimed by some beneficiaries) by highlighting the significant extraction and suppression involved. It also avoids mislabeling it as a pure Snare by acknowledging the genuine, albeit contested, coordination problem of grid stability. The 'contested' status of the founding problem further underscores the ongoing debate about whether the original problem (grid instability from renewables) is still as severe as claimed, or if the constraint has drifted towards rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseload_necessity_empirical_validity,
    'Is the claim that renewables cannot provide dispatchable baseload power at scale empirically valid, considering advancements in storage and grid management?',
    'Long-term empirical data from grids with high renewable penetration and advanced storage, coupled with independent modeling of future grid architectures.',
    'If proven invalid, the constraint''s extractiveness and suppression would be reclassified as pure rent-seeking, shifting it towards a Snare. If validated, the coordination function would be strengthened, potentially moving it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity_empirical_validity, empirical, 'Empirical validity of the core claim regarding renewable energy limitations.').

omega_variable(
    cost_benefit_analysis_of_alternatives,
    'What is the true societal cost-benefit of baseload-centric decarbonization pathways (nuclear, CCS) versus renewable-plus-storage pathways, accounting for externalities and long-term risks?',
    'Comprehensive, independent lifecycle assessment and economic modeling comparing all pathways, including social and environmental externalities (e.g., nuclear waste, CCS leakage risks).',
    'If baseload pathways are found to be significantly more costly or risky, the justification for their promotion would erode, increasing the perceived extractiveness. If found superior, the constraint''s legitimacy would increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_analysis_of_alternatives, preference, 'Societal cost-benefit comparison of different decarbonization technology mixes.').

omega_variable(
    framing_of_grid_reliability_risk,
    'To what extent is the emphasis on ''baseload necessity'' a genuine technical requirement versus a rhetorical framing that benefits incumbent industries?',
    'Analysis of policy advocacy by different industry groups, media framing, and expert consensus shifts over time, particularly in response to new technological developments.',
    'If primarily rhetorical, the theater_ratio would increase significantly, and the constraint would lean more towards a Piton or Snare. If primarily technical, the coordination function would be more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_of_grid_reliability_risk, conceptual, 'Distinguishing technical necessity from rhetorical framing in energy policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(clim_tr_t2008, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(clim_tr_t2016, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2016, 0.18).
narrative_ontology:measurement(clim_tr_t2024, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(clim_be_t2008, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2008, 0.55).
narrative_ontology:measurement(clim_be_t2016, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2016, 0.6).
narrative_ontology:measurement(clim_be_t2024, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(clim_su_t2008, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement(clim_su_t2016, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2016, 0.65).
narrative_ontology:measurement(clim_su_t2024, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_mitigation_legitimacy' kernel, focusing on the necessity of dispatchable baseload power. It is linked to sibling readings that propose alternative decarbonization pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
