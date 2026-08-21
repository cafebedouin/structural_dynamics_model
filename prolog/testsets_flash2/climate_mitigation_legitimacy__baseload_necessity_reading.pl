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
 *   human_readable: Baseload Power Necessity for Decarbonization (Baseload Necessity Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'baseload necessity' reading of the
 *   climate mitigation legitimacy kernel. It asserts that reliable
 *   decarbonization requires dispatchable baseload power, which renewables
 *   alone cannot provide at scale. This framing prioritizes grid stability
 *   and energy security, often leading to policy support for nuclear power
 *   and fossil fuels with carbon capture and storage (CCS), while classifying
 *   purely renewable pathways as insufficient or risky. The constraint is
 *   claimed as a 'tangled_rope' because it genuinely coordinates grid
 *   stability but also extracts resources and suppresses alternatives by
 *   framing them as unreliable.
 *
 * KEY AGENTS:
 *   - nuclear_power_industry: Primary beneficiary (institutional/constrained)
 *   - fossil_fuel_industry_with_ccs: Secondary beneficiary (institutional/constrained)
 *   - grid_operators: Agenda setter (institutional/constrained)
 *   - renewable_energy_advocates: Primary payer (organized/constrained)
 *   - environmental_activists: Secondary payer (organized/constrained)
 *   - taxpayers: Diffuse payer (moderate/trapped)
 *   - policy_makers: Agenda setter (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.45).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.6).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Power Necessity for Decarbonization (Baseload Necessity Reading)").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, '19609518-9acd-4969-b0b8-7a57e3ff1d93').
narrative_ontology:cs_kernel_codification('19609518-9acd-4969-b0b8-7a57e3ff1d93', distributed).
narrative_ontology:cs_authority_grounding('19609518-9acd-4969-b0b8-7a57e3ff1d93', expertise).
narrative_ontology:cs_interpretation_layer_present('19609518-9acd-4969-b0b8-7a57e3ff1d93').
narrative_ontology:cs_reading_relation('19609518-9acd-4969-b0b8-7a57e3ff1d93', climate_mitigation_legitimacy__renewable_primacy_reading, influences).
narrative_ontology:cs_reading_relation('19609518-9acd-4969-b0b8-7a57e3ff1d93', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_reading_relation('19609518-9acd-4969-b0b8-7a57e3ff1d93', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('19609518-9acd-4969-b0b8-7a57e3ff1d93', foundational, grid_stability_requires_dispatchable_baseload).
narrative_ontology:cs_axiom_status(grid_stability_requires_dispatchable_baseload, holdable).
narrative_ontology:cs_axiom_grounding('19609518-9acd-4969-b0b8-7a57e3ff1d93', grid_stability_requires_dispatchable_baseload, empirically_contingent).
narrative_ontology:cs_axiom('19609518-9acd-4969-b0b8-7a57e3ff1d93', foundational, renewables_alone_cannot_meet_scale_and_reliability).
narrative_ontology:cs_axiom_status(renewables_alone_cannot_meet_scale_and_reliability, holdable).
narrative_ontology:cs_axiom_grounding('19609518-9acd-4969-b0b8-7a57e3ff1d93', renewables_alone_cannot_meet_scale_and_reliability, empirically_contingent).
narrative_ontology:cs_reference_frame('19609518-9acd-4969-b0b8-7a57e3ff1d93', traditional_grid_engineering_principles).
narrative_ontology:cs_drift_state('19609518-9acd-4969-b0b8-7a57e3ff1d93', contemporary_high_renewable_penetration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('19609518-9acd-4969-b0b8-7a57e3ff1d93', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_power_industry).
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

% Benefits from the framing that nuclear power is essential for reliable decarbonization, justifying investment in new plants and extending the life of existing ones. Faces high capital costs but secures long-term revenue streams and policy support.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_power_industry, beneficiary,
    institutional, generational, constrained, global).

% Benefits from the argument that dispatchable baseload is necessary, allowing for continued operation of fossil fuel plants if carbon capture and storage (CCS) technologies are deployed, delaying full transition to renewables.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, fossil_fuel_industry_with_ccs, beneficiary,
    institutional, generational, constrained, global).

% Responsible for maintaining grid stability and reliability. They advocate for dispatchable baseload capacity to manage intermittency from renewables, influencing policy and investment decisions towards traditional power sources.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators, agenda_setter,
    institutional, immediate, constrained, national).

% Bear the cost of policy and investment diverted from purely renewable solutions. They argue that grid modernization, storage, and demand-side management can provide reliability without baseload, but face an uphill battle against established infrastructure and policy inertia.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_advocates, payer,
    organized, biographical, constrained, global).

% Oppose nuclear power due to waste and safety concerns, and CCS as a false solution that prolongs fossil fuel use. They bear the cost of policy decisions that prioritize these technologies over rapid, full renewable deployment.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, environmental_activists, payer,
    organized, generational, constrained, global).

% Fund subsidies, research, and infrastructure for baseload technologies (nuclear, CCS) through taxes, often without direct input on energy policy. Their costs are diffuse and difficult to avoid.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, taxpayers, payer,
    moderate, biographical, trapped, national).

% Influence energy policy and funding based on various inputs, including grid operator recommendations and industry lobbying. They are constrained by political cycles and the need to ensure energy security and affordability.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates energy policy and investment towards a stable, reliable electricity grid capable of meeting continuous demand, ensuring energy security during decarbonization.
% TRANSFER_FUNCTION: Transfers public funds and policy support to industries providing dispatchable baseload power (nuclear, fossil with CCS), and shifts investment away from purely renewable pathways, from taxpayers and renewable advocates to these industries.
% ABSENT_VOICES: Advocates for decentralized energy systems, advanced grid management, and aggressive demand-side reduction are often marginalized in policy discussions dominated by large-scale generation and grid stability concerns. Their models for reliability without baseload are not fully integrated into mainstream planning.
% DISAPPEARANCE_RATIONALE: If the belief in baseload necessity vanished, energy policy would rapidly shift towards accelerated renewable deployment, massive investment in storage and grid modernization, and potentially a re-evaluation of energy demand. Existing nuclear and fossil plants would face immediate decommissioning pressure, and the energy investment landscape would be fundamentally altered.
% FOUNDING_PROBLEM: The historical challenge of providing continuous, reliable electricity supply to industrial and residential consumers, especially as intermittent renewable sources began to be integrated into grids designed for centralized, dispatchable generation.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and energy security agencies consistently attest that maintaining grid stability with high renewable penetration remains a live and complex engineering challenge. While renewable advocates contest the 'necessity' of baseload, the underlying problem of balancing supply and demand is universally acknowledged.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).
:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because while there's a genuine coordination problem (grid stability), the solutions promoted by this reading often involve high capital costs and long-term commitments that lock in specific technologies, creating a transfer of wealth. Suppression (0.6) is significant as this reading actively downplays or dismisses the viability of purely renewable solutions, limiting policy and investment alternatives. Theater ratio (0.2) is low, indicating that the concern for grid reliability is genuine, though it may be leveraged to support specific industrial interests. The temporal measurements show a rise in extractiveness and suppression as the debate over decarbonization intensifies and baseload solutions gain more policy traction, before a slight dip as renewable integration challenges become better understood.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of grid operators and baseload industries, this constraint is a necessary 'rope' for energy security. From renewable advocates and environmentalists, it's a 'snare' that entrenches incumbent technologies and extracts resources while delaying true decarbonization. The engine's classification as 'tangled_rope' reflects the hybrid nature: a real coordination problem (grid stability) coupled with asymmetric extraction and suppression of alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear and fossil fuel (with CCS) industries are clear beneficiaries, as this reading justifies their continued or expanded role. Grid operators, as agenda setters, also benefit from a policy environment that supports their traditional operational models. Renewable advocates and environmental activists are payers, as their preferred solutions are de-prioritized. Taxpayers bear the diffuse costs of subsidies and infrastructure. Policy makers are agenda setters, but also constrained by the political and technical arguments presented by grid operators and industry.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by acknowledging the genuine coordination function of ensuring grid stability while simultaneously identifying the extractive elements that arise from prioritizing specific, capital-intensive baseload technologies. It avoids the trap of seeing it as a pure 'rope' (ignoring extraction) or a pure 'snare' (ignoring the real engineering challenge of grid stability). The 'contested' status of the founding problem highlights the ongoing debate about whether the original problem (reliable power) still necessitates the same solutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    renewable_storage_cost_viability,
    'Can renewable energy combined with advanced storage and grid management achieve equivalent reliability and cost-effectiveness as baseload power at scale?',
    'Large-scale, long-duration energy storage demonstrations and grid modernization projects proving technical and economic viability over multi-year periods in diverse geographies.',
    'If proven viable, the ''baseload necessity'' claim would be empirically undermined, shifting this constraint towards a ''snare'' by exposing its coordination function as cover for extraction. If not, the ''tangled_rope'' classification would be reinforced, or even shift towards ''rope'' if costs are genuinely prohibitive for alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_storage_cost_viability, empirical, 'Empirical viability and cost of renewable-only grid solutions.').

omega_variable(
    risk_tolerance_framing,
    'Is the perceived ''necessity'' of baseload power driven by an objective engineering requirement or a culturally conditioned low tolerance for grid reliability risk?',
    'Comparative analysis of energy policy and grid design in nations with different risk appetites for energy transitions, coupled with public opinion research on acceptable reliability trade-offs.',
    'If primarily cultural, the ''suppression'' metric would be re-evaluated as partly internalized, and the constraint''s ''tangled_rope'' nature would be seen as leveraging cultural norms for extraction. If purely objective, the engineering necessity would be reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(risk_tolerance_framing, conceptual, 'Cultural vs. objective drivers of baseload necessity.').

omega_variable(
    intermittency_management_technologies,
    'To what extent do emerging technologies (e.g., advanced demand response, virtual power plants, AI-driven grid optimization) reduce the need for traditional baseload capacity?',
    'Pilot programs and full-scale deployments demonstrating the effectiveness of these technologies in managing grid intermittency and reducing peak demand, with independent validation of their impact on baseload requirements.',
    'Significant impact would reduce the perceived ''accessibility_collapse'' for renewable-only pathways, weakening the justification for baseload and potentially shifting the constraint towards a ''snare'' by revealing its extractive core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermittency_management_technologies, empirical, 'Impact of new grid management technologies on baseload requirements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2030, 0.22).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2040, 0.2).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2050, 0.18).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2030, 0.45).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2040, 0.48).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2050, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2030, 0.6).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2040, 0.62).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2050, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_waste_disposal_liability).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, carbon_capture_subsidies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
