% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__systems_transition_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__systems_transition_reading
 *   human_readable: Climate Mitigation Imperative: Systems Transition Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'systems transition' reading of the
 *   climate mitigation imperative, arguing that effective climate action
 *   requires a fundamental shift towards decentralized, democratically
 *   controlled energy systems. From this perspective, technologies like
 *   nuclear power, despite being low-carbon, perpetuate an extractive,
 *   centralized model that hinders true mitigation. The constraint itself is
 *   the governance structure and policy environment that favors centralized
 *   energy, actively suppressing alternatives. The claimed type is
 *   'tangled_rope' because it presents a coordination function (climate
 *   mitigation) but operates with significant asymmetric extraction and
 *   requires active enforcement to maintain the centralized status quo.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.75).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Climate Mitigation Imperative: Systems Transition Reading").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, 'e248d7b9-0f14-494e-a1dd-fc53726ddc27').
narrative_ontology:cs_kernel_codification('e248d7b9-0f14-494e-a1dd-fc53726ddc27', distributed).
narrative_ontology:cs_authority_grounding('e248d7b9-0f14-494e-a1dd-fc53726ddc27', extraction).
narrative_ontology:cs_interpretation_layer_present('e248d7b9-0f14-494e-a1dd-fc53726ddc27').
narrative_ontology:cs_reading_relation('e248d7b9-0f14-494e-a1dd-fc53726ddc27', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('e248d7b9-0f14-494e-a1dd-fc53726ddc27', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_axiom('e248d7b9-0f14-494e-a1dd-fc53726ddc27', foundational, energy_systems_must_be_democratically_controlled).
narrative_ontology:cs_axiom_status(energy_systems_must_be_democratically_controlled, holdable).
narrative_ontology:cs_axiom_grounding('e248d7b9-0f14-494e-a1dd-fc53726ddc27', energy_systems_must_be_democratically_controlled, deontological).
narrative_ontology:cs_axiom('e248d7b9-0f14-494e-a1dd-fc53726ddc27', foundational, centralized_energy_perpetuates_extraction).
narrative_ontology:cs_axiom_status(centralized_energy_perpetuates_extraction, holdable).
narrative_ontology:cs_axiom_grounding('e248d7b9-0f14-494e-a1dd-fc53726ddc27', centralized_energy_perpetuates_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('e248d7b9-0f14-494e-a1dd-fc53726ddc27', decentralized_democratic_energy_future).
narrative_ontology:cs_drift_state('e248d7b9-0f14-494e-a1dd-fc53726ddc27', contemporary_energy_policy_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e248d7b9-0f14-494e-a1dd-fc53726ddc27', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, centralized_utility_companies).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, nuclear_industry_lobby).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, local_communities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, energy_democracy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the perpetuation of centralized energy infrastructure, which delays a true systems transition and allows them to continue operating existing assets, often with political influence that shapes energy policy.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, fossil_fuel_incumbents, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from large-scale, capital-intensive energy projects like nuclear, which fit their existing business models and regulatory frameworks, resisting a shift to decentralized, democratically controlled grids.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, centralized_utility_companies, beneficiary,
    institutional, generational, constrained, national).

% Actively promotes nuclear power as a climate solution, securing subsidies and regulatory pathways that favor large-scale, centralized projects, thereby perpetuating a system that is antithetical to a democratic energy transition.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_industry_lobby, agenda_setter,
    organized, biographical, constrained, national).

% Bear the environmental and social costs of centralized energy production (e.g., waste disposal, land use, lack of control over energy sources) and are excluded from decision-making processes regarding energy infrastructure.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, local_communities, payer,
    powerless, generational, trapped, local).

% Face systemic barriers (e.g., grid access, regulatory hurdles, financial incentives favoring incumbents) that suppress the growth of decentralized, democratically controlled renewable energy projects, despite their alignment with a systems transition.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_developers, payer,
    moderate, biographical, constrained, regional).

% Actively campaign for policies that promote decentralized, community-owned renewable energy and democratic control over energy systems. They bear the costs of political struggle against entrenched interests and face suppression of their policy alternatives.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_democracy_advocates, payer,
    organized, generational, constrained, national).

% Provide scientific consensus on climate change and the need for mitigation, but their input is interpreted differently by various stakeholders, and they do not directly control policy or infrastructure decisions.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates the continued operation and development of large-scale, centralized energy infrastructure, ensuring grid stability and power delivery through established utility models, while ostensibly contributing to climate mitigation.
% TRANSFER_FUNCTION: Transfers political influence, financial resources, and long-term control over energy infrastructure from local communities and distributed energy initiatives to centralized utility companies and the nuclear industry, under the guise of climate action.
% ABSENT_VOICES: Indigenous communities, environmental justice groups, and grassroots energy cooperatives are often marginalized or excluded from energy planning and policy discussions, despite bearing disproportionate impacts and offering alternative models for energy governance.
% DISAPPEARANCE_RATIONALE: If the imperative to perpetuate centralized, extractive energy systems for climate mitigation vanished, there would be a rapid acceleration of decentralized, democratically controlled renewable energy projects. Investment would shift, regulatory barriers would fall, and local communities would gain significant agency over their energy futures, fundamentally reorganizing the energy landscape.
% FOUNDING_PROBLEM: The urgent need to decarbonize energy systems to address climate change, coupled with the perceived necessity of maintaining grid stability and large-scale power generation capacity.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and international bodies (e.g., IPCC) corroborate the live status of the climate mitigation problem. Energy democracy advocates and local communities corroborate the need for systems transformation, while centralized utilities and nuclear proponents corroborate the need for large-scale, stable power sources.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__systems_transition_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__systems_transition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__systems_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the current energy system, perpetuated by this constraint, extracts wealth and control from communities and distributed developers. Suppression (0.75) is also high, reflecting the active political and regulatory efforts to maintain centralized control and block decentralized alternatives. Theater ratio (0.20) is moderate, as some efforts genuinely aim for climate mitigation, but a significant portion of the 'mitigation' narrative serves to justify the existing power structures. Accessibility collapse is moderate (0.45) because alternatives exist but are heavily constrained, leading to high resistance (0.70) from those advocating for a systems transition.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries (centralized energy actors) perceive this as a necessary coordination mechanism for reliable energy and climate action, while the victims (communities, distributed developers) experience it as an extractive force that actively suppresses a more equitable and effective mitigation pathway. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil fuel incumbents, centralized utilities, and the nuclear industry are beneficiaries, as the constraint allows them to maintain or expand their existing business models. Local communities, distributed renewable developers, and energy democracy advocates are victims, bearing the costs of centralized control and suppressed alternatives. Climate scientists act as observers, providing data that is then interpreted through these competing frames.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    centralization_necessity_ambiguity,
    'Is centralized energy infrastructure (including nuclear) a structural necessity for grid stability and reliable baseload power, or can a fully decentralized system provide equivalent reliability?',
    'Empirical studies of advanced decentralized grid architectures, microgrids, and energy storage solutions at scale; comparative analysis of grid resilience in regions with varying degrees of centralization.',
    'If centralization is proven unnecessary for reliability, the justification for perpetuating extractive centralized systems weakens significantly, reclassifying the constraint towards a snare. If proven necessary, the coordination function of centralized systems would be re-emphasized, potentially shifting it towards a tangled rope with lower extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(centralization_necessity_ambiguity, empirical, 'Ambiguity regarding the structural necessity of centralized energy systems for grid stability.').

omega_variable(
    democratic_control_definition_ambiguity,
    'What constitutes ''democratic control'' in energy systems, and is it achievable within existing regulatory and market structures, or does it require fundamental institutional overhaul?',
    'Comparative political science research on energy governance models, case studies of successful community energy projects, and analysis of policy pathways for increasing local ownership and decision-making power.',
    'If ''democratic control'' is defined narrowly and found achievable within existing structures, the perceived suppression of alternatives might decrease. If it requires radical overhaul, the current system''s extractiveness and suppression would be further highlighted, pushing the classification towards a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_control_definition_ambiguity, conceptual, 'Ambiguity in the definition and feasibility of ''democratic control'' in energy systems.').

omega_variable(
    nuclear_role_in_transition_ambiguity,
    'Is nuclear power a transitional technology that can accelerate decarbonization while a full systems transition to decentralized renewables is underway, or does its long-term nature inherently lock in centralization?',
    'Analysis of nuclear project timelines, capital costs, and operational lifespans compared to the pace of renewable deployment and grid modernization; assessment of ''small modular reactor'' (SMR) potential for decentralization.',
    'If nuclear is found to be a viable transitional technology that does not impede decentralization, its inclusion in a mitigation portfolio might be seen as less extractive. If it locks in centralization, its role as a victim-creating technology within this reading would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_role_in_transition_ambiguity, empirical, 'Ambiguity regarding nuclear power''s role as a transitional vs. perpetuating technology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__opportunity_cost_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'climate_mitigation_imperative' kernel. This 'systems_transition_reading' emphasizes decentralized, democratic control, contrasting with the 'portfolio_optimization_reading' (maximizing all low-carbon sources) and the 'opportunity_cost_reading' (nuclear's capital intensity makes it net-harmful).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
