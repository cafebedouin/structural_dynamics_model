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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   broader 'climate mitigation imperative' kernel. It posits that effective
 *   climate action requires a fundamental shift in energy governance towards
 *   decentralization and democratic control, viewing nuclear power as
 *   perpetuating an extractive, centralized paradigm. The constraint itself
 *   is the imperative for this transformation, acting as a Scaffold to guide
 *   the transition away from the current extractive system. Its high
 *   extractiveness reflects the severe costs of failing to meet this
 *   imperative, while high resistance and suppression come from incumbent
 *   centralized forces.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.8).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.7).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, scaffold).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Climate Mitigation Imperative: Systems Transition Reading").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).
narrative_ontology:has_sunset_clause(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, '6612a3fa-0b0c-4b2b-aca1-ca706fb9627c').
narrative_ontology:cs_kernel_codification('6612a3fa-0b0c-4b2b-aca1-ca706fb9627c', implicit).
narrative_ontology:cs_authority_grounding('6612a3fa-0b0c-4b2b-aca1-ca706fb9627c', distributed).
narrative_ontology:cs_reading_relation('6612a3fa-0b0c-4b2b-aca1-ca706fb9627c', climate_mitigation_imperative__portfolio_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('6612a3fa-0b0c-4b2b-aca1-ca706fb9627c', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_axiom('6612a3fa-0b0c-4b2b-aca1-ca706fb9627c', foundational, energy_democracy_is_mitigation).
narrative_ontology:cs_axiom_status(energy_democracy_is_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('6612a3fa-0b0c-4b2b-aca1-ca706fb9627c', energy_democracy_is_mitigation, deontological).
narrative_ontology:cs_axiom('6612a3fa-0b0c-4b2b-aca1-ca706fb9627c', foundational, centralization_is_extractive).
narrative_ontology:cs_axiom_status(centralization_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('6612a3fa-0b0c-4b2b-aca1-ca706fb9627c', centralization_is_extractive, empirically_contingent).
narrative_ontology:cs_reference_frame('6612a3fa-0b0c-4b2b-aca1-ca706fb9627c', decentralized_democratic_energy_future).
narrative_ontology:cs_drift_state('6612a3fa-0b0c-4b2b-aca1-ca706fb9627c', contemporary_energy_policy_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6612a3fa-0b0c-4b2b-aca1-ca706fb9627c', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, local_communities).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, climate_justice_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, centralized_energy_utilities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities currently operate the large-scale, centralized energy infrastructure. The imperative for systems transition demands they dismantle or fundamentally alter their business models, incurring significant costs and loss of control. They actively resist this transition while also influencing policy.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, centralized_energy_utilities, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, centralized_energy_utilities, agenda_setter).

% As a key component of centralized energy production, the nuclear industry faces direct opposition from this reading of the mitigation imperative. It is seen as perpetuating the very structure that needs to be transformed, incurring costs in terms of public acceptance and investment redirection.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_industry, payer,
    institutional, generational, constrained, national).

% These developers benefit from the imperative for systems transition, as it creates demand and policy support for their decentralized energy solutions. However, they still face significant structural barriers and competition from incumbent centralized systems.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_developers, beneficiary,
    moderate, biographical, constrained, regional).

% These communities are primary beneficiaries of the imperative, as it promises energy autonomy, local economic development, and relief from the environmental burdens of centralized energy. They are often trapped by existing infrastructure and policy, making the transition a vital pathway to empowerment.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, local_communities, beneficiary,
    powerless, generational, trapped, local).

% These advocates actively champion the systems transition reading, pushing for policies and social movements that prioritize decentralization and democratic control. They benefit from the imperative gaining traction, as it aligns with their core mission.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, climate_justice_advocates, agenda_setter,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, climate_justice_advocates, beneficiary).

% These scholars analyze the feasibility, impacts, and pathways of energy systems transformation. They provide intellectual grounding for the imperative but are not directly subject to its costs or benefits in a material sense.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, systems_transition_scholars, observer,
    analytical, generational, analytical, global).

% Policy makers are tasked with translating the climate mitigation imperative into actionable policies. They can either facilitate or obstruct the systems transition, facing pressure from both incumbent industries and climate justice movements.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action towards a shared vision of a sustainable, equitable energy future, overcoming the fragmentation of individual climate actions and the inertia of incumbent systems.
% TRANSFER_FUNCTION: Demands a transfer of power, resources, and economic benefits from centralized, extractive energy entities to decentralized, democratically controlled ones, and a transfer of environmental burden away from vulnerable communities.
% ABSENT_VOICES: Future generations and non-human ecosystems are the ultimate absent voices; they would unequivocally demand the rapid and just systems transition, bearing the full cost of inaction.
% DISAPPEARANCE_RATIONALE: If this imperative vanished, the political and economic will to dismantle centralized, extractive energy systems would dissipate. This would lead to continued climate degradation, social inequity, and the failure to achieve mitigation goals, fundamentally altering the future trajectory of human and ecological systems.
% FOUNDING_PROBLEM: The dual crisis of climate change driven by fossil fuels and the associated social and environmental injustices perpetuated by centralized, often extractive, energy systems.
% FOUNDING_PROBLEM_CORROBORATION: IPCC reports, climate scientists, frontline communities, and climate justice organizations universally corroborate the live status of both climate change and energy injustice. Centralized energy utilities and the nuclear industry, while acknowledging climate change, often contest the 'extractive centralization' framing.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__systems_transition_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The imperative for systems transition is classified as a Scaffold because it is a temporary support structure designed to facilitate a fundamental shift in energy governance. It has a clear sunset clause: it ceases to be necessary once a decentralized, democratic energy system is achieved. Extractiveness is high (0.8) because the cost of *not* transitioning (i.e., perpetuating the current extractive system) is immense, and the imperative demands significant 'costs' from those who benefit from the status quo. Suppression (0.7) is substantial due to active resistance from powerful centralized energy interests. Theater ratio (0.4) reflects a gap between rhetorical commitment to 'transition' and actual structural change. The measurement series shows increasing extractiveness and suppression as the urgency of the imperative grows and resistance hardens.
 *
 * PERSPECTIVAL GAP:
 *   Those who benefit from the imperative (e.g., local communities) experience it as a liberating force, while those who are targeted by it (e.g., nuclear industry) experience it as a threat to their existence. The engine's per-seat classification will reflect this divergence, showing the imperative as a beneficiary-side Rope/Scaffold and a payer-side Snare/Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperative benefits distributed renewable developers, local communities, and climate justice advocates, as it aligns with their goals and creates opportunities for their models. Conversely, it targets (and thus extracts from) centralized energy utilities and the nuclear industry, whose business models are incompatible with the demanded transformation. Policy makers and systems transition scholars act as agenda-setters and observers, respectively.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nuclear_compatibility_ambiguity,
    'Is nuclear energy inherently incompatible with a decentralized, democratic energy system, or can its governance and deployment be reformed to align with these goals?',
    'Empirical observation of nuclear projects developed and operated under genuinely decentralized and democratic control, or a conceptual re-evaluation of ''decentralization'' to include large-scale, non-fossil assets within a democratic governance framework.',
    'If compatible, nuclear would move from the victim set to a potential beneficiary or neutral party of the imperative, altering the perceived extractiveness of the existing system and the scope of the imperative itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_compatibility_ambiguity, conceptual, 'Ambiguity regarding nuclear energy''s structural role in a systems transition.').

omega_variable(
    transition_timeline_feasibility,
    'Is a rapid, full-scale systems transition to decentralized, democratic control technically and economically feasible within the necessary climate timeline, or does it risk energy instability and social disruption?',
    'Detailed engineering and economic modeling, pilot projects demonstrating scalability, and policy experiments in different jurisdictions, alongside social science research on transition management.',
    'If infeasible, the imperative''s scope or timeline might need adjustment, potentially leading to a re-evaluation of interim solutions or a different claimed_type (e.g., Tangled Rope if compromises are necessary to manage the transition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_timeline_feasibility, empirical, 'Feasibility of rapid, comprehensive energy systems transition.').

omega_variable(
    governance_vs_carbon_priority,
    'Is the primary goal of climate mitigation to reduce carbon emissions by any means, or to achieve a just energy transition with specific governance outcomes (decentralization, democracy)?',
    'Policy choices and public discourse that explicitly prioritize one goal over the other, or a philosophical consensus on the scope of ''climate justice'' within climate action.',
    'If carbon reduction is primary, the constraint might be reclassified as a Rope (pure coordination on carbon reduction) or a different reading of the kernel would dominate, potentially re-evaluating nuclear''s role as a necessary low-carbon source regardless of governance structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(governance_vs_carbon_priority, preference, 'Prioritization of governance outcomes versus carbon reduction in climate mitigation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(clim_tr_t2005, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2010, 0.34).
narrative_ontology:measurement(clim_tr_t2015, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2015, 0.37).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(clim_tr_t2025, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2030, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(clim_be_t2005, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2005, 0.69).
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2010, 0.73).
narrative_ontology:measurement(clim_be_t2015, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement(clim_be_t2025, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2025, 0.79).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2030, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(clim_su_t2005, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2005, 0.59).
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(clim_su_t2015, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2015, 0.66).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(clim_su_t2025, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2025, 0.69).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2030, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
