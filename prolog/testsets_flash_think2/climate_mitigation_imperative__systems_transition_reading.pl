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
 *   human_readable: Climate Mitigation: Systems Transition Imperative (Democratic Control Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'systems transition' reading of the
 *   broader climate mitigation imperative. From this perspective, effective
 *   climate action is not merely about reducing carbon emissions, but
 *   fundamentally about transforming energy systems towards decentralization
 *   and democratic control. Nuclear power, despite being low-carbon, is seen
 *   as perpetuating extractive centralization and is therefore incompatible
 *   with this imperative. The constraint is the structural requirement for
 *   this transformation, which actively extracts from and suppresses existing
 *   centralized energy interests.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.8).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.75).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Climate Mitigation: Systems Transition Imperative (Democratic Control Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, 'c0d0a1f6-5879-4f2b-ab0b-7c4cf3755bf5').
narrative_ontology:cs_kernel_codification('c0d0a1f6-5879-4f2b-ab0b-7c4cf3755bf5', implicit).
narrative_ontology:cs_authority_grounding('c0d0a1f6-5879-4f2b-ab0b-7c4cf3755bf5', distributed).
narrative_ontology:cs_reading_relation('c0d0a1f6-5879-4f2b-ab0b-7c4cf3755bf5', climate_mitigation_imperative__portfolio_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('c0d0a1f6-5879-4f2b-ab0b-7c4cf3755bf5', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_axiom('c0d0a1f6-5879-4f2b-ab0b-7c4cf3755bf5', foundational, energy_system_democratization_is_mitigation).
narrative_ontology:cs_axiom_status(energy_system_democratization_is_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('c0d0a1f6-5879-4f2b-ab0b-7c4cf3755bf5', energy_system_democratization_is_mitigation, deontological).
narrative_ontology:cs_axiom('c0d0a1f6-5879-4f2b-ab0b-7c4cf3755bf5', foundational, centralized_energy_is_extractive).
narrative_ontology:cs_axiom_status(centralized_energy_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('c0d0a1f6-5879-4f2b-ab0b-7c4cf3755bf5', centralized_energy_is_extractive, empirically_contingent).
narrative_ontology:cs_reference_frame('c0d0a1f6-5879-4f2b-ab0b-7c4cf3755bf5', decentralized_democratic_energy_future).
narrative_ontology:cs_drift_state('c0d0a1f6-5879-4f2b-ab0b-7c4cf3755bf5', contemporary_energy_policy_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c0d0a1f6-5879-4f2b-ab0b-7c4cf3755bf5', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, energy_democracy_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, local_communities).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_proponents).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, fossil_fuel_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, centralized_utilities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively push for policies and investments that prioritize decentralized, democratically controlled energy systems, viewing this as integral to climate mitigation and social justice. They seek to dismantle existing centralized power structures.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_democracy_advocates, agenda_setter,
    organized, generational, constrained, global).

% Would benefit from greater local control over energy production, reduced energy costs, and resilience against centralized system failures. Currently, they often bear the environmental burdens of centralized energy without commensurate benefits.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, local_communities, beneficiary,
    powerless, biographical, constrained, local).

% Advocate for and develop small-scale, localized renewable energy solutions (e.g., rooftop solar, community wind farms). They benefit from policies that favor decentralization and face barriers from existing centralized infrastructure.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_proponents, beneficiary,
    organized, biographical, mobile, national).

% Represents a highly centralized, capital-intensive energy sector that this reading views as perpetuating extractive power structures. It faces significant challenges and potential obsolescence under a systems transition imperative.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_industry, payer,
    institutional, generational, constrained, national).

% The primary target of climate mitigation efforts, its business model is fundamentally incompatible with the imperative for a rapid, democratic energy transition. It faces immense pressure to cease operations.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, fossil_fuel_industry, payer,
    institutional, generational, constrained, global).

% Their traditional business model relies on large-scale, centralized generation and distribution, which is challenged by the imperative for decentralization and local control. They resist changes that undermine their existing infrastructure and revenue streams.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, centralized_utilities, payer,
    institutional, biographical, constrained, national).

% Hold the power to enact legislation and regulations that either accelerate or impede the transition to decentralized, democratic energy systems. They are subject to lobbying from all sides of the energy debate.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, policy_makers, agenda_setter,
    institutional, generational, analytical, national).

% Provide the foundational scientific understanding of climate change and its impacts, informing the urgency of mitigation. They do not, however, prescribe the specific social or governance structures of the energy transition.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global and national efforts to transform energy systems away from fossil fuels and centralized, extractive models towards decentralized, democratically controlled renewable energy, ensuring a just and equitable transition.
% TRANSFER_FUNCTION: Transfers control, investment, and long-term benefits from large, centralized energy corporations (including nuclear and fossil fuels) to local communities, distributed renewable energy producers, and the broader public through democratic governance.
% ABSENT_VOICES: While proponents of 'all-of-the-above' low-carbon strategies (e.g., those who see nuclear as a necessary baseload) are present and actively opposed by this reading, the voices truly absent from the conversation are those who benefit from the status quo of extractive centralization and deny the climate imperative itself, or those who are structurally excluded from energy decision-making.
% DISAPPEARANCE_RATIONALE: If the imperative for a systems transition vanished, the default trajectory would be continued reliance on centralized, often extractive, energy sources (fossil fuels, large-scale nuclear), perpetuating existing power structures, energy injustices, and accelerating climate risks. The energy landscape would not spontaneously shift to democratic control.
% FOUNDING_PROBLEM: The existential threat of climate change, coupled with historical patterns of energy injustice, resource wars, and the concentration of wealth and power in centralized energy industries, which this reading argues are fundamentally intertwined.
% FOUNDING_PROBLEM_CORROBORATION: The urgency of climate action is corroborated by the IPCC and global scientific consensus. The critique of centralized energy's extractive nature and its link to social injustice is corroborated by energy justice movements, historical analyses of energy systems, and independent economic studies of distributed energy benefits.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.8) reflects the reading's view that the existing centralized energy system is inherently extractive, and the imperative demands dismantling this. Suppression (0.75) is high because the existing system actively resists the shift to decentralized, democratic control through policy, lobbying, and market power. Theater ratio (0.4) indicates that while some 'green' initiatives may be performative, the core imperative for systemic change is genuine, though often diluted by incremental approaches. Resistance (0.7) is high due to the fundamental opposition from entrenched centralized interests. Accessibility collapse (0.65) is moderate-high, as existing infrastructure and regulatory frameworks make decentralized alternatives difficult to implement at scale without active policy intervention.
 *
 * PERSPECTIVAL GAP:
 *   This reading fundamentally diverges from others on the role of nuclear power and the nature of the energy transition. The 'portfolio optimization' reading would see nuclear as a beneficiary, while the 'opportunity cost' reading would also see nuclear as a victim, but for different (economic) reasons. This constraint's classification is specific to the 'systems transition' perspective, which prioritizes democratic control and decentralization over mere carbon reduction.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'systems transition' imperative structurally benefits energy democracy advocates, local communities, and distributed renewable proponents by aligning policy and resource flows with their vision. Conversely, it targets and extracts from the nuclear industry, fossil fuel industry, and centralized utilities, whose business models and power structures are incompatible with the desired transition. Policy makers are agenda setters, navigating these competing interests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    systems_vs_emissions_focus,
    'Is climate mitigation primarily an emissions reduction problem (technology-agnostic) or a systemic transformation problem (requiring specific governance structures)?',
    'Empirical analysis of mitigation pathways that achieve climate goals: do successful pathways consistently involve decentralization and democratic control, or can centralized, extractive systems also achieve targets effectively?',
    'If mitigation is purely an emissions problem, the extractiveness and suppression metrics for nuclear power would be re-evaluated, potentially shifting it from victim to beneficiary. If systemic transformation is essential, the current classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systems_vs_emissions_focus, conceptual, 'Ambiguity in the core framing of climate mitigation.').

omega_variable(
    nuclear_centralization_inevitability,
    'Is nuclear power inherently and inevitably centralized and extractive, or can it be integrated into a democratically controlled, decentralized energy system?',
    'Technological and governance innovation: if small modular reactors (SMRs) or other nuclear technologies can be deployed and governed in a truly decentralized, democratic manner, the premise of nuclear''s inherent centralization would be challenged.',
    'If nuclear can be decentralized, its position in the victim set would be re-evaluated, potentially shifting it to a neutral or even beneficiary role under this reading, reducing the constraint''s overall extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_centralization_inevitability, empirical, 'Whether nuclear power is intrinsically centralized and extractive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(clim_tr_t40, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(clim_tr_t50, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement(clim_be_t40, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(clim_be_t50, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 50, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(clim_su_t40, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(clim_su_t50, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, nuclear_power_subsidies).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, fossil_fuel_infrastructure_permitting).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, grid_modernization_standards).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, carbon_pricing_mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
