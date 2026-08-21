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
 *   This constraint, 'Climate Mitigation Imperative: Systems Transition
 *   Reading,' interprets climate mitigation not merely as a carbon reduction
 *   problem, but as a fundamental transformation of energy governance towards
 *   decentralization and democratic control. From this perspective, nuclear
 *   power, despite being low-carbon, is seen as perpetuating an extractive,
 *   centralized energy paradigm, making it incompatible with genuine climate
 *   mitigation. The constraint is the existing governance structure that
 *   favors and enables this centralized approach, actively suppressing
 *   alternatives. This reading frames the 'climate mitigation imperative' as
 *   a Snare, extracting from communities and distributed developers while
 *   benefiting incumbents who maintain control.
 *
 * KEY AGENTS:
 *   - fossil_fuel_incumbents: Beneficiary (institutional/arbitrage) — delays systemic change
 *   - centralized_energy_utilities: Beneficiary (institutional/constrained) — maintains existing model
 *   - nuclear_industry_lobby: Agenda Setter (organized/constrained) — promotes nuclear as solution
 *   - local_communities: Payer (powerless/trapped) — bears risks, lacks control
 *   - distributed_renewable_developers: Payer (moderate/constrained) — faces systemic barriers
 *   - energy_democracy_advocates: Payer (organized/constrained) — resists centralized paradigm
 *   - climate_scientists: Observer (analytical/analytical) — provides scientific context
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.85).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.7).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, snare).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Climate Mitigation Imperative: Systems Transition Reading").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, 'ffe2f995-526b-43b6-a080-16ccab03b2a0').
narrative_ontology:cs_kernel_codification('ffe2f995-526b-43b6-a080-16ccab03b2a0', distributed).
narrative_ontology:cs_authority_grounding('ffe2f995-526b-43b6-a080-16ccab03b2a0', extraction).
narrative_ontology:cs_interpretation_layer_present('ffe2f995-526b-43b6-a080-16ccab03b2a0').
narrative_ontology:cs_reading_relation('ffe2f995-526b-43b6-a080-16ccab03b2a0', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('ffe2f995-526b-43b6-a080-16ccab03b2a0', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_axiom('ffe2f995-526b-43b6-a080-16ccab03b2a0', foundational, energy_system_governance_is_climate_mitigation).
narrative_ontology:cs_axiom_status(energy_system_governance_is_climate_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('ffe2f995-526b-43b6-a080-16ccab03b2a0', energy_system_governance_is_climate_mitigation, deontological).
narrative_ontology:cs_axiom('ffe2f995-526b-43b6-a080-16ccab03b2a0', foundational, centralized_energy_is_inherently_extractive).
narrative_ontology:cs_axiom_status(centralized_energy_is_inherently_extractive, holdable).
narrative_ontology:cs_axiom_grounding('ffe2f995-526b-43b6-a080-16ccab03b2a0', centralized_energy_is_inherently_extractive, empirically_contingent).
narrative_ontology:cs_reference_frame('ffe2f995-526b-43b6-a080-16ccab03b2a0', decentralized_democratic_energy_future).
narrative_ontology:cs_drift_state('ffe2f995-526b-43b6-a080-16ccab03b2a0', contemporary_energy_policy, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ffe2f995-526b-43b6-a080-16ccab03b2a0', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, centralized_energy_utilities).
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

% Benefits from the perpetuation of centralized energy systems, even if low-carbon, as it delays a fundamental shift to decentralized, democratically controlled energy that would undermine their existing infrastructure and business models. They can pivot to large-scale nuclear projects, maintaining their market position.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, fossil_fuel_incumbents, beneficiary,
    institutional, generational, arbitrage, global).

% Prefers large-scale, capital-intensive projects like nuclear power plants, which fit their existing operational models and regulatory frameworks, over distributed renewable energy systems that require new governance and ownership structures. They benefit from maintaining centralized control.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, centralized_energy_utilities, beneficiary,
    institutional, generational, constrained, national).

% Actively promotes nuclear power as a climate solution, often downplaying its centralized, capital-intensive nature and its incompatibility with a rapid, democratic energy transition. They influence policy and public discourse to secure investments and regulatory support for nuclear projects.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_industry_lobby, agenda_setter,
    organized, biographical, constrained, global).

% Bear the long-term risks and costs associated with centralized energy infrastructure, including nuclear waste disposal and potential environmental impacts, without democratic control over energy production or distribution. They are often excluded from decision-making processes.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, local_communities, payer,
    powerless, generational, trapped, local).

% Face systemic barriers to entry and growth due to regulatory frameworks and market structures that favor large, centralized energy projects. Their efforts to build decentralized, democratically controlled energy systems are suppressed by the existing paradigm.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_developers, payer,
    moderate, biographical, constrained, regional).

% Actively resist the perpetuation of centralized energy systems, including nuclear, arguing for a fundamental shift towards decentralized, community-owned, and democratically controlled renewable energy. They bear the costs of advocating against a powerful incumbent system.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_democracy_advocates, payer,
    organized, generational, constrained, national).

% Provide the scientific basis for climate mitigation, but their findings are interpreted through different lenses regarding energy system transformation. From this reading, they observe the socio-technical implications of different energy pathways.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates the continued operation and expansion of large-scale, centralized energy infrastructure, including nuclear, by aligning regulatory bodies, financial institutions, and incumbent utilities around a familiar, capital-intensive model of energy production.
% TRANSFER_FUNCTION: Transfers economic and political power, as well as long-term environmental risks, from local communities and distributed energy developers to centralized energy utilities, fossil fuel incumbents, and the nuclear industry, by prioritizing large-scale projects over decentralized alternatives.
% ABSENT_VOICES: Indigenous communities, environmental justice groups, and grassroots energy cooperatives are largely excluded from the dominant discourse and decision-making processes, despite bearing disproportionate impacts and offering alternative models for energy governance.
% DISAPPEARANCE_RATIONALE: If the imperative to maintain centralized, extractive energy systems vanished, there would be a rapid shift towards decentralized, democratically controlled renewable energy. Investment would flow to local projects, regulatory barriers would fall, and communities would gain greater control over their energy futures, fundamentally reorganizing the energy landscape.
% FOUNDING_PROBLEM: The founding problem was to provide reliable, large-scale energy to support industrial and societal development, often framed as a national security imperative, leading to the establishment of centralized energy grids and large power plants (including nuclear).
% FOUNDING_PROBLEM_CORROBORATION: Centralized utilities and the nuclear industry claim the problem of reliable baseload power is still live. Energy democracy advocates and many economists argue that distributed renewables, coupled with storage and smart grids, can now provide equivalent or superior reliability, rendering the original problem 'dead' in its centralized framing. Independent analyses of grid stability and renewable integration support the contested status.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__systems_transition_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the transfer of wealth and power to centralized entities, and the imposition of long-term risks on communities, by maintaining a system that prioritizes large-scale, capital-intensive projects. Suppression (0.7) is significant due to regulatory capture, market design, and lobbying efforts that actively disadvantage decentralized alternatives. The theater ratio (0.4) indicates that while 'climate mitigation' is the stated goal, a substantial portion of the activity serves to maintain the existing power structures rather than genuinely transform the energy system. The claimed type is Snare because the coordination story (reliable energy supply) is seen as cover for the extraction of control and resources, with identifiable victims and active suppression of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries (incumbents, utilities, nuclear lobby), the constraint is a necessary 'Rope' for stable energy supply and climate action. From the perspective of the payers (communities, distributed developers, advocates), it is a 'Snare' that perpetuates an extractive system under the guise of mitigation. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The fossil fuel incumbents and centralized energy utilities are beneficiaries (low d) as the constraint allows them to maintain their market position and operational models. The nuclear industry lobby is an agenda-setter (low d) actively shaping the narrative and policy. Local communities, distributed renewable developers, and energy democracy advocates are payers/victims (high d) as they bear the costs and face suppressed alternatives. Climate scientists are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the perpetuation of centralized energy systems as genuine 'coordination' for climate mitigation. By identifying it as a Snare, it highlights that the mandate (climate mitigation) is being used to justify an extractive structure that actively works against a more democratic and decentralized energy transition. The 'contested' status of the founding problem further supports this, indicating that the original justification for centralization is no longer universally accepted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_cost_of_centralization,
    'What is the full lifecycle cost of centralized energy systems (including nuclear), accounting for waste, decommissioning, security, and grid resilience, compared to distributed systems?',
    'Comprehensive, independent economic and social cost-benefit analysis that internalizes all externalities and compares across system architectures.',
    'If the full costs of centralization are significantly higher than currently accounted for, it would further strengthen the ''Snare'' classification by revealing hidden extraction and inefficiency, making the ''systems_transition_reading'' more empirically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_of_centralization, empirical, 'Assessing the hidden costs of centralized energy infrastructure.').

omega_variable(
    democratic_control_feasibility,
    'To what extent is ''democratic control'' of energy systems a practically achievable goal, given technical complexities and economies of scale, versus an aspirational ideal?',
    'Empirical studies of existing energy cooperatives and decentralized grids, combined with policy analysis of regulatory barriers and enablers for democratic energy models.',
    'If democratic control is shown to be widely feasible and efficient, it would validate the core premise of this reading and highlight the ''suppression'' of such models. If it proves largely unfeasible, it would weaken the reading''s practical force, potentially shifting its classification towards a ''Piton'' if the resistance becomes purely performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_control_feasibility, empirical, 'Feasibility of democratic control in energy systems.').

omega_variable(
    nuclear_role_in_transition,
    'Can nuclear power be integrated into a decentralized, democratically controlled energy system, or is its inherent nature fundamentally incompatible with such a transition?',
    'Technological and governance innovation: development of small modular reactors (SMRs) with local ownership models, or new regulatory frameworks that enable community-level control over nuclear assets.',
    'If nuclear can be democratized, it would challenge a core tenet of this reading, potentially shifting the ''nuclear perpetuates extractive centralization'' claim to ''nuclear can be reformed,'' altering the victim set and reducing the perceived extractiveness. If not, the incompatibility is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_role_in_transition, conceptual, 'Compatibility of nuclear power with democratic energy transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1950, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(clim_tr_t1970, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(clim_tr_t1990, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(clim_tr_t2024, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t1950, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(clim_be_t1970, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(clim_be_t1990, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(clim_be_t2024, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1950, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(clim_su_t1970, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(clim_su_t1990, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(clim_su_t2024, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__opportunity_cost_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'climate_mitigation_imperative' kernel. This 'systems_transition_reading' focuses on energy governance and decentralization, contrasting with the 'portfolio_optimization_reading' (maximizing all low-carbon sources) and the 'opportunity_cost_reading' (economic efficiency of mitigation investments).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
