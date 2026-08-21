% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__mitigation_priority, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Climate Harm Prevention: Mitigation Priority
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the dominant policy paradigm for climate
 *   change, prioritizing emissions reduction through technological innovation
 *   and market mechanisms, while assuming continued economic growth. It
 *   frames climate action as a necessary investment to prevent future harm,
 *   primarily benefiting future generations. The constraint is a reading of
 *   the broader 'climate_harm_prevention' kernel, distinguishing itself from
 *   approaches that prioritize adaptation or degrowth.
 *
 * KEY AGENTS:
 *   - future_generations: Primary beneficiary (powerless/trapped)
 *   - present_governments: Agenda setter (institutional/constrained)
 *   - carbon_intensive_industries: Primary payer (powerful/constrained)
 *   - renewable_energy_sector: Secondary beneficiary (organized/mobile)
 *   - climate_scientists: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.65).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.7).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Climate Harm Prevention: Mitigation Priority").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, 'c949ff99-bf34-49bb-a663-5adc94ae3beb').
narrative_ontology:cs_kernel_codification('c949ff99-bf34-49bb-a663-5adc94ae3beb', formalized).
narrative_ontology:cs_authority_grounding('c949ff99-bf34-49bb-a663-5adc94ae3beb', expertise).
narrative_ontology:cs_interpretation_layer_present('c949ff99-bf34-49bb-a663-5adc94ae3beb').
narrative_ontology:cs_reading_relation('c949ff99-bf34-49bb-a663-5adc94ae3beb', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('c949ff99-bf34-49bb-a663-5adc94ae3beb', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('c949ff99-bf34-49bb-a663-5adc94ae3beb', foundational, emissions_reduction_is_primary_lever).
narrative_ontology:cs_axiom_status(emissions_reduction_is_primary_lever, holdable).
narrative_ontology:cs_axiom_grounding('c949ff99-bf34-49bb-a663-5adc94ae3beb', emissions_reduction_is_primary_lever, empirically_contingent).
narrative_ontology:cs_axiom('c949ff99-bf34-49bb-a663-5adc94ae3beb', foundational, decarbonization_compatible_with_growth).
narrative_ontology:cs_axiom_status(decarbonization_compatible_with_growth, holdable).
narrative_ontology:cs_axiom_grounding('c949ff99-bf34-49bb-a663-5adc94ae3beb', decarbonization_compatible_with_growth, empirically_contingent).
narrative_ontology:cs_reference_frame('c949ff99-bf34-49bb-a663-5adc94ae3beb', scientific_consensus_on_mitigation).
narrative_ontology:cs_drift_state('c949ff99-bf34-49bb-a663-5adc94ae3beb', contemporary_political_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c949ff99-bf34-49bb-a663-5adc94ae3beb', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, renewable_energy_sector).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_producers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Primary beneficiaries of reduced future climate impacts, but have no direct voice or agency in current policy decisions. Their interests are represented by proxy.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, generational, trapped, global).

% Responsible for setting and enforcing climate policies, balancing economic growth with mitigation targets. They face political pressure from both industry and environmental advocates.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, present_governments, agenda_setter,
    institutional, biographical, constrained, global).

% Bear the direct costs of emissions reductions, such as carbon taxes, regulations, and investments in new technologies. They resist policies that threaten their existing business models.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_industries, payer,
    powerful, immediate, constrained, global).

% Face declining demand and stranded asset risks due to mitigation policies. They actively lobby against stringent emissions targets.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_producers, payer,
    powerful, immediate, constrained, global).

% Benefits from policies that incentivize renewable energy deployment and phase out fossil fuels. They advocate for stronger mitigation efforts.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, renewable_energy_sector, beneficiary,
    organized, generational, mobile, global).

% Bear indirect costs through higher energy prices or changes in consumption patterns. Their resistance often manifests as political opposition to climate policies.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_consumers, payer,
    moderate, immediate, constrained, national).

% Provide the scientific basis for understanding climate change and the effectiveness of mitigation strategies. They advocate for evidence-based policy.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts to reduce greenhouse gas emissions, aligning national policies and investments towards a common goal of preventing catastrophic climate change.
% TRANSFER_FUNCTION: Transfers economic costs (e.g., carbon taxes, investment in green tech) from the present generation (especially carbon-intensive sectors) to future generations (as avoided damages), and from carbon-intensive industries to the renewable energy sector.
% ABSENT_VOICES: Future generations, who are the primary beneficiaries, have no direct voice. Their interests are represented by proxy, which can be imperfectly aligned with their actual future needs. Degrowth advocates are also largely excluded from mainstream policy discussions.
% DISAPPEARANCE_RATIONALE: If the priority on mitigation vanished, global emissions would likely accelerate, leading to more severe climate impacts. Economic investments would shift away from decarbonization, and the long-term habitability of the planet would be further jeopardized, fundamentally altering future societal structures.
% FOUNDING_PROBLEM: The scientific consensus on anthropogenic climate change indicated a severe risk of irreversible environmental and societal harm if greenhouse gas emissions continued unchecked.
% FOUNDING_PROBLEM_CORROBORATION: The Intergovernmental Panel on Climate Change (IPCC) reports, national science academies, and a vast body of peer-reviewed scientific literature from outside the benefiting parties consistently corroborate the ongoing and escalating nature of the climate problem.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates global action on a collective-action problem (climate change mitigation) but also involves significant asymmetric extraction. Present carbon-intensive industries and consumers bear the costs, while future generations and the renewable energy sector benefit. Active enforcement (regulations, carbon pricing) is required to overcome resistance from those who bear the costs. Extractiveness is high (0.65) due to the substantial economic restructuring required and the transfer of wealth. Suppression (0.70) reflects the political and economic pressure applied to ensure compliance and limit alternatives to the growth-compatible mitigation pathway. Theater ratio is relatively low (0.20) as the mitigation efforts are largely genuine, though some 'greenwashing' exists.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of future generations, this constraint is a vital Rope, ensuring their long-term well-being. For carbon-intensive industries, it is a Snare, imposing significant costs and threatening their existence. Present governments and the renewable energy sector view it as a necessary, albeit challenging, Rope or Scaffold. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are full beneficiaries (d=0.0) as they receive the primary benefit of avoided harm. Carbon-intensive industries and fossil fuel producers are full targets (d=1.0) as they bear the most significant costs and face existential threats. Present governments are closer to symmetric (d=0.5) as they balance benefits and costs. The renewable energy sector is a beneficiary (d=0.2) due to market expansion. Carbon-intensive consumers are targets (d=0.8) due to indirect costs. Climate scientists are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring extraction) or a pure Snare (ignoring coordination). It highlights the dual nature: a genuine coordination function to address a global externality, coupled with significant, actively enforced extraction from specific sectors. The 'live' status of the founding problem (climate change risk) indicates it is not a Piton, as its mandate is still highly relevant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_decoupling_feasibility,
    'Is it empirically feasible to achieve the necessary emissions reductions while maintaining economic growth, as this reading assumes?',
    'Long-term empirical data on decoupling rates of GDP from emissions, and technological breakthroughs in carbon capture and renewable energy storage.',
    'If decoupling proves infeasible, the ''mitigation_priority'' reading''s foundational premise is challenged, potentially shifting policy towards ''degrowth_reading'' or ''adaptation_priority'' as more realistic alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decoupling_feasibility, empirical, 'Uncertainty about the empirical possibility of growth-compatible decarbonization.').

omega_variable(
    intergenerational_equity_burden,
    'Does the current distribution of mitigation costs (borne by present generations) fairly reflect the intergenerational equity principles this reading implicitly invokes?',
    'Ethical and economic analysis of intergenerational burden-sharing, considering historical emissions, current capabilities, and future benefits.',
    'If the burden is deemed inequitable, it could lead to increased resistance from present payers or demands for greater compensation/support for affected sectors, potentially altering the constraint''s political viability or requiring new transfer mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_equity_burden, preference, 'Ambiguity regarding the fairness of cost distribution across generations.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.70) primarily structural (e.g., regulatory barriers, carbon pricing) or internalized (e.g., industry belief in ''green growth'' narratives, fear of being left behind)?',
    'Post-policy-removal trajectory: if resistance to decarbonization persists after direct regulatory pressure is removed, reclassify as partially internalized. Analysis of industry lobbying vs. internal R&D shifts.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as actors carry the suppression with them. If purely structural, removing the policy would immediately unleash alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in climate policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 1990, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_harm_prevention__mitigation_priority, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(clim_tr_t2000, climate_harm_prevention__mitigation_priority, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(clim_tr_t2010, climate_harm_prevention__mitigation_priority, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__mitigation_priority, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(clim_tr_t2030, climate_harm_prevention__mitigation_priority, theater_ratio, 2030, 0.18).
narrative_ontology:measurement_basis(clim_tr_t2030, projected).
narrative_ontology:measurement(clim_tr_t2040, climate_harm_prevention__mitigation_priority, theater_ratio, 2040, 0.15).
narrative_ontology:measurement_basis(clim_tr_t2040, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_harm_prevention__mitigation_priority, theater_ratio, 2050, 0.12).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_harm_prevention__mitigation_priority, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(clim_be_t2000, climate_harm_prevention__mitigation_priority, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(clim_be_t2010, climate_harm_prevention__mitigation_priority, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__mitigation_priority, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(clim_be_t2030, climate_harm_prevention__mitigation_priority, base_extractiveness, 2030, 0.7).
narrative_ontology:measurement_basis(clim_be_t2030, projected).
narrative_ontology:measurement(clim_be_t2040, climate_harm_prevention__mitigation_priority, base_extractiveness, 2040, 0.72).
narrative_ontology:measurement_basis(clim_be_t2040, projected).
narrative_ontology:measurement(clim_be_t2050, climate_harm_prevention__mitigation_priority, base_extractiveness, 2050, 0.75).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_harm_prevention__mitigation_priority, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(clim_su_t2000, climate_harm_prevention__mitigation_priority, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(clim_su_t2010, climate_harm_prevention__mitigation_priority, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__mitigation_priority, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(clim_su_t2030, climate_harm_prevention__mitigation_priority, suppression_requirement, 2030, 0.75).
narrative_ontology:measurement_basis(clim_su_t2030, projected).
narrative_ontology:measurement(clim_su_t2040, climate_harm_prevention__mitigation_priority, suppression_requirement, 2040, 0.78).
narrative_ontology:measurement_basis(clim_su_t2040, projected).
narrative_ontology:measurement(clim_su_t2050, climate_harm_prevention__mitigation_priority, suppression_requirement, 2050, 0.8).
narrative_ontology:measurement_basis(clim_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, enforcement_mechanism).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, global_carbon_markets).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, renewable_energy_subsidies).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, fossil_fuel_divestment_campaigns).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
