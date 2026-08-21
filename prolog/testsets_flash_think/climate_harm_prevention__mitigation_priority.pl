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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Legitimate Climate Response: Mitigation Priority within Growth Framework
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint describes the dominant framing of climate response,
 *   prioritizing emissions reduction (mitigation) to prevent future harm,
 *   primarily through technological transition, while explicitly operating
 *   within a framework of continued economic growth. It is one reading of the
 *   'climate_harm_prevention' kernel, distinguishing itself from
 *   'adaptation_priority' and 'degrowth_reading' by its core tenets. The
 *   constraint functions as a Tangled Rope, coordinating global action but
 *   with significant asymmetric extraction from present generations and
 *   carbon-intensive sectors for the benefit of future generations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.65).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.7).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Legitimate Climate Response: Mitigation Priority within Growth Framework").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, 'cfd8b538-8933-465d-beba-454fc27d1ba2').
narrative_ontology:cs_kernel_codification('cfd8b538-8933-465d-beba-454fc27d1ba2', formalized).
narrative_ontology:cs_authority_grounding('cfd8b538-8933-465d-beba-454fc27d1ba2', lineage).
narrative_ontology:cs_interpretation_layer_present('cfd8b538-8933-465d-beba-454fc27d1ba2').
narrative_ontology:cs_reading_relation('cfd8b538-8933-465d-beba-454fc27d1ba2', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('cfd8b538-8933-465d-beba-454fc27d1ba2', climate_harm_prevention__degrowth_reading, forecloses).
narrative_ontology:cs_axiom('cfd8b538-8933-465d-beba-454fc27d1ba2', foundational, technological_decoupling_is_possible).
narrative_ontology:cs_axiom_status(technological_decoupling_is_possible, holdable).
narrative_ontology:cs_axiom_grounding('cfd8b538-8933-465d-beba-454fc27d1ba2', technological_decoupling_is_possible, empirically_contingent).
narrative_ontology:cs_axiom('cfd8b538-8933-465d-beba-454fc27d1ba2', foundational, intergenerational_equity_demands_mitigation).
narrative_ontology:cs_axiom_status(intergenerational_equity_demands_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('cfd8b538-8933-465d-beba-454fc27d1ba2', intergenerational_equity_demands_mitigation, deontological).
narrative_ontology:cs_reference_frame('cfd8b538-8933-465d-beba-454fc27d1ba2', unfccc_paris_agreement_framework).
narrative_ontology:cs_drift_state('cfd8b538-8933-465d-beba-454fc27d1ba2', contemporary_climate_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cfd8b538-8933-465d-beba-454fc27d1ba2', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, present_day_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiaries of emissions reduction, as they will experience fewer and less severe climate impacts. They have no direct voice in current policy decisions but are the ultimate reason for the constraint.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Bear significant costs from emissions regulations, carbon pricing, and mandates for technological transition. Their business models are directly challenged, leading to strong resistance and lobbying efforts. Exit means abandoning existing capital and market share.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_industries, payer,
    organized, biographical, constrained, global).

% Experience costs through higher energy prices, taxes, and changes in consumption patterns required for the transition. While some benefit from cleaner air and green jobs, the overall economic shift represents a burden for many, especially those in lower income brackets.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, present_day_citizens, payer,
    moderate, biographical, constrained, global).

% Responsible for designing, implementing, and enforcing climate policies that prioritize mitigation. They navigate competing interests from industries, citizens, and scientific bodies, aiming to achieve emissions targets within political and economic constraints.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Provide the foundational data and projections that justify the need for mitigation. They observe the effectiveness of policies and the ongoing climate crisis, often advocating for more ambitious action.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_scientists, observer,
    analytical, generational, analytical, global).

% Argue that mitigation within a growth framework is insufficient or impossible, advocating for planned economic contraction. Their proposals are largely excluded from mainstream policy discourse by the 'growth framework' axiom of this constraint.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, degrowth_advocates, excluded,
    organized, generational, constrained, global).

% Prioritize building resilience to current and unavoidable climate impacts, arguing that mitigation efforts are too slow or politically infeasible. While not entirely excluded from climate discourse, their emphasis is secondary to mitigation within this constraint's framing.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, adaptation_advocates, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__mitigation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_harm_prevention__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global and national efforts to reduce greenhouse gas emissions, preventing catastrophic future climate change by guiding technological transition and economic restructuring.
% TRANSFER_FUNCTION: Transfers economic costs, investment, and behavioral changes from present generations and carbon-intensive sectors to prevent future climate damages, primarily benefiting future generations.
% ABSENT_VOICES: Degrowth advocates are excluded by the fundamental commitment to a growth framework. Adaptation-first advocates are marginalized by the prioritization of mitigation. Developing nations often argue for differentiated responsibilities based on historical emissions, which can be underrepresented in frameworks emphasizing universal mitigation targets.
% DISAPPEARANCE_RATIONALE: If this framework vanished, global emissions would likely accelerate without coordinated policy, leading to more severe and rapid climate change. Economic sectors would revert to high-carbon practices, and the global response would become chaotic and fragmented, with devastating long-term consequences.
% FOUNDING_PROBLEM: Anthropogenic greenhouse gas emissions causing global warming, leading to predicted future climate catastrophes, ecosystem collapse, and human suffering.
% FOUNDING_PROBLEM_CORROBORATION: The Intergovernmental Panel on Climate Change (IPCC) reports, the vast majority of peer-reviewed climate science, and international agreements like the UNFCCC and Paris Agreement provide corroboration from outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.65) because the costs of decarbonization are substantial and borne by current economic actors, while the benefits accrue diffusely to future generations. Suppression (0.70) is significant, as alternative approaches (like degrowth) are actively excluded from the policy discourse, and resistance from affected industries is met with enforcement. Theater ratio (0.40) reflects a gap between ambitious targets and actual implementation, with some policy actions serving more as symbolic gestures than effective mitigation. Resistance is high (0.75) due to the profound economic restructuring required and the concentrated costs on powerful industries.
 *
 * PERSPECTIVAL GAP:
 *   The 'mitigation_priority' reading is experienced as a necessary, rational coordination effort by climate scientists and many policymakers, while carbon-intensive industries and degrowth advocates experience it as an extractive and suppressive force, respectively. The engine's per-seat classification will highlight this divergence, showing a 'rope-like' function for future generations and a 'snare-like' function for those bearing the costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are the ultimate beneficiaries (d near 0.0), receiving the benefit of a more stable climate without bearing the costs. Carbon-intensive industries and present-day citizens are the primary targets (d near 1.0), bearing the direct and indirect costs of the transition. Policy makers act as agenda-setters, mediating the coordination and extraction. Degrowth and adaptation advocates are structurally excluded, their perspectives suppressed by the core tenets of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (ignoring the extraction from present generations) or a pure Snare (ignoring the genuine coordination function of preventing future harm). The 'live' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, indicates that the constraint's mandate is still relevant, even if its implementation involves significant extraction and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_decoupling_feasibility,
    'Is technological decarbonization truly compatible with sustained economic growth, or does the ''growth framework'' axiom suppress the recognition of fundamental limits?',
    'Empirical observation of global emissions trajectories relative to GDP growth over the next decade, and independent economic modeling without growth assumptions.',
    'If decoupling proves infeasible, the ''growth framework'' becomes a cover story for insufficient action, shifting the constraint towards a Snare or Piton, and strengthening the Degrowth reading''s position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decoupling_feasibility, empirical, 'The empirical viability of decoupling economic growth from emissions.').

omega_variable(
    intergenerational_equity_framing,
    'Is the prioritization of future generations'' well-being over present-day economic costs a universally accepted ethical principle, or is it a contested normative choice?',
    'Cross-cultural ethical surveys and philosophical analysis of intergenerational justice, particularly concerning the discount rate applied to future harms.',
    'If contested, the ''deontological'' grounding of intergenerational equity becomes a ''preference'' rather than a ''foundational'' axiom, weakening the moral authority of this reading for some stakeholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_equity_framing, conceptual, 'The normative grounding of intergenerational equity in climate policy.').

omega_variable(
    suppression_of_alternatives_mechanism,
    'Is the suppression of degrowth and adaptation-first alternatives primarily structural (e.g., political economy, institutional inertia) or internalized (e.g., cognitive lock-in to techno-optimism)?',
    'Analysis of policy discourse and media framing, alongside studies of political lobbying and institutional barriers to alternative policy proposals. If alternatives gain traction when structural barriers are lowered, it''s primarily structural.',
    'If largely internalized, the effective suppression is higher than structural measures suggest, as agents carry the suppression with them, making policy shifts harder even with reduced external barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternatives_mechanism, empirical, 'Structural vs. internalized suppression of climate policy alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 1990, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_harm_prevention__mitigation_priority, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(clim_tr_t2000, climate_harm_prevention__mitigation_priority, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(clim_tr_t2010, climate_harm_prevention__mitigation_priority, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__mitigation_priority, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(clim_tr_t2030, climate_harm_prevention__mitigation_priority, theater_ratio, 2030, 0.4).
narrative_ontology:measurement(clim_tr_t2040, climate_harm_prevention__mitigation_priority, theater_ratio, 2040, 0.42).
narrative_ontology:measurement(clim_tr_t2050, climate_harm_prevention__mitigation_priority, theater_ratio, 2050, 0.45).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_harm_prevention__mitigation_priority, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(clim_be_t2000, climate_harm_prevention__mitigation_priority, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(clim_be_t2010, climate_harm_prevention__mitigation_priority, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__mitigation_priority, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(clim_be_t2030, climate_harm_prevention__mitigation_priority, base_extractiveness, 2030, 0.65).
narrative_ontology:measurement(clim_be_t2040, climate_harm_prevention__mitigation_priority, base_extractiveness, 2040, 0.68).
narrative_ontology:measurement(clim_be_t2050, climate_harm_prevention__mitigation_priority, base_extractiveness, 2050, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_harm_prevention__mitigation_priority, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(clim_su_t2000, climate_harm_prevention__mitigation_priority, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(clim_su_t2010, climate_harm_prevention__mitigation_priority, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__mitigation_priority, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(clim_su_t2030, climate_harm_prevention__mitigation_priority, suppression_requirement, 2030, 0.7).
narrative_ontology:measurement(clim_su_t2040, climate_harm_prevention__mitigation_priority, suppression_requirement, 2040, 0.72).
narrative_ontology:measurement(clim_su_t2050, climate_harm_prevention__mitigation_priority, suppression_requirement, 2050, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, global_infrastructure).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, carbon_pricing_mechanisms).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, renewable_energy_subsidies).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, fossil_fuel_extraction_permits).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'climate_harm_prevention' kernel. It focuses on mitigation within a growth framework, while 'adaptation_priority' emphasizes resilience, and 'degrowth_reading' advocates for economic contraction. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
