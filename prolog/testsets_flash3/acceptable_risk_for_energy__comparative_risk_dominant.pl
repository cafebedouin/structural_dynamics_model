% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__comparative_risk_dominant, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative Risk Dominant Nuclear Acceptability
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint represents a reading of acceptable risk for energy policy
 *   where nuclear power's risks are deemed acceptable primarily in comparison
 *   to the risks posed by fossil fuels and climate change. It prioritizes
 *   immediate climate action and energy security, accepting certain long-term
 *   or low-probability risks as a trade-off. This reading's victim set
 *   explicitly includes climate-vulnerable populations as beneficiaries of
 *   the comparative approach, while local communities near waste sites and
 *   future generations bear the costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.45).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.3).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.45).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative Risk Dominant Nuclear Acceptability").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, '9bd4b243-3c76-4c54-9a61-fd6593634120').
narrative_ontology:cs_kernel_codification('9bd4b243-3c76-4c54-9a61-fd6593634120', formalized).
narrative_ontology:cs_authority_grounding('9bd4b243-3c76-4c54-9a61-fd6593634120', expertise).
narrative_ontology:cs_interpretation_layer_present('9bd4b243-3c76-4c54-9a61-fd6593634120').
narrative_ontology:cs_reading_relation('9bd4b243-3c76-4c54-9a61-fd6593634120', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('9bd4b243-3c76-4c54-9a61-fd6593634120', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('9bd4b243-3c76-4c54-9a61-fd6593634120', foundational, climate_urgency_trumps_long_tail_risk).
narrative_ontology:cs_axiom_status(climate_urgency_trumps_long_tail_risk, holdable).
narrative_ontology:cs_axiom_grounding('9bd4b243-3c76-4c54-9a61-fd6593634120', climate_urgency_trumps_long_tail_risk, instrumental).
narrative_ontology:cs_axiom('9bd4b243-3c76-4c54-9a61-fd6593634120', foundational, relative_harm_is_the_primary_metric).
narrative_ontology:cs_axiom_status(relative_harm_is_the_primary_metric, holdable).
narrative_ontology:cs_axiom_grounding('9bd4b243-3c76-4c54-9a61-fd6593634120', relative_harm_is_the_primary_metric, conventional).
narrative_ontology:cs_reference_frame('9bd4b243-3c76-4c54-9a61-fd6593634120', climate_crisis_response_framework).
narrative_ontology:cs_drift_state('9bd4b243-3c76-4c54-9a61-fd6593634120', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9bd4b243-3c76-4c54-9a61-fd6593634120', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_energy_proponents).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, local_communities_near_waste_sites).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for nuclear power as a critical tool for decarbonization, emphasizing its low operational emissions compared to fossil fuels. They frame nuclear risk as manageable and acceptable when weighed against climate change impacts.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_energy_proponents, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the reduction of fossil fuel emissions and the mitigation of climate change, which nuclear power contributes to. Their immediate and severe vulnerability to climate impacts makes them prioritize solutions that reduce greenhouse gases, even if they carry other risks.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, beneficiary,
    organized, immediate, trapped, global).

% Bear the long-term burden of nuclear waste storage, facing potential environmental and health risks over millennia. Their concerns about intergenerational equity and irreversible contamination are often downplayed in the comparative risk framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, local_communities_near_waste_sites, payer,
    powerless, generational, trapped, local).

% Will inherit the legacy of nuclear waste, requiring perpetual management and posing risks that extend far beyond current human timescales. Their interests are represented by advocates but they have no direct voice.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, future_generations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(acceptable_risk_for_energy__comparative_risk_dominant, future_generations).

% Would argue against nuclear power's expansion, as it directly competes with their energy sources. Their arguments often focus on nuclear safety and cost, but their structural position makes them an interested party in the energy mix debate.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, fossil_fuel_industry, excluded,
    institutional, biographical, constrained, global).

% Are tasked with ensuring the safe operation of nuclear facilities and waste management. They operate within the policy framework set by governments, which may or may not prioritize comparative risk over absolute safety thresholds.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_safety_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates energy policy decisions by providing a framework to evaluate nuclear power's risks and benefits against those of other energy sources, particularly fossil fuels, to achieve decarbonization goals.
% TRANSFER_FUNCTION: Transfers the burden of long-term nuclear waste management and low-probability, high-consequence risks to local communities and future generations, in exchange for immediate climate benefits and energy security for current populations.
% ABSENT_VOICES: Future generations and the ecosystems that will bear the long-term burden of nuclear waste are structurally absent from the direct decision-making process, their interests represented by advocates. Their concerns about irreversible contamination and intergenerational equity are often marginalized by the urgency of climate action.
% DISAPPEARANCE_RATIONALE: If this comparative risk framework vanished, nuclear power expansion would likely halt or significantly slow, as its risks would be evaluated against absolute safety thresholds rather than relative to fossil fuels. This would force a re-evaluation of decarbonization strategies, potentially leading to increased reliance on other energy sources or slower climate action.
% FOUNDING_PROBLEM: The dual challenge of energy security and climate change mitigation, requiring large-scale, low-carbon energy sources while managing inherent risks.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists, international energy agencies, and many governments corroborate the live status of the energy security and climate change problems. Environmental groups and local communities near waste sites corroborate the problem of managing nuclear risks, but contest the 'acceptable' framing.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).
:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the transfer of long-term risk burdens to specific populations and future generations, which is a cost of this comparative framing. Suppression (0.30) is moderate, as there is active debate, but the urgency of climate change often suppresses arguments for absolute risk thresholds. Theater ratio is low (0.10) because the comparative risk assessment is a genuine, if contested, analytical framework, not primarily performative. The claimed type is 'rope' because it genuinely coordinates a complex policy problem, even if it involves significant extraction from certain seats.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading see it as a pragmatic and necessary approach to a global crisis, where the benefits of climate action outweigh the specific nuclear risks. Opponents, particularly those focused on intergenerational equity or catastrophic tail risks, view it as an unacceptable externalization of risk. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear energy proponents and climate-vulnerable populations are beneficiaries (low d) as this framework supports their goals. Local communities near waste sites and future generations are targets (high d) as they bear the costs of this risk assessment approach. Regulators and the fossil fuel industry are observers or excluded, with their directionality determined by their specific roles and interests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_equity_weighting,
    'How should the risks borne by future generations (e.g., nuclear waste) be weighted against immediate climate benefits for current generations?',
    'Development of intergenerational ethical frameworks with broad societal consensus, or legal precedents establishing rights for future generations.',
    'A higher weighting for future generations'' risks would increase the perceived extractiveness of this reading, potentially shifting its classification towards a Snare or Tangled Rope due to the long-term burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_equity_weighting, preference, 'Ambiguity in weighting intergenerational risks versus immediate benefits.').

omega_variable(
    absolute_vs_relative_risk_framing,
    'Is it conceptually sound to assess nuclear risk purely relative to other energy sources, or should there be an absolute, irreducible threshold for catastrophic risk regardless of alternatives?',
    'Philosophical and ethical debate leading to a consensus on the nature of ''acceptable'' risk, or a shift in public perception and policy towards precautionary principles.',
    'If an absolute threshold is deemed necessary, this comparative reading would be seen as structurally flawed, potentially reclassifying it as a Snare (if the coordination story is cover) or a Piton (if the framework persists despite its conceptual flaws).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_vs_relative_risk_framing, conceptual, 'Conceptual debate over absolute vs. relative risk assessment.').

omega_variable(
    climate_vulnerability_as_beneficiary,
    'Is it appropriate to classify ''climate_vulnerable_populations'' as beneficiaries of a nuclear energy policy, given that they may also bear other risks from nuclear power?',
    'Detailed empirical studies on the net benefit/harm to these populations, considering all direct and indirect impacts of both climate change and nuclear energy.',
    'If the net harm from nuclear power to these populations is found to be significant, their classification as beneficiaries would be challenged, increasing the overall perceived extractiveness and potentially shifting the constraint''s type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_vulnerability_as_beneficiary, empirical, 'Whether climate-vulnerable populations are net beneficiaries of nuclear power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0, 0.08).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 5, 0.09).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 10, 0.1).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 15, 0.1).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'acceptable_risk_for_energy' kernel. This 'comparative_risk_dominant' reading emphasizes the relative benefits of nuclear power against fossil fuels and climate change, influencing how other risk assessment frameworks are applied.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
