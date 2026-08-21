% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__catastrophic_tail_dominant, []).

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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Energy Risk Assessment: Catastrophic Tail Dominance
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint describes a dominant approach to energy risk assessment
 *   that prioritizes avoiding low-probability, high-impact catastrophic
 *   outcomes (e.g., nuclear meltdowns) even if it leads to higher expected
 *   aggregate harm (e.g., from fossil fuel emissions). This is one reading of
 *   the broader 'acceptable_risk_energy' kernel. The constraint is claimed as
 *   a Tangled Rope because it offers a coordination function (addressing
 *   public fear of catastrophe) but does so with significant asymmetric
 *   extraction (higher costs, suboptimal energy mixes) and active suppression
 *   of alternatives (e.g., nuclear pathways).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.78).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.85).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.78).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Energy Risk Assessment: Catastrophic Tail Dominance").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, '15ea79a0-87bc-4198-ae28-4d4ebf421e23').
narrative_ontology:cs_kernel_codification('15ea79a0-87bc-4198-ae28-4d4ebf421e23', distributed).
narrative_ontology:cs_authority_grounding('15ea79a0-87bc-4198-ae28-4d4ebf421e23', practice).
narrative_ontology:cs_interpretation_layer_present('15ea79a0-87bc-4198-ae28-4d4ebf421e23').
narrative_ontology:cs_reading_relation('15ea79a0-87bc-4198-ae28-4d4ebf421e23', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('15ea79a0-87bc-4198-ae28-4d4ebf421e23', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('15ea79a0-87bc-4198-ae28-4d4ebf421e23', foundational, catastrophic_risk_intolerable).
narrative_ontology:cs_axiom_status(catastrophic_risk_intolerable, holdable).
narrative_ontology:cs_axiom_grounding('15ea79a0-87bc-4198-ae28-4d4ebf421e23', catastrophic_risk_intolerable, deontological).
narrative_ontology:cs_axiom('15ea79a0-87bc-4198-ae28-4d4ebf421e23', foundational, maximin_principle_applies_to_energy_risk).
narrative_ontology:cs_axiom_status(maximin_principle_applies_to_energy_risk, holdable).
narrative_ontology:cs_axiom_grounding('15ea79a0-87bc-4198-ae28-4d4ebf421e23', maximin_principle_applies_to_energy_risk, instrumental).
narrative_ontology:cs_reference_frame('15ea79a0-87bc-4198-ae28-4d4ebf421e23', post_chernobyl_fukushima_risk_aversion).
narrative_ontology:cs_drift_state('15ea79a0-87bc-4198-ae28-4d4ebf421e23', contemporary_climate_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('15ea79a0-87bc-4198-ae28-4d4ebf421e23', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, risk_averse_public).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_industry).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, energy_consumers).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, proponents_of_expected_value_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforce risk assessment frameworks that prioritize avoiding low-probability, high-impact catastrophic events. They gain legitimacy from public safety concerns and manage the political fallout of any perceived catastrophic risk.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the perceived safety of avoiding catastrophic events, particularly nuclear accidents, even if it means higher energy costs or reliance on energy sources with higher aggregate, but distributed, harms. Their fears are directly addressed by this framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, risk_averse_public, beneficiary,
    organized, biographical, constrained, national).

% Bear the costs of energy policies shaped by this risk framework, which can include higher electricity prices, reduced energy independence, or reliance on less efficient or more polluting energy sources if nuclear is suppressed.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, energy_consumers, payer,
    moderate, biographical, constrained, national).

% Faces significant regulatory hurdles, public opposition, and investment challenges due to the emphasis on low-probability catastrophic risks, despite its low aggregate mortality per TWh. Their pathway is actively suppressed.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry, payer,
    organized, generational, constrained, national).

% Benefits from the suppression of nuclear energy, as it reduces a major competitor. The distributed, long-term harms of fossil fuels (e.g., air pollution, climate change) are often discounted or framed as reversible within this risk framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Advocate for risk assessment based on minimizing aggregate expected harm (e.g., mortality per TWh) across all energy pathways. Their arguments are often marginalized or dismissed in policy debates dominated by catastrophic tail risk aversion.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, proponents_of_expected_value_risk, excluded,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework for energy policy and infrastructure planning by establishing a clear, albeit contested, priority for avoiding low-probability, high-impact catastrophic events, thereby coordinating public and political anxieties.
% TRANSFER_FUNCTION: Transfers resources (e.g., higher energy costs, foregone optimal energy mixes, investment in less efficient alternatives) from energy consumers and nuclear energy proponents to a perceived reduction in catastrophic risk and to industries whose risks are discounted (e.g., fossil fuels).
% ABSENT_VOICES: Economists and risk analysts advocating for expected value approaches, and populations disproportionately affected by the distributed harms of non-nuclear energy sources (e.g., air pollution, climate change impacts) whose risks are downplayed by this framework.
% DISAPPEARANCE_RATIONALE: If this risk framework vanished overnight, energy policy would undergo a significant re-evaluation. Nuclear power would likely see a resurgence, fossil fuel reliance would be challenged more directly on aggregate harm, and investment decisions would shift to optimize for overall efficiency and lower total mortality, rather than just avoiding tail risks.
% FOUNDING_PROBLEM: Public and political anxieties following highly visible, catastrophic nuclear accidents (e.g., Chernobyl, Fukushima) created a demand for risk frameworks that explicitly prioritized avoiding such events, regardless of their low probability or the higher aggregate harm of alternatives.
% FOUNDING_PROBLEM_CORROBORATION: Public opinion polls consistently show high aversion to nuclear risk, media coverage amplifies catastrophic events, and political discourse frequently invokes 'worst-case scenarios' in energy debates. This is corroborated by independent social science research on risk perception and political science analysis of energy policy formation.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the policy choices driven by this framework often result in higher overall societal costs and harms than alternative risk management strategies. Suppression is very high (0.85) due to the active regulatory and political barriers placed on energy sources with catastrophic tail risks, regardless of their aggregate safety profile. Theater ratio is moderate (0.45) as some of the emphasis on tail risk serves to manage public perception and political agendas, rather than solely reflecting a rational, comprehensive risk assessment. Accessibility collapse is high (0.70) for energy pathways with perceived catastrophic risks, as they are effectively excluded or heavily constrained.
 *
 * PERSPECTIVAL GAP:
 *   Regulatory bodies and the risk-averse public perceive this as a necessary coordination mechanism for public safety, a 'Rope' that protects society from unacceptable risks. However, energy consumers and the nuclear industry experience it as a 'Snare' or 'Tangled Rope' due to the significant costs, foregone opportunities, and active suppression of alternatives, which they argue leads to higher aggregate harm.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies (agenda_setter) and the risk-averse public (beneficiary) benefit from this framework by gaining legitimacy and perceived safety, respectively. The fossil fuel industry also benefits by having a major competitor (nuclear) suppressed. Energy consumers and the nuclear industry are the primary payers/victims, bearing higher costs and facing existential challenges. Proponents of expected value risk assessment are excluded, as their analytical framework is not adopted.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_weighting_objectivity,
    'Is the differential weighting of catastrophic vs. distributed harms in energy policy objectively justified by risk science, or is it primarily a socio-political construct?',
    'Cross-cultural comparative studies of risk perception and policy outcomes, combined with expert elicitation on the scientific basis for weighting different types of harm.',
    'If primarily a socio-political construct, the ''naturalness'' of this constraint is undermined, increasing its perceived extractiveness and suppression. If objectively justified, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_weighting_objectivity, conceptual, 'Whether the weighting of different harm types is objective or constructed.').

omega_variable(
    nuclear_suppression_necessity,
    'Is the high suppression of nuclear energy pathways a necessary consequence of genuinely unmanageable catastrophic risk, or an overreaction driven by public perception and political expediency?',
    'Independent, comprehensive risk assessments comparing nuclear to other energy sources on a full lifecycle basis, including waste management and accident probabilities, without pre-weighting catastrophic outcomes.',
    'If suppression is an overreaction, the constraint''s extractiveness and theater_ratio would be higher, and its classification would lean more towards Snare. If necessary, the coordination function is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_suppression_necessity, empirical, 'Necessity of nuclear energy pathway suppression.').

omega_variable(
    reversibility_of_distributed_harms,
    'Are the distributed harms from alternative energy sources (e.g., fossil fuels) truly reversible or manageable in the long term, or are their cumulative impacts underestimated by this framework?',
    'Longitudinal epidemiological studies on health impacts of air pollution, and climate modeling with updated feedback loops, to assess the true reversibility and long-term costs of distributed harms.',
    'If distributed harms are underestimated and less reversible, the ''higher expected aggregate harm'' becomes more salient, increasing the perceived extractiveness of this constraint from the perspective of those bearing those harms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_of_distributed_harms, empirical, 'True reversibility and long-term impact of distributed harms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(acce_tr_t2005, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(acce_tr_t2015, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2015, 0.43).
narrative_ontology:measurement(acce_tr_t2020, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2020, 0.45).
narrative_ontology:measurement(acce_tr_t2025, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2025, 0.45).
narrative_ontology:measurement(acce_tr_t2030, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2030, 0.45).

% Extraction over time
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(acce_be_t2005, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2010, 0.72).
narrative_ontology:measurement(acce_be_t2015, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(acce_be_t2020, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2020, 0.77).
narrative_ontology:measurement(acce_be_t2025, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2025, 0.78).
narrative_ontology:measurement(acce_be_t2030, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2030, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(acce_su_t2005, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(acce_su_t2015, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2015, 0.83).
narrative_ontology:measurement(acce_su_t2020, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2020, 0.84).
narrative_ontology:measurement(acce_su_t2025, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2025, 0.85).
narrative_ontology:measurement(acce_su_t2030, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2030, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, energy_infrastructure_investment).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, public_health_policy).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, climate_change_mitigation_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
