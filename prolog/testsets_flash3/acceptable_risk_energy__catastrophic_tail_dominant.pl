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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail-Dominant Energy Risk Assessment
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint describes a specific reading of 'acceptable risk' in
 *   energy policy, where avoiding low-probability, high-impact catastrophic
 *   events (like nuclear accidents) takes precedence over minimizing higher
 *   aggregate, but more diffuse, harms (like those from fossil fuels). This
 *   framework leads to a high suppression of nuclear pathways and a
 *   discounting of distributed fossil fuel deaths. The constraint is claimed
 *   as a Tangled Rope because it offers a coordination function (managing
 *   public fear) but also involves significant asymmetric extraction
 *   (disproportionate costs on nuclear, uncounted costs of fossil fuels).
 *
 * KEY AGENTS:
 *   - fossil_fuel_industry: Primary beneficiary (institutional/arbitrage)
 *   - risk_averse_public: Beneficiary (organized/constrained)
 *   - regulatory_authorities: Agenda-setter (institutional/constrained)
 *   - nuclear_energy_proponents: Payer (powerful/constrained)
 *   - populations_affected_by_aggregate_harm: Payer (powerless/trapped)
 *   - decision_theorists: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.75).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic Tail-Dominant Energy Risk Assessment").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, 'e8bf7a99-797b-4a00-8e03-38660dfd1896').
narrative_ontology:cs_kernel_codification('e8bf7a99-797b-4a00-8e03-38660dfd1896', formalized).
narrative_ontology:cs_authority_grounding('e8bf7a99-797b-4a00-8e03-38660dfd1896', extraction).
narrative_ontology:cs_interpretation_layer_present('e8bf7a99-797b-4a00-8e03-38660dfd1896').
narrative_ontology:cs_reading_relation('e8bf7a99-797b-4a00-8e03-38660dfd1896', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('e8bf7a99-797b-4a00-8e03-38660dfd1896', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('e8bf7a99-797b-4a00-8e03-38660dfd1896', foundational, catastrophic_risk_infinite_weight).
narrative_ontology:cs_axiom_status(catastrophic_risk_infinite_weight, holdable).
narrative_ontology:cs_axiom_grounding('e8bf7a99-797b-4a00-8e03-38660dfd1896', catastrophic_risk_infinite_weight, deontological).
narrative_ontology:cs_axiom('e8bf7a99-797b-4a00-8e03-38660dfd1896', secondary, diffuse_harms_discountable).
narrative_ontology:cs_axiom_status(diffuse_harms_discountable, holdable).
narrative_ontology:cs_axiom_grounding('e8bf7a99-797b-4a00-8e03-38660dfd1896', diffuse_harms_discountable, empirically_contingent).
narrative_ontology:cs_reference_frame('e8bf7a99-797b-4a00-8e03-38660dfd1896', post_chernobyl_risk_aversion).
narrative_ontology:cs_drift_state('e8bf7a99-797b-4a00-8e03-38660dfd1896', contemporary_climate_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e8bf7a99-797b-4a00-8e03-38660dfd1896', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, risk_averse_public).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, regulatory_authorities).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_energy_proponents).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, populations_affected_by_aggregate_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a risk framework that downplays the distributed, long-term harms of its operations while magnifying the risks of alternatives. This allows continued operation and expansion without bearing the full social cost of its emissions and pollution.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Perceives itself as protected from highly visible, low-probability catastrophic events (e.g., nuclear meltdowns), even if this leads to higher aggregate harm from less dramatic sources. Their preference for avoiding 'worst-case' scenarios is reflected in policy.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, risk_averse_public, beneficiary,
    organized, biographical, constrained, national).

% Implement and enforce policies based on this risk assessment framework. They prioritize avoiding politically costly, highly visible disasters, which often means favoring energy sources with diffuse, less visible harms. Their mandate is often shaped by public perception of risk.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, regulatory_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Bear the burden of disproportionate regulatory hurdles, public opposition, and financial disincentives due to the emphasis on catastrophic tail risk. Their pathway is suppressed despite potentially lower aggregate harm metrics.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_energy_proponents, payer,
    powerful, generational, constrained, global).

% Suffer the cumulative, often diffuse, health and environmental consequences of energy policies that prioritize avoiding catastrophic tails over minimizing total expected harm. These harms are often geographically distributed and temporally distant, making collective action difficult.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, populations_affected_by_aggregate_harm, payer,
    powerless, generational, trapped, global).

% Analyze the structural biases in risk assessment frameworks, highlighting the divergence between minimizing catastrophic risk and minimizing expected value. They provide an external critique of the policy choices.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, decision_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public and regulatory focus on a specific class of risks (catastrophic, low-probability events) to achieve a perceived sense of safety and avoid high-profile failures, thereby stabilizing public discourse around energy choices.
% TRANSFER_FUNCTION: Transfers the burden of diffuse, aggregate harm (e.g., from fossil fuels) onto a broader, less visible population, while transferring the costs of heightened regulation and public aversion onto energy pathways with catastrophic tail risks (e.g., nuclear).
% ABSENT_VOICES: Future generations and populations in developing nations, who will disproportionately bear the long-term aggregate harms of current energy choices, are largely absent from the decision-making process. Their interests would advocate for a framework minimizing total expected harm.
% DISAPPEARANCE_RATIONALE: If this risk assessment framework vanished, energy policy would likely shift towards a more aggregate-harm-minimizing approach, potentially leading to a re-evaluation of nuclear power and a more stringent accounting of fossil fuel externalities. This would fundamentally alter investment, regulatory, and public acceptance landscapes for energy technologies.
% FOUNDING_PROBLEM: The need to manage public fear and political fallout from highly visible, low-probability catastrophic events, particularly in the context of nuclear power development after major accidents.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and segments of the public continue to attest to the live nature of catastrophic risk aversion, citing the potential for public panic and political instability from high-profile disasters. However, energy economists and climate scientists (outside the direct beneficiaries) increasingly challenge the framework's overall efficacy in minimizing total societal harm.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.68) reflects the societal cost of prioritizing one type of risk over another, leading to suboptimal outcomes in terms of overall harm reduction. Suppression (0.75) is high due to the active regulatory and public pressure against nuclear energy, and the political difficulty of shifting away from established fossil fuel infrastructure. Theater ratio (0.20) is moderate; while genuine risk assessment occurs, a portion of the effort is performative, reinforcing existing biases rather than objectively comparing all risks. Accessibility collapse (0.60) indicates that alternative risk frameworks are difficult to implement due to entrenched interests and public perception. Resistance (0.45) is present from nuclear proponents and some scientific communities, but not strong enough to overturn the dominant framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the fossil fuel industry and risk-averse public, this framework provides essential coordination by preventing catastrophic outcomes and maintaining stability. From the perspective of nuclear energy proponents and those suffering aggregate harms, it is a highly extractive and suppressive mechanism that distorts rational decision-making. Regulatory authorities navigate these competing perspectives, often leaning towards the politically safer option of avoiding visible catastrophes.
 *
 * DIRECTIONALITY LOGIC:
 *   The fossil fuel industry benefits directly from the framework's bias, making them a clear beneficiary. The risk-averse public also benefits from the perceived safety, even if indirectly. Regulatory authorities, as agenda-setters, benefit from maintaining public trust and avoiding political crises. Nuclear energy proponents bear the costs of this framework through suppressed development and increased regulatory burden. Populations affected by aggregate harm are victims, as their suffering is systematically undervalued. Decision theorists act as analytical observers, providing external critique.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Snare, acknowledging the genuine coordination function of managing public perception of catastrophic risk. However, it highlights that this coordination comes with significant, asymmetric extraction. The framework's mandate to prevent catastrophe is still 'live,' but its application has drifted into a form that benefits specific industries and public anxieties at the expense of broader societal well-being. Resolving mandatrophy would require a re-evaluation of the core risk axioms and a shift towards a more holistic harm-minimization approach.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophic_weighting_justification,
    'Is the ''infinite'' weighting of catastrophic tail risks a rational decision-theoretic choice under deep uncertainty, or a cognitive bias amplified by political and media dynamics?',
    'Development of robust decision-making frameworks that explicitly model deep uncertainty and compare outcomes with and without infinite weighting, alongside empirical studies of public risk perception and media influence.',
    'If it''s a rational choice, the extractiveness might be re-evaluated as a necessary cost of robust decision-making. If it''s a bias, the extractiveness is a pure rent, and the constraint is more Snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophic_weighting_justification, conceptual, 'Examines the underlying justification for prioritizing catastrophic tail risks.').

omega_variable(
    fossil_fuel_externalities_accounting,
    'To what extent are the ''diffuse'' and ''reversible'' harms of fossil fuels truly accounted for in policy decisions, and how does this compare to the accounting for nuclear risks?',
    'Comprehensive, independent, and transparent lifecycle assessments of all energy sources, including full externality costs (health, climate, environmental degradation), integrated into regulatory frameworks.',
    'If fossil fuel externalities are significantly undercounted, the extractiveness of this constraint is higher than currently estimated, and the ''fossil_fuel_industry'' beneficiary role is more pronounced. This would push the classification closer to a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fossil_fuel_externalities_accounting, empirical, 'Assesses the completeness and fairness of harm accounting across energy sources.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of nuclear pathways structural (regulatory barriers, financial disincentives) or internalized (public fear, identity-based opposition)?',
    'Post-policy-change trajectory: if suppression persists after structural barriers are removed (e.g., through new legislation), reclassify as partially internalized. Public opinion surveys and sociological studies on risk perception would also inform this.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as public opposition would continue to hinder nuclear development even with favorable policy. This would make the constraint more resilient to policy changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for nuclear energy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1986, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 1986, 0.1).
narrative_ontology:measurement_basis(acce_tr_t1986, observed).
narrative_ontology:measurement(acce_tr_t1995, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 1995, 0.15).
narrative_ontology:measurement_basis(acce_tr_t1995, observed).
narrative_ontology:measurement(acce_tr_t2005, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2005, 0.18).
narrative_ontology:measurement_basis(acce_tr_t2005, observed).
narrative_ontology:measurement(acce_tr_t2015, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2015, 0.2).
narrative_ontology:measurement_basis(acce_tr_t2015, observed).
narrative_ontology:measurement(acce_tr_t2024, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2024, 0.2).
narrative_ontology:measurement_basis(acce_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(acce_be_t1986, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 1986, 0.55).
narrative_ontology:measurement_basis(acce_be_t1986, observed).
narrative_ontology:measurement(acce_be_t1995, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement_basis(acce_be_t1995, observed).
narrative_ontology:measurement(acce_be_t2005, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement_basis(acce_be_t2005, observed).
narrative_ontology:measurement(acce_be_t2015, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement_basis(acce_be_t2015, observed).
narrative_ontology:measurement(acce_be_t2024, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(acce_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1986, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 1986, 0.6).
narrative_ontology:measurement_basis(acce_su_t1986, observed).
narrative_ontology:measurement(acce_su_t1995, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement_basis(acce_su_t1995, observed).
narrative_ontology:measurement(acce_su_t2005, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2005, 0.72).
narrative_ontology:measurement_basis(acce_su_t2005, observed).
narrative_ontology:measurement(acce_su_t2015, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement_basis(acce_su_t2015, observed).
narrative_ontology:measurement(acce_su_t2024, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2024, 0.75).
narrative_ontology:measurement_basis(acce_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
