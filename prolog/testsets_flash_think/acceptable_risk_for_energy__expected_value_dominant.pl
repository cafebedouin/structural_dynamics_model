% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__expected_value_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Expected Value Dominant Risk Assessment for Energy
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint represents one reading of how 'acceptable risk' for
 *   energy projects, particularly nuclear, is determined. From this
 *   'expected_value_dominant' perspective, the acceptability hinges on annual
 *   expected costs and climate benefits, with rare, high-consequence events
 *   weighted by their probability. This framework aims to provide a rational,
 *   quantitative basis for decision-making, often making nuclear energy
 *   appear more favorable compared to alternatives when climate benefits are
 *   factored in and tail risks are mathematically discounted. The claim is
 *   'rope' because it functions as a coordination mechanism for risk
 *   assessment, but its specific weighting can lead to perceived extraction
 *   by those whose concerns are downplayed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.35).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.4).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.35).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected Value Dominant Risk Assessment for Energy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "risk_assessment/energy_policy/public_safety_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, '2d56cdbe-f73b-4f00-b4ac-a0235de1efaa').
narrative_ontology:cs_kernel_codification('2d56cdbe-f73b-4f00-b4ac-a0235de1efaa', formalized).
narrative_ontology:cs_authority_grounding('2d56cdbe-f73b-4f00-b4ac-a0235de1efaa', expertise).
narrative_ontology:cs_interpretation_layer_present('2d56cdbe-f73b-4f00-b4ac-a0235de1efaa').
narrative_ontology:cs_reading_relation('2d56cdbe-f73b-4f00-b4ac-a0235de1efaa', acceptable_risk_for_energy__catastrophic_tail_dominant, forecloses).
narrative_ontology:cs_reading_relation('2d56cdbe-f73b-4f00-b4ac-a0235de1efaa', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('2d56cdbe-f73b-4f00-b4ac-a0235de1efaa', foundational, risk_quantifiable_by_expected_value).
narrative_ontology:cs_axiom_status(risk_quantifiable_by_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('2d56cdbe-f73b-4f00-b4ac-a0235de1efaa', risk_quantifiable_by_expected_value, empirically_contingent).
narrative_ontology:cs_axiom('2d56cdbe-f73b-4f00-b4ac-a0235de1efaa', foundational, climate_benefits_monetizable_and_comparable).
narrative_ontology:cs_axiom_status(climate_benefits_monetizable_and_comparable, holdable).
narrative_ontology:cs_axiom_grounding('2d56cdbe-f73b-4f00-b4ac-a0235de1efaa', climate_benefits_monetizable_and_comparable, empirically_contingent).
narrative_ontology:cs_reference_frame('2d56cdbe-f73b-4f00-b4ac-a0235de1efaa', rational_risk_optimization).
narrative_ontology:cs_drift_state('2d56cdbe-f73b-4f00-b4ac-a0235de1efaa', contemporary_policy_debate, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('2d56cdbe-f73b-4f00-b4ac-a0235de1efaa', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_energy_proponents).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, quantitative_risk_analysts).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, environmental_advocates_climate_focus).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, environmental_advocates_tail_risk_focus).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, local_communities_near_sites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for nuclear energy projects, finding this risk framework favorable as it allows for a quantitative assessment that often highlights the climate benefits and manageable expected costs, making their projects more acceptable.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_energy_proponents, beneficiary,
    powerful, generational, mobile, global).

% Develop and apply the methodologies for calculating expected costs, benefits, and probability-weighted rare events. Their expertise is central to the operation of this constraint, and they provide the 'objective' basis for policy decisions.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, quantitative_risk_analysts, agenda_setter,
    institutional, biographical, arbitrage, global).

% Prioritize climate change mitigation and may accept nuclear energy as a necessary tool, provided its expected risks are low and climate benefits are high. This framework aligns with their goal of reducing carbon emissions.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, environmental_advocates_climate_focus, beneficiary,
    organized, generational, constrained, global).

% Focus on the catastrophic potential of low-probability, high-consequence events (e.g., nuclear accidents, long-term waste storage). While their concerns are included in the calculation, they feel their weight is systematically undervalued by the probability-weighting, effectively making them 'pay' by having their primary concerns discounted.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, environmental_advocates_tail_risk_focus, payer,
    organized, generational, constrained, global).

% Bear the direct, localized risks of energy projects (e.g., proximity to nuclear plants, waste disposal sites). While the framework deems these risks acceptable in an expected value sense, they experience the residual risk and potential long-term burdens, often with limited agency to influence decisions.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, local_communities_near_sites, payer,
    powerless, generational, trapped, local).

% Utilize this framework to justify energy policy decisions, balancing economic development, climate goals, and public safety. They rely on the quantitative outputs to present a 'rational' basis for their choices, often mediating between competing stakeholder interests.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, policymakers, agenda_setter,
    institutional, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, quantitative method for assessing and comparing the risks and benefits of different energy technologies, enabling policymakers to make decisions based on a common metric of 'acceptability'.
% TRANSFER_FUNCTION: Transfers the burden of accepting probability-weighted rare event risks and long-term waste disposal challenges to local communities and future generations, in exchange for immediate energy and climate benefits, primarily to current populations and energy industries.
% ABSENT_VOICES: Future generations, who will inherit long-lived waste and potential catastrophic risks, are structurally absent from the decision-making process, though their interests are theoretically represented in long-term risk calculations. Indigenous communities whose lands are considered for waste disposal often face systemic exclusion.
% DISAPPEARANCE_RATIONALE: If this framework vanished, energy policy decisions would lose a primary quantitative justification. The debate would immediately shift to qualitative assessments, moral arguments about intergenerational equity, and unweighted catastrophic risk, leading to a complete reorganization of how energy projects are evaluated and approved.
% FOUNDING_PROBLEM: How to rationally evaluate and compare the complex, long-term, and uncertain risks and benefits of large-scale energy projects (especially nuclear) to inform public policy and investment decisions.
% FOUNDING_PROBLEM_CORROBORATION: Academic literature in risk assessment and engineering, international energy agencies (e.g., IAEA), and government reports consistently attest to the ongoing need for such frameworks. While the specific weighting is contested, the underlying problem of complex risk evaluation remains live.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_dominant_tests).
:- end_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate because while the framework provides a coordination function, it imposes a specific calculus that can systematically undervalue certain types of risk (e.g., unquantifiable dread risk) from the perspective of some stakeholders. Suppression (0.40) is moderate-low; tail-risk concerns are not actively silenced but are re-weighted within the framework, which can feel like suppression to those who believe such risks should dominate. Theater ratio is low (0.10) as the framework is a genuine analytical tool, not primarily performative. The temporal measurements show relative stability, reflecting the entrenchment of this quantitative approach in policy circles.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear proponents, this framework is a fair and rational way to assess risk, enabling necessary energy transitions. From the perspective of tail-risk advocates, it systematically downplays existential threats, effectively extracting their safety concerns in favor of a calculated 'acceptable' risk. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear energy proponents and climate-focused environmental advocates are beneficiaries, as the framework's weighting often supports their objectives. Quantitative risk analysts are agenda-setters, as their expertise defines the framework. Tail-risk environmental advocates and local communities are payers, as their concerns are either discounted or they bear the residual risks deemed 'acceptable' by the framework. Policymakers act as agenda-setters, using the framework to guide decisions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    probability_consequence_accuracy,
    'Are the assigned probabilities and consequences for rare events accurate and robust, especially for long-term, complex systems like nuclear waste disposal?',
    'Long-term empirical observation, independent expert consensus on complex systems modeling, and validation against historical ''black swan'' events.',
    'If probabilities/consequences are systematically underestimated, the framework''s calculated ''acceptability'' is flawed, leading to higher effective extraction from victims and a reclassification towards Snare or Tangled Rope. If robust, the Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probability_consequence_accuracy, empirical, 'Accuracy of rare event probability and consequence weighting.').

omega_variable(
    social_acceptability_of_expected_value,
    'Does the public and affected communities genuinely accept the ''probability × consequence'' product as a sufficient measure of risk, or do they demand a lower threshold for catastrophic events?',
    'Sociological studies of risk perception, public referenda, and direct engagement with affected communities to gauge their ''willingness to accept'' specific risk profiles.',
    'If social acceptability is lower than the calculated expected value, the framework''s legitimacy erodes, increasing resistance and potentially shifting its classification towards a Snare (if enforced despite public rejection) or Piton (if maintained theatrically).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_acceptability_of_expected_value, preference, 'Gap between calculated and socially acceptable risk.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''expected_value_dominant'' reading of the ''acceptable_risk_for_energy'' kernel?',
    'Review of foundational texts and policy documents to confirm the explicit or implicit dominance of expected value calculations over other risk assessment methodologies.',
    'If misidentified, the analysis of inter-reading relations and axiom conflicts would be incorrect, leading to a flawed understanding of the broader kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being analyzed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(acce_tr_t2005, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(acce_tr_t2015, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(acce_tr_t2020, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(acce_tr_t2025, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(acce_tr_t2030, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2030, 0.1).

% Extraction over time
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(acce_be_t2005, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2005, 0.32).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement(acce_be_t2015, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2015, 0.34).
narrative_ontology:measurement(acce_be_t2020, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement(acce_be_t2025, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2025, 0.35).
narrative_ontology:measurement(acce_be_t2030, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2030, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(acce_su_t2005, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2005, 0.37).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement(acce_su_t2015, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2015, 0.39).
narrative_ontology:measurement(acce_su_t2020, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(acce_su_t2025, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2025, 0.4).
narrative_ontology:measurement(acce_su_t2030, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2030, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'acceptable_risk_for_energy' kernel. The other readings are 'catastrophic_tail_dominant' and 'comparative_risk_dominant'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
