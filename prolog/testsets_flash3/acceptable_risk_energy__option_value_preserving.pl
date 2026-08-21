% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__option_value_preserving, []).

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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Option Value Preserving Acceptable Risk in Energy Policy
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint represents a reading of 'acceptable risk' in energy
 *   policy that prioritizes maintaining a diverse portfolio of energy
 *   pathways (e.g., nuclear, fossil, renewables) to preserve decision
 *   flexibility under conditions of deep uncertainty. It explicitly avoids
 *   premature closure on any single pathway, even if it means tolerating
 *   higher short-term costs or risks. This reading contrasts with those that
 *   prioritize minimizing expected harm or avoiding catastrophic tails.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.45).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.6).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.45).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Option Value Preserving Acceptable Risk in Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__option_value_preserving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, 'a6a227f6-fd82-4d79-aa96-108b28e3d3bb').
narrative_ontology:cs_kernel_codification('a6a227f6-fd82-4d79-aa96-108b28e3d3bb', formalized).
narrative_ontology:cs_authority_grounding('a6a227f6-fd82-4d79-aa96-108b28e3d3bb', expertise).
narrative_ontology:cs_interpretation_layer_present('a6a227f6-fd82-4d79-aa96-108b28e3d3bb').
narrative_ontology:cs_reading_relation('a6a227f6-fd82-4d79-aa96-108b28e3d3bb', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('a6a227f6-fd82-4d79-aa96-108b28e3d3bb', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('a6a227f6-fd82-4d79-aa96-108b28e3d3bb', foundational, flexibility_has_intrinsic_value).
narrative_ontology:cs_axiom_status(flexibility_has_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('a6a227f6-fd82-4d79-aa96-108b28e3d3bb', flexibility_has_intrinsic_value, instrumental).
narrative_ontology:cs_axiom('a6a227f6-fd82-4d79-aa96-108b28e3d3bb', foundational, irreversibility_is_a_primary_risk).
narrative_ontology:cs_axiom_status(irreversibility_is_a_primary_risk, holdable).
narrative_ontology:cs_axiom_grounding('a6a227f6-fd82-4d79-aa96-108b28e3d3bb', irreversibility_is_a_primary_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('a6a227f6-fd82-4d79-aa96-108b28e3d3bb', adaptive_management_paradigm).
narrative_ontology:cs_drift_state('a6a227f6-fd82-4d79-aa96-108b28e3d3bb', contemporary_climate_crisis, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a6a227f6-fd82-4d79-aa96-108b28e3d3bb', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, future_decision_makers).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, energy_security_advocates).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, single_pathway_advocates).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, premature_decommissioning_targets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a wider array of viable energy options in the future, allowing for adaptation to unforeseen technological, environmental, or geopolitical shifts. They are the conceptual beneficiaries of preserved flexibility.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, future_decision_makers, beneficiary,
    analytical, generational, analytical, global).

% Advocate for maintaining diverse energy sources (e.g., fossil fuels, nuclear, renewables) to ensure resilience against supply disruptions or technological failures. They benefit from policies that prevent over-reliance on any single pathway.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, energy_security_advocates, beneficiary,
    organized, generational, constrained, national).

% Advocate for rapid transition to a single dominant energy source (e.g., 100% renewables or all-nuclear). They bear the cost of maintaining 'suboptimal' or 'legacy' pathways that this constraint requires, seeing it as a diversion of resources from their preferred solution.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, single_pathway_advocates, payer,
    powerful, biographical, constrained, national).

% These are existing energy infrastructure (e.g., nuclear plants, fossil fuel plants) that would be decommissioned under a single-pathway strategy but are kept operational to preserve options. They bear the cost of continued operation, maintenance, and regulatory compliance, often facing public opposition.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, premature_decommissioning_targets, payer,
    moderate, immediate, trapped, local).

% Would argue that maintaining fossil fuel pathways, even for option value, imposes unacceptable environmental costs and forecloses the option of a livable planet. Their voice is often marginalized in decision-making focused on energy security and economic flexibility.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, environmental_activists, excluded,
    organized, generational, constrained, global).

% Analyze the costs and benefits of maintaining diverse energy portfolios, including the economic value of flexibility versus the direct costs of infrastructure. They provide data and models that inform policy decisions.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, economic_planners, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates long-term energy policy to avoid irreversible commitments to single energy pathways, ensuring that future generations retain the flexibility to adapt to evolving knowledge and circumstances regarding energy technologies and climate impacts.
% TRANSFER_FUNCTION: Transfers resources (investment, maintenance, regulatory support) to maintain a diverse portfolio of energy infrastructure, from those advocating for rapid, singular transitions to those valuing long-term optionality and resilience.
% ABSENT_VOICES: Strong advocates for immediate, singular energy transitions (e.g., rapid fossil fuel phase-out or nuclear-only expansion) are often excluded from the core 'option value' framing, as their positions inherently reduce flexibility. Environmental justice communities who bear the localized burdens of maintained 'legacy' infrastructure are also often absent.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, energy policy would likely polarize towards rapid, singular transitions (e.g., immediate fossil fuel phase-out or accelerated nuclear build-out), leading to premature decommissioning of some assets and potentially foreclosing future options. The long-term strategic landscape would be significantly altered.
% FOUNDING_PROBLEM: The problem of deep uncertainty in long-term energy planning: inability to predict future technological breakthroughs, climate impacts, resource availability, or geopolitical shifts, making irreversible commitments to a single energy pathway highly risky.
% FOUNDING_PROBLEM_CORROBORATION: Decision theorists, national security strategists, and intergovernmental panels (e.g., IPCC scenarios that include diverse energy mixes) corroborate the ongoing challenge of deep uncertainty in energy futures, supporting the live status of the founding problem. This corroboration comes from outside the direct beneficiaries of specific energy pathways.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__option_value_preserving, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__option_value_preserving, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(acceptable_risk_energy__option_value_preserving, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__option_value_preserving, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__option_value_preserving_tests).
:- end_tests(acceptable_risk_energy__option_value_preserving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the opportunity costs and direct expenses of maintaining pathways that might be considered suboptimal by other criteria (e.g., continued operation of fossil fuel plants). Suppression (0.6) is moderate, as it involves actively resisting political and economic pressures for rapid, singular transitions. Theater ratio is low (0.1) because the policy genuinely aims to preserve options, not merely to appear to do so. The constraint is claimed as a Rope because it provides a genuine coordination function (managing long-term uncertainty) with net benefits for future decision-makers, despite imposing costs on current advocates of single pathways.
 *
 * PERSPECTIVAL GAP:
 *   Advocates for a single, rapid energy transition would experience this constraint as highly extractive, diverting resources and delaying their preferred future. Conversely, those focused on long-term strategic resilience and adaptability would see it as a necessary and beneficial coordination mechanism. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Future decision-makers and energy security advocates are beneficiaries, as they gain flexibility and resilience. Advocates for single pathways and targets for premature decommissioning are payers, bearing the costs of maintaining diversity. Environmental activists are excluded, as their concerns about the costs of maintaining certain options are often sidelined by the 'flexibility' argument.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantifying_option_value,
    'How can the ''option value'' of maintaining diverse energy pathways be rigorously quantified and compared against the direct costs and risks of those pathways?',
    'Development of robust, interdisciplinary decision-making frameworks that integrate real options analysis, deep uncertainty methods, and multi-criteria evaluation, with empirical validation against historical policy outcomes.',
    'A clear quantification would strengthen the justification for this reading, potentially reducing perceived extractiveness by demonstrating the tangible benefits of flexibility. Lack of quantification leaves it vulnerable to critiques from expected-value or catastrophic-tail perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantifying_option_value, empirical, 'The challenge of rigorously valuing decision flexibility in energy policy.').

omega_variable(
    threshold_of_unacceptable_risk,
    'At what point does the ''acceptable risk'' of maintaining a pathway (e.g., fossil fuels due to climate change) outweigh its option value, making its continued viability an unacceptable cost?',
    'Societal deliberation and scientific consensus on irreversible environmental thresholds, combined with updated economic models that fully internalize long-term externalities.',
    'Defining such a threshold would shift the balance of the constraint, potentially leading to the foreclosure of certain pathways even under an option-value framework, thus reducing the victim set for environmental advocates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_of_unacceptable_risk, preference, 'The ethical and scientific boundary for ''acceptable'' risk in option-value preservation.').

omega_variable(
    framing_of_uncertainty,
    'Is the ''deep uncertainty'' framing a genuine reflection of epistemic limits, or a rhetorical device to justify maintaining status quo energy pathways?',
    'Analysis of policy discourse and decision-making processes to identify whether uncertainty is genuinely explored or selectively invoked, and comparison with expert elicitation on the reducibility of key uncertainties.',
    'If primarily rhetorical, the constraint''s coordination function is weaker, and its extractiveness (costs of maintaining ''suboptimal'' pathways) is less justified, potentially reclassifying it towards a Tangled Rope or Snare for single-pathway advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_uncertainty, conceptual, 'Whether ''deep uncertainty'' is an epistemic fact or a political framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__option_value_preserving, theater_ratio, 0, 0.05).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__option_value_preserving, theater_ratio, 10, 0.07).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__option_value_preserving, theater_ratio, 20, 0.08).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_energy__option_value_preserving, theater_ratio, 30, 0.09).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__option_value_preserving, theater_ratio, 40, 0.1).
narrative_ontology:measurement(acce_tr_t50, acceptable_risk_energy__option_value_preserving, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__option_value_preserving, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__option_value_preserving, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__option_value_preserving, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_energy__option_value_preserving, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__option_value_preserving, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(acce_be_t50, acceptable_risk_energy__option_value_preserving, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__option_value_preserving, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__option_value_preserving, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__option_value_preserving, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_energy__option_value_preserving, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__option_value_preserving, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(acce_su_t50, acceptable_risk_energy__option_value_preserving, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, energy_infrastructure_investment_policy).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, climate_mitigation_targets).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'acceptable_risk_energy' kernel, alongside 'catastrophic_tail_dominant' and 'expected_value_dominant'. Each reading defines 'acceptable risk' differently, leading to distinct policy implications and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
