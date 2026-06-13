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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Expected Value Dominant Risk Assessment for Energy Projects
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint represents the 'expected value dominant' reading of
 *   acceptable risk in energy policy, where annual expected costs and climate
 *   benefits are the primary determinants of acceptability, and rare events
 *   are weighted by their probability-consequence product. This framework
 *   tends to favor technologies with low expected risks, even if they carry
 *   high-consequence, low-probability tail risks. It is a 'rope' because it
 *   genuinely coordinates complex risk assessment, but its specific weighting
 *   scheme can lead to a subtle, diffuse form of extraction by externalizing
 *   tail risks onto specific populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.3).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.2).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.3).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected Value Dominant Risk Assessment for Energy Projects").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "risk_assessment/energy_policy/public_safety_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, '949d2497-1696-4f04-ac6e-9c57b77b6a1b').
narrative_ontology:cs_kernel_codification('949d2497-1696-4f04-ac6e-9c57b77b6a1b', formalized).
narrative_ontology:cs_authority_grounding('949d2497-1696-4f04-ac6e-9c57b77b6a1b', expertise).
narrative_ontology:cs_interpretation_layer_present('949d2497-1696-4f04-ac6e-9c57b77b6a1b').
narrative_ontology:cs_reading_relation('949d2497-1696-4f04-ac6e-9c57b77b6a1b', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('949d2497-1696-4f04-ac6e-9c57b77b6a1b', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('949d2497-1696-4f04-ac6e-9c57b77b6a1b', foundational, risk_quantification_by_expected_value).
narrative_ontology:cs_axiom_status(risk_quantification_by_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('949d2497-1696-4f04-ac6e-9c57b77b6a1b', risk_quantification_by_expected_value, empirically_contingent).
narrative_ontology:cs_axiom('949d2497-1696-4f04-ac6e-9c57b77b6a1b', secondary, climate_benefits_outweigh_tail_risks).
narrative_ontology:cs_axiom_status(climate_benefits_outweigh_tail_risks, holdable).
narrative_ontology:cs_axiom_grounding('949d2497-1696-4f04-ac6e-9c57b77b6a1b', climate_benefits_outweigh_tail_risks, instrumental).
narrative_ontology:cs_reference_frame('949d2497-1696-4f04-ac6e-9c57b77b6a1b', rational_economic_decision_theory).
narrative_ontology:cs_drift_state('949d2497-1696-4f04-ac6e-9c57b77b6a1b', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('949d2497-1696-4f04-ac6e-9c57b77b6a1b', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_energy_proponents).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, climate_mitigation_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, economic_planners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, local_communities_near_facilities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a risk framework that normalizes rare events by their probability, making nuclear power's overall risk profile appear favorable compared to fossil fuels or climate change. This allows for project approval and investment.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_energy_proponents, beneficiary,
    organized, generational, mobile, national).

% Supports this framework as it prioritizes the large, certain benefits of reduced carbon emissions over the low-probability, high-consequence risks of nuclear accidents, aligning with their primary goal of climate action.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_mitigation_advocates, beneficiary,
    organized, civilizational, mobile, global).

% Utilizes this framework for cost-benefit analysis in energy infrastructure planning, as it provides a quantifiable method for comparing diverse energy sources and their associated risks and benefits, facilitating rational resource allocation.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, economic_planners, agenda_setter,
    institutional, generational, analytical, national).

% Bears the residual, albeit low-probability, risk of catastrophic events. While the expected value framework deems these risks acceptable, the consequences for these communities, if an event occurs, are disproportionately high and irreversible. They have limited options to avoid these risks once a facility is sited.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, local_communities_near_facilities, payer,
    powerless, generational, trapped, local).

% Would argue that the expected value framework systematically undervalues the disproportionate impact of rare catastrophic events on vulnerable communities, and that the distribution of risk is as important as its aggregate sum. Their concerns are often marginalized in purely quantitative risk assessments.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, environmental_justice_advocates, excluded,
    moderate, generational, constrained, national).

% Enforces the safety standards derived from this risk assessment framework. Their mandate is to ensure that projects meet the 'acceptable risk' thresholds, which are defined by the expected value calculation. They operate within the confines of this established methodology.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, public_safety_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, quantitative method for evaluating and comparing risks and benefits across different energy technologies, enabling consistent policy decisions and resource allocation in complex energy systems.
% TRANSFER_FUNCTION: Transfers the burden of low-probability, high-consequence risks from energy project developers and beneficiaries (who gain from climate benefits and economic growth) to local communities and future generations (who bear the residual risk of rare events).
% ABSENT_VOICES: Advocates for catastrophic tail risk and environmental justice are often marginalized, as their qualitative concerns about irreversibility, intergenerational equity, and disproportionate impacts are difficult to integrate into a purely quantitative expected-value framework. They would argue for a more precautionary approach.
% DISAPPEARANCE_RATIONALE: Without a dominant expected-value framework, energy policy decisions would become highly contentious and fragmented. Projects like nuclear power, which rely on this framework for their justification, would face significant hurdles, leading to a complete re-evaluation of national energy strategies and investment flows.
% FOUNDING_PROBLEM: The need for a rational, consistent, and quantifiable method to compare the diverse risks and benefits of different energy sources, particularly for large-scale infrastructure projects with long-term impacts and complex externalities.
% FOUNDING_PROBLEM_CORROBORATION: Economists, engineers, and international energy agencies corroborate the ongoing need for such a framework to guide energy policy and investment, especially in the context of climate change and growing energy demand. While the specific weighting of risks is debated, the need for a systematic approach is widely accepted outside of purely anti-nuclear or anti-development advocacy groups.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).

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
 *   The extractiveness (0.3) is low but non-zero, reflecting the subtle transfer of unmitigated tail risk to local communities. Suppression (0.2) is also low, as the framework is widely accepted in technical and policy circles, but it does suppress alternative, more precautionary risk framings. Theater ratio (0.1) is minimal, as the framework is genuinely applied in decision-making, not merely for show. Accessibility collapse (0.7) is high because once this framework is adopted, alternative risk assessment methodologies become difficult to implement. Resistance (0.15) is low, primarily from environmental justice groups, but not widespread enough to challenge the framework's dominance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear energy proponents and economic planners, this framework is a neutral, rational tool for optimal resource allocation, a 'rope' that enables necessary energy transitions. From the perspective of local communities near facilities, it can feel like a 'snare' that discounts their existential risks, even if the aggregate expected value is positive. The engine will compute this divergence based on their differing power, exit options, and roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear energy proponents and climate mitigation advocates are beneficiaries (d near 0.0) as the framework supports their goals. Economic planners and public safety regulators are agenda-setters (d near 0.2-0.3), actively applying and enforcing the framework. Local communities near facilities are payers (d near 0.8-0.9) as they bear the unquantified or discounted tail risks. Environmental justice advocates are excluded (d near 1.0) as their concerns are systematically marginalized by the framework's quantitative focus.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy. The founding problem of needing a rational risk assessment framework is still live. The challenge is not that the mandate has atrophied, but that the specific 'expected value dominant' reading of that mandate may be incomplete or biased in its distribution of risk, leading to subtle extraction rather than a pure coordination function. The framework continues to serve its intended purpose of guiding energy policy, but the contestation lies in its specific parameters and ethical implications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophic_tail_risk_weighting,
    'Is the probability × consequence product an adequate weighting for low-probability, high-consequence events, or should such events be weighted more heavily due to irreversibility and intergenerational burden?',
    'Development of a consensus-based ethical framework for intergenerational risk, or a shift in public perception and political will to prioritize precautionary principles over expected utility.',
    'If tail risks are weighted more heavily, nuclear power projects would face higher perceived risks, potentially shifting the classification towards a ''tangled_rope'' or ''snare'' for local communities, and increasing the overall extractiveness of the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophic_tail_risk_weighting, conceptual, 'Adequacy of expected value for catastrophic risks.').

omega_variable(
    framing_of_nuclear_waste_disposal,
    'Is nuclear waste disposal a solvable engineering challenge (as implied by this reading), or an intergenerational burden that fundamentally alters the risk calculus?',
    'Demonstrated long-term, safe, and publicly accepted geological repositories, or a failure to achieve such solutions within a reasonable timeframe.',
    'If waste disposal is proven to be an intractable intergenerational burden, the ''expected_value_dominant'' reading''s benign assessment of nuclear risk would collapse, increasing its extractiveness and suppression, and potentially reclassifying it as a ''tangled_rope'' or ''snare'' for future generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_nuclear_waste_disposal, empirical, 'Status of nuclear waste disposal as a ''solvable'' problem.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''expected_value_dominant'' reading of acceptable risk, or is it a ''catastrophic_tail_dominant'' or ''comparative_risk_dominant'' reading in disguise?',
    'Analysis of policy outcomes and public discourse: if decisions consistently prioritize expected value over tail risk or comparative risk, this reading is confirmed. If not, it suggests a different underlying framework is at play.',
    'If this is a disguised reading, the true beneficiaries and victims, and thus the extractiveness and suppression, would be different, leading to a reclassification of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''acceptable_risk_for_energy'' kernel. This ''expected_value_dominant'' reading prioritizes annual expected costs and climate benefits, weighting rare events by probability x consequence. A ''catastrophic_tail_dominant'' reading would prioritize low-probability, high-consequence events, while a ''comparative_risk_dominant'' reading would assess nuclear risk relative to other energy risks. The core disagreement is on the weighting of different risk types and the ethical implications of those weightings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1970, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(acce_tr_t1985, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(acce_tr_t2024, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(acce_be_t1970, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(acce_be_t1985, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 1985, 0.25).
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2010, 0.29).
narrative_ontology:measurement(acce_be_t2024, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1970, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(acce_su_t1985, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1985, 0.18).
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(acce_su_t2024, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'acceptable_risk_for_energy' kernel. The other readings are 'catastrophic_tail_dominant' and 'comparative_risk_dominant'. Each reading instantiates a distinct constraint with its own epsilon and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
