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
 *   This constraint describes a risk assessment framework where the
 *   acceptability of energy projects is primarily determined by annual
 *   expected costs and climate benefits, with rare, high-consequence events
 *   weighted by their probability-consequence product. It is one reading of
 *   the broader 'acceptable_risk_for_energy' kernel, emphasizing a
 *   utilitarian, aggregate-benefit approach. This framework tends to favor
 *   technologies with high expected benefits and low expected costs, even if
 *   they carry small probabilities of large-scale harm, as long as those
 *   harms are sufficiently discounted by their rarity.
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
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected Value Dominant Risk Assessment for Energy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "risk_assessment/energy_policy/public_safety_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, '27da9b24-7359-41d9-ae07-f234056c61d2').
narrative_ontology:cs_kernel_codification('27da9b24-7359-41d9-ae07-f234056c61d2', formalized).
narrative_ontology:cs_authority_grounding('27da9b24-7359-41d9-ae07-f234056c61d2', expertise).
narrative_ontology:cs_interpretation_layer_present('27da9b24-7359-41d9-ae07-f234056c61d2').
narrative_ontology:cs_reading_relation('27da9b24-7359-41d9-ae07-f234056c61d2', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('27da9b24-7359-41d9-ae07-f234056c61d2', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('27da9b24-7359-41d9-ae07-f234056c61d2', foundational, risk_is_quantifiable_by_expected_value).
narrative_ontology:cs_axiom_status(risk_is_quantifiable_by_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('27da9b24-7359-41d9-ae07-f234056c61d2', risk_is_quantifiable_by_expected_value, empirically_contingent).
narrative_ontology:cs_axiom('27da9b24-7359-41d9-ae07-f234056c61d2', foundational, societal_welfare_is_aggregate_utility).
narrative_ontology:cs_axiom_status(societal_welfare_is_aggregate_utility, holdable).
narrative_ontology:cs_axiom_grounding('27da9b24-7359-41d9-ae07-f234056c61d2', societal_welfare_is_aggregate_utility, instrumental).
narrative_ontology:cs_reference_frame('27da9b24-7359-41d9-ae07-f234056c61d2', rational_economic_decision_theory).
narrative_ontology:cs_drift_state('27da9b24-7359-41d9-ae07-f234056c61d2', contemporary_ethical_critiques, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('27da9b24-7359-41d9-ae07-f234056c61d2', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_energy_proponents).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, climate_mitigation_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, local_communities_near_facilities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for nuclear power, arguing that its expected benefits (low carbon emissions, reliable baseload power) outweigh its expected risks when rare events are properly weighted by probability. This framework makes nuclear energy appear more acceptable.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_energy_proponents, beneficiary,
    organized, generational, mobile, national).

% Supports energy policies that prioritize climate benefits, finding the expected-value approach useful for justifying low-carbon energy sources like nuclear, despite their associated risks. They benefit from a framework that emphasizes aggregate benefits.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_mitigation_advocates, beneficiary,
    organized, civilizational, constrained, global).

% Responsible for setting and enforcing safety standards for energy infrastructure. They adopt this framework to quantify and manage risks, balancing economic and environmental goals with public safety concerns. Their decisions are shaped by this calculus.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, public_safety_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the localized, albeit low-probability, risks associated with energy facilities (e.g., nuclear waste storage, potential accidents). Their concerns about rare, high-consequence events are often downplayed by an expected-value approach, which averages out their specific vulnerability.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, local_communities_near_facilities, payer,
    powerless, generational, trapped, local).

% Argue that expected-value calculations often disproportionately burden marginalized communities with localized risks, even if the aggregate societal benefit is positive. Their focus on equitable distribution of risk is often sidelined by this framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, environmental_justice_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, quantitative method for evaluating and comparing risks and benefits across different energy technologies, facilitating policy decisions and regulatory oversight.
% TRANSFER_FUNCTION: Transfers the burden of managing low-probability, high-consequence risks (e.g., nuclear waste disposal) to future generations or specific communities, in exchange for immediate climate and energy security benefits.
% ABSENT_VOICES: Advocates for catastrophic tail risk and environmental justice are often marginalized in this framework, as their concerns about extreme events and equitable risk distribution are not fully captured by a probability-weighted average.
% DISAPPEARANCE_RATIONALE: If this framework vanished, energy policy decisions would become significantly more contentious, lacking a common quantitative basis for comparing diverse risks and benefits. Investment in certain energy technologies (e.g., nuclear) would likely stall without a clear method to justify their risks, leading to a re-evaluation of energy portfolios.
% FOUNDING_PROBLEM: The need for a rational, consistent method to compare diverse risks and benefits of complex energy systems (e.g., fossil fuels, nuclear, renewables) to inform policy and investment decisions.
% FOUNDING_PROBLEM_CORROBORATION: Economists and risk analysts widely corroborate the need for a consistent framework to evaluate complex energy projects. Industry and government bodies also attest to its utility in decision-making, though environmental groups often contest its sufficiency for addressing all risk dimensions.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.3, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.3) because the framework itself is a tool for decision-making, not a direct extractor of resources. However, it can enable extraction by justifying projects that impose diffuse or localized risks for broader benefits. Suppression is low (0.2) as the framework doesn't actively suppress dissent, but rather provides a dominant language that can marginalize alternative risk framings. Theater ratio is low (0.1) as the calculations are generally taken seriously, though their completeness and ethical implications are debated. The framework functions as a 'rope' by coordinating diverse stakeholders around a common quantitative language for risk, even if some parties feel their concerns are not fully captured.
 *
 * PERSPECTIVAL GAP:
 *   While nuclear energy proponents and climate mitigation advocates see this as a rational, beneficial framework, local communities near facilities and environmental justice advocates often experience it as a mechanism that externalizes their specific, concentrated risks for the benefit of a broader, diffuse population. The framework's 'objectivity' can mask these distributional inequities.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear energy proponents and climate mitigation advocates are beneficiaries as this framework provides a strong justification for their preferred energy solutions. Public safety regulators are agenda-setters, as they implement and legitimize this approach. Local communities near facilities are payers, as they bear the residual risks that are deemed 'acceptable' by the framework. Environmental justice advocates are excluded, as their concerns about disproportionate risk burdens are not central to this expected-value calculus.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to provide a rational basis for energy risk assessment remains live. The classification as 'rope' prevents mislabeling it as pure extraction, acknowledging its genuine coordination function in a complex domain. However, the omegas highlight the potential for this coordination to become extractive if the framework's limitations (e.g., handling of catastrophic tails, distributional justice) are not critically addressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophic_tail_risk_weighting,
    'Does the probability × consequence product adequately capture the societal and ethical implications of low-probability, high-consequence (catastrophic tail) events, or should such events be weighted non-linearly?',
    'Societal consensus shifts towards a precautionary principle for irreversible harms, or new ethical frameworks gain dominance that assign infinite or disproportionate weight to catastrophic outcomes.',
    'If tail risks are weighted non-linearly, the framework would shift towards the ''catastrophic_tail_dominant'' reading, making certain energy technologies (e.g., nuclear) less acceptable, and increasing the perceived extractiveness of projects that impose such risks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophic_tail_risk_weighting, conceptual, 'The adequacy of expected-value weighting for catastrophic tail risks.').

omega_variable(
    distributional_equity_integration,
    'Can the expected-value framework be modified to explicitly account for the equitable distribution of risks and benefits across different communities and generations, or does its aggregate nature inherently obscure these concerns?',
    'Development and adoption of multi-criteria decision analysis tools that integrate equity metrics alongside expected value, or legal mandates requiring explicit distributional impact assessments.',
    'If equity concerns are integrated, the framework''s perceived extractiveness from vulnerable communities would decrease, and it might shift towards a ''tangled_rope'' or even ''rope'' for environmental justice advocates, as their concerns would be explicitly addressed rather than excluded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_equity_integration, preference, 'Integration of distributional equity into aggregate risk assessment.').

omega_variable(
    nuclear_waste_disposal_solvability,
    'Is nuclear waste disposal a solvable engineering challenge with a quantifiable expected cost, or does it represent an intergenerational burden with irreducible uncertainty and ethical implications that defy simple probabilistic weighting?',
    'Demonstrated long-term safe disposal solutions with broad public and scientific acceptance, or a fundamental shift in scientific understanding that reveals insurmountable technical barriers.',
    'If disposal is deemed an irreducible, unquantifiable burden, nuclear energy would re-enter the ''victim'' set for this reading, and the framework''s ability to justify nuclear projects would be severely undermined, pushing it towards the ''catastrophic_tail_dominant'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_waste_disposal_solvability, empirical, 'The nature of nuclear waste disposal as a quantifiable risk or irreducible burden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1970, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(acce_tr_t1990, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(acce_tr_t2024, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(acce_be_t1970, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(acce_be_t1990, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(acce_be_t2024, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1970, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(acce_su_t1990, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1990, 0.18).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(acce_su_t2024, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'acceptable_risk_for_energy' kernel, focusing on expected value. It is linked to 'catastrophic_tail_dominant' and 'comparative_risk_dominant' readings, which offer alternative frameworks for risk assessment in energy policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
