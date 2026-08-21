% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__risk_stratification_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Vaccine Mandate Legitimacy: Risk Stratification Reading
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'risk stratification' reading of
 *   vaccine mandate legitimacy. This reading posits that state-imposed
 *   vaccine mandates are legitimate only if they are proportionate to an
 *   actuarially defined risk threshold and are targeted, rather than blanket,
 *   measures. Blanket mandates are considered to fail proportionality. The
 *   constraint operates as a Tangled Rope, providing a coordination function
 *   for public health while extracting individual autonomy, with its
 *   legitimacy contingent on adherence to proportionality principles.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.65).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.7).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Vaccine Mandate Legitimacy: Risk Stratification Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, '99812ad3-237f-472a-a00c-d90f9ffc77f2').
narrative_ontology:cs_kernel_codification('99812ad3-237f-472a-a00c-d90f9ffc77f2', formalized).
narrative_ontology:cs_authority_grounding('99812ad3-237f-472a-a00c-d90f9ffc77f2', lineage).
narrative_ontology:cs_interpretation_layer_present('99812ad3-237f-472a-a00c-d90f9ffc77f2').
narrative_ontology:cs_reading_relation('99812ad3-237f-472a-a00c-d90f9ffc77f2', vaccine_mandate_legitimacy__public_health_primacy_reading, influences).
narrative_ontology:cs_reading_relation('99812ad3-237f-472a-a00c-d90f9ffc77f2', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_axiom('99812ad3-237f-472a-a00c-d90f9ffc77f2', foundational, mandates_must_be_proportional_to_risk).
narrative_ontology:cs_axiom_status(mandates_must_be_proportional_to_risk, holdable).
narrative_ontology:cs_axiom_grounding('99812ad3-237f-472a-a00c-d90f9ffc77f2', mandates_must_be_proportional_to_risk, deontological).
narrative_ontology:cs_axiom('99812ad3-237f-472a-a00c-d90f9ffc77f2', foundational, blanket_mandates_fail_proportionality).
narrative_ontology:cs_axiom_status(blanket_mandates_fail_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('99812ad3-237f-472a-a00c-d90f9ffc77f2', blanket_mandates_fail_proportionality, instrumental).
narrative_ontology:cs_reference_frame('99812ad3-237f-472a-a00c-d90f9ffc77f2', proportionality_principle_in_public_health).
narrative_ontology:cs_drift_state('99812ad3-237f-472a-a00c-d90f9ffc77f2', post_pandemic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('99812ad3-237f-472a-a00c-d90f9ffc77f2', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, general_public).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_systems).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, targeted_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting collective health, they seek to implement mandates that are proportionate to risk, balancing public good with individual rights. They face pressure from both sides of the debate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Individuals identified by public health authorities as posing a specific actuarial risk, and thus subject to mandates. They bear the direct cost of bodily autonomy infringement and may face social or economic penalties for non-compliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, targeted_individuals, payer,
    powerless, immediate, constrained, local).

% Benefits from reduced disease transmission, lower healthcare strain, and a quicker return to normalcy during outbreaks. They generally support mandates that are perceived as fair and effective.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, general_public, beneficiary,
    organized, biographical, mobile, national).

% Benefits from reduced patient load during epidemics, allowing them to maintain essential services. They advocate for public health measures that prevent system overload.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_systems, beneficiary,
    institutional, biographical, constrained, national).

% Analyze the legal and constitutional implications of mandates, focusing on proportionality, necessity, and the balance of rights. They provide critical commentary on policy decisions.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, universal).

% Examine the ethical justifications and consequences of vaccine mandates, particularly concerning individual autonomy, social justice, and collective responsibility. They contribute to the normative debate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, bioethicists, observer,
    analytical, generational, analytical, universal).

% Advocate for absolute medical self-sovereignty, viewing any state-imposed mandate as an unacceptable infringement. While present in the broader debate, their core premise is foreclosed by this reading's acceptance of *some* mandates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, bodily_autonomy_advocates, excluded,
    organized, biographical, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__risk_stratification_reading, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__risk_stratification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate collective action to mitigate public health risks by ensuring sufficient vaccination coverage, while respecting individual liberties where risks are low and mandates are disproportionate.
% TRANSFER_FUNCTION: Transfers a degree of individual bodily autonomy from targeted individuals to the collective (general public, healthcare systems) in exchange for reduced public health risk, but only when justified by a clear actuarial risk threshold.
% ABSENT_VOICES: Those who hold an absolute bodily autonomy position are structurally excluded from the internal logic of this reading, as their premise forecloses any mandate, even risk-stratified ones. They would argue against the very concept of legitimate state coercion in this domain.
% DISAPPEARANCE_RATIONALE: If the principle of risk-stratified mandate legitimacy vanished, public health authorities would either revert to blanket mandates (if legally permissible) or lose a key tool for managing epidemics, leading to different public health outcomes, potentially more severe restrictions, or higher disease burden. The legal and ethical landscape of public health interventions would be fundamentally altered.
% FOUNDING_PROBLEM: How to balance individual liberty (bodily autonomy) with collective welfare (public health) during infectious disease outbreaks, particularly when interventions carry individual costs, without resorting to disproportionate or arbitrary state coercion.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts, bioethics commissions, and public health organizations globally grapple with this balance, indicating the problem is actively debated and unresolved. Legislative hearings and scholarly publications from diverse fields consistently highlight the ongoing tension and the need for principled frameworks.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because even targeted mandates infringe on bodily autonomy, a significant cost. Suppression is high (0.70) as mandates require active enforcement and limit individual choice. Theater ratio is low (0.10) because the debate and implementation are genuinely focused on public health outcomes and legal/ethical principles, not performative maintenance. Accessibility collapse is moderate (0.60) as targeted individuals face limited alternatives. Resistance is high (0.75) due to fundamental disagreements over the scope of state power and individual rights.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities, operating within this reading, perceive the constraint as a necessary, albeit carefully limited, tool for collective welfare. Targeted individuals, however, experience the constraint as a direct imposition on their autonomy, often disputing the risk assessment or the proportionality of the measure. Observers like legal scholars and bioethicists analyze the structural tensions inherent in this balancing act.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are the agenda-setters, aiming to implement mandates within this reading's framework. The general public and healthcare systems are beneficiaries, gaining from reduced disease burden. Targeted individuals are payers, bearing the direct costs of compliance. Bodily autonomy advocates are excluded from the internal logic of this reading, as their core premise (absolute autonomy) is foreclosed by the acceptance of any mandate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actuarial_risk_threshold_definition,
    'What constitutes a sufficiently robust ''actuarial risk threshold'' to justify a vaccine mandate, and who defines it?',
    'Consensus among epidemiologists, statisticians, and public health ethicists on a standardized methodology for risk assessment, coupled with transparent public deliberation and judicial review.',
    'A clear, agreed-upon threshold would strengthen the legitimacy of targeted mandates and reduce contestation; an ambiguous or disputed threshold would undermine the constraint''s proportionality claim, potentially increasing perceived extraction and resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actuarial_risk_threshold_definition, empirical, 'Ambiguity in defining the actuarial risk threshold for mandate legitimacy.').

omega_variable(
    proportionality_assessment_framework,
    'How is the proportionality of a targeted mandate assessed in practice, and what criteria are used to determine if it is the least restrictive means?',
    'Development and adoption of a legally and ethically robust proportionality framework by courts and public health bodies, including clear tests for necessity, suitability, and strict proportionality.',
    'A consistent and transparent proportionality framework would enhance the perceived fairness and legitimacy of mandates, potentially reducing suppression and resistance. A lack of such a framework would leave mandates vulnerable to challenges of arbitrariness and overreach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_assessment_framework, conceptual, 'Uncertainty in the practical application of proportionality principles to targeted mandates.').

omega_variable(
    internalized_resistance_component,
    'To what extent is resistance to even risk-stratified mandates driven by internalized beliefs about bodily autonomy and distrust of authority, rather than purely structural barriers?',
    'Sociological and psychological studies examining the motivations of individuals resisting mandates, particularly after structural barriers or direct risks have been mitigated. If resistance persists, it suggests an internalized component.',
    'If resistance is substantially internalized, the effective suppression of the constraint is higher than structural measures suggest, as individuals carry their resistance with them. This would complicate policy interventions, requiring approaches beyond mere legal enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_resistance_component, empirical, 'Structural vs. internalized components of resistance to vaccine mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 2020, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t2020, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(vacc_tr_t2021, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2021, 0.12).
narrative_ontology:measurement(vacc_tr_t2022, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2022, 0.1).
narrative_ontology:measurement(vacc_tr_t2023, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2023, 0.09).

% Extraction over time
narrative_ontology:measurement(vacc_be_t2020, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(vacc_be_t2021, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement(vacc_be_t2022, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2022, 0.68).
narrative_ontology:measurement(vacc_be_t2023, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t2020, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(vacc_su_t2021, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2021, 0.7).
narrative_ontology:measurement(vacc_su_t2022, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2022, 0.75).
narrative_ontology:measurement(vacc_su_t2023, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2023, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_emergency_powers).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, individual_rights_protections).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vaccine_mandate_legitimacy' kernel. It focuses on the necessity of risk stratification and proportionality for mandate legitimacy, distinguishing it from readings that prioritize absolute bodily autonomy or broad public health authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
