% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__harm_reduction_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Substance Use as Health Condition (Harm Reduction Reading)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'harm reduction' reading of the substance
 *   control kernel, where substance use is primarily viewed as a health
 *   condition requiring pragmatic interventions to reduce associated harms,
 *   independent of use cessation. The state shifts towards a service provider
 *   role for users, but the illicit drug supply chain remains criminalized,
 *   leading to a hybrid coordination/extraction structure. This reading aims
 *   to mitigate the most severe consequences of drug use while maintaining a
 *   degree of state control.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.65).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.7).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Substance Use as Health Condition (Harm Reduction Reading)").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, '668ca5f7-0624-43ba-a6a3-999211662533').
narrative_ontology:cs_kernel_codification('668ca5f7-0624-43ba-a6a3-999211662533', formalized).
narrative_ontology:cs_authority_grounding('668ca5f7-0624-43ba-a6a3-999211662533', expertise).
narrative_ontology:cs_interpretation_layer_present('668ca5f7-0624-43ba-a6a3-999211662533').
narrative_ontology:cs_reading_relation('668ca5f7-0624-43ba-a6a3-999211662533', substance_control_kernel__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('668ca5f7-0624-43ba-a6a3-999211662533', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('668ca5f7-0624-43ba-a6a3-999211662533', foundational, public_health_priority_over_moral_judgment).
narrative_ontology:cs_axiom_status(public_health_priority_over_moral_judgment, holdable).
narrative_ontology:cs_axiom_grounding('668ca5f7-0624-43ba-a6a3-999211662533', public_health_priority_over_moral_judgment, instrumental).
narrative_ontology:cs_axiom('668ca5f7-0624-43ba-a6a3-999211662533', secondary, state_has_right_to_intervene_for_health).
narrative_ontology:cs_axiom_status(state_has_right_to_intervene_for_health, holdable).
narrative_ontology:cs_axiom_grounding('668ca5f7-0624-43ba-a6a3-999211662533', state_has_right_to_intervene_for_health, conventional).
narrative_ontology:cs_reference_frame('668ca5f7-0624-43ba-a6a3-999211662533', public_health_paradigm_shift).
narrative_ontology:cs_drift_state('668ca5f7-0624-43ba-a6a3-999211662533', contemporary_policy_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('668ca5f7-0624-43ba-a6a3-999211662533', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, medical_professionals).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, illicit_drug_suppliers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, law_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and funds harm reduction programs, shifting focus from criminalization to health outcomes. Benefits from reduced public health burden but must navigate political resistance and resource allocation challenges.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Receive funding and legitimacy to provide services like needle exchanges, overdose prevention, and safe consumption sites. Their existence and efficacy are tied to the harm reduction framework, but they operate within a system that still criminalizes supply.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers, beneficiary,
    organized, biographical, constrained, local).

% Benefit from reduced immediate harms and access to services, but remain subject to paternalistic health interventions and the risks associated with a criminalized drug supply. Their identity is framed as 'patients' or 'clients' of the health system, making exit from intervention difficult.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, people_who_use_drugs, payer,
    powerless, immediate, identity_locked, local).

% Continues to enforce laws against illicit drug supply, maintaining a criminalized market. Experiences a shift in focus from individual users to suppliers, but still expends resources on drug-related crime, often in tension with public health goals.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, law_enforcement, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, law_enforcement, payer).

% Remain targets of law enforcement, operating in a high-risk, criminalized environment. Bear the costs of prohibition, which paradoxically can drive up prices and increase the potency and danger of substances.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, illicit_drug_suppliers, payer,
    powerless, immediate, trapped, regional).

% Gain new roles and responsibilities in treating substance use as a health condition, including prescribing medications for opioid use disorder and managing complex health needs. Benefit from an expanded scope of practice and increased funding for related services.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, medical_professionals, beneficiary,
    moderate, biographical, mobile, national).

% Are marginalized in this policy framework, which prioritizes harm reduction over moral condemnation and punitive measures. They would argue for stricter enforcement and abstinence-only approaches, but their voices are not central to this reading.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, prohibition_advocates, excluded,
    organized, generational, constrained, national).

% Are also excluded from the core framing, as this reading maintains state control and criminalization of supply, rather than embracing individual liberty and regulated markets. They would argue for full decriminalization and legal regulation.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, legalization_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate a public health response to substance use, reducing overdose deaths, disease transmission, and other harms associated with drug use, by providing services and managing health outcomes.
% TRANSFER_FUNCTION: Transfers resources and legitimacy from traditional criminal justice enforcement against users to public health interventions and service provision. It also transfers some individual autonomy from people who use drugs to health systems and state oversight.
% ABSENT_VOICES: Prohibition advocates would object to the perceived leniency and moral compromise, arguing for stricter punitive measures. Legalization advocates would object to the continued criminalization of the supply chain and the paternalistic nature of health interventions, arguing for individual liberty and regulated markets.
% DISAPPEARANCE_RATIONALE: If the harm reduction framework vanished overnight, public health systems would lose their primary approach to substance use, leading to a resurgence of overdose deaths and disease transmission. Criminal justice systems would likely revert to more punitive, user-focused enforcement, and the social safety net for people who use drugs would collapse, causing significant societal reorganization.
% FOUNDING_PROBLEM: The punitive, prohibition-focused approach to substance use led to escalating overdose deaths, HIV/HCV transmission, criminalization of vulnerable populations, and a failure to address the underlying health and social determinants of drug use.
% FOUNDING_PROBLEM_CORROBORATION: Public health data, medical research, and international health organizations (e.g., WHO) consistently corroborate the ongoing public health crisis related to substance use and the failures of purely punitive approaches. Many governments and health bodies now officially endorse harm reduction principles, providing corroboration from outside the immediate beneficiary set.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates public health efforts to reduce harm (benefiting public health agencies and service providers) but simultaneously extracts autonomy from people who use drugs through paternalistic interventions and maintains a criminalized supply chain (victimizing users and suppliers). Active enforcement is required to manage the illicit supply and ensure compliance with health interventions. Extractiveness is moderate-high, reflecting the loss of autonomy and continued criminalization. Suppression is also moderate-high due to ongoing enforcement against suppliers and the coercive aspects of some health interventions. The theater ratio is moderate, as some 'health' interventions may serve to maintain state control or legitimacy rather than purely reduce harm.
 *
 * PERSPECTIVAL GAP:
 *   Public health agencies and harm reduction providers perceive this as a progressive, life-saving approach, a genuine coordination mechanism. However, people who use drugs may experience it as a new form of control, replacing criminal justice paternalism with health-system paternalism, while still facing the dangers of an illicit market. Law enforcement, while shifting focus, still sees a need for suppression of supply.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies, harm reduction providers, and medical professionals are beneficiaries, gaining resources, legitimacy, and expanded roles. People who use drugs are payers/victims, as they lose autonomy and navigate a system that still carries risks from criminalization. Illicit drug suppliers are clear victims, facing continued enforcement. Law enforcement acts as an agenda-setter for the criminalized supply, but also bears costs in terms of resource allocation and potential conflict with public health goals.
 *
 * MANDATROPHY ANALYSIS:
 *   The harm reduction reading attempts to resolve the mandatrophy of pure prohibition, where the original mandate (protecting social order) led to perverse outcomes (increased harm). By shifting focus to health, it seeks a new, more effective mandate. However, the continued criminalization of supply and paternalistic interventions suggest a potential for new forms of mandatrophy, where the 'health' mandate could become a cover for control if not carefully managed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paternalism_vs_autonomy,
    'Is the ''paternalistic health intervention'' truly in the best interest of people who use drugs, or does it unduly restrict their autonomy under the guise of care?',
    'Longitudinal studies on patient-reported outcomes, satisfaction with services, and perceived coercion within harm reduction programs, compared to outcomes in fully decriminalized/legalized contexts.',
    'If interventions are found to be overly coercive or autonomy-restricting, the effective extractiveness from people who use drugs would be higher, pushing the constraint closer to a Snare for that seat. If genuinely empowering, extractiveness would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_vs_autonomy, empirical, 'Ambiguity regarding the balance between health intervention and individual autonomy within the harm reduction framework.').

omega_variable(
    criminalized_supply_chain_impact,
    'To what extent does the continued criminalization of the drug supply chain undermine the public health goals of harm reduction?',
    'Comparative analysis of overdose rates, disease transmission, and drug purity in jurisdictions with criminalized vs. regulated supply chains, while maintaining harm reduction services.',
    'If criminalization significantly impedes harm reduction, the constraint''s overall effectiveness as a ''Rope'' component is diminished, and its ''Snare'' component (extraction from suppliers and users) is amplified, pushing it closer to a pure Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(criminalized_supply_chain_impact, empirical, 'The tension between harm reduction goals and the persistence of a criminalized drug supply.').

omega_variable(
    framing_under_determination_harm_reduction,
    'Is the ''harm reduction'' framing the most appropriate for addressing substance use, or does it obscure alternative framings like individual liberty or social justice?',
    'Analysis of policy outcomes and public discourse in jurisdictions that have adopted legalization or social justice models, compared to those primarily focused on harm reduction.',
    'If alternative framings prove more effective or equitable, the conceptual legitimacy of the harm reduction reading could be challenged, leading to a re-evaluation of its claimed coordination function and potential reclassification towards a more extractive type if its benefits are found to be limited or unequally distributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_under_determination_harm_reduction, conceptual, 'Alternative conceptual framings for substance use policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(subs_tr_t4, substance_control_kernel__harm_reduction_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement(subs_tr_t8, substance_control_kernel__harm_reduction_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(subs_tr_t12, substance_control_kernel__harm_reduction_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(subs_tr_t16, substance_control_kernel__harm_reduction_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__harm_reduction_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(subs_be_t4, substance_control_kernel__harm_reduction_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(subs_be_t8, substance_control_kernel__harm_reduction_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement(subs_be_t12, substance_control_kernel__harm_reduction_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(subs_be_t16, substance_control_kernel__harm_reduction_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__harm_reduction_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(subs_su_t4, substance_control_kernel__harm_reduction_reading, suppression_requirement, 4, 0.72).
narrative_ontology:measurement(subs_su_t8, substance_control_kernel__harm_reduction_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(subs_su_t12, substance_control_kernel__harm_reduction_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(subs_su_t16, substance_control_kernel__harm_reduction_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__harm_reduction_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'substance_control_kernel', each representing a different policy approach to substance use. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
