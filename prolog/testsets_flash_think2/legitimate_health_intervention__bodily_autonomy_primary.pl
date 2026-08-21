% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary in Health Interventions
 *   domain: Public Health Policy / Medical Ethics / Constitutional Law
 *
 * SUMMARY:
 *   This constraint represents the reading of
 *   'legitimate_health_intervention' that prioritizes individual bodily
 *   autonomy and informed consent above all other considerations, including
 *   public benefit. It asserts that state coercion in medical matters
 *   fundamentally violates bodily integrity. The constraint is claimed as a
 *   'mountain' (a fundamental ethical principle), but its metrics reflect the
 *   high extractiveness and suppression experienced by individuals when this
 *   principle is violated by state action, triggering a false summit
 *   detection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.85).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.9).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, mountain).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "Bodily Autonomy as Primary in Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "Public Health Policy / Medical Ethics / Constitutional Law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).
domain_priors:emerges_naturally(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '4736b70d-6ea9-4052-87f3-a99ad0358d5c').
narrative_ontology:cs_kernel_codification('4736b70d-6ea9-4052-87f3-a99ad0358d5c', formalized).
narrative_ontology:cs_authority_grounding('4736b70d-6ea9-4052-87f3-a99ad0358d5c', lineage).
narrative_ontology:cs_interpretation_layer_present('4736b70d-6ea9-4052-87f3-a99ad0358d5c').
narrative_ontology:cs_reading_relation('4736b70d-6ea9-4052-87f3-a99ad0358d5c', legitimate_health_intervention__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('4736b70d-6ea9-4052-87f3-a99ad0358d5c', legitimate_health_intervention__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('4736b70d-6ea9-4052-87f3-a99ad0358d5c', foundational, individual_bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(individual_bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('4736b70d-6ea9-4052-87f3-a99ad0358d5c', individual_bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('4736b70d-6ea9-4052-87f3-a99ad0358d5c', foundational, informed_consent_unwaivable).
narrative_ontology:cs_axiom_status(informed_consent_unwaivable, holdable).
narrative_ontology:cs_axiom_grounding('4736b70d-6ea9-4052-87f3-a99ad0358d5c', informed_consent_unwaivable, deontological).
narrative_ontology:cs_reference_frame('4736b70d-6ea9-4052-87f3-a99ad0358d5c', post_nuremberg_code_ethics).
narrative_ontology:cs_drift_state('4736b70d-6ea9-4052-87f3-a99ad0358d5c', contemporary_pandemic_response, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4736b70d-6ea9-4052-87f3-a99ad0358d5c', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, individuals_with_autonomy).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, civil_liberties_advocates).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, medical_professionals).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, informed_consent_doctrine).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, bodily_integrity_principle).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, human_rights_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their right to self-determination over their body and medical choices is upheld, free from state or institutional coercion. They benefit from the ethical boundary this constraint establishes.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, individuals_with_autonomy, beneficiary,
    powerless, biographical, mobile, local).

% Are forced to undergo medical interventions against their will, or face severe penalties (e.g., loss of employment, access to public spaces) for refusal. They are the direct victims when the principle of bodily autonomy is violated.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals, payer,
    powerless, immediate, trapped, local).

% Seek to implement population-level health measures, but are ethically and legally constrained by the requirement for individual informed consent. Their actions are delegitimized by this constraint if they resort to coercion.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Actively defend individual rights and bodily integrity against state overreach, using this constraint as a foundational legal and ethical argument. They benefit from the clarity and strength of this principle.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, civil_liberties_advocates, beneficiary,
    organized, biographical, analytical, national).

% Advocate for population-level health outcomes as the primary concern, often arguing for interventions that may infringe on individual autonomy. Their perspective is fundamentally excluded by this reading's absolute stance on bodily integrity.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, public_health_primary_advocates, excluded,
    organized, biographical, constrained, national).

% Are ethically bound to obtain informed consent for all interventions, navigating this principle while also facing pressure from public health directives and institutional policies. They bear the cost of ensuring compliance.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, medical_professionals, payer,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear ethical and legal boundary for medical practice and public health interventions, ensuring patient trust, upholding individual dignity, and preventing medical paternalism or abuse of power.
% TRANSFER_FUNCTION: Transfers ultimate decision-making authority regarding medical interventions from the state or medical establishment to the individual; transfers the burden of risk assessment and acceptance to the individual.
% ABSENT_VOICES: Advocates for a 'public_health_primary' approach, who would argue for collective good over individual choice in certain circumstances, are structurally excluded from this reading's foundational premise. They would argue that individual refusal imposes externalities on the community.
% DISAPPEARANCE_RATIONALE: If this principle vanished, state and medical authorities could impose interventions without consent, fundamentally altering the relationship between individuals and the healthcare system, leading to widespread distrust, potential abuses, and a collapse of ethical medical practice.
% FOUNDING_PROBLEM: Historical abuses of medical power, eugenics programs, and non-consensual experimentation (e.g., Nazi medical experiments, Tuskegee Syphilis Study), which necessitated the establishment of fundamental ethical codes and human rights principles like the Nuremberg Code and the Helsinki Declaration.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, medical ethics boards, constitutional legal scholars, and historical records of medical abuses consistently corroborate the ongoing relevance of this problem and the necessity of this principle. The risk of medical coercion remains, especially during public health crises.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, ExtMetricName, E),
    domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(legitimate_health_intervention__bodily_autonomy_primary),
    narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) and suppression (0.90) reflect the severe impact on individuals when state coercion is applied to medical interventions, regardless of the stated public health goal. The 'mountain' claim for the principle itself, combined with these high metrics and the presence of beneficiaries and victims, is designed to trigger the False Summit Mountain (FSM) detection, highlighting the gap between the claimed naturalness of the principle and the extractive reality of its violation. Theater ratio is low (0.10) because the principle itself is not performative; any theatricality would arise from attempts to justify coercion as 'consent'. The increasing extractiveness and suppression over the interval reflect periods where state power has increasingly challenged this principle.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state public health authorities and public health primary advocates, this constraint might be seen as an impediment to effective population-level health management, or as a 'rope' that needs to be balanced against collective welfare. However, from the 'bodily_autonomy_primary' reading, any such balancing act is a violation of a fundamental, non-negotiable right, and thus an act of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals whose autonomy is protected are the primary beneficiaries (low d). Mandate-coerced individuals are the direct targets and victims (high d). State public health authorities are agenda-setters who are constrained by this principle; their actions are delegitimized if they resort to coercion. Civil liberties advocates benefit from the principle's strength, while public health primary advocates are excluded from its foundational premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_or_snare_in_practice,
    'Is this constraint a genuine natural law of ethics (a Mountain), or does its enforcement against state coercion function as a Snare for individuals?',
    'Analysis of historical and contemporary legal challenges to medical mandates: if the principle consistently requires active defense against state power, it functions more as a constructed Snare in practice, despite its ethical claim.',
    'If reclassified as a Snare, it would highlight the active, coercive nature of state actions that violate this principle, rather than framing the principle itself as a fixed, natural boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mountain_or_snare_in_practice, conceptual, 'Ambiguity between the ethical principle''s ''naturalness'' and its practical function against state coercion.').

omega_variable(
    coercion_definition_threshold,
    'What constitutes ''coercion'' in the context of medical interventions? Is it only direct physical force, or does it include severe social/economic penalties for non-compliance?',
    'Legal and ethical consensus-building on the definition of ''undue influence'' and ''coercion'' in medical ethics, informed by psychological and sociological research on behavioral responses to mandates.',
    'A broader definition of coercion would increase the measured extractiveness and suppression of many public health policies, leading to more frequent classification as a Snare. A narrower definition would reduce these metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_definition_threshold, conceptual, 'Ambiguity in the threshold for defining ''coercion'' in medical contexts.').

omega_variable(
    autonomy_vs_collective_responsibility,
    'How does this reading''s absolute stance on individual autonomy reconcile with the concept of collective responsibility for public health outcomes?',
    'This is a foundational conceptual disagreement between ethical frameworks, unlikely to be resolved empirically. Resolution would require a shift in underlying normative priorities.',
    'If a framework prioritizing collective responsibility were adopted, this constraint would be re-evaluated, likely leading to a lower extractiveness score for state interventions and a reclassification away from Mountain/Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_vs_collective_responsibility, preference, 'Irreducible tension between individual autonomy and collective public health responsibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.05).
narrative_ontology:measurement(legi_tr_t10, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 10, 0.06).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 20, 0.07).
narrative_ontology:measurement(legi_tr_t30, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 30, 0.08).
narrative_ontology:measurement(legi_tr_t40, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 40, 0.09).
narrative_ontology:measurement(legi_tr_t50, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(legi_be_t10, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(legi_be_t30, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(legi_be_t40, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(legi_be_t50, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(legi_su_t10, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(legi_su_t30, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(legi_su_t40, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(legi_su_t50, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, identity_coordination).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, public_health_policy_design).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, medical_licensing_standards).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, patient_rights_legislation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
