% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary Constraint on Medical Coercion
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint asserts that medical intervention without consent is
 *   categorically impermissible, regardless of any potential collective
 *   benefit. It represents the 'bodily_autonomy_primary' reading of the
 *   'coercion_legitimacy_boundary' kernel, prioritizing individual rights
 *   over state-compelled health measures. The constraint itself is a
 *   protective principle, preventing extraction from individuals, and is
 *   claimed as a normative mountain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.05).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.08).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.05).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, mountain).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Bodily Autonomy as Primary Constraint on Medical Coercion").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:emerges_naturally(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '29705202-2530-4333-b9f4-317b98066ec5').
narrative_ontology:cs_kernel_codification('29705202-2530-4333-b9f4-317b98066ec5', formalized).
narrative_ontology:cs_authority_grounding('29705202-2530-4333-b9f4-317b98066ec5', lineage).
narrative_ontology:cs_interpretation_layer_present('29705202-2530-4333-b9f4-317b98066ec5').
narrative_ontology:cs_reading_relation('29705202-2530-4333-b9f4-317b98066ec5', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('29705202-2530-4333-b9f4-317b98066ec5', coercion_legitimacy_boundary__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('29705202-2530-4333-b9f4-317b98066ec5', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('29705202-2530-4333-b9f4-317b98066ec5', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('29705202-2530-4333-b9f4-317b98066ec5', foundational, individual_consent_non_negotiable).
narrative_ontology:cs_axiom_status(individual_consent_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('29705202-2530-4333-b9f4-317b98066ec5', individual_consent_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('29705202-2530-4333-b9f4-317b98066ec5', post_nuremberg_ethical_consensus).
narrative_ontology:cs_drift_state('29705202-2530-4333-b9f4-317b98066ec5', contemporary_public_health_crises, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('29705202-2530-4333-b9f4-317b98066ec5', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, individuals_seeking_autonomy).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_professionals_upholding_ethics).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, patient_rights_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, informed_consent_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals are protected from unwanted medical interventions by this constraint. Their right to self-determination over their body is upheld, ensuring that medical procedures require their explicit consent.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, individuals_seeking_autonomy, beneficiary,
    moderate, biographical, constrained, national).

% Medical professionals who adhere to ethical codes benefit from a clear, consistent standard of practice that builds patient trust and legitimizes their profession. This constraint guides their conduct and protects them from demands to perform non-consensual interventions.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_professionals_upholding_ethics, beneficiary,
    organized, generational, constrained, global).

% These authorities are bound by the constraint, limiting their ability to compel medical interventions for collective benefit. They must operate within the bounds of individual consent, which can complicate public health responses but ensures ethical governance.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% While not directly victimized by this constraint, these individuals are indirectly exposed to higher risks from infectious diseases in a society where medical interventions cannot be compelled for collective immunity. Their vulnerability is a consequence of the constraint's operation, not its direct extraction.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, excluded,
    powerless, immediate, trapped, local).

% These observers analyze the ethical and practical implications of prioritizing bodily autonomy, considering its historical grounding, its impact on public health, and its interaction with other normative claims.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a fundamental ethical boundary for medical practice, ensuring trust between patients and providers by making individual consent the non-negotiable prerequisite for intervention.
% TRANSFER_FUNCTION: Prevents the transfer of bodily control or decision-making authority from individuals to the state or medical institutions, thereby protecting individual sovereignty.
% ABSENT_VOICES: Public health advocates and immunocompromised individuals would argue for a more flexible approach to medical coercion in emergencies, prioritizing collective harm-prevention over absolute individual autonomy. They are excluded from the direct framing of this constraint, which is absolute.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the foundational principles of medical ethics would collapse, potentially leading to state-sanctioned forced medical procedures, eroding public trust in healthcare, and fundamentally altering the relationship between individuals and medical authority. The entire structure of patient rights would need to be rebuilt.
% FOUNDING_PROBLEM: Historical abuses of medical power, forced sterilization, unethical human experimentation (e.g., Nuremberg trials), and state-mandated medical procedures without individual consent, which led to a global consensus on patient rights.
% FOUNDING_PROBLEM_CORROBORATION: International human rights declarations (e.g., Universal Declaration of Human Rights, Nuremberg Code, Helsinki Declaration), national constitutional protections, bioethics committees, and patient advocacy groups consistently corroborate the ongoing relevance of preventing non-consensual medical interventions.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, ExtMetricName, E),
    domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(coercion_legitimacy_boundary__bodily_autonomy_primary),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's extractiveness is very low because its function is to *prevent* extraction (unwanted medical intervention). Suppression is low as it protects individual autonomy, rather than suppressing it. Theater ratio is negligible as it represents a fundamental ethical principle with little performative maintenance. Accessibility collapse is high because, as a categorical imperative, it leaves no legitimate alternative for non-consensual intervention. Resistance is low because the principle of informed consent is widely accepted in medical ethics and law.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals and ethical medical professionals, this constraint is a fundamental protection. From the perspective of public health authorities, it is a necessary limitation that complicates collective action. The engine will compute these different experiences based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals seeking autonomy are direct beneficiaries, as the constraint protects their fundamental right to self-determination. Medical professionals upholding ethics also benefit from a clear, stable ethical framework. Public health authorities, while constrained by this principle, ultimately benefit from the trust it fosters. Immunocompromised individuals are excluded from the direct benefits of collective immunity that might arise from compelled interventions, but are not directly victimized by this constraint itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, universally applicable ethical mountain, or a constructed normative framework that benefits identifiable agents?',
    'Analysis of historical and cross-cultural variations in medical ethics and patient rights, and the political processes through which these norms were established and defended.',
    'If found to be a constructed framework, its classification as a ''mountain'' would be re-evaluated, potentially shifting towards a ''rope'' or ''tangled_rope'' if its persistence is tied to active defense by beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity between a fundamental ethical truth and a socially constructed norm.').

omega_variable(
    consequences_for_vulnerable_populations,
    'Does the categorical impermissibility of non-consensual intervention, while protecting individual autonomy, create an unacceptable level of risk for vulnerable populations (e.g., immunocompromised individuals) by limiting collective health measures?',
    'Empirical studies on public health outcomes in jurisdictions with strict vs. flexible consent laws during epidemics, coupled with ethical analysis of competing rights claims.',
    'If the risk to vulnerable populations is deemed severe and unmitigable by other means, it could lead to pressure for a re-evaluation of the ''categorical'' nature of this constraint, potentially shifting towards a ''proportionality_reading'' of the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consequences_for_vulnerable_populations, empirical, 'Indirect harm to vulnerable groups as a consequence of prioritizing individual autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.02).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 10, 0.02).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 20, 0.02).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 30, 0.02).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 40, 0.02).
narrative_ontology:measurement(coer_tr_t50, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 50, 0.02).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(coer_be_t50, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 50, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 10, 0.08).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 20, 0.08).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 30, 0.08).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 40, 0.08).
narrative_ontology:measurement(coer_su_t50, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
