% ============================================================================
% CONSTRAINT STORY: dignified_death__relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__relational_autonomy, []).

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
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Relational Autonomy in End-of-Life Decisions
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint describes the 'relational autonomy' reading of
 *   end-of-life dignity, where decision authority is distributed across the
 *   patient-family-clinician triad, guided by procedural safeguards. It aims
 *   to solve the coordination problem of complex end-of-life decisions by
 *   integrating multiple perspectives, rather than privileging a single one.
 *   The constraint is claimed as a Rope due to its genuine coordination
 *   function, despite the inherent procedural overhead and potential for
 *   friction, which contribute to its moderate extractiveness and
 *   suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.38).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.45).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.38).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational Autonomy in End-of-Life Decisions").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, 'ec43bc6b-ee8f-4e21-b42b-608294d80180').
narrative_ontology:cs_kernel_codification('ec43bc6b-ee8f-4e21-b42b-608294d80180', formalized).
narrative_ontology:cs_authority_grounding('ec43bc6b-ee8f-4e21-b42b-608294d80180', practice).
narrative_ontology:cs_interpretation_layer_present('ec43bc6b-ee8f-4e21-b42b-608294d80180').
narrative_ontology:cs_reading_relation('ec43bc6b-ee8f-4e21-b42b-608294d80180', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('ec43bc6b-ee8f-4e21-b42b-608294d80180', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('ec43bc6b-ee8f-4e21-b42b-608294d80180', foundational, dignity_is_relational).
narrative_ontology:cs_axiom_status(dignity_is_relational, holdable).
narrative_ontology:cs_axiom_grounding('ec43bc6b-ee8f-4e21-b42b-608294d80180', dignity_is_relational, deontological).
narrative_ontology:cs_axiom('ec43bc6b-ee8f-4e21-b42b-608294d80180', foundational, shared_decision_making_is_ethical).
narrative_ontology:cs_axiom_status(shared_decision_making_is_ethical, holdable).
narrative_ontology:cs_axiom_grounding('ec43bc6b-ee8f-4e21-b42b-608294d80180', shared_decision_making_is_ethical, conventional).
narrative_ontology:cs_reference_frame('ec43bc6b-ee8f-4e21-b42b-608294d80180', triadic_decision_making).
narrative_ontology:cs_drift_state('ec43bc6b-ee8f-4e21-b42b-608294d80180', contemporary_medical_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ec43bc6b-ee8f-4e21-b42b-608294d80180', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, patient_family_clinician_triad).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, relational_network).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, isolated_patient).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, vulnerable_patient).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patient).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The individual whose life and death are at stake. Bears the ultimate consequences of decisions. May be physically or cognitively vulnerable, making their 'autonomy' dependent on the relational context. Their identity is deeply tied to their bodily integrity and life choices.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patient, payer,
    powerless, immediate, identity_locked, local).

% Close relatives who provide care, emotional support, and often act as advocates. Participate in decision-making, bringing their knowledge of the patient's values and wishes. Benefit from shared responsibility and a sense of having honored the patient's dignity, but bear emotional costs.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, family_members, agenda_setter,
    moderate, biographical, constrained, local).

% Medical professionals responsible for diagnosis, prognosis, and treatment options. Guide the decision process with medical expertise and ethical principles. Benefit from clear ethical guidelines and shared decision-making, but face moral distress if the process fails.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, clinicians, agenda_setter,
    institutional, biographical, constrained, local).

% Institutional bodies that provide ethical consultation and guidance on complex cases. Observe and advise on the application of relational autonomy principles, ensuring procedural safeguards are met.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, bioethics_committees, observer,
    institutional, generational, analytical, regional).

% Groups and individuals who prioritize absolute individual self-determination in end-of-life decisions. Their perspective, while respected, is structurally constrained by the relational framework's emphasis on shared authority, leading to their partial exclusion from the core decision-making model.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, autonomy_advocates, excluded,
    organized, generational, mobile, national).

% Groups and individuals who prioritize the intrinsic value of life and oppose intentional life-termination. Their perspective is also structurally constrained by the relational framework's allowance for patient-centered decisions, leading to their partial exclusion from the core decision-making model.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, sanctity_advocates, excluded,
    organized, civilizational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__relational_autonomy, diffuse).
narrative_ontology:fixing_cost_class(dignified_death__relational_autonomy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate complex end-of-life decisions by distributing authority and responsibility across the patient, family, and clinician triad, ensuring that dignity is understood and upheld within a relational context, rather than solely individualistic or purely medical terms.
% TRANSFER_FUNCTION: Transfers ultimate decision authority from a single locus (e.g., the patient in pure autonomy, or the clinician in medical paternalism) to a shared, procedurally mediated process involving multiple parties. This transfers the burden of decision-making and the responsibility for upholding dignity.
% ABSENT_VOICES: Advocates for pure individual autonomy or absolute sanctity of life are structurally excluded from fully dictating the terms of end-of-life decisions within this framework. They would argue for a simpler, less distributed authority structure.
% DISAPPEARANCE_RATIONALE: If this framework vanished, end-of-life decisions would likely revert to either a more individualistic (potentially isolating patients and families) or a more paternalistic/sanctity-driven model, leading to increased conflict, moral distress for clinicians, and a loss of the nuanced understanding of dignity that the relational model provides.
% FOUNDING_PROBLEM: Unresolved conflicts and ethical dilemmas in end-of-life care arising from competing claims of individual autonomy, family wishes, and medical judgment, often leading to patient suffering, family distress, and moral injury for clinicians.
% FOUNDING_PROBLEM_CORROBORATION: Bioethicists, medical professional bodies, patient advocacy groups, and legal scholars (outside the immediate decision triad) corroborate the ongoing challenge of balancing these claims and the need for a robust framework to navigate them.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__relational_autonomy, 0.38, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__relational_autonomy_tests).
:- end_tests(dignified_death__relational_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the procedural safeguards and distributed authority introduce significant overhead and potential for friction, which can feel like a cost to participants, even if the overall outcome is beneficial. Suppression is moderate (0.45) because the framework actively constrains alternative, simpler decision models (e.g., pure individual autonomy or pure medical authority) in favor of the relational approach. Resistance is low (0.30) as the framework aims for consensus, though friction can arise. Theater ratio is low (0.10) as the procedural safeguards are genuinely functional, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the patient, the framework can be a source of support and dignity, but also a source of burden if their voice is not adequately heard within the triad. From the perspective of clinicians, it provides ethical clarity but adds procedural complexity. The engine will compute these divergences based on the declared roles and positional atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   The patient, family, and clinicians are all beneficiaries of the framework's coordination function, as it provides a structured way to navigate difficult decisions and uphold dignity. However, the patient also bears the ultimate consequences, and the family/clinicians bear emotional/moral costs, making their directionality complex. The 'isolated patient' and 'vulnerable patient' are victims if the relational process fails to adequately represent their voice or protect their interests. Advocates for pure autonomy or sanctity are excluded, as their frameworks are not fully integrated.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relational_autonomy_kernel_reading,
    'Is this constraint a valid instantiation of the ''dignified_death'' kernel under the ''relational_autonomy'' reading?',
    'Comparison with other readings of the ''dignified_death'' kernel (autonomy_primary, sanctity_primary) to assess internal consistency and distinct structural properties.',
    'If not a valid reading, the classification of this constraint as a Rope may be inaccurate, and its relationship to other end-of-life frameworks would need re-evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(relational_autonomy_kernel_reading, conceptual, 'This constraint is the ''relational_autonomy'' reading of the ''dignified_death'' kernel.').

omega_variable(
    victim_set_under_autonomy_primary,
    'If the ''autonomy_primary'' reading of dignified_death were adopted, how would the victim set change?',
    'Analysis of cases where patient autonomy is prioritized above all else, potentially leading to isolation from family support or medical guidance.',
    'Under ''autonomy_primary'', the ''isolated_patient'' would be less likely to be a victim of the *process* itself, but ''family_members'' and ''clinicians'' might become victims of moral distress or unmanageable responsibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_under_autonomy_primary, conceptual, 'Impact of ''autonomy_primary'' reading on victim set.').

omega_variable(
    victim_set_under_sanctity_primary,
    'If the ''sanctity_primary'' reading of dignified_death were adopted, how would the victim set change?',
    'Analysis of cases where life''s intrinsic value is prioritized, potentially overriding patient wishes or prolonging suffering.',
    'Under ''sanctity_primary'', the ''vulnerable_patient'' (whose wishes are overridden) would be a primary victim, and ''clinicians'' might face moral distress from being compelled to act against patient-centered care principles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_under_sanctity_primary, conceptual, 'Impact of ''sanctity_primary'' reading on victim set.').

omega_variable(
    dignity_definition_ambiguity,
    'Is the concept of ''dignity'' sufficiently well-defined within the relational autonomy framework to prevent arbitrary application or subtle forms of extraction?',
    'Qualitative research on patient and family experiences, and ethical review of cases where ''dignity'' is invoked to justify particular outcomes.',
    'If ''dignity'' remains ambiguous, the framework''s coordination function could be undermined, and it could inadvertently become a Snare for vulnerable patients whose ''dignity'' is defined for them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_definition_ambiguity, empirical, 'Ambiguity of ''dignity'' in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dign_tr_t6, dignified_death__relational_autonomy, theater_ratio, 6, 0.1).
narrative_ontology:measurement(dign_tr_t12, dignified_death__relational_autonomy, theater_ratio, 12, 0.1).
narrative_ontology:measurement(dign_tr_t18, dignified_death__relational_autonomy, theater_ratio, 18, 0.1).
narrative_ontology:measurement(dign_tr_t24, dignified_death__relational_autonomy, theater_ratio, 24, 0.1).
narrative_ontology:measurement(dign_tr_t30, dignified_death__relational_autonomy, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(dign_be_t6, dignified_death__relational_autonomy, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(dign_be_t12, dignified_death__relational_autonomy, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(dign_be_t18, dignified_death__relational_autonomy, base_extractiveness, 18, 0.37).
narrative_ontology:measurement(dign_be_t24, dignified_death__relational_autonomy, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(dign_be_t30, dignified_death__relational_autonomy, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dign_su_t6, dignified_death__relational_autonomy, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(dign_su_t12, dignified_death__relational_autonomy, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(dign_su_t18, dignified_death__relational_autonomy, suppression_requirement, 18, 0.44).
narrative_ontology:measurement(dign_su_t24, dignified_death__relational_autonomy, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(dign_su_t30, dignified_death__relational_autonomy, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, attachment_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
