% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Contextual Humane Treatment Standard (National Security Override)
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'contextual_necessity' reading of the
 *   humane treatment standard, where Common Article 3 sets a baseline but
 *   permits 'enhanced interrogation' when national security imperatives are
 *   deemed to override. Humane treatment is thus considered
 *   context-dependent, granting state security agencies significant
 *   discretion. This reading leads to conditional detainee protections and a
 *   shrinking victim set in 'necessity' scenarios. The constraint is claimed
 *   as a Tangled Rope, acknowledging a coordination function (setting a
 *   baseline) but with substantial asymmetric extraction (permitting enhanced
 *   interrogation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.78).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.85).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.78).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Contextual Humane Treatment Standard (National Security Override)").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, '7b4d7a14-7d7c-4503-b6d9-bc1243372e23').
narrative_ontology:cs_kernel_codification('7b4d7a14-7d7c-4503-b6d9-bc1243372e23', fixed_text).
narrative_ontology:cs_authority_grounding('7b4d7a14-7d7c-4503-b6d9-bc1243372e23', extraction).
narrative_ontology:cs_interpretation_layer_present('7b4d7a14-7d7c-4503-b6d9-bc1243372e23').
narrative_ontology:cs_reading_relation('7b4d7a14-7d7c-4503-b6d9-bc1243372e23', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('7b4d7a14-7d7c-4503-b6d9-bc1243372e23', humane_treatment_standard__proportionality_balancing, coexists_with).
narrative_ontology:cs_axiom('7b4d7a14-7d7c-4503-b6d9-bc1243372e23', foundational, national_security_supremacy).
narrative_ontology:cs_axiom_status(national_security_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('7b4d7a14-7d7c-4503-b6d9-bc1243372e23', national_security_supremacy, conventional).
narrative_ontology:cs_axiom('7b4d7a14-7d7c-4503-b6d9-bc1243372e23', foundational, humane_treatment_is_context_dependent).
narrative_ontology:cs_axiom_status(humane_treatment_is_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('7b4d7a14-7d7c-4503-b6d9-bc1243372e23', humane_treatment_is_context_dependent, conventional).
narrative_ontology:cs_reference_frame('7b4d7a14-7d7c-4503-b6d9-bc1243372e23', state_sovereignty_security_paradigm).
narrative_ontology:cs_drift_state('7b4d7a14-7d7c-4503-b6d9-bc1243372e23', post_9_11_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7b4d7a14-7d7c-4503-b6d9-bc1243372e23', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, state_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, national_governments).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, detainees_in_national_security_cases).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These agencies gain discretion to define 'humane treatment' based on national security imperatives, allowing for 'enhanced interrogation' techniques. They benefit from perceived operational flexibility and the ability to extract intelligence, often with reduced accountability.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, state_security_agencies, agenda_setter,
    institutional, biographical, mobile, national).

% Governments benefit from the perceived ability to protect national security interests, often citing public safety. They support the agencies' interpretation, balancing international obligations with domestic security concerns, and bear political costs if abuses become public.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, national_governments, beneficiary,
    institutional, generational, constrained, global).

% These individuals are the primary targets of 'enhanced interrogation' and experience conditional protections. Their rights are subject to redefinition based on state-declared necessity, and their ability to challenge treatment is severely constrained or non-existent.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, detainees_in_national_security_cases, payer,
    powerless, immediate, trapped, local).

% These groups actively resist the redefinition of humane treatment and document abuses. They bear the costs of continuous advocacy, legal challenges, and public education against state practices that erode universal human rights standards.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, human_rights_advocates, payer,
    organized, generational, constrained, global).

% These bodies (e.g., UN committees, international courts) monitor state compliance with international law. They analyze state practices against treaty obligations and issue findings, but their enforcement power is often limited to moral suasion or sanctions by member states.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__contextual_necessity, state_security_agencies).
narrative_ontology:fixing_cost_class(humane_treatment_standard__contextual_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to navigate the tension between national security imperatives and the baseline requirement for humane treatment of detainees, aiming to prevent arbitrary and widespread abuse while allowing for operational flexibility in exceptional circumstances.
% TRANSFER_FUNCTION: Transfers discretion over the interpretation and application of humane treatment standards from absolute international legal norms to state security agencies, making detainee protections conditional. It transfers the costs of 'enhanced interrogation' onto detainees and the burden of proof for 'humane' treatment onto human rights advocates.
% ABSENT_VOICES: Detainees themselves, whose experiences are often dismissed, classified, or deemed unreliable, and independent medical/psychological experts whose ethical guidelines often conflict with 'enhanced interrogation' practices.
% DISAPPEARANCE_RATIONALE: If this reading vanished, states would either default to a stricter absolute prohibition (if the absolute_prohibition reading gained dominance) or face greater international scrutiny and legal challenges for any deviation from humane treatment, forcing a re-evaluation of interrogation practices and legal frameworks. The legal and operational landscape for state security would fundamentally shift.
% FOUNDING_PROBLEM: How to reconcile the absolute prohibition against torture and cruel, inhuman, or degrading treatment (as codified in Common Article 3) with the perceived need for states to extract critical intelligence in extreme national security scenarios, particularly in asymmetric conflicts.
% FOUNDING_PROBLEM_CORROBORATION: State security agencies and national governments attest the problem is still live, citing ongoing and evolving threats that necessitate flexible intelligence gathering. Human rights organizations and international legal bodies attest that while the problem of intelligence gathering is real, the 'necessity' argument is often a pretext for abuse, and the founding problem has been reframed to justify extraction rather than genuine coordination.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the reading allows for practices that inflict significant harm under the guise of necessity, extracting information and compliance from detainees. Suppression is very high (0.85) due to the active enforcement of these interrogation methods, the classification of information, and the suppression of legal challenges and alternative interpretations. Theater ratio is moderate (0.45) as there is a performance of legality and necessity, but a substantial portion of the activity is genuinely aimed at intelligence gathering, albeit through extractive means. Accessibility collapse is high (0.85) for detainees, as their protections are made conditional and subject to the discretion of security agencies. Resistance is high (0.75) from human rights organizations and some international legal bodies.
 *
 * PERSPECTIVAL GAP:
 *   State security agencies and national governments perceive this constraint as a necessary, albeit difficult, coordination mechanism to protect national interests. From their seat, the constraint is a Rope or even a Mountain (natural law of state survival). Detainees and human rights advocates, however, experience it as a Snare, where the coordination story is a cover for pure extraction and abuse, with their rights actively suppressed.
 *
 * DIRECTIONALITY LOGIC:
 *   State security agencies and national governments are the primary beneficiaries (low directionality), gaining operational flexibility and perceived security. Detainees are the full targets (high directionality), bearing the direct costs of conditional treatment and 'enhanced interrogation'. Human rights advocates are also targets (high directionality), as they expend significant resources to resist the erosion of standards. International legal bodies act as observers, attempting to uphold the baseline but often lacking direct enforcement power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_threshold_ambiguity,
    'What constitutes a ''national security imperative'' sufficient to override baseline humane treatment, and who legitimately defines this threshold?',
    'Establishment of independent, internationally recognized judicial or oversight bodies with binding authority to review and define ''necessity'' in specific cases, rather than allowing self-declaration by states.',
    'If the threshold is narrowly defined and independently adjudicated, the constraint''s extractiveness would decrease, and its classification might shift closer to a Tangled Rope with less asymmetric extraction. If it remains broadly defined and self-declared, the Snare-like qualities persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_threshold_ambiguity, conceptual, 'Ambiguity in defining ''national security imperatives'' and the authority to do so.').

omega_variable(
    effectiveness_of_enhanced_interrogation,
    'Is ''enhanced interrogation'' empirically effective at producing reliable, actionable intelligence that could not be obtained through humane, lawful methods?',
    'Independent, declassified, and rigorous empirical studies comparing intelligence yields from ''enhanced'' versus humane interrogation techniques, conducted by experts without ties to security agencies.',
    'If proven ineffective, the ''necessity'' justification would collapse, exposing the practices as pure extraction and increasing the constraint''s perceived extractiveness and suppression. If proven effective (a highly contested claim), it would strengthen the coordination narrative, though not eliminate the extractive component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effectiveness_of_enhanced_interrogation, empirical, 'Empirical efficacy of ''enhanced interrogation'' techniques.').

omega_variable(
    structural_vs_internalized_suppression_detainees,
    'To what extent is the suppression experienced by detainees structural (external barriers) versus internalized (psychological effects that persist after release)?',
    'Longitudinal studies of former detainees, including psychological assessments and analysis of their ability to seek redress post-release, to determine the persistence of fear, trauma, and self-censorship.',
    'If suppression is significantly internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit, making recovery and redress more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression_detainees, empirical, 'Structural vs. internalized suppression mechanism for detainees.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.3).
narrative_ontology:measurement(huma_tr_t6, humane_treatment_standard__contextual_necessity, theater_ratio, 6, 0.35).
narrative_ontology:measurement(huma_tr_t12, humane_treatment_standard__contextual_necessity, theater_ratio, 12, 0.4).
narrative_ontology:measurement(huma_tr_t18, humane_treatment_standard__contextual_necessity, theater_ratio, 18, 0.42).
narrative_ontology:measurement(huma_tr_t24, humane_treatment_standard__contextual_necessity, theater_ratio, 24, 0.44).
narrative_ontology:measurement(huma_tr_t30, humane_treatment_standard__contextual_necessity, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(huma_be_t6, humane_treatment_standard__contextual_necessity, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(huma_be_t12, humane_treatment_standard__contextual_necessity, base_extractiveness, 12, 0.74).
narrative_ontology:measurement(huma_be_t18, humane_treatment_standard__contextual_necessity, base_extractiveness, 18, 0.76).
narrative_ontology:measurement(huma_be_t24, humane_treatment_standard__contextual_necessity, base_extractiveness, 24, 0.77).
narrative_ontology:measurement(huma_be_t30, humane_treatment_standard__contextual_necessity, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(huma_su_t6, humane_treatment_standard__contextual_necessity, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(huma_su_t12, humane_treatment_standard__contextual_necessity, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(huma_su_t18, humane_treatment_standard__contextual_necessity, suppression_requirement, 18, 0.82).
narrative_ontology:measurement(huma_su_t24, humane_treatment_standard__contextual_necessity, suppression_requirement, 24, 0.84).
narrative_ontology:measurement(huma_su_t30, humane_treatment_standard__contextual_necessity, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'humane_treatment_standard' kernel, which also includes 'absolute_prohibition' and 'proportionality_balancing' readings. Each reading instantiates a distinct constraint with different structural properties and ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
