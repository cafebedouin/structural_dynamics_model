% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__harm_threshold_reading, []).

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
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Speech Protection Conditional on Demonstrable Harm
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'harm threshold' reading of the broader
 *   'speech protection kernel'. It posits that the protection afforded to
 *   speech is not absolute but is conditional on the absence of demonstrable
 *   harm to identifiable victims. This reading prioritizes the protection of
 *   individuals and groups from speech-related harms, such as incitement to
 *   violence, defamation, or harassment, over an expansive interpretation of
 *   speaker autonomy. It leads to a narrower boundary for protected speech
 *   and a broader scope for categories of unprotected expression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.68).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.75).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Speech Protection Conditional on Demonstrable Harm").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, '4090d07d-6ec3-41c0-abc0-d88bcf87930e').
narrative_ontology:cs_kernel_codification('4090d07d-6ec3-41c0-abc0-d88bcf87930e', formalized).
narrative_ontology:cs_authority_grounding('4090d07d-6ec3-41c0-abc0-d88bcf87930e', lineage).
narrative_ontology:cs_interpretation_layer_present('4090d07d-6ec3-41c0-abc0-d88bcf87930e').
narrative_ontology:cs_reading_relation('4090d07d-6ec3-41c0-abc0-d88bcf87930e', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('4090d07d-6ec3-41c0-abc0-d88bcf87930e', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('4090d07d-6ec3-41c0-abc0-d88bcf87930e', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('4090d07d-6ec3-41c0-abc0-d88bcf87930e', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('4090d07d-6ec3-41c0-abc0-d88bcf87930e', foundational, harm_principle_applies_to_speech).
narrative_ontology:cs_axiom_status(harm_principle_applies_to_speech, holdable).
narrative_ontology:cs_axiom_grounding('4090d07d-6ec3-41c0-abc0-d88bcf87930e', harm_principle_applies_to_speech, deontological).
narrative_ontology:cs_axiom('4090d07d-6ec3-41c0-abc0-d88bcf87930e', foundational, victim_protection_priority).
narrative_ontology:cs_axiom_status(victim_protection_priority, holdable).
narrative_ontology:cs_axiom_grounding('4090d07d-6ec3-41c0-abc0-d88bcf87930e', victim_protection_priority, deontological).
narrative_ontology:cs_reference_frame('4090d07d-6ec3-41c0-abc0-d88bcf87930e', balancing_rights_framework).
narrative_ontology:cs_drift_state('4090d07d-6ec3-41c0-abc0-d88bcf87930e', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4090d07d-6ec3-41c0-abc0-d88bcf87930e', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, victims_of_harmful_speech).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, society_at_large).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speakers_of_harmful_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, advocates_for_broad_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or groups who suffer direct, demonstrable harm (e.g., incitement to violence, defamation, harassment) as a result of speech. They benefit from the constraint's enforcement.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, victims_of_harmful_speech, beneficiary,
    powerless, immediate, trapped, local).

% The broader community that benefits from reduced social friction, increased safety, and the protection of vulnerable groups from speech-related harm. This benefit is diffuse but significant.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, society_at_large, beneficiary,
    organized, generational, mobile, national).

% Individuals or groups whose speech is restricted or punished because it is deemed to cause demonstrable harm. They bear the cost of the constraint through limitations on their expression.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speakers_of_harmful_speech, payer,
    moderate, immediate, constrained, local).

% Legal scholars, civil liberties organizations, and activists who argue for expansive speech protections, often viewing harm thresholds as overly restrictive or prone to abuse. They bear the cost of a narrower speech domain.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, advocates_for_broad_speech, payer,
    organized, biographical, mobile, national).

% Judicial bodies and regulatory agencies responsible for interpreting and enforcing the harm threshold, balancing competing rights, and adjudicating specific cases of alleged harmful speech. They define and administer the constraint.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Academics and legal experts who analyze the theoretical underpinnings, practical application, and societal impact of speech protection laws, including the harm threshold. They provide critical analysis without direct enforcement power.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__harm_threshold_reading, society_at_large).
narrative_ontology:fixing_cost_class(speech_protection_kernel__harm_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the exercise of free speech with the imperative to protect individuals and groups from demonstrable harm, establishing a boundary where speech rights yield to safety and dignity concerns.
% TRANSFER_FUNCTION: Transfers the burden of demonstrable harm from potential victims to speakers, by restricting or penalizing speech that crosses the harm threshold. It also transfers the cost of enforcement and adjudication to the public and legal system.
% ABSENT_VOICES: Those whose speech is suppressed but lack the resources or platform to effectively challenge the harm determination, or those who believe their speech is unjustly categorized as harmful. Their perspectives are often marginalized in legal and public discourse.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, societies would struggle to protect vulnerable populations from speech-related harms, leading to increased social fragmentation, potential violence, and a breakdown of civil discourse. The legal and social frameworks for managing speech would need to be fundamentally re-established.
% FOUNDING_PROBLEM: How to reconcile the fundamental right to free expression with the equally fundamental need to protect individuals and communities from direct, tangible harm caused by speech, preventing speech from being weaponized.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested by ongoing legal disputes, social science research on the impact of hate speech and misinformation, and international human rights declarations that balance freedom of expression with other rights. This corroboration comes from independent legal bodies, academic researchers, and victim advocacy groups.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__harm_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__harm_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is moderate-to-high because it significantly limits the scope of speech for some, effectively extracting the right to certain expressions. Suppression (0.75) is high due to the active enforcement required to identify, adjudicate, and restrict harmful speech. The theater ratio (0.15) is low, indicating that the constraint is genuinely applied and enforced, rather than being performative. Accessibility collapse (0.60) is moderate, as while some speech is clearly unprotected, the definition and threshold of 'demonstrable harm' remain areas of ongoing debate and legal contestation. Resistance (0.55) is moderate, coming from civil liberties advocates and speakers whose expression is curtailed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of victims of harmful speech, this constraint is a necessary protection, ensuring their safety and dignity. From the perspective of speakers whose speech is restricted, or advocates for broad speech, it can be seen as an overreach that chills legitimate expression and risks subjective application. The courts and regulators, as agenda-setters, navigate this tension, attempting to balance competing rights.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims of harmful speech and society at large are beneficiaries, as the constraint aims to protect them from harm. Speakers of harmful speech and advocates for broad speech are victims, as their expressive freedom is curtailed. Courts and regulators act as agenda-setters, defining and enforcing the boundaries of this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_demonstrable_harm,
    'What constitutes ''demonstrable harm'' in practice, and how is it objectively measured versus subjectively experienced?',
    'Development of clearer legal standards, empirical research on the psychological and social impacts of speech, and judicial consistency in applying harm thresholds.',
    'If harm is defined too broadly or subjectively, the constraint''s extractiveness and suppression increase, potentially chilling legitimate speech. If defined too narrowly, it fails to protect vulnerable groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_demonstrable_harm, conceptual, 'Ambiguity in defining and measuring ''demonstrable harm'' from speech.').

omega_variable(
    causality_threshold_for_speech_harm,
    'How direct and foreseeable must the causal link between speech and harm be to trigger restriction?',
    'Legal precedents establishing clear tests for imminence and intent, and social consensus on the threshold of responsibility for speech outcomes.',
    'A low causality threshold expands the scope of restricted speech, increasing extraction. A high threshold narrows it, potentially leaving victims unprotected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_threshold_for_speech_harm, empirical, 'Uncertainty regarding the causal link required between speech and harm.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine balancing of rights, or an extractive mechanism cloaked in the language of harm, particularly when compared to other readings of the speech protection kernel?',
    'Comparative analysis of legal outcomes across jurisdictions adopting different readings, and longitudinal studies of the impact on marginalized groups'' ability to speak versus their protection from harm.',
    'If found to be primarily extractive, the classification would shift towards Snare, highlighting the suppression of dissent under the guise of protection. If genuinely balancing, the Tangled Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is one reading of the ''speech_protection_kernel''; its classification depends on the interpretation of ''harm'' and the balance of rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__harm_threshold_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__harm_threshold_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__harm_threshold_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__harm_threshold_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(spee_tr_t50, speech_protection_kernel__harm_threshold_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(spee_be_t50, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(spee_su_t50, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
