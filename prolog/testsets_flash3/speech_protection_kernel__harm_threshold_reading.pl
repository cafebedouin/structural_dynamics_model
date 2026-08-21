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
 *   human_readable: Speech Protection Conditional on Absence of Demonstrable Harm to Victims
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'harm threshold' reading of speech
 *   protection, where the exercise of free speech is conditional on the
 *   absence of demonstrable harm to victims. This reading posits a narrower
 *   boundary for protected speech compared to absolutist or marketplace
 *   readings, prioritizing victim protection and public order. It is one of
 *   several competing interpretations of the 'speech_protection_kernel'. The
 *   metrics reflect a system that actively enforces these boundaries, leading
 *   to substantial extraction from speakers whose expression is deemed
 *   harmful, and requiring ongoing suppression to maintain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.65).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.7).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Speech Protection Conditional on Absence of Demonstrable Harm to Victims").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, 'dbef62fa-3950-4753-b87e-d1e0e1324272').
narrative_ontology:cs_kernel_codification('dbef62fa-3950-4753-b87e-d1e0e1324272', formalized).
narrative_ontology:cs_authority_grounding('dbef62fa-3950-4753-b87e-d1e0e1324272', lineage).
narrative_ontology:cs_interpretation_layer_present('dbef62fa-3950-4753-b87e-d1e0e1324272').
narrative_ontology:cs_reading_relation('dbef62fa-3950-4753-b87e-d1e0e1324272', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('dbef62fa-3950-4753-b87e-d1e0e1324272', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbef62fa-3950-4753-b87e-d1e0e1324272', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbef62fa-3950-4753-b87e-d1e0e1324272', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('dbef62fa-3950-4753-b87e-d1e0e1324272', foundational, speech_causes_demonstrable_harm).
narrative_ontology:cs_axiom_status(speech_causes_demonstrable_harm, holdable).
narrative_ontology:cs_axiom_grounding('dbef62fa-3950-4753-b87e-d1e0e1324272', speech_causes_demonstrable_harm, empirically_contingent).
narrative_ontology:cs_axiom('dbef62fa-3950-4753-b87e-d1e0e1324272', foundational, protection_from_harm_outweighs_absolute_speech).
narrative_ontology:cs_axiom_status(protection_from_harm_outweighs_absolute_speech, holdable).
narrative_ontology:cs_axiom_grounding('dbef62fa-3950-4753-b87e-d1e0e1324272', protection_from_harm_outweighs_absolute_speech, deontological).
narrative_ontology:cs_reference_frame('dbef62fa-3950-4753-b87e-d1e0e1324272', balancing_rights_framework).
narrative_ontology:cs_drift_state('dbef62fa-3950-4753-b87e-d1e0e1324272', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dbef62fa-3950-4753-b87e-d1e0e1324272', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, victims_of_harmful_speech).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, public_order_authorities).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speakers_of_potentially_harmful_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, advocates_for_broad_speech_rights).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals or groups experience direct, demonstrable harm from speech (e.g., incitement to violence, defamation, harassment). They benefit from the constraint by having a mechanism to seek redress or prevent further harm, but their ability to activate this protection is often constrained by legal processes and power imbalances.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, victims_of_harmful_speech, beneficiary,
    moderate, immediate, constrained, local).

% Governmental bodies (courts, police, regulatory agencies) tasked with maintaining public safety and order. They interpret and enforce the harm threshold, benefiting from the ability to restrict speech that threatens social cohesion or individual safety. Their role is to balance speech rights against public welfare.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, public_order_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Individuals or groups whose speech is deemed to cause demonstrable harm. They bear the cost of this constraint through censorship, legal penalties, or self-censorship. Their exit options are limited to modifying their speech, challenging legal rulings, or facing consequences.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speakers_of_potentially_harmful_speech, payer,
    moderate, immediate, constrained, local).

% Organizations and legal scholars who argue for expansive speech protections, often viewing any harm-based restriction as a slippery slope to censorship. They bear the cost by seeing their preferred interpretation of speech rights curtailed and must actively litigate or advocate against the harm threshold.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, advocates_for_broad_speech_rights, payer,
    organized, generational, mobile, national).

% Benefits from a perceived reduction in social friction and protection from extreme forms of harmful speech, contributing to a more civil public discourse. However, they may also experience a chilling effect on certain forms of expression or a reduction in the diversity of viewpoints.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundary between protected expression and actionable harm, providing a framework for adjudicating conflicts between speaker autonomy and victim protection, thereby maintaining public order and individual safety.
% TRANSFER_FUNCTION: Transfers the burden of potential harm from victims to speakers, by allowing for the restriction or punishment of speech that crosses a demonstrable harm threshold. It also transfers interpretive authority over speech boundaries to public order authorities.
% ABSENT_VOICES: Those who believe that any restriction on speech, even for demonstrable harm, is an unacceptable infringement on fundamental liberty, or that the concept of 'harm' is too easily manipulated to suppress dissent, are often marginalized in the legal and public discourse that defines this threshold.
% DISAPPEARANCE_RATIONALE: If the harm threshold vanished, there would be an immediate increase in unprotected harmful speech, leading to greater social friction, potential incitement to violence, and a breakdown in public order. Victims would have no recourse, and public authorities would lose a key tool for maintaining peace, forcing a rapid re-establishment of some form of harm-based limitation.
% FOUNDING_PROBLEM: The problem of balancing individual freedom of expression against the collective need for public order and the protection of individuals from direct, tangible harm caused by speech.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil society organizations, and international human rights bodies consistently attest to the ongoing challenge of balancing speech rights and harm prevention, citing numerous contemporary cases where this tension is actively litigated and debated. This corroboration comes from outside the direct beneficiaries of speech restriction.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.65) is substantial because speakers bear the cost of self-censorship or legal penalties, and the definition of 'harm' can expand over time, increasing the scope of unprotected speech. Suppression (0.70) is high due to the active enforcement mechanisms (courts, regulations) required to identify and restrict harmful speech. Theater ratio (0.20) is relatively low, as the enforcement is genuinely aimed at preventing harm, though some performative aspects may exist in high-profile cases. Accessibility collapse (0.40) is moderate, as alternatives (e.g., private platforms, coded language) exist but are constrained. Resistance (0.55) is also moderate, reflecting ongoing legal challenges and advocacy from broad speech rights groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of victims and public order authorities, this constraint is a necessary 'rope' or 'tangled rope' that coordinates safety and civility. From the perspective of speakers and broad speech rights advocates, it functions more as a 'snare' or 'tangled rope' that extracts expressive freedom and suppresses dissent. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims of harmful speech and public order authorities are beneficiaries (d near 0.0-0.3), as the constraint provides them with protection and a mechanism for control. Speakers of potentially harmful speech and advocates for broad speech rights are targets (d near 0.7-1.0), bearing the costs of restriction and the narrowing of their expressive freedom. The general public is a diffuse beneficiary, gaining from perceived order but potentially losing expressive diversity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_demonstrable_harm,
    'How is ''demonstrable harm'' defined and measured in practice, and is this definition consistently applied or subject to political/social pressures?',
    'Empirical analysis of legal precedents and regulatory guidelines across jurisdictions, tracking changes in the scope and application of ''harm'' over time and across different types of speech.',
    'If ''harm'' is broadly and inconsistently applied, the constraint''s extractiveness and suppression are higher than measured, functioning more as a ''snare''. If narrowly and consistently applied, it aligns more with a ''tangled rope'' or even a ''rope'' for its coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_demonstrable_harm, empirical, 'Ambiguity in the definition and application of ''demonstrable harm''.').

omega_variable(
    chilling_effect_vs_actual_harm_prevention,
    'To what extent does the harm threshold primarily prevent actual, direct harm versus creating a ''chilling effect'' that suppresses legitimate, but controversial, speech?',
    'Sociological studies on self-censorship among speakers, surveys of public perception of speech boundaries, and comparative analysis with jurisdictions having different speech protection regimes.',
    'If the chilling effect is dominant, the constraint''s suppression is higher and its coordination function is weaker, pushing it towards a ''snare''. If actual harm prevention is dominant, its coordination function is stronger, supporting a ''tangled rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_vs_actual_harm_prevention, empirical, 'Balance between preventing harm and chilling legitimate speech.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''harm_threshold_reading'' genuinely distinct from other readings of the ''speech_protection_kernel'' (e.g., ''dignity_reading''), or do they converge in practice?',
    'Comparative legal analysis of case outcomes and legislative intent across different readings, identifying instances where the application of one reading would lead to a different outcome than another.',
    'If the readings converge in practice, the distinctiveness of this constraint as a separate reading is reduced, suggesting a need for re-decomposition or re-evaluation of the kernel''s structure. If they diverge, it reinforces the validity of this as a distinct constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Distinction between this reading and other kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__harm_threshold_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__harm_threshold_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__harm_threshold_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__harm_threshold_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(spee_tr_t50, speech_protection_kernel__harm_threshold_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(spee_be_t50, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(spee_su_t50, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 50, 0.7).


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
